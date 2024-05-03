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

  Version 1.1.1 (2024-04-14)

  Last change 2024-04-14

  ©2023-2024 František Milt

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

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BasicUIM_UseAuxExceptions for details).

  Indirect dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit BasicUIM;
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

{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}
{$H+}

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
  EUIMDuplicateItem     = class(EUIMException);
  EUIMInvalidIdentifier = class(EUIMException);

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
    Function GetCapacity: Integer; virtual; abstract;
    procedure SetCapacity(Value: Integer); virtual; abstract;
    Function GetCount: Integer; virtual; abstract;
    class Function GrowDelta: Integer; virtual; abstract;
    procedure Grow; virtual;  // only linear growth by GrowFactor
  public
    Function LowIndex: Integer; virtual; abstract;
    Function HighIndex: Integer; virtual; abstract;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TUIMRouting
--------------------------------------------------------------------------------
===============================================================================}
type
  TUIMIdentifier = type Integer;

  TUIMRoutingType = (rtFunction,rtMethod,rtObject,rtClass);

  TUIMImplementationFlag = (ifSelect,ifAvailable,ifSupported);
  TUIMImplementationFlags = set of TUIMImplementationFlag;

  TUIMImplementationType = (itFunction,itMethod,itObject,itClass,itAlias);

type
  TUIMImplementation = record
    ImplementationID:     TUIMIdentifier;
    ImplementationFlags:  TUIMImplementationFlags;
    case ImplementorType: TUIMImplementationType of
      itFunction: (FunctionImplementor: Pointer);
      itMethod:   (MethodImplementor:   TMethod);
      itObject:   (ObjectImplementor:   TObject);
      itClass:    (ClassImplementor:    TClass);
      itAlias:    (OriginalImplementor: TUIMIdentifier);
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
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    class Function GrowDelta: Integer; override;
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
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function IndexOf(ImplementationID: TUIMIdentifier): Integer; virtual;
    Function Find(ImplementationID: TUIMIdentifier; out Index: Integer): Boolean; virtual;
    Function Add(ImplementationID: TUIMIdentifier; FunctionImplementor: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; MethodImplementor: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; MethodImplementorCode,MethodImplementorData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ObjectImplementor: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ClassImplementor: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function AddAlias(ReferencedImplementationID, AliasImplementationID: TUIMIdentifier; ImplementationFlags: TUIMImplementationFlags = []): Integer; virtual;
    Function Copy(SourceImplementationID, NewImplementationID: TUIMIdentifier): Integer; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; FunctionImplementor: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; MethodImplementor: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; MethodImplementorCode,MethodImplementorData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ObjectImplementor: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ClassImplementor: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
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
    property ImplementationCount: Integer read GetCount;
    property SelectedIndex: Integer read GetSelectedIndex;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TImplementationManager
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TImplementationManager - class declaration
===============================================================================}
type
  TImplementationManager = class(TUIMCommonClass)
  protected
    fRoutings:      array of TUIMRouting;
    fRoutingCount:  Integer;
    Function GetRouting(Index: Integer): TUIMRouting; virtual;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    class Function GrowDelta: Integer; override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    Function Add(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer): Integer; overload; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function IndexOf(RoutingID: TUIMIdentifier): Integer; virtual;
    Function Find(RoutingID: TUIMIdentifier; out Index: Integer): Boolean; virtual;
    Function FindObj(RoutingID: TUIMIdentifier): TUIMRouting; virtual;
    Function Add(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): Integer; overload; virtual;
    Function Add(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): Integer; overload; virtual;
    Function Add(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): Integer; overload; virtual;
    Function Add(RoutingID: TUIMIdentifier; var ClassVariable: TClass): Integer; overload; virtual;
    Function AddObj(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): TUIMRouting; overload; virtual;
    Function AddObj(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): TUIMRouting; overload; virtual;
    Function AddObj(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): TUIMRouting; overload; virtual;
    Function AddObj(RoutingID: TUIMIdentifier; var ClassVariable: TClass): TUIMRouting; overload; virtual;
    Function Remove(RoutingID: TUIMIdentifier): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    property Routings[Index: Integer]: TUIMRouting read GetRouting; default;
    property RoutingCount: Integer read GetCount;
  end;

type
  // some aliases
  TUnitImplementationManager = TImplementationManager;
  TUnitImplManager = TImplementationManager;

implementation

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
--------------------------------------------------------------------------------
                                 TUIMCommonClass
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUIMCommonClass - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUIMCommonClass - protectecd methods
-------------------------------------------------------------------------------}

procedure TUIMCommonClass.Grow;
begin
If Count >= Capacity then
  Capacity := Capacity + GrowDelta; 
end;

{-------------------------------------------------------------------------------
    TUIMCommonClass - public methods
-------------------------------------------------------------------------------}

Function TUIMCommonClass.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
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
    TUIMRouting - protectecd methods
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
    If ifSelect in Value then
      SelectIndex(Index);
    fImplementations[Index].ImplementationFlags := Value - [ifSelect];
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

Function TUIMRouting.GetCapacity: Integer;
begin
Result := Length(fImplementations);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.SetCapacity(Value: Integer);
begin
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

Function TUIMRouting.GetCount: Integer;
begin
Result := fImplementationCount;
end;

//------------------------------------------------------------------------------

class Function TUIMRouting.GrowDelta: Integer;
begin
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
        Grow;
        Result := fImplementationCount;
        fImplementations[Result].ImplementationID := ImplementationID;
        fImplementations[Result].ImplementationFlags := ImplementationFlags - [ifSelect];
        fImplementations[Result].ImplementorType := ImplementorType;
        case ImplementorType of
          itFunction: fImplementations[Result].FunctionImplementor := Pointer(ImplementorPtr^);
          itMethod:   fImplementations[Result].MethodImplementor := TMethod(ImplementorPtr^);
          itObject:   fImplementations[Result].ObjectImplementor := TObject(ImplementorPtr^);
          itClass:    fImplementations[Result].ClassImplementor := TClass(ImplementorPtr^);
          itAlias:    fImplementations[Result].OriginalImplementor := TUIMIdentifier(ImplementorPtr^);
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
          itFunction: fImplementations[Result].FunctionImplementor := Pointer(ImplementorPtr^);
          itMethod:   fImplementations[Result].MethodImplementor := TMethod(ImplementorPtr^);
          itObject:   fImplementations[Result].ObjectImplementor := TObject(ImplementorPtr^);
          itClass:    fImplementations[Result].ClassImplementor := TClass(ImplementorPtr^);
          // itAlias should not happen here, but meh...
          itAlias:    fImplementations[Result].OriginalImplementor := TUIMIdentifier(ImplementorPtr^);
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

//------------------------------------------------------------------------------

Function TUIMRouting.Find(ImplementationID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementationID);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; FunctionImplementor: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itFunction,@FunctionImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; MethodImplementor: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itMethod,@MethodImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; MethodImplementorCode,MethodImplementorData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
var
  MethodTemp: TMethod;
begin
MethodTemp.Code := MethodImplementorCode;
MethodTemp.Data := MethodImplementorData;
Result := Add(ImplementationID,itMethod,@MethodTemp,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ObjectImplementor: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itObject,@ObjectImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ClassImplementor: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itClass,@ClassImplementor,ImplementationFlags);
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
        itFunction: Result := Add(NewImplementationID,FunctionImplementor,ImplementationFlags);
        itMethod:   Result := Add(NewImplementationID,MethodImplementor,ImplementationFlags);
        itObject:   Result := Add(NewImplementationID,ObjectImplementor,ImplementationFlags);
        itClass:    Result := Add(NewImplementationID,ClassImplementor,ImplementationFlags);
        itAlias:    Result := AddAlias(OriginalImplementor,NewImplementationID,ImplementationFlags);
      else
        raise EUIMInvalidValue.CreateFmt('TUIMRouting.Copy: Invalid implementor type (%d).',[Ord(ImplementorType)])
      end;
  end
else raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.Copy: Implementation with selected ID (%d) not found.',[SourceImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; FunctionImplementor: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itFunction,@FunctionImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; MethodImplementor: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itMethod,@MethodImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; MethodImplementorCode,MethodImplementorData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
var
  MethodTemp: TMethod;
begin
MethodTemp.Code := MethodImplementorCode;
MethodTemp.Data := MethodImplementorData;
Result := Replace(ImplementationID,itMethod,@MethodTemp,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ObjectImplementor: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itObject,@ObjectImplementor,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ClassImplementor: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itClass,@ClassImplementor,ImplementationFlags);
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
SetCapacity(0);
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
  raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.FlagsGet: Implementation with selected ID (%d) not found.',[ImplementationID]);
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
else raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.FlagsSet: Implementation with selected ID (%d) not found.',[ImplementationID]);
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
else raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.FlagAdd: Implementation with selected ID (%d) not found.',[ImplementationID]);
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
else raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.FlagRemove: Implementation with selected ID (%d) not found.',[ImplementationID]);
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
  Result := 0;
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
          rtFunction: Pointer(fRoutingVarAddr^) := fImplementations[fSelectedIndex].FunctionImplementor;
          rtMethod:   TMethod(fRoutingVarAddr^) := fImplementations[fSelectedIndex].MethodImplementor;
          rtObject:   TObject(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ObjectImplementor;
          rtClass:    TClass(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ClassImplementor;
        else
          raise EUIMInvalidValue.CreateFmt('TUIMRouting.SelectIndex: Invalid routing type (%d).',[Ord(fRoutingType)]);
        end;
      end
    else Select(fImplementations[Index].OriginalImplementor);
    Result := fImplementations[Index].ImplementationID;
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
  raise EUIMInvalidIdentifier.CreateFmt('TUIMRouting.Select: Implementation with selected ID (%d) not found.',[ImplementationID]);
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
end;

//------------------------------------------------------------------------------

Function TUIMRouting.CheckRouting: Boolean;
begin
If CheckIndex(fSelectedIndex) then
  case fRoutingType of
    rtFunction: Result := Pointer(fRoutingVarAddr^) = fImplementations[fSelectedIndex].FunctionImplementor;
    rtMethod:   Result := (TMethod(fRoutingVarAddr^).Code = fImplementations[fSelectedIndex].MethodImplementor.Code) and
                          (TMethod(fRoutingVarAddr^).Data = fImplementations[fSelectedIndex].MethodImplementor.Data);
    rtObject:   Result := TObject(fRoutingVarAddr^) = fImplementations[fSelectedIndex].ObjectImplementor;
    rtClass:    Result := TClass(fRoutingVarAddr^) = fImplementations[fSelectedIndex].ClassImplementor;
  else
    raise EUIMInvalidValue.CreateFmt('TUIMRouting.CheckRouting: Invalid routing type (%d).',[Ord(fRoutingType)])
  end
else Result := True;
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
If CheckIndex(Index) then
  Result := fRoutings[Index]
else
  raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.GetRouting: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.GetCapacity: Integer;
begin
Result := Length(fRoutings);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  begin
    If Value < fRoutingCount then
      begin
        For i := Value to HighIndex do
          FreeAndNil(fRoutings[i]);
        fRoutingCount := Value;
      end;
    SetLength(fRoutings,Value);
  end
else raise EUIMInvalidValue.CreateFmt('TImplementationManager.SetCapacity: Invalid capacity value (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.GetCount: Integer;
begin
Result := fRoutingCount;
end;

//------------------------------------------------------------------------------

class Function TImplementationManager.GrowDelta: Integer;
begin
Result := 16;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Initialize;
begin
SetLength(fRoutings,0);
fRoutingCount := 0;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Finalize;
begin
Clear;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.Add(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer): Integer;
begin
If not Find(RoutingID,Result) then
  begin
    Grow;
    Result := fRoutingCount;
    case RoutingType of
      rtFunction: fRoutings[Result] := TUIMRouting.Create(RoutingID,Pointer(RoutingVarAddr^));
      rtMethod:   fRoutings[Result] := TUIMRouting.Create(RoutingID,TMethod(RoutingVarAddr^));
      rtObject:   fRoutings[Result] := TUIMRouting.Create(RoutingID,TObject(RoutingVarAddr^));
      rtClass:    fRoutings[Result] := TUIMRouting.Create(RoutingID,TClass(RoutingVarAddr^));
    else
      raise EUIMInvalidValue.CreateFmt('TImplementationManager.Add: Invalid routing type (%d).',[Ord(RoutingType)])
    end;
    Inc(fRoutingCount);
  end
else raise EUIMDuplicateItem.CreateFmt('TImplementationManager.Add: Routing with selected ID (%d) already exists.',[RoutingID]);
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

Function TImplementationManager.LowIndex: Integer;
begin
Result := Low(fRoutings);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.HighIndex: Integer;
begin
Result := Pred(fRoutingCount);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.IndexOf(RoutingID: TUIMIdentifier): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fRoutings[i].RoutingID = RoutingID then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.Find(RoutingID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := IndexOf(RoutingID);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.FindObj(RoutingID: TUIMIdentifier): TUIMRouting;
var
  Index:  Integer;
begin
If Find(RoutingID,Index) then
  Result := fRoutings[Index]
else
  raise EUIMInvalidIdentifier.CreateFmt('TImplementationManager.FindObj: Routing with selected ID (%d) not found.',[RoutingID]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.Add(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): Integer;
begin
Result := Add(RoutingID,rtFunction,@FunctionVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.Add(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): Integer;
begin
Result := Add(RoutingID,rtMethod,@MethodVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.Add(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): Integer;
begin
Result := Add(RoutingID,rtObject,@ObjectVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.Add(RoutingID: TUIMIdentifier; var ClassVariable: TClass): Integer;
begin
Result := Add(RoutingID,rtClass,@ClassVariable);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.AddObj(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): TUIMRouting;
var
  Index:  Integer;
begin
Index := Add(RoutingID,FunctionVariable);
Result := fRoutings[Index];
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.AddObj(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): TUIMRouting;
var
  Index:  Integer;
begin
Index := Add(RoutingID,MethodVariable);
Result := Routings[Index];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.AddObj(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): TUIMRouting;
var
  Index:  Integer;
begin
Index := Add(RoutingID,ObjectVariable);
Result := Routings[Index];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.AddObj(RoutingID: TUIMIdentifier; var ClassVariable: TClass): TUIMRouting;
var
  Index:  Integer;
begin
Index := Add(RoutingID,ClassVariable);
Result := Routings[Index];
end;

//------------------------------------------------------------------------------

Function TImplementationManager.Remove(RoutingID: TUIMIdentifier): Integer;
begin
If Find(RoutingID,Result) then
  Delete(Result)
else
  Result := -1;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    FreeAndNil(fRoutings[Index]);
    For i := Index to Pred(HighIndex) do
      fRoutings[i] := fRoutings[i + 1];
    Dec(fRoutingCount);
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Clear;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  FreeAndNil(fRoutings[i]);
SetLength(fRoutings,0);
fRoutingCount := 0;
end;

end.
