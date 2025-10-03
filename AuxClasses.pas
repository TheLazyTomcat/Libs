{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

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

{
  AuxClasses_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  AuxClasses_UseAuxExceptions to achieve this.
}
{$IF Defined(AuxClasses_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// do not touch following...
{$UNDEF AC_Include_Declaration}
{$UNDEF AC_Include_Implementation}
{$UNDEF AC_Include_Interfaced}
{$UNDEF AC_Include_SInterfaced}

interface

uses
  SysUtils,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EACException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EACIndexOutOfBounds  = class(EACException);
  EACIncompatibleClass = class(EACException);

{===============================================================================
    Event and callback types
===============================================================================}
type
  TSimpleEvent    = procedure of object;
  TSimpleCallback = procedure;

  TPlainEvent = TSimpleEvent;
  TPlainCallback = TSimpleCallback;
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

  TProgressEvent    = procedure(Sender: TObject; Progress: Double) of object;
  TProgressCallback = procedure(Sender: TObject; Progress: Double);

  TStringEvent    = procedure(Sender: TObject; const Value: String) of object;
  TStringCallback = procedure(Sender: TObject; const Value: String);

  TMemoryEvent    = procedure(Sender: TObject; Addr: Pointer) of object;
  TMemoryCallback = procedure(Sender: TObject; Addr: Pointer);

  TBufferEvent    = procedure(Sender: TObject; const Buffer; Size: TMemSize) of object;
  TBufferCallback = procedure(Sender: TObject; const Buffer; Size: TMemSize);

  TObjectEvent    = procedure(Sender: TObject; Obj: TObject) of object;
  TObjectCallback = procedure(Sender: TObject; Obj: TObject);

  TOpenArrayEvent    = procedure(Sender: TObject; Values: array of const) of object;
  TOpenArrayCallback = procedure(Sender: TObject; Values: array of const);

  TOpenEvent    = TOpenArrayEvent;
  TOpenCallback = TOpenArrayCallback;

{===============================================================================
    List growing and shrinking settings
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
  smNormal  - if count is zero (or lower), then the capacity is set to zero,
              when count is greater than zero but lower than or equal to
              capacity * ShrinkFactor, and at the same time capacity is higher
              than ShrinkLimit, then capacity is set either to capacity *
              ShrinkFactor or ShrinkLimit, whichever is bigger, in all other
              cases the capacity is preserved
  smToCount - capacity is set to count
}
  TShrinkMode = (smKeepCap, smNormal, smToCount);

type
  // structure used to store grow settings in one place
  TListGrowSettings = record
    GrowInit:     Integer;
    GrowMode:     TGrowMode;
    GrowFactor:   Double;
    GrowLimit:    Integer;
    ShrinkMode:   TShrinkMode;
    ShrinkFactor: Double;
    ShrinkLimit:  Integer;
  end;
  PListGrowSettings = ^TListGrowSettings;

const
  // default list grow/shrink settings
  AC_LIST_GROW_SETTINGS_DEF: TListGrowSettings = (
    GrowInit:     32;
    GrowMode:     gmFast;
    GrowFactor:   1.0;
    GrowLimit:    128 * 1024 * 1024;
    ShrinkMode:   smNormal;
    ShrinkFactor: 0.5;
    ShrinkLimit:  32);

{===============================================================================
    Public functions - declaration
===============================================================================}

Function GetInstanceString(Instance: TObject): String;

{===============================================================================
--------------------------------------------------------------------------------
                               Classes declaration
--------------------------------------------------------------------------------
===============================================================================}
{
  TSimpleInterfacedObject

  This class is intended as an ancestor for classes that wants to implement
  interfaces but without the automatic reference counting.

  Method QueryInterface is fully implemented, _AddRef and _Release do nothing
  and always return 1.
}
type
  TSimpleInterfacedObject = class(TObject,IInterface)
  protected
  {$IFDEF FPC}
    Function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): LongInt; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  {$ELSE}
    Function QueryInterface(const IID: TGUID; out Obj): HResult; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  {$ENDIF}
    Function _AddRef: Integer; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
    Function _Release: Integer; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  end;

//==============================================================================
// classes based on TObject
{$DEFINE AC_Include_Declaration}
  {$INCLUDE '.\AuxClasses.inc'}
{$UNDEF AC_Include_Declaration}

// classes based on TInterfacedObject
{$DEFINE AC_Include_Declaration}
  {$DEFINE AC_Include_Interfaced}
    {$INCLUDE '.\AuxClasses.inc'}
  {$UNDEF AC_Include_Interfaced}
{$UNDEF AC_Include_Declaration}

// classes based on TSimpleInterfacedObject
{$DEFINE AC_Include_Declaration}
  {$DEFINE AC_Include_SInterfaced}
    {$INCLUDE '.\AuxClasses.inc'}
  {$UNDEF AC_Include_SInterfaced}
{$UNDEF AC_Include_Declaration}

implementation

{$IF not Defined(FPC) and Defined(Windows) and Defined(PurePascal)}
uses
  Windows;
{$IFEND}

{===============================================================================
    Public functions - implementation
===============================================================================}

Function GetInstanceString(Instance: TObject): String;
begin
If Assigned(Instance) then
  Result := Format('%s(%p)',[Instance.ClassName,Pointer(Instance)])
else
  Result := 'TObject(nil)'; // return some sensible string, not just nothing
end;

{===============================================================================
    Internal functions - implementation
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

//------------------------------------------------------------------------------

Function ResolveGrowDelta(Capacity,Count,MinDelta: Integer; GrowSettings: TListGrowSettings; out Delta: Integer): Boolean;
begin
// we are assuming sane inputs, namely that count is NOT bigger than capacity
If (Count + MinDelta) > Capacity then
  begin
    If Capacity > 0 then
      case GrowSettings.GrowMode of
        gmLinear:
          Delta := Trunc(GrowSettings.GrowFactor);
        gmFast:
          Delta := Trunc(Capacity * GrowSettings.GrowFactor);
        gmFastAttenuated:
          If Capacity < GrowSettings.GrowLimit then
            Delta := Trunc(Capacity * GrowSettings.GrowFactor)
          else
            Delta := GrowSettings.GrowLimit shr 4;
      else
       {gmSlow}
        Delta := 1;
      end
    else Delta := GrowSettings.GrowInit;
    If Delta < MinDelta then
      Delta := MinDelta;
  end
else Delta := 0;
Result := Delta > 0;
end;

//------------------------------------------------------------------------------

Function ResolveShrinkCapacity(Capacity,Count: Integer; GrowSettings: TListGrowSettings; out NewCapacity: Integer): Boolean;

  Function Max(A,B: Integer): Integer;
  begin
    If A > B then
      Result := A
    else
      Result := B;
  end;

begin
Result := True;
If Capacity > 0 then
  case GrowSettings.ShrinkMode of
    smNormal:
      If Count <= 0 then
        NewCapacity := 0
      else If (Capacity > GrowSettings.ShrinkLimit) and
              (Count <= Trunc(Capacity * GrowSettings.ShrinkFactor)) then
        NewCapacity := Max(Trunc(Capacity * GrowSettings.ShrinkFactor),GrowSettings.ShrinkLimit)
      else
        Result := False;
    smToCount:
      NewCapacity := Count;
  else
    {smKeepCap}
    Result := False;
  end
else Result := False;
end;

{===============================================================================
--------------------------------------------------------------------------------
                             Classes implementation
--------------------------------------------------------------------------------
===============================================================================}

{$IFDEF FPC}
Function TSimpleInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): LongInt; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
Function TSimpleInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
{$ENDIF}
begin
If GetInterface(IID,Obj) then
  Result := S_OK
else
  Result := E_NOINTERFACE;
end;

//------------------------------------------------------------------------------

Function TSimpleInterfacedObject._AddRef: Integer; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
begin
Result := 1;
end;

//------------------------------------------------------------------------------

Function TSimpleInterfacedObject._Release: Integer; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
begin
Result := 1;
end;

//==============================================================================

// classes based on TObject
{$DEFINE AC_Include_Implementation}
  {$INCLUDE '.\AuxClasses.inc'}
{$UNDEF AC_Include_Implementation}

// classes based on TInterfacedObject
{$DEFINE AC_Include_Implementation}
  {$DEFINE AC_Include_Interfaced}
    {$INCLUDE '.\AuxClasses.inc'}
  {$UNDEF AC_Include_Interfaced}
{$UNDEF AC_Include_Implementation}

// classes based on TSimpleInterfacedObject
{$DEFINE AC_Include_Implementation}
  {$DEFINE AC_Include_SInterfaced}
    {$INCLUDE '.\AuxClasses.inc'}
  {$UNDEF AC_Include_SInterfaced}
{$UNDEF AC_Include_Implementation}

end.
