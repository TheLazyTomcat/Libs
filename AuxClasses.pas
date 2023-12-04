{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

  Version 1.2.1 (2023-11-12)

  Last change 2023-11-12

  ©2018-2023 František Milt

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

//------------------------------------------------------------------------------
// do not touch following...
{$UNDEF AC_Include_Declaration}
{$UNDEF AC_Include_Implementation}
{$UNDEF AC_Include_Interfaced}

interface

uses
  SysUtils,
  AuxTypes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EACException = class(Exception);

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

Function Max(A,B: Integer): Integer;
begin
If A > B then
  Result := A
else
  Result := B;
end;

{===============================================================================
--------------------------------------------------------------------------------
                             Classes implementation
--------------------------------------------------------------------------------
===============================================================================}

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

end.
