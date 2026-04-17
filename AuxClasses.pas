{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

  Version 1.6 (2026-04-15)

  Last change 2026-04-17

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
  {$MODESWITCH DuplicateLocals+}
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
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF}, ListUtils;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EACException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EACInvalidValue      = class(EACException);
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

  TInt64Event    = procedure(Sender: TObject; Value: Int64) of object;
  TInt64Callback = procedure(Sender: TObject; Value: Int64);

  TIndexEvent    = procedure(Sender: TObject; Index: Integer) of object;
  TIndexCallback = procedure(Sender: TObject; Index: Integer);

  TIndex64Event    = procedure(Sender: TObject; Index: Int64) of object;
  TIndex64Callback = procedure(Sender: TObject; Index: Int64);

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
type
{
  TChangesTrackingState

  Used internally as storage for change tracking. You should have no need to
  use this type in normal code.
}
  TChangesTrackingState = record
    ChangingDone:   Boolean;
    ChangeCounter:  Integer;
  end;
  PChangesTrackingState = ^TChangesTrackingState;

const
  ChangesTrackingStateInit: TChangesTrackingState = (
    ChangingDone:   False;
    ChangeCounter:  0);

//------------------------------------------------------------------------------
type    
{
  TItemChangePropagation

  Used to select how are changes made to list items propagated to global or
  list change tracking and reporting.

    prpNone       - changes are not propagated

    prpToGlobal   - changes are propagated to global changes (DoChanging,
                    DoChange)

    prpToList     - item changes are propagated to list changes (DoListChanging,
                    DoListChange)

    prpToBoth     - item changes are propagated to both global and list changes.
                    First list changes are called and then the global changes.
                    Direct propagation to global changes occurs only if list
                    changes are not itself propagated (PropagateListChanges
                    property must be false).
}
  TItemChangePropagation = (prpNone,prpToGlobal,prpToList,prpToBoth);

//------------------------------------------------------------------------------

type
  // change reporting events and callbacks
  TListEvent    = procedure(Sender: TObject; List: Integer) of object;
  TListCallback = procedure(Sender: TObject; List: Integer);

  TListIndexEvent    = procedure(Sender: TObject; List: Integer; Index: Integer) of object;
  TListIndexCallback = procedure(Sender: TObject; List: Integer; Index: Integer);

  TListIndex64Event    = procedure(Sender: TObject; List: Integer; Index: Int64) of object;
  TListIndex64Callback = procedure(Sender: TObject; List: Integer; Index: Int64);

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

Function ConsumeArgs(const Args: array of const): Integer;
begin
// nothing to be seen here...
Result := Length(Args);
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

