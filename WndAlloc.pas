{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WndAlloc

    When the system calls window messages handler, it expects it to be a normal
    function. But we want it to be method of an object. Methods have hidden
    (or implicit) first parameter (Self).
    This pose a problem - handling method and handling function has different
    signature and of course, system cannot know the Self parameter.
    It is therefore not possible to assign method to a window messages handler
    function.

    Two possible solutions are implemented in this library. One purely in
    pascal (working with WinAPI), and the other using assembly.


    PurePascal solution

      When new window gets created, information about the handler method is
      stored in memory (on heap) and pointer to this information is in turn
      stored directly in the window object using WinAPI. An universal function
      (common for all windows) is assigned as a messages handler function.
      When window calls this function, the function again uses WinAPI to get
      pointer to handler method information stored in the window and then, using
      this information, calls proper method.

      This solution is cleaner, but since the universal function has to obtain
      pointer to handler method info from the window every time it is executed,
      it might be slower.


    ASM solution

      When a window allocation is requested, the manager creates the window
      (using WinAPI) and along with it a new mediator (small piece of code,
      only few instructions long) bound to that particular window (every window
      has it own unique mediator).
      Information about method that should handle messages of the created
      window is stored in the mediator and also the mediator is assigned as
      a window messages handler function.
      When the window requires handler function to process a message, it calls
      the mediator as it is assigned as the handler function, mediator stores
      pointer to information about handler method into XMM0 register (SSE
      register, unused at this point) and then passes execution to an universal
      function.
      This universal function (common for all windows) loads information stored
      in XMM0 register and uses it to call proper handling method.

      This solution is sligtly faster than the pascal one, but it is a nasty
      hack and requires the use of SSE2 instruction and SSE register.

  Version 1.2.1 (2020-03-09)

  Last change 2022-09-14

  ©2015-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.WndAlloc

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

    SimpleCPUID is required only when PurePascal symbol is not defined.

===============================================================================}
unit WndAlloc;

{
  WndAlloc_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF WndAlloc_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{
  ImplicitManager

  When defined, an internally managed utility window manager is created at
  the unit initialization and freed at finalization. This allows the use of
  functions AllocateHWND and DeallocateHWND.
  It is here mainly for cases where implicit manager is not desired, so it can
  be disabled.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol WndAlloc_ImplicitManager_Off.
}
{$DEFINE ImplicitManager}
{$IFDEF WndAlloc_ImplicitManager_Off}
  {$UNDEF ImplicitManager}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, SyncObjs;

{===============================================================================
--------------------------------------------------------------------------------
                              TUtilityWindowManager
--------------------------------------------------------------------------------
===============================================================================}

const
  WA_MAXWINDOWS_DEF  = 512;
  WA_WINDOWCLASS_DEF = 'TUtilityWindow';

type
  EWAException = class(Exception);

  EWAOutOfResources = class(EWAException);

{===============================================================================
    TUtilityWindowManager - class declaration
===============================================================================}

type
  TUtilityWindowManager = class(TObject)
  protected
    fWindowClassName: String;
    fMaxWindows:      Integer;
    fSynchronizer:    TCriticalSection;
    fWindowCount:     Integer;
    fUsesASM:         Boolean;
    fMediators:       Pointer;
    procedure RegisterWindowClass; virtual;
    procedure UnregisterWindowClass; virtual;
    Function NewMediator(Method: TMethod): Pointer; virtual;
    procedure RemoveMediator(Mediator: Pointer); virtual;
  public
    constructor Create(WindowClassName: String = WA_WINDOWCLASS_DEF; MaxWindows: Integer = WA_MAXWINDOWS_DEF; CanUseASM: Boolean = True);
    destructor Destroy; override;
    Function AllocateHWND(Method: TWndMethod): HWND; virtual;
    procedure DeallocateHWND(Wnd: HWND); virtual;
    property WindowClassName: String read fWindowClassName;
    property MaxWindows: Integer read fMaxWindows;
    property WindowCount: Integer read fWindowCount;
    property UsesASM: Boolean read fUsesASM;
  end;

{===============================================================================
    Functions of implicit manager
===============================================================================}

{$IFDEF ImplicitManager}
  Function AllocateHWND(Method: TWndMethod): HWND;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
  procedure DeallocateHWND(Wnd: HWND);{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
{$ENDIF}

implementation

uses
  Messages,
  AuxTypes, StrRect{$IFNDEF PurePascal}, SimpleCPUID{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Auxiliary functions, types, constants, ...
===============================================================================}

const
  GWLP_WNDPROC  = -4;
  GWLP_USERDATA = -21;

{$IF not Declared(LONG_PTR)}
type
  LONG_PTR = Pointer;
{$IFEND}

//------------------------------------------------------------------------------

{$IF not(Declared(GetWindowLongPtr) and Declared(SetWindowLongPtr))}

{
  Following code is supposed to work only in 32 bits, 64bit OS provides these
  functions in WinAPI.
}
{$IF SizeOf(Pointer) <> 4}
  {$MESSAGE FATAL 'Unsupported platform.'}
{$IFEND}

Function GetWindowLongPtr(hWnd: HWND; nIndex: Int32): Pointer;
begin
Result := Pointer(GetWindowLong(hWnd,nIndex));
end;

//------------------------------------------------------------------------------

Function SetWindowLongPtr(hWnd: HWND; nIndex: Int32; dwNewLong: Pointer): Pointer;
begin
Result := Pointer(SetWindowLong(hWnd,nIndex,Int32(dwNewLong)));
end;

{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                              TUtilityWindowManager
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TUtilityWindowManager - internal types, constants, functions, ...
===============================================================================}

type
  TMediator = packed record
  {$IFDEF x64}
    MOV_RAX_MethodAddr:   array[0..1] of Byte;
    MethodAddr:           Pointer;
    MOVQ_XMM0_RAX:        array[0..4] of Byte;
    MOV_RAX_HandlerAddr:  array[0..1] of Byte;
    HandlerAddr:          Pointer;
    JMP_RAX:              array[0..1] of Byte;
    Padding:              array[0..4] of Byte;
  {$ELSE}
    MOV_EAX_MethodAddr:   Byte;
    MethodAddr:           Pointer;
    MOVD_XMM0_EAX:        array[0..3] of Byte;
    MOV_EAX_HandlerAddr:  Byte;
    HandlerAddr:          Pointer;
    JMP_EAX:              array[0..1] of Byte;
  {$ENDIF}
    Method:               TMethod;  
  end;
  PMediator = ^TMediator;

const
  WA_MEDIATOR_DEF: TMediator = (
  {$IFDEF x64}
    MOV_RAX_MethodAddr:   ($48,$B8);              //  MOV     RAX,    MethodAddr
    MethodAddr:           nil;
    MOVQ_XMM0_RAX:        ($66,$48,$0F,$6E,$C0);  //  MOVD    XMM0,   RAX
    MOV_RAX_HandlerAddr:  ($48,$B8);              //  MOV     RAX,    HandlerAddr
    HandlerAddr:          nil;
    JMP_RAX:              ($FF,$E0);              //  JMP     RAX
    Padding:              (0,0,0,0,0);
  {$ELSE}
    MOV_EAX_MethodAddr:   $B8;                    //  MOV     EAX,    MethodAddr
    MethodAddr:           nil;
    MOVD_XMM0_EAX:        ($66,$0F,$6E,$C0);      //  MOVD    XMM0,   EAX
    MOV_EAX_HandlerAddr:  $B8;                    //  MOV     EAX,    HandlerAddr
    HandlerAddr:          nil;
    JMP_EAX:              ($FF,$E0);              //  JMP     EAX
  {$ENDIF}
    Method: (
      Code:                 nil;
      Data:                 nil));

//------------------------------------------------------------------------------

Function WndHandler_Pascal(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MediatorPtr:  PMediator;
  MethodAddr:   Pointer;
  Msg:          TMessage;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
MediatorPtr := PMediator(GetWindowLongPtr(Window,GWLP_USERDATA));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
If Assigned(MediatorPtr) then
  MethodAddr := MediatorPtr^.MethodAddr
else
  MethodAddr := nil;
If Assigned(TWndMethod(MethodAddr^)) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(MethodAddr^)(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}
procedure LoadMethodInfoAddr(out Addr: Pointer); assembler; register;
asm
{$IFDEF x64}
    MOVSD   [Addr], XMM0
{$ELSE}
    MOVSS   [Addr], XMM0
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function WndHandler_ASM(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$IFDEF PurePascal}
begin
Result := DefWindowProc(Window,Message,wParam,lParam);
end;
{$ELSE}
var
  MethodAddr: Pointer;
  Msg:        TMessage;
begin
LoadMethodInfoAddr(MethodAddr);
If Assigned(TWndMethod(MethodAddr^)) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(MethodAddr^)(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;
{$ENDIF}

{===============================================================================
    TUtilityWindowManager - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUtilityWindowManager - protected methods
-------------------------------------------------------------------------------}

procedure TUtilityWindowManager.RegisterWindowClass;
var
  WindowClass:  TWndClass;
begin
ZeroMemory(@WindowClass,SizeOf(TWndClass));
// unregister window class if it is already registered
If Windows.GetClassInfo(hInstance,PChar(StrToWin(fWindowClassName)),WindowClass) then
  Windows.UnregisterClass(PChar(StrToWin(fWindowClassName)),hInstance);
// register the window class
ZeroMemory(@WindowClass,SizeOf(TWndClass));
WindowClass.lpfnWndProc := @DefWindowProc;
WindowClass.hInstance := hInstance;
WindowClass.lpszClassName := PChar(StrToWin(fWindowClassName));
If Windows.RegisterClass(WindowClass) = 0 then
  raise EWAException.CreateFmt('TUtilityWindowManager.RegisterWindowClass: Unable to register utility window class. (%s)',[SysErrorMessage(GetLastError)]);
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.UnregisterWindowClass;
begin
Windows.UnregisterClass(PChar(StrToWin(fWindowClassName)),hInstance);
end;

//------------------------------------------------------------------------------

Function TUtilityWindowManager.NewMediator(Method: TMethod): Pointer;
var
  i:    Integer;
  Temp: PMediator;
begin
For i := 0 to Pred(fMaxWindows) do
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    Temp := PMediator(PtrUInt(fMediators) + PtrUInt(i * SizeOf(TMediator)));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    If not Assigned(Temp^.MethodAddr) then
      begin
        Temp^ := WA_MEDIATOR_DEF;
        Temp^.MethodAddr := Addr(Temp^.Method);
        Temp^.HandlerAddr := @WndHandler_ASM;
        Temp^.Method := Method;
        Result := Pointer(Temp);
        Exit;
      end;
  end;
raise EWAOutOfResources.Create('TUtilityWindowManager.NewMediator: Out of resources.');
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.RemoveMediator(Mediator: Pointer);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Mediator) >= PtrUInt(fMediators)) and
   (PtrUInt(Mediator) <= (PtrUInt(fMediators) + PtrUInt(Pred(fMaxWindows) * SizeOf(TMediator)))) then
  PMediator(Mediator)^.MethodAddr := nil;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{-------------------------------------------------------------------------------
    TUtilityWindowManager - public methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
constructor TUtilityWindowManager.Create(WindowClassName: String = WA_WINDOWCLASS_DEF; MaxWindows: Integer = WA_MAXWINDOWS_DEF; CanUseASM: Boolean = True);
begin
inherited Create;
fWindowClassName := WindowClassName;
fMaxWindows := MaxWindows;
fSynchronizer := TCriticalSection.Create;
fWindowCount := 0;
{$IFDEF PurePascal}
fUsesAsm := False;
{$ELSE}
with TSimpleCPUID.Create do
try
  fUsesAsm := CanUseASM and Info.SupportedExtensions.SSE2;
finally
  Free;
end;
{$ENDIF}
If fUsesAsm then
  fMediators := VirtualAlloc(nil,fMaxWindows * SizeOf(TMediator),MEM_COMMIT,PAGE_EXECUTE_READWRITE)
else
  fMediators := AllocMem(fMaxWindows * SizeOf(TMediator));
RegisterWindowClass;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

destructor TUtilityWindowManager.Destroy;
begin
UnregisterWindowClass;
If fUsesAsm then
  VirtualFree(fMediators,fMaxWindows * SizeOf(TMediator),MEM_RELEASE)
else
  FreeMem(fMediators,fMaxWindows * SizeOf(TMediator));
fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TUtilityWindowManager.AllocateHWND(Method: TWndMethod): HWND;
begin
Result := 0;
fSynchronizer.Enter;
try
  If fWindowCount < MaxWindows then
    begin
      Result := CreateWindowEx(WS_EX_TOOLWINDOW,PChar(StrToWin(fWindowClassName)),'',WS_POPUP,0,0,0,0,0,0,hInstance,nil);
      If Result <> 0 then
        begin
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          If fUsesAsm then
            begin
              SetWindowLongPtr(Result,GWLP_WNDPROC,LONG_PTR(NewMediator(TMethod(Method))));
            end
          else
            begin
              SetWindowLongPtr(Result,GWLP_WNDPROC,LONG_PTR(@WndHandler_Pascal));
              SetWindowLongPtr(Result,GWLP_USERDATA,LONG_PTR(NewMediator(TMethod(Method))));
            end;
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
          Inc(fWindowCount);
        end
      else raise EWAException.CreateFmt('TUtilityWindowManager.AllocateHWND: Unable to create utility window. %s',[SysErrorMessage(GetLastError)]);
    end
  else raise EWAException.Create('TUtilityWindowManager.AllocateHWND: Unable to create new mediator.');
finally
  fSynchronizer.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.DeallocateHWND(Wnd: HWND);
var
  Mediator: Pointer;
begin
fSynchronizer.Enter;
try
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  If fUsesAsm then
    Mediator := Pointer(GetWindowLongPtr(Wnd,GWLP_WNDPROC))
  else
    Mediator := Pointer(GetWindowLongPtr(Wnd,GWLP_USERDATA));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  If Assigned(Mediator) then
    begin
      DestroyWindow(Wnd);
      RemoveMediator(Mediator);
      Dec(fWindowCount);
    end;
finally
  fSynchronizer.Leave;
end;
end;

{===============================================================================
    Implementation of implicit manager                                         
===============================================================================}

{$IFDEF ImplicitManager}
var
  UtilityWindowManager: TUtilityWindowManager;
  
//------------------------------------------------------------------------------

Function AllocateHWND(Method: TWndMethod): HWND;
begin
Result := UtilityWindowManager.AllocateHWND(Method);
end;

//------------------------------------------------------------------------------

procedure DeallocateHWND(Wnd: HWND);
begin
UtilityWindowManager.DeallocateHWND(Wnd);
end;

//------------------------------------------------------------------------------

initialization
  UtilityWindowManager := TUtilityWindowManager.Create;

finalization
  UtilityWindowManager.Free;

{$ENDIF}

end.

