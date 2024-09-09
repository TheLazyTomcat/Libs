{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinTaskbarProgress

    Provides a small set of functions for setting-up the progress state and
    progress value in taskbar icon (supported from Windows 7 up).
    It also provides some of the interfaces for accessing the taskbar.

  Version 1.1 (2019-10-04)

  Last change 2022-09-13

  ©2018-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.WinTaskbarProgress

  Dependencies:
  * AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

  AuxTypes is only required when compiled in Delphi.

===============================================================================}
unit WinTaskbarProgress;

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

{
  ImplicitTaskbarProgress

  When symbol ImplicitTaskbarProgress is defined, this unit creates implicit
  taskbar progress manager object and provides some standalone functions that
  are using it.
  It is here mainly for situations, where the standalone manager is not desired,
  so it can be disabled.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol WinTaskbarProgress_ImplicitTaskbarProgress_Off.
}
{$DEFINE ImplicitTaskbarProgress}
{$IFDEF WinTaskbarProgress_ImplicitTaskbarProgress_Off}
  {$UNDEF ImplicitTaskbarProgress}
{$ENDIF}

{$MINENUMSIZE 4}

interface

uses
  Windows{$IFNDEF FPC}, AuxTypes{$ENDIF};

type
  // some basic types used in interfaces
  ULONGLONG  = UInt64;
  HIMAGELIST = THandle;

//==============================================================================

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd562322(v=vs.85).aspx
}
  THUMBBUTTONMASK = (
    THB_BITMAP  = $00000001,
    THB_ICON    = $00000002,
    THB_TOOLTIP = $00000004,
    THB_FLAGS   = $00000008
  );

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd562321(v=vs.85).aspx
}
  THUMBBUTTONFLAGS = (
    THBF_ENABLED        = $00000000,
    THBF_DISABLED       = $00000001,
    THBF_DISMISSONCLICK = $00000002,
    THBF_NOBACKGROUND   = $00000004,
    THBF_HIDDEN         = $00000008,
    THBF_NONINTERACTIVE = $00000010
  );

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd391559(v=vs.85).aspx
}
  THUMBBUTTON = record
    dwMask:   THUMBBUTTONMASK;
    iId:      UINT;
    iBitmap:  UINT;
    hIcon:    HICON;
    szTip:    array[0..259] of WideChar;
    dwFlags:  THUMBBUTTONFLAGS;
  end;
  PTHUMBBUTTON = ^THUMBBUTTON;
  LPTHUMBBUTTON = PTHUMBBUTTON;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd391697(v=vs.85).aspx#TBPF_NOPROGRESS
}
  TBPFLAG = (
    TBPF_NOPROGRESS    = $00000000,
    TBPF_INDETERMINATE = $00000001,
    TBPF_NORMAL        = $00000002,
    TBPF_ERROR         = $00000004,
    TBPF_PAUSED        = $00000008
  );

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd562320(v=vs.85).aspx
}
  STPFLAG = (
    STPF_NONE                      = $00000000,
    STPF_USEAPPTHUMBNAILALWAYS     = $00000001,
    STPF_USEAPPTHUMBNAILWHENACTIVE = $00000002,
    STPF_USEAPPPEEKALWAYS          = $00000004,
    STPF_USEAPPPEEKWHENACTIVE      = $00000008
  );

//==============================================================================
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/bb774652(v=vs.85).aspx
}
  ITaskbarList = interface(IUnknown)
  ['{56FDF342-FD6D-11d0-958A-006097C9A090}']
    Function HrInit: HRESULT; stdcall;
    Function AddTab(hwnd: HWND): HRESULT; stdcall;
    Function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    Function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    Function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/bb774638(v=vs.85).aspx
}
  ITaskbarList2 = interface(ITaskbarList)
  ['{602D4995-B13A-429b-A66E-1935E44F4317}']
    Function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd391692(v=vs.85).aspx
}
  ITaskbarList3 = interface(ITaskbarList2)
  ['{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}']
    Function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HRESULT; stdcall;
    Function SetProgressState(hwnd: HWND; tbpFlags: TBPFLAG): HRESULT; stdcall;
    Function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    Function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    Function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT; stdcall;
    Function SetTabActive(hwndTab: HWND; hwndMDI: HWND; dwReserved: DWORD): HRESULT; stdcall;
    Function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: LPTHUMBBUTTON): HRESULT; stdcall;
    Function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: LPTHUMBBUTTON): HRESULT; stdcall;
    Function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    Function SetOverlayIcon(hwnd: HWND; hicon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    Function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    Function SetThumbnailClip(hwnd: HWND; prcClip: PRECT): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd562049(v=vs.85).aspx
}
  ITaskbarList4 = interface(ITaskbarList3)
  ['{c43dc798-95d1-4bea-9030-bb99e2983a1a}']
    Function SetTabProperties(hwndTab: HWND; stpFlags: STPFLAG): HRESULT; stdcall;
  end;

//==============================================================================

const
  IID_ITaskbarList:  TGUID = '{56FDF342-FD6D-11d0-958A-006097C9A090}';
  IID_ITaskbarList2: TGUID = '{602D4995-B13A-429b-A66E-1935E44F4317}';
  IID_ITaskbarList3: TGUID = '{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}';
  IID_ITaskbarList4: TGUID = '{c43dc798-95d1-4bea-9030-bb99e2983a1a}';

  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';

{===============================================================================
--------------------------------------------------------------------------------
                              Pascal implementation
--------------------------------------------------------------------------------
===============================================================================}

type
  TTaskbarProgressState = (tpsNoProgress, tpsIndeterminate, tpsNormal, tpsError, tpsPaused);

{===============================================================================
    TTaskBarProgress - class declaration
===============================================================================}

type
  TTaskBarProgress = class(TObject)
  protected
    fWindowHandle:    HWND;
    fHandleObtained:  Boolean;
    fTaskbarList:     ITaskbarList4;
    fStateLastResult: HResult;
    fValueLastResult: HResult;
    fProgressState:   TTaskbarProgressState;
    fProgressMax:     UInt64;
    fProgressValue:   UInt64;
    fProgressCoef:    UInt64;
    fProgress:        Double;
    class Function GetWindowHandle: HWND; virtual;
    Function GetActive: Boolean; virtual;
    procedure SetProgressState(Value: TTaskbarProgressState); virtual;
    procedure SetProgressMax(Value: UInt64); virtual;
    procedure SetProgressValue(Value: UInt64); virtual;
    procedure SetProgressCoef(Value: UInt64); virtual;
    procedure SetProgress(Value: Double); virtual;
    procedure ObtainWindowHandle; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure UpdateProgress; virtual;
  public
    constructor Create(WindowHandle: HWND = 0);
    destructor Destroy; override;
    procedure ReinitHandle(WindowHandle: HWND = 0); virtual;
    property WindowHandle: HWND read fWindowHandle;
    property Active: Boolean read GetActive;
    property StateLastResult: HResult read fStateLastResult;
    property ValueLastResult: HResult read fValueLastResult;
    property ProgressState: TTaskbarProgressState read fProgressState write SetProgressState;
    property ProgressMaxValue: UInt64 read fProgressMax write SetProgressMax;
    property ProgressValue: UInt64 read fProgressValue write SetProgressValue;
    property ProgressCoefficient: UInt64 read fProgressCoef write SetProgressCoef;
    property Progress: Double read fProgress write SetProgress;
  end;

{$IFDEF ImplicitTaskbarProgress}
{===============================================================================
    Standalone functions
===============================================================================}

Function TaskbarProgressActive: Boolean;

Function SetTaskbarProgressState(State: TTaskbarProgressState): Boolean;
Function SetTaskbarProgressValue(Completed, Total: UInt64): Boolean; overload;
Function SetTaskbarProgressValue(Value: Double; IntCoef: UInt64 = 1000): Boolean; overload;

{$ENDIF}

implementation

uses
  SysUtils, ActiveX,{$IFDEF FPC} InterfaceBase{$ELSE} Forms{$ENDIF};

{===============================================================================
    TTaskBarProgress - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TTaskBarProgress - protected functions
-------------------------------------------------------------------------------}

Function TTaskBarProgress.GetActive: Boolean;
begin
Result := Assigned(fTaskbarList);
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.SetProgressState(Value: TTaskbarProgressState);
begin
fProgressState := Value;
UpdateProgress;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.SetProgressMax(Value: UInt64);
begin
fProgressMax := Value;
If fProgressValue > fProgressMax then
  fProgressValue := fProgressMax;
fProgressCoef := fProgressMax;
If fProgressMax > 0 then
  fProgress := fProgressValue / fProgressMax
else
  fProgress := 0.0;
UpdateProgress;
end;
 
//------------------------------------------------------------------------------

procedure TTaskBarProgress.SetProgressValue(Value: UInt64);
begin
If Value <= fProgressMax then
  fProgressValue := Value
else
  fProgressValue := fProgressMax;
If fProgressMax > 0 then
  fProgress := fProgressValue / fProgressMax
else
  fProgress := 0.0;
UpdateProgress;
end;
 
//------------------------------------------------------------------------------

procedure TTaskBarProgress.SetProgressCoef(Value: UInt64);
begin
fProgressCoef := Value;
fProgressMax := fProgressCoef;
fProgressValue := Round(fProgress * fProgressCoef);
UpdateProgress;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.SetProgress(Value: Double);
begin
If Value < 0.0 then
  fProgress := 0.0
else If Value > 1.0 then
  fProgress := 1.0
else
  fProgress := Value;
fProgressMax := fProgressCoef;
fProgressValue := Round(fProgressCoef * fProgress);
UpdateProgress;
end;

//------------------------------------------------------------------------------

class Function TTaskBarProgress.GetWindowHandle: HWND;
{$IFNDEF FPC}
var
  WindowInfo: TWindowInfo;
{$ENDIF}  
begin
{$IFDEF FPC}
{$PUSH}{$WARN SYMBOL_PLATFORM OFF} // for AppHandle
Result := WidgetSet.AppHandle;
{$POP}
{$ELSE}
Result := Application.Handle;
If Assigned(Application.MainForm) then
  If GetWindowInfo(Application.MainForm.Handle,WindowInfo) then
    If WindowInfo.dwExStyle and WS_EX_APPWINDOW <> 0 then
      Result := Application.MainForm.Handle;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.ObtainWindowHandle;
begin
If not fHandleObtained then
  begin
    If not IsWindow(fWindowHandle) then
      fWindowHandle := GetWindowHandle;
    fHandleObtained := True;  
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.Initialize;
begin
{
  obtaining of true fWindowHandle is deffered to first call of
  UpdateProgress method - it is done because implicit manager is created before
  the windows, so it cannot get it at that point
}
fHandleObtained := False;
If Succeeded(CoInitialize(nil)) then
  begin
    If Succeeded(CoCreateInstance(CLSID_TaskbarList,nil,CLSCTX_INPROC_SERVER,IID_ITaskbarList4,fTaskbarList)) then
      begin
        If fTaskbarList.HrInit <> S_OK then
          begin
            fTaskbarList := nil;  // fTaskbarList.Relase
            CoUninitialize;
          end;
      end
    else CoUninitialize;
  end;
fStateLastResult := S_OK;
fValueLastResult := S_OK;
fProgressState := tpsNoProgress;
fProgressMax := 100;
fProgressValue := 0;
fProgressCoef := 100;
fProgress := 0.0;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.Finalize;
begin
If Assigned(fTaskbarList) then
  begin
    fTaskbarList := nil;  // TaskbarList.Relase
    CoUninitialize;
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.UpdateProgress;
var
  SysProgressState: TBPFLAG;
begin
ObtainWindowHandle;
If Assigned(fTaskbarList) then
  begin
    If fProgressState <> tpsIndeterminate then
      fValueLastResult := fTaskbarList.SetProgressValue(fWindowHandle,fProgressValue,fProgressMax);
    case fProgressState of
      tpsIndeterminate: SysProgressState := TBPF_INDETERMINATE;
      tpsNormal:        SysProgressState := TBPF_NORMAL;
      tpsError:         SysProgressState := TBPF_ERROR;
      tpsPaused:        SysProgressState := TBPF_PAUSED;
    else
     {tpsNoProgress}
      SysProgressState := TBPF_NOPROGRESS;
    end;
    fStateLastResult := fTaskbarList.SetProgressState(fWindowHandle,SysProgressState);
  end;
end;

{-------------------------------------------------------------------------------
    TTaskBarProgress - public functions
-------------------------------------------------------------------------------}

constructor TTaskBarProgress.Create(WindowHandle: HWND = 0);
begin
inherited Create;
fWindowHandle := WindowHandle;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TTaskBarProgress.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TTaskBarProgress.ReinitHandle(WindowHandle: HWND = 0);
begin
fWindowHandle := WindowHandle;
fHandleObtained := False;
end;

{$IFDEF ImplicitTaskbarProgress}
{===============================================================================
    Standalone functions
===============================================================================}

var
  ImplicitTaskbarProgress: TTaskBarProgress = nil;

//------------------------------------------------------------------------------

Function TaskbarProgressActive: Boolean;
begin
If Assigned(ImplicitTaskbarProgress) then
  Result := ImplicitTaskbarProgress.Active
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function SetTaskbarProgressState(State: TTaskbarProgressState): Boolean;
begin
If Assigned(ImplicitTaskbarProgress) then
  begin
    ImplicitTaskbarProgress.ProgressState := State;
    Result := Succeeded(ImplicitTaskbarProgress.StateLastResult);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function SetTaskbarProgressValue(Completed, Total: UInt64): Boolean;
begin
If Assigned(ImplicitTaskbarProgress) then
  begin
    ImplicitTaskbarProgress.ProgressMaxValue := Total;
    ImplicitTaskbarProgress.ProgressValue := Completed;
    Result := Succeeded(ImplicitTaskbarProgress.ValueLastResult);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetTaskbarProgressValue(Value: Double; IntCoef: UInt64 = 1000): Boolean;
begin
If Assigned(ImplicitTaskbarProgress) then
  begin
    ImplicitTaskbarProgress.ProgressCoefficient := IntCoef;
    ImplicitTaskbarProgress.Progress := Value;
    Result := Succeeded(ImplicitTaskbarProgress.ValueLastResult);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

initialization
  ImplicitTaskbarProgress := TTaskBarProgress.Create;

finalization
  FreeAndNil(ImplicitTaskbarProgress);

{$ENDIF}

end.
