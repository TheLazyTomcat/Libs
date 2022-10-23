{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinRawInput
  
    Constants, structures, external functions definitions and macros (here
    implemented as normal functions) used in handling of raw input in Windows
    operating system.

  Version 1.2.2 (2019-10-02)

  Last change 2020-11-12

  ©2016-2019 František Milt  

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.WinRawInput

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes  

===============================================================================}
unit WinRawInput;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
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

interface

uses
  Windows, SysUtils;

{
  Basic types used in Raw Input structures and function parameters.
}
type
  USHORT = Word;
  LONG   = LongInt;
  INT    = Integer;
  HANDLE = THandle;
  QWORD  = UInt64;

  HRAWINPUT = THandle;

{===============================================================================
    Raw Input constants
===============================================================================}
const
{
  Codes of windows messages tied to raw input.
}
  WM_INPUT_DEVICE_CHANGE = $00FE;
  WM_INPUT               = $00FF;

{
  Possible values of wParam in WM_INPUT message.
}
  RIM_INPUT     = 0;
  RIM_INPUTSINK = 1;

{
  Possible values of wParam in WM_INPUT_DEVICE_CHANGE message.
}
  GIDC_ARRIVAL = 1;
  GIDC_REMOVAL = 2;

{
  Values for field RAWINPUTDEVICE.dwFlags.
}
  RIDEV_REMOVE       = $00000001;
  RIDEV_EXCLUDE      = $00000010;
  RIDEV_PAGEONLY     = $00000020;
  RIDEV_NOLEGACY     = $00000030;
  RIDEV_INPUTSINK    = $00000100;
  RIDEV_NOHOTKEYS    = $00000200;
  RIDEV_CAPTUREMOUSE = $00000200;
  RIDEV_APPKEYS      = $00000400;
  RIDEV_EXINPUTSINK  = $00001000;
  RIDEV_DEVNOTIFY    = $00002000;
  RIDEV_EXMODEMASK   = $000000F0;

{
  Values for fields RAWINPUTDEVICELIST.dwType, RAWINPUTHEADER.dwType and
  RID_DEVICE_INFO.dwType.
}
  RIM_TYPEMOUSE    = 0;
  RIM_TYPEKEYBOARD = 1;
  RIM_TYPEHID      = 2;
  RIM_TYPEMAX      = 2;

{
  Values for field RAWMOUSE.usFlags.
}
  MOUSE_MOVE_RELATIVE      = $00;
  MOUSE_MOVE_ABSOLUTE      = $01;
  MOUSE_VIRTUAL_DESKTOP    = $02;
  MOUSE_ATTRIBUTES_CHANGED = $04;
  MOUSE_MOVE_NOCOALESCE    = $08;

{
  Values for field RAWMOUSE.usButtonFlags.
}
  RI_MOUSE_LEFT_BUTTON_DOWN   = $0001;
  RI_MOUSE_LEFT_BUTTON_UP     = $0002;
  RI_MOUSE_RIGHT_BUTTON_DOWN  = $0004;
  RI_MOUSE_RIGHT_BUTTON_UP    = $0008;
  RI_MOUSE_MIDDLE_BUTTON_DOWN = $0010;
  RI_MOUSE_MIDDLE_BUTTON_UP   = $0020;
  RI_MOUSE_BUTTON_1_DOWN      = RI_MOUSE_LEFT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_1_UP        = RI_MOUSE_LEFT_BUTTON_UP;
  RI_MOUSE_BUTTON_2_DOWN      = RI_MOUSE_RIGHT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_2_UP        = RI_MOUSE_RIGHT_BUTTON_UP;
  RI_MOUSE_BUTTON_3_DOWN      = RI_MOUSE_MIDDLE_BUTTON_DOWN;
  RI_MOUSE_BUTTON_3_UP        = RI_MOUSE_MIDDLE_BUTTON_UP;
  RI_MOUSE_BUTTON_4_DOWN      = $0040;
  RI_MOUSE_BUTTON_4_UP        = $0080;
  RI_MOUSE_BUTTON_5_DOWN      = $0100;
  RI_MOUSE_BUTTON_5_UP        = $0200;
  RI_MOUSE_WHEEL              = $0400;
  RI_MOUSE_HWHEEL             = $0800;  // Windows Vista+

{
  Values for field RAWKEYBOARD.Flags.
}
  RI_KEY_MAKE            = 0;
  RI_KEY_BREAK           = 1;
  RI_KEY_E0              = 2;
  RI_KEY_E1              = 4;
  RI_KEY_TERMSRV_SET_LED = 8;
  RI_KEY_TERMSRV_SHADOW  = $10;

{
  Values for parameter uiCommand in function GetRawInputData.
}
  RID_INPUT  = $10000003;
  RID_HEADER = $10000005;

{
  Values for parameter uiCommand in function GetRawInputDeviceInfo.
}
  RIDI_PREPARSEDDATA = $20000005;
  RIDI_DEVICENAME    = $20000007;
  RIDI_DEVICEINFO    = $2000000b;

{
  Other raw input constants.
}
  KEYBOARD_OVERRUN_MAKE_CODE = $FF;


{===============================================================================
    Raw Input structures
===============================================================================}

type
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645565(v=vs.85).aspx
}
  tagRAWINPUTDEVICE = record
    usUsagePage:  USHORT;
    usUsage:      USHORT;
    dwFlags:      DWORD;
    hwndTarget:   HWND;
  end;
  
   RAWINPUTDEVICE = tagRAWINPUTDEVICE;
  TRAWINPUTDEVICE = tagRAWINPUTDEVICE;   
  PRAWINPUTDEVICE = ^TRAWINPUTDEVICE;
 LPRAWINPUTDEVICE = ^TRAWINPUTDEVICE;

  TRAWINPUTDEVICEARRAY = array[0..High(Word)] of TRAWINPUTDEVICE;
  PRAWINPUTDEVICEARRAY = ^TRAWINPUTDEVICEARRAY;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645568(v=vs.85).aspx
}
  tagRAWINPUTDEVICELIST = record
    hDevice:  HANDLE;
    dwType:   DWORD;
  end;

   RAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  TRAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  PRAWINPUTDEVICELIST = ^TRAWINPUTDEVICELIST;

//------------------------------------------------------------------------------  

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645571(v=vs.85).aspx
}
  tagRAWINPUTHEADER = record
    dwType:   DWORD;
    dwSize:   DWORD;
    hDevice:  HANDLE;
    wParam:   WPARAM;
  end;

   RAWINPUTHEADER = tagRAWINPUTHEADER;
  TRAWINPUTHEADER = tagRAWINPUTHEADER;
  PRAWINPUTHEADER = ^TRAWINPUTHEADER;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx
}
  tagRAWMOUSE = record
    usFlags:  USHORT;
    case Integer of
      0:  (ulButtons:     ULONG);
      1:  (usButtonFlags: USHORT;
           usButtonsData: USHORT;
    ulRawButtons:       ULONG;
    lLastX:             LONG;
    lLastY:             LONG;
    ulExtraInformation: ULONG);
  end;

   RAWMOUSE = tagRAWMOUSE;
  TRAWMOUSE = tagRAWMOUSE;
  PRAWMOUSE = ^TRAWMOUSE;
 LPRAWMOUSE = ^TRAWMOUSE;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645575(v=vs.85).aspx
}
  tagRAWKEYBOARD = record
    MakeCode:         USHORT;
    Flags:            USHORT;
    Reserved:         USHORT;
    VKey:             USHORT;
    Message:          UINT;
    ExtraInformation: ULONG;
  end;

   RAWKEYBOARD = tagRAWKEYBOARD;
  TRAWKEYBOARD = tagRAWKEYBOARD;
  PRAWKEYBOARD = ^TRAWKEYBOARD;
 LPRAWKEYBOARD = ^TRAWKEYBOARD;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645549(v=vs.85).aspx
}
  tagRAWHID = record
    dwSizeHid:  DWORD;
    dwCount:    DWORD;
    bRawData:   Byte;   // this is actually a variable-length array of bytes
  end;

   RAWHID = tagRAWHID;
  TRAWHID = tagRAWHID;
  PRAWHID = ^TRAWHID;
 LPRAWHID = ^TRAWHID;

//------------------------------------------------------------------------------
 
{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645562(v=vs.85).aspx
}
  tagRAWINPUT = record
    header: RAWINPUTHEADER;
    case Integer of
      RIM_TYPEMOUSE:   (mouse:     RAWMOUSE);
      RIM_TYPEKEYBOARD:(keyboard:  RAWKEYBOARD);
      RIM_TYPEHID:     (hid:       RAWHID);
  end;
  
   RAWINPUT = tagRAWINPUT;
  TRAWINPUT = tagRAWINPUT;
  PRAWINPUT = ^TRAWINPUT;
 LPRAWINPUT = ^TRAWINPUT;

 PPRAWINPUT = ^PRAWINPUT;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645589(v=vs.85).aspx
}
  tagRID_DEVICE_INFO_MOUSE = record
    dwId:                 DWORD;
    dwNumberOfButtons:    DWORD;
    dwSampleRate:         DWORD;
    fHasHorizontalWheel:  BOOL;   // supported only from Windows Vista up
  end;

   RID_DEVICE_INFO_MOUSE = tagRID_DEVICE_INFO_MOUSE;
  TRID_DEVICE_INFO_MOUSE = tagRID_DEVICE_INFO_MOUSE;
  PRID_DEVICE_INFO_MOUSE = ^TRID_DEVICE_INFO_MOUSE;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645587(v=vs.85).aspx
}
  tagRID_DEVICE_INFO_KEYBOARD = record
    dwType:                 DWORD;
    dwSubType:              DWORD;
    dwKeyboardMode:         DWORD;
    dwNumberOfFunctionKeys: DWORD;
    dwNumberOfIndicators:   DWORD;
    dwNumberOfKeysTotal:    DWORD;
  end;

   RID_DEVICE_INFO_KEYBOARD = tagRID_DEVICE_INFO_KEYBOARD;
  TRID_DEVICE_INFO_KEYBOARD = tagRID_DEVICE_INFO_KEYBOARD;
  PRID_DEVICE_INFO_KEYBOARD = ^TRID_DEVICE_INFO_KEYBOARD;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645584(v=vs.85).aspx
}
  tagRID_DEVICE_INFO_HID = record
    dwVendorId:       DWORD;
    dwProductId:      DWORD;
    dwVersionNumber:  DWORD;
    usUsagePage:      USHORT;
    usUsage:          USHORT;
  end;

   RID_DEVICE_INFO_HID = tagRID_DEVICE_INFO_HID;
  TRID_DEVICE_INFO_HID = tagRID_DEVICE_INFO_HID;
  PRID_DEVICE_INFO_HID = ^TRID_DEVICE_INFO_HID;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645581(v=vs.85).aspx
}
  tagRID_DEVICE_INFO = record
    cbSize: DWORD;
    case dwType: DWORD of
      RIM_TYPEMOUSE:   (mouse:    RID_DEVICE_INFO_MOUSE);
      RIM_TYPEKEYBOARD:(keyboard: RID_DEVICE_INFO_KEYBOARD);
      RIM_TYPEHID:     (hid:      RID_DEVICE_INFO_HID);
  end;

   RID_DEVICE_INFO = tagRID_DEVICE_INFO;
  TRID_DEVICE_INFO = tagRID_DEVICE_INFO;
  PRID_DEVICE_INFO = ^TRID_DEVICE_INFO;
 LPRID_DEVICE_INFO = ^TRID_DEVICE_INFO;  

{===============================================================================
    Raw Input functions
===============================================================================}

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645594(v=vs.85).aspx
}
Function DefRawInputProc(
            paRawInput:   PPRAWINPUT;
            nInput:       INT;
            cbSizeHeader: UINT): LRESULT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645595(v=vs.85).aspx

  There are issues with memory alignment, see linked description for details.
}
Function GetRawInputBuffer(
            pData:        PRAWINPUT;
            pcbSize:      PUINT; 
            cbSizeHeader: UINT): UINT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645596(v=vs.85).aspx
}
Function GetRawInputData(
            hRawInput:    HRAWINPUT;
            uiCommand:    UINT;
            pData:        Pointer;  // must be aligned by 8 bytes on Win64
            pcbSize:      PUINT;
            cbSizeHeader: UINT): UINT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645597(v=vs.85).aspx
}
Function GetRawInputDeviceInfo(
            hDevice:    THandle;
            uiCommand:  UINT;
            pData:      Pointer;
            pcbSize:    PUINT): UINT; stdcall; external user32 name{$IFDEF UNICODE}'GetRawInputDeviceInfoW'{$ELSE}'GetRawInputDeviceInfoA'{$ENDIF};

Function GetRawInputDeviceInfoA(
            hDevice:    THandle;
            uiCommand:  UINT;
            pData:      Pointer;
            pcbSize:    PUINT): UINT; stdcall; external user32;

Function GetRawInputDeviceInfoW(
            hDevice:    THandle;
            uiCommand:  UINT;
            pData:      Pointer;
            pcbSize:    PUINT): UINT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645598(v=vs.85).aspx
}
Function GetRawInputDeviceList(
            pRawInputDeviceLis: PRAWINPUTDEVICELIST;
            puiNumDevices:      PUINT;
            cbSize:             UINT): UINT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645599(v=vs.85).aspx
}
Function GetRegisteredRawInputDevices(
            pRawInputDevices: PRAWINPUTDEVICE;
            puiNumDevices:    PUINT;
            cbSize:           UINT): UINT; stdcall; external user32;

//------------------------------------------------------------------------------

{
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms645600(v=vs.85).aspx
}
Function RegisterRawInputDevices(
            pRawInputDevices: PRAWINPUTDEVICE;
            uiNumDevices:     UINT;
            cbSize:           UINT): BOOL; stdcall; external user32;

{===============================================================================
    Raw Input macros
===============================================================================}

Function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): WPARAM;{$IFDEF CanInline} inline; {$ENDIF}
Function RAWINPUT_ALIGN(x: Pointer): Pointer;{$IFDEF CanInline} inline; {$ENDIF}
Function NEXTRAWINPUTBLOCK(ptr: PRAWINPUT): PRAWINPUT;{$IFDEF CanInline} inline; {$ENDIF}
Function RIDEV_EXMODE(Mode: DWORD): DWORD;{$IFDEF CanInline} inline; {$ENDIF}
Function GET_DEVICE_CHANGE_WPARAM(wParam: wParam): wParam;{$IFDEF CanInline} inline; {$ENDIF}
Function GET_DEVICE_CHANGE_LPARAM(lParam: lParam): lParam;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
    Auxiliary types and functions
===============================================================================}

type
{
  Structure that is designed to be used to access data returned by function
  GetRawInputBuffer in WoW64 (data structures in the array returned by this
  function have different memory alignment of fields in WoW64 then they have
  in native 32bit OS).
}
  TRawInputWoW64 = record
    header:   RAWINPUTHEADER;
    padding:  Int64;
    case Integer of
      RIM_TYPEMOUSE:   (mouse:     RAWMOUSE);
      RIM_TYPEKEYBOARD:(keyboard:  RAWKEYBOARD);
      RIM_TYPEHID:     (hid:       RAWHID);
  end;
  PRawInputWoW64 = ^TRawInputWoW64;

//------------------------------------------------------------------------------

{
  Converts RAWINPUT structure that have WoW64 memory alignment to normal 32bit
  structure.

  Cannot convert values that contain HID data (field bRawData). For variables
  containing large amount of HID data, use overload that accepts general
  pointer and does in-place conversion.

  Returns true when conversion was successfully completed, false otherwise
  (eg. when RawInput contains too much of HID data) - in that case the
  converted structure is filled with zeroes.
}
Function ConvertFromWoW64(RawInput: TRawInputWoW64; out Converted: RAWINPUT): Boolean; overload;

//------------------------------------------------------------------------------

{
  Converts RAWINPUT structure that have WoW64 memory alignment to normal 32bit
  structure.

  Cannot convert values that contain HID data (field bRawData).

  If the conversion cannot be performed, the result is filled with zeroes.
}
Function ConvertFromWoW64(RawInput: TRawInputWoW64): RAWINPUT; overload;

//------------------------------------------------------------------------------

{
  Performs in-place conversion from RAWINPUT structure with WoW64 memory
  aligment to normal 32bit structure - it shifts data (that is, field
  mouse/keyboard/hid) down by 8 bytes.

  When ChangeSize is true, the size stored in structure's header is decreased
  by 8, otherwise it is not changed.
  Do not change stored size if you want to pass the pointer to NEXTRAWINPUTBLOCK
  macro function.
  
  Data parameter MUST point to the start of TRawInputWoW64 structure.
}
procedure ConvertFromWoW64(Data: Pointer; ChangeSize: Boolean = False); overload;

//------------------------------------------------------------------------------

{
  Indicates whether current process is running under WoW64.

  For 64bit processes, it just returns false. For 32bit processes, it returns
  true when IsWow64Process funtion is successfully loaded and it indicates the
  process is running under WoW64, false otherwise.
}
Function IsWoW64: Boolean;

type
  EWRIException = class(Exception);

implementation

uses
  AuxTypes;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

//------------------------------------------------------------------------------

Function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): WPARAM;
begin
Result := wParam and $FF;
end;

//------------------------------------------------------------------------------

Function RAWINPUT_ALIGN(x: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF CPU64bit}
Result := Pointer((PtrUInt(x) + (SizeOf(QWORD) - 1)) and not (SizeOf(QWORD) - 1));
{$ELSE}
Result := Pointer((PtrUInt(x) + (SizeOf(DWORD) - 1)) and not (SizeOf(DWORD) - 1));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function NEXTRAWINPUTBLOCK(ptr: PRAWINPUT): PRAWINPUT;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PRAWINPUT(RAWINPUT_ALIGN(Pointer(PtrUInt(ptr) + ptr^.header.dwSize)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function RIDEV_EXMODE(Mode: DWORD): DWORD;
begin
Result := Mode and RIDEV_EXMODEMASK;
end;

//------------------------------------------------------------------------------

Function GET_DEVICE_CHANGE_WPARAM(wParam: wParam): wParam;
begin
Result := LoWord(wParam);
end;

//------------------------------------------------------------------------------

Function GET_DEVICE_CHANGE_LPARAM(lParam: lParam): lParam;
begin
Result := LoWord(lParam);
end;

//==============================================================================

Function ConvertFromWoW64(RawInput: TRawInputWoW64; out Converted: RAWINPUT): Boolean;
begin
FillChar(Addr(Converted)^,SizeOf(Converted),0);
If RawInput.header.dwSize <= SizeOf(RAWINPUT) then
  begin
    Converted.header := RawInput.header;
    Result := True;
    case RawInput.header.dwType of
      RIM_TYPEMOUSE:    Converted.mouse := RawInput.mouse;
      RIM_TYPEKEYBOARD: Converted.keyboard := RawInput.keyboard;
      RIM_TYPEHID:      Converted.hid := RawInput.hid;
    else
      Result := False;
    end;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConvertFromWoW64(RawInput: TRawInputWoW64): RAWINPUT;
begin
ConvertFromWoW64(RawInput,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ConvertFromWoW64(Data: Pointer; ChangeSize: Boolean = False);
var
  DataOffset: PtrUInt;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
DataOffset := PtrUInt(Addr(TRawInputWoW64(nil^).mouse));
Move(Pointer(PtrUInt(Data) + DataOffset)^,Addr(PRawInput(Data)^.mouse)^,TMemSize(PRawInput(Data)^.header.dwSize) - TMemSize(DataOffset));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
If ChangeSize then
  Dec(PRawInput(Data)^.header.dwSize,8);
end;

//------------------------------------------------------------------------------

Function IsWoW64: Boolean;
{$IFDEF CPU32bit}
type     
  TIsWoW64Process = Function(hProcess: THandle; Wow64Process: PBOOL): BOOL; stdcall;
var
  ModuleHandle:   THandle;
  IsWoW64Process: TIsWoW64Process;
  ResultValue:    BOOL;
begin
Result := False;
ModuleHandle := GetModuleHandle('kernel32.dll');  // do not FreeLibrary
If ModuleHandle <> 0 then
  begin
    IsWoW64Process := TIsWoW64Process(GetProcAddress(ModuleHandle,'IsWow64Process'));
    If Assigned(IsWoW64Process) then
      If IsWoW64Process(GetCurrentProcess,@ResultValue) then
        Result := ResultValue;
  end
else raise EWRIException.CreateFmt('Unable to get handle to module kernel32.dll (%.8x).',[GetLastError]);
end;
{$ELSE}
begin
Result := False;  // 64bit cannot run under wow64
end;
{$ENDIF}

end.
