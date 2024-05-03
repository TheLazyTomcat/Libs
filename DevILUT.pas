{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  DevILUT - header file for the ImageLib utility toolkit

    Direct translation of C header file ilut.h, a part of bindings for DevIL
    library, into pascal.

    More info about the DevIL library can be found at:

      https://openil.sourceforge.net

    Translation notes:

      - macros were expanded in-place or implemented as normal functions
      - some function parameters and structure fields were renamed (usually by
        prepending the name with "a") because they collide with pascal reserved
        words (eg. Type, File, ...)
      - most inline comments present in header files were copied into the
        translation at corresponding place
      - DevILU requires DevIL (DevIL.dll) to be already loaded and initialized,
        similarly DevILUT require both DevIL and DevILU to be already loaded
        and initialized
      - if you plan to use unicode version of the api, remember to also use
        proper linked libraries (DLLs) builds
      - DevILUT provides only GDI and OpenGL interfaces, as these are the only
        ones exported by provided binaries (DLLs)
      - some helper function are provided for conversions between usual pascal
        types and types used in the API (booleans, strings)
      - current translation is for Windows OS only (Linux is planned)

    WARNING - the DevIL library is, as far as I know, NOT thread safe. You can
              use it in any number of threads, but make sure it will never
              execute any of its code concurrently (in more than one thread at
              any given time).

  Version 1.0.1 (2024-05-03)

  Build against DevIL library version 1.8.0

  Last change 2024-05-03

  ©2023-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.DevIL

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    DynLibUtils   - github.com/TheLazyTomcat/Lib.DynLibUtils
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol DevIL_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils    - github.com/TheLazyTomcat/Lib.UInt64Utils
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    WinFileInfo    - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit DevILUT;

{$INCLUDE '.\DevIL_defs.inc'}

interface

uses
  {$IFDEF DevILUT_UseOpenGL}{$IFDEF FPC}GL,{$ELSE}OpenGL,{$ENDIF}{$ENDIF}
  {$IFDEF DevILUT_UseWin32}Windows,{$ENDIF}
  DevIL;

{===============================================================================
    Constants
===============================================================================}
const
  ILUT_VERSION = 180;
  

  // Attribute Bits
  ILUT_OPENGL_BIT      = $00000001;
  ILUT_D3D_BIT         = $00000002;
  ILUT_ALL_ATTRIB_BITS = $000FFFFF;


  // Error Types
  ILUT_INVALID_ENUM        = $0501;
  ILUT_OUT_OF_MEMORY       = $0502;
  ILUT_INVALID_VALUE       = $0505;
  ILUT_ILLEGAL_OPERATION   = $0506;
  ILUT_INVALID_PARAM       = $0509;
  ILUT_COULD_NOT_OPEN_FILE = $050A;
  ILUT_STACK_OVERFLOW      = $050E;
  ILUT_STACK_UNDERFLOW     = $050F;
  ILUT_BAD_DIMENSIONS      = $0511;
  ILUT_NOT_SUPPORTED       = $0550;


  // State Definitions
  ILUT_PALETTE_MODE         = $0600;
  ILUT_OPENGL_CONV          = $0610;
  ILUT_D3D_MIPLEVELS        = $0620;
  ILUT_MAXTEX_WIDTH         = $0630;
  ILUT_MAXTEX_HEIGHT        = $0631;
  ILUT_MAXTEX_DEPTH         = $0632;
  ILUT_GL_USE_S3TC          = $0634;
  ILUT_D3D_USE_DXTC         = $0634;
  ILUT_GL_GEN_S3TC          = $0635;
  ILUT_D3D_GEN_DXTC         = $0635;
  ILUT_S3TC_FORMAT          = $0705;
  ILUT_DXTC_FORMAT          = $0705;
  ILUT_D3D_POOL             = $0706;
  ILUT_D3D_ALPHA_KEY_COLOR  = $0707;
  ILUT_D3D_ALPHA_KEY_COLOUR = $0707;
  ILUT_FORCE_INTEGER_FORMAT = $0636;

  //This new state does automatic texture target detection
  //if enabled. Currently, only cubemap detection is supported.
  //if the current image is no cubemap, the 2d texture is chosen.
  ILUT_GL_AUTODETECT_TEXTURE_TARGET = $0807;


  // Values
  ILUT_VERSION_NUM = IL_VERSION_NUM;
  ILUT_VENDOR      = IL_VENDOR;

  // The different rendering api's...more to be added later?
  ILUT_OPENGL     = 0;
  ILUT_ALLEGRO    = 1;
  ILUT_WIN32      = 2;
  ILUT_DIRECT3D8  = 3;
  ILUT_DIRECT3D9  = 4;
  ILUT_X11        = 5;
  ILUT_DIRECT3D10 = 6;


{===============================================================================
    API functions (as procedural variables)
===============================================================================}
var
  // ImageLib Utility Toolkit Functions
  ilutDisable:     Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilutEnable:      Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilutGetBoolean:  Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilutGetBooleanv: procedure(Mode: ILenum; Param: ILboolean_p); stdcall = nil;
  ilutGetInteger:  Function(Mode: ILenum): ILint; stdcall = nil;
  ilutGetIntegerv: procedure(Mode: ILenum; Param: ILint_p); stdcall = nil;
  ilutGetString:   Function(StringName: ILenum): ILstring; stdcall = nil;
  ilutInit:        procedure; stdcall = nil;
  ilutIsDisabled:  Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilutIsEnabled:   Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilutPopAttrib:   procedure; stdcall = nil;
  ilutPushAttrib:  procedure(Bits: ILuint); stdcall = nil;
  ilutSetInteger:  procedure(Mode: ILenum; Param: ILint); stdcall = nil;
  ilutRenderer:    Function(Renderer: ILenum): ILboolean; stdcall = nil;

{$IFDEF DevILUT_UseOpenGL}
  // ImageLib Utility Toolkit's OpenGL Functions
  ilutGLBindTexImage: Function: GLuint; stdcall = nil;
  ilutGLBindMipmaps:  Function: GLuint; stdcall = nil;
  ilutGLBuildMipmaps: Function: ILboolean; stdcall = nil;
  ilutGLLoadImage:    Function(FileName: ILstring): GLuint; stdcall = nil;
  ilutGLScreen:       Function: ILboolean; stdcall = nil;
  ilutGLScreenie:     Function: ILboolean; stdcall = nil;
  ilutGLSaveImage:    Function(FileName: ILstring; TexID: GLuint): ILboolean; stdcall = nil;
  ilutGLSubTex2D:     Function(TexID: GLuint; XOff,YOff: ILuint): ILboolean; stdcall = nil;
  ilutGLSubTex3D:     Function(TexID: GLuint; XOff,YOff,ZOff: ILuint): ILboolean; stdcall = nil;
  ilutGLSetTex2D:     Function(TexID: GLuint): ILboolean; stdcall = nil;
  ilutGLSetTex3D:     Function(TexID: GLuint): ILboolean; stdcall = nil;
  ilutGLTexImage:     Function(Level: GLuint): ILboolean; stdcall = nil;
  ilutGLSetTex:       Function(TexID: GLuint): ILboolean; stdcall = nil;                    // Deprecated - use ilutGLSetTex2D.
  ilutGLSubTex:       Function(TexID: GLuint; XOff,YOff: ILuint): ILboolean; stdcall = nil; // Use ilutGLSubTex2D.
{$ENDIF}

{$IFDEF DevILUT_UseWin32}
  // ImageLib Utility Toolkit's Win32 GDI Functions
  ilutConvertToHBitmap:      Function(hDC: HDC): HBITMAP; stdcall = nil;
  ilutConvertSliceToHBitmap: Function(hDC: HDC; slice: ILuint): HBITMAP; stdcall = nil;
  ilutFreePaddedData:        procedure(Data: ILubyte_p); stdcall = nil;
  ilutGetBmpInfo:            procedure(Info: PBITMAPINFO); stdcall = nil;
  ilutGetHPal:               Function: HPALETTE; stdcall = nil;
  ilutGetPaddedData:         Function: ILubyte_p; stdcall = nil;
  ilutGetWinClipboard:       Function: ILboolean; stdcall = nil;
  ilutLoadResource:          Function(hInst: HINST; ID: ILint; ResourceType: ILstring; aType: ILenum): ILboolean; stdcall = nil;
  ilutSetHBitmap:            Function(Bitmap: HBITMAP): ILboolean; stdcall = nil;
  ilutSetHPal:               Function(Pal: HPALETTE): ILboolean; stdcall = nil;
  ilutSetWinClipboard:       Function: ILboolean; stdcall = nil;
  ilutWinLoadImage:          Function(FileName: ILstring; hDC: HDC): HBITMAP; stdcall = nil;
  ilutWinLoadUrl:            Function(Url: ILstring): ILboolean; stdcall = nil;
  ilutWinPrint:              Function(XPos,YPos,Width,Height: ILuint; hDC: HDC): ILboolean; stdcall = nil;
  ilutWinSaveImage:          Function(FileName: ILstring; Bitmap: HBITMAP): ILboolean; stdcall = nil;
{$ENDIF}

{===============================================================================
    Library loading - declaration
===============================================================================}
const
  DevILUT_LibFileName = 'ILUT.dll';

Function DevILUT_Initialized: Boolean;
Function DevILUT_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
Function DevILUT_InitializeWithRenderer(const LibPath: String = DevIL_LibFileName;
  Renderer: ILenum = {$IFDEF Windows}ILUT_WIN32{$ELSE}ILUT_OPENGL{$ENDIF}): Boolean;
procedure DevILUT_Finalize(FinalLib: Boolean = True);

implementation

uses
  DynLibUtils;

{===============================================================================
    Library loading - implementation
===============================================================================}
var
  DevILUT_LibraryHandle: TDLULibraryHandle = DefaultLibraryHandle;

//------------------------------------------------------------------------------

Function DevILUT_Initialized: Boolean;
begin
Result := CheckLibrary(DevILUT_LibraryHandle);
end;

//------------------------------------------------------------------------------

Function DevILUT_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
begin
Result := OpenLibraryAndResolveSymbols(LibPath,DevILUT_LibraryHandle,[
  Symbol(@@ilutDisable,    'ilutDisable'),
  Symbol(@@ilutEnable,     'ilutEnable'),
  Symbol(@@ilutGetBoolean, 'ilutGetBoolean'),
  Symbol(@@ilutGetBooleanv,'ilutGetBooleanv'),
  Symbol(@@ilutGetInteger, 'ilutGetInteger'),
  Symbol(@@ilutGetIntegerv,'ilutGetIntegerv'),
  Symbol(@@ilutGetString,  'ilutGetString'),
  Symbol(@@ilutInit,       'ilutInit'),
  Symbol(@@ilutIsDisabled, 'ilutIsDisabled'),
  Symbol(@@ilutIsEnabled,  'ilutIsEnabled'),
  Symbol(@@ilutPopAttrib,  'ilutPopAttrib'),
  Symbol(@@ilutPushAttrib, 'ilutPushAttrib'),
  Symbol(@@ilutSetInteger, 'ilutSetInteger'),
  Symbol(@@ilutRenderer,   'ilutRenderer')
{$IFDEF DevILUT_UseOpenGL},
  Symbol(@@ilutGLBindTexImage,'ilutGLBindTexImage'),
  Symbol(@@ilutGLBindMipmaps, 'ilutGLBindMipmaps'),
  Symbol(@@ilutGLBuildMipmaps,'ilutGLBuildMipmaps'),
  Symbol(@@ilutGLLoadImage,   'ilutGLLoadImage'),
  Symbol(@@ilutGLScreen,      'ilutGLScreen'),
  Symbol(@@ilutGLScreenie,    'ilutGLScreenie'),
  Symbol(@@ilutGLSaveImage,   'ilutGLSaveImage'),
{$IF Defined(Windows) and Defined(x86)}
  Symbol(@@ilutGLSubTex2D,    '_ilutGLSubTex2D@12'),
  Symbol(@@ilutGLSubTex3D,    '_ilutGLSubTex3D@16'),
  Symbol(@@ilutGLSetTex2D,    '_ilutGLSetTex2D@4'),
  Symbol(@@ilutGLSetTex3D,    '_ilutGLSetTex3D@4'),
{$ELSE}
  Symbol(@@ilutGLSubTex2D,    'ilutGLSubTex2D'),
  Symbol(@@ilutGLSubTex3D,    'ilutGLSubTex3D'),
  Symbol(@@ilutGLSetTex2D,    'ilutGLSetTex2D'),
  Symbol(@@ilutGLSetTex3D,    'ilutGLSetTex3D'),
{$IFEND}
  Symbol(@@ilutGLTexImage,    'ilutGLTexImage'),
  Symbol(@@ilutGLSetTex,      'ilutGLSetTex'),
  Symbol(@@ilutGLSubTex,      'ilutGLSubTex')
{$ENDIF}
{$IFDEF DevILUT_UseWin32},
  Symbol(@@ilutConvertToHBitmap,     'ilutConvertToHBitmap'),
  Symbol(@@ilutConvertSliceToHBitmap,'ilutConvertSliceToHBitmap'),
  Symbol(@@ilutFreePaddedData,       'ilutFreePaddedData'),
  Symbol(@@ilutGetBmpInfo,           'ilutGetBmpInfo'),
  Symbol(@@ilutGetHPal,              'ilutGetHPal'),
  Symbol(@@ilutGetPaddedData,        'ilutGetPaddedData'),
  Symbol(@@ilutGetWinClipboard,      'ilutGetWinClipboard'),
  Symbol(@@ilutLoadResource,         'ilutLoadResource'),
  Symbol(@@ilutSetHBitmap,           'ilutSetHBitmap'),
  Symbol(@@ilutSetHPal,              'ilutSetHPal'),
  Symbol(@@ilutSetWinClipboard,      'ilutSetWinClipboard'),
  Symbol(@@ilutWinLoadImage,         'ilutWinLoadImage'),
  Symbol(@@ilutWinLoadUrl,           'ilutWinLoadUrl'),
  Symbol(@@ilutWinPrint,             'ilutWinPrint'),
  Symbol(@@ilutWinSaveImage,         'ilutWinSaveImage')  
{$ENDIF}
],True) = 14 {$IFDEF DevILUT_UseOpenGL}+14{$ENDIF}{$IFDEF DevILUT_UseWin32}+15{$ENDIF};
// init
If Result and InitLib then
  ilutInit;
end;

//------------------------------------------------------------------------------

Function DevILUT_InitializeWithRenderer(const LibPath: String = DevIL_LibFileName;
  Renderer: ILenum = {$IFDEF Windows}ILUT_WIN32{$ELSE}ILUT_OPENGL{$ENDIF}): Boolean;
begin
If DevILUT_Initialize(LibPath,True) then
  Result := ILBoolDecode(ilutRenderer(Renderer))
else
  Result := False;
end;

//------------------------------------------------------------------------------

procedure DevILUT_Finalize(FinalLib: Boolean = True);
begin
If FinalLib then; // do nothing
CloseLibrary(DevILUT_LibraryHandle);
end;

end.
