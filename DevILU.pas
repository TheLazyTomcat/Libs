{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  DevILU - header file for the ImageLib utility

    Direct translation of C header file ilu.h, a part of bindings for DevIL
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

  Version 1.0 (2023-05-27)

  Build against DevIL library version 1.8.0

  Last change 2023-05-27

  ©2023 František Milt

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
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

===============================================================================}
unit DevILU;

{$INCLUDE '.\DevIL_defs.inc'}

interface

uses
  DevIL;

{===============================================================================
    Constants
===============================================================================}
const
  ILU_VERSION = 180;


  ILU_FILTER         = $2600;
  ILU_NEAREST        = $2601;
  ILU_LINEAR         = $2602;
  ILU_BILINEAR       = $2603;
  ILU_SCALE_BOX      = $2604;
  ILU_SCALE_TRIANGLE = $2605;
  ILU_SCALE_BELL     = $2606;
  ILU_SCALE_BSPLINE  = $2607;
  ILU_SCALE_LANCZOS3 = $2608;
  ILU_SCALE_MITCHELL = $2609;


  // Error types
  ILU_INVALID_ENUM      = $0501;
  ILU_OUT_OF_MEMORY     = $0502;
  ILU_INTERNAL_ERROR    = $0504;
  ILU_INVALID_VALUE     = $0505;
  ILU_ILLEGAL_OPERATION = $0506;
  ILU_INVALID_PARAM     = $0509;


  // Values
  ILU_PLACEMENT          = $0700;
  ILU_LOWER_LEFT         = $0701;
  ILU_LOWER_RIGHT        = $0702;
  ILU_UPPER_LEFT         = $0703;
  ILU_UPPER_RIGHT        = $0704;
  ILU_CENTER             = $0705;
  ILU_CONVOLUTION_MATRIX = $0710;

  ILU_VERSION_NUM = IL_VERSION_NUM;
  ILU_VENDOR      = IL_VENDOR;


  // Languages
  ILU_ENGLISH            = $0800;
  ILU_ARABIC             = $0801;
  ILU_DUTCH              = $0802;
  ILU_JAPANESE           = $0803;
  ILU_SPANISH            = $0804;
  ILU_GERMAN             = $0805;
  ILU_FRENCH             = $0806;
  ILU_ITALIAN            = $0807;


{
  Following constants are commented-out in the original header file too. They
  seem to be not used anywhere (maybe a relic from earlier version?).
}
  // Filters
(*
  ILU_FILTER_BLUR         = $0803;
  ILU_FILTER_GAUSSIAN_3x3 = $0804;
  ILU_FILTER_GAUSSIAN_5X5 = $0805;
  ILU_FILTER_EMBOSS1      = $0807;
  ILU_FILTER_EMBOSS2      = $0808;
  ILU_FILTER_LAPLACIAN1   = $080A;
  ILU_FILTER_LAPLACIAN2   = $080B;
  ILU_FILTER_LAPLACIAN3   = $080C;
  ILU_FILTER_LAPLACIAN4   = $080D;
  ILU_FILTER_SHARPEN1     = $080E;
  ILU_FILTER_SHARPEN2     = $080F;
  ILU_FILTER_SHARPEN3     = $0810;
*)

{===============================================================================
    Types
===============================================================================}
type
  ILinfo = record
    Id:         ILuint;     // the image's id
    Data:       ILubyte_p;  // the image's data
    Width:      ILuint;     // the image's width
    Height:     ILuint;     // the image's height
    Depth:      ILuint;     // the image's depth
    Bpp:        ILubyte;    // bytes per pixel (not bits) of the image
    SizeOfData: ILuint;     // the total size of the data (in bytes)
    Format:     ILenum;     // image format (in IL enum style)
    aType:      ILenum;     // image type (in IL enum style)
    Origin:     ILenum;     // origin of the image
    Palette:    ILubyte_p;  // the image's palette
    PalType:    ILenum;     // palette type
    PalSize:    ILuint;     // palette size
    CubeFlags:  ILenum;     // flags for what cube map sides are present
    NumNext:    ILuint;     // number of images following
    NumMips:    ILuint;     // number of mipmaps
    NumLayers:  ILuint;     // number of layers
  end;
  ILinfo_p = ^ILinfo;

  ILpointf = record
    x:  ILfloat;
    y:  ILfloat;
  end;
  ILpointf_p = ^ILpointf;

  ILpointi = record
    x:  ILint;
    y:  ILint;
  end;
  ILpointi_p = ^ILpointi;

{===============================================================================
    API functions (as procedural variables)
===============================================================================}
var 
  iluAlienify:       Function: ILboolean; stdcall = nil;
  iluBlurAvg:        Function(Iter: ILuint): ILboolean; stdcall = nil;
  iluBlurGaussian:   Function(Iter: ILuint): ILboolean; stdcall = nil;
  iluBuildMipmaps:   Function: ILboolean; stdcall = nil;
  iluColoursUsed:    Function: ILuint; stdcall = nil;
  iluCompareImage:   Function(Comp: ILuint): ILboolean; stdcall = nil;
  iluContrast:       Function(Contrast: ILfloat): ILboolean; stdcall = nil;
  iluCrop:           Function(XOff,YOff,ZOff,Width,Height,Depth: ILuint): ILboolean; stdcall = nil;
  iluDeleteImage:    procedure(Id: ILuint); stdcall = nil;                      // Deprecated (use ilDeleteImage)
  iluEdgeDetectE:    Function: ILboolean; stdcall = nil;
  iluEdgeDetectP:    Function: ILboolean; stdcall = nil;
  iluEdgeDetectS:    Function: ILboolean; stdcall = nil;
  iluEmboss:         Function: ILboolean; stdcall = nil;
  iluEnlargeCanvas:  Function(Width,Height,Depth: ILuint): ILboolean; stdcall = nil;
  iluEnlargeImage:   Function(XDim,YDim,ZDim: ILfloat): ILboolean; stdcall = nil;
  iluEqualize:       Function: ILboolean; stdcall = nil;                        // sometimes produces floating point error (invalid operation)
  iluEqualize2:      Function: ILboolean; stdcall = nil;
  iluErrorString:    Function(Error: ILenum): ILconst_string; stdcall = nil;
  iluConvolution:    Function(matrix: ILint_p; scale,bias: ILint): ILboolean; stdcall = nil;
  iluFlipImage:      Function: ILboolean; stdcall = nil;                        // vertical mirror
  iluGammaCorrect:   Function(Gamma: ILfloat): ILboolean; stdcall = nil;
  iluGenImage:       Function: ILuint; stdcall = nil;                           // Deprecated (use ilGenImage)
  iluGetImageInfo:   procedure(Info: ILinfo_p); stdcall = nil;
  iluGetInteger:     Function(Mode: ILenum): ILuint; stdcall = nil;
  iluGetIntegerv:    procedure(Mode: ILenum; Param: ILint_p); stdcall = nil;
  iluGetString:      Function(StringName: ILenum): ILstring; stdcall = nil;
  iluImageParameter: procedure(PName,Param: ILenum); stdcall = nil;
  iluInit:           procedure; stdcall = nil;
  iluInvertAlpha:    Function: ILboolean; stdcall = nil;
  iluLoadImage:      Function(FileName: ILconst_string): ILuint; stdcall = nil;
  iluMirror:         Function: ILboolean; stdcall = nil;                        // horizontal mirror
  iluNegative:       Function: ILboolean; stdcall = nil;
  iluNoisify:        Function(Tolerance: ILclampf): ILboolean; stdcall = nil;
  iluPixelize:       Function(PixSize: ILuint): ILboolean; stdcall = nil;
  iluRegionfv:       procedure(Points: ILpointf_p; n: ILuint); stdcall = nil;
  iluRegioniv:       procedure(Points: ILpointi_p; n: ILuint); stdcall = nil;
  iluReplaceColour:  Function(Red,Green,Blue: ILubyte; Tolerance: ILfloat): ILboolean; stdcall = nil;
  iluRotate:         Function(Angle: ILfloat): ILboolean; stdcall = nil;
  iluRotate3D:       Function(x,y,z,Angle: ILfloat): ILboolean; stdcall = nil;  // not implemented
  iluSaturate1f:     Function(Saturation: ILfloat): ILboolean; stdcall = nil;
  iluSaturate4f:     Function(r,g,b,Saturation: ILfloat): ILboolean; stdcall = nil;
  iluScale:          Function(Width,Height,Depth: ILuint): ILboolean; stdcall = nil;
  iluScaleAlpha:     Function(scale: ILfloat): ILboolean; stdcall = nil;
  iluScaleColours:   Function(r,g,b: ILfloat): ILboolean; stdcall = nil;
  iluSepia:          Function: ILboolean; stdcall = nil;
  iluSetLanguage:    Function(Language: ILenum): ILboolean; stdcall = nil;
  iluSharpen:        Function(Factor: ILfloat; Iter: ILuint): ILboolean; stdcall = nil;
{
  iluSwapColours does not work (data are not changed) and also produces access
  violation if internal variable iluCurImage was not set by a previous call(s).
}
  iluSwapColours:    Function: ILboolean; stdcall = nil;
  iluWave:           Function(Angle: ILfloat): ILboolean; stdcall = nil;

  iluColorsUsed:     Function: ILuint; stdcall = nil;
  iluSwapColors:     Function: ILboolean; stdcall = nil;
  iluReplaceColor:   Function(Red,Green,Blue: ILubyte; Tolerance: ILfloat): ILboolean; stdcall = nil;
{
  In the original header, the following function is declared as:

    #define iluScaleColor   iluScaleColour

  So as an alias for iluScaleColour, which afaik does not exist (note the
  missing "s" at the end).
  I am assuming this is an error, and it should look this way:

    #define iluScaleColors  iluScaleColours
}
  iluScaleColors:    Function(r,g,b: ILfloat): ILboolean; stdcall = nil;

{===============================================================================
    Library loading - declaration
===============================================================================}
const
  DevILU_LibFileName = 'ILU.dll';

Function DevILU_Initialized: Boolean;
Function DevILU_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
procedure DevILU_Finalize(FinalLib: Boolean = True);

implementation

uses
  DynLibUtils;

{===============================================================================
    Library loading - implementation
===============================================================================}
var
  DevILU_LibraryHandle: TDLULibraryHandle = DefaultLibraryHandle;

//------------------------------------------------------------------------------

Function DevILU_Initialized: Boolean;
begin
Result := CheckLibrary(DevILU_LibraryHandle);
end;

//------------------------------------------------------------------------------

Function DevILU_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
begin
Result := OpenLibraryAndResolveSymbols(LibPath,DevILU_LibraryHandle,[
  Symbol(@@iluAlienify,      'iluAlienify'),
  Symbol(@@iluBlurAvg,       'iluBlurAvg'),
  Symbol(@@iluBlurGaussian,  'iluBlurGaussian'),
  Symbol(@@iluBuildMipmaps,  'iluBuildMipmaps'),
  Symbol(@@iluColoursUsed,   'iluColoursUsed'),
  Symbol(@@iluCompareImage,  'iluCompareImage'),
  Symbol(@@iluContrast,      'iluContrast'),
  Symbol(@@iluCrop,          'iluCrop'),
  Symbol(@@iluDeleteImage,   'iluDeleteImage'),
  Symbol(@@iluEdgeDetectE,   'iluEdgeDetectE'),
  Symbol(@@iluEdgeDetectP,   'iluEdgeDetectP'),
  Symbol(@@iluEdgeDetectS,   'iluEdgeDetectS'),
  Symbol(@@iluEmboss,        'iluEmboss'),
  Symbol(@@iluEnlargeCanvas, 'iluEnlargeCanvas'),
  Symbol(@@iluEnlargeImage,  'iluEnlargeImage'),
  Symbol(@@iluEqualize,      'iluEqualize'),
  Symbol(@@iluEqualize2,     'iluEqualize2'),
  Symbol(@@iluErrorString,   'iluErrorString'),
  Symbol(@@iluConvolution,   'iluConvolution'),
  Symbol(@@iluFlipImage,     'iluFlipImage'),
  Symbol(@@iluGammaCorrect,  'iluGammaCorrect'),
  Symbol(@@iluGenImage,      'iluGenImage'),
  Symbol(@@iluGetImageInfo,  'iluGetImageInfo'),
  Symbol(@@iluGetInteger,    'iluGetInteger'),
  Symbol(@@iluGetIntegerv,   'iluGetIntegerv'),
  Symbol(@@iluGetString,     'iluGetString'),
  Symbol(@@iluImageParameter,'iluImageParameter'),
  Symbol(@@iluInit,          'iluInit'),
  Symbol(@@iluInvertAlpha,   'iluInvertAlpha'),
  Symbol(@@iluLoadImage,     'iluLoadImage'),
  Symbol(@@iluMirror,        'iluMirror'),
  Symbol(@@iluNegative,      'iluNegative'),
  Symbol(@@iluNoisify,       'iluNoisify'),
  Symbol(@@iluPixelize,      'iluPixelize'),
  Symbol(@@iluRegionfv,      'iluRegionfv'),
  Symbol(@@iluRegioniv,      'iluRegioniv'),
  Symbol(@@iluReplaceColour, 'iluReplaceColour'),
  Symbol(@@iluRotate,        'iluRotate'),
  Symbol(@@iluRotate3D,      'iluRotate3D'),
  Symbol(@@iluSaturate1f,    'iluSaturate1f'),
  Symbol(@@iluSaturate4f,    'iluSaturate4f'),
  Symbol(@@iluScale,         'iluScale'),
  Symbol(@@iluScaleAlpha,    'iluScaleAlpha'),
  Symbol(@@iluScaleColours,  'iluScaleColours'),
  Symbol(@@iluSepia,         'iluSepia'),
  Symbol(@@iluSetLanguage,   'iluSetLanguage'),
  Symbol(@@iluSharpen,       'iluSharpen'),
  Symbol(@@iluSwapColours,   'iluSwapColours'),
  Symbol(@@iluWave,          'iluWave')],True) = 49;
// aliasses  
iluColorsUsed := iluColoursUsed;
iluSwapColors := iluSwapColours;
iluReplaceColor := iluReplaceColour;
iluScaleColors := iluScaleColours;
// init
If Result and InitLib then
  iluInit; 
end;

//------------------------------------------------------------------------------

procedure DevILU_Finalize(FinalLib: Boolean = True);
begin
If FinalLib then; // do nothing, afaik ILU does not implement any finalization
CloseLibrary(DevILU_LibraryHandle);
end;

end.
