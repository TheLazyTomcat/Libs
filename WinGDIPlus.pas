{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Windows GDI+ reimplementation in object pascal

    This is just a naive reimplemntation of original sources (*.h files)
    provided with Windows SDK. This is NOT, and was never intended to be,
    a comprehensive implementation that wraps entire subsystem into a neat
    package (like Graphics unit is doing with GDI).

    Almost no attempt was made to change the interface to be more pascal-like,
    so expect things like frequent use of pointers (eg. for output or array
    arguments), enum values without prefixes, use of windows types instead of
    native ones, ... the list continues.

    Translation notes:

      - sources used for this translation were taken from Windows SDK of
        version 10.0.22000.0

      - everything was moved into one unit (obviously)

      - all commens were copied from original sources with no change, comments
        that start with double exclamation character (!!) were added during
        translation

      - lot of code was moved to different places, mainly to avoid circular
        referencing

      - macros were either expanded in-situ or replaced by functions

      - all type identifiers were prepended with capital T (eg. TGraphics,
        TRect), pointers to them with capital P (PGraphics, PRect)

      - most enumerated types were translated as that, an enum, but some were
        changed to a pair of type (usually INT) and a set of constants, this is
        because values of these enums are expected to be combined (eg. using
        bitwise OR), and this would be problematic (though possible) in pascal

      - some methods, functions and arguments were renamed, ususally to deal
        with conflicts with reserved words or clashing identifiers

      - library initialization was reworked, see further [1]

      - all provided classes implement functions that are not part of the GDI+,
        these are there to simplify translation of some peculiar constructs
        (eg. ternary operators - see declaration of TGdiPlusWrapper class for
        more info)

      - classes intended only as data containers (eg. Point, RectF, ...) were
        reworked into records and their methods were implemented as normal
        functions (note that some were renamed to avoid conflicts and ambiguous
        overloads), this was done because of fundamental incompatibility of
        object pascal and C++ classes/objects

      - all constructors (with exceptions in TGraphics, see there for
        explanation) are named Create, therefore are named differently than in
        the original (C++ constructors are just named after the class)

      - some class methods that originally returned references to "global"
        objects were reworked to return unique instances, requiring explicit
        freeing of the returned objects, see further for details [2]

      - where declaration and implementation were separate and something was
        not matching (argument name, order of methods, ...), the declaration
        part was used as a template and the implementation was only consulted

      - where required, new method overloads were added (eg. because the
        declaration called for a default value for structure/record, which is
        not allowed in pascal)

      - if method accepted PWideChar, an overload accepting default type String
        was added, the same goes for arguments of type IStream, they were
        supplemented with TStream-accepting overloads

      - remember to free any object you create, be it explicitly or implicitly,
        as these are usual objects, not interfaces

      - number of helper functions was implemented, more might be added in the
        future

      - I have found parts in the original code that seemed to be erroneous or
        incomplete, some were marked, but all of them were translated as found,
        with the probable errors

      - code that could be simplified without changing its behavior was
        simplified (this was mainly due to a fact that pascal can work with
        function result anywhere in the function's body, C++ code sets returned
        value and exits as one operation, necessitating a temporary variables)

      - given the extent of GDI+ library, only small part of this translation
        was tested, please report any errors and bugs you may find


  [1] Functions GdiplusStartup and GdiplusShutdown are no longer pointing to
      imported GDI+ functions of the same name. They are instead locally
      implemented and are managing dynamic symbol resolving of functions
      provided by highed versions of GDI+ (for details, see description of
      symbol NewGDIPStatic). They also call the imported init and final
      functions as required by the documentation, so it is not necessary to
      call them explicitly.

      Simply put, call functions GdiplusStartup and GdiplusShutdown as
      described by GDI+ documentation, just be aware that you are not calling
      directly to imports.

      The imported functions are still available under names LibGdiplusStartup
      and LibGdiplusShutdown.


  [2] If I understand it correctly, following five functions are, in the
      original code, returning instances of objects that are stored in global
      static buffers.

          TFontFamily.GenericSansSerif
          TFontFamily.GenericSerif
          TFontFamily.GenericMonospace

          TStringFormat.GenericDefault
          TStringFormat.GenericTypographic

      These objects are, at least in the case of TFontFamily methods,
      instantiated only once and then they exist the entire lifetime of the
      program (they should not be destroyed). This saves memory and might
      increase performance.

      It is, in theory, possible to emulate this in pascal, but it would be
      somewhat complex and problematic in multithread environment.
      Therefore, I have decided to replace this by just returning new unique
      instance of the particular object every time it is requested.
      
      This all means one important thing - you have to free the returned object
      after use to prevent memory leak!

  version 1.0.2 (2024-10-14)

  Last change 2024-10-14

  ©2023-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.WinGDIPlus

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    DynLibUtils   - github.com/TheLazyTomcat/Lib.DynLibUtils
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol WinGDIPlus_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils    - github.com/TheLazyTomcat/Lib.UInt64Utils
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    WinFileInfo    - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit WinGDIPlus;
{
  WinGDIPlus_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  WinGDIPlus_UseAuxExceptions to achieve this.
}
{$IF Defined(WinGDIPlus_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17} //!! Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{$MINENUMSIZE 4}
{$ALIGN 8}  //!! this should be default, but to be sure

//!!----------------------------------------------------------------------------
{!!
  NewGDIP

  When this symbol is defined, then types, constants, functions, classes and
  methods from GDI+ version 1.1 are made accessible. When it is not defined,
  these objects are inccessible and are completely removed from compilation
  and only objects from GDI+ version 1.0 are provided.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol WinGDIPlus_NewGDIP_Off.
}
{$DEFINE NewGDIP}
{$IFDEF WinGDIPlus_NewGDIP_Off}
  {$UNDEF NewGDIP}
{$ENDIF}

{!!
  NewGDIPStatic

  When defined, the external functions from newer GDI+ versions (currently
  version 1.1) are statically bound/resolved.
  When not defined (the default state), then these functions are resolved
  dynamically and only if new GDI+ version is requested when initializing
  (calling GdiplusStartup).

  This is here to allow use of code that is calling these functions on older
  systems not supporting new GDI+ versions. There, the initialization and used
  code can be selected at runtime based on supported version (use provided
  helper function VersionSupported).
  If the symbols were resolved statically, it would lead to fatal error even if
  they would not be called.

    NOTE - this symbol has effect only when symbol NewGDIP is defined.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol WinGDIPlus_NewGDIPStatic_ON.
}
{$UNDEF NewGDIPStatic}
{$IFDEF WinGDIPlus_NewGDIPStatic_ON}
  {$DEFINE NewGDIPStatic}
{$ENDIF}

interface

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W3018:={$WARN 3018 OFF}} //!! Constructor should be public
  {$DEFINE W3031:={$WARN 3031 OFF}} //!! Values in enumeration types have to be ascending
{$ENDIF}

uses
  Windows, SysUtils, Classes, ActiveX{!! for IStream}, Math,
  {$IFDEF FPC} jwawingdi,{$ENDIF}
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{!!-----------------------------------------------------------------------------
    Library-specific exceptions
-------------------------------------------------------------------------------}
type
  EGDIPlusException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EGDIPlusError             = class(EGDIPlusException);
  EGDIPlusIndexOutOfBounds  = class(EGDIPlusException);
  EGDIPlusObjectNotAssigned = class(EGDIPlusException);
  EGDIPlusCodecNotFound     = class(EGDIPlusException);
  
{!!-----------------------------------------------------------------------------
    Constants and types
-------------------------------------------------------------------------------}
const
  GDIPLIB = 'gdiplus.dll';

type
  // integer types
  INT       = Int32;        PINT = ^INT;
  DWORDLONG = UInt64;
  size_t    = PtrUInt;
  ULONG_PTR = PtrUInt;      PULONG_PTR = ^ULONG_PTR;
  UINT_PTR  = PtrUInt;
  PROPID    = ULONG;

  // handle types
  HINSTANCE = THandle;
  HANDLE = THandle;

  // floating-point types
  float = Single;

  // pointer types
  PHRGN         = ^HRGN;
  PHBITMAP      = ^HBITMAP;
  PHICON        = ^HICON;
  PHDC          = ^HDC;
  PIStream      = ^IStream;
  PLANGID       = ^LANGID;
  LPBYTE        = PByte;
  PHENHMETAFILE = ^HENHMETAFILE;

  // structured types
  RECTL  = Windows.TRect;
  SIZEL  = Windows.TSize;
  TCLSID = TGUID;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   Gdiplus.h
*
* Abstract:
*
*   GDI+ public header file
*
\**************************************************************************)
const
  GDIPVER = {$IFDEF NewGDIP}$0110{$ELSE}$0100{$ENDIF};  //!! for conditional compilation

type
  //!! just some placeholder I assume
  IDirectDrawSurface7 = IUnknown;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusMem.h
*
* Abstract:
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)
//----------------------------------------------------------------------------
// Memory Allocation APIs
//----------------------------------------------------------------------------

Function GdipAlloc(size: size_t): Pointer; stdcall; external GDIPLIB;

procedure GdipFree(ptr: Pointer); stdcall; external GDIPLIB;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusBase.h
*
* Abstract:
*
*   GDI+ base memory allocation class
*
\**************************************************************************)
{!!=============================================================================
    TGdiPlusWrapper - class declaration
===============================================================================}
{!!
  This class defines interface used when replacing some ternary operators used
  in the original source, namely operators of style...

      If Assigned(Obj) then
        Var := Obj.NativeObjectField
      else
        Var := nil;

    ...which might in the original look something like:

      Obj ? Obj->NativeObjectField : NULL
}
type
  TGdiPlusWrapper = class(TObject)
  protected
    Function GetNativeObject: Pointer; virtual; abstract;
    Function GetNativeObjectAddr: Pointer; virtual; abstract;
  public
    Function NativeObject: Pointer;
    Function NativeObjectAddr: Pointer;
  end;

type
  TGdiPlusBase = class(TGdiPlusWrapper);  //!! common ancestor for "big" objects


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusEnums.h
*
* Abstract:
*
*   GDI+ Enumeration Types
*
\**************************************************************************)
//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------
const
  FlatnessDefault:  Single = 1.0/4.0;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  TGraphicsState     = UINT;    PGraphicsState     = ^TGraphicsState;
  TGraphicsContainer = UINT;    PGraphicsContainer = ^TGraphicsContainer;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------
type
  TFillMode = (
    FillModeAlternate,  // 0
    FillModeWinding     // 1
  );
  PFillMode = ^TFillMode;

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------
type
  TQualityMode = (
    QualityModeInvalid = -1,
    QualityModeDefault = 0,
    QualityModeLow     = 1, // Best performance
    QualityModeHigh    = 2  // Best rendering quality
  );
  PQualityMode = ^TQualityMode;

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  TCompositingMode = (
    CompositingModeSourceOver,  // 0
    CompositingModeSourceCopy   // 1
  );
  PCompositingMode = ^TCompositingMode;

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
type
  TCompositingQuality = (
    CompositingQualityInvalid          = Ord(QualityModeInvalid),
    CompositingQualityDefault          = Ord(QualityModeDefault),
    CompositingQualityHighSpeed        = Ord(QualityModeLow),
    CompositingQualityHighQuality      = Ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  PCompositingQuality = ^TCompositingQuality;

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------
type
  TUnit = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );
  PUnit = ^TUnit;

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
type
  TMetafileFrameUnit =(
    MetafileFrameUnitPixel      = Ord(UnitPixel),
    MetafileFrameUnitPoint      = Ord(UnitPoint),
    MetafileFrameUnitInch       = Ord(UnitInch),
    MetafileFrameUnitDocument   = Ord(UnitDocument),
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi                        // GDI compatible .01 MM units
  );
  PMetafileFrameUnit = ^TMetafileFrameUnit;

//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------
type
  TCoordinateSpace = (
    CoordinateSpaceWorld,   // 0
    CoordinateSpacePage,    // 1
    CoordinateSpaceDevice   // 2
  );
  PCoordinateSpace = ^TCoordinateSpace;

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------
type
  TWrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  PWrapMode = ^TWrapMode;

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------
type
  THatchStyle = (
    HatchStyleHorizontal,                   // 0
    HatchStyleVertical,                     // 1
    HatchStyleForwardDiagonal,              // 2
    HatchStyleBackwardDiagonal,             // 3
    HatchStyleCross,                        // 4
    HatchStyleDiagonalCross,                // 5
    HatchStyle05Percent,                    // 6
    HatchStyle10Percent,                    // 7
    HatchStyle20Percent,                    // 8
    HatchStyle25Percent,                    // 9
    HatchStyle30Percent,                    // 10
    HatchStyle40Percent,                    // 11
    HatchStyle50Percent,                    // 12
    HatchStyle60Percent,                    // 13
    HatchStyle70Percent,                    // 14
    HatchStyle75Percent,                    // 15
    HatchStyle80Percent,                    // 16
    HatchStyle90Percent,                    // 17
    HatchStyleLightDownwardDiagonal,        // 18
    HatchStyleLightUpwardDiagonal,          // 19
    HatchStyleDarkDownwardDiagonal,         // 20
    HatchStyleDarkUpwardDiagonal,           // 21
    HatchStyleWideDownwardDiagonal,         // 22
    HatchStyleWideUpwardDiagonal,           // 23
    HatchStyleLightVertical,                // 24
    HatchStyleLightHorizontal,              // 25
    HatchStyleNarrowVertical,               // 26
    HatchStyleNarrowHorizontal,             // 27
    HatchStyleDarkVertical,                 // 28
    HatchStyleDarkHorizontal,               // 29
    HatchStyleDashedDownwardDiagonal,       // 30
    HatchStyleDashedUpwardDiagonal,         // 31
    HatchStyleDashedHorizontal,             // 32
    HatchStyleDashedVertical,               // 33
    HatchStyleSmallConfetti,                // 34
    HatchStyleLargeConfetti,                // 35
    HatchStyleZigZag,                       // 36
    HatchStyleWave,                         // 37
    HatchStyleDiagonalBrick,                // 38
    HatchStyleHorizontalBrick,              // 39
    HatchStyleWeave,                        // 40
    HatchStylePlaid,                        // 41
    HatchStyleDivot,                        // 42
    HatchStyleDottedGrid,                   // 43
    HatchStyleDottedDiamond,                // 44
    HatchStyleShingle,                      // 45
    HatchStyleTrellis,                      // 46
    HatchStyleSphere,                       // 47
    HatchStyleSmallGrid,                    // 48
    HatchStyleSmallCheckerBoard,            // 49
    HatchStyleLargeCheckerBoard,            // 50
    HatchStyleOutlinedDiamond,              // 51
    HatchStyleSolidDiamond,                 // 52

    HatchStyleTotal,
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    HatchStyleLargeGrid = HatchStyleCross,  // 4
  {$IFDEF FPCDWM}{$POP}{$ENDIF}

    HatchStyleMin       = HatchStyleHorizontal,
    HatchStyleMax       = Ord(HatchStyleTotal) - 1
  );
  PHatchStyle = ^THatchStyle;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------
type
  TDashStyle = (
    DashStyleSolid,       // 0
    DashStyleDash,        // 1
    DashStyleDot,         // 2
    DashStyleDashDot,     // 3
    DashStyleDashDotDot,  // 4
    DashStyleCustom       // 5
  );
  PDashStyle = ^TDashStyle;

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
type
  TDashCap = (
    DashCapFlat     = 0,
    DashCapRound    = 2,
    DashCapTriangle = 3
  );
  PDashCap = ^TDashCap;

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
type
  TLineCap = (
    LineCapFlat          = 0,
    LineCapSquare        = 1,
    LineCapRound         = 2,
    LineCapTriangle      = 3,

    LineCapNoAnchor      = $10,   // corresponds to flat cap
    LineCapSquareAnchor  = $11,   // corresponds to square cap
    LineCapRoundAnchor   = $12,   // corresponds to round cap
    LineCapDiamondAnchor = $13,   // corresponds to triangle cap
    LineCapArrowAnchor   = $14,   // no correspondence

    LineCapCustom        = $ff,   // custom cap
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    LineCapAnchorMask    = $f0    // mask to check for anchor or not.
  );
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  PLineCap = ^TLineCap;

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------
type
  TCustomLineCapType =(
    CustomLineCapTypeDefault         = 0,
    CustomLineCapTypeAdjustableArrow = 1
  );
  PCustomLineCapType = ^TCustomLineCapType;

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------
type
  TLineJoin = (
    LineJoinMiter        = 0,
    LineJoinBevel        = 1,
    LineJoinRound        = 2,
    LineJoinMiterClipped = 3
  );
  PLineJoin = ^TLineJoin;

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------
type
  TPathPointType = (
    PathPointTypeStart        = 0,    // move
    PathPointTypeLine         = 1,    // line
    PathPointTypeBezier       = 3,    // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask = $07,  // type mask (lowest 3 bits).
    PathPointTypeDashMode     = $10,  // currently in dash mode.
    PathPointTypePathMarker   = $20,  // a marker for the path.
    PathPointTypeCloseSubpath = $80,  // closed flag

    // Path types used for advanced path.
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    PathPointTypeBezier3      = 3     // cubic Bezier
  );
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  PPathPointType = ^TPathPointType;

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------
type
  TWarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  PWarpMode = ^TWarpMode;

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------
type
  TLinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  PLinearGradientMode = ^TLinearGradientMode;

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------
type
  TCombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  PCombineMode = ^TCombineMode;

//--------------------------------------------------------------------------
// Image types
//--------------------------------------------------------------------------
type
  TImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );
  PImageType = ^TImageType;

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
type
  TInterpolationMode = (
    InterpolationModeInvalid          = Ord(QualityModeInvalid),
    InterpolationModeDefault          = Ord(QualityModeDefault),
    InterpolationModeLowQuality       = Ord(QualityModeLow),
    InterpolationModeHighQuality      = Ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  PInterpolationMode = ^TInterpolationMode;

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------
type
  TPenAlignment = (
    PenAlignmentCenter = 0,
    PenAlignmentInset  = 1
  );
  PPenAlignment = ^TPenAlignment;

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------
type
  TBrushType = (
   BrushTypeSolidColor     = 0,
   BrushTypeHatchFill      = 1,
   BrushTypeTextureFill    = 2,
   BrushTypePathGradient   = 3,
   BrushTypeLinearGradient = 4
  );
  PBrushType = ^TBrushType;

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
type
  TPenType = (
   PenTypeSolidColor     = Ord(BrushTypeSolidColor),
   PenTypeHatchFill      = Ord(BrushTypeHatchFill),
   PenTypeTextureFill    = Ord(BrushTypeTextureFill),
   PenTypePathGradient   = Ord(BrushTypePathGradient),
   PenTypeLinearGradient = Ord(BrushTypeLinearGradient),
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
   PenTypeUnknown        = -1
  );
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  PPenType = ^TPenType;

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------
type
  TMatrixOrder = (
    MatrixOrderPrepend = 0,
    MatrixOrderAppend  = 1
  );
  PMatrixOrder = ^TMatrixOrder;

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------
type
  TGenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  PGenericFontFamily = ^TGenericFontFamily;

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
  TFontStyle = INT;     PFontStyle = ^TFontStyle;
const
  FontStyleRegular    = 0;
  FontStyleBold       = 1;
  FontStyleItalic     = 2;
  FontStyleBoldItalic = 3;
  FontStyleUnderline  = 4;
  FontStyleStrikeout  = 8;

//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
type
  TSmoothingMode = (
    SmoothingModeInvalid      = Ord(QualityModeInvalid),
    SmoothingModeDefault      = Ord(QualityModeDefault),
    SmoothingModeHighSpeed    = Ord(QualityModeLow),
    SmoothingModeHighQuality  = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  {$IF GDIPVER >= $0110},
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    SmoothingModeAntiAlias8x4 = Ord(SmoothingModeAntiAlias),
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    SmoothingModeAntiAlias8x8
  {$IFEND}
  );
  PSmoothingMode = ^TSmoothingMode;

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
type
  TPixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
  PPixelOffsetMode = ^TPixelOffsetMode;

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------
type
  TTextRenderingHint = (
    TextRenderingHintSystemDefault = 0,         // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,  // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,         // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,          // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                 // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit           // Glyph CT bitmap with hinting
  );
  PTextRenderingHint = ^TTextRenderingHint;

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------
type
  TMetafileType = (
    MetafileTypeInvalid,        // Invalid metafile
    MetafileTypeWmf,            // Standard WMF
    MetafileTypeWmfPlaceable,   // Placeable WMF
    MetafileTypeEmf,            // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,    // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual     // EMF+ with dual, down-level records
  );
  PMetafileType = ^TMetafileType;

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
type
  TEmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
  PEmfType = ^TEmfType;

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------
type
  TObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap,
  {$IF GDIPVER >= $0110} 
    ObjectTypeGraphics,

  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    ObjectTypeMax = ObjectTypeGraphics,
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  {$ELSE}
    ObjectTypeMax = ObjectTypeCustomLineCap,
  {$IFEND}
    ObjectTypeMin = ObjectTypeBrush
  );
  PObjectType = ^TObjectType;

Function ObjectTypeIsValid(type_: TObjectType): BOOL;{$IFDEF CanInline} inline;{$ENDIF}

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

// We have to change the WMF record numbers so that they don't conflict with
// the EMF and EMF+ record numbers.
const
  GDIP_EMFPLUS_RECORD_BASE = $00004000;
  GDIP_WMF_RECORD_BASE     = $00010000;

type
  TEmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor            = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    WmfRecordTypeSetBkMode             = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    WmfRecordTypeSetMapMode            = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2               = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs             = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode       = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode     = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra      = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor          = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification  = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg          = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt          = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg        = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt        = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg       = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt        = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg     = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt      = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo                = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo                = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect       = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect     = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc                   = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse               = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill             = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie                   = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle             = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect             = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt                = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC                = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel              = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn         = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut               = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt                = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt            = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon               = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline              = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape                = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC             = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion            = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion           = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion          = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion           = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion      = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject          = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign          = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText              = ($062F or GDIP_WMF_RECORD_BASE),  // META_DRAWTEXT
    WmfRecordTypeChord                 = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags        = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut            = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev           = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette         = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette        = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette        = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries         = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon           = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette         = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt             = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt         = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB            = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill          = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout             = ($0149 or GDIP_WMF_RECORD_BASE),  // META_SETLAYOUT
    WmfRecordTypeResetDC               = ($014C or GDIP_WMF_RECORD_BASE),  // META_RESETDC
    WmfRecordTypeStartDoc              = ($014D or GDIP_WMF_RECORD_BASE),  // META_STARTDOC
    WmfRecordTypeStartPage             = ($004F or GDIP_WMF_RECORD_BASE),  // META_STARTPAGE
    WmfRecordTypeEndPage               = ($0050 or GDIP_WMF_RECORD_BASE),  // META_ENDPAGE
    WmfRecordTypeAbortDoc              = ($0052 or GDIP_WMF_RECORD_BASE),  // META_ABORTDOC
    WmfRecordTypeEndDoc                = ($005E or GDIP_WMF_RECORD_BASE),  // META_ENDDOC
    WmfRecordTypeDeleteObject          = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette         = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush           = ($00F8 or GDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush    = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect     = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect    = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect   = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect  = ($02FD or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap          = ($06FE or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion          = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,
  {$IF GDIPVER >= $0110}
    EmfPlusRecordTypeStrokeFillPath,
    EmfPlusRecordTypeSerializableObject,

    EmfPlusRecordTypeSetTSGraphics,
    EmfPlusRecordTypeSetTSClip,
  {$IFEND}
    // NOTE: New records *must* be added immediately before this line.

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = Ord(EmfPlusRecordTotal) - 1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );
  PEmfPlusRecordType = ^TEmfPlusRecordType;

Function GDIP_WMF_RECORD_TO_EMFPLUS(Value: TEmfPlusRecordType): TEmfPlusRecordType;
Function GDIP_EMFPLUS_RECORD_TO_WMF(Value: TEmfPlusRecordType): TEmfPlusRecordType;
Function GDIP_IS_WMF_RECORDTYPE(Value: TEmfPlusRecordType): Boolean;

//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------
type
  TStringFormatFlags = INT;     PStringFormatFlags = ^TStringFormatFlags;
const
  StringFormatFlagsDirectionRightToLeft  = $00000001;
  StringFormatFlagsDirectionVertical     = $00000002;
  StringFormatFlagsNoFitBlackBox         = $00000004;
  StringFormatFlagsDisplayFormatControl  = $00000020;
  StringFormatFlagsNoFontFallback        = $00000400;
  StringFormatFlagsMeasureTrailingSpaces = $00000800;
  StringFormatFlagsNoWrap                = $00001000;
  StringFormatFlagsLineLimit             = $00002000;

  StringFormatFlagsNoClip                = $00004000;
  StringFormatFlagsBypassGDI             = $80000000;

//---------------------------------------------------------------------------
// StringTrimming
//---------------------------------------------------------------------------
type
  TStringTrimming = (
    StringTrimmingNone              = 0,
    StringTrimmingCharacter         = 1,
    StringTrimmingWord              = 2,
    StringTrimmingEllipsisCharacter = 3,
    StringTrimmingEllipsisWord      = 4,
    StringTrimmingEllipsisPath      = 5
  );
  PStringTrimming = ^TStringTrimming;

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------
type
  TStringDigitSubstitute = (
    StringDigitSubstituteUser        = 0,  // As NLS setting
    StringDigitSubstituteNone        = 1,
    StringDigitSubstituteNational    = 2,
    StringDigitSubstituteTraditional = 3
  );
  PStringDigitSubstitute = ^TStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------
type
  THotkeyPrefix = (
    HotkeyPrefixNone = 0,
    HotkeyPrefixShow = 1,
    HotkeyPrefixHide = 2
  );
  PHotkeyPrefix = ^THotkeyPrefix;

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------
type
  TStringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear   = 0,
    StringAlignmentCenter = 1,
    StringAlignmentFar    = 2
  );
  PStringAlignment = ^TStringAlignment;

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------
type
  TDriverStringOptions = INT;     PDriverStringOptions = ^TDriverStringOptions;
const
  DriverStringOptionsCmapLookup      = 1;
  DriverStringOptionsVertical        = 2;
  DriverStringOptionsRealizedAdvance = 4;
  DriverStringOptionsLimitSubpixel   = 8;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------
type
  TFlushIntention = (
    FlushIntentionFlush = 0,  // Flush all batched rendering operations
    FlushIntentionSync  = 1   // Flush all batched rendering operations
                              // and wait for them to complete
  );
  PFlushIntention = ^TFlushIntention;

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------
type
  TEncoderParameterValueType = (
    EncoderParameterValueTypeByte           = 1,  // 8-bit unsigned int
    EncoderParameterValueTypeASCII          = 2,  // 8-bit byte containing one 7-bit ASCII
                                                  // code. NULL terminated.
    EncoderParameterValueTypeShort          = 3,  // 16-bit unsigned int
    EncoderParameterValueTypeLong           = 4,  // 32-bit unsigned int
    EncoderParameterValueTypeRational       = 5,  // Two Longs. The first Long is the
                                                  // numerator, the second Long expresses the
                                                  // denomintor.
    EncoderParameterValueTypeLongRange      = 6,  // Two longs which specify a range of
                                                  // integer values. The first Long specifies
                                                  // the lower end and the second one
                                                  // specifies the higher end. All values
                                                  // are inclusive at both ends
    EncoderParameterValueTypeUndefined      = 7,  // 8-bit byte that can take any value
                                                  // depending on field definition
    EncoderParameterValueTypeRationalRange  = 8   // Two Rationals. The first Rational
                                                  // specifies the lower end and the second
                                                  // specifies the higher end. All values
                                                  // are inclusive at both ends
  {$IF GDIPVER >= $0110},
    EncoderParameterValueTypePointer        = 9   // a pointer to a parameter defined data.
  {$IFEND}
  );
  PEncoderParameterValueType = ^TEncoderParameterValueType;

//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------
type
  TEncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  {$IF GDIPVER >= $0110},
    EncoderValueColorTypeGray,
    EncoderValueColorTypeRGB
  {$IFEND}
  );
  PEncoderValue = ^TEncoderValue;

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
type
  TEmfToWmfBitsFlags = INT;   PEmfToWmfBitsFlags = ^TEmfToWmfBitsFlags;
const
  EmfToWmfBitsFlagsDefault          = $00000000;
  EmfToWmfBitsFlagsEmbedEmf         = $00000001;
  EmfToWmfBitsFlagsIncludePlaceable = $00000002;
  EmfToWmfBitsFlagsNoXORClip        = $00000004;


{$IF GDIPVER >= $0110}
//---------------------------------------------------------------------------
// Conversion of Emf To Emf+ Bits flags
//---------------------------------------------------------------------------
type
  TConvertToEmfPlusFlags = (
    ConvertToEmfPlusFlagsDefault       = $00000000,
    ConvertToEmfPlusFlagsRopUsed       = $00000001,
    ConvertToEmfPlusFlagsText          = $00000002,
    ConvertToEmfPlusFlagsInvalidRecord = $00000004
  );
  PConvertToEmfPlusFlags = ^TConvertToEmfPlusFlags;
{$IFEND}

//---------------------------------------------------------------------------
// Test Control flags
//---------------------------------------------------------------------------
type
  TGpTestControlEnum = (
    TestControlForceBilinear  = 0,
    TestControlNoICM          = 1,
    TestControlGetBuildNumber = 2
  );
  PGpTestControlEnum = ^TGpTestControlEnum;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusTypes.h
*
* Abstract:
*
*   GDI+ Types
*
\**************************************************************************)

// Support NOMINMAX by defining local min max macros
Function GDIPLUS_MIN(a,b: Integer): Integer; overload;
Function GDIPLUS_MIN(a,b: Single): Single; overload;

Function GDIPLUS_MAX(a,b: Integer): Integer; overload;
Function GDIPLUS_MAX(a,b: Single): Single; overload;

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------
type
  TImageAbort = Function(Param: Pointer): BOOL; stdcall;
  TDrawImageAbort = TImageAbort;
  TGetThumbnailImageAbort = TImageAbort;

  TEnumerateMetafileProc = Function(RecordType: TEmfPlusRecordType; flags,dataSize: UINT; data: PByte; callbackData: Pointer): BOOL; stdcall;

{$IF GDIPVER >= $0110}
// This is the main GDI+ Abort interface
(*!!
      struct __declspec(novtable) GdiplusAbort
      {
          virtual HRESULT __stdcall Abort(void) = 0;
      };

  This type is clearly not an interface (as in COM interface) nor object.
  I have no knowledge on how C++ creates these "advanced structs" with methods,
  but it seems a pointer to (virtual) method table is expected to be at the
  beginning and the call is made through there. The following implementation
  is based on this assumption and it seems to work just fine.

  If you want to pass data to the call, create a structure that will start with
  TGdiplusAbort (there are multiple ways to do that) and add fields with values
  you want to have. You can then reference them using Self argument, which
  points to the beginning of the entire record.

  To propertly prepare the structure, use function GdiplusAbortSetup.

    NOTE - I know the structure corresponds to objects in Delphi, so an object
           instance could be used here. But it certainly does not correspond
           to how objects look in FPC.
*)
type
  TGdiplusAbortCallback = Function(Self: Pointer): HRESULT; stdcall;

  PGdiplusAbort = ^TGdiplusAbort;
  TGdiplusAbort = record
    VMTPtr:         Pointer;
    AbortCallback:  TGdiplusAbortCallback;
  end;

procedure GdiplusAbortSetup(Struct: PGdiplusAbort; Callback: TGdiplusAbortCallback);
{$IFEND}

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------
type
  REAL = Single;  PREAL = ^REAL;

const
  REAL_MAX       = MaxSingle;
  REAL_MIN       = MinSingle;
  REAL_TOLERANCE = MinSingle * 100;
  REAL_EPSILON   = 1.192092896e-07; (* FLT_EPSILON *)

//--------------------------------------------------------------------------
// Forward declarations of common classes
//--------------------------------------------------------------------------
{!!
  Forward declarations removed - they are not needed here.

  Original text:

class Size;
class SizeF;
class Point;
class PointF;
class Rect;
class RectF;
class CharacterRange;
}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  TStatus = (
    Ok                        = 0,
    GenericError              = 1,
    InvalidParameter          = 2,
    OutOfMemory               = 3,
    ObjectBusy                = 4,
    InsufficientBuffer        = 5,
    NotImplemented            = 6,
    Win32Error                = 7,
    WrongState                = 8,
    Aborted                   = 9,
    FileNotFound              = 10,
    ValueOverflow             = 11,
    AccessDenied              = 12,
    UnknownImageFormat        = 13,
    FontFamilyNotFound        = 14,
    FontStyleNotFound         = 15,
    NotTrueTypeFont           = 16,
    UnsupportedGdiplusVersion = 17,
    GdiplusNotInitialized     = 18,
    PropertyNotFound          = 19,
    PropertyNotSupported      = 20
  {$IF GDIPVER >= $0110},
    ProfileNotFound           = 21
  {$IFEND}
  );
  PStatus = ^TStatus;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------
type
  TSizeF = record
    Width:  REAL;
    Height: REAL;
  end;
  PSizeF = ^TSizeF;

Function SizeF: TSizeF; overload;
Function SizeF(const sz: TSizeF): TSizeF; overload;
Function SizeF(width,height: REAL): TSizeF; overload;

Function Add(const sza,szb: TSizeF): TSizeF; overload;
Function Subtract(const sza,szb: TSizeF): TSizeF; overload;

Function Equals(const sza,szb: TSizeF): BOOL; overload;
Function Empty(const sz: TSizeF): BOOL; overload;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------
type
  TSize = record
    Width:  INT;
    Height: INT;
  end;
  PSize = ^TSize;

Function SizeI: TSize; overload;
Function SizeI(const sz: TSize): TSize; overload;
Function SizeI(width,height: INT): TSize; overload;

Function Add(const sza,szb: TSize): TSize; overload;
Function Subtract(const sza,szb: TSize): TSize; overload;

Function Equals(const sza,szb: TSize): BOOL; overload;
Function Empty(const sz: TSize): BOOL; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------
type
  TPointF = record
    X:  REAL;
    Y:  REAL;
  end;
  PPointF = ^TPointF;

Function PointF: TPointF; overload;
Function PointF(const pt: TPointF): TPointF; overload;
Function PointF(const sz: TSizeF): TPointF; overload;
Function PointF(x,y: REAL): TPointF; overload;

Function Add(const pta,ptb: TPointF): TPointF; overload;
Function Subtract(const pta,ptb: TPointF): TPointF; overload;

Function Equals(const pta,ptb: TPointF): BOOL; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------
type
  TPoint = record
    X:  INT;
    Y:  INT;
  end;
  PPoint = ^TPoint;

Function PointI: TPoint; overload;
Function PointI(const pt: TPoint): TPoint; overload;
Function PointI(const sz: TSize): TPoint; overload;
Function PointI(x,y: INT): TPoint; overload;

Function Add(const pta,ptb: TPoint): TPoint; overload;
Function Subtract(const pta,ptb: TPoint): TPoint; overload;

Function Equals(const pta,ptb: TPoint): BOOL; overload;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------
type
  TRectF = record
    X:      REAL;
    Y:      REAL;
    Width:  REAL;
    Height: REAL;
  end;
  PRectF = ^TRectF;

Function RectF: TRectF; overload;
Function RectF(x,y,width,height: REAL): TRectF; overload;
Function RectF(const location: TPointF; const sz: TSizeF): TRectF; overload;

Function Clone(const rc: TRectF): TRectF; overload;

Function GetLocation(const rc: TRectF): TPointF; overload;
Function GetSize(const rc: TRectF): TSizeF; overload;
Function GetBounds(const rc: TRectF): TRectF; overload;
Function GetLeft(const rc: TRectF): REAL; overload;
Function GetTop(const rc: TRectF): REAL; overload;
Function GetRight(const rc: TRectF): REAL; overload;
Function GetBottom(const rc: TRectF): REAL; overload;

Function IsEmptyArea(const rc: TRectF): BOOL; overload;
Function Equals(const rca,rcb: TRectF): BOOL; overload;
Function Contains(const rc: TRectF; x,y: REAL): BOOL; overload;
Function Contains(const rc: TRectF; const pt: TPointF): BOOL; overload;
Function Contains(const rca,rcb: TRectF): BOOL; overload;

Function Inflate(const rc: TRectF; dx,dy: REAL): TRectF; overload;
Function Inflate(const rc: TRectF; const pt: TPointF): TRectF; overload;
Function Intersect(const rca,rcb: TRectF): TRectF; overload;
Function Intersect(out rcc: TRectF; const rca,rcb: TRectF): BOOL; overload;
Function IntersectsWith(const rca,rcb: TRectF): BOOL; overload;
Function Union(const rca,rcb: TRectF): TRectF; overload;
Function Union(out rcc: TRectF; const rca,rcb: TRectF): BOOL; overload;
Function Offset(const rc: TRectF; const pt: TPointF): TRectF; overload;
Function Offset(const rc: TRectF; dx,dy: REAL): TRectF; overload;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------
type
  TRect = record
    X:      INT;
    Y:      INT;
    Width:  INT;
    Height: INT;
  end;
  PRect = ^TRect;

Function RectI: TRect; overload;
Function RectI(x,y,width,height: INT): TRect; overload;
Function RectI(const location: TPoint; const sz: TSize): TRect; overload;

Function Clone(const rc: TRect): TRect; overload;

Function GetLocation(const rc: TRect): TPoint; overload;
Function GetSize(const rc: TRect): TSize; overload;
Function GetBounds(const rc: TRect): TRect; overload;
Function GetLeft(const rc: TRect): INT; overload;
Function GetTop(const rc: TRect): INT; overload;
Function GetRight(const rc: TRect): INT; overload;
Function GetBottom(const rc: TRect): INT; overload;

Function IsEmptyArea(const rc: TRect): BOOL; overload;
Function Equals(const rca,rcb: TRect): BOOL; overload;
Function Contains(const rc: TRect; x,y: INT): BOOL; overload;
Function Contains(const rc: TRect; const pt: TPoint): BOOL; overload;
Function Contains(const rca,rcb: TRect): BOOL; overload;

Function Inflate(const rc: TRect; dx,dy: INT): TRect; overload;
Function Inflate(const rc: TRect; const pt: TPoint): TRect; overload;
Function Intersect(const rca,rcb: TRect): TRect; overload;
Function Intersect(out rcc: TRect; const rca,rcb: TRect): BOOL; overload;
Function IntersectsWith(const rca,rcb: TRect): BOOL; overload;
Function Union(const rca,rcb: TRect): TRect; overload;
Function Union(out rcc: TRect; const rca,rcb: TRect): BOOL; overload;
Function Offset(const rc: TRect; const pt: TPoint): TRect; overload;
Function Offset(const rc: TRect; dx,dy: INT): TRect; overload;

{!!-----------------------------------------------------------------------------
    TPathData - declaration
-------------------------------------------------------------------------------}
type
  TPathData = record
    Count:  INT;
    Points: PPointF;  //!! pointer to an array of TPointF data
    Types:  PByte;
  end;
  PPathData = ^TPathData;

{!!
  Following functions are not part of the original code, but are not considered
  helper functions either, as they are needed for proper management of TPathData
  variables.
}
procedure PathDataInit(out PathData: TPathData);
procedure PathDataAlloc(var PathData: TPathData; Count: Integer);
procedure PathDataFree(var PathData: TPathData);

Function PathDataPointGet(const PathData: TPathData; Index: Integer): TPointF;
procedure PathDataPointSet(const PathData: TPathData; Index: Integer; Value: TPointF);

Function PathDataTypeGet(const PathData: TPathData; Index: Integer): Byte;
procedure PathDataTypeSet(const PathData: TPathData; Index: Integer; Value: Byte);

{!!-----------------------------------------------------------------------------
    TCharacterRange - declaration
-------------------------------------------------------------------------------}
type
  TCharacterRange = record
    First:  INT;
    Length: INT;
  end;
  PCharacterRange = ^TCharacterRange;

Function CharacterRange: TCharacterRange; overload;
Function CharacterRange(first,length: INT): TCharacterRange; overload;

procedure Assign(var range: TCharacterRange; const rhs: TCharacterRange); overload;


(**************************************************************************
*
* Copyright (c) 2000-2003 Microsoft Corporation
*
* Module Name:
*
*   Gdiplus initialization
*
* Abstract:
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  TDebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  PDebugEventLevel = ^TDebugEventLevel;

// Callback function that GDI+ can call, on debug builds, for assertions
// and warnings.
type
  //!! dunno about ansi vs unicode, the only declaration in gdipluinit.h is "..., CHAR *message);"
  TDebugEventProc = procedure(level: TDebugEventLevel; message: PAnsiChar); stdcall;

// Notification functions which the user must call appropriately if
// "SuppressBackgroundThread" (below) is set.
type
  TNotificationHookProc   = Function(token: PULONG_PTR): TStatus; stdcall;
  TNotificationUnhookProc = procedure(token: ULONG_PTR); stdcall;

// Input structure for GdiplusStartup()
type
  TGdiplusStartupInput = record
    GdiplusVersion:           UINT32;           // Must be 1  (or 2 for the Ex version)
    DebugEventCallback:       TDebugEventProc;  // Ignored on free builds
    SuppressBackgroundThread: BOOL;             // FALSE unless you're prepared to call
                                                // the hook/unhook functions properly
    SuppressExternalCodecs:   BOOL;             // FALSE unless you want GDI+ only to use
                                                // its internal image codecs.
  end;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

Function GdiplusStartupInput(
  debugEventCallback:       TDebugEventProc = nil;
  suppressBackgroundThread: BOOL = False;
  suppressExternalCodecs:   BOOL = False): TGdiplusStartupInput;


{$IF GDIPVER >= $0110}
type
  TGdiplusStartupInputEx = record
    GdiplusVersion:           UINT32;
    DebugEventCallback:       TDebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs:   BOOL;
    StartupParameters:        Int;  // Do we not set the FPU rounding mode
  end;
  PGdiplusStartupInputEx = ^TGdiplusStartupInputEx;

Function GdiplusStartupInputEx(
  startupParameters:        INT = 0;
  debugEventCallback:       TDebugEventProc = nil;
  suppressBackgroundThread: BOOL = False;
  suppressExternalCodecs:   BOOL = False): TGdiplusStartupInputEx;

type
  TGdiplusStartupParams = INT;
const
  GdiplusStartupDefault          = 0;     
  GdiplusStartupNoSetRound       = 1;
  GdiplusStartupSetPSValue       = 2;
  GdiplusStartupTransparencyMask = INT($FF000000);

{$IFEND}

// Output structure for GdiplusStartup()
type
  TGdiplusStartupOutput = record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.
    NotificationHook:   TNotificationHookProc;
    NotificationUnhook: TNotificationUnhookProc;
  end;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

// GDI+ initialization. Must not be called from DllMain - can cause deadlock.
//
// Must be called before GDI+ API's or constructors are used.
//
// token  - may not be NULL - accepts a token to be passed in the corresponding
//          GdiplusShutdown call.
// input  - may not be NULL
// output - may be NULL only if input->SuppressBackgroundThread is FALSE.
Function LibGdiplusStartup(token: PULONG_PTR; input: PGdiplusStartupInput;
  output: PGdiplusStartupOutput): TStatus; stdcall; external GDIPLIB name 'GdiplusStartup';

//!! wrapper...
Function GdiplusStartup(token: PULONG_PTR; input: PGdiplusStartupInput; output: PGdiplusStartupOutput): TStatus;

// GDI+ termination. Must be called before GDI+ is unloaded. 
// Must not be called from DllMain - can cause deadlock.
//
// GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
// to GDI+ object destructors.
procedure LibGdiplusShutdown(token: ULONG_PTR); stdcall; external GDIPLIB name 'GdiplusShutdown';

//!! wrapper
procedure GdiplusShutdown(token: ULONG_PTR);


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   Gdiplus Pixel Formats
*
* Abstract:
*
*   GDI+ Pixel Formats
*
\**************************************************************************)
type
  TARGB   = DWORD;        PARGB   = ^TARGB;
  TARGB64 = DWORDLONG;    PARGB64 = ^TARGB64;

const
  ALPHA_SHIFT = 24;
  RED_SHIFT   = 16;
  GREEN_SHIFT = 8;
  BLUE_SHIFT  = 0;
  ALPHA_MASK  = TARGB($FF) shl ALPHA_SHIFT;

// In-memory pixel data formats:
// bits 0-7 = format index
// bits 8-15 = pixel size (in bits)
// bits 16-23 = flags
// bits 24-31 = reserved
type
  TPixelFormat = INT;     PPixelFormat = ^TPixelFormat;

const
  PixelFormatIndexed   = $00010000; // Indexes into a palette
  PixelFormatGDI       = $00020000; // Is a GDI-supported format
  PixelFormatAlpha     = $00040000; // Has an alpha component
  PixelFormatPAlpha    = $00080000; // Pre-multiplied alpha
  PixelFormatExtended  = $00100000; // Extended color 16 bits/channel
  PixelFormatCanonical = $00200000;

  PixelFormatUndefined = 0;
  PixelFormatDontCare  = 0;

  PixelFormat1bppIndexed    = (1 or (1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed    = (2 or (4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed    = (3 or (8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4 or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555    = (5 or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565    = (6 or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555  = (7 or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB       = (8 or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB       = (9 or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormat32bppCMYK      = (15 or (32 shl 8));
  PixelFormatMax            = 16;

Function GetPixelFormatSize(pixfmt: TPixelFormat): UINT;{$IFDEF CanInline} inline;{$ENDIF}

Function IsIndexedPixelFormat(pixfmt: TPixelFormat): BOOL;{$IFDEF CanInline} inline;{$ENDIF}

Function IsAlphaPixelFormat(pixfmt: TPixelFormat): BOOL;{$IFDEF CanInline} inline;{$ENDIF}

Function IsExtendedPixelFormat(pixfmt: TPixelFormat): BOOL;{$IFDEF CanInline} inline;{$ENDIF}

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

Function IsCanonicalPixelFormat(pixfmt: TPixelFormat): BOOL;{$IFDEF CanInline} inline;{$ENDIF}

{$IF GDIPVER >= $0110}
//----------------------------------------------------------------------------
// Color format conversion parameters
//----------------------------------------------------------------------------
type
  TPaletteType = (
    // Arbitrary custom palette provided by caller.
    
    PaletteTypeCustom           = 0,
    
    // Optimal palette generated using a median-cut algorithm.
    
    PaletteTypeOptimal          = 1,
    
    // Black and white palette.
    
    PaletteTypeFixedBW          = 2,
    
    // Symmetric halftone palettes.
    // Each of these halftone palettes will be a superset of the system palette.
    // E.g. Halftone8 will have it's 8-color on-off primaries and the 16 system
    // colors added. With duplicates removed, that leaves 16 colors.
    
    PaletteTypeFixedHalftone8   = 3, // 8-color, on-off primaries
    PaletteTypeFixedHalftone27  = 4, // 3 intensity levels of each color
    PaletteTypeFixedHalftone64  = 5, // 4 intensity levels of each color
    PaletteTypeFixedHalftone125 = 6, // 5 intensity levels of each color
    PaletteTypeFixedHalftone216 = 7, // 6 intensity levels of each color

    // Assymetric halftone palettes.
    // These are somewhat less useful than the symmetric ones, but are 
    // included for completeness. These do not include all of the system
    // colors.
    
    PaletteTypeFixedHalftone252 = 8, // 6-red, 7-green, 6-blue intensities
    PaletteTypeFixedHalftone256 = 9  // 8-red, 8-green, 4-blue intensities
  );
  PPaletteType = ^TPaletteType;

  TDitherType = (
    DitherTypeNone           = 0,
    
    // Solid color - picks the nearest matching color with no attempt to 
    // halftone or dither. May be used on an arbitrary palette.
    
    DitherTypeSolid          = 1,
    
    // Ordered dithers and spiral dithers must be used with a fixed palette.
    
    // NOTE: DitherOrdered4x4 is unique in that it may apply to 16bpp 
    // conversions also.
    
    DitherTypeOrdered4x4     = 2,
    
    DitherTypeOrdered8x8     = 3,
    DitherTypeOrdered16x16   = 4,
    DitherTypeSpiral4x4      = 5,
    DitherTypeSpiral8x8      = 6,
    DitherTypeDualSpiral4x4  = 7,
    DitherTypeDualSpiral8x8  = 8,
    
    // Error diffusion. May be used with any palette.
    
    DitherTypeErrorDiffusion = 9,

    DitherTypeMax            = 10
  );
  PDitherType = ^TDitherType;
{$IFEND}

type
  TPaletteFlags = UINT;     PPaletteFlags = ^TPaletteFlags;
const
  PaletteFlagsHasAlpha  = $0001;
  PaletteFlagsGrayScale = $0002;
  PaletteFlagsHalftone  = $0004;

type
  TColorPalette = record
    Flags:    UINT;                 // Palette flags
    Count:    UINT;                 // Number of color entries
    Entries:  array[0..0] of TARGB; // Palette color entries
  end;
  PColorPalette = ^TColorPalette;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusColor.h
*
* Abstract:
*
*   GDI+ Color Object
*
\**************************************************************************)
//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------
type
  TColorMode = (
    ColorModeARGB32 = 0,
    ColorModeARGB64 = 1
  );
  PColorMode = ^TColorMode;

//----------------------------------------------------------------------------
// Color Channel flags
//----------------------------------------------------------------------------
type
  TColorChannelFlags = (
    ColorChannelFlagsC = 0,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );
  PColorChannelFlags = ^TColorChannelFlags;

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------
const
  // Common color constants
  AliceBlue            = $FFF0F8FF;
  AntiqueWhite         = $FFFAEBD7;
  Aqua                 = $FF00FFFF;
  Aquamarine           = $FF7FFFD4;
  Azure                = $FFF0FFFF;
  Beige                = $FFF5F5DC;
  Bisque               = $FFFFE4C4;
  Black                = $FF000000;
  BlanchedAlmond       = $FFFFEBCD;
  Blue                 = $FF0000FF;
  BlueViolet           = $FF8A2BE2;
  Brown                = $FFA52A2A;
  BurlyWood            = $FFDEB887;
  CadetBlue            = $FF5F9EA0;
  Chartreuse           = $FF7FFF00;
  Chocolate            = $FFD2691E;
  Coral                = $FFFF7F50;
  CornflowerBlue       = $FF6495ED;
  Cornsilk             = $FFFFF8DC;
  Crimson              = $FFDC143C;
  Cyan                 = $FF00FFFF;
  DarkBlue             = $FF00008B;
  DarkCyan             = $FF008B8B;
  DarkGoldenrod        = $FFB8860B;
  DarkGray             = $FFA9A9A9;
  DarkGreen            = $FF006400;
  DarkKhaki            = $FFBDB76B;
  DarkMagenta          = $FF8B008B;
  DarkOliveGreen       = $FF556B2F;
  DarkOrange           = $FFFF8C00;
  DarkOrchid           = $FF9932CC;
  DarkRed              = $FF8B0000;
  DarkSalmon           = $FFE9967A;
  DarkSeaGreen         = $FF8FBC8B;
  DarkSlateBlue        = $FF483D8B;
  DarkSlateGray        = $FF2F4F4F;
  DarkTurquoise        = $FF00CED1;
  DarkViolet           = $FF9400D3;
  DeepPink             = $FFFF1493;
  DeepSkyBlue          = $FF00BFFF;
  DimGray              = $FF696969;
  DodgerBlue           = $FF1E90FF;
  Firebrick            = $FFB22222;
  FloralWhite          = $FFFFFAF0;
  ForestGreen          = $FF228B22;
  Fuchsia              = $FFFF00FF;
  Gainsboro            = $FFDCDCDC;
  GhostWhite           = $FFF8F8FF;
  Gold                 = $FFFFD700;
  Goldenrod            = $FFDAA520;
  Gray                 = $FF808080;
  Green                = $FF008000;
  GreenYellow          = $FFADFF2F;
  Honeydew             = $FFF0FFF0;
  HotPink              = $FFFF69B4;
  IndianRed            = $FFCD5C5C;
  Indigo               = $FF4B0082;
  Ivory                = $FFFFFFF0;
  Khaki                = $FFF0E68C;
  Lavender             = $FFE6E6FA;
  LavenderBlush        = $FFFFF0F5;
  LawnGreen            = $FF7CFC00;
  LemonChiffon         = $FFFFFACD;
  LightBlue            = $FFADD8E6;
  LightCoral           = $FFF08080;
  LightCyan            = $FFE0FFFF;
  LightGoldenrodYellow = $FFFAFAD2;
  LightGray            = $FFD3D3D3;
  LightGreen           = $FF90EE90;
  LightPink            = $FFFFB6C1;
  LightSalmon          = $FFFFA07A;
  LightSeaGreen        = $FF20B2AA;
  LightSkyBlue         = $FF87CEFA;
  LightSlateGray       = $FF778899;
  LightSteelBlue       = $FFB0C4DE;
  LightYellow          = $FFFFFFE0;
  Lime                 = $FF00FF00;
  LimeGreen            = $FF32CD32;
  Linen                = $FFFAF0E6;
  Magenta              = $FFFF00FF;
  Maroon               = $FF800000;
  MediumAquamarine     = $FF66CDAA;
  MediumBlue           = $FF0000CD;
  MediumOrchid         = $FFBA55D3;
  MediumPurple         = $FF9370DB;
  MediumSeaGreen       = $FF3CB371;
  MediumSlateBlue      = $FF7B68EE;
  MediumSpringGreen    = $FF00FA9A;
  MediumTurquoise      = $FF48D1CC;
  MediumVioletRed      = $FFC71585;
  MidnightBlue         = $FF191970;
  MintCream            = $FFF5FFFA;
  MistyRose            = $FFFFE4E1;
  Moccasin             = $FFFFE4B5;
  NavajoWhite          = $FFFFDEAD;
  Navy                 = $FF000080;
  OldLace              = $FFFDF5E6;
  Olive                = $FF808000;
  OliveDrab            = $FF6B8E23;
  Orange               = $FFFFA500;
  OrangeRed            = $FFFF4500;
  Orchid               = $FFDA70D6;
  PaleGoldenrod        = $FFEEE8AA;
  PaleGreen            = $FF98FB98;
  PaleTurquoise        = $FFAFEEEE;
  PaleVioletRed        = $FFDB7093;
  PapayaWhip           = $FFFFEFD5;
  PeachPuff            = $FFFFDAB9;
  Peru                 = $FFCD853F;
  Pink                 = $FFFFC0CB;
  Plum                 = $FFDDA0DD;
  PowderBlue           = $FFB0E0E6;
  Purple               = $FF800080;
  Red                  = $FFFF0000;
  RosyBrown            = $FFBC8F8F;
  RoyalBlue            = $FF4169E1;
  SaddleBrown          = $FF8B4513;
  Salmon               = $FFFA8072;
  SandyBrown           = $FFF4A460;
  SeaGreen             = $FF2E8B57;
  SeaShell             = $FFFFF5EE;
  Sienna               = $FFA0522D;
  Silver               = $FFC0C0C0;
  SkyBlue              = $FF87CEEB;
  SlateBlue            = $FF6A5ACD;
  SlateGray            = $FF708090;
  Snow                 = $FFFFFAFA;
  SpringGreen          = $FF00FF7F;
  SteelBlue            = $FF4682B4;
  Tan                  = $FFD2B48C;
  Teal                 = $FF008080;
  Thistle              = $FFD8BFD8;
  Tomato               = $FFFF6347;
  Transparent          = $00FFFFFF;
  Turquoise            = $FF40E0D0;
  Violet               = $FFEE82EE;
  Wheat                = $FFF5DEB3;
  White                = $FFFFFFFF;
  WhiteSmoke           = $FFF5F5F5;
  Yellow               = $FFFFFF00;
  YellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components
  AlphaShift = 24;
  RedShift   = 16;
  GreenShift = 8;
  BlueShift  = 0;

  AlphaMask = $ff000000;
  RedMask   = $00ff0000;
  GreenMask = $0000ff00;
  BlueMask  = $000000ff;

{!!-----------------------------------------------------------------------------
    TColor - declaration
-------------------------------------------------------------------------------}
type
  TColor = record
    Argb:  TARGB;
  end;
  PColor = ^TColor;

  //!! do not use TColorArray, it is here only for declaration of PColorArray
  TColorArray = array[0..Pred(MaxInt div SizeOf(TColor))] of TColor;
  PColorArray = ^TColorArray;

// Assemble A, R, G, B values into a 32-bit integer
Function MakeARGB(a,r,g,b: Byte): TARGB;

Function Color: TColor; overload;
// Construct an opaque Color object with
// the specified Red, Green, Blue values.
//
// Color values are not premultiplied.
Function Color(r,g,b: Byte): TColor; overload;
Function Color(a,r,g,b: Byte): TColor; overload;
Function Color(argb: TARGB): TColor; overload;

Function GetAlpha(const cl: TColor): Byte;
Function GetA(const cl: TColor): Byte;
Function GetRed(const cl: TColor): Byte;
Function GetR(const cl: TColor): Byte;
Function GetGreen(const cl: TColor): Byte;
Function GetG(const cl: TColor): Byte;
Function GetBlue(const cl: TColor): Byte;
Function GetB(const cl: TColor): Byte;

Function GetValue(const cl: TColor): TARGB; overload;
procedure SetValue(var cl: TColor; argb: TARGB); overload;
procedure SetFromCOLORREF(var cl: TColor; rgb: TCOLORREF);
Function ToCOLORREF(const cl: TColor): TCOLORREF;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   Metafile headers
*
* Abstract:
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)
type
  TENHMETAHEADER3 = record
    iType:          DWORD;    // Record type EMR_HEADER
    nSize:          DWORD;    // Record size in bytes.  This may be greater
                              // than the sizeof(ENHMETAHEADER).
    rclBounds:      RECTL;    // Inclusive-inclusive bounds in device units
    rclFrame:       RECTL;    // Inclusive-inclusive Picture Frame .01mm unit
    dSignature:     DWORD;    // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion:       DWORD;    // Version number
    nBytes:         DWORD;    // Size of the metafile in bytes
    nRecords:       DWORD;    // Number of records in the metafile
    nHandles:       WORD;     // Number of handles in the handle table
                              // Handle index zero is reserved.
    sReserved:      WORD;     // Reserved.  Must be zero.
    nDescription:   DWORD;    // Number of chars in the unicode desc string
                              // This is 0 if there is no description string
    offDescription: DWORD;    // Offset to the metafile description record.
                              // This is 0 if there is no description string
    nPalEntries:    DWORD;    // Number of entries in the metafile palette.
    szlDevice:      SIZEL;    // Size of the reference device in pels
    szlMillimeters: SIZEL;    // Size of the reference device in millimeters
  end;
  PENHMETAHEADER3 = ^TENHMETAHEADER3;

// Placeable WMFs

// Placeable Metafiles were created as a non-standard way of specifying how 
// a metafile is mapped and scaled on an output device.
// Placeable metafiles are quite wide-spread, but not directly supported by
// the Windows API. To playback a placeable metafile using the Windows API,
// you will first need to strip the placeable metafile header from the file.
// This is typically performed by copying the metafile to a temporary file
// starting at file offset 22 (0x16). The contents of the temporary file may
// then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
// CopyMetaFile(), etc. GDI functions.

// Each placeable metafile begins with a 22-byte header,
//  followed by a standard metafile:

{$ALIGN 2}  //!! TWmfPlaceableFileHeader should be 22 bytes

type
  TPWMFRect16 = record
    Left:   INT16;
    Top:    INT16;
    Right:  INT16;
    Bottom: INT16;
  end;
  PPWMFRect16 = ^TPWMFRect16;

  TWmfPlaceableFileHeader = record
    Key:          UINT32;       // GDIP_WMF_PLACEABLEKEY
    Hmf:          INT16;        // Metafile HANDLE number (always 0)
    BoundingBox:  TPWMFRect16;  // Coordinates in metafile units
    Inch:         INT16;        // Number of metafile units per inch
    Reserved:     UINT32;       // Reserved (always 0)
    Checksum:     INT16;        // Checksum value for previous 10 WORDs
  end;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

{$ALIGN 8}

// Key contains a special identification value that indicates the presence
// of a placeable metafile header and is always 0x9AC6CDD7.

// Handle is used to stored the handle of the metafile in memory. When written
// to disk, this field is not used and will always contains the value 0.

// Left, Top, Right, and Bottom contain the coordinates of the upper-left
// and lower-right corners of the image on the output device. These are
// measured in twips.

// A twip (meaning "twentieth of a point") is the logical unit of measurement
// used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
// twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

// Inch contains the number of twips per inch used to represent the image.
// Normally, there are 1440 twips per inch; however, this number may be
// changed to scale the image. A value of 720 indicates that the image is
// double its normal size, or scaled to a factor of 2:1. A value of 360
// indicates a scale of 4:1, while a value of 2880 indicates that the image
// is scaled down in size by a factor of two. A value of 1440 indicates
// a 1:1 scale ratio.

// Reserved is not used and is always set to 0.

// Checksum contains a checksum value for the previous 10 WORDs in the header.
// This value can be used in an attempt to detect if the metafile has become
// corrupted. The checksum is calculated by XORing each WORD value to an
// initial value of 0.

// If the metafile was recorded with a reference Hdc that was a display.
const
  GDIP_EMFPLUSFLAGS_DISPLAY = $00000001;

type
  TMetafileHeader = record
    Type_:                    TMetafileType;
    Size:                     UINT;     // Size of the metafile (in bytes)
    Version:                  UINT;     // EMF+, EMF, or WMF version
    EmfPlusFlags:             UINT;
    DpiX:                     REAL;
    DpiY:                     REAL;
    X:                        INT;      // Bounds in device units
    Y:                        INT;
    Width:                    INT;
    Height:                   INT;
    case Integer of
      0: (WmfHeader:          METAHEADER);
      1: (EmfHeader:          TENHMETAHEADER3;
          EmfPlusHeaderSize:  INT;      // size of the EMF+ header in file
          LogicalDpiX:        INT;      // Logical Dpi of reference Hdc
          LogicalDpiY:        INT)      // usually valid only for EMF+
  end;
  PMetafileHeader = ^TMetafileHeader;

Function GetType(const header: TMetafileHeader): TMetafileType;
Function GetMetafileSize(const header: TMetafileHeader): UINT;
// If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
Function GetVersion(const header: TMetafileHeader): UINT;
// Get the EMF+ flags associated with the metafile
Function GetEmfPlusFlags(const header: TMetafileHeader): UINT;
Function GetDpiX(const header: TMetafileHeader): REAL;
Function GetDpiY(const header: TMetafileHeader): REAL;
Function GetBounds(const header: TMetafileHeader): TRect; overload;

// Is it any type of WMF (standard or Placeable Metafile)?
Function IsWmf(const header: TMetafileHeader): BOOL;
// Is this an Placeable Metafile?
Function IsWmfPlaceable(const header: TMetafileHeader): BOOL;
// Is this an EMF (not an EMF+)?
Function IsEmf(const header: TMetafileHeader): BOOL;
// Is this an EMF or EMF+ file?
Function IsEmfOrEmfPlus(const header: TMetafileHeader): BOOL;
// Is this an EMF+ file?
Function IsEmfPlus(const header: TMetafileHeader): BOOL;
// Is this an EMF+ dual (has dual, down-level records) file?
Function IsEmfPlusDual(const header: TMetafileHeader): BOOL;
// Is this an EMF+ only (no dual records) file?
Function IsEmfPlusOnly(const header: TMetafileHeader): BOOL;
// If it's an EMF+ file, was it recorded against a display Hdc?
Function IsDisplay(const header: TMetafileHeader): BOOL;
// Get the WMF header of the metafile (if it is a WMF)
Function GetWmfHeader(const header: TMetafileHeader): PMETAHEADER;
// Get the EMF header of the metafile (if it is an EMF)
Function GetEmfHeader(const header: TMetafileHeader): PENHMETAHEADER3;


(**************************************************************************\
* 
* Copyright (c) 1999-2000  Microsoft Corporation
*
* Module Name:
*
*   GdiplusImaging.h
*
* Abstract:
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)
//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------
const
  ImageFormatUndefined: TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatMemoryBMP: TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatBMP:       TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEMF:       TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatWMF:       TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatJPEG:      TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatPNG:       TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatGIF:       TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatTIFF:      TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEXIF:      TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatIcon:      TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatHEIF:      TGUID = '{b96b3cb6-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatWEBP:      TGUID = '{b96b3cb7-0728-11d3-9d7b-0000f81ef32e}';

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime:       TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  FrameDimensionResolution: TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  FrameDimensionPage:       TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation: TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  FormatIDJpegAppHeaders:   TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression:      TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  EncoderColorDepth:       TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  EncoderScanMethod:       TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  EncoderVersion:          TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  EncoderRenderMethod:     TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  EncoderQuality:          TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  EncoderTransformation:   TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  EncoderLuminanceTable:   TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  EncoderChrominanceTable: TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  EncoderSaveFlag:         TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';

{$IF GDIPVER >= $0110}
  EncoderColorSpace: TGUID = '{ae7a62a0-ee2c-49d8-9d07-1ba8a927596e}';
  EncoderImageItems: TGUID = '{63875e13-1f1d-45ab-9195-a29b6066a650}';
  EncoderSaveAsCMYK: TGUID = '{a219bbc9-0a9d-4005-a3ee-3a421b8bb06c}';
{$IFEND}

  CodecIImageBytes: TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';

type
  IImageBytes = interface(IUnknown)
  ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    Function CountBytes(pcb: PUINT): HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    Function LockBytes(cb: UINT; ulOffset: ULONG; ppvBytes: PPointer): HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    Function UnlockBytes(pvBytes: Pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------
type
  TImageCodecInfo = record
    Clsid:              TCLSID;
    FormatID:           TGUID;
    CodecName:          PWideChar;
    DllName:            PWideChar;
    FormatDescription:  PWideChar;
    FilenameExtension:  PWideChar;
    MimeType:           PWideChar;
    Flags:              DWORD;
    Version:            DWORD;
    SigCount:           DWORD;
    SigSize:            DWORD;
    SigPattern:         PByte;
    SigMask:            PByte;
  end;
  PImageCodecInfo = ^TImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
type
  TImageCodecFlags = DWORD;   PImageCodecFlags = ^TImageCodecFlags;
const
  ImageCodecFlagsEncoder        = $00000001;
  ImageCodecFlagsDecoder        = $00000002;
  ImageCodecFlagsSupportBitmap  = $00000004;
  ImageCodecFlagsSupportVector  = $00000008;
  ImageCodecFlagsSeekableEncode = $00000010;
  ImageCodecFlagsBlockingDecode = $00000020;

  ImageCodecFlagsBuiltin        = $00010000;
  ImageCodecFlagsSystem         = $00020000;
  ImageCodecFlagsUser           = $00040000;

//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------
type
  TImageLockMode = UINT;    PImageLockMode = ^TImageLockMode;
const
  ImageLockModeRead         = $0001;
  ImageLockModeWrite        = $0002;
  ImageLockModeUserInputBuf = $0004;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------
type
  TBitmapData = record
    Width:        UINT;
    Height:       UINT;
    Stride:       INT;
    PixelFormat:  TPixelFormat;
    Scan0:        Pointer;
    Reserved:     UINT_PTR;
  end;
  PBitmapData = ^TBitmapData;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
type
  TImageFlags = UINT;   PImageFlags = ^TImageFlags;
const
  ImageFlagsNone              = 0;

  // Low-word: shared with SINKFLAG_x

  ImageFlagsScalable          = $0001;
  ImageFlagsHasAlpha          = $0002;
  ImageFlagsHasTranslucent    = $0004;
  ImageFlagsPartiallyScalable = $0008;

  // Low-word: color space definition

  ImageFlagsColorSpaceRGB     = $0010;
  ImageFlagsColorSpaceCMYK    = $0020;
  ImageFlagsColorSpaceGRAY    = $0040;
  ImageFlagsColorSpaceYCBCR   = $0080;
  ImageFlagsColorSpaceYCCK    = $0100;

  // Low-word: image size info

  ImageFlagsHasRealDPI        = $1000;
  ImageFlagsHasRealPixelSize  = $2000;

  // High-word

  ImageFlagsReadOnly          = $00010000;
  ImageFlagsCaching           = $00020000;

type
  TRotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    RotateNoneFlipY    = Rotate180FlipX,
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  PRotateFlipType = ^TRotateFlipType;

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------
type
  TEncoderParameter = record
    Guid:           TGUID;      // GUID of the parameter
    NumberOfValues: ULONG;      // Number of the parameter values
    Type_:          ULONG;      // Value type, like ValueTypeLONG  etc.
    Value:          Pointer;    // A pointer to the parameter values
  end;
  PEncoderParameter = ^TEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------
type
  TEncoderParameters = record
    Count:      UINT;                             // Number of parameters in this structure
    Parameter:  array[0..0] of TEncoderParameter; // Parameter values
  end;
  PEncoderParameters = ^TEncoderParameters;

{$IF GDIPVER >= $0110}
  TItemDataPosition = (
    ItemDataPositionAfterHeader  = $0,
    ItemDataPositionAfterPalette = $1,
    ItemDataPositionAfterBits    = $2
  );
  PItemDataPosition = ^TItemDataPosition;

//---------------------------------------------------------------------------
// External Data Item
//---------------------------------------------------------------------------
type
  TImageItemData = record
    Size:     UINT;       // size of the structure
    Position: UINT;       // flags describing how the data is to be used.
    Desc:     Pointer;    // description on how the data is to be saved.
                          // it is different for every codec type.
    DescSize: UINT;       // size memory pointed by Desc
    Data:     Pointer;    // pointer to the data that is to be saved in the
                          // file, could be anything saved directly.
    DataSize: UINT;       // size memory pointed by Data
    Cookie:   UINT;       // opaque for the apps data member used during
                          // enumeration of image data items.
  end;
  PImageItemData = ^TImageItemData;

{$IFEND}

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------
type
  TPropertyItem = record
    id:     PROPID;     // ID of this property
    length: ULONG;      // Length of the property value, in bytes
    type_:  WORD;       // Type of the value, as one of TAG_TYPE_XXX
                        // defined above
    value:  Pointer;    // property value
  end;
  PPropertyItem = ^TPropertyItem;

//---------------------------------------------------------------------------
// Image property types 
//---------------------------------------------------------------------------
const
  PropertyTagTypeByte      =  1;
  PropertyTagTypeASCII     =  2;
  PropertyTagTypeShort     =  3;
  PropertyTagTypeLong      =  4;
  PropertyTagTypeRational  =  5;
  PropertyTagTypeUndefined =  7;
  PropertyTagTypeSLONG     =  9;
  PropertyTagTypeSRational = 10;

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------
const
  PropertyTagExifIFD                = $8769;
  PropertyTagGpsIFD                 = $8825;

  PropertyTagNewSubfileType         = $00FE;
  PropertyTagSubfileType            = $00FF;
  PropertyTagImageWidth             = $0100;
  PropertyTagImageHeight            = $0101;
  PropertyTagBitsPerSample          = $0102;
  PropertyTagCompression            = $0103;
  PropertyTagPhotometricInterp      = $0106;
  PropertyTagThreshHolding          = $0107;
  PropertyTagCellWidth              = $0108;
  PropertyTagCellHeight             = $0109;
  PropertyTagFillOrder              = $010A;
  PropertyTagDocumentName           = $010D;
  PropertyTagImageDescription       = $010E;
  PropertyTagEquipMake              = $010F;
  PropertyTagEquipModel             = $0110;
  PropertyTagStripOffsets           = $0111;
  PropertyTagOrientation            = $0112;
  PropertyTagSamplesPerPixel        = $0115;
  PropertyTagRowsPerStrip           = $0116;
  PropertyTagStripBytesCount        = $0117;
  PropertyTagMinSampleValue         = $0118;
  PropertyTagMaxSampleValue         = $0119;
  PropertyTagXResolution            = $011A;    // Image resolution in width direction
  PropertyTagYResolution            = $011B;    // Image resolution in height direction
  PropertyTagPlanarConfig           = $011C;    // Image data arrangement
  PropertyTagPageName               = $011D;
  PropertyTagXPosition              = $011E;
  PropertyTagYPosition              = $011F;
  PropertyTagFreeOffset             = $0120;
  PropertyTagFreeByteCounts         = $0121;
  PropertyTagGrayResponseUnit       = $0122;
  PropertyTagGrayResponseCurve      = $0123;
  PropertyTagT4Option               = $0124;
  PropertyTagT6Option               = $0125;
  PropertyTagResolutionUnit         = $0128;   // Unit of X and Y resolution
  PropertyTagPageNumber             = $0129;
  PropertyTagTransferFuncition      = $012D;
  PropertyTagSoftwareUsed           = $0131;
  PropertyTagDateTime               = $0132;
  PropertyTagArtist                 = $013B;
  PropertyTagHostComputer           = $013C;
  PropertyTagPredictor              = $013D;
  PropertyTagWhitePoint             = $013E;
  PropertyTagPrimaryChromaticities  = $013F;
  PropertyTagColorMap               = $0140;
  PropertyTagHalftoneHints          = $0141;
  PropertyTagTileWidth              = $0142;
  PropertyTagTileLength             = $0143;
  PropertyTagTileOffset             = $0144;
  PropertyTagTileByteCounts         = $0145;
  PropertyTagInkSet                 = $014C;
  PropertyTagInkNames               = $014D;
  PropertyTagNumberOfInks           = $014E;
  PropertyTagDotRange               = $0150;
  PropertyTagTargetPrinter          = $0151;
  PropertyTagExtraSamples           = $0152;
  PropertyTagSampleFormat           = $0153;
  PropertyTagSMinSampleValue        = $0154;
  PropertyTagSMaxSampleValue        = $0155;
  PropertyTagTransferRange          = $0156;

  PropertyTagJPEGProc               = $0200;
  PropertyTagJPEGInterFormat        = $0201;
  PropertyTagJPEGInterLength        = $0202;
  PropertyTagJPEGRestartInterval    = $0203;
  PropertyTagJPEGLosslessPredictors = $0205;
  PropertyTagJPEGPointTransforms    = $0206;
  PropertyTagJPEGQTables            = $0207;
  PropertyTagJPEGDCTables           = $0208;
  PropertyTagJPEGACTables           = $0209;

  PropertyTagYCbCrCoefficients      = $0211;
  PropertyTagYCbCrSubsampling       = $0212;
  PropertyTagYCbCrPositioning       = $0213;
  PropertyTagREFBlackWhite          = $0214;

  PropertyTagICCProfile             = $8773;    // This TAG is defined by ICC
                                                // for embedded ICC in TIFF
  PropertyTagGamma                  = $0301;
  PropertyTagICCProfileDescriptor   = $0302;
  PropertyTagSRGBRenderingIntent    = $0303;

  PropertyTagImageTitle             = $0320;
  PropertyTagCopyright              = $8298;

  // Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  PropertyTagResolutionYUnit           = $5002;
  PropertyTagResolutionXLengthUnit     = $5003;
  PropertyTagResolutionYLengthUnit     = $5004;
  PropertyTagPrintFlags                = $5005;
  PropertyTagPrintFlagsVersion         = $5006;
  PropertyTagPrintFlagsCrop            = $5007;
  PropertyTagPrintFlagsBleedWidth      = $5008;
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  PropertyTagHalftoneLPI               = $500A;
  PropertyTagHalftoneLPIUnit           = $500B;
  PropertyTagHalftoneDegree            = $500C;
  PropertyTagHalftoneShape             = $500D;
  PropertyTagHalftoneMisc              = $500E;
  PropertyTagHalftoneScreen            = $500F;
  PropertyTagJPEGQuality               = $5010;
  PropertyTagGridSize                  = $5011;
  PropertyTagThumbnailFormat           = $5012;   // 1 = JPEG, 0 = RAW RGB
  PropertyTagThumbnailWidth            = $5013;
  PropertyTagThumbnailHeight           = $5014;
  PropertyTagThumbnailColorDepth       = $5015;
  PropertyTagThumbnailPlanes           = $5016;
  PropertyTagThumbnailRawBytes         = $5017;
  PropertyTagThumbnailSize             = $5018;
  PropertyTagThumbnailCompressedSize   = $5019;
  PropertyTagColorTransferFunction     = $501A;
  PropertyTagThumbnailData             = $501B;   // RAW thumbnail bits in
                                                  // JPEG format or RGB format
                                                  // depends on
                                                  // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth            = $5020;  // Thumbnail width
  PropertyTagThumbnailImageHeight           = $5021;  // Thumbnail height
  PropertyTagThumbnailBitsPerSample         = $5022;  // Number of bits per
                                                      // component
  PropertyTagThumbnailCompression           = $5023;  // Compression Scheme
  PropertyTagThumbnailPhotometricInterp     = $5024;  // Pixel composition
  PropertyTagThumbnailImageDescription      = $5025;  // Image Tile
  PropertyTagThumbnailEquipMake             = $5026;  // Manufacturer of Image
                                                      // Input equipment
  PropertyTagThumbnailEquipModel            = $5027;  // Model of Image input
                                                      // equipment
  PropertyTagThumbnailStripOffsets          = $5028;  // Image data location
  PropertyTagThumbnailOrientation           = $5029;  // Orientation of image
  PropertyTagThumbnailSamplesPerPixel       = $502A;  // Number of components
  PropertyTagThumbnailRowsPerStrip          = $502B;  // Number of rows per strip
  PropertyTagThumbnailStripBytesCount       = $502C;  // Bytes per compressed
                                                      // strip
  PropertyTagThumbnailResolutionX           = $502D;  // Resolution in width
                                                      // direction
  PropertyTagThumbnailResolutionY           = $502E;  // Resolution in height
                                                      // direction
  PropertyTagThumbnailPlanarConfig          = $502F;  // Image data arrangement
  PropertyTagThumbnailResolutionUnit        = $5030;  // Unit of X and Y
                                                      // Resolution
  PropertyTagThumbnailTransferFunction      = $5031;  // Transfer function
  PropertyTagThumbnailSoftwareUsed          = $5032;  // Software used
  PropertyTagThumbnailDateTime              = $5033;  // File change date and
                                                      // time
  PropertyTagThumbnailArtist                = $5034;  // Person who created the
                                                      // image
  PropertyTagThumbnailWhitePoint            = $5035;  // White point chromaticity
  PropertyTagThumbnailPrimaryChromaticities = $5036;
                                                      // Chromaticities of
                                                      // primaries
  PropertyTagThumbnailYCbCrCoefficients     = $5037;  // Color space transforma-
                                                      // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling      = $5038;  // Subsampling ratio of Y
                                                      // to C
  PropertyTagThumbnailYCbCrPositioning      = $5039;  // Y and C position
  PropertyTagThumbnailRefBlackWhite         = $503A;  // Pair of black and white
                                                      // reference values
  PropertyTagThumbnailCopyRight             = $503B;  // CopyRight holder

  PropertyTagLuminanceTable                 = $5090;
  PropertyTagChrominanceTable               = $5091;

  PropertyTagFrameDelay                     = $5100;
  PropertyTagLoopCount                      = $5101;

{$IF GDIPVER >= $0110}
  PropertyTagGlobalPalette                  = $5102;
  PropertyTagIndexBackground                = $5103;
  PropertyTagIndexTransparent               = $5104;
{$IFEND}

  PropertyTagPixelUnit        = $5110;  // Unit specifier for pixel/unit
  PropertyTagPixelPerUnitX    = $5111;  // Pixels per unit in X
  PropertyTagPixelPerUnitY    = $5112;  // Pixels per unit in Y
  PropertyTagPaletteHistogram = $5113;  // Palette histogram

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  PropertyTagExifFNumber       = $829D;

  PropertyTagExifExposureProg  = $8822;
  PropertyTagExifSpectralSense = $8824;
  PropertyTagExifISOSpeed      = $8827;
  PropertyTagExifOECF          = $8828;

  PropertyTagExifVer           = $9000;
  PropertyTagExifDTOrig        = $9003;   // Date & time of original
  PropertyTagExifDTDigitized   = $9004;   // Date & time of digital data generation

  PropertyTagExifCompConfig    = $9101;
  PropertyTagExifCompBPP       = $9102;

  PropertyTagExifShutterSpeed  = $9201;
  PropertyTagExifAperture      = $9202;
  PropertyTagExifBrightness    = $9203;
  PropertyTagExifExposureBias  = $9204;
  PropertyTagExifMaxAperture   = $9205;
  PropertyTagExifSubjectDist   = $9206;
  PropertyTagExifMeteringMode  = $9207;
  PropertyTagExifLightSource   = $9208;
  PropertyTagExifFlash         = $9209;
  PropertyTagExifFocalLength   = $920A;
  PropertyTagExifSubjectArea   = $9214;   // exif 2.2 Subject Area
  PropertyTagExifMakerNote     = $927C;
  PropertyTagExifUserComment   = $9286;
  PropertyTagExifDTSubsec      = $9290;   // Date & Time subseconds
  PropertyTagExifDTOrigSS      = $9291;   // Date & Time original subseconds
  PropertyTagExifDTDigSS       = $9292;   // Date & TIme digitized subseconds

  PropertyTagExifFPXVer        = $A000;
  PropertyTagExifColorSpace    = $A001;
  PropertyTagExifPixXDim       = $A002;
  PropertyTagExifPixYDim       = $A003;
  PropertyTagExifRelatedWav    = $A004;   // related sound file
  PropertyTagExifInterop       = $A005;
  PropertyTagExifFlashEnergy   = $A20B;
  PropertyTagExifSpatialFR     = $A20C;   // Spatial Frequency Response
  PropertyTagExifFocalXRes     = $A20E;   // Focal Plane X Resolution
  PropertyTagExifFocalYRes     = $A20F;   // Focal Plane Y Resolution
  PropertyTagExifFocalResUnit  = $A210;   // Focal Plane Resolution Unit
  PropertyTagExifSubjectLoc    = $A214;
  PropertyTagExifExposureIndex = $A215;
  PropertyTagExifSensingMethod = $A217;
  PropertyTagExifFileSource    = $A300;
  PropertyTagExifSceneType     = $A301;
  PropertyTagExifCfaPattern    = $A302;

  // New EXIF 2.2 properties

  PropertyTagExifCustomRendered        = $A401;
  PropertyTagExifExposureMode          = $A402;
  PropertyTagExifWhiteBalance          = $A403;
  PropertyTagExifDigitalZoomRatio      = $A404;
  PropertyTagExifFocalLengthIn35mmFilm = $A405;
  PropertyTagExifSceneCaptureType      = $A406;
  PropertyTagExifGainControl           = $A407;
  PropertyTagExifContrast              = $A408;
  PropertyTagExifSaturation            = $A409;
  PropertyTagExifSharpness             = $A40A;
  PropertyTagExifDeviceSettingDesc     = $A40B;
  PropertyTagExifSubjectDistanceRange  = $A40C;
  PropertyTagExifUniqueImageID         = $A420;


  PropertyTagGpsVer              = $0000;
  PropertyTagGpsLatitudeRef      = $0001;
  PropertyTagGpsLatitude         = $0002;
  PropertyTagGpsLongitudeRef     = $0003;
  PropertyTagGpsLongitude        = $0004;
  PropertyTagGpsAltitudeRef      = $0005;
  PropertyTagGpsAltitude         = $0006;
  PropertyTagGpsGpsTime          = $0007;
  PropertyTagGpsGpsSatellites    = $0008;
  PropertyTagGpsGpsStatus        = $0009;
  PropertyTagGpsGpsMeasureMode   = $000A;
  PropertyTagGpsGpsDop           = $000B;   // Measurement precision
  PropertyTagGpsSpeedRef         = $000C;
  PropertyTagGpsSpeed            = $000D;
  PropertyTagGpsTrackRef         = $000E;
  PropertyTagGpsTrack            = $000F;
  PropertyTagGpsImgDirRef        = $0010;
  PropertyTagGpsImgDir           = $0011;
  PropertyTagGpsMapDatum         = $0012;
  PropertyTagGpsDestLatRef       = $0013;
  PropertyTagGpsDestLat          = $0014;
  PropertyTagGpsDestLongRef      = $0015;
  PropertyTagGpsDestLong         = $0016;
  PropertyTagGpsDestBearRef      = $0017;
  PropertyTagGpsDestBear         = $0018;
  PropertyTagGpsDestDistRef      = $0019;
  PropertyTagGpsDestDist         = $001A;
  PropertyTagGpsProcessingMethod = $001B;
  PropertyTagGpsAreaInformation  = $001C;
  PropertyTagGpsDate             = $001D;
  PropertyTagGpsDifferential     = $001E;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusColorMatrix.h
*
* Abstract:
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)
{$IF GDIPVER >= $0110}
//----------------------------------------------------------------------------
// Color channel look up table (LUT)
//----------------------------------------------------------------------------
type
  TColorChannelLUT = array[0..255] of Byte;
  PColorChannelLUT = ^TColorChannelLUT;

//----------------------------------------------------------------------------
// Per-channel Histogram for 8bpp images.
//----------------------------------------------------------------------------
type
  THistogramFormat = (
    HistogramFormatARGB,
    HistogramFormatPARGB,
    HistogramFormatRGB,
    HistogramFormatGray,
    HistogramFormatB,
    HistogramFormatG,
    HistogramFormatR,
    HistogramFormatA
  );
  PHistogramFormat = ^THistogramFormat;
{$IFEND}

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------
type
  TColorMatrix = record
    m:  array[0..4,0..4] of REAL;
  end;
  PColorMatrix = ^TColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------
type
  TColorMatrixFlags = (
    ColorMatrixFlagsDefault   = 0,
    ColorMatrixFlagsSkipGrays = 1,
    ColorMatrixFlagsAltGray   = 2
  );
  PColorMatrixFlags = ^TColorMatrixFlags;

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------
type
  TColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );
  PColorAdjustType = ^TColorAdjustType;

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------
type
  TColorMap = record
    oldColor: TColor;
    newColor: TColor;
  end;
  PColorMap = ^TColorMap;


(**************************************************************************
*
* Copyright (c) 2001 Microsoft Corporation
*
* Module Name:
*
*   Gdiplus effect objects.
*
* Created:
*
*   05/29/2001 asecchia
*      Created it.
*
**************************************************************************)
{$IF GDIPVER >= $0110}

//-----------------------------------------------------------------------------
// GDI+ effect GUIDs
//----------------------------------------------------------------------------- 
const
  // {633C80A4-1843-482b-9EF2-BE2834C5FDD4}
  BlurEffectGuid: TGUID = '{633C80A4-1843-482b-9EF2-BE2834C5FDD4}';

  // {63CBF3EE-C526-402c-8F71-62C540BF5142}
  SharpenEffectGuid: TGUID = '{63CBF3EE-C526-402c-8F71-62C540BF5142}';

  // {718F2615-7933-40e3-A511-5F68FE14DD74}
  ColorMatrixEffectGuid: TGUID = '{718F2615-7933-40e3-A511-5F68FE14DD74}';

  // {A7CE72A9-0F7F-40d7-B3CC-D0C02D5C3212}
  ColorLUTEffectGuid: TGUID = '{A7CE72A9-0F7F-40d7-B3CC-D0C02D5C3212}';

  // {D3A1DBE1-8EC4-4c17-9F4C-EA97AD1C343D}
  BrightnessContrastEffectGuid: TGUID = '{D3A1DBE1-8EC4-4c17-9F4C-EA97AD1C343D}';

  // {8B2DD6C3-EB07-4d87-A5F0-7108E26A9C5F}
  HueSaturationLightnessEffectGuid: TGUID = '{8B2DD6C3-EB07-4d87-A5F0-7108E26A9C5F}';

  // {99C354EC-2A31-4f3a-8C34-17A803B33A25}
  LevelsEffectGuid: TGUID = '{99C354EC-2A31-4f3a-8C34-17A803B33A25}';

  // {1077AF00-2848-4441-9489-44AD4C2D7A2C}
  TintEffectGuid: TGUID = '{1077AF00-2848-4441-9489-44AD4C2D7A2C}';

  // {537E597D-251E-48da-9664-29CA496B70F8}
  ColorBalanceEffectGuid: TGUID = '{537E597D-251E-48da-9664-29CA496B70F8}';

  // {74D29D05-69A4-4266-9549-3CC52836B632}
  RedEyeCorrectionEffectGuid: TGUID = '{74D29D05-69A4-4266-9549-3CC52836B632}';

  // {DD6A0022-58E4-4a67-9D9B-D48EB881A53D}
  ColorCurveEffectGuid: TGUID = '{DD6A0022-58E4-4a67-9D9B-D48EB881A53D}';

//-----------------------------------------------------------------------------
type
  TSharpenParams = record
    radius: float;
    anount: float;
  end;
  PSharpenParams = ^TSharpenParams;

  TBlurParams = record
    radius:     float;
    expandEdge: BOOL;
  end;
  PBlurParams = ^TBlurParams;

  TBrightnessContrastParams = record
    brightnessLevel:  INT;
    contrastLevel:    INT;
  end;
  PBrightnessContrastParams = ^TBrightnessContrastParams;

  TRedEyeCorrectionParams = record
    numberOfAreas:  UINT;
    areas:          Windows.PRECT;
  end;
  PRedEyeCorrectionParams = ^TRedEyeCorrectionParams;

  THueSaturationLightnessParams = record
    hueLevel:         INT;
    saturationLevel:  INT;
    lightnessLevel:   INT;
  end;
  PHueSaturationLightnessParams = ^THueSaturationLightnessParams;

  TTintParams = record
    hue:    INT;
    amount: INT;
  end;
  PTintParams = ^TTintParams;

  TLevelsParams = record
    highlight:  INT;
    midtone:    INT;
    shadow:     INT;
  end;
  PLevelsParams = ^TLevelsParams;

  TColorBalanceParams = record
    cyanRed:      INT;
    magentaGreen: INT;
    yellowBlue:   INT;
  end;
  PColorBalanceParams = ^TColorBalanceParams;

  TColorLUTParams = record
    // look up tables for each color channel.
    lutB: TColorChannelLUT;
    lutG: TColorChannelLUT;
    lutR: TColorChannelLUT;
    lutA: TColorChannelLUT;
  end;
  PColorLUTParams = ^TColorLUTParams;

type
  TCurveAdjustments = (
    AdjustExposure,
    AdjustDensity,
    AdjustContrast,
    AdjustHighlight,
    AdjustShadow,
    AdjustMidtone,
    AdjustWhiteSaturation,
    AdjustBlackSaturation
  );
  PCurveAdjustments = ^TCurveAdjustments;

  TCurveChannel = (
    CurveChannelAll,
    CurveChannelRed,
    CurveChannelGreen,
    CurveChannelBlue
  );
  PCurveChannel = ^TCurveChannel;

type
  TColorCurveParams = record
    adjustment:   TCurveAdjustments;
    channel:      TCurveChannel;
    adjustValue:  INT;
  end;
  PColorCurveParams = ^TColorCurveParams;

type
  //!! zero-size placeholder type
  TCGpEffect  = record end;
  PCGpEffect  = ^TCGpEffect;
  PPCGpEffect = ^PCGpEffect;

Function GdipCreateEffect(guid: TGUID; effect: PPCGpEffect): TStatus; stdcall; external GDIPLIB;

Function GdipDeleteEffect(effect: PCGpEffect): TStatus; stdcall; external GDIPLIB;

Function GdipGetEffectParameterSize(effect: PCGpEffect; size: PUINT): TStatus; stdcall; external GDIPLIB;

Function GdipSetEffectParameters(effect: PCGpEffect; params: Pointer; size: UINT): TStatus; stdcall; external GDIPLIB;

Function GdipGetEffectParameters(effect: PCGpEffect; size: PUINT; params: Pointer): TStatus; stdcall; external GDIPLIB;

{!!=============================================================================
    TEffect - class declaration
===============================================================================}
type
  TEffect = class(TGdiPlusWrapper)
  protected
    // protected data members.
    fNativeEffect:  PCGpEffect;
    fAuxDataSize:   INT;
    fAuxData:       Pointer;
    fUseAuxData:    BOOL;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    Function SetParameters(Params: Pointer; Size: UINT): TStatus; virtual;
    Function GetParameters(Size: PUINT; Params: Pointer): TStatus; virtual; 
  public
    constructor Create;
    destructor Destroy; override;
    Function GetAuxDataSize: INT; 
    Function GetAuxData: Pointer; 
    procedure UseAuxData(UseAuxDataFlag: BOOL); 
    Function GetParameterSize(Size: PUINT): TStatus;
  end;

{!!=============================================================================
    TBlur - class declaration
===============================================================================}
// Blur
type
  TBlur = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PBlurParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PBlurParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TSharpen - class declaration
===============================================================================}
// Sharpen
type
  TSharpen = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PSharpenParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PSharpenParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TRedEyeCorrection - class declaration
===============================================================================}
// RedEye Correction
type
  TRedEyeCorrection = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PRedEyeCorrectionParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PRedEyeCorrectionParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TBrightnessContrast - class declaration
===============================================================================}
// Brightness/Contrast
type
  TBrightnessContrast = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PBrightnessContrastParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PBrightnessContrastParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    THueSaturationLightness - class declaration
===============================================================================}
// Hue/Saturation/Lightness
type
  THueSaturationLightness = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PHueSaturationLightnessParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PHueSaturationLightnessParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TLevels - class declaration
===============================================================================}
// Highlight/Midtone/Shadow curves
type
  TLevels = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PLevelsParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PLevelsParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TTint - class declaration
===============================================================================}
// Tint
type
  TTint = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PTintParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PTintParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TColorBalance - class declaration
===============================================================================}
// ColorBalance
type
  TColorBalance = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PColorBalanceParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PColorBalanceParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TColorMatrixEffect - class declaration
===============================================================================}
// ColorMatrix
type
  TColorMatrixEffect = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Matrix: PColorMatrix): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Matrix: PColorMatrix): TStatus; reintroduce;
  end;

{!!=============================================================================
    TColorLUT - class declaration
===============================================================================}
// ColorLUT
type
  TColorLUT = class(TEffect)
  public
    constructor Create;
    Function SetParameters(LUT: PColorLUTParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; LUT: PColorLUTParams): TStatus; reintroduce;
  end;

{!!=============================================================================
    TColorCurve - class declaration
===============================================================================}
// Color Curve
type
  TColorCurve = class(TEffect)
  public
    constructor Create;
    Function SetParameters(Parameters: PColorCurveParams): TStatus; reintroduce;
    Function GetParameters(Size: PUINT; Parameters: PColorCurveParams): TStatus; reintroduce;
  end;

{$IFEND}

(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusGpStubs.h
*
* Abstract:
*
*   Private GDI+ header file.
*
\**************************************************************************)
//---------------------------------------------------------------------------
// GDI+ classes for forward reference
//---------------------------------------------------------------------------
{!!
  Forward declarations removed.

  Original text:

class Graphics;
class Pen;
class Brush;
class Matrix;
class Bitmap;
class Metafile;
class GraphicsPath;
class PathIterator;
class Region;
class Image;
class TextureBrush;
class HatchBrush;
class SolidBrush;
class LinearGradientBrush;
class PathGradientBrush;
class Font;
class FontFamily;
class FontCollection;
class InstalledFontCollection;
class PrivateFontCollection;
class ImageAttributes;
class CachedBitmap;
}

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------
type
  TGpGraphics = record end;   PGpGraphics = ^TGpGraphics;   PPGpGraphics = ^PGpGraphics;

  TGpBrush        = record end;       PGpBrush        = ^TGpBrush;          PPGpBrush        = ^PGpBrush;
  TGpTexture      = type TGpBrush;    PGpTexture      = ^TGpTexture;        PPGpTexture      = ^PGpTexture;
  TGpSolidFill    = type TGpBrush;    PGpSolidFill    = ^TGpSolidFill;      PPGpSolidFill    = ^PGpSolidFill;
  TGpLineGradient = type TGpBrush;    PGpLineGradient = ^TGpLineGradient;   PPGpLineGradient = ^PGpLineGradient;
  TGpPathGradient = type TGpBrush;    PGpPathGradient = ^TGpPathGradient;   PPGpPathGradient = ^PGpPathGradient;
  TGpHatch        = type TGpBrush;    PGpHatch        = ^TGpHatch;          PPGpHatch        = ^PGpHatch;

  TGpPen                = record end;               PGpPen                = ^TGpPen;                  PPGpPen                = ^PGpPen;
  TGpCustomLineCap      = record end;               PGpCustomLineCap      = ^TGpCustomLineCap;        PPGpCustomLineCap      = ^PGpCustomLineCap;
  TGpAdjustableArrowCap = type TGpCustomLineCap;    PGpAdjustableArrowCap = ^TGpAdjustableArrowCap;   PPGpAdjustableArrowCap = ^PGpAdjustableArrowCap;

  TGpImage           = record end;      PGpImage           = ^TGpImage;             PPGpImage           = ^PGpImage;
  TGpBitmap          = type TGpImage;   PGpBitmap          = ^TGpBitmap;            PPGpBitmap          = ^PGpBitmap;
  TGpMetafile        = type TGpImage;   PGpMetafile        = ^TGpMetafile;          PPGpMetafile        = ^PGpMetafile;
  TGpImageAttributes = record end;      PGpImageAttributes = ^TGpImageAttributes;   PPGpImageAttributes = ^PGpImageAttributes;

  TGpPath         = record end;   PGpPath         = ^TGpPath;           PPGpPath         = ^PGpPath;
  TGpRegion       = record end;   PGpRegion       = ^TGpRegion;         PPGpRegion       = ^PGpRegion;
  TGpPathIterator = record end;   PGpPathIterator = ^TGpPathIterator;   PPGpPathIterator = ^PGpPathIterator;

  TGpFontFamily              = record end;                PGpFontFamily              = ^TGpFontFamily;                PPGpFontFamily     = ^PGpFontFamily;
  TGpFont                    = record end;                PGpFont                    = ^TGpFont;                      PPGpFont           = ^PGpFont;
  TGpStringFormat            = record end;                PGpStringFormat            = ^TGpStringFormat;              PPGpStringFormat   = ^PGpStringFormat;
  TGpFontCollection          = record end;                PGpFontCollection          = ^TGpFontCollection;            PPGpFontCollection = ^PGpFontCollection;
  TGpInstalledFontCollection = type TGpFontCollection;    PGpInstalledFontCollection = ^TGpInstalledFontCollection;
  TGpPrivateFontCollection   = type TGpFontCollection;    PGpPrivateFontCollection   = ^TGpPrivateFontCollection;

  TGpCachedBitmap = record end;   PGpCachedBitmap = ^TGpCachedBitmap;   PPGpCachedBitmap = ^PGpCachedBitmap;

type
  TGpStatus          = TStatus;             PGpStatus          = ^TGpStatus;
  TGpFillMode        = TFillMode;           PGpFillMode        = ^TGpFillMode;
  TGpWrapMode        = TWrapMode;           PGpWrapMode        = ^TGpWrapMode;
  TGpUnit            = TUnit;               PGpUnit            = ^TGpUnit;
  TGpCoordinateSpace = TCoordinateSpace;    PGpCoordinateSpace = ^TGpCoordinateSpace;
  TGpPointF          = TPointF;             PGpPointF          = ^TGpPointF;
  TGpPoint           = TPoint;              PGpPoint           = ^TGpPoint;
  TGpRectF           = TRectF;              PGpRectF           = ^TGpRectF;
  TGpRect            = TRect;               PGpRect            = ^TGpRect;
  TGpSizeF           = TSizeF;              PGpSizeF           = ^TGpSizeF;
  TGpHatchStyle      = THatchStyle;         PGpHatchStyle      = ^TGpHatchStyle;
  TGpDashStyle       = TDashStyle;          PGpDashStyle       = ^TGpDashStyle;
  TGpLineCap         = TLineCap;            PGpLineCap         = ^TGpLineCap;
  TGpDashCap         = TDashCap;            PGpDashCap         = ^TGpDashCap;

  TGpPenAlignment = TPenAlignment;    PGpPenAlignment = ^TGpPenAlignment;

  TGpLineJoin = TLineJoin;    PGpLineJoin = ^TGpLineJoin;
  TGpPenType  = TPenType;     PGpPenType  = ^TGpPenType;

  TGpMatrix         = record end;         PGpMatrix         = ^TGpMatrix;           PPGpMatrix = ^PGpMatrix;
  TGpBrushType      = TBrushType;         PGpBrushType      = ^TGpBrushType;
  TGpMatrixOrder    = TMatrixOrder;       PGpMatrixOrder    = ^TGpMatrixOrder;
  TGpFlushIntention = TFlushIntention;    PGpFlushIntention = ^TGpFlushIntention;
  TGpPathData       = TPathData;          PGpPathData       = ^TGpPathData;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusHeaders.h
*
* Abstract:
*
*   GDI+ Region, Font, Image, CustomLineCap class definitions.
*
*
* Class definition and inline class implementation are separated into
* different files to avoid circular dependencies.
*
\**************************************************************************)
{!!
  Some pre-declarations (not forward declarations!).

  It is here because of circular dependencies of some classes.
}
type
  TMatrixBase = class(TGdiPlusBase);
  TGraphicsPathBase = class(TGdiPlusBase);
  TGraphicsBase = class(TGdiPlusBase);
  TFontCollectionBase = class(TGdiPlusBase);    

{!!=============================================================================
    TRegion - class declaration
===============================================================================}
type
  PRegion = ^TRegion;
  TRegion = class(TGdiPlusBase)
  protected
    fNativeRegion:  PGpRegion;
    fLastResult:    TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    Function SetStatus(Status: TStatus): TStatus;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativeRegion: PGpRegion); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeRegion(NewNativeRegion: PGpRegion);
  public
    constructor Create; overload;
    constructor Create(const Rect: TRectF); overload;
    constructor Create(const Rect: TRect); overload;
    constructor Create(Path: TGraphicsPathBase); overload;
    constructor Create(RegionData: PBYTE; Size: INT); overload;
    constructor Create(HRgn: HRGN); overload;
    class Function FromHRGN(HRgn: HRGN): TRegion;
    destructor Destroy; override;
    Function Clone: TRegion;
    Function MakeInfinite: TStatus;
    Function MakeEmpty: TStatus;
    // Get the size of the buffer needed for the GetData method
    Function GetDataSize: UINT;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    Function GetData(Buffer: PBYTE; BufferSize: UINT; SizeFilled: PUINT = nil): TStatus;
    Function Intersect(const Rect: TRect): TStatus; overload;
    Function Intersect(const Rect: TRectF): TStatus; overload;
    Function Intersect(Path: TGraphicsPathBase): TStatus; overload;
    Function Intersect(Region: TRegion): TStatus; overload;
    Function Union(const Rect: TRect): TStatus; overload;
    Function Union(const Rect: TRectF): TStatus; overload;
    Function Union(Path: TGraphicsPathBase): TStatus; overload;
    Function Union(Region: TRegion): TStatus; overload;
    //!! XOR is a reserved word in pascal...
    Function ExclusiveOR(const Rect: TRect): TStatus; overload;
    Function ExclusiveOR(const Rect: TRectF): TStatus; overload;
    Function ExclusiveOR(Path: TGraphicsPathBase): TStatus; overload;
    Function ExclusiveOR(Region: TRegion): TStatus; overload;
    Function Exclude(const Rect: TRect): TStatus; overload;
    Function Exclude(const Rect: TRectF): TStatus; overload;
    Function Exclude(Path: TGraphicsPathBase): TStatus; overload;
    Function Exclude(Region: TRegion): TStatus; overload;
    Function Complement(const Rect: TRect): TStatus; overload;
    Function Complement(const Rect: TRectF): TStatus; overload;
    Function Complement(Path: TGraphicsPathBase): TStatus; overload;
    Function Complement(Region: TRegion): TStatus; overload;
    Function Translate(DX,DY: REAL): TStatus; overload;
    Function Translate(DX,DY: INT): TStatus; overload;
    Function Transform(Matrix: TMatrixBase): TStatus;
    Function GetBounds(Rect: PRect; G: TGraphicsBase): TStatus; overload;
    Function GetBounds(Rect: PRectF; G: TGraphicsBase): TStatus; overload;
    Function GetHRGN(G: TGraphicsBase): HRGN;
    Function IsEmpty(G: TGraphicsBase): BOOL;
    Function IsInfinite(G: TGraphicsBase): BOOL;
  (**
   * Hit testing operations
   *)
    Function IsVisible(X,Y: INT; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(const Point: TPoint; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(X,Y: REAL; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(const Point: TPointF; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(X,Y,Width,Height: INT; G: TGraphicsBase): BOOL; overload;
    Function IsVisible(const Rect: TRect; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(X,Y,Width,Height: REAL; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(const Rect: TRectF; G: TGraphicsBase = nil): BOOL; overload;
    Function IsEqual(Region: TRegion; G: TGraphicsBase): BOOL; //!! renamed from Equals (conflict with rtl method)
    Function GetRegionScansCount(Matrix: TMatrixBase): UINT;
    // If rects is NULL, return the count of rects in the region.
    // Otherwise, assume rects is big enough to hold all the region rects
    // and fill them in and return the number of rects filled in.
    // The rects are returned in the units specified by the matrix
    // (which is typically a world-to-device transform).
    // Note that the number of rects returned can vary, depending on the
    // matrix that is used.
    Function GetRegionScans(Matrix: TMatrixBase; Rects: PRectF; Count: PINT): TStatus; overload;
    Function GetRegionScans(Matrix: TMatrixBase; Rects: PRect; Count: PINT): TStatus; overload;
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TFontFamily - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// FontFamily
//--------------------------------------------------------------------------
type
  PFontFamily = ^TFontFamily;
  TFontFamily = class(TGdiPlusBase)
  protected
    fNativeFamily:  PGpFontFamily;
    fLastResult:    TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    Function SetStatus(Status: TStatus): TStatus;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativeFamily: PGpFontFamily; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  public
    constructor Create; overload;
    constructor Create(Name: PWideChar; FontCollection: TFontCollectionBase = nil); overload;
    constructor Create(const Name: String; FontCollection: TFontCollectionBase = nil); overload;
    destructor Destroy; override;
  {!!
    Unlike in original C++ source, the following functions are returning unique
    instances of TFontFamily, which means they must be explicitly freed.
  }
    class Function GenericSansSerif: TFontFamily;
    class Function GenericSerif: TFontFamily;
    class Function GenericMonospace: TFontFamily;
    //!! Name must point to a buffer LF_FACESIZE wide characters (not bytes!) long
    Function GetFamilyName(Name: PWideChar; Language: LANGID = 0): TStatus; overload;
    Function GetFamilyName(out Name: String; Language: LANGID = 0): TStatus; overload;
    Function Clone: TFontFamily;
    Function IsAvailable: BOOL;
    Function IsStyleAvailable(Style: INT): BOOL;
    Function GetEmHeight(Style: INT): UINT16;
    Function GetCellAscent(Style: INT): UINT16;
    Function GetCellDescent(Style: INT): UINT16;
    Function GetLineSpacing(Style: INT): UINT16;
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TFont - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Font
//--------------------------------------------------------------------------
type
  TFont = class(TGdiPlusBase)
  protected
    fNativeFont:  PGpFont;
    fLastResult:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(Font: PGpFont; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeFont(Font: PGpFont);
    Function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create(hDC: HDC); overload;
    constructor Create(hDC: HDC; LogFont: PLogFontA); overload;
    constructor Create(hDC: HDC; LogFont: PLogFontW); overload;
    constructor Create(hDC: HDC; hFont: HFONT); overload;
    constructor Create(Family: TFontFamily; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint); overload;
    constructor Create(FamilyName: PWideChar; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint;
      FontCollection: TFontCollectionBase = nil); overload;
    constructor Create(const FamilyName: String; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint;
      FontCollection: TFontCollectionBase = nil); overload;
    Function GetLogFontA(G: TGraphicsBase; LogFontA: PLOGFONTA): TStatus;
    Function GetLogFontW(G: TGraphicsBase; LogFontW: PLOGFONTW): TStatus;
    Function GetLogFont(G: TGraphicsBase; LogFont: PLOGFONT): TStatus;
    Function Clone: TFont;
    destructor Destroy; override;
    // Operations
    Function IsAvailable: BOOL;
    Function GetStyle: INT;
    Function GetSize: REAL;
    Function GetUnit: TUnit;
    Function GetLastStatus: TStatus;
    Function GetHeight(Graphics: TGraphicsBase): REAL; overload;
    Function GetHeight(Dpi: REAL): REAL; overload;
    Function GetFamily(Family: TFontFamily): TStatus;
  end;

{!!=============================================================================
    TFontCollection - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Font Collection
//--------------------------------------------------------------------------
type
  TFontCollection = class(TFontCollectionBase)
  protected
    fNativeFontCollection:  PGpFontCollection;
    fLastResult:            TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    Function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create;
    Function GetFamilyCount: INT;
    Function GetFamilies(NumSought: INT; Gpfamilies: PFontFamily; NumFound: PINT): TStatus;
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TInstalledFontCollection - class declaration
===============================================================================}
type
  TInstalledFontCollection = class(TFontCollection)
  public
    constructor Create;
  end;

{!!=============================================================================
    TPrivateFontCollection - class declaration
===============================================================================}
type
  TPrivateFontCollection = class(TFontCollection)
  public
    constructor Create;
    destructor Destroy; override;
    Function AddFontFile(Filename: PWideChar): TStatus; overload;
    Function AddFontFile(const Filename: String): TStatus; overload;
    Function AddMemoryFont(Memory: Pointer; Length: INT): TStatus;
  end;

{!!=============================================================================
    TImage - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Abstract base class for Image and Metafile
//--------------------------------------------------------------------------
type
  TImage = class(TGdiPlusBase)
  protected
    fNativeImage: PGpImage;
    fLastResult:  TStatus;
    fLoadStatus:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    procedure SetNativeImage(NewNativeImage: PGpImage);
    Function SetStatus(Status: TStatus): TStatus;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativeImage: PGpImage; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  public
    constructor Create(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False); overload;
    constructor Create(const Filename: String; UseEmbeddedColorManagement: BOOL = False); overload;
  {!!
    Be warned, the following constructors are NOT equivalent to loading. The
    streams are touched only when needed, meaning the underlying objects must
    exist and must not be altered the whole time the TBitmap instance exists.
  }    
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: BOOL = False); overload;
    constructor Create(Stream: TStream; UseEmbeddedColorManagement: BOOL = False); overload;
    class Function FromFile(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False): TImage; overload;
    class Function FromFile(const Filename: String; UseEmbeddedColorManagement: BOOL = False): TImage; overload;
    class Function FromStream(Stream: IStream; UseEmbeddedColorManagement: BOOL = False): TImage; overload;
    class Function FromStream(Stream: TStream; UseEmbeddedColorManagement: BOOL = False): TImage; overload;
    destructor Destroy; override;
    Function Clone: TImage;
    Function Save(Filename: PWideChar; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus; overload;
    Function Save(const Filename: String; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus; overload;
    Function Save(Stream: IStream; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus; overload;
    Function Save(Stream: TStream; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus; overload;
    Function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    Function SaveAdd(NewImage: TImage; EncoderParams: PEncoderParameters): TStatus; overload;
    Function GetType: TImageType;
    Function GetPhysicalDimension(Size: PSizeF): TStatus;
    Function GetBounds(SrcRect: PRectF; SrcUnit: PUnit): TStatus;
    Function GetWidth: UINT;
    Function GetHeight: UINT;
    Function GetHorizontalResolution: REAL;
    Function GetVerticalResolution: REAL;   
    Function GetFlags: UINT;
    Function GetRawFormat(Format: PGUID): TStatus;
    Function GetPixelFormat: TPixelFormat;
    Function GetPaletteSize: INT;
    Function GetPalette(Palette: PColorPalette; Size: INT): TStatus;
    Function SetPalette(Palette: PColorPalette): TStatus; 
    Function GetThumbnailImage(ThumbWidth,ThumbHeight: UINT; Callback: TGetThumbnailImageAbort = nil; CallbackData: Pointer = nil): TImage;
    Function GetFrameDimensionsCount: UINT;
    Function GetFrameDimensionsList(DimensionsIDs: PGUID; Count: UINT): TStatus;
    Function GetFrameCount(DimensionID: PGUID): UINT;
    Function SelectActiveFrame(DimensionID: PGUID; FrameIndex: UINT): TStatus;
    Function RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
    Function GetPropertyCount: UINT;
    Function GetPropertyIdList(NumOfProperty: UINT; List: PPROPID): TStatus;
    Function GetPropertyItemSize(PropId: PROPID): UINT;
    Function GetPropertyItem(PropId: PROPID; PropSize: UINT; Buffer: PPropertyItem): TStatus;
    Function GetPropertySize(TotalBufferSize,NumProperties: PUINT): TStatus;
    Function GetAllPropertyItems(TotalBufferSize,NumProperties: UINT; AllItems: PPropertyItem): TStatus;
    Function RemovePropertyItem(PropId: PROPID): TStatus;
    Function SetPropertyItem(Item: PPropertyItem): TStatus;
    Function GetEncoderParameterListSize(ClsidEncoder: PCLSID): UINT;
    Function GetEncoderParameterList(ClsidEncoder: PCLSID; Size: UINT; Buffer: PEncoderParameters): TStatus;
  {$IF GDIPVER >= $0110}
    Function FindFirstItem(Item: PImageItemData): TStatus;
    Function FindNextItem(Item: PImageItemData): TStatus;
    Function GetItemData(Item: PImageItemData): TStatus;
    Function SetAbort(PIAbort: PGdiplusAbort): TStatus;
  {$IFEND}
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TBitmap - class declaration
===============================================================================}
type
  PBitmap = ^TBitmap;
  TBitmap = class(TImage)
  protected
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativeBitmap: PGpBitmap); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  public
    constructor Create(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False); overload;
    constructor Create(const Filename: String; UseEmbeddedColorManagement: BOOL = False); overload;
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: BOOL = False); overload;
    constructor Create(Stream: TStream; UseEmbeddedColorManagement: BOOL = False); overload;
    class Function FromFile(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False): TBitmap; overload;
    class Function FromFile(const Filename: String; UseEmbeddedColorManagement: BOOL = False): TBitmap; overload;
    class Function FromStream(Stream: IStream; UseEmbeddedColorManagement: BOOL = False): TBitmap; overload;
    class Function FromStream(Stream: TStream; UseEmbeddedColorManagement: BOOL = False): TBitmap; overload;
    constructor Create(Width,Height,Stride: INT; Format: TPixelFormat; Scan0: PBYTE); overload;
    constructor Create(Width,Height: INT; Format: TPixelFormat); overload;
    constructor Create(Width,Height: INT; Target: TGraphicsBase); overload;
    Function Clone(const Rect: TRect; Format: TPixelFormat): TBitmap; overload;
    Function Clone(X,Y,Width,Height: INT; Format: TPixelFormat): TBitmap; overload;
    Function Clone(const Rect: TRectF; Format: TPixelFormat): TBitmap; overload;
    Function Clone(X,Y,Width,Height: REAL; Format: TPixelFormat): TBitmap; overload;
    Function LockBits(const Rect: TRect; Flags: UINT; Format: TPixelFormat; LockedBitmapData: PBitmapData): TStatus;
    Function UnlockBits(LockedBitmapData: PBitmapData): TStatus;
    Function GetPixel(X,Y: INT; Color: PColor): TStatus;
    Function SetPixel(X,Y: INT; const Color: TColor): TStatus;
  {$IF GDIPVER >= $0110}
    Function ConvertFormat(Format: TPixelFormat; DitherType: TDitherType; PaletteType: TPaletteType; Palette: PColorPalette;
      AlphaThresholdPercent: REAL): TStatus;
    // The palette must be allocated and count must be set to the number of
    // entries in the palette. If there are not enough, the API will fail.
    class Function InitializePalette(
        Palette:              PColorPalette;  // Palette to initialize. output palette. must be allocated.
        PaletteType:          TPaletteType;   // palette enumeration type.
        OptimalColors:        INT;            // how many optimal colors
        UseTransparentColor:  BOOL;           // add a transparent color to the palette.
        Bitmap:               TBitmap         // optional bitmap for median cut.
      ): TStatus;
    Function ApplyEffect(Effect: TEffect; ROI: Windows.PRECT): TStatus; overload;
    class Function ApplyEffect(Inputs: PBitmap; NumInputs: INT; Effect: TEffect; ROI,OutputRect: Windows.PRECT; // optional parameter.
      Output: PBitmap): TStatus; overload;
    Function GetHistogram(Format: THistogramFormat; NumberOfEntries: UINT; Channel0,Channel1,Channel2,Channel3: PUINT): TStatus;
    class Function GetHistogramSize(Format: THistogramFormat; NumberOfEntries: PUINT): TStatus;
  {$IFEND}
    Function SetResolution(XDpi,YDpi: REAL): TStatus;
    constructor Create(Surface: IDirectDrawSurface7); overload;
    constructor Create(GdiBitmapInfo: PBITMAPINFO; GdiBitmapData: Pointer); overload;
    constructor Create(hBM: HBITMAP; hPal: HPALETTE); overload;
    constructor Create(hIcon: HICON); overload;
    constructor Create(hInstance: HINSTANCE; BitmapName: PWideChar); overload;
    constructor Create(hInstance: HINSTANCE; const BitmapName: String); overload;
    class Function FromDirectDrawSurface7(Surface: IDirectDrawSurface7): TBitmap;
    class Function FromBITMAPINFO(GdiBitmapInfo: PBITMAPINFO; GdiBitmapData: Pointer): TBitmap;
    class Function FromHBITMAP(hBM: HBITMAP; hPal: HPALETTE): TBitmap;
    class Function FromHICON(hIcon: HICON): TBitmap;
    class Function FromResource(hInstance: HINSTANCE; BitmapName: PWideChar): TBitmap; overload;
    class Function FromResource(hInstance: HINSTANCE; const BitmapName: String): TBitmap; overload;
    Function GetHBITMAP(const ColorBackground: TColor; hBMReturn: PHBITMAP): TStatus;
    Function GetHICON(hIcon: PHICON): TStatus;
  end;

{!!=============================================================================
    TCustomLineCap - class declaration
===============================================================================}
type
  TCustomLineCap = class(TGdiPlusBase)
  protected
    fNativeCap:   PGpCustomLineCap;
    fLastResult:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create; overload;
    constructor Create(NativeCapArg: PGpCustomLineCap; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeCap(NativeCapArg: PGpCustomLineCap);
    Function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create(FillPath,StrokePath: TGraphicsPathBase; BaseCap: TLineCap = LineCapFlat; BaseInset: REAL = 0); overload;
    destructor Destroy; override;
    Function Clone: TCustomLineCap;
    // This changes both the start and end cap.
    Function SetStrokeCap(StrokeCap: TLineCap): TStatus;
    Function SetStrokeCaps(StartCap,EndCap: TLineCap): TStatus;
    Function GetStrokeCaps(StartCap,EndCap: PLineCap): TStatus;
    Function SetStrokeJoin(LineJoin: TLineJoin): TStatus;
    Function GetStrokeJoin: TLineJoin;
    Function SetBaseCap(BaseCap: TLineCap): TStatus;
    Function GetBaseCap: TLineCap;
    Function SetBaseInset(Inset: REAL): TStatus;
    Function GetBaseInset: REAL;
    Function SetWidthScale(WidthScale: REAL): TStatus;
    Function GetWidthScale: REAL;
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TCachedBitmap - class declaration
===============================================================================}
type
  TCachedBitmap = class(TGdiPlusBase)
  protected
    fNativeCachedBitmap:  PGpCachedBitmap;
    fLastResult:          TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  public
    constructor Create(Bitmap: TBitmap; Graphics: TGraphicsBase);
    destructor Destroy; override;
    Function GetLastStatus: TStatus;
  end;

{!!=============================================================================
    TMetafile - class declaration
===============================================================================}
type
  TMetafile = class(TImage)
  protected
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create; overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  public
    // Playback a metafile from a HMETAFILE
    // If deleteWmf is TRUE, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.
    constructor Create(HWmf: HMETAFILE; WmfPlaceableFileHeader: PWmfPlaceableFileHeader; DeleteWmf: BOOL = FALSE); overload;
    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is TRUE, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.
    constructor Create(HEmf: HENHMETAFILE; DeleteWmf: BOOL = FALSE); overload;
    constructor Create(Filename: PWideChar); overload;
    constructor Create(const Filename: String); overload;
    // Playback a WMF metafile from a file.
    constructor Create(Filename: PWideChar; WmfPlaceableFileHeader: PWmfPlaceableFileHeader); overload;
    constructor Create(const Filename: String; WmfPlaceableFileHeader: PWmfPlaceableFileHeader); overload;
    constructor Create(Stream: IStream); overload;
    constructor Create(Stream: TStream); overload;
    // Record a metafile to memory.
    constructor Create(ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(ReferenceHdc: HDC; EmfType: TEmfType; const Description: String); overload;
    // Record a metafile to memory.
    constructor Create(ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    // Record a metafile to memory.
    constructor Create(ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Filename: PWideChar; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(const Filename: String; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String); overload;
    constructor Create(Filename: PWideChar; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(const Filename: String; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Filename: PWideChar; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(const Filename: String; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil); overload;
    constructor Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    constructor Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
      const Description: String); overload;
    class Function GetMetafileHeader(HWmf: HMETAFILE; WmfPlaceableFileHeader: PWmfPlaceableFileHeader; Header: PMetafileHeader): TStatus; overload;
    class Function GetMetafileHeader(HEmf: HENHMETAFILE; Header: PMetafileHeader): TStatus; overload;
    class Function GetMetafileHeader(Filename: PWideChar; Header: PMetafileHeader): TStatus; overload;
    class Function GetMetafileHeader(const Filename: String; Header: PMetafileHeader): TStatus; overload;
    class Function GetMetafileHeader(Stream: IStream; Header: PMetafileHeader): TStatus; overload;
    class Function GetMetafileHeader(Stream: TStream; Header: PMetafileHeader): TStatus; overload;
    Function GetMetafileHeader(Header: PMetafileHeader): TStatus; overload;
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.
    Function GetHENHMETAFILE: HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    Function PlayRecord(RecordType: TEmfPlusRecordType; Flags,DataSize: UINT; Data: PBYTE): TStatus;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    Function SetDownLevelRasterizationLimit(MetafileRasterizationLimitDpi: UINT): TStatus;
    Function GetDownLevelRasterizationLimit: UINT;
    class Function EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT16; pData16: LPBYTE; iMapMode: INT = MM_ANISOTROPIC;
      eFlags: INT = EmfToWmfBitsFlagsDefault): UINT;
  {$IF GDIPVER >= $0110}
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; ConversionFailureFlag: PINT = nil;
      EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; ConversionFailureFlag: PINT;
      EmfType: TEmfType; const Description: String): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; Filename: PWideChar; ConversionFailureFlag: PINT = nil;
      EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; const Filename: String; ConversionFailureFlag: PINT;
      EmfType: TEmfType; const Description: String): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: IStream; ConversionFailureFlag: PINT = nil;
      EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: TStream; ConversionFailureFlag: PINT = nil;
      EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: IStream; ConversionFailureFlag: PINT;
      EmfType: TEmfType; const Description: String): TStatus; overload;
    Function ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: TStream; ConversionFailureFlag: PINT;
      EmfType: TEmfType; const Description: String): TStatus; overload;
  {$IFEND}
  end;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusFlat.h
*
* Abstract:
*
*   Private GDI+ header file.
*
\**************************************************************************)
//----------------------------------------------------------------------------
// GraphicsPath APIs
//----------------------------------------------------------------------------

Function GdipCreatePath(brushMode: TGpFillMode; path: PPGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreatePath2(points: PGpPointF; types: PByte; count: INT; fillMode: TGpFillMode; path: PPGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreatePath2I(points: PGpPoint; types: PByte; count: INT; fillMode: TGpFillMode; path: PPGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipClonePath(path: PGpPath; clonePath: PPGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeletePath(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetPath(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPointCount(path: PGpPath; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathTypes(path: PGpPath; types: PByte; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathPoints(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathPointsI(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathFillMode(path: PGpPath; fillmode: PGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathFillMode(path: PGpPath; fillmode: TGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathData(path: PGpPath; pathData: PGpPathData): TGpStatus; stdcall; external GDIPLIB;

Function GdipStartPathFigure(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipClosePathFigure(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipClosePathFigures(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathMarker(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipClearPathMarkers(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipReversePath(path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathLastPoint(path: PGpPath; lastPoint: PGpPointF): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathLine(path: PGpPath; x1,y1,x2,y2: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathLine2(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathArc(path: PGpPath; x,y,width,height,startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathBezier(path: PGpPath; x1,y1,x2,y2,x3,y3,x4,y4: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathBeziers(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurve(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurve2(path: PGpPath; points: PGpPointF; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurve3(path: PGpPath; points: PGpPointF; count,offset,numberOfSegments: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathClosedCurve(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathClosedCurve2(path: PGpPath; points: PGpPointF; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathRectangle(path: PGpPath; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathRectangles(path: PGpPath; rects: PGpRectF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathEllipse(path: PGpPath; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathPie(path: PGpPath; x,y,width,height,startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathPolygon(path: PGpPath; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathPath(path: PGpPath; addingPath: PGpPath; connect: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathString(path: PGpPath; str: PWideChar; length: INT; family: PGpFontFamily; style: INT;
  emSize: REAL; layoutRect: PRectF; format: PGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathStringI(path: PGpPath; str: PWideChar; length: INT; family: PGpFontFamily; style: INT;
  emSize: REAL; layoutRect: PRect; format: PGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathLineI(path: PGpPath; x1,y1,x2,y2: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathLine2I(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathArcI(path: PGpPath; x,y,width,height: INT; startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathBezierI(path: PGpPath; x1,y1,x2,y2,x3,y3,x4,y4: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathBeziersI(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurveI(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurve2I(path: PGpPath; points: PGpPoint; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathCurve3I(path: PGpPath; points: PGpPoint; count,offset,numberOfSegments: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathClosedCurveI(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathClosedCurve2I(path: PGpPath; points: PGpPoint; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathRectangleI(path: PGpPath; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathRectanglesI(path: PGpPath; rects: PGpRect; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathEllipseI(path: PGpPath; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathPieI(path: PGpPath; x,y,width,height: INT; startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipAddPathPolygonI(path: PGpPath; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFlattenPath(path: PGpPath; matrix: PGpMatrix; flatness: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipWindingModeOutline(path: PGpPath; matrix: PGpMatrix; flatness: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipWidenPath(nativePath: PGpPath; pen: PGpPen; matrix: PGpMatrix; flatness: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipWarpPath(path: PGpPath; matrix: PGpMatrix; points: PGpPointF; count: INT; srcx,srcy,srcwidth,srcheight: REAL;
  warpMode: TWarpMode; flatness: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformPath(path: PGpPath; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathWorldBounds(path: PGpPath; bounds: PGpRectF; matrix: PGpMatrix; pen: PGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathWorldBoundsI(path: PGpPath; bounds: PGpRect; matrix: PGpMatrix; pen: PGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisiblePathPoint(path: PGpPath; x,y: REAL; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisiblePathPointI(path: PGpPath; x,y: INT; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsOutlineVisiblePathPoint(path: PGpPath; x,y: REAL; pen: PGpPen; graphics: PGpGraphics;
  result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsOutlineVisiblePathPointI(path: PGpPath; x,y: INT; pen: PGpPen; graphics: PGpGraphics;
  result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// PathIterator APIs
//----------------------------------------------------------------------------

Function GdipCreatePathIter(iterator: PPGpPathIterator; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeletePathIter(iterator: PGpPathIterator): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterNextSubpath(iterator: PGpPathIterator; resultCount,startIndex,endIndex: PINT;
  isClosed: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterNextSubpathPath(iterator: PGpPathIterator; resultCount: PINT; path: PGpPath;
  isClosed: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterNextPathType(iterator: PGpPathIterator; resultCount: PINT; pathType: PBYTE;
  startIndex,endIndex: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterNextMarker(iterator: PGpPathIterator; resultCount,startIndex,endIndex: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterNextMarkerPath(iterator: PGpPathIterator; resultCount: PINT; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterGetCount(iterator: PGpPathIterator; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterGetSubpathCount(iterator: PGpPathIterator; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterIsValid(iterator: PGpPathIterator; valid: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterHasCurve(iterator: PGpPathIterator; hasCurve: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterRewind(iterator: PGpPathIterator): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterEnumerate(iterator: PGpPathIterator; resultCount: PINT; points: PGpPointF;
  types: PBYTE; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPathIterCopyData(iterator: PGpPathIterator; resultCount: PINT; points: PGpPointF;
  types: PBYTE; startIndex,endIndex: INT): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

Function GdipCreateMatrix(matrix: PPGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMatrix2(m11,m12,m21,m22,dx,dy: REAL; matrix: PPGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMatrix3(rect: PGpRectF; dstplg: PGpPointF; matrix: PPGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMatrix3I(rect: PGpRect; dstplg: PGpPoint; matrix: PPGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneMatrix(matrix: PGpMatrix; cloneMatrix: PPGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteMatrix(matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetMatrixElements(matrix: PGpMatrix; m11,m12,m21,m22,dx,dy: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyMatrix(matrix: PGpMatrix; matrix2: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateMatrix(matrix: PGpMatrix; offsetX,offsetY: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScaleMatrix(matrix: PGpMatrix; scaleX,scaleY: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotateMatrix(matrix: PGpMatrix; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipShearMatrix(matrix: PGpMatrix; shearX,shearY: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipInvertMatrix(matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformMatrixPoints(matrix: PGpMatrix; pts: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformMatrixPointsI(matrix: PGpMatrix; pts: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipVectorTransformMatrixPoints(matrix: PGpMatrix; pts: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipVectorTransformMatrixPointsI(matrix: PGpMatrix; pts: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMatrixElements(matrix: PGpMatrix; matrixOut: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsMatrixInvertible(matrix: PGpMatrix; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsMatrixIdentity(matrix: PGpMatrix; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsMatrixEqual(matrix,matrix2: PGpMatrix; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

Function GdipCreateRegion(region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateRegionRect(rect: PGpRectF; region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateRegionRectI(rect: PGpRect; region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateRegionPath(path: PGpPath; region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateRegionRgnData(regionData: PBYTE; size: INT; region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateRegionHrgn(hRgn: HRGN; region: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneRegion(region: PGpRegion; cloneRegion: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteRegion(region: PGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetInfinite(region: PGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetEmpty(region: PGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipCombineRegionRect(region: PGpRegion; rect: PGpRectF; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipCombineRegionRectI(region: PGpRegion; rect: PGpRect; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipCombineRegionPath(region: PGpRegion; path: PGpPath; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipCombineRegionRegion(region,region2: PGpRegion; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateRegion(region: PGpRegion; dx,dy: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateRegionI(region: PGpRegion; dx,dy: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformRegion(region: PGpRegion; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionBounds(region: PGpRegion; graphics: PGpGraphics; rect: PGpRectF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionBoundsI(region: PGpRegion; graphics: PGpGraphics; rect: PGpRect): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionHRgn(region: PGpRegion; graphics: PGpGraphics; hRgn: PHRGN): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsEmptyRegion(region: PGpRegion; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsInfiniteRegion(region: PGpRegion; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsEqualRegion(region,region2: PGpRegion; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionDataSize(region: PGpRegion; bufferSize: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionData(region: PGpRegion; buffer: PBYTE; bufferSize: UINT; sizeFilled: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRegionPoint(region: PGpRegion; x,y: REAL; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRegionPointI(region: PGpRegion; x,y: INT; graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRegionRect(region: PGpRegion; x,y,width,height: REAL; graphics: PGpGraphics;
  result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRegionRectI(region: PGpRegion; x,y,width,height: INT; graphics: PGpGraphics;
  result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionScansCount(region: PGpRegion; count: PUINT; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionScans(region: PGpRegion; rects: PGpRectF; count: PINT; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRegionScansI(region: PGpRegion; rects: PGpRect; count: PINT; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

Function GdipCloneBrush(brush: PGpBrush; cloneBrush: PPGpBrush): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteBrush(brush: PGpBrush): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetBrushType(brush: PGpBrush; type_: PGpBrushType): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

Function GdipCreateHatchBrush(hatchstyle: TGpHatchStyle; forecol,backcol: TARGB; brush: PPGpHatch): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetHatchStyle(brush: PGpHatch; hatchstyle: PGpHatchStyle): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetHatchForegroundColor(brush: PGpHatch; forecol: PARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetHatchBackgroundColor(brush: PGpHatch; backcol: PARGB): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------

Function GdipCreateTexture(image: PGpImage; wrapmode: TGpWrapMode; texture: PPGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateTexture2(image: PGpImage; wrapmode: TGpWrapMode; x,y,width,height: REAL;
  texture: PPGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateTextureIA(image: PGpImage; imageAttributes: PGpImageAttributes; x,y,width,height: REAL;
  texture: PPGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateTexture2I(image: PGpImage; wrapmode: TGpWrapMode; x,y,width,height: INT;
  texture: PPGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateTextureIAI(image: PGpImage; imageAttributes: PGpImageAttributes; x,y,width,height: INT;
  texture: PPGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetTextureTransform(brush: PGpTexture; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetTextureTransform(brush: PGpTexture; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetTextureTransform(brush: PGpTexture): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyTextureTransform(brush: PGpTexture; matrix: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateTextureTransform(brush: PGpTexture; dx,dy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScaleTextureTransform(brush: PGpTexture; sx,sy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotateTextureTransform(brush: PGpTexture; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetTextureWrapMode(brush: PGpTexture; wrapmode: TGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetTextureWrapMode(brush: PGpTexture; wrapmode: PGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetTextureImage(brush: PGpTexture; image: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

Function GdipCreateSolidFill(color: TARGB; brush: PPGpSolidFill): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetSolidFillColor(brush: PGpSolidFill; color: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetSolidFillColor(brush: PGpSolidFill; color: PARGB): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

Function GdipCreateLineBrush(point1,point2: PGpPointF; color1,color2: TARGB;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateLineBrushI(point1,point2: PGpPoint; color1,color2: TARGB;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateLineBrushFromRect(rect: PGpRectF; color1,color2: TARGB; mode: TLinearGradientMode;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateLineBrushFromRectI(rect: PGpRect; color1,color2: TARGB; mode: TLinearGradientMode;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateLineBrushFromRectWithAngle(rect: PGpRectF; color1,color2: TARGB; angle: REAL; isAngleScalable: BOOL;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateLineBrushFromRectWithAngleI(rect: PGpRect; color1,color2: TARGB; angle: REAL; isAngleScalable: BOOL;
  wrapMode: TGpWrapMode; lineGradient: PPGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineColors(brush: PGpLineGradient; color1,color2: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineColors(brush: PGpLineGradient; colors: PARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineRect(brush: PGpLineGradient; rect: PGpRectF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineRectI(brush: PGpLineGradient; rect: PGpRect): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineGammaCorrection(brush: PGpLineGradient; useGammaCorrection: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineGammaCorrection(brush: PGpLineGradient; useGammaCorrection: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineBlendCount(brush: PGpLineGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineBlend(brush: PGpLineGradient; blend,positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineBlend(brush: PGpLineGradient; blend,positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLinePresetBlendCount(brush: PGpLineGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLinePresetBlend(brush: PGpLineGradient; blend: PARGB; positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLinePresetBlend(brush: PGpLineGradient; blend: PARGB; positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineSigmaBlend(brush: PGpLineGradient; focus,scale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineLinearBlend(brush: PGpLineGradient; focus,scale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineWrapMode(brush: PGpLineGradient; wrapmode: TGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineWrapMode(brush: PGpLineGradient; wrapmode: PGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineTransform(brush: PGpLineGradient; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetLineTransform(brush: PGpLineGradient; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetLineTransform(brush: PGpLineGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyLineTransform(brush: PGpLineGradient; matrix: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateLineTransform(brush: PGpLineGradient; dx,dy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScaleLineTransform(brush: PGpLineGradient; sx,sy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotateLineTransform(brush: PGpLineGradient; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

Function GdipCreatePathGradient(points: PGpPointF; count: INT; wrapMode: TGpWrapMode;
  polyGradient: PPGpPathGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreatePathGradientI(points: PGpPoint; count: INT; wrapMode: TGpWrapMode;
  polyGradient: PPGpPathGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreatePathGradientFromPath(path: PGpPath; polyGradient: PPGpPathGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientCenterColor(brush: PGpPathGradient; colors: PARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientCenterColor(brush: PGpPathGradient; colors: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientSurroundColorsWithCount(brush: PGpPathGradient; color: PARGB; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientSurroundColorsWithCount(brush: PGpPathGradient; color: PARGB; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientPath(brush: PGpPathGradient; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientPath(brush: PGpPathGradient; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientCenterPoint(brush: PGpPathGradient; points: PGpPointF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientCenterPointI(brush: PGpPathGradient; points: PGpPoint): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientCenterPoint(brush: PGpPathGradient; points: PGpPointF): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientCenterPointI(brush: PGpPathGradient; points: PGpPoint): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientRect(brush: PGpPathGradient; rect: PGpRectF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientRectI(brush: PGpPathGradient; rect: PGpRect): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientPointCount(brush: PGpPathGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientSurroundColorCount(brush: PGpPathGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientGammaCorrection(brush: PGpPathGradient; useGammaCorrection: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientGammaCorrection(brush: PGpPathGradient; useGammaCorrection: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientBlendCount(brush: PGpPathGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientBlend(brush: PGpPathGradient; blend,positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientBlend(brush: PGpPathGradient; blend,positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientPresetBlendCount(brush: PGpPathGradient; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientPresetBlend(brush: PGpPathGradient; blend: PARGB; positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientPresetBlend(brush: PGpPathGradient; blend: PARGB; positions: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientSigmaBlend(brush: PGpPathGradient; focus,scale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientLinearBlend(brush: PGpPathGradient; focus,scale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientWrapMode(brush: PGpPathGradient; wrapmode: PGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientWrapMode(brush: PGpPathGradient; wrapmode: TGpWrapMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientTransform(brush: PGpPathGradient; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientTransform(brush: PGpPathGradient; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetPathGradientTransform(brush: PGpPathGradient): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyPathGradientTransform(brush: PGpPathGradient; matrix: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslatePathGradientTransform(brush: PGpPathGradient; dx,dy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScalePathGradientTransform(brush: PGpPathGradient; sx,sy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotatePathGradientTransform(brush: PGpPathGradient; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPathGradientFocusScales(brush: PGpPathGradient; xScale,yScale: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPathGradientFocusScales(brush: PGpPathGradient; xScale,yScale: REAL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

Function GdipCreatePen1(color: TARGB; width: REAL; unit_: TGpUnit; pen: PPGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreatePen2(brush: PGpBrush; width: REAL; unit_: TGpUnit; pen: PPGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipClonePen(pen: PGpPen; clonepen: PPGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeletePen(pen: PGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenWidth(pen: PGpPen; width: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenWidth(pen: PGpPen; width: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenUnit(pen: PGpPen; unit_: TGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenUnit(pen: PGpPen; unit_: PGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenLineCap197819(pen: PGpPen; startCap,endCap: TGpLineCap; dashCap: TGpDashCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenStartCap(pen: PGpPen; startCap: TGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenEndCap(pen: PGpPen; endCap: TGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenDashCap197819(pen: PGpPen; dashCap: TGpDashCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenStartCap(pen: PGpPen; startCap: PGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenEndCap(pen: PGpPen; endCap: PGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenDashCap197819(pen: PGpPen; dashCap: PGpDashCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenLineJoin(pen: PGpPen; lineJoin: TGpLineJoin): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenLineJoin(pen: PGpPen; lineJoin: PGpLineJoin): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenCustomStartCap(pen: PGpPen; customCap: PGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenCustomStartCap(pen: PGpPen; customCap: PPGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenCustomEndCap(pen: PGpPen; customCap: PGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenCustomEndCap(pen: PGpPen; customCap: PPGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenMiterLimit(pen: PGpPen; miterLimit: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenMiterLimit(pen: PGpPen; miterLimit: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenMode(pen: PGpPen; penMode: TGpPenAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenMode(pen: PGpPen; penMode: PGpPenAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenTransform(pen: PGpPen; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenTransform(pen: PGpPen; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetPenTransform(pen: PGpPen): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyPenTransform(pen: PGpPen; matrix: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslatePenTransform(pen: PGpPen; dx,dy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScalePenTransform(pen: PGpPen; sx,sy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotatePenTransform(pen: PGpPen; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenColor(pen: PGpPen; argb: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenColor(pen: PGpPen; argb: PARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenBrushFill(pen: PGpPen; brush: PGpBrush): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenBrushFill(pen: PGpPen; brush: PPGpBrush): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenFillType(pen: PGpPen; type_: PGpPenType): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenDashStyle(pen: PGpPen; dashstyle: PGpDashStyle): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenDashStyle(pen: PGpPen; dashstyle: TGpDashStyle): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenDashOffset(pen: PGpPen; offset: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenDashOffset(pen: PGpPen; offset: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenDashCount(pen: PGpPen; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenDashArray(pen: PGpPen; dash: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenDashArray(pen: PGpPen; dash: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenCompoundCount(pen: PGpPen; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPenCompoundArray(pen: PGpPen; dash: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPenCompoundArray(pen: PGpPen; dash: PREAL; count: INT): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

Function GdipCreateCustomLineCap(fillPath,strokePath: PGpPath; baseCap: TGpLineCap; baseInset: REAL;
  customCap: PPGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteCustomLineCap(customCap: PGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneCustomLineCap(customCap: PGpCustomLineCap; clonedCap: PPGpCustomLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapType(customCap: PGpCustomLineCap; capType: PCustomLineCapType): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCustomLineCapStrokeCaps(customCap: PGpCustomLineCap; startCap,endCap: TGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapStrokeCaps(customCap: PGpCustomLineCap; startCap,endCap: PGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCustomLineCapStrokeJoin(customCap: PGpCustomLineCap; lineJoin: TGpLineJoin): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapStrokeJoin(customCap: PGpCustomLineCap; lineJoin: PGpLineJoin): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCustomLineCapBaseCap(customCap: PGpCustomLineCap; baseCap: TGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapBaseCap(customCap: PGpCustomLineCap; baseCap: PGpLineCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCustomLineCapBaseInset(customCap: PGpCustomLineCap; inset: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapBaseInset(customCap: PGpCustomLineCap; inset: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCustomLineCapWidthScale(customCap: PGpCustomLineCap; widthScale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCustomLineCapWidthScale(customCap: PGpCustomLineCap; widthScale: PREAL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

Function GdipCreateAdjustableArrowCap(height,width: REAL; isFilled: BOOL; cap: PPGpAdjustableArrowCap): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetAdjustableArrowCapHeight(cap: PGpAdjustableArrowCap; height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetAdjustableArrowCapHeight(cap: PGpAdjustableArrowCap; height: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetAdjustableArrowCapWidth(cap: PGpAdjustableArrowCap; width: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetAdjustableArrowCapWidth(cap: PGpAdjustableArrowCap; width: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetAdjustableArrowCapMiddleInset(cap: PGpAdjustableArrowCap; middleInset: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetAdjustableArrowCapMiddleInset(cap: PGpAdjustableArrowCap; middleInset: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetAdjustableArrowCapFillState(cap: PGpAdjustableArrowCap; fillState: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetAdjustableArrowCapFillState(cap: PGpAdjustableArrowCap; fillState: PBOOL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Image APIs
//----------------------------------------------------------------------------

//!! note in pascal IStream is already a pointer to the interface
Function GdipLoadImageFromStream(stream: IStream; image: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipLoadImageFromFile(filename: PWideChar; image: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipLoadImageFromStreamICM(stream: IStream; image: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipLoadImageFromFileICM(filename: PWideChar; image: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneImage(image: PGpImage; cloneImage: PPGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipDisposeImage(image: PGpImage): TGpStatus; stdcall; external GDIPLIB;

Function GdipSaveImageToFile(image: PGpImage; filename: PWideChar; clsidEncoder: PCLSID;
  encoderParams: PEncoderParameters): TGpStatus; stdcall; external GDIPLIB;

Function GdipSaveImageToStream(image: PGpImage; stream: IStream; clsidEncoder: PCLSID;
  encoderParams: PEncoderParameters): TGpStatus; stdcall; external GDIPLIB;

Function GdipSaveAdd(image: PGpImage; encoderParams: PEncoderParameters): TGpStatus; stdcall; external GDIPLIB;

Function GdipSaveAddImage(image,newImage: PGpImage; encoderParams: PEncoderParameters): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageGraphicsContext(image: PGpImage; graphics: PPGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageBounds(image: PGpImage; srcRect: PGpRectF; srcUnit: PGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageDimension(image: PGpImage; width,height: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageType(image: PGpImage; type_: PImageType): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageWidth(image: PGpImage; width: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageHeight(image: PGpImage; height: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageHorizontalResolution(image: PGpImage; resolution: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageVerticalResolution(image: PGpImage; resolution: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageFlags(image: PGpImage; flags: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageRawFormat(image: PGpImage; format: PGUID): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImagePixelFormat(image: PGpImage; format: PPixelFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageThumbnail(image: PGpImage; thumbWidth,thumbHeight: UINT; thumbImage: PPGpImage;
  callback: TGetThumbnailImageAbort; callbackData: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetEncoderParameterListSize(image: PGpImage; clsidEncoder: PCLSID; size: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetEncoderParameterList(image: PGpImage; clsidEncoder: PCLSID; size: UINT;
  bufer: PEncoderParameters): TGpStatus; stdcall; external GDIPLIB;

Function GdipImageGetFrameDimensionsCount(image: PGpImage; count: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipImageGetFrameDimensionsList(image: PGpImage; dimensionIDs: PGUID; count: UINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipImageGetFrameCount(image: PGpImage; dimensionID: PGUID; count: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipImageSelectActiveFrame(image: PGpImage; dimensionID: PGUID; frameIndex: UINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipImageRotateFlip(image: PGpImage; rfType: TRotateFlipType): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImagePalette(image: PGpImage; palette: PColorPalette; size: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImagePalette(image: PGpImage; palette: PColorPalette): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImagePaletteSize(image: PGpImage; size: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPropertyCount(image: PGpImage; numOfProperty: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPropertyIdList(image: PGpImage; numOfProperty: UINT; list: PPROPID): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPropertyItemSize(image: PGpImage; propId: TPROPID; size: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPropertyItem(image: PGpImage; propId: TPROPID; propSize: UINT; buffer: PPropertyItem): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPropertySize(image: PGpImage; totalBufferSize,numProperties: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetAllPropertyItems(image: PGpImage; totalBufferSize,numProperties: UINT; allItems: PPropertyItem): TGpStatus; stdcall; external GDIPLIB;

Function GdipRemovePropertyItem(image: PGpImage; propId: TPROPID): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPropertyItem(image: PGpImage; item: PPropertyItem): TGpStatus; stdcall; external GDIPLIB;

{$IF GDIPVER >= $0110}
{$IFDEF NewGDIPStatic}

Function GdipFindFirstImageItem(image: PGpImage; item: PImageItemData): TGpStatus; stdcall; external GDIPLIB;

Function GdipFindNextImageItem(image: PGpImage; item: PImageItemData): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageItemData(image: PGpImage; item: PImageItemData): TGpStatus; stdcall; external GDIPLIB;

{$ELSE}
var
  GdipFindFirstImageItem: Function(image: PGpImage; item: PImageItemData): TGpStatus; stdcall = nil;

  GdipFindNextImageItem: Function(image: PGpImage; item: PImageItemData): TGpStatus; stdcall = nil;

  GdipGetImageItemData: Function(image: PGpImage; item: PImageItemData): TGpStatus; stdcall = nil;

{$ENDIF}
{$IFEND}

Function GdipImageForceValidation(image: PGpImage): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Bitmap APIs
//----------------------------------------------------------------------------

Function GdipCreateBitmapFromStream(stream: IStream; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromFile(filename: PWideChar; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromStreamICM(stream: IStream; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromFileICM(filename: PWideChar; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromScan0(width,height,stride: INT; format: TPixelFormat; scan0: PBYTE;
  bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromGraphics(width,height: INT; target: PGpGraphics; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromDirectDrawSurface(surface: IDirectDrawSurface7; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromGdiDib(gdiBitmapInfo: PBITMAPINFO; gdiBitmapData: Pointer; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromHBITMAP(hbm: HBITMAP; hpal: HPALETTE; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateHBITMAPFromBitmap(bitmap: PGpBitmap; hbmReturn: PHBITMAP; background: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromHICON(hicon: HICON; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateHICONFromBitmap(bitmap: PGpBitmap; hbmReturn: PHICON): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateBitmapFromResource(hInstance: HINSTANCE; lpBitmapName: PWideChar; bitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneBitmapArea(x,y,width,height: REAL; format: TPixelFormat; srcBitmap: PGpBitmap;
  dstBitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneBitmapAreaI(x,y,width,height: INT; format: TPixelFormat; srcBitmap: PGpBitmap;
  dstBitmap: PPGpBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapLockBits(bitmap: PGpBitmap; rect: PGpRect; flags: UINT; format: TPixelFormat;
  lockedBitmapData: PBitmapData): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapUnlockBits(bitmap: PGpBitmap; lockedBitmapData: PBitmapData): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapGetPixel(bitmap: PGpBitmap; x,y: INT; color: PARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapSetPixel(bitmap: PGpBitmap; x,y: INT; color: TARGB): TGpStatus; stdcall; external GDIPLIB;

{$IF GDIPVER >= $0110}
{$IFDEF NewGDIPStatic}

Function GdipImageSetAbort(pImage: PGpImage; pIAbort: PGdiplusAbort): TGpStatus; stdcall; external GDIPLIB;

Function GdipGraphicsSetAbort(pGraphics: PGpGraphics; pIAbort: PGdiplusAbort): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapConvertFormat(pInputBitmap : PGpBitmap; format: TPixelFormat; dithertype: TDitherType;
  palettetype: TPaletteType; palette: PColorPalette; alphaThresholdPercent: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipInitializePalette(
    palette:              PColorPalette;    // output palette. must be allocated.
    palettetype:          TPaletteType;     // palette enumeration type.
    optimalColors:        INT;              // how many optimal colors
    useTransparentColor:  BOOL;             // add a transparent color to the palette.
    bitmap:               PGpBitmap         // optional bitmap for median cut.
  ): TGpStatus; stdcall; external GDIPLIB;
    
Function GdipBitmapApplyEffect(bitmap: PGpBitmap; effect: PCGpEffect; roi: Windows.PRECT;
  useAuxData: BOOL; auxData: PPointer; auxDataSize: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapCreateApplyEffect(inputBitmaps: PPGpBitmap; numInputs: INT; effect: PCGpEffect; roi: Windows.PRECT;
  outputRect: Windows.PRECT; outputBitmap: PPGpBitmap; useAuxData: BOOL; auxData: PPointer; auxDataSize: PINT
  ): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapGetHistogram(bitmap: PGpBitmap; format: THistogramFormat; NumberOfEntries: UINT;
  channel0,channel1,channel2,channel3: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipBitmapGetHistogramSize(format: THistogramFormat; NumberOfEntries: PUINT): TGpStatus; stdcall; external GDIPLIB;

{$ELSE}
var
  GdipImageSetAbort: Function(pImage: PGpImage; pIAbort: PGdiplusAbort): TGpStatus; stdcall = nil;

  GdipGraphicsSetAbort: Function(pGraphics: PGpGraphics; pIAbort: PGdiplusAbort): TGpStatus; stdcall = nil;

  GdipBitmapConvertFormat: Function(pInputBitmap : PGpBitmap; format: TPixelFormat; dithertype: TDitherType; palettetype: TPaletteType;
                                    palette: PColorPalette; alphaThresholdPercent: REAL): TGpStatus; stdcall = nil;

  GdipInitializePalette: Function(palette: PColorPalette; palettetype: TPaletteType; optimalColors: INT; useTransparentColor: BOOL;
                                  bitmap: PGpBitmap): TGpStatus; stdcall = nil;
    
  GdipBitmapApplyEffect: Function(bitmap: PGpBitmap; effect: PCGpEffect; roi: Windows.PRECT; useAuxData: BOOL;
                                  auxData: PPointer; auxDataSize: PINT): TGpStatus; stdcall = nil;

  GdipBitmapCreateApplyEffect: Function(inputBitmaps: PPGpBitmap; numInputs: INT; effect: PCGpEffect; roi: Windows.PRECT; outputRect: Windows.PRECT;
                                        outputBitmap: PPGpBitmap; useAuxData: BOOL; auxData: PPointer; auxDataSize: PINT): TGpStatus; stdcall = nil;

  GdipBitmapGetHistogram: Function(bitmap: PGpBitmap; format: THistogramFormat; NumberOfEntries: UINT;
                                   channel0,channel1,channel2,channel3: PUINT): TGpStatus; stdcall = nil;

  GdipBitmapGetHistogramSize: Function(format: THistogramFormat; NumberOfEntries: PUINT): TGpStatus; stdcall = nil;

{$ENDIF}
{$IFEND}

Function GdipBitmapSetResolution(bitmap: PGpBitmap; xdpi,ydpi: REAL): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

Function GdipCreateImageAttributes(imageattr: PPGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneImageAttributes(imageattr: PGpImageAttributes; cloneImageattr: PPGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipDisposeImageAttributes(imageattr: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesToIdentity(imageattr: PGpImageAttributes; type_: TColorAdjustType): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetImageAttributes(imageattr: PGpImageAttributes; type_: TColorAdjustType): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesColorMatrix(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  colorMatrix,grayMatrix: PColorMatrix; flags: TColorMatrixFlags): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesThreshold(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  threshold: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesGamma(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  gamma: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesNoOp(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesColorKeys(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  colorLow,colorHigh: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesOutputChannel(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  channelFlags: TColorChannelFlags): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesOutputChannelColorProfile(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  colorProfileFilename: PWideChar): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesRemapTable(imageattr: PGpImageAttributes; type_: TColorAdjustType; enableFlag: BOOL;
  mapSize: UINT; map: PColorMap): TGpStatus; stdcall; external GDIPLIB;
  
Function GdipSetImageAttributesWrapMode(imageAttr: PGpImageAttributes; wrap: TWrapMode; argb: TARGB;
  clamp: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetImageAttributesICMMode(imageAttr: PGpImageAttributes; on_: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageAttributesAdjustedPalette(imageAttr: PGpImageAttributes; colorPalette: PColorPalette;
  colorAdjustType: TColorAdjustType): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

Function GdipFlush(graphics: PGpGraphics; intention: TGpFlushIntention): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateFromHDC(hdc: HDC; graphics: PPGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateFromHDC2(hdc: HDC; hDevice: HANDLE; graphics: PPGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateFromHWND(hwnd: HWND; graphics: PPGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateFromHWNDICM(hwnd: HWND; graphics: PPGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteGraphics(graphics: PGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetDC(graphics: PGpGraphics; hdc: PHDC): TGpStatus; stdcall; external GDIPLIB;

Function GdipReleaseDC(graphics: PGpGraphics; hdc: HDC): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCompositingMode(graphics: PGpGraphics; compositingMode: TCompositingMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCompositingMode(graphics: PGpGraphics; compositingMode: PCompositingMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetRenderingOrigin(graphics: PGpGraphics; x,y: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetRenderingOrigin(graphics: PGpGraphics; x,y: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetCompositingQuality(graphics: PGpGraphics; compositingQuality: TCompositingQuality): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCompositingQuality(graphics: PGpGraphics; compositingQuality: PCompositingQuality): TGpStatus; stdcall; external GDIPLIB;
                          
Function GdipSetSmoothingMode(graphics: PGpGraphics; smoothingMode: TSmoothingMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetSmoothingMode(graphics: PGpGraphics; smoothingMode: PSmoothingMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPixelOffsetMode(graphics: PGpGraphics; pixelOffsetMode: TPixelOffsetMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPixelOffsetMode(graphics: PGpGraphics; pixelOffsetMode: PPixelOffsetMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetTextRenderingHint(graphics: PGpGraphics; mode: TTextRenderingHint): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetTextRenderingHint(graphics: PGpGraphics; mode: PTextRenderingHint): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetTextContrast(graphics: PGpGraphics; contrast: UINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetTextContrast(graphics: PGpGraphics; contrast: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetInterpolationMode(graphics: PGpGraphics; interpolationMode: TInterpolationMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetInterpolationMode(graphics: PGpGraphics; interpolationMode: PInterpolationMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetWorldTransform(graphics: PGpGraphics; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetWorldTransform(graphics: PGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipMultiplyWorldTransform(graphics: PGpGraphics; matrix: PGpMatrix; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateWorldTransform(graphics: PGpGraphics; dx,dy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipScaleWorldTransform(graphics: PGpGraphics; sx,sy: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipRotateWorldTransform(graphics: PGpGraphics; angle: REAL; order: TGpMatrixOrder): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetWorldTransform(graphics: PGpGraphics; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetPageTransform(graphics: PGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPageUnit(graphics: PGpGraphics; unit_: PGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetPageScale(graphics: PGpGraphics; scale: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPageUnit(graphics: PGpGraphics; unit_: TGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetPageScale(graphics: PGpGraphics; scale: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetDpiX(graphics: PGpGraphics; dpi: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetDpiY(graphics: PGpGraphics; dpi: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformPoints(graphics: PGpGraphics; destSpace,srcSpace: TGpCoordinateSpace;
  points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipTransformPointsI(graphics: PGpGraphics; destSpace,srcSpace: TGpCoordinateSpace;
  points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetNearestColor(graphics: PGpGraphics; argb: PARGB): TGpStatus; stdcall; external GDIPLIB;

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors
Function GdipCreateHalftonePalette(): HPALETTE; stdcall; external GDIPLIB;

Function GdipDrawLine(graphics: PGpGraphics; pen: PGpPen; x1,y1,x2,y2: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawLineI(graphics: PGpGraphics; pen: PGpPen; x1,y1,x2,y2: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawLines(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawLinesI(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawArc(graphics: PGpGraphics; pen: PGpPen; x,y,width,height,startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawArcI(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: INT; startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawBezier(graphics: PGpGraphics; pen: PGpPen; x1,y1,x2,y2,x3,y3,x4,y4: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawBezierI(graphics: PGpGraphics; pen: PGpPen; x1,y1,x2,y2,x3,y3,x4,y4: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawBeziers(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawBeziersI(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawRectangle(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawRectangleI(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawRectangles(graphics: PGpGraphics; pen: PGpPen; rects: PGpRectF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawRectanglesI(graphics: PGpGraphics; pen: PGpPen; rects: PGpRect; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawEllipse(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawEllipseI(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawPie(graphics: PGpGraphics; pen: PGpPen; x,y,width,height,startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawPieI(graphics: PGpGraphics; pen: PGpPen; x,y,width,height: INT; startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawPolygon(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawPolygonI(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawPath(graphics: PGpGraphics; pen: PGpPen; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurve(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurveI(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurve2(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurve2I(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurve3(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count,offset,numberOfSegments: INT;
  tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCurve3I(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count,offset,numberOfSegments: INT;
  tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawClosedCurve(graphics: PGpGraphics; pen: PGpPen; points: pGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawClosedCurveI(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawClosedCurve2(graphics: PGpGraphics; pen: PGpPen; points: PGpPointF; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawClosedCurve2I(graphics: PGpGraphics; pen: PGpPen; points: PGpPoint; count: INT; tension: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGraphicsClear(graphics: PGpGraphics; color: TARGB): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillRectangle(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillRectangleI(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillRectangles(graphics: PGpGraphics; brush: PGpBrush; rects: PGpRectF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillRectanglesI(graphics: PGpGraphics; brush: PGpBrush; rects: PGpRect; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPolygon(graphics: PGpGraphics; brush: PGpBrush; points: PGpPointF; count: INT;
  fillMode: TGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPolygonI(graphics: PGpGraphics; brush: PGpBrush; points: PGpPoint; count: INT;
  fillMode: TGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPolygon2(graphics: PGpGraphics; brush: PGpBrush; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPolygon2I(graphics: PGpGraphics; brush: PGpBrush; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillEllipse(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillEllipseI(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPie(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height,startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPieI(graphics: PGpGraphics; brush: PGpBrush; x,y,width,height: INT;
  startAngle,sweepAngle: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillPath(graphics: PGpGraphics; brush: PGpBrush; path: PGpPath): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillClosedCurve(graphics: PGpGraphics; brush: PGpBrush; points: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillClosedCurveI(graphics: PGpGraphics; brush: PGpBrush; points: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillClosedCurve2(graphics: PGpGraphics; brush: PGpBrush; points: PGpPointF; count: INT; tension: REAL;
  fillMode: TGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillClosedCurve2I(graphics: PGpGraphics; brush: PGpBrush; points: PGpPoint; count: INT; tension: REAL;
  fillMode: TGpFillMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipFillRegion(graphics: PGpGraphics; brush: PGpBrush; region: PGpRegion): TGpStatus; stdcall; external GDIPLIB;

{$IF GDIPVER >= $0110}
{$IFDEF NewGDIPStatic}

Function GdipDrawImageFX(graphics: PGpGraphics; image: PGpImage; source: PGpRectF; xForm: PGpMatrix; effect: PCGpEffect;
  imageAttributes: PGpImageAttributes; srcUnit: TGpUnit): TGpStatus; stdcall; external GDIPLIB;

{$ELSE}
var
  GdipDrawImageFX: Function(graphics: PGpGraphics; image: PGpImage; source: PGpRectF; xForm: PGpMatrix; effect: PCGpEffect;
                            imageAttributes: PGpImageAttributes; srcUnit: TGpUnit): TGpStatus; stdcall = nil;

{$ENDIF}
{$IFEND}

Function GdipDrawImage(graphics: PGpGraphics; image: PGpImage; x,y: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImageI(graphics: PGpGraphics; image: PGpImage; x,y: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImageRect(graphics: PGpGraphics; image: PGpImage; x,y,width,height: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImageRectI(graphics: PGpGraphics; image: PGpImage; x,y,width,height: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePoints(graphics: PGpGraphics; image: PGpImage; dstpoints: PGpPointF; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePointsI(graphics: PGpGraphics; image: PGpImage; dstpoints: PGpPoint; count: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePointRect(graphics: PGpGraphics; image: PGpImage; x,y,srcx,srcy,srcwidth,srcheight: REAL;
  srcUnit: TGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePointRectI(graphics: PGpGraphics; image: PGpImage; x,y,srcx,srcy,srcwidth,srcheight: INT;
  srcUnit: TGpUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImageRectRect(graphics: PGpGraphics; image: PGpImage; dstx,dsty,dstwidth,dstheight,srcx,srcy,srcwidth,srcheight: REAL;
  srcUnit: TGpUnit; imageAttributes: PGpImageAttributes; callback: TDrawImageAbort; callbackData: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImageRectRectI(graphics: PGpGraphics; image: PGpImage; dstx,dsty,dstwidth,dstheight,srcx,srcy,srcwidth,srcheight: INT;
  srcUnit: TGpUnit; imageAttributes: PGpImageAttributes; callback: TDrawImageAbort; callbackData: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePointsRect(graphics: PGpGraphics; image: PGpImage; points: PGpPointF; count: INT; srcx,srcy,srcwidth,srcheight: REAL;
  srcUnit: TGpUnit; imageAttributes: PGpImageAttributes; callback: TDrawImageAbort; callbackData: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawImagePointsRectI(graphics: PGpGraphics; image: PGpImage; points: PGpPoint; count,srcx,srcy,srcwidth,srcheight: INT;
  srcUnit: TGpUnit; imageAttributes: PGpImageAttributes; callback: TDrawImageAbort; callbackData: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestPoint(graphics: PGpGraphics; metafile: PGpMetafile; destPoint: PPointF; callback: TEnumerateMetafileProc;
  callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestPointI(graphics: PGpGraphics; metafile: PGpMetafile; destPoint: PPoint; callback: TEnumerateMetafileProc;
  callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestRect(graphics: PGpGraphics; metafile: PGpMetafile; destRect: PRectF; callback: TEnumerateMetafileProc;
  callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestRectI(graphics: PGpGraphics; metafile: PGpMetafile; destRect: PRect; callback: TEnumerateMetafileProc;
  callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestPoints(graphics: PGpGraphics; metafile: PGpMetafile; destPoints: PPointF; count: INT;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileDestPointsI(graphics: PGpGraphics; metafile: PGpMetafile; destPoints: PPoint; count: INT;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestPoint(graphics: PGpGraphics; metafile: PGpMetafile; destPoint: PPointF; srcRect: PRectF; srcUnit: TUnit;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestPointI(graphics: PGpGraphics; metafile: PGpMetafile; destPoint: PPoint; srcRect: PRect; srcUnit: TUnit;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestRect(graphics: PGpGraphics; metafile: PGpMetafile; destRect,srcRect: PRectF; srcUnit: TUnit;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestRectI(graphics: PGpGraphics; metafile: PGpMetafile; destRect,srcRect: PRect; srcUnit: TUnit;
  callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestPoints(graphics: PGpGraphics; metafile: PGpMetafile; destPoints: PPointF; count: INT; srcRect: PRectF;
  srcUnit: TUnit; callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipEnumerateMetafileSrcRectDestPointsI(graphics: PGpGraphics; metafile: PGpMetafile; destPoints: PPoint; count: INT; srcRect: PRect;
  srcUnit: TUnit; callback: TEnumerateMetafileProc; callbackData: Pointer; imageAttributes: PGpImageAttributes): TGpStatus; stdcall; external GDIPLIB;

Function GdipPlayMetafileRecord(metafile: PGpMetafile; recordType: TEmfPlusRecordType; flags,dataSize: UINT;
  data: PBYTE): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipGraphics(graphics: PGpGraphics; srcgraphics: PGpGraphics; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipRect(graphics: PGpGraphics; x,y,width,height: REAL; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipRectI(graphics: PGpGraphics; x,y,width,height: INT; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipPath(graphics: PGpGraphics; path: PGpPath; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipRegion(graphics: PGpGraphics; region: PGpRegion; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetClipHrgn(graphics: PGpGraphics; hRgn: HRGN; combineMode: TCombineMode): TGpStatus; stdcall; external GDIPLIB;

Function GdipResetClip(graphics: PGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateClip(graphics: PGpGraphics; dx,dy: REAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipTranslateClipI(graphics: PGpGraphics; dx,dy: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetClip(graphics: PGpGraphics; region: PGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetClipBounds(graphics: PGpGraphics; rect: PGpRectF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetClipBoundsI(graphics: PGpGraphics; rect: PGpRect): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsClipEmpty(graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetVisibleClipBounds(graphics: PGpGraphics; rect: PGpRectF): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetVisibleClipBoundsI(graphics: PGpGraphics; rect: PGpRect): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleClipEmpty(graphics: PGpGraphics; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisiblePoint(graphics: PGpGraphics; x,y: REAL; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisiblePointI(graphics: PGpGraphics; x,y: INT; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRect(graphics: PGpGraphics; x,y,width,height: REAL; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsVisibleRectI(graphics: PGpGraphics; x,y,width,height: INT; result: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipSaveGraphics(graphics: PGpGraphics; state: PGraphicsState): TGpStatus; stdcall; external GDIPLIB;

Function GdipRestoreGraphics(graphics: PGpGraphics; state: TGraphicsState): TGpStatus; stdcall; external GDIPLIB;

Function GdipBeginContainer(graphics: PGpGraphics; dstrect,srcrect: PGpRectF; unit_: TGpUnit;
   state: PGraphicsContainer): TGpStatus; stdcall; external GDIPLIB;

Function GdipBeginContainerI(graphics: PGpGraphics; dstrect,srcrect: PGpRect; unit_: TGpUnit;
  state: PGraphicsContainer): TGpStatus; stdcall; external GDIPLIB;

Function GdipBeginContainer2(graphics: PGpGraphics; state: PGraphicsContainer): TGpStatus; stdcall; external GDIPLIB;

Function GdipEndContainer(graphics: PGpGraphics; state: TGraphicsContainer): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileHeaderFromWmf(hWmf: HMETAFILE; wmfPlaceableFileHeader: PWmfPlaceableFileHeader;
  header: PMetafileHeader): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileHeaderFromEmf(hEmf: HENHMETAFILE; header: PMetafileHeader): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileHeaderFromFile(filename: PWIdeChar; header: PMetafileHeader): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileHeaderFromStream(stream: IStream; header: PMetafileHeader): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileHeaderFromMetafile(metafile: PGpMetafile; header: PMetafileHeader): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetHemfFromMetafile(metafile: PGpMetafile; hEmf: PHENHMETAFILE): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateStreamOnFile(filename: PWideChar; access: UINT; stream: PIStream): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMetafileFromWmf(hWmf: HMETAFILE; deleteWmf: BOOL; wmfPlaceableFileHeader: PWmfPlaceableFileHeader;
  metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMetafileFromEmf(hEmf: HENHMETAFILE; deleteEmf: BOOL; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMetafileFromFile(file_: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMetafileFromWmfFile(file_: PWideChar; wmfPlaceableFileHeader: PWmfPlaceableFileHeader;
  metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateMetafileFromStream(stream: IStream; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafile(referenceHdc: HDC; type_: TEmfType; frameRect: PGpRectF; frameUnit: TMetafileFrameUnit;
  description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafileI(referenceHdc: HDC; type_: TEmfType; frameRect: PGpRect; frameUnit: TMetafileFrameUnit;
  description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafileFileName(fileName: PWideChar; referenceHdc: HDC; type_: TEmfType; frameRect: PGpRectF;
  frameUnit: TMetafileFrameUnit; description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafileFileNameI(fileName: PWideChar; referenceHdc: HDC; type_: TEmfType; frameRect: PGpRect;
  frameUnit: TMetafileFrameUnit; description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafileStream(stream: IStream; referenceHdc: HDC; type_: TEmfType; frameRect: PGpRectF;
  frameUnit: TMetafileFrameUnit; description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipRecordMetafileStreamI(stream: IStream; referenceHdc: HDC; type_: TEmfType; frameRect: PGpRect;
  frameUnit: TMetafileFrameUnit; description: PWideChar; metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetMetafileDownLevelRasterizationLimit(metafile: PGpMetafile; metafileRasterizationLimitDpi: UINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetMetafileDownLevelRasterizationLimit(metafile: PGpMetafile; metafileRasterizationLimitDpi: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageDecodersSize(numDecoders,size: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageDecoders(numDecoders,size: UINT; decoders: PImageCodecInfo): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageEncodersSize(numEncoders,size: PUINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetImageEncoders(numEncoders,size: UINT; encoders: PImageCodecInfo): TGpStatus; stdcall; external GDIPLIB;

Function GdipComment(graphics: PGpGraphics; sizeData: UINT; data: PBYTE): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

Function GdipCreateFontFamilyFromName(name: PWideChar; fontCollection: PGpFontCollection;
  fontFamily: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteFontFamily(fontFamily: PGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneFontFamily(fontFamily: PGpFontFamily; clonedFontFamily: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetGenericFontFamilySansSerif(nativeFamily: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetGenericFontFamilySerif(nativeFamily: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetGenericFontFamilyMonospace(nativeFamily: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFamilyName(family: PGpFontFamily; name: PWideChar; language: LANGID): TGpStatus; stdcall; external GDIPLIB;

Function GdipIsStyleAvailable(family: PGpFontFamily; style: INT; IsStyleAvailable: PBOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipFontCollectionEnumerable(fontCollection: PGpFontCollection; graphics: PGpGraphics;
  numFound: PINT): TGpStatus; stdcall; external GDIPLIB;

//!! gpfamilies is a pointer to array of PGpFontFamily, original declaration: GpFontFamily* gpfamilies[]
Function GdipFontCollectionEnumerate(fontCollection: PGpFontCollection; numSought: INT; gpfamilies: PPGpFontFamily;
  numFound: PINT; graphics: PGpGraphics): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetEmHeight(family: PGpFontFamily; style: INT; EmHeight: PUINT16): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCellAscent(family: PGpFontFamily; style: INT; CellAscent: PUINT16): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetCellDescent(family: PGpFontFamily; style: INT; CellDescent: PUINT16): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLineSpacing(family: PGpFontFamily; style: INT; LineSpacing: PUINT16): TGpStatus; stdcall; external GDIPLIB;  

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

Function GdipCreateFontFromDC(hdc: HDC; font: PPGpFont): TGpStatus; stdcall; external GDIPLIB;

Function GdipCreateFontFromLogfontA(hdc: HDC; logfont: PLOGFONTA; font: PPGpFont): TGpStatus; stdcall; external GDIPLIB;
Function GdipCreateFontFromLogfontW(hdc: HDC; logfont: PLOGFONTW; font: PPGpFont): TGpStatus; stdcall; external GDIPLIB; 
Function GdipCreateFontFromLogfont(hdc: HDC; logfont: PLOGFONT; font: PPGpFont): TGpStatus; stdcall; external GDIPLIB name
{$IFDEF Unicode}'GdipCreateFontFromLogfontW'{$ELSE}'GdipCreateFontFromLogfontA'{$ENDIF};

Function GdipCreateFont(fontFamily: PGpFontFamily; emSize: REAL; style: INT; unit_: TUnit; font: PPGpFont): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneFont(font: PGpFont; cloneFont: PPGpFont): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteFont(font: PGpFont): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFamily(font: PGpFont; family: PPGpFontFamily): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontStyle(font: PGpFont; style: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontSize(font: PGpFont; size: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontUnit(font: PGpFont; unit_: PUnit): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontHeight(font: PGpFont; graphics: PGpGraphics; height: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontHeightGivenDPI(font: PGpFont; dpi: REAL; height: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetLogFontA(font: PGpFont; graphics: PGpGraphics; logfontA: PLOGFONTA): TGpStatus; stdcall; external GDIPLIB;
Function GdipGetLogFontW(font: PGpFont; graphics: PGpGraphics; logfontW: PLOGFONTW): TGpStatus; stdcall; external GDIPLIB;
Function GdipGetLogFont(font: PGpFont; graphics: PGpGraphics; logfontW: PLOGFONT): TGpStatus; stdcall; external GDIPLIB name
{$IFDEF Unicode}'GdipGetLogFontW'{$ELSE}'GdipGetLogFontA'{$ENDIF};

Function GdipNewInstalledFontCollection(fontCollection: PPGpFontCollection): TGpStatus; stdcall; external GDIPLIB;

Function GdipNewPrivateFontCollection(fontCollection: PPGpFontCollection): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeletePrivateFontCollection(fontCollection: PPGpFontCollection): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontCollectionFamilyCount(fontCollection: PGpFontCollection; numFound: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetFontCollectionFamilyList(fontCollection: PGpFontCollection; numSought: INT; gpfamilies:
  PPGpFontFamily{!!pointer to array of PGpFontFamily}; numFound: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipPrivateAddFontFile(fontCollection: PGpFontCollection; filename: PWideChar): TGpStatus; stdcall; external GDIPLIB;

Function GdipPrivateAddMemoryFont(fontCollection: PGpFontCollection; memory: Pointer; length: INT): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

Function GdipDrawString(graphics: PGpGraphics; str: PWideChar; length: INT; font: PGpFont; layoutRect: PRectF; stringFormat: PGpStringFormat;
  brush: PGpBrush): TGpStatus; stdcall; external GDIPLIB;

Function GdipMeasureString(graphics: PGpGraphics; str: PWideChar; length: INT; font: PGpFont; layoutRect: PRectF; stringFormat: PGpStringFormat;
  boundingBox: PRectF; codepointsFitted: PINT; linesFilled: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipMeasureCharacterRanges(graphics: PGpGraphics; str: PWideChar; length: INT; font: PGpFont; layoutRect: PRectF;
  stringFormat: PGpStringFormat; regionCount: INT; regions: PPGpRegion): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawDriverString(graphics: PGpGraphics; text: PUINT16; length: INT; font: PGpFont; brush: PGpBrush;
  positions: PPointF; flags: INT; matrix: PGpMatrix): TGpStatus; stdcall; external GDIPLIB;

Function GdipMeasureDriverString(graphics: PGpGraphics; text: PUINT16; length: INT; font: PGpFont; positions: PPointF;
  flags: INT; matrix: PGpMatrix; boundingBox: PRectF): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

Function GdipCreateStringFormat(formatAttributes: INT; language: LANGID; format: PPGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipStringFormatGetGenericDefault(format: PPGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipStringFormatGetGenericTypographic(format: PPGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteStringFormat(format: PGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipCloneStringFormat(format: PGpStringFormat; newFormat: PPGpStringFormat): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatFlags(format: PGpStringFormat; flags: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatFlags(format: PGpStringFormat; flags: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatAlign(format: PGpStringFormat; align: TStringAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatAlign(format: PGpStringFormat; align: PStringAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatLineAlign(format: PGpStringFormat; align: TStringAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatLineAlign(format: PGpStringFormat; align: PStringAlignment): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatTrimming(format: PGpStringFormat; trimming: TStringTrimming): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatTrimming(format: PGpStringFormat; trimming: PStringTrimming): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatHotkeyPrefix(format: PGpStringFormat; hotkeyPrefix: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatHotkeyPrefix(format: PGpStringFormat; hotkeyPrefix: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatTabStops(format: PGpStringFormat; firstTabOffset: REAL; count: INT;
  tabStops: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatTabStops(format: PGpStringFormat; count: INT; firstTabOffset,tabStops: PREAL): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatTabStopCount(format: PGpStringFormat; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatDigitSubstitution(format: PGpStringFormat; language: LANGID;
  substitute: TStringDigitSubstitute): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatDigitSubstitution(format: PGpStringFormat; language: PLANGID;
  substitute: PStringDigitSubstitute): TGpStatus; stdcall; external GDIPLIB;

Function GdipGetStringFormatMeasurableCharacterRangeCount(format: PGpStringFormat; count: PINT): TGpStatus; stdcall; external GDIPLIB;

Function GdipSetStringFormatMeasurableCharacterRanges(format: PGpStringFormat; rangeCount: INT;
  ranges: PCharacterRange): TGpStatus; stdcall; external GDIPLIB;

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

Function GdipCreateCachedBitmap(bitmap: PGpBitmap; graphics: PGpGraphics; cachedBitmap: PPGpCachedBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipDeleteCachedBitmap(cachedBitmap: PGpCachedBitmap): TGpStatus; stdcall; external GDIPLIB;

Function GdipDrawCachedBitmap(graphics: PGpGraphics; cachedBitmap: PGpCachedBitmap; x,y: INT): TGpStatus; stdcall; external GDIPLIB;

Function GdipEmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: LPBYTE; iMapMode,eFlags: INT): UINT; stdcall; external GDIPLIB;

Function GdipSetImageAttributesCachedBackground(imageattr: PGpImageAttributes; enableFlag: BOOL): TGpStatus; stdcall; external GDIPLIB;

Function GdipTestControl(control: TGpTestControlEnum; param: Pointer): TGpStatus; stdcall; external GDIPLIB;

Function GdiplusNotificationHook(token: PULONG_PTR): TGpStatus; stdcall; external GDIPLIB;

procedure GdiplusNotificationUnhook(token: ULONG_PTR); stdcall; external GDIPLIB;

{$IF GDIPVER >= $0110}
{$IFDEF NewGDIPStatic}

Function GdipConvertToEmfPlus(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; emfType: TEmfType;
  description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipConvertToEmfPlusToFile(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; filename: PWideChar;
   emfType: TEmfType; description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

Function GdipConvertToEmfPlusToStream(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; stream: IStream;
   emfType: TEmfType; description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall; external GDIPLIB;

{$ELSE}
var
  GdipConvertToEmfPlus: Function(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; emfType: TEmfType;
                                 description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall = nil;

  GdipConvertToEmfPlusToFile: Function(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; filename: PWideChar;
                                       emfType: TEmfType; description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall = nil;

  GdipConvertToEmfPlusToStream: Function(refGraphics: PGpGraphics; metafile: PGpMetafile; conversionFailureFlag: PINT; stream: IStream;
                                         emfType: TEmfType; description: PWideChar; out_metafile: PPGpMetafile): TGpStatus; stdcall = nil;

{$ENDIF}
{$IFEND}


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   Image Attributes
*
* Abstract:
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\********************************************************************F******)
//!! that F is in the original too ;)
{!!=============================================================================
    TImageAttributes - class declaration
===============================================================================}
type
  TImageAttributes = class(TGdiPlusBase)
  protected
    fNativeImageAttr: PGpImageAttributes;
    fLastResult:      TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(ImageAttr: PGpImageAttributes; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeImageAttr(NativeImageAttrArg: PGpImageAttributes); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    constructor Create; overload;
    destructor Destroy; override;
    Function Clone: TImageAttributes;
    Function SetToIdentity(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function Reset(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetColorMatrix(ColorMatrix: PColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearColorMatrix(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetColorMatrices(ColorMatrix,GrayMatrix: PColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearColorMatrices(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetThreshold(Threshold: REAL; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearThreshold(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetGamma(Gamma: REAL; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearGamma(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetNoOp(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearNoOp(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetColorKey(const ColorLow,ColorHigh: TColor; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearColorKey(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetOutputChannel(ChannelFlags: TColorChannelFlags; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearOutputChannel(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetOutputChannelColorProfile(ColorProfileFilename: PWideChar;
      TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; overload; 
    Function SetOutputChannelColorProfile(const ColorProfileFilename: String;
      TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; overload; 
    Function ClearOutputChannelColorProfile(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetRemapTable(MapSize: UINT; Map: PColorMap; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus; 
    Function ClearRemapTable(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    Function SetBrushRemapTable(MapSize: UINT; Map: PColorMap): TStatus; 
    Function ClearBrushRemapTable: TStatus;
    Function SetWrapMode(Wrap: TWrapMode; const Color: TColor; Clamp: BOOL = False): TStatus; overload; 
    //!! since record parameters (Color) cannot have default value...
    Function SetWrapMode(Wrap: TWrapMode): TStatus; overload;
    // The flags of the palette are ignored.
    Function GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TStatus;       
    Function GetLastStatus: TStatus; 
  end;


(**************************************************************************\
* 
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusMatrix.h
*
* Abstract:
*
*   GDI+ Matrix class
*
\**************************************************************************)
{!!=============================================================================
    TMatrix - class declaration
===============================================================================}
type
  TMatrix = class(TMatrixBase)
  protected
    fNativeMatrix:  PGpMatrix;
    fLastResult:    TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativeMatrixArg: PGpMatrix); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeMatrix(NativeMatrixArg: PGpMatrix); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    // Default constructor is set to identity matrix.
    constructor Create; overload;
    constructor Create(M11,M12,M21,M22,DX,DY: REAL); overload;
    constructor Create(const Rect: TRectF; const DstPlg: TPointF); overload;
    constructor Create(const Rect: TRect; const DstPlg: TPoint); overload;
    destructor Destroy; override;
    Function Clone: TMatrix;
    Function GetElements(M: PREAL): TStatus; 
    Function SetElements(M11,M12,M21,M22,DX,DY: REAL): TStatus;
    Function OffsetX: REAL; 
    Function OffsetY: REAL;
    Function Reset: TStatus; 
    Function Multiply(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function Translate(OffsetX,OffsetY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function Scale(ScaleX,ScaleY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function Rotate(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function RotateAt(Angle: REAL; const Center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function Shear(ShearX,ShearY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function Invert: TStatus;
    // float version
    Function TransformPoints(Pts: PPointF; Count: INT = 1): TStatus; overload; 
    Function TransformPoints(Pts: PPoint; Count: INT = 1): TStatus; overload; 
    Function TransformVectors(Pts: PPointF; Count: INT = 1): TStatus; overload; 
    Function TransformVectors(Pts: PPoint; Count: INT = 1): TStatus; overload;
    Function IsInvertible: BOOL; 
    Function IsIdentity: BOOL;
    Function IsEqual(Matrix: TMatrix): BOOL;  //!! renamed from Equals (conflict with rtl method)
    Function GetLastStatus: TStatus; 
  end;


(**************************************************************************\
* 
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusBrush.h
*
* Abstract:
*
*   GDI+ Brush class
*
\**************************************************************************)
{!!=============================================================================
    TBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Abstract base class for various brush types
//--------------------------------------------------------------------------
type
  TBrush = class(TGdiPlusBase)
  protected
    fNativeBrush: PGpBrush;
    fLastResult:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create; overload;
    constructor Create(NativeBrushArg: PGpBrush; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeBrush(NativeBrushArg: PGpBrush); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    destructor Destroy; override;
    Function Clone: TBrush;
    Function GetType: TBrushType;
    Function GetLastStatus: TStatus; 
  end;

{!!=============================================================================
    TSolidBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Solid Fill Brush Object
//--------------------------------------------------------------------------
type
  TSolidBrush = class(TBrush)
  public
    constructor Create(const Color: TColor); overload;
    Function GetColor(aColor: PColor): TStatus;
    Function SetColor(const Color: TColor): TStatus; 
  end;

{!!=============================================================================
    TTextureBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Texture Brush Fill Object
//--------------------------------------------------------------------------
type
  TTextureBrush = class(TBrush)
  public
    constructor Create(Image: TImage; WrapMode: TWrapMode = WrapModeTile); overload;
    // When creating a texture brush from a metafile image, the dstRect
    // is used to specify the size that the metafile image should be
    // rendered at in the device units of the destination graphics.
    // It is NOT used to crop the metafile image, so only the width
    // and height values matter for metafiles.
    constructor Create(Image: TImage; WrapMode: TWrapMode; const DstRect: TRectF); overload;
    constructor Create(Image: TImage; const DstRect: TRectF; ImageAttributes: TImageAttributes = nil); overload;
    constructor Create(Image: TImage; const DstRect: TRect; ImageAttributes: TImageAttributes = nil); overload;
    constructor Create(Image: TImage; WrapMode: TWrapMode; const DstRect: TRect); overload;
    constructor Create(Image: TImage; WrapMode: TWrapMode; DstX,DstY,DstWidth,DstHeight: REAL); overload;
    constructor Create(Image: TImage; WrapMode: TWrapMode; DstX,DstY,DstWidth,DstHeight: INT); overload;
    Function SetTransform(Matrix: TMatrix): TStatus;
    Function GetTransform(Matrix: TMatrix): TStatus;
    Function ResetTransform: TStatus;
    Function MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function SetWrapMode(WrapMode: TWrapMode): TStatus; 
    Function GetWrapMode: TWrapMode;
    Function GetImage: TImage;
  end;

{!!=============================================================================
    TLinearGradientBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Linear Gradient Brush Object
//--------------------------------------------------------------------------
type
  TLinearGradientBrush = class(TBrush)
  public
    constructor Create(const Point1,Point2: TPointF; const Color1,Color2: TColor); overload;
    constructor Create(const Point1,Point2: TPoint; const Color1,Color2: TColor); overload;
    constructor Create(const Rect: TRectF; const Color1,Color2: TColor; Mode: TLinearGradientMode); overload;
    constructor Create(const Rect: TRect; const Color1,Color2: TColor; Mode: TLinearGradientMode); overload;
    constructor Create(const Rect: TRectF; const Color1,Color2: TColor; Angle: REAL; IsAngleScalable: BOOL = False); overload;
    constructor Create(const Rect: TRect; const Color1,Color2: TColor; Angle: REAL; IsAngleScalable: BOOL = False); overload;
    Function SetLinearColors(const Color1,Color2: TColor): TStatus; 
    Function GetLinearColors(Colors: PColor): TStatus;
    Function GetRectangle(Rect: PRectF): TStatus; overload; 
    Function GetRectangle(Rect: PRect): TStatus; overload;
    Function SetGammaCorrection(UseGammaCorrection: BOOL): TStatus; 
    Function GetGammaCorrection: BOOL;
    Function GetBlendCount: INT; 
    Function SetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus; 
    Function GetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
    Function GetInterpolationColorCount: INT; 
    Function SetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus; 
    Function GetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
    Function SetBlendBellShape(Focus: REAL; Scale: REAL = 1.0): TStatus; 
    Function SetBlendTriangularShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
    Function SetTransform(Matrix: TMatrix): TStatus;
    Function GetTransform(Matrix: TMatrix): TStatus;
    Function ResetTransform: TStatus;
    Function MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function SetWrapMode(WrapMode: TWrapMode): TStatus; 
    Function GetWrapMode: TWrapMode; 
  end;

//--------------------------------------------------------------------------
// PathGradientBrush object is defined
// in gdipluspath.h.
//--------------------------------------------------------------------------

{!!=============================================================================
    THatchBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Hatch Brush Object
//--------------------------------------------------------------------------
type
  THatchBrush = class(TBrush)
  public
    constructor Create(HatchStyle: THatchStyle; const ForeColor,BackColor: TColor); overload;
    constructor Create(HatchStyle: THatchStyle; const ForeColor: TColor); overload;  
    Function GetHatchStyle: THatchStyle; 
    Function GetForegroundColor(Color: PColor): TStatus; 
    Function GetBackgroundColor(Color: PColor): TStatus; 
  end;


(**************************************************************************\
* 
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusPen.h
*
* Abstract:
*
*   GDI+ Pen class
*
\**************************************************************************)
{!!=============================================================================
    TPen - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Pen class
//--------------------------------------------------------------------------
type
  TPen = class(TGdiPlusBase)
  protected
    fNativePen:   PGpPen;
    fLastResult:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(NativePenArg: PGpPen; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativePen(NativePenArg: PGpPen); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    constructor Create(const Color: TColor; Width: REAL = 1.0); overload;
    constructor Create(Brush: TBrush; Width: REAL = 1.0); overload;
    destructor Destroy; override;
    Function Clone: TPen;
    Function SetWidth(Width: REAL): TStatus; 
    Function GetWidht: REAL;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    Function SetLineCap(StartCap,EndCap: TLineCap; DashCap: TDashCap): TStatus; 
    Function SetStartCap(StartCap: TLineCap): TStatus; 
    Function SetEndCap(EndCap: TLineCap): TStatus; 
    Function SetDashCap(DashCap: TDashCap): TStatus; 
    Function GetStartCap: TLineCap; 
    Function GetEndCap: TLineCap; 
    Function GetDashCap: TDashCap;
    Function SetLineJoin(LineJoin: TLineJoin): TStatus; 
    Function GetLineJoin: TLineJoin;
    Function SetCustomStartCap(CustomCap: TCustomLineCap): TStatus;
    Function GetCustomStartCap(CustomCap: TCustomLineCap): TStatus;
    Function SetCustomEndCap(CustomCap: TCustomLineCap): TStatus; 
    Function GetCustomEndCap(CustomCap: TCustomLineCap): TStatus;
    Function SetMiterLimit(MiterLimit: REAL): TStatus; 
    Function GetMiterLimit: REAL;
    Function SetAlignment(PenAlignment: TPenAlignment): TStatus; 
    Function GetAlignment: TPenAlignment;
    Function SetTransform(Matrix: TMatrix): TStatus; 
    Function GetTransform(Matrix: TMatrix): TStatus; 
    Function ResetTransform: TStatus;
    Function MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus; 
    Function RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function GetPenType: TPenType; 
    Function SetColor(const Color: TColor): TStatus; 
    Function SetBrush(Brush: TBrush): TStatus; 
    Function GetColor(aColor: PColor): TStatus;
    Function GetBrush: TBrush;
    Function GetDashStyle: TDashStyle; 
    Function SetDashStyle(DashStyle: TDashStyle): TStatus;
    Function GetDashOffset: REAL; 
    Function SetDashOffset(DashOffset: REAL): TStatus;
    Function SetDashPattern(DashArray: PREAL; Count: INT): TStatus; 
    Function GetDashPatternCount: INT; 
    Function GetDashPattern(DashArray: PREAL; Count: INT): TStatus;
    Function SetCompoundArray(CompoundArray: PREAL; Count: INT): TStatus; 
    Function GetCompoundArrayCount: INT; 
    Function GetCompoundArray(CompoundArray: PREAL; Count: INT): TStatus;  
    Function GetLastStatus: TStatus; 
  end;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusStringFormat.h
*
* Abstract:
*
*   GDI+ StringFormat class
*
\**************************************************************************)
{!!=============================================================================
    TStringFormat - class declaration
===============================================================================}
type
  TStringFormat = class(TGdiPlusBase)
  protected
    fNativeFormat:  PGpStringFormat;
    fLastError:     TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
    Function SetStatus(NewStatus: TGpStatus): TStatus;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(ClonedStringFormat: PGpStringFormat; Status: TStatus); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  public
    constructor Create(FormatFlags: INT = 0; Language: LANGID = LANG_NEUTRAL); overload;
  {!!
    Implementation of methods GenericDefault and GenericTypographic differs
    significantly from original C++ headers.

    In the original, these functions return pointer to a global variable that
    technically contains the requested object. Not going into the gory details,
    this would be problematic in pascal.

    Therefore here, these funtions are instead returning full unique instance
    of TStringFormat class. This means that, unlike in the original, you must
    manually free the returned object after you are done with it.
  }
    class Function GenericDefault: TStringFormat;
    class Function GenericTypographic: TStringFormat;
    constructor Create(Format: TStringFormat); overload;
    Function Clone: TStringFormat; 
    destructor Destroy; override;
    Function SetFormatFlags(Flags: INT): TStatus; 
    Function GetFormatFlags: INT;
    Function SetAlignment(Align: TStringAlignment): TStatus; 
    Function GetAlignment: TStringAlignment;
    Function SetLineAlignment(Align: TStringAlignment): TStatus; 
    Function GetLineAlignment: TStringAlignment;
    Function SetHotkeyPrefix(HotkeyPrefix: THotkeyPrefix): TStatus; 
    Function GetHotkeyPrefix: THotkeyPrefix;
    Function SetTabStops(FirstTabOffset: REAL; Count: INT; TabStops: PREAL): TStatus; 
    Function GetTabStopCount: INT; 
    Function GetTabStops(Count: INT; FirstTabOffset,TabStops: PREAL): TStatus;
    Function SetDigitSubstitution(Language: LANGID; Substitute: TStringDigitSubstitute): TStatus; 
    Function GetDigitSubstitutionLanguage: LANGID; 
    Function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    Function SetTrimming(Trimming: TStringTrimming): TStatus; 
    Function GetTrimming: TStringTrimming;
    Function SetMeasurableCharacterRanges(RangeCount: INT; Ranges: PCharacterRange): TStatus; 
    Function GetMeasurableCharacterRangeCount: INT; 
    Function GetLastStatus: TStatus; 
  end;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusPath.h
*
* Abstract:
*
*   GDI+ Graphics Path class
*
\**************************************************************************)
{!!=============================================================================
    TGraphicsPath - class declaration
===============================================================================}
type
  TGraphicsPath = class(TGraphicsPathBase)
  protected
    fNativePath:  PGpPath;
    fLastResult:  TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(Path: TGraphicsPath); overload;
    constructor Create(NativePath: PGpPath); overload;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativePath(NativePathArg: PGpPath); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    constructor Create(FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPointF; Types: PBYTE; Count: INT; FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPoint; Types: PBYTE; Count: INT; FillMode: TFillMode = FillModeAlternate); overload;
    destructor Destroy; override;
    Function Clone: TGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    Function Reset: TStatus;
    Function GetFillMode: TFillMode; 
    Function SetFillMode(FillMode: TFillMode): TStatus;
    Function GetPathData(PathData: PPathData): TStatus;
    Function StartFigure: TStatus; 
    Function CloseFigure: TStatus; 
    Function CloseAllFigures: TStatus;
    Function SetMarker: TStatus; 
    Function ClearMarkers: TStatus;
    Function Reverse: TStatus;
    Function GetLastPoint(LastPoint: PPointF): TStatus;
    Function AddLine(const Pt1,Pt2: TPointF): TStatus; overload; 
    Function AddLine(X1,Y1,X2,Y2: REAL): TStatus; overload; 
    Function AddLines(Points: PPointF; Count: INT): TStatus; overload; 
    Function AddLine(const Pt1,Pt2: TPoint): TStatus; overload; 
    Function AddLine(X1,Y1,X2,Y2: INT): TStatus; overload; 
    Function AddLines(Points: PPoint; Count: INT): TStatus; overload;
    Function AddArc(const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddArc(X,Y,Width,Height: REAL; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddArc(const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddArc(X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function AddBezier(const Pt1,Pt2,Pt3,Pt4: TPointF): TStatus; overload; 
    Function AddBezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4: REAL): TStatus; overload; 
    Function AddBeziers(Points: PPointF; Count: INT): TStatus; overload; 
    Function AddBezier(const Pt1,Pt2,Pt3,Pt4: TPoint): TStatus; overload; 
    Function AddBezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4: INT): TStatus; overload; 
    Function AddBeziers(Points: PPoint; Count: INT): TStatus; overload;
    Function AddCurve(Points: PPointF; Count: INT): TStatus; overload; 
    Function AddCurve(Points: PPointF; Count: INT; Tension: REAL): TStatus; overload; 
    Function AddCurve(Points: PPointF; Count,Offset,NumberOfSegments: INT; Tension: REAL): TStatus; overload; 
    Function AddCurve(Points: PPoint; Count: INT): TStatus; overload; 
    Function AddCurve(Points: PPoint; Count: INT; Tension: REAL): TStatus; overload; 
    Function AddCurve(Points: PPoint; Count,Offset,NumberOfSegments: INT; Tension: REAL): TStatus; overload;
    Function AddClosedCurve(Points: PPointF; Count: INT): TStatus; overload; 
    Function AddClosedCurve(Points: PPointF; Count: INT; Tension: REAL): TStatus; overload; 
    Function AddClosedCurve(Points: PPoint; Count: INT): TStatus; overload; 
    Function AddClosedCurve(Points: PPoint; Count: INT; Tension: REAL): TStatus; overload;
    Function AddRectangle(const Rect: TRectF): TStatus; overload; 
    Function AddRectangles(Rects: PRectF; Count: INT): TStatus; overload;
    Function AddRectangle(const Rect: TRect): TStatus; overload; 
    Function AddRectangles(Rects: PRect; Count: INT): TStatus; overload;
    Function AddEllipse(const Rect: TRectF): TStatus; overload; 
    Function AddEllipse(X,Y,Width,Height: REAL): TStatus; overload; 
    Function AddEllipse(const Rect: TRect): TStatus; overload; 
    Function AddEllipse(X,Y,Width,Height: INT): TStatus; overload;
    Function AddPie(const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddPie(X,Y,Width,Height: REAL; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddPie(const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus; overload; 
    Function AddPie(X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function AddPolygon(Points: PPointF; Count: INT): TStatus; overload; 
    Function AddPolygon(Points: PPoint; Count: INT): TStatus; overload;
    Function AddPath(AddingPath: TGraphicsPath; Connect: BOOL): TStatus;  
    Function AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL{World units};
      const Origin: TPointF; Format: TStringFormat): TStatus; overload;
    Function AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const Origin: TPointF; Format: TStringFormat): TStatus; overload;
    Function AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const LayoutRect: TRectF; Format: TStringFormat): TStatus; overload;
    Function AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const LayoutRect: TRectF; Format: TStringFormat): TStatus; overload;
    Function AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const Origin: TPoint; Format: TStringFormat): TStatus; overload;
    Function AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const Origin: TPoint; Format: TStringFormat): TStatus; overload;
    Function AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const LayoutRect: TRect; Format: TStringFormat): TStatus; overload;
    Function AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
      const LayoutRect: TRect; Format: TStringFormat): TStatus; overload;
    Function Transform(Matrix: TMatrix): TStatus;
    // This is not always the tightest bounds.
    Function GetBounds(const Bounds: TRectF; Matrix: TMatrix = nil; Pen: TPen = nil): TStatus; overload; 
    Function GetBounds(const Bounds: TRect; Matrix: TMatrix = nil; Pen: TPen = nil): TStatus; overload;
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    Function Flatten(Matrix: TMatrix; Flatness: REAL): TStatus; overload; 
    Function Flatten(Matrix: TMatrix = nil): TStatus; overload;
    Function Widen(Pen: TPen; Matrix: TMatrix; Flatness: REAL): TStatus; overload; 
    Function Widen(Pen: TPen; Matrix: TMatrix = nil): TStatus; overload;
    Function Outline(Matrix: TMatrix; Flatness: REAL): TStatus; overload; 
    Function Outline(Matrix: TMatrix = nil): TStatus; overload;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the 
    // identity matrix is assumed.
    Function Warp(DestPoints: PPointF; Count: INT; const SrcRect: TRectF; Matrix: TMatrix;
      WarpMode: TWarpMode; Flatness: REAL): TStatus; overload; 
    Function Warp(DestPoints: PPointF; Count: INT; const SrcRect: TRectF; Matrix: TMatrix = nil;
      WarpMode: TWarpMode = WarpModePerspective): TStatus; overload;
    Function GetPointCount: INT;
    Function GetPathTypes(Types: PBYTE; Count: INT): TStatus; 
    Function GetPathPoints(Points: PPointF; Count: INT): TStatus; overload; 
    Function GetPathPoints(Points: PPoint; Count: INT): TStatus; overload;
    Function GetLastStatus: TStatus;
    Function IsVisible(const Point: TPointF; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(X,Y: REAL; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(const Point: TPoint; G: TGraphicsBase = nil): BOOL; overload;
    Function IsVisible(X,Y: INT; G: TGraphicsBase = nil): BOOL; overload;
    Function IsOutlineVisible(const Point: TPointF; Pen: TPen; G: TGraphicsBase = nil): BOOL; overload;
    Function IsOutlineVisible(X,Y: REAL; Pen: TPen; G: TGraphicsBase = nil): BOOL; overload;
    Function IsOutlineVisible(const Point: TPoint; Pen: TPen; G: TGraphicsBase = nil): BOOL; overload; 
    Function IsOutlineVisible(X,Y: INT; Pen: TPen; G: TGraphicsBase = nil): BOOL; overload; 
  end;


{!!=============================================================================
    TGraphicsPathIterator - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// GraphisPathIterator class
//-------------------------------------------------------------------------- 
type
  TGraphicsPathIterator = class(TGdiPlusBase)
  protected
    fNativeIterator:  PGpPathIterator;
    fLastResult:      TStatus;
    procedure SetNativeIterator(NativeIteratorArg: PGpPathIterator); 
    Function SetStatus(Status: TStatus): TStatus; 
  public
    constructor Create(Path: TGraphicsPath);
    destructor Destroy; override;
    Function NextSubPath(StartIndex,EndIndex: PINT; IsClosed: PBOOL): INT; overload; 
    Function NextSubPath(Path: TGraphicsPath; IsClosed: PBOOL): INT; overload;
    Function NextPathType(PathType: PBYTE; StartIndex,EndIndex: PINT): INT;
    Function NextMarker(StartIndex,EndIndex: PINT): INT; overload; 
    Function NextMarker(Path: TGraphicsPath): INT; overload;
    Function GetCount: INT; 
    Function GetSubpathCount: INT;
    Function HasCurve: BOOL;
    procedure Rewind;
    Function Enumerate(Points: PPointF; Types: PBYTE; Count: INT): INT; 
    Function CopyData(Points: PPointF; Types: PBYTE; StartIndex,EndIndex: INT): INT; 
    Function GetLastStatus: TStatus; 
  end;

{!!=============================================================================
    TPathGradientBrush - class declaration
===============================================================================}
//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------
type
  TPathGradientBrush = class(TBrush)
  public
    constructor Create(Points: PPointF; Count: INT; WrapMode: TWrapMode = WrapModeClamp); overload;
    constructor Create(Points: PPoint; Count: INT; WrapMode: TWrapMode = WrapModeClamp); overload;
    constructor Create(Path: TGraphicsPath); overload;
    Function GetCenterColor(Color: PColor): TStatus; 
    Function SetCenterColor(const Color: TColor): TStatus;
    Function GetPointCount: INT; 
    Function GetSurroundColorCount: INT;
    Function GetSurroundColors(Colors: PColor; Count: PINT): TStatus; 
    Function SetSurroundColors(Colors: PColor; Count: PINT): TStatus;
    Function GetGraphicsPath(Path: TGraphicsPath): TStatus; 
    Function SetGraphicsPath(Path: TGraphicsPath): TStatus;
    Function GetCenterPoint(Point: PPointF): TStatus; overload; 
    Function GetCenterPoint(Point: PPoint): TStatus; overload; 
    Function SetCenterPoint(const Point: TPointF): TStatus; overload; 
    Function SetCenterPoint(const Point: TPoint): TStatus; overload;
    Function GetRectangle(Rect: PRectF): TStatus; overload; 
    Function GetRectangle(Rect: PRect): TStatus; overload;
    Function SetGammaCorrection(UseGammaCorrection: BOOL): TStatus;
    Function GetGammaCorrection: BOOL;
    Function GetBlendCount: INT; 
    Function GetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus; 
    Function SetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
    Function GetInterpolationColorCount: INT; 
    Function SetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus; 
    Function GetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
    Function SetBlendBellShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
    Function SetBlendTriangularShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
    Function GetTransform(Matrix: TMatrix): TStatus;
    Function SetTransform(Matrix: TMatrix): TStatus;
    Function ResetTransform: TStatus;
    Function MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function GetFocusScales(XScale,YScale: PREAL): TStatus;
    Function SetFocusScales(XScale,YScale: REAL): TStatus;
    Function GetWrapMode: TWrapMode;
    Function SetWrapMode(WrapMode: TWrapMode): TStatus;
  end;


(**************************************************************************\
* 
* Copyright (c) 2000-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:        
* 
*    GdiplusLineCaps.h
*
* Abstract:
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)
{!!=============================================================================
    TAdjustableArrowCap - class declaration
===============================================================================}
type
  TAdjustableArrowCap = class(TCustomLineCap)
  public
    constructor Create(Height,Width: REAL; IsFilled: BOOL = True); overload;
    Function SetHeight(Height: REAL): TStatus;
    Function GetHeight: REAL;
    Function SetWidth(Width: REAL): TStatus;
    Function GetWidth: REAL;
    Function SetMiddleInset(MiddleInset: REAL): TStatus;
    Function GetMiddleInset: REAL; 
    Function SetFillState(IsFilled: BOOL): TStatus;
    Function IsFilled: BOOL;
  end;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusGraphics.h
*
* Abstract:
*
*   GDI+ Graphics Object
*
\**************************************************************************)
{!!=============================================================================
    TGraphics - class declaration
===============================================================================}
type
  TGraphics = class(TGraphicsBase)
  protected
    fNativeGraphics:  PGpGraphics;
    fLastResult:      TStatus;
    Function GetNativeObject: Pointer; override;
    Function GetNativeObjectAddr: Pointer; override;
  {$IFDEF FPCDWM}{$PUSH}W3018{$ENDIF}
    constructor Create(Graphics: PGpGraphics);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    procedure SetNativeGraphics(Graphics: PGpGraphics);
    Function SetStatus(Status: TStatus): TStatus;
    Function GetNativeGraphics: PGpGraphics;
    Function GetNativePen(Pen: TPen): PGpPen;
  public
    class Function FromHDC(hDC: HDC): TGraphics; overload;
    class Function FromHDC(hDC: HDC; hDevice: HANDLE): TGraphics; overload;
    class Function FromHWND(hWnd: HWND; ICM: BOOL = False): TGraphics;
    class Function FromImage(Image: TImage): TGraphics;
  {!!
    Following constructors have extended names (ie. not just "Create") because
    delphi cannot distinguish between HDC and HWND (it sees them as the same
    type) and select proper overload/constructor.
  }
    constructor CreateFromHDC(hDC: HDC); overload;
    constructor CreateFromHDC(hDC: HDC; hDevice: HANDLE); overload;
    constructor CreateFromHWND(hWnd: HWND; ICM: BOOL = False);
    constructor CreateFromImage(Image: TImage);
    destructor Destroy; override;
    procedure Flush(Intention: TFlushIntention = FlushIntentionFlush);
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    Function GetHDC: HDC;
    procedure ReleaseHDC(hDC: HDC);
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    Function SetRenderingOrigin(X,Y: INT): TStatus;
    Function GetRenderingOrigin(X,Y: PINT): TStatus; 
    Function SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
    Function GetCompositingMode: TCompositingMode;
    Function SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
    Function GetCompositingQuality: TCompositingQuality;
    Function SetTextRenderingHint(NewMode: TTextRenderingHint): TStatus;
    Function GetTextRenderingHint: TTextRenderingHint;
    Function SetTextContrast(Contrast: UINT): TStatus;
    Function GetTextContrast: UINT;
    Function GetInterpolationMode: TInterpolationMode;
    Function SetInterpolationMode(InterpolationMode: TInterpolationMode): TStatus;
  {$IF GDIPVER >= $0110}
    Function SetAbort(PIAbort: PGdiplusAbort): TStatus;
  {$IFEND}
    Function GetSmoothingMode: TSmoothingMode;
    Function SetSmoothingMode(SmoothingMode: TSmoothingMode): TStatus;
    Function GetPixelOffsetMode: TPixelOffsetMode;
    Function SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TStatus;
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    Function SetTransform(Matrix: TMatrix): TStatus;
    Function ResetTransform: TStatus;
    Function MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    Function GetTransform(Matrix: TMatrix): TStatus;
    Function SetPageUnit(PageUnit: TUnit): TStatus;
    Function SetPageScale(Scale: REAL): TStatus;
    Function GetPageUnit: TUnit;
    Function GetPageScale: REAL;
    Function GetDpiX: REAL;
    Function GetDpiY: REAL;
    Function TransformPoints(DestSpace,SrcSpace: TCoordinateSpace; Pts: PPointF; Count: INT): TStatus; overload;
    Function TransformPoints(DestSpace,SrcSpace: TCoordinateSpace; Pts: PPoint; Count: INT): TStatus; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    Function GetNearestColor(Color: PColor): TStatus;
    Function DrawLine(Pen: TPen; X1,Y1,X2,Y2: REAL): TStatus; overload;
    Function DrawLine(Pen: TPen; const Pt1,Pt2: TPointF): TStatus; overload;
    Function DrawLines(Pen: TPen; Points: PPointF; Count: INT): TStatus; overload;
    Function DrawLine(Pen: TPen; X1,Y1,X2,Y2: INT): TStatus; overload;
    Function DrawLine(Pen: TPen; const Pt1,Pt2: TPoint): TStatus; overload;
    Function DrawLines(Pen: TPen; Points: PPoint; Count: INT): TStatus; overload;
    Function DrawArc(Pen: TPen; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawArc(Pen: TPen; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawArc(Pen: TPen; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawArc(Pen: TPen; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawBezier(Pen: TPen; X1,Y1,X2,Y2,X3,Y3,X4,Y4: REAL): TStatus; overload;
    Function DrawBezier(Pen: TPen; const Pt1,Pt2,Pt3,Pt4: TPointF): TStatus; overload;
    Function DrawBeziers(Pen: TPen; Points: PPointF; Count: INT): TStatus; overload;
    Function DrawBezier(Pen: TPen; X1,Y1,X2,Y2,X3,Y3,X4,Y4: INT): TStatus; overload;
    Function DrawBezier(Pen: TPen; const Pt1,Pt2,Pt3,Pt4: TPoint): TStatus; overload;
    Function DrawBeziers(Pen: TPen; Points: PPoint; Count: INT): TStatus; overload;
    Function DrawRectangle(Pen: TPen; const Rect: TRectF): TStatus; overload;
    Function DrawRectangle(Pen: TPen; X,Y,Width,Height: REAL): TStatus; overload;
    Function DrawRectangles(Pen: TPen; Rects: PRectF; Count: INT): TStatus; overload;
    Function DrawRectangle(Pen: TPen; const Rect: TRect): TStatus; overload;
    Function DrawRectangle(Pen: TPen; X,Y,Width,Height: INT): TStatus; overload;
    Function DrawRectangles(Pen: TPen; Rects: PRect; Count: INT): TStatus; overload;
    Function DrawEllipse(Pen: TPen; const Rect: TRectF): TStatus; overload;
    Function DrawEllipse(Pen: TPen; X,Y,Width,Height: REAL): TStatus; overload;
    Function DrawEllipse(Pen: TPen; const Rect: TRect): TStatus; overload;
    Function DrawEllipse(Pen: TPen; X,Y,Width,Height: INT): TStatus; overload;
    Function DrawPie(Pen: TPen; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawPie(Pen: TPen; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawPie(Pen: TPen; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawPie(Pen: TPen; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function DrawPolygon(Pen: TPen; Points: PPointF; Count: INT): TStatus; overload;
    Function DrawPolygon(Pen: TPen; Points: PPoint; Count: INT): TStatus; overload;
    Function DrawPath(Pen: TPen; Path: TGraphicsPath): TStatus;
    Function DrawCurve(Pen: TPen; Points: PPointF; Count: INT): TStatus; overload;
    Function DrawCurve(Pen: TPen; Points: PPointF; Count: INT; Tension: REAL): TStatus; overload;
    Function DrawCurve(Pen: TPen; Points: PPointF; Count,Offset,NumberOfSegments: INT; Tension: REAL = 0.5): TStatus; overload;
    Function DrawCurve(Pen: TPen; Points: PPoint; Count: INT): TStatus; overload;
    Function DrawCurve(Pen: TPen; Points: PPoint; Count: INT; Tension: REAL): TStatus; overload;
    Function DrawCurve(Pen: TPen; Points: PPoint; Count,Offset,NumberOfSegments: INT; Tension: REAL = 0.5): TStatus; overload;
    Function DrawClosedCurve(Pen: TPen; Points: PPointF; Count: INT): TStatus; overload;
    Function DrawClosedCurve(Pen: TPen; Points: PPointF; Count: INT; Tension: REAL): TStatus; overload;
    Function DrawClosedCurve(Pen: TPen; Points: PPoint; Count: INT): TStatus; overload;
    Function DrawClosedCurve(Pen: TPen; Points: PPoint; Count: INT; Tension: REAL): TStatus; overload;
    Function Clear(const Color: TColor): TStatus;
    Function FillRectangle(Brush: TBrush; const Rect: TRectF): TStatus; overload;
    Function FillRectangle(Brush: TBrush; X,Y,Width,Height: REAL): TStatus; overload;
    Function FillRectangles(Brush: TBrush; Rects: PRectF; Count: INT): TStatus; overload;
    Function FillRectangle(Brush: TBrush; const Rect: TRect): TStatus; overload;
    Function FillRectangle(Brush: TBrush; X,Y,Width,Height: INT): TStatus; overload;
    Function FillRectangles(Brush: TBrush; Rects: PRect; Count: INT): TStatus; overload;
    Function FillPolygon(Brush: TBrush; Points: PPointF; Count: INT): TStatus; overload;
    Function FillPolygon(Brush: TBrush; Points: PPointF; Count: INT; FillMode: TFillMode): TStatus; overload;
    Function FillPolygon(Brush: TBrush; Points: PPoint; Count: INT): TStatus; overload;
    Function FillPolygon(Brush: TBrush; Points: PPoint; Count: INT; FillMode: TFillMode): TStatus; overload;
    Function FillEllipse(Brush: TBrush; const Rect: TRectF): TStatus; overload;
    Function FillEllipse(Brush: TBrush; X,Y,Width,Height: REAL): TStatus; overload;
    Function FillEllipse(Brush: TBrush; const Rect: TRect): TStatus; overload;
    Function FillEllipse(Brush: TBrush; X,Y,Width,Height: INT): TStatus; overload;
    Function FillPie(Brush: TBrush; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function FillPie(Brush: TBrush; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus; overload;
    Function FillPie(Brush: TBrush; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function FillPie(Brush: TBrush; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus; overload;
    Function FillPath(Brush: TBrush; Path: TGraphicsPath): TStatus;
    Function FillClosedCurve(Brush: TBrush; Points: PPointF; Count: INT): TStatus; overload;
    Function FillClosedCurve(Brush: TBrush; Points: PPointF; Count: INT; FillMode: TFillMode; Tension: REAL = 0.5): TStatus; overload;
    Function FillClosedCurve(Brush: TBrush; Points: PPoint; Count: INT): TStatus; overload;
    Function FillClosedCurve(Brush: TBrush; Points: PPoint; Count: INT; FillMode: TFillMode; Tension: REAL = 0.5): TStatus; overload;
    Function FillRegion(Brush: TBrush; Region: TRegion): TStatus;
    Function DrawString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      Brush: TBrush): TStatus; overload;
    Function DrawString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      Brush: TBrush): TStatus; overload;
    Function DrawString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; Brush: TBrush): TStatus; overload;
    Function DrawString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; Brush: TBrush): TStatus; overload;
    Function DrawString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
      Brush: TBrush): TStatus; overload;
    Function DrawString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
      Brush: TBrush): TStatus; overload;
    Function MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      BoundingBox: PRectF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus; overload;
    Function MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      BoundingBox: PRectF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus; overload;
    Function MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRectSize: TSizeF; StringFormat: TStringFormat;
      Size: PSizeF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus; overload;
    Function MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRectSize: TSizeF; StringFormat: TStringFormat;
      Size: PSizeF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus; overload;
    Function MeasureString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
      BoundingBox: PRectF): TStatus; overload;
    Function MeasureString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
      BoundingBox: PRectF): TStatus; overload;
    Function MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; BoundingBox: PRectF): TStatus; overload;
    Function MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; BoundingBox: PRectF): TStatus; overload;
    Function MeasureString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; BoundingBox: PRectF): TStatus; overload;
    Function MeasureString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; BoundingBox: PRectF): TStatus; overload;
    Function MeasureCharacterRanges(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      RegionCount: INT; Regions: PRegion): TStatus; overload;
    Function MeasureCharacterRanges(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
      RegionCount: INT; Regions: PRegion): TStatus; overload;
    Function DrawDriverString(Text: PUINT16; Length: INT; Font: TFont; Brush: TBrush; Positions: PPointF;
      Flags: INT; Matrix: TMatrix): TStatus;
    Function MeasureDriverString(Text: PUINT16; Length: INT; Font: TFont; Positions: PPointF; Flags: INT;
      Matrix: TMatrix; BoundingBox: PRectF): TStatus;
    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    Function DrawCachedBitmap(CB: TCachedBitmap; X,Y: INT): TStatus;
    Function DrawImage(Image: TImage; const Point: TPointF): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y: REAL): TStatus; overload;
    Function DrawImage(Image: TImage; const Rect: TRectF): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y,Width,Height: REAL): TStatus; overload;
    Function DrawImage(Image: TImage; const Point: TPoint): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y: INT): TStatus; overload;
    Function DrawImage(Image: TImage; const Rect: TRect): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y,Width,Height: INT): TStatus; overload;
    Function DrawImage(Image: TImage; DestPoints: PPointF; Count: INT): TStatus; overload;
    Function DrawImage(Image: TImage; DestPoints: PPoint; Count: INT): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y,SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit): TStatus; overload;
    Function DrawImage(Image: TImage; const DestRect: TRectF; SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit;
      ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus; overload;
    Function DrawImage(Image: TImage; DestPoints: PPointF; Count: INT; SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit;
      ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus; overload;
    Function DrawImage(Image: TImage; X,Y,SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit): TStatus; overload;
    Function DrawImage(Image: TImage; const DestRect: TRect; SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit;
      ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus; overload;
    Function DrawImage(Image: TImage; DestPoints: PPoint; Count: INT; SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit;
      ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus; overload;
  {$IF GDIPVER >= $0110}
    Function DrawImage(Image: TImage; const DestRect,SourceRect: TRectF; SrcUnit: TUnit;
      ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function DrawImage(Image: TImage; const SourceRect: TRectF; XForm: TMatrix; Effect: TEffect;
      ImageAttributes: TImageAttributes; SrcUnit: TUnit): TStatus; overload;
  {$IFEND}
    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    Function EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPointF; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPoint; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestRect: TRectF; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestRect: TRect; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPointF; Count: INT; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPoint; Count: INT; Callback: TEnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;      
    Function EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPointF; const SrcRect: TRectF; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPoint; const SrcRect: TRect; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestRect,SrcRect: TRectF; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; const DestRect,SrcRect: TRect; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPointF; Count: INT; const SrcRect: TRectF; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPoint; Count: INT; const SrcRect: TRect; SrcUnit: TUnit;
      Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus; overload;
    Function SetClip(G: TGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    Function SetClip(const Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    Function SetClip(const Rect: TRect; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    Function SetClip(Path: TGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    Function SetClip(Region: TRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    Function SetClip(HRgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    Function IntersectClip(const Rect: TRectF): TStatus; overload;
    Function IntersectClip(const Rect: TRect): TStatus; overload;
    Function IntersectClip(Region: TRegion): TStatus; overload;
    Function ExcludeClip(const Rect: TRectF): TStatus; overload;
    Function ExcludeClip(const Rect: TRect): TStatus; overload;
    Function ExcludeClip(Region: TRegion): TStatus; overload;
    Function ResetClip: TStatus;
    Function TranslateClip(DX,DY: REAL): TStatus; overload;
    Function TranslateClip(DX,DY: INT): TStatus; overload;
    Function GetClip(Region: TRegion): TStatus;
    Function GetClipBounds(Rect: PRectF): TStatus; overload;
    Function GetClipBounds(Rect: PRect): TStatus; overload;
    Function IsClipEmpty: BOOL;
    Function GetVisibleClipBounds(Rect: PRectF): TStatus; overload;
    Function GetVisibleClipBounds(Rect: PRect): TStatus; overload;
    Function IsVisibleClipEmpty: BOOL;
    Function IsVisible(X,Y: INT): BOOL; overload;
    Function IsVisible(const Point: TPoint): BOOL; overload;
    Function IsVisible(X,Y,Width,Height: INT): BOOL; overload;
    Function IsVisible(const Rect: TRect): BOOL; overload;
    Function IsVisible(X,Y: REAL): BOOL; overload;
    Function IsVisible(const Point: TPointF): BOOL; overload;
    Function IsVisible(X,Y,Width,Height: REAL): BOOL; overload;
    Function IsVisible(const Rect: TRectF): BOOL; overload;
    Function Save: TGraphicsState;
    Function Restore(GState: TGraphicsState): TStatus;
    Function BeginContainer(const DstRect,SrcRect: TRectF; aUnit: TUnit): TGraphicsContainer; overload;
    Function BeginContainer(const DstRect,SrcRect: TRect; aUnit: TUnit): TGraphicsContainer; overload;
    Function BeginContainer: TGraphicsContainer; overload;
    Function EndContainer(State: TGraphicsContainer): TStatus;
    // Only valid when recording metafiles.
    Function AddMetafileComment(Data: PBYTE; SizeData: UINT): TStatus;
    Function GetHalftonePalette: HPALETTE;
    Function GetLastStatus: TStatus;
  end;

//----------------------------------------------------------------------------
// Implementation of GraphicsPath methods that use Graphics
//----------------------------------------------------------------------------

{!!
  Implemantation of methods TGraphicsPath.GetBounds, TGraphicsPath.IsVisible
  and TGraphicsPath.IsOutlineVisible, originally placed here, were moved to
  main implementation of TGraphicsPath class.
}


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusMetafile.h
*
* Abstract:
*
*   GDI+ Metafile class
*
\**************************************************************************)


(**************************************************************************
*
* Copyright (c) 2000 Microsoft Corporation
*
* Module Name:
*
*   CachedBitmap class definition
*
* Abstract:
*
*   GDI+ CachedBitmap is a representation of an accelerated drawing
*   that has restrictions on what operations are allowed in order
*   to accelerate the drawing to the destination.
*
*   Look for class definition in GdiplusHeaders.h
*
**************************************************************************)


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusRegion.h
*
* Abstract:
*
*   GDI+ Region class implementation
*
\**************************************************************************)
type
  TRegionArray = array[0..Pred(MaxInt div SizeOf(TRegion))] of TRegion;
  PRegionArray = ^TRegionArray;


(**************************************************************************\
*
* Copyright (c) 2000, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
* 
*   GdiplusFontCollection.h
*
* Abstract:
*
*   Font collections (Installed and Private)
*
\**************************************************************************)


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusFontFamily.h
*
* Abstract:
*
*   GDI+ Font Family class
*
\**************************************************************************)
type
  TFontFamilyArray = array[0..Pred(MaxInt div SizeOf(TFontFamily))] of TFontFamily;
  PFontFamilyArray = ^TFontFamilyArray;


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusFont.h
*
* Abstract:
*
*   GDI+ Font class
*
\**************************************************************************)


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusBitmap.h
*
* Abstract:
*
*   GDI+ Bitmap class
*
\**************************************************************************)
type
  TBitmapArray = array[0..Pred(MaxInt div SizeOf(TBitmap))] of TBitmap;
  PBitmapArray = ^TBitmapArray;


(**************************************************************************\
*
* Copyright (c) 2000-2001, Microsoft Corp.  All Rights Reserved.
*
* Module Name:
*
*   GdiplusImageCodec.h
*
* Abstract:
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)
//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

Function GetImageDecodersSize(NumDecoders: PUINT; Size: PUINT): TStatus;{$IFDEF CanInline} inline;{$ENDIF}

Function GetImageDecoders(NumDecoders: UINT; Size: UINT; Decoders: PImageCodecInfo): TStatus;{$IFDEF CanInline} inline;{$ENDIF}

Function GetImageEncodersSize(NumEncoders: PUINT; Size: PUINT): TStatus;{$IFDEF CanInline} inline;{$ENDIF}

Function GetImageEncoders(NumEncoders: UINT; Size: UINT; Encoders: PImageCodecInfo): TStatus;{$IFDEF CanInline} inline;{$ENDIF}


{!!*****************************************************************************
    Helpers - declaration
*******************************************************************************}
{!!
  VersionSupported

  Checks for existance of gdiplus.dll and selected symbols (functions) exported
  by it.

  If the library cannot be loaded, it returns 0, othervise following values
  can be returned (note that if a value not listed here is returned, it should
  be considered an error):

    1 ... GDI+ version 1.0 is supported
    2 ... GDI+ version 1.1 and older is supported
}
Function VersionSupported: Integer;

//!!----------------------------------------------------------------------------

Function StatusAsStr(Status: TStatus): String;

Function StatusCheck(Status: TStatus): Boolean;
Function StatusRaise(Status: TStatus): TStatus;

//!!----------------------------------------------------------------------------
type
  TCodecString = (csName,csDLLName,csFmtDescr,csFileExt,csMimeType);

procedure GetDecoders(List: TStringList; CodecString: TCodecString = csMimeType);
procedure GetEncoders(List: TStringList; CodecString: TCodecString = csMimeType);

//!!----------------------------------------------------------------------------

Function GetDecoderCLSID(const MimeType: String): TCLSID;
Function GetEncoderCLSID(const MimeType: String): TCLSID;

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$PUSH}{$WARN 2005 OFF}           // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
    {$DEFINE W4110:={$WARN 4110 OFF}} // range check error while evaluating constants ($1 must be between $2 and $3)
  {$ELSE}
    {$DEFINE W4110:=}
  {$IFEND}
  {$POP}
{$ENDIF}

uses
  DynLibUtils, StrRect;

{!!*****************************************************************************
    gdiplusbase.h
*******************************************************************************}
{!!=============================================================================
    TGdiPlusWrapper - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TGdiPlusWrapper - public methods
-------------------------------------------------------------------------------}

Function TGdiPlusWrapper.NativeObject: Pointer;
begin
If Assigned(Self) then
  Result := GetNativeObject
else
  Result := nil;
end;

//!!----------------------------------------------------------------------------

Function TGdiPlusWrapper.NativeObjectAddr: Pointer;
begin
If Assigned(Self) then
  Result := GetNativeObjectAddr
else
  raise EGDIPlusObjectNotAssigned.CreateFmt('TGdiPlusWrapper.NativeObjectAddr: Instance of class %s not assigned.',[ClassName]);
end;


{!!*****************************************************************************
    gdiplusenums.h
*******************************************************************************}

Function ObjectTypeIsValid(type_: TObjectType): BOOL;
begin
Result := (type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax)
end;

//!!----------------------------------------------------------------------------

Function GDIP_WMF_RECORD_TO_EMFPLUS(Value: TEmfPlusRecordType): TEmfPlusRecordType;
begin
Result := TEmfPlusRecordType(Ord(Value) or GDIP_WMF_RECORD_BASE);
end;

//!!----------------------------------------------------------------------------

Function GDIP_EMFPLUS_RECORD_TO_WMF(Value: TEmfPlusRecordType): TEmfPlusRecordType;
begin
Result := TEmfPlusRecordType(Ord(Value) and not GDIP_WMF_RECORD_BASE);
end;

//!!----------------------------------------------------------------------------

Function GDIP_IS_WMF_RECORDTYPE(Value: TEmfPlusRecordType): Boolean;
begin
Result := (Ord(Value) and GDIP_WMF_RECORD_BASE) <> 0;
end;


{!!*****************************************************************************
    gdiplustypes.h
*******************************************************************************}

Function GDIPLUS_MIN(a,b: Integer): Integer;
begin
If a < b then
  Result := a
else
  Result := b;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GDIPLUS_MIN(a,b: Single): Single;
begin
If a < b then
  Result := a
else
  Result := b;
end;

//!!----------------------------------------------------------------------------

Function GDIPLUS_MAX(a,b: Integer): Integer;
begin
If a > b then
  Result := a
else
  Result := b;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GDIPLUS_MAX(a,b: Single): Single;
begin
If a > b then
  Result := a
else
  Result := b;
end;

//!!----------------------------------------------------------------------------

procedure GdiplusAbortSetup(Struct: PGdiplusAbort; Callback: TGdiplusAbortCallback);
begin
Struct^.AbortCallback := Callback;
Struct^.VMTPtr := @@(Struct^).AbortCallback;
end;


{!!-----------------------------------------------------------------------------
    TSizeF - implementation
-------------------------------------------------------------------------------}

Function SizeF: TSizeF;
begin
Result.Width := 0.0;
Result.Height := 0.0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SizeF(const sz: TSizeF): TSizeF;
begin
Result.Width := sz.Width;
Result.Height := sz.Height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SizeF(width,height: REAL): TSizeF;
begin
Result.Width := width;
Result.Height := height;
end;

//!!----------------------------------------------------------------------------

Function Add(const sza,szb: TSizeF): TSizeF;
begin
Result.Width := sza.Width + szb.Width;
Result.Height := sza.Height + szb.Height;
end;

//!!----------------------------------------------------------------------------

Function Subtract(const sza,szb: TSizeF): TSizeF;
begin
Result.Width := sza.Width - szb.Width;
Result.Height := sza.Height - szb.Height;
end;

//!!----------------------------------------------------------------------------

Function Equals(const sza,szb: TSizeF): BOOL;
begin
Result := (sza.Width = szb.Width) and (sza.Height = szb.Height);
end;

//!!----------------------------------------------------------------------------

Function Empty(const sz: TSizeF): BOOL;
begin
Result := (sz.Width = 0.0) and (sz.Height = 0.0);
end;


{!!-----------------------------------------------------------------------------
    TSize - implementation
-------------------------------------------------------------------------------}

Function SizeI: TSize;
begin
Result.Width := 0;
Result.Height := 0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SizeI(const sz: TSize): TSize;
begin
Result.Width := sz.Width;
Result.Height := sz.Height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SizeI(width,height: INT): TSize;
begin
Result.Width := width;
Result.Height := height;
end;

//!!----------------------------------------------------------------------------

Function Add(const sza,szb: TSize): TSize;
begin
Result.Width := sza.Width + szb.Width;
Result.Height := sza.Height + szb.Height;
end;

//!!----------------------------------------------------------------------------

Function Subtract(const sza,szb: TSize): TSize;
begin
Result.Width := sza.Width - szb.Width;
Result.Height := sza.Height - szb.Height;
end;

//!!----------------------------------------------------------------------------

Function Equals(const sza,szb: TSize): BOOL;
begin
Result := (sza.Width = szb.Width) and (sza.Height = szb.Height);
end;

//!!----------------------------------------------------------------------------

Function Empty(const sz: TSize): BOOL;
begin
Result := (sz.Width = 0) and (sz.Height = 0);
end;


{!!-----------------------------------------------------------------------------
    TPointF - implementation
-------------------------------------------------------------------------------}

Function PointF: TPointF;
begin
Result.X := 0.0;
Result.Y := 0.0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointF(const pt: TPointF): TPointF;
begin
Result.X := pt.X;
Result.Y := pt.Y;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointF(const sz: TSizeF): TPointF;
begin
Result.X := sz.Width;
Result.Y := sz.Height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointF(x,y: REAL): TPointF;
begin
Result.X := x;
Result.Y := y;
end;

//!!----------------------------------------------------------------------------

Function Add(const pta,ptb: TPointF): TPointF;
begin
Result.X := pta.X + ptb.X;
Result.Y := pta.Y + ptb.Y;
end;

//!!----------------------------------------------------------------------------

Function Subtract(const pta,ptb: TPointF): TPointF;
begin
Result.X := pta.X - ptb.X;
Result.Y := pta.Y - ptb.Y;
end;

//!!----------------------------------------------------------------------------

Function Equals(const pta,ptb: TPointF): BOOL;
begin
Result := (pta.X = ptb.X) and (pta.Y = ptb.Y);
end;


{!!-----------------------------------------------------------------------------
    TPoint - implementation
-------------------------------------------------------------------------------}

Function PointI: TPoint;
begin
Result.X := 0;
Result.Y := 0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointI(const pt: TPoint): TPoint;
begin
Result.X := pt.X;
Result.Y := pt.Y;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointI(const sz: TSize): TPoint;
begin
Result.X := sz.Width;
Result.Y := sz.Height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PointI(x,y: INT): TPoint;
begin
Result.X := x;
Result.Y := y;
end;

//!!----------------------------------------------------------------------------

Function Add(const pta,ptb: TPoint): TPoint;
begin
Result.X := pta.X + ptb.X;
Result.Y := pta.Y + ptb.Y;
end;

//!!----------------------------------------------------------------------------

Function Subtract(const pta,ptb: TPoint): TPoint;
begin
Result.X := pta.X - ptb.X;
Result.Y := pta.Y - ptb.Y;
end;

//!!----------------------------------------------------------------------------

Function Equals(const pta,ptb: TPoint): BOOL;
begin
Result := (pta.X = ptb.X) and (pta.Y = ptb.Y);
end;


{!!-----------------------------------------------------------------------------
    TRectF - implementation
-------------------------------------------------------------------------------}

Function RectF: TRectF;
begin
Result.X := 0.0;
Result.Y := 0.0;
Result.Width := 0.0;
Result.Height := 0.0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RectF(x,y,width,height: REAL): TRectF;
begin
Result.X := x;
Result.Y := y;
Result.Width := width;
Result.Height := height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RectF(const location: TPointF; const sz: TSizeF): TRectF;
begin
Result.X := location.X;
Result.Y := location.Y;
Result.Width := sz.Width;
Result.Height := sz.Height;
end;

//!!----------------------------------------------------------------------------

Function Clone(const rc: TRectF): TRectF;
begin
Result := RectF(rc.X,rc.Y,rc.Width,rc.Height);
end;

//!!----------------------------------------------------------------------------

Function GetLocation(const rc: TRectF): TPointF;
begin
Result.X := rc.X;
Result.Y := rc.Y;
end;

//!!----------------------------------------------------------------------------

Function GetSize(const rc: TRectF): TSizeF;
begin
Result.Width := rc.Width;
Result.Height := rc.Height;
end;

//!!----------------------------------------------------------------------------

Function GetBounds(const rc: TRectF): TRectF;
begin
Result.X := rc.X;
Result.Y := rc.Y;
Result.Width := rc.Width;
Result.Height := rc.Height;
end;

//!!----------------------------------------------------------------------------

Function GetLeft(const rc: TRectF): REAL;
begin
Result := rc.X;
end;

//!!----------------------------------------------------------------------------

Function GetTop(const rc: TRectF): REAL;
begin
Result := rc.Y;
end;

//!!----------------------------------------------------------------------------

Function GetRight(const rc: TRectF): REAL;
begin
Result := rc.X + rc.Width;
end;

//!!----------------------------------------------------------------------------

Function GetBottom(const rc: TRectF): REAL;
begin
Result := rc.Y + rc.Height;
end;

//!!----------------------------------------------------------------------------

Function IsEmptyArea(const rc: TRectF): BOOL;
begin
Result := (rc.Width <= REAL_EPSILON) or (rc.Height <= REAL_EPSILON);
end;

//!!----------------------------------------------------------------------------

Function Equals(const rca,rcb: TRectF): BOOL;
begin
Result := (rca.X = rcb.X) and (rca.Y = rcb.Y) and (rca.Width = rcb.Width) and (rca.Height = rcb.Height);
end;

//!!----------------------------------------------------------------------------

Function Contains(const rc: TRectF; x,y: REAL): BOOL;
begin
Result := ((x >= rc.X) and (x < (rc.X + rc.Width))) and
          ((y >= rc.Y) and (y < (rc.Y + rc.Height)))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Contains(const rc: TRectF; const pt: TPointF): BOOL;
begin
Result := Contains(rc,pt.X,pt.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Contains(const rca,rcb: TRectF): BOOL;
begin
Result := ((rca.X <= rcb.X) and (GetRight(rcb) <= GetRight(rca))) and
          ((rca.Y <= rcb.Y) and (GetBottom(rcb) <= GetBottom(rca)));
end;

//!!----------------------------------------------------------------------------

Function Inflate(const rc: TRectF; dx,dy: REAL): TRectF;
begin
Result.X := rc.X - dx;
Result.Y := rc.Y - dy;
Result.Width := rc.Width + (2 * dx);
Result.Height := rc.Height + (2 * dy);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Inflate(const rc: TRectF; const pt: TPointF): TRectF;
begin
Result := Inflate(rc,pt.X,pt.Y);
end;

//!!----------------------------------------------------------------------------

Function Intersect(const rca,rcb: TRectF): TRectF;
begin
Intersect(Result,rca,rcb);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Intersect(out rcc: TRectF; const rca,rcb: TRectF): BOOL;
var
  right,bottom,left,top:  REAL;
begin
right := GDIPLUS_MIN(GetRight(rca),GetRight(rcb));
bottom := GDIPLUS_MIN(GetBottom(rca),GetBottom(rcb));
left := GDIPLUS_MAX(GetLeft(rca),GetLeft(rcb));
top := GDIPLUS_MAX(GetTop(rca),GetTop(rcb));
rcc.X := left;
rcc.Y := top;
rcc.Width := right - left;
rcc.Height := bottom - top;
Result := not IsEmptyArea(rcc);
end;

//!!----------------------------------------------------------------------------

Function IntersectsWith(const rca,rcb: TRectF): BOOL;
begin
Result := (GetLeft(rca) < GetRight(rcb)) and (GetTop(rca) < GetBottom(rcb)) and
          (GetRight(rca) > GetLeft(rcb)) and (GetBottom(rca) > GetTop(rcb));
end;

//!!----------------------------------------------------------------------------

Function Union(const rca,rcb: TRectF): TRectF;
begin
Union(Result,rca,rcb);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Union(out rcc: TRectF; const rca,rcb: TRectF): BOOL;
var
  right,bottom,left,top:  REAL;
begin
right := GDIPLUS_MAX(GetRight(rca),GetRight(rcb));
bottom := GDIPLUS_MAX(GetBottom(rca),GetBottom(rcb));
left := GDIPLUS_MIN(GetLeft(rca),GetLeft(rcb));
top := GDIPLUS_MIN(GetTop(rca),GetTop(rcb));
rcc.X := left;
rcc.Y := top;
rcc.Width := right - left;
rcc.Height := bottom - top;
Result := not IsEmptyArea(rcc);
end;

//!!----------------------------------------------------------------------------

Function Offset(const rc: TRectF; const pt: TPointF): TRectF;
begin
Result := Offset(rc,pt.X,pt.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Offset(const rc: TRectF; dx,dy: REAL): TRectF;
begin
Result.X := rc.X + dx;
Result.Y := rc.Y + dy;
end;


{!!-----------------------------------------------------------------------------
    TRect - implementation
-------------------------------------------------------------------------------}

Function RectI: TRect;
begin
Result.X := 0;
Result.Y := 0;
Result.Width := 0;
Result.Height := 0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RectI(x,y,width,height: INT): TRect;
begin
Result.X := x;
Result.Y := y;
Result.Width := width;
Result.Height := height;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RectI(const location: TPoint; const sz: TSize): TRect;
begin
Result.X := location.X;
Result.Y := location.Y;
Result.Width := sz.Width;
Result.Height := sz.Height;
end;

//!!----------------------------------------------------------------------------

Function Clone(const rc: TRect): TRect;
begin
Result := RectI(rc.X,rc.Y,rc.Width,rc.Height);
end;

//!!----------------------------------------------------------------------------

Function GetLocation(const rc: TRect): TPoint;
begin
Result.X := rc.X;
Result.Y := rc.Y;
end;

//!!----------------------------------------------------------------------------

Function GetSize(const rc: TRect): TSize;
begin
Result.Width := rc.Width;
Result.Height := rc.Height;
end;

//!!----------------------------------------------------------------------------

Function GetBounds(const rc: TRect): TRect;
begin
Result.X := rc.X;
Result.Y := rc.Y;
Result.Width := rc.Width;
Result.Height := rc.Height;
end;

//!!----------------------------------------------------------------------------

Function GetLeft(const rc: TRect): INT;
begin
Result := rc.X;
end;

//!!----------------------------------------------------------------------------

Function GetTop(const rc: TRect): INT;
begin
Result := rc.Y;
end;

//!!----------------------------------------------------------------------------

Function GetRight(const rc: TRect): INT;
begin
Result := rc.X + rc.Width;
end;

//!!----------------------------------------------------------------------------

Function GetBottom(const rc: TRect): INT;
begin
Result := rc.Y + rc.Height;
end;

//!!----------------------------------------------------------------------------

Function IsEmptyArea(const rc: TRect): BOOL;
begin
Result := (rc.Width <= 0) or (rc.Height <= 0);
end;

//!!----------------------------------------------------------------------------

Function Equals(const rca,rcb: TRect): BOOL;
begin
Result := (rca.X = rcb.X) and (rca.Y = rcb.Y) and (rca.Width = rcb.Width) and (rca.Height = rcb.Height);
end;

//!!----------------------------------------------------------------------------

Function Contains(const rc: TRect; x,y: INT): BOOL;
begin
Result := ((x >= rc.X) and (x < (rc.X + rc.Width))) and
          ((y >= rc.Y) and (y < (rc.Y + rc.Height)))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Contains(const rc: TRect; const pt: TPoint): BOOL;
begin
Result := Contains(rc,pt.X,pt.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Contains(const rca,rcb: TRect): BOOL;
begin
Result := ((rca.X <= rcb.X) and (GetRight(rcb) <= GetRight(rca))) and
          ((rca.Y <= rcb.Y) and (GetBottom(rcb) <= GetBottom(rca)));
end;

//!!----------------------------------------------------------------------------

Function Inflate(const rc: TRect; dx,dy: INT): TRect;
begin
Result.X := rc.X - dx;
Result.Y := rc.Y - dy;
Result.Width := rc.Width + (2 * dx);
Result.Height := rc.Height + (2 * dy);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Inflate(const rc: TRect; const pt: TPoint): TRect;
begin
Result := Inflate(rc,pt.X,pt.Y);
end;

//!!----------------------------------------------------------------------------

Function Intersect(const rca,rcb: TRect): TRect;
begin
Intersect(Result,rca,rcb);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Intersect(out rcc: TRect; const rca,rcb: TRect): BOOL;
var
  right,bottom,left,top:  INT;
begin
right := GDIPLUS_MIN(GetRight(rca),GetRight(rcb));
bottom := GDIPLUS_MIN(GetBottom(rca),GetBottom(rcb));
left := GDIPLUS_MAX(GetLeft(rca),GetLeft(rcb));
top := GDIPLUS_MAX(GetTop(rca),GetTop(rcb));
rcc.X := left;
rcc.Y := top;
rcc.Width := right - left;
rcc.Height := bottom - top;
Result := not IsEmptyArea(rcc);
end;

//!!----------------------------------------------------------------------------

Function IntersectsWith(const rca,rcb: TRect): BOOL;
begin
Result := (GetLeft(rca) < GetRight(rcb)) and (GetTop(rca) < GetBottom(rcb)) and
          (GetRight(rca) > GetLeft(rcb)) and (GetBottom(rca) > GetTop(rcb));
end;

//!!----------------------------------------------------------------------------

Function Union(const rca,rcb: TRect): TRect;
begin
Union(Result,rca,rcb);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Union(out rcc: TRect; const rca,rcb: TRect): BOOL;
var
  right,bottom,left,top:  INT;
begin
right := GDIPLUS_MAX(GetRight(rca),GetRight(rcb));
bottom := GDIPLUS_MAX(GetBottom(rca),GetBottom(rcb));
left := GDIPLUS_MIN(GetLeft(rca),GetLeft(rcb));
top := GDIPLUS_MIN(GetTop(rca),GetTop(rcb));
rcc.X := left;
rcc.Y := top;
rcc.Width := right - left;
rcc.Height := bottom - top;
Result := not IsEmptyArea(rcc);
end;

//!!----------------------------------------------------------------------------

Function Offset(const rc: TRect; const pt: TPoint): TRect;
begin
Result := Offset(rc,pt.X,pt.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Offset(const rc: TRect; dx,dy: INT): TRect;
begin
Result.X := rc.X + dx;
Result.Y := rc.Y + dy;
end;


{!!-----------------------------------------------------------------------------
    TPathData - implementation
-------------------------------------------------------------------------------}

procedure PathDataInit(out PathData: TPathData);
begin
PathData.Count := 0;
PathData.Points := nil;
PathData.Types := nil;
end;

//!!----------------------------------------------------------------------------

procedure PathDataAlloc(var PathData: TPathData; Count: Integer);
begin
If Count > 0 then
  begin
    PathData.Count := count;
    PathData.Points := AllocMem(count * SizeOf(TPointF));
    PathData.Types := AllocMem(count);
  end
else PathDataFree(PathData);
end;

//!!----------------------------------------------------------------------------

procedure PathDataFree(var PathData: TPathData);
begin
If Assigned(PathData.Points) and (PathData.Count > 0) then
  FreeMem(PathData.Points,PathData.Count * SizeOf(TPointF));
IF Assigned(PathData.Types) and (PathData.Count > 0) then
  FreeMem(PathData.Types,PathData.Count);
PathData.Count := 0;
PathData.Points := nil;
PathData.Types := nil;
end;

//!!----------------------------------------------------------------------------

Function PathDataPointGet(const PathData: TPathData; Index: Integer): TPointF;
begin
If (Index >= 0) and (Index < PathData.Count) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PPointF(PtrUInt(PathData.Points) + PtrUInt(Index * SizeOf(TPointF)))^
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise EGDIPlusIndexOutOfBounds.CreateFmt('PathDataPointGet: Index (%d) out of bounds.',[Index]);
end;

//!!----------------------------------------------------------------------------

procedure PathDataPointSet(const PathData: TPathData; Index: Integer; Value: TPointF);
begin
If (Index >= 0) and (Index < PathData.Count) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  PPointF(PtrUInt(PathData.Points) + PtrUInt(Index * SizeOf(TPointF)))^ := Value
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise EGDIPlusIndexOutOfBounds.CreateFmt('PathDataPointSet: Index (%d) out of bounds.',[Index]);
end;

//!!----------------------------------------------------------------------------

Function PathDataTypeGet(const PathData: TPathData; Index: Integer): Byte;
begin
If (Index >= 0) and (Index < PathData.Count) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PByte(PtrUInt(PathData.Points) + PtrUInt(Index))^
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise EGDIPlusIndexOutOfBounds.CreateFmt('PathDataTypeGet: Index (%d) out of bounds.',[Index]);
end;

//!!----------------------------------------------------------------------------

procedure PathDataTypeSet(const PathData: TPathData; Index: Integer; Value: Byte);
begin
If (Index >= 0) and (Index < PathData.Count) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  PByte(PtrUInt(PathData.Points) + PtrUInt(Index))^ := Value
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise EGDIPlusIndexOutOfBounds.CreateFmt('PathDataTypeSet: Index (%d) out of bounds.',[Index]);
end;


{!!-----------------------------------------------------------------------------
    TCharacterRange - implementation
-------------------------------------------------------------------------------}

Function CharacterRange: TCharacterRange;
begin
Result.First := 0;
Result.Length := 0;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CharacterRange(first,length: INT): TCharacterRange;
begin
Result.First := first;
Result.Length := length;
end;

//!!----------------------------------------------------------------------------

procedure Assign(var range: TCharacterRange; const rhs: TCharacterRange);
begin
range.First := rhs.First;
range.Length := rhs.Length;
end;


{!!*****************************************************************************
    gdiplusinit.h
*******************************************************************************}

Function GdiplusStartupInput(
  debugEventCallback:       TDebugEventProc = nil;
  suppressBackgroundThread: BOOL = False;
  suppressExternalCodecs:   BOOL = False): TGdiplusStartupInput;
begin
Result.GdiplusVersion := 1;
Result.DebugEventCallback := debugEventCallback;
Result.SuppressBackgroundThread := suppressBackgroundThread;
Result.SuppressExternalCodecs := suppressExternalCodecs;
end;

{$IF GDIPVER >= $0110}     
//!!----------------------------------------------------------------------------
                      
Function GdiplusStartupInputEx(
  startupParameters:        INT = 0;
  debugEventCallback:       TDebugEventProc = nil;
  suppressBackgroundThread: BOOL = False;
  suppressExternalCodecs:   BOOL = False): TGdiplusStartupInputEx;
begin
Result.GdiplusVersion := 2;
Result.DebugEventCallback := debugEventCallback;
Result.SuppressBackgroundThread := suppressBackgroundThread;
Result.SuppressExternalCodecs := suppressExternalCodecs;
Result.StartupParameters := startupParameters;
end;
{$IFEND}

//!!----------------------------------------------------------------------------
{$IF (GDIPVER >= $0110) and not Defined(NewGDIPStatic)}
var
  GDIPLusLibraryContext: TDLULibraryContext;
{$IFEND}

Function GdiplusStartup(token: PULONG_PTR; input: PGdiplusStartupInput; output: PGdiplusStartupOutput): TStatus;
begin
{$IF (GDIPVER >= $0110) and not Defined(NewGDIPStatic)}  
ContextLock(GDIPLusLibraryContext);         
try                                   
  If OpenLibrary(GDIPLIB,GDIPLusLibraryContext,[optExceptionOnFailure]) then
    begin
      If input^.GdiplusVersion >= 2 then
        ResolveSymbols(GDIPLusLibraryContext,[
          Symbol('GdipFindFirstImageItem',      @@GdipFindFirstImageItem),
          Symbol('GdipFindNextImageItem',       @@GdipFindNextImageItem),
          Symbol('GdipGetImageItemData',        @@GdipGetImageItemData),
          Symbol('GdipImageSetAbort',           @@GdipImageSetAbort),
          Symbol('GdipGraphicsSetAbort',        @@GdipGraphicsSetAbort),
          Symbol('GdipBitmapConvertFormat',     @@GdipBitmapConvertFormat),
          Symbol('GdipInitializePalette',       @@GdipInitializePalette),
          Symbol('GdipBitmapApplyEffect',       @@GdipBitmapApplyEffect),
          Symbol('GdipBitmapCreateApplyEffect', @@GdipBitmapCreateApplyEffect),
          Symbol('GdipBitmapGetHistogram',      @@GdipBitmapGetHistogram),
          Symbol('GdipBitmapGetHistogramSize',  @@GdipBitmapGetHistogramSize),
          Symbol('GdipDrawImageFX',             @@GdipDrawImageFX),
          Symbol('GdipConvertToEmfPlus',        @@GdipConvertToEmfPlus),
          Symbol('GdipConvertToEmfPlusToFile',  @@GdipConvertToEmfPlusToFile),
          Symbol('GdipConvertToEmfPlusToStream',@@GdipConvertToEmfPlusToStream)
        ],[optExceptionOnFailure]);
    end;
finally
  ContextUnlock(GDIPLusLibraryContext);
end;
{$IFEND}
Result := LibGdiplusStartup(token,Input,output);
{$IF (GDIPVER >= $0110) and not Defined(NewGDIPStatic)}
If Result <> Ok then
  CloseLibrary(GDIPLusLibraryContext);
{$IFEND}
end;

//!!----------------------------------------------------------------------------

procedure GdiplusShutdown(token: ULONG_PTR);
begin
LibGdiplusShutdown(token);
{$IF (GDIPVER >= $0110) and not Defined(NewGDIPStatic)}
CloseLibrary(GDIPLusLibraryContext);
{$IFEND}
end;


{!!*****************************************************************************
    gdipluspixelformats.h
*******************************************************************************}

Function GetPixelFormatSize(pixfmt: TPixelFormat): UINT;
begin
Result := (pixfmt shr 8) and $FF;
end;

//!!----------------------------------------------------------------------------

Function IsIndexedPixelFormat(pixfmt: TPixelFormat): BOOL;
begin
Result := (pixfmt and PixelFormatIndexed) <> 0;
end;

//!!----------------------------------------------------------------------------

Function IsAlphaPixelFormat(pixfmt: TPixelFormat): BOOL;
begin
Result := (pixfmt and PixelFormatAlpha) <> 0;
end;

//!!----------------------------------------------------------------------------

Function IsExtendedPixelFormat(pixfmt: TPixelFormat): BOOL;
begin
Result := (pixfmt and PixelFormatExtended) <> 0;
end;

//!!----------------------------------------------------------------------------

Function IsCanonicalPixelFormat(pixfmt: TPixelFormat): BOOL;
begin
Result := (pixfmt and PixelFormatCanonical) <> 0;
end;


{!!*****************************************************************************
    gdipluscolor.h
*******************************************************************************}
{!!-----------------------------------------------------------------------------
    TColor - implementation
-------------------------------------------------------------------------------}

Function MakeARGB(a,r,g,b: Byte): TARGB;
begin
Result := (TARGB(b) shl BlueShift) or
          (TARGB(g) shl GreenShift) or
          (TARGB(r) shl RedShift) or
          (TARGB(a) shl AlphaShift);
end;

//!!----------------------------------------------------------------------------

Function Color: TColor;
begin
Result.Argb := TARGB(Black);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Color(r,g,b: Byte): TColor;
begin
Result.Argb := MakeARGB(255,r,g,b);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Color(a,r,g,b: Byte): TColor;
begin
Result.Argb := MakeARGB(a,r,g,b);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Color(argb: TARGB): TColor;
begin
Result.Argb := argb;
end;

//!!----------------------------------------------------------------------------

Function GetAlpha(const cl: TColor): Byte;
begin
Result := Byte(cl.Argb shr AlphaShift);
end;

//!!----------------------------------------------------------------------------

Function GetA(const cl: TColor): Byte;
begin
Result := GetAlpha(cl);
end;

//!!----------------------------------------------------------------------------

Function GetRed(const cl: TColor): Byte;
begin
Result := Byte(cl.Argb shr RedShift);
end;

//!!----------------------------------------------------------------------------

Function GetR(const cl: TColor): Byte;
begin
Result := GetRed(cl);
end;

//!!----------------------------------------------------------------------------

Function GetGreen(const cl: TColor): Byte;
begin
Result := Byte(cl.Argb shr GreenShift);
end;

//!!----------------------------------------------------------------------------

Function GetG(const cl: TColor): Byte;
begin
Result := GetGreen(cl);
end;

//!!---------------------------------------------------------------------------

Function GetBlue(const cl: TColor): Byte;
begin
Result := Byte(cl.Argb shr BlueShift);
end;

//!!----------------------------------------------------------------------------

Function GetB(const cl: TColor): Byte;
begin
Result := GetBlue(cl);
end;

//!!----------------------------------------------------------------------------

Function GetValue(const cl: TColor): TARGB;
begin
Result := cl.Argb;
end;

//!!----------------------------------------------------------------------------

procedure SetValue(var cl: TColor; argb: TARGB);
begin
cl.Argb := argb;
end;

//!!----------------------------------------------------------------------------

procedure SetFromCOLORREF(var cl: TColor; rgb: TCOLORREF);
begin
cl.Argb := MakeARGB(255,GetRValue(rgb),GetGValue(rgb),GetBValue(rgb));
end;

//!!----------------------------------------------------------------------------

Function ToCOLORREF(const cl: TColor): TCOLORREF;
begin
Result := RGB(GetRed(cl),GetGreen(cl),GetBlue(cl));
end;


{!!*****************************************************************************
    gdiplusmetaheader.h
*******************************************************************************}
{!!-----------------------------------------------------------------------------
    TMetafileHeader - implementation
-------------------------------------------------------------------------------}

Function GetType(const header: TMetafileHeader): TMetafileType;
begin
Result := header.Type_;
end;  

//!!----------------------------------------------------------------------------

Function GetMetafileSize(const header: TMetafileHeader): UINT;
begin
Result := header.Size;
end;

//!!----------------------------------------------------------------------------

Function GetVersion(const header: TMetafileHeader): UINT;
begin
Result := header.Version;
end;

//!!----------------------------------------------------------------------------

Function GetEmfPlusFlags(const header: TMetafileHeader): UINT;
begin
Result := header.EmfPlusFlags;
end;

//!!----------------------------------------------------------------------------

Function GetDpiX(const header: TMetafileHeader): REAL;
begin
Result := header.DpiX;
end;

//!!----------------------------------------------------------------------------

Function GetDpiY(const header: TMetafileHeader): REAL;
begin
Result := header.DpiY;
end;

//!!----------------------------------------------------------------------------

Function GetBounds(const header: TMetafileHeader): TRect;
begin
Result.X := header.X;
Result.Y := header.Y;
Result.Width := header.Width;
Result.Height := header.Height;
end;

//!!---------------------------------------------------------------------------

Function IsWmf(const header: TMetafileHeader): BOOL;
begin
Result := (header.Type_ = MetafileTypeWmf) or (header.Type_ = MetafileTypeWmfPlaceable);
end;

//!!----------------------------------------------------------------------------

Function IsWmfPlaceable(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ = MetafileTypeWmfPlaceable;
end;

//!!----------------------------------------------------------------------------

Function IsEmf(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ = MetafileTypeEmf;
end;
 
//!!----------------------------------------------------------------------------

Function IsEmfOrEmfPlus(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ >= MetafileTypeEmf;
end;
 
//!!----------------------------------------------------------------------------

Function IsEmfPlus(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ >= MetafileTypeEmfPlusOnly;
end;
 
//!!----------------------------------------------------------------------------

Function IsEmfPlusDual(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ = MetafileTypeEmfPlusDual;
end;

//!!----------------------------------------------------------------------------

Function IsEmfPlusOnly(const header: TMetafileHeader): BOOL;
begin
Result := header.Type_ = MetafileTypeEmfPlusOnly;
end;
 
//!!----------------------------------------------------------------------------

Function IsDisplay(const header: TMetafileHeader): BOOL;
begin
Result := IsEmfPlus(header) and ((header.EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0);
end;

//!!----------------------------------------------------------------------------

Function GetWmfHeader(const header: TMetafileHeader): PMETAHEADER;
begin
If IsWmf(header) then
  Result := Addr(header.WmfHeader)
else
  Result := nil;
end;

//!!----------------------------------------------------------------------------

Function GetEmfHeader(const header: TMetafileHeader): PENHMETAHEADER3;
begin
If IsEmfOrEmfPlus(header) then
  Result := Addr(header.EmfHeader)
else
  Result := nil;
end;


{!!*****************************************************************************
    gdipluseffects.h
*******************************************************************************}
{$IF GDIPVER >= $0110}
{!!=============================================================================
    TEffect - class implmentation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TEffect - protected methods
-------------------------------------------------------------------------------}

Function TEffect.GetNativeObject: Pointer;
begin
Result := fNativeEffect;
end;

//!!----------------------------------------------------------------------------

Function TEffect.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TEffect.SetParameters(Params: Pointer; Size: UINT): TStatus;
begin
Result := GdipSetEffectParameters(fNativeEffect,Params,Size);
end;

//!!----------------------------------------------------------------------------

Function TEffect.GetParameters(Size: PUINT; Params: Pointer): TStatus;
begin
Result := GdipGetEffectParameters(fNativeEffect,Size,Params);
end;

{!!-----------------------------------------------------------------------------
    TEffect - public methods
-------------------------------------------------------------------------------}

constructor TEffect.Create;
begin
inherited Create;
fAuxDataSize := 0;
fAuxData := nil;
fNativeEffect := nil;
fUseAuxData := False
end;

//!!----------------------------------------------------------------------------

destructor TEffect.Destroy;
begin
// pvData is allocated by ApplyEffect. Return the pointer so that
// it can be freed by the appropriate memory manager.
GdipFree(fAuxData);
// Release the native Effect.
GdipDeleteEffect(fNativeEffect);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TEffect.GetAuxDataSize: INT;
begin
Result := fAuxDataSize;
end;

//!!----------------------------------------------------------------------------

Function TEffect.GetAuxData: Pointer;
begin
Result := fAuxData;
end;

//!!----------------------------------------------------------------------------

procedure TEffect.UseAuxData(UseAuxDataFlag: BOOL);
begin
fUseAuxData := UseAuxDataFlag;
end;

//!!----------------------------------------------------------------------------

Function TEffect.GetParameterSize(Size: PUINT): TStatus;
begin
Result := GdipGetEffectParameterSize(fNativeEffect,Size);
end;


{!!=============================================================================
    TBlur - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TBlur - public methods
-------------------------------------------------------------------------------}

constructor TBlur.Create;
begin
inherited Create;
// constructors cannot return an error code.
GdipCreateEffect(BlurEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TBlur.SetParameters(Parameters: PBlurParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TBlurParams)));
end;

//!!----------------------------------------------------------------------------

Function TBlur.GetParameters(Size: PUINT; Parameters: PBlurParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TSharpen - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TSharpen - public methods
-------------------------------------------------------------------------------}

constructor TSharpen.Create;
begin
inherited Create;
GdipCreateEffect(SharpenEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TSharpen.SetParameters(Parameters: PSharpenParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TSharpenParams)));
end;

//!!----------------------------------------------------------------------------

Function TSharpen.GetParameters(Size: PUINT; Parameters: PSharpenParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TRedEyeCorrection - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TRedEyeCorrection - public methods
-------------------------------------------------------------------------------}

constructor TRedEyeCorrection.Create;
begin
inherited Create;
// constructors cannot return an error code.
GdipCreateEffect(RedEyeCorrectionEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TRedEyeCorrection.SetParameters(Parameters: PRedEyeCorrectionParams): TStatus;
begin
Result := InvalidParameter;
If Assigned(Parameters) then
  Result := inherited SetParameters(Parameters,UINT(SizeOf(TRedEyeCorrectionParams) + (Parameters^.numberOfAreas * SizeOf(Windows.TRECT))));
end;

//!!----------------------------------------------------------------------------

Function TRedEyeCorrection.GetParameters(Size: PUINT; Parameters: PRedEyeCorrectionParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TBrightnessContrast - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TBrightnessContrast - public methods
-------------------------------------------------------------------------------}

constructor TBrightnessContrast.Create;
begin
inherited Create;
GdipCreateEffect(BrightnessContrastEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TBrightnessContrast.SetParameters(Parameters: PBrightnessContrastParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TBrightnessContrastParams)));
end;

//!!----------------------------------------------------------------------------

Function TBrightnessContrast.GetParameters(Size: PUINT; Parameters: PBrightnessContrastParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    THueSaturationLightness - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    THueSaturationLightness - public methods
-------------------------------------------------------------------------------}

constructor THueSaturationLightness.Create;
begin
inherited Create;
GdipCreateEffect(HueSaturationLightnessEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function THueSaturationLightness.SetParameters(Parameters: PHueSaturationLightnessParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(THueSaturationLightnessParams)));
end;

//!!----------------------------------------------------------------------------

Function THueSaturationLightness.GetParameters(Size: PUINT; Parameters: PHueSaturationLightnessParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TLevels - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TLevels - public methods
-------------------------------------------------------------------------------}

constructor TLevels.Create;
begin
inherited Create;
GdipCreateEffect(LevelsEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TLevels.SetParameters(Parameters: PLevelsParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TLevelsParams)));
end;

//!!----------------------------------------------------------------------------

Function TLevels.GetParameters(Size: PUINT; Parameters: PLevelsParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TTint - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TTint - public methods
-------------------------------------------------------------------------------}

constructor TTint.Create;
begin
inherited Create;
GdipCreateEffect(TintEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TTint.SetParameters(Parameters: PTintParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TTintParams)));
end;

//!!----------------------------------------------------------------------------

Function TTint.GetParameters(Size: PUINT; Parameters: PTintParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TColorBalance - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TColorBalance - public methods
-------------------------------------------------------------------------------}

constructor TColorBalance.Create;
begin
inherited Create;
GdipCreateEffect(ColorBalanceEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TColorBalance.SetParameters(Parameters: PColorBalanceParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TColorBalanceParams)));
end;

//!!----------------------------------------------------------------------------

Function TColorBalance.GetParameters(Size: PUINT; Parameters: PColorBalanceParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;


{!!=============================================================================
    TColorMatrixEffect - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TColorMatrixEffect - public methods
-------------------------------------------------------------------------------}

constructor TColorMatrixEffect.Create;
begin
inherited Create;
GdipCreateEffect(ColorMatrixEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TColorMatrixEffect.SetParameters(Matrix: PColorMatrix): TStatus;
begin
Result := inherited SetParameters(matrix,UINT(SizeOf(TColorMatrix)));
end;

//!!----------------------------------------------------------------------------

Function TColorMatrixEffect.GetParameters(Size: PUINT; Matrix: PColorMatrix): TStatus;
begin
Result := inherited GetParameters(Size,matrix);
end;


{!!=============================================================================
    TColorLUT - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TColorLUT - public methods
-------------------------------------------------------------------------------}

constructor TColorLUT.Create;
begin
inherited Create;
GdipCreateEffect(ColorLUTEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TColorLUT.SetParameters(LUT: PColorLUTParams): TStatus;
begin
Result := inherited SetParameters(LUT,UINT(SizeOf(TColorLUTParams)));
end;

//!!----------------------------------------------------------------------------

Function TColorLUT.GetParameters(Size: PUINT; LUT: PColorLUTParams): TStatus;
begin
Result := inherited GetParameters(Size,LUT);
end;


{!!=============================================================================
    TColorCurve - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TColorCurve - public methods
-------------------------------------------------------------------------------}

constructor TColorCurve.Create;
begin
inherited Create;
GdipCreateEffect(ColorCurveEffectGuid,@fNativeEffect);
end;

//!!----------------------------------------------------------------------------

Function TColorCurve.SetParameters(Parameters: PColorCurveParams): TStatus;
begin
Result := inherited SetParameters(Parameters,UINT(SizeOf(TColorCurveParams)));
end;

//!!----------------------------------------------------------------------------

Function TColorCurve.GetParameters(Size: PUINT; Parameters: PColorCurveParams): TStatus;
begin
Result := inherited GetParameters(Size,Parameters);
end;

{$IFEND}


{!!*****************************************************************************
    gdiplusimageattributes.h
*******************************************************************************}
{!!=============================================================================
    TImageAttributes - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TImageAttributes - protected methods
-------------------------------------------------------------------------------}

Function TImageAttributes.GetNativeObject: Pointer;
begin
Result := fNativeImageAttr;
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeImageAttr);
end;

//!!----------------------------------------------------------------------------

constructor TImageAttributes.Create(ImageAttr: PGpImageAttributes; Status: TStatus);
begin
inherited Create;
SetNativeImageAttr(ImageAttr);
fLastResult := Status;
end;

//!!----------------------------------------------------------------------------

procedure TImageAttributes.SetNativeImageAttr(NativeImageAttrArg: PGpImageAttributes);
begin
fNativeImageAttr := NativeImageAttrArg;
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TImageAttributes - public methods
-------------------------------------------------------------------------------}

constructor TImageAttributes.Create;
begin
inherited Create;
fNativeImageAttr := nil;
fLastResult := GdipCreateImageAttributes(@fNativeImageAttr);
end;

//!!----------------------------------------------------------------------------

destructor TImageAttributes.Destroy;
begin
GdipDisposeImageAttributes(fNativeImageAttr);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.Clone: TImageAttributes;
var
  CloneAttr:  PGpImageAttributes;
begin
SetStatus(GdipCloneImageAttributes(fNativeImageAttr,@CloneAttr));
Result := TImageAttributes.Create(CloneAttr,fLastResult);
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetToIdentity(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesToIdentity(fNativeImageAttr,TypeAdjusted));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.Reset(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipResetImageAttributes(fNativeImageAttr,TypeAdjusted));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetColorMatrix(ColorMatrix: PColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorMatrix(fNativeImageAttr,TypeAdjusted,True,ColorMatrix,nil,Mode));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearColorMatrix(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorMatrix(fNativeImageAttr,TypeAdjusted,False,nil,nil,ColorMatrixFlagsDefault));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetColorMatrices(ColorMatrix,GrayMatrix: PColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorMatrix(fNativeImageAttr,TypeAdjusted,True,ColorMatrix,GrayMatrix,Mode));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearColorMatrices(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorMatrix(fNativeImageAttr,TypeAdjusted,False,nil,nil,ColorMatrixFlagsDefault));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetThreshold(Threshold: REAL; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesThreshold(fNativeImageAttr,TypeAdjusted,True,Threshold));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearThreshold(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesThreshold(fNativeImageAttr,TypeAdjusted,False,0.0));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetGamma(Gamma: REAL; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesGamma(fNativeImageAttr,TypeAdjusted,True,Gamma));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearGamma(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesGamma(fNativeImageAttr,TypeAdjusted,False,0.0));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetNoOp(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesNoOp(fNativeImageAttr,TypeAdjusted,True));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearNoOp(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesNoOp(fNativeImageAttr,TypeAdjusted,False));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetColorKey(const ColorLow,ColorHigh: TColor; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorKeys(fNativeImageAttr,TypeAdjusted,True,GetValue(ColorLow),GetValue(ColorHigh)));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearColorKey(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesColorKeys(fNativeImageAttr,TypeAdjusted,False,0,0));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetOutputChannel(ChannelFlags: TColorChannelFlags; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesOutputChannel(fNativeImageAttr,TypeAdjusted,True,ChannelFlags));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearOutputChannel(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesOutputChannel(fNativeImageAttr,TypeAdjusted,False,ColorChannelFlagsLast));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetOutputChannelColorProfile(ColorProfileFilename: PWideChar;
  TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(fNativeImageAttr,TypeAdjusted,True,ColorProfileFilename));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImageAttributes.SetOutputChannelColorProfile(const ColorProfileFilename: String;
  TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetOutputChannelColorProfile(PWideChar(StrToWide(ColorProfileFilename)),TypeAdjusted)
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearOutputChannelColorProfile(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(fNativeImageAttr,TypeAdjusted,False,nil));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetRemapTable(MapSize: UINT; Map: PColorMap; TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesRemapTable(fNativeImageAttr,TypeAdjusted,True,MapSize,Map));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearRemapTable(TypeAdjusted: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesRemapTable(fNativeImageAttr,TypeAdjusted,False,0,nil));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetBrushRemapTable(MapSize: UINT; Map: PColorMap): TStatus;
begin
Result := SetRemapTable(MapSize,Map,ColorAdjustTypeBrush);
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.ClearBrushRemapTable: TStatus;
begin
Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.SetWrapMode(Wrap: TWrapMode; const Color: TColor; Clamp: BOOL = False): TStatus;
begin
Result := SetStatus(GdipSetImageAttributesWrapMode(fNativeImageAttr,Wrap,GetValue(Color),Clamp));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImageAttributes.SetWrapMode(Wrap: TWrapMode): TStatus;
begin
Result := SetWrapMode(Wrap,Color(),False);
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TStatus;
begin
Result := SetStatus(GdipGetImageAttributesAdjustedPalette(fNativeImageAttr,ColorPalette,ColorAdjustType));
end;

//!!----------------------------------------------------------------------------

Function TImageAttributes.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusmatrix.h
*******************************************************************************}
{!!=============================================================================
    TMatrix - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TMatrix - protected methods
-------------------------------------------------------------------------------}

Function TMatrix.GetNativeObject: Pointer;
begin
Result := fNativeMatrix;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeMatrix);
end;

//!!----------------------------------------------------------------------------

constructor TMatrix.Create(NativeMatrixArg: PGpMatrix);
begin
inherited Create;
fLastResult := Ok;
SetNativeMatrix(NativeMatrixArg);
end;

//!!----------------------------------------------------------------------------

procedure TMatrix.SetNativeMatrix(NativeMatrixArg: PGpMatrix);
begin
fNativeMatrix := NativeMatrixArg;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TMatrix - public methods
-------------------------------------------------------------------------------}

constructor TMatrix.Create;
var
  Matrix: PGpMatrix;
begin
inherited Create;
Matrix := nil;
fLastResult := GdipCreateMatrix(@Matrix);
SetNativeMatrix(Matrix);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMatrix.Create(M11,M12,M21,M22,DX,DY: REAL);
var
  Matrix: PGpMatrix;
begin
inherited Create;
Matrix := nil;
fLastResult := GdipCreateMatrix2(M11,M12,M21,M22,DX,DY,@Matrix);
SetNativeMatrix(Matrix);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMatrix.Create(const Rect: TRectF; const DstPlg: TPointF);
var
  Matrix: PGpMatrix;
begin
inherited Create;
Matrix := nil;
fLastResult := GdipCreateMatrix3(@Rect,@DstPlg,@Matrix);
SetNativeMatrix(Matrix);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMatrix.Create(const Rect: TRect; const DstPlg: TPoint);
var
  Matrix: PGpMatrix;
begin
inherited Create;
Matrix := nil;
fLastResult := GdipCreateMatrix3I(@Rect,@DstPlg,@Matrix);
SetNativeMatrix(Matrix);
end;

//!!----------------------------------------------------------------------------

destructor TMatrix.Destroy;
begin
GdipDeleteMatrix(fNativeMatrix);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Clone: TMatrix;
var
  CloneMatrix:  PGpMatrix;
begin
SetStatus(GdipCloneMatrix(fNativeMatrix,@CloneMatrix));
If fLastResult = Ok then
  Result := TMatrix.Create(CloneMatrix)
else
  Result := nil;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.GetElements(M: PREAL): TStatus;
begin
Result := SetStatus(GdipGetMatrixElements(fNativeMatrix,M));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.SetElements(M11,M12,M21,M22,DX,DY: REAL): TStatus;
begin
Result := SetStatus(GdipSetMatrixElements(fNativeMatrix,M11,M12,M21,M22,DX,DY));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.OffsetX: REAL;
var
  Elements: array[0..5] of REAL;
begin
If GetElements(Addr(Elements[0])) = Ok then
  Result := Elements[4]
else
  Result := 0.0;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.OffsetY: REAL;
var
  Elements: array[0..5] of REAL;
begin
If GetElements(Addr(Elements[0])) = Ok then
  Result := Elements[5]
else
  Result := 0.0;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Reset: TStatus;
begin
// set identity matrix elements
Result := SetStatus(GdipSetMatrixElements(fNativeMatrix,1.0,0.0,0.0,1.0,0.0,0.0));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Multiply(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyMatrix(fNativeMatrix,Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Translate(OffsetX,OffsetY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslateMatrix(fNativeMatrix,OffsetX,OffsetY,Order));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Scale(ScaleX,ScaleY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScaleMatrix(fNativeMatrix,ScaleX,ScaleY,Order));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Rotate(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotateMatrix(fNativeMatrix,Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.RotateAt(Angle: REAL; const Center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
If Order = MatrixOrderPrepend then
  begin
    SetStatus(GdipTranslateMatrix(fNativeMatrix,Center.X,Center.Y,Order));
    SetStatus(GdipRotateMatrix(fNativeMatrix,Angle,Order));
    Result := SetStatus(GdipTranslateMatrix(fNativeMatrix,-Center.X,-Center.Y,Order));
  end
else
  begin
    SetStatus(GdipTranslateMatrix(fNativeMatrix,-Center.X,-Center.Y,Order));
    SetStatus(GdipRotateMatrix(fNativeMatrix,Angle,Order));
    Result := SetStatus(GdipTranslateMatrix(fNativeMatrix,Center.X,Center.Y,Order));
  end;
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Shear(ShearX,ShearY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipShearMatrix(fNativeMatrix,ShearX,ShearY,Order));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.Invert: TStatus;
begin
Result := SetStatus(GdipInvertMatrix(fNativeMatrix));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.TransformPoints(Pts: PPointF; Count: INT = 1): TStatus;
begin
Result := SetStatus(GdipTransformMatrixPoints(fNativeMatrix,PGpPointF(Pts),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMatrix.TransformPoints(Pts: PPoint; Count: INT = 1): TStatus;
begin
Result := SetStatus(GdipTransformMatrixPointsI(fNativeMatrix,PGpPoint(Pts),Count));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.TransformVectors(Pts: PPointF; Count: INT = 1): TStatus;
begin
Result := SetStatus(GdipVectorTransformMatrixPoints(fNativeMatrix,PGpPointF(Pts),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMatrix.TransformVectors(Pts: PPoint; Count: INT = 1): TStatus;
begin
Result := SetStatus(GdipVectorTransformMatrixPointsI(fNativeMatrix,PGpPoint(Pts),Count));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.IsInvertible: BOOL;
begin
Result := False;
SetStatus(GdipIsMatrixInvertible(fNativeMatrix,@Result));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.IsIdentity: BOOL;
begin
Result := False;
SetStatus(GdipIsMatrixIdentity(fNativeMatrix,@Result));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.IsEqual(Matrix: TMatrix): BOOL;
begin
Result := False;
SetStatus(GdipIsMatrixEqual(fNativeMatrix,Matrix.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TMatrix.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusbrush.h
*******************************************************************************}
{!!=============================================================================
    TBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TBrush - protected methods
-------------------------------------------------------------------------------}

Function TBrush.GetNativeObject: Pointer;
begin
Result := fNativeBrush;
end;

//!!----------------------------------------------------------------------------

Function TBrush.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeBrush);
end;

//!!----------------------------------------------------------------------------

constructor TBrush.Create;
begin
inherited Create;
SetStatus(NotImplemented);
end;

//!!----------------------------------------------------------------------------

constructor TBrush.Create(NativeBrushArg: PGpBrush; Status: TStatus);
begin
inherited Create;
fLastResult := Status;
SetNativeBrush(NativeBrushArg);
end;

//!!----------------------------------------------------------------------------

procedure TBrush.SetNativeBrush(NativeBrushArg: PGpBrush);
begin
fNativeBrush := NativeBrushArg;
end;

//!!----------------------------------------------------------------------------

Function TBrush.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TBrush - public methods
-------------------------------------------------------------------------------}

destructor TBrush.Destroy;
begin
GdipDeleteBrush(fNativeBrush);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TBrush.Clone: TBrush;
var
  Brush:  PGpBrush;
begin
Brush := nil;
SetStatus(GdipCloneBrush(fNativeBrush,@Brush));
Result := TBrush.Create(Brush,fLastResult);
If not Assigned(Result) then
  GdipDeleteBrush(Brush);
end;

//!!----------------------------------------------------------------------------

Function TBrush.GetType: TBrushType;
begin
{$IFDEF FPCDWM}{$PUSH}W4110{$ENDIF}
Result := TBrushType(-1);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
SetStatus(GdipGetBrushType(fNativeBrush,@Result));
end;

//!!----------------------------------------------------------------------------

Function TBrush.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!=============================================================================
    TSolidBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TSolidBrush - public methods
-------------------------------------------------------------------------------}

constructor TSolidBrush.Create(const Color: TColor);
var
  Brush:  PGpSolidFill;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateSolidFill(GetValue(Color),@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!!----------------------------------------------------------------------------

Function TSolidBrush.GetColor(aColor: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(aColor) then
  begin
    SetStatus(GdipGetSolidFillColor(PGpSolidFill(fNativeBrush),@Argb));
    aColor^ := Color(Argb);
    Result := fLastResult;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TSolidBrush.SetColor(const Color: TColor): TStatus;
begin
Result := SetStatus(GdipSetSolidFillColor(PGpSolidFill(fNativeBrush),GetValue(Color)));
end;


{!!=============================================================================
    TTextureBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TTextureBrush - public methods
-------------------------------------------------------------------------------}

constructor TTextureBrush.Create(Image: TImage; WrapMode: TWrapMode = WrapModeTile);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTexture(Image.NativeObject,WrapMode,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; WrapMode: TWrapMode; const DstRect: TRectF);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTexture2(Image.NativeObject,WrapMode,DstRect.X,DstRect.Y,DstRect.Width,DstRect.Height,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; const DstRect: TRectF; ImageAttributes: TImageAttributes = nil);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTextureIA(Image.NativeObject,ImageAttributes.NativeObject,DstRect.X,DstRect.Y,DstRect.Width,DstRect.Height,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; const DstRect: TRect; ImageAttributes: TImageAttributes = nil);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTextureIAI(Image.NativeObject,ImageAttributes.NativeObject,DstRect.X,DstRect.Y,DstRect.Width,DstRect.Height,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; WrapMode: TWrapMode; const DstRect: TRect);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTexture2I(Image.NativeObject,WrapMode,DstRect.X,DstRect.Y,DstRect.Width,DstRect.Height,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; WrapMode: TWrapMode; DstX,DstY,DstWidth,DstHeight: REAL);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTexture2(Image.NativeObject,WrapMode,DstX,DstY,DstWidth,DstHeight,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTextureBrush.Create(Image: TImage; WrapMode: TWrapMode; DstX,DstY,DstWidth,DstHeight: INT);
var
  Texture: PGpTexture;
begin
inherited Create;
Texture := nil;
fLastResult := GdipCreateTexture2I(Image.NativeObject,WrapMode,DstX,DstY,DstWidth,DstHeight,@Texture);
SetNativeBrush(PGpBrush(Texture));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.SetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipSetTextureTransform(PGpTexture(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.GetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipGetTextureTransform(PGpTexture(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.ResetTransform: TStatus;
begin
Result := SetStatus(GdipResetTextureTransform(PGpTexture(fNativeBrush)));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyTextureTransform(PGpTexture(fNativeBrush),Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslateTextureTransform(PGpTexture(fNativeBrush),DX,DY,Order));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScaleTextureTransform(PGpTexture(fNativeBrush),SX,SY,Order));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotateTextureTransform(PGpTexture(fNativeBrush),Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
Result := SetStatus(GdipSetTextureWrapMode(PGpTexture(fNativeBrush),WrapMode));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.GetWrapMode: TWrapMode;
begin
SetStatus(GdipGetTextureWrapMode(PGpTexture(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TTextureBrush.GetImage: TImage;
var
  Image:  PGpImage;
begin
SetStatus(GdipGetTextureImage(PGpTexture(fNativeBrush),@Image));
Result := TImage.Create(Image,fLastResult);
If not Assigned(Result) then
  GdipDisposeImage(Image);
end;


{!!=============================================================================
    TLinearGradientBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TLinearGradientBrush - public methods
-------------------------------------------------------------------------------}

constructor TLinearGradientBrush.Create(const Point1,Point2: TPointF; const Color1,Color2: TColor);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrush(@Point1,@Point2,GetValue(Color1),GetValue(Color2),WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinearGradientBrush.Create(const Point1,Point2: TPoint; const Color1,Color2: TColor);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrushI(@Point1,@Point2,GetValue(Color1),GetValue(Color2),WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinearGradientBrush.Create(const Rect: TRectF; const Color1,Color2: TColor; Mode: TLinearGradientMode);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrushFromRect(@Rect,GetValue(Color1),GetValue(Color2),Mode,WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinearGradientBrush.Create(const Rect: TRect; const Color1,Color2: TColor; Mode: TLinearGradientMode);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrushFromRectI(@Rect,GetValue(Color1),GetValue(Color2),Mode,WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinearGradientBrush.Create(const Rect: TRectF; const Color1,Color2: TColor; Angle: REAL; IsAngleScalable: BOOL = False);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrushFromRectWithAngle(@Rect,GetValue(Color1),GetValue(Color2),Angle,IsAngleScalable,WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinearGradientBrush.Create(const Rect: TRect; const Color1,Color2: TColor; Angle: REAL; IsAngleScalable: BOOL = False);
var
  Brush: PGpLineGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateLineBrushFromRectWithAngleI(@Rect,GetValue(Color1),GetValue(Color2),Angle,IsAngleScalable,WrapModeTile,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetLinearColors(const Color1,Color2: TColor): TStatus;
begin
Result := SetStatus(GdipSetLineColors(PGpLineGradient(fNativeBrush),GetValue(Color1),GetValue(Color2)));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetLinearColors(Colors: PColor): TStatus;
var
  Argbs:  array[0..1] of TARGB;
begin
If Assigned(Colors) then
  begin
    Result := SetStatus(GdipGetLineColors(PGpLineGradient(fNativeBrush),@Argbs));
    If Result = Ok then
      begin
        // use bitwise copy operator for Color copy
        PColorArray(Colors)^[0] := Color(Argbs[0]);
        PColorArray(Colors)^[1] := Color(Argbs[1]);
      end;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------
 
Function TLinearGradientBrush.GetRectangle(Rect: PRectF): TStatus;
begin
Result := SetStatus(GdipGetLineRect(PGpLineGradient(fNativeBrush),PGpRectF(Rect)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TLinearGradientBrush.GetRectangle(Rect: PRect): TStatus;
begin
Result := SetStatus(GdipGetLineRectI(PGpLineGradient(fNativeBrush),PGpRect(Rect)));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetGammaCorrection(UseGammaCorrection: BOOL): TStatus;
begin
Result := SetStatus(GdipSetLineGammaCorrection(PGpLineGradient(fNativeBrush),UseGammaCorrection));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetGammaCorrection: BOOL;
begin
SetStatus(GdipGetLineGammaCorrection(PGpLineGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetBlendCount: INT;
begin
Result := 0;
SetStatus(GdipGetLineBlendCount(PGpLineGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipSetLineBlend(PGpLineGradient(fNativeBrush),BlendFactors,BlendPositions,Count));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipGetLineBlend(PGpLineGradient(fNativeBrush),BlendFactors,BlendPositions,Count));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetInterpolationColorCount: INT;
begin
Result := 0;
SetStatus(GdipGetLinePresetBlendCount(PGpLineGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
var
  Argbs:  array of TARGB;
  i:      Integer;
begin
If (Count > 0) and Assigned(PresetColors) then
  begin
    Argbs := nil;
    SetLength(Argbs,Count);
    If Length(Argbs) > 0 then
      begin
        For i := 0 to Pred(Count) do
          Argbs[i] := GetValue(PColorArray(PresetColors)^[i]);
        Result := SetStatus(GdipSetLinePresetBlend(PGpLineGradient(fNativeBrush),Pointer(Argbs),BlendPositions,Count));
        SetLength(Argbs,0);
      end
    else Result := SetStatus(OutOfMemory);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
var
  Argbs:  array of TARGB;
  i:      Integer;
begin
If (Count > 0) and Assigned(PresetColors) then
  begin
    Argbs := nil;
    SetLength(Argbs,Count);
    If Length(Argbs) > 0 then
      begin
        Result := SetStatus(GdipGetLinePresetBlend(PGpLineGradient(fNativeBrush),Pointer(Argbs),BlendPositions,Count));
        If Result = Ok then
          For i := 0 to Pred(Count) do
            PColorArray(PresetColors)^[i] := Color(Argbs[i]);
        SetLength(Argbs,0);
      end
    else Result := SetStatus(OutOfMemory);      
  end
else Result := SetStatus(InvalidParameter);  
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetBlendBellShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
begin
Result := SetStatus(GdipSetLineSigmaBlend(PGpLineGradient(fNativeBrush),Focus,Scale));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetBlendTriangularShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
begin
Result := SetStatus(GdipSetLineLinearBlend(PGpLineGradient(fNativeBrush),Focus,Scale));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipSetLineTransform(PGpLineGradient(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipGetLineTransform(PGpLineGradient(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.ResetTransform: TStatus;
begin
Result := SetStatus(GdipResetLineTransform(PGpLineGradient(fNativeBrush)));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyLineTransform(PGpLineGradient(fNativeBrush),Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslateLineTransform(PGpLineGradient(fNativeBrush),DX,DY,Order));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScaleLineTransform(PGpLineGradient(fNativeBrush),SX,SY,Order));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotateLineTransform(PGpLineGradient(fNativeBrush),Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
Result := SetStatus(GdipSetLineWrapMode(PGpLineGradient(fNativeBrush),WrapMode));
end;

//!!----------------------------------------------------------------------------

Function TLinearGradientBrush.GetWrapMode: TWrapMode;
begin
SetStatus(GdipGetLineWrapMode(PGpLineGradient(fNativeBrush),@Result));
end;


{!!=============================================================================
    THatchBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    THatchBrush - public methods
-------------------------------------------------------------------------------}

constructor THatchBrush.Create(HatchStyle: THatchStyle; const ForeColor,BackColor: TColor);
var
  Brush: PGpHatch;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreateHatchBrush(HatchStyle,GetValue(ForeColor),GetValue(BackColor),@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor THatchBrush.Create(HatchStyle: THatchStyle; const ForeColor: TColor);
begin
Create(HatchStyle,ForeColor,Color());
end;

//!!----------------------------------------------------------------------------

Function THatchBrush.GetHatchStyle: THatchStyle;
begin
SetStatus(GdipGetHatchStyle(PGpHatch(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function THatchBrush.GetForegroundColor(Color: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(Color) then
  begin
    Result := SetStatus(GdipGetHatchForegroundColor(PGpHatch(fNativeBrush),@Argb));
    SetValue(Color^,Argb);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function THatchBrush.GetBackgroundColor(Color: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(Color) then
  begin
    Result := SetStatus(GdipGetHatchBackgroundColor(PGpHatch(fNativeBrush),@Argb));
    SetValue(Color^,Argb);
  end
else Result := SetStatus(InvalidParameter);
end;


{!!*****************************************************************************
    gdipluspen.h
*******************************************************************************}
{!!=============================================================================
    TPen - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TPen - protected methods
-------------------------------------------------------------------------------}

Function TPen.GetNativeObject: Pointer;
begin
Result := fNativePen;
end;

//!!----------------------------------------------------------------------------

Function TPen.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativePen);
end;

//!!----------------------------------------------------------------------------

constructor TPen.Create(NativePenArg: PGpPen; Status: TStatus);
begin
inherited Create;
fLastResult := Status;
SetNativePen(NativePenArg);
end;

//!!----------------------------------------------------------------------------

procedure TPen.SetNativePen(NativePenArg: PGpPen);
begin
fNativePen := NativePenArg;
end;

//!!----------------------------------------------------------------------------

Function TPen.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TPen - public methods
-------------------------------------------------------------------------------}

constructor TPen.Create(const Color: TColor; Width: REAL = 1.0);
var
  aUnit:  TUnit;
begin
inherited Create;
aUnit := UnitWorld;
fNativePen := nil;
fLastResult := GdipCreatePen1(GetValue(Color),Width,aUnit,@fNativePen);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TPen.Create(Brush: TBrush; Width: REAL = 1.0);
var
  aUnit:  TUnit;
begin
inherited Create;
aUnit := UnitWorld;
fNativePen := nil;
fLastResult := GdipCreatePen2(Brush.NativeObject,Width,aUnit,@fNativePen);
end;

//!!----------------------------------------------------------------------------

destructor TPen.Destroy;
begin
GdipDeletePen(fNativePen);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TPen.Clone: TPen;
var
  ClonePen: PGpPen;
begin
ClonePen := nil;
fLastResult := GdipClonePen(fNativePen,@ClonePen);
Result := TPen.Create(ClonePen,fLastResult);
end;

//!!----------------------------------------------------------------------------

Function TPen.SetWidth(Width: REAL): TStatus;
begin
Result := SetStatus(GdipSetPenWidth(fNativePen,Width));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetWidht: REAL;
begin
SetStatus(GdipGetPenWidth(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetLineCap(StartCap,EndCap: TLineCap; DashCap: TDashCap): TStatus;
begin
Result := SetStatus(GdipSetPenLineCap197819(fNativePen,StartCap,EndCap,DashCap));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetStartCap(StartCap: TLineCap): TStatus;
begin
Result := SetStatus(GdipSetPenStartCap(fNativePen,StartCap));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetEndCap(EndCap: TLineCap): TStatus;
begin
Result := SetStatus(GdipSetPenEndCap(fNativePen,EndCap));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetDashCap(DashCap: TDashCap): TStatus;
begin
Result := SetStatus(GdipSetPenDashCap197819(fNativePen,DashCap));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetStartCap: TLineCap;
begin
SetStatus(GdipGetPenStartCap(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetEndCap: TLineCap;
begin
SetStatus(GdipGetPenEndCap(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetDashCap: TDashCap;
begin
SetStatus(GdipGetPenDashCap197819(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetLineJoin(LineJoin: TLineJoin): TStatus;
begin
Result := SetStatus(GdipSetPenLineJoin(fNativePen,LineJoin));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetLineJoin: TLineJoin;
begin
SetStatus(GdipGetPenLineJoin(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetCustomStartCap(CustomCap: TCustomLineCap): TStatus;
begin
Result := SetStatus(GdipSetPenCustomStartCap(fNativePen,CustomCap.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetCustomStartCap(CustomCap: TCustomLineCap): TStatus;
begin
If Assigned(CustomCap) then
  Result := SetStatus(GdipGetPenCustomStartCap(fNativePen,CustomCap.NativeObjectAddr))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPen.SetCustomEndCap(CustomCap: TCustomLineCap): TStatus;
begin
Result := SetStatus(GdipSetPenCustomEndCap(fNativePen,CustomCap.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetCustomEndCap(CustomCap: TCustomLineCap): TStatus;
begin
If Assigned(CustomCap) then
  Result := SetStatus(GdipGetPenCustomEndCap(fNativePen,CustomCap.NativeObjectAddr))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPen.SetMiterLimit(MiterLimit: REAL): TStatus;
begin
Result := SetStatus(GdipSetPenMiterLimit(fNativePen,MiterLimit));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetMiterLimit: REAL;
begin
SetStatus(GdipGetPenMiterLimit(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetAlignment(PenAlignment: TPenAlignment): TStatus;
begin
Result := SetStatus(GdipSetPenMode(fNativePen,PenAlignment));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetAlignment: TPenAlignment;
begin
SetStatus(GdipGetPenMode(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipSetPenTransform(fNativePen,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipGetPenTransform(fNativePen,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPen.ResetTransform: TStatus;
begin
Result := SetStatus(GdipResetPenTransform(fNativePen));
end;

//!!----------------------------------------------------------------------------

Function TPen.MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyPenTransform(fNativePen,Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TPen.TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslatePenTransform(fNativePen,DX,DY,Order));
end;

//!!----------------------------------------------------------------------------

Function TPen.ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScalePenTransform(fNativePen,SX,SY,Order));
end;

//!!----------------------------------------------------------------------------

Function TPen.RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotatePenTransform(fNativePen,Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetPenType: TPenType;
begin
SetStatus(GdipGetPenFillType(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetColor(const Color: TColor): TStatus;
begin
Result := SetStatus(GdipSetPenColor(fNativePen,GetValue(Color)));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetBrush(Brush: TBrush): TStatus;
begin
Result := SetStatus(GdipSetPenBrushFill(fNativePen,Brush.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetColor(aColor: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(aColor) then
  begin
    If GetPenType = PenTypeSolidColor then
      begin
        SetStatus(GdipGetPenColor(fNativePen,@Argb));
        If fLastResult = Ok then
          aColor^ := Color(Argb);
        Result := fLastResult;
      end
    else Result := WrongState;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPen.GetBrush: TBrush;
var
  NativeBrush:  PGpBrush;
begin
Result := nil;
case GetPenType of
  PenTypeSolidColor:      Result := TSolidBrush.Create;
  PenTypeHatchFill:       Result := THatchBrush.Create;
  PenTypeTextureFill:     Result := TTextureBrush.Create;
  PenTypePathGradient:    Result := TBrush.Create;
  PenTypeLinearGradient:  Result := TLinearGradientBrush.Create;
end;
If Assigned(Result) then
  begin
    SetStatus(GdipGetPenBrushFill(fNativePen,@NativeBrush));
    Result.SetNativeBrush(NativeBrush);
  end;
end;

//!!----------------------------------------------------------------------------

Function TPen.GetDashStyle: TDashStyle;
begin
SetStatus(GdipGetPenDashStyle(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetDashStyle(DashStyle: TDashStyle): TStatus;
begin
Result := SetStatus(GdipSetPenDashStyle(fNativePen,DashStyle));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetDashOffset: REAL;
begin
SetStatus(GdipGetPenDashOffset(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetDashOffset(DashOffset: REAL): TStatus;
begin
Result := SetStatus(GdipSetPenDashOffset(fNativePen,DashOffset));
end;

//!!----------------------------------------------------------------------------

Function TPen.SetDashPattern(DashArray: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipSetPenDashArray(fNativePen,DashArray,Count));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetDashPatternCount: INT;
begin
Result := 0;
SetStatus(GdipGetPenDashCount(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetDashPattern(DashArray: PREAL; Count: INT): TStatus;
begin
If Assigned(DashArray) and (Count > 0) then
  Result := SetStatus(GdipGetPenDashArray(fNativePen,DashArray,Count))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPen.SetCompoundArray(CompoundArray: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipSetPenCompoundArray(fNativePen,CompoundArray,Count));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetCompoundArrayCount: INT;
begin
SetStatus(GdipGetPenCompoundCount(fNativePen,@Result));
end;

//!!----------------------------------------------------------------------------

Function TPen.GetCompoundArray(CompoundArray: PREAL; Count: INT): TStatus;
begin
If Assigned(CompoundArray) and (Count > 0) then
  Result := SetStatus(GdipGetPenCompoundArray(fNativePen,CompoundArray,Count))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPen.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusstringformat.h
*******************************************************************************}
{!!=============================================================================
    TStringFormat - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TStringFormat - protected methods
-------------------------------------------------------------------------------}

Function TStringFormat.GetNativeObject: Pointer;
begin
Result := fNativeFormat;
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeFormat);
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetStatus(NewStatus: TGpStatus): TStatus;
begin
If NewStatus <> Ok then
  fLastError := NewStatus;
Result := NewStatus;
end;

//!!----------------------------------------------------------------------------

constructor TStringFormat.Create(ClonedStringFormat: PGpStringFormat; Status: TStatus);
begin
inherited Create;
fLastError := Status;
fNativeFormat := ClonedStringFormat;
end;

{!!-----------------------------------------------------------------------------
    TStringFormat - public methods
-------------------------------------------------------------------------------}

constructor TStringFormat.Create(FormatFlags: INT = 0; Language: LANGID = LANG_NEUTRAL);
begin
inherited Create;
fNativeFormat := nil;
fLastError := GdipCreateStringFormat(FormatFlags,Language,@fNativeFormat);
end;

//!!----------------------------------------------------------------------------

class Function TStringFormat.GenericDefault: TStringFormat;
var
  Status:       TStatus;
  NativeFormat: PGpStringFormat;
begin
Status := GdipStringFormatGetGenericDefault(@NativeFormat);
Result := TStringFormat.Create(NativeFormat,Status);
end;

//!!----------------------------------------------------------------------------

class Function TStringFormat.GenericTypographic: TStringFormat;
var
  Status:       TStatus;
  NativeFormat: PGpStringFormat;
begin
Status := GdipStringFormatGetGenericTypographic(@NativeFormat);
Result := TStringFormat.Create(NativeFormat,Status);
end;

//!!----------------------------------------------------------------------------

constructor TStringFormat.Create(Format: TStringFormat);
begin
inherited Create;
fNativeFormat := nil;
fLastError := GdipCloneStringFormat(Format.NativeObject,@fNativeFormat)
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.Clone: TStringFormat;
var
  ClonedStringFormat: PGpStringFormat;
begin
fLastError := GdipCloneStringFormat(fNativeFormat,@ClonedStringFormat);
If fLastError = Ok then
  Result := TStringFormat.Create(ClonedStringFormat,fLastError)
else
  Result := nil;
end;

//!!----------------------------------------------------------------------------

destructor TStringFormat.Destroy;
begin
GdipDeleteStringFormat(fNativeFormat);
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetFormatFlags(Flags: INT): TStatus;
begin
Result := SetStatus(GdipSetStringFormatFlags(fNativeFormat,Flags));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetFormatFlags: INT;
begin
SetStatus(GdipGetStringFormatFlags(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetAlignment(Align: TStringAlignment): TStatus;
begin
Result := SetStatus(GdipSetStringFormatAlign(fNativeFormat,Align));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetAlignment: TStringAlignment;
begin
SetStatus(GdipGetStringFormatAlign(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetLineAlignment(Align: TStringAlignment): TStatus;
begin
Result := SetStatus(GdipSetStringFormatLineAlign(fNativeFormat,Align));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetLineAlignment: TStringAlignment;
begin
SetStatus(GdipGetStringFormatLineAlign(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetHotkeyPrefix(HotkeyPrefix: THotkeyPrefix): TStatus;
begin
Result := SetStatus(GdipSetStringFormatHotkeyPrefix(fNativeFormat,INT(HotkeyPrefix)));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetHotkeyPrefix: THotkeyPrefix;
begin
SetStatus(GdipGetStringFormatHotkeyPrefix(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetTabStops(FirstTabOffset: REAL; Count: INT; TabStops: PREAL): TStatus;
begin
Result := SetStatus(GdipSetStringFormatTabStops(fNativeFormat,FirstTabOffset,Count,TabStops));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetTabStopCount: INT;
begin
SetStatus(GdipGetStringFormatTabStopCount(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetTabStops(Count: INT; FirstTabOffset,TabStops: PREAL): TStatus;
begin
Result := SetStatus(GdipGetStringFormatTabStops(fNativeFormat,Count,FirstTabOffset,TabStops));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetDigitSubstitution(Language: LANGID; Substitute: TStringDigitSubstitute): TStatus;
begin
Result := SetStatus(GdipSetStringFormatDigitSubstitution(fNativeFormat,Language,Substitute));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetDigitSubstitutionLanguage: LANGID;
begin
SetStatus(GdipGetStringFormatDigitSubstitution(fNativeFormat,@Result,nil));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
SetStatus(GdipGetStringFormatDigitSubstitution(fNativeFormat,nil,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetTrimming(Trimming: TStringTrimming): TStatus;
begin
Result := SetStatus(GdipSetStringFormatTrimming(fNativeFormat,Trimming));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetTrimming: TStringTrimming;
begin
SetStatus(GdipGetStringFormatTrimming(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.SetMeasurableCharacterRanges(RangeCount: INT; Ranges: PCharacterRange): TStatus;
begin
Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(fNativeFormat,RangeCount,Ranges));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetMeasurableCharacterRangeCount: INT;
begin
SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(fNativeFormat,@Result));
end;

//!!----------------------------------------------------------------------------

Function TStringFormat.GetLastStatus: TStatus;
begin
Result := fLastError;
fLastError := Ok;
end;


{!!*****************************************************************************
    gdipluspath.h
*******************************************************************************}
{!!=============================================================================
    TGraphicsPath - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TGraphicsPath - protected methods
-------------------------------------------------------------------------------}

Function TGraphicsPath.GetNativeObject: Pointer;
begin
Result := fNativePath;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativePath);
end;

//!!----------------------------------------------------------------------------

constructor TGraphicsPath.Create(Path: TGraphicsPath);
var
  ClonePath:  PGpPath;
begin
inherited Create;
ClonePath := nil;
SetStatus(GdipClonePath(Path.NativeObject,@ClonePath));
SetNativePath(ClonePath);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGraphicsPath.Create(NativePath: PGpPath);
begin
inherited Create;
fLastResult := Ok;
SetNativePath(NativePath);
end;

//!!----------------------------------------------------------------------------

procedure TGraphicsPath.SetNativePath(NativePathArg: PGpPath);
begin
fNativePath := NativePathArg;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TGraphicsPath - public methods
-------------------------------------------------------------------------------}

constructor TGraphicsPath.Create(FillMode: TFillMode = FillModeAlternate);
begin
inherited Create;
fNativePath := nil;
fLastResult := GdipCreatePath(FillMode,@fNativePath);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGraphicsPath.Create(Points: PPointF; Types: PBYTE; Count: INT; FillMode: TFillMode = FillModeAlternate);
begin
inherited Create;
fNativePath := nil;
fLastResult := GdipCreatePath2(PGpPointF(Points),Types,Count,FillMode,@fNativePath);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGraphicsPath.Create(Points: PPoint; Types: PBYTE; Count: INT; FillMode: TFillMode = FillModeAlternate);
begin
inherited Create;
fNativePath := nil;
fLastResult := GdipCreatePath2I(PGpPoint(Points),Types,Count,FillMode,@fNativePath);
end;

//!!----------------------------------------------------------------------------

destructor TGraphicsPath.Destroy;
begin
GdipDeletePath(fNativePath);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Clone: TGraphicsPath;
var
  ClonePath: PGpPath;
begin
ClonePath := nil;
SetStatus(GdipClonePath(fNativePath,@ClonePath));
Result := TGraphicsPath.Create(ClonePath);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Reset: TStatus;
begin
Result := SetStatus(GdipResetPath(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetFillMode: TFillMode;
begin
Result := FillModeAlternate;
SetStatus(GdipGetPathFillMode(fNativePath,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.SetFillMode(FillMode: TFillMode): TStatus;
begin
Result := SetStatus(GdipSetPathFillMode(fNativePath,FillMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetPathData(PathData: PPathData): TStatus;
var
  Count:  INT;
begin
If Assigned(PathData) then
  begin
    Count := GetPointCount;
    If (Count <= 0) or ((PathData^.Count > 0) and (PathData^.Count < Count)) then
      begin
        PathData^.Count := 0;
        If Assigned(PathData^.Points) then
          begin
            FreeMem(PathData^.Points);
            PathData^.Points := nil;
          end;
        If Assigned(PathData^.Types) then
          begin
            FreeMem(PathData^.Types);
            PathData^.Types := nil;
          end;
        If Count <= 0 then
          begin
            Result := Ok;
            Exit;
          end;
      end;
    If PathData^.Count = 0 then
      begin
        GetMem(PathData^.Points,Count * SizeOf(TPointF));
        If not Assigned(PathData^.Points) then
          begin
            Result := SetStatus(OutOfMemory);
            Exit;
          end;
        GetMem(PathData^.Types,Count);
        If not Assigned(PathData^.Types) then
          begin
            FreeMem(PathData^.Points);
            PathData^.Points := nil;
            Result := SetStatus(OutOfMemory);
            Exit;          
          end;
        PathData^.Count := Count;
      end;
    Result := SetStatus(GdipGetPathData(fNativePath,PGpPathData(PathData)));
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.StartFigure: TStatus;
begin
Result := SetStatus(GdipStartPathFigure(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.CloseFigure: TStatus;
begin
Result := SetStatus(GdipClosePathFigure(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.CloseAllFigures: TStatus;
begin
Result := SetStatus(GdipClosePathFigures(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.SetMarker: TStatus;
begin
Result := SetStatus(GdipSetPathMarker(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.ClearMarkers: TStatus;
begin
Result := SetStatus(GdipClearPathMarkers(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Reverse: TStatus;
begin
Result := SetStatus(GdipReversePath(fNativePath));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetLastPoint(LastPoint: PPointF): TStatus;
begin
Result := SetStatus(GdipGetPathLastPoint(fNativePath,PGpPointF(LastPoint)));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddLine(const Pt1,Pt2: TPointF): TStatus;
begin
Result := AddLine(Pt1.X,Pt1.Y,Pt2.X,Pt2.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddLine(X1,Y1,X2,Y2: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathLine(fNativePath,X1,Y1,X2,Y2));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddLines(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathLine2(fNativePath,PGpPointF(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddLine(const Pt1,Pt2: TPoint): TStatus;
begin
Result := AddLine(Pt1.X,Pt1.Y,Pt2.X,Pt2.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddLine(X1,Y1,X2,Y2: INT): TStatus;
begin
Result := SetStatus(GdipAddPathLineI(fNativePath,X1,Y1,X2,Y2));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddLines(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathLine2I(fNativePath,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddArc(const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := AddArc(Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddArc(X,Y,Width,Height: REAL; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathArc(fNativePath,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddArc(const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := AddArc(Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddArc(X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathArcI(fNativePath,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddBezier(const Pt1,Pt2,Pt3,Pt4: TPointF): TStatus;
begin
Result := AddBezier(Pt1.X,Pt1.Y,Pt2.X,Pt2.Y,Pt3.X,Pt3.Y,Pt4.X,Pt4.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddBezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathBezier(fNativePath,X1,Y1,X2,Y2,X3,Y3,X4,Y4));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddBeziers(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathBeziers(fNativePath,PGpPointF(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddBezier(const Pt1,Pt2,Pt3,Pt4: TPoint): TStatus;
begin
Result := AddBezier(Pt1.X,Pt1.Y,Pt2.X,Pt2.Y,Pt3.X,Pt3.Y,Pt4.X,Pt4.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddBezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4: INT): TStatus;
begin
Result := SetStatus(GdipAddPathBezierI(fNativePath,X1,Y1,X2,Y2,X3,Y3,X4,Y4));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddBeziers(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathBeziersI(fNativePath,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddCurve(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathCurve(fNativePath,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddCurve(Points: PPointF; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathCurve2(fNativePath,PGpPointF(Points),Count,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddCurve(Points: PPointF; Count,Offset,NumberOfSegments: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathCurve3(fNativePath,PGpPointF(Points),Count,Offset,NumberOfSegments,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddCurve(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathCurveI(fNativePath,PGpPoint(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddCurve(Points: PPoint; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathCurve2I(fNativePath,PGpPoint(Points),Count,Tension));
end;             

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddCurve(Points: PPoint; Count,Offset,NumberOfSegments: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathCurve3I(fNativePath,PGpPoint(Points),Count,Offset,NumberOfSegments,Tension));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddClosedCurve(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathClosedCurve(fNativePath,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddClosedCurve(Points: PPointF; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathClosedCurve2(fNativePath,PGpPointF(Points),Count,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddClosedCurve(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathClosedCurveI(fNativePath,PGpPoint(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddClosedCurve(Points: PPoint; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathClosedCurve2I(fNativePath,PGpPoint(Points),Count,Tension));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddRectangle(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipAddPathRectangle(fNativePath,Rect.X,Rect.Y,Rect.Width,Rect.Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddRectangles(Rects: PRectF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathRectangles(fNativePath,PGpRectF(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddRectangle(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipAddPathRectangleI(fNativePath,Rect.X,Rect.Y,Rect.Width,Rect.Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddRectangles(Rects: PRect; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathRectanglesI(fNativePath,PGpRect(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddEllipse(const Rect: TRectF): TStatus;
begin
Result := AddEllipse(Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddEllipse(X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathEllipse(fNativePath,X,Y,Width,Height));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddEllipse(const Rect: TRect): TStatus;
begin
Result := AddEllipse(Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddEllipse(X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipAddPathEllipseI(fNativePath,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddPie(const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := AddPie(Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddPie(X,Y,Width,Height: REAL; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathPie(fNativePath,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddPie(const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := AddPie(Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddPie(X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipAddPathPieI(fNativePath,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddPolygon(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathPolygon(fNativePath,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddPolygon(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipAddPathPolygonI(fNativePath,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddPath(AddingPath: TGraphicsPath; Connect: BOOL): TStatus;
begin
Result := SetStatus(GdipAddPathPath(fNativePath,AddingPath.NativeObject,Connect))
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const Origin: TPointF; Format: TStringFormat): TStatus;
var
  LayoutRect: TRectF;
begin
LayoutRect := RectF(Origin.X,Origin.Y,0.0,0.0);
Result := SetStatus(GdipAddPathString(fNativePath,Str,Length,Family.NativeObject,Style,EmSize,@LayoutRect,Format.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const Origin: TPointF; Format: TStringFormat): TStatus;
begin
Result := AddString(PWideChar(StrToWide(Str)),Length,Family,Style,EmSize,Origin,Format);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL{World units};
  const LayoutRect: TRectF; Format: TStringFormat): TStatus;
begin
Result := SetStatus(GdipAddPathString(fNativePath,Str,Length,Family.NativeObject,Style,EmSize,@LayoutRect,Format.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const LayoutRect: TRectF; Format: TStringFormat): TStatus;
begin
Result := AddString(PWideChar(StrToWide(Str)),Length,Family,Style,EmSize,LayoutRect,Format);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const Origin: TPoint; Format: TStringFormat): TStatus;
var
  LayoutRect:   TRect;
begin
LayoutRect := RectI(Origin.X,Origin.Y,0,0);
Result := SetStatus(GdipAddPathStringI(fNativePath,Str,Length,Family.NativeObject,Style,EmSize,@LayoutRect,Format.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const Origin: TPoint; Format: TStringFormat): TStatus;
begin
Result := AddString(PWideChar(StrToWide(Str)),Length,Family,Style,EmSize,Origin,Format);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(Str: PWideChar; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL{World units};
  const LayoutRect: TRect; Format: TStringFormat): TStatus;
begin
Result := SetStatus(GdipAddPathStringI(fNativePath,Str,Length,Family.NativeObject,Style,EmSize,@LayoutRect,Format.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.AddString(const Str: String; Length: Int; Family: TFontFamily; Style: INT; EmSize: REAL;
  const LayoutRect: TRect; Format: TStringFormat): TStatus;
begin
Result := AddString(PWideChar(StrToWide(Str)),Length,Family,Style,EmSize,LayoutRect,Format);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Transform(Matrix: TMatrix): TStatus;
begin
If Assigned(Matrix) then
  Result := SetStatus(GdipTransformPath(fNativePath,Matrix.NativeObject))
else
  Result := Ok;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetBounds(const Bounds: TRectF; Matrix: TMatrix = nil; Pen: TPen = nil): TStatus;
//!! originally implemented in gdiplusgraphics.h
begin
Result := SetStatus(GdipGetPathWorldBounds(fNativePath,@Bounds,Matrix.NativeObject,Pen.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.GetBounds(const Bounds: TRect; Matrix: TMatrix = nil; Pen: TPen = nil): TStatus;
//!! originally implemented in gdiplusgraphics.h
begin
Result := SetStatus(GdipGetPathWorldBoundsI(fNativePath,@Bounds,Matrix.NativeObject,Pen.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Flatten(Matrix: TMatrix; Flatness: REAL): TStatus;
begin
Result := SetStatus(GdipFlattenPath(fNativePath,Matrix.NativeObject,Flatness));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.Flatten(Matrix: TMatrix = nil): TStatus;
begin
Result := Flatten(Matrix,FlatnessDefault);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Widen(Pen: TPen; Matrix: TMatrix; Flatness: REAL): TStatus;
begin
Result := SetStatus(GdipWidenPath(fNativePath,Pen.NativeObject,Matrix.NativeObject,Flatness));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.Widen(Pen: TPen; Matrix: TMatrix = nil): TStatus;
begin
Result := Widen(Pen,Matrix,FlatnessDefault);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Outline(Matrix: TMatrix; Flatness: REAL): TStatus;
begin
Result := SetStatus(GdipWindingModeOutline(fNativePath,Matrix.NativeObject,Flatness));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.Outline(Matrix: TMatrix = nil): TStatus;
begin
Result := Outline(Matrix,FlatnessDefault);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.Warp(DestPoints: PPointF; Count: INT; const SrcRect: TRectF; Matrix: TMatrix;
  WarpMode: TWarpMode; Flatness: REAL): TStatus;
begin
Result := SetStatus(GdipWarpPath(fNativePath,Matrix.NativeObject,PGpPointF(DestPoints),Count,
  SrcRect.X,SrcRect.Y,SrcRect.Width,SrcRect.Height,WarpMode,Flatness));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.Warp(DestPoints: PPointF; Count: INT; const SrcRect: TRectF; Matrix: TMatrix = nil;
  WarpMode: TWarpMode = WarpModePerspective): TStatus;
begin
Result := Warp(DestPoints,Count,SrcRect,Matrix,WarpMode,FlatnessDefault);
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetPointCount: INT;
begin
SetStatus(GdipGetPointCount(fNativePath,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetPathTypes(Types: PBYTE; Count: INT): TStatus;
begin
Result := SetStatus(GdipGetPathTypes(fNativePath,Types,Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetPathPoints(Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipGetPathPoints(fNativePath,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.GetPathPoints(Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipGetPathPointsI(fNativePath,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.IsVisible(const Point: TPointF; G: TGraphicsBase = nil): BOOL;
begin
Result := IsVisible(Point.X,Point.Y,G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsVisible(X,Y: REAL; G: TGraphicsBase = nil): BOOL;
//!! originally implemented in gdiplusgraphics.h
begin
Result := False;
SetStatus(GdipIsVisiblePathPoint(fNativePath,X,Y,G.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsVisible(const Point: TPoint; G: TGraphicsBase = nil): BOOL;
begin
Result := IsVisible(Point.X,Point.Y,G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsVisible(X,Y: INT; G: TGraphicsBase = nil): BOOL;
//!! originally implemented in gdiplusgraphics.h
begin
Result := False;
SetStatus(GdipIsVisiblePathPointI(fNativePath,X,Y,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPath.IsOutlineVisible(const Point: TPointF; Pen: TPen; G: TGraphicsBase = nil): BOOL;
begin
Result := IsOutlineVisible(Point.X,Point.Y,Pen,G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsOutlineVisible(X,Y: REAL; Pen: TPen; G: TGraphicsBase = nil): BOOL;
//!! originally implemented in gdiplusgraphics.h
begin
Result := False;
SetStatus(GdipIsOutlineVisiblePathPoint(fNativePath,X,Y,Pen.NativeObject,G.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsOutlineVisible(const Point: TPoint; Pen: TPen; G: TGraphicsBase = nil): BOOL;
begin
Result := IsOutlineVisible(Point.X,Point.Y,Pen,G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPath.IsOutlineVisible(X,Y: INT; Pen: TPen; G: TGraphicsBase = nil): BOOL;
//!! originally implemented in gdiplusgraphics.h
begin
Result := False;
SetStatus(GdipIsOutlineVisiblePathPointI(fNativePath,X,Y,Pen.NativeObject,G.NativeObject,@Result));
end;


{!!=============================================================================
    TGraphicsPathIterator - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TGraphicsPathIterator - protected methods
-------------------------------------------------------------------------------}

procedure TGraphicsPathIterator.SetNativeIterator(NativeIteratorArg: PGpPathIterator);
begin
fNativeIterator := NativeIteratorArg;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TGraphicsPathIterator - public methods
-------------------------------------------------------------------------------}

constructor TGraphicsPathIterator.Create(Path: TGraphicsPath);
var
  Iter: PGpPathIterator;
begin
inherited Create;
Iter := nil;
fLastResult := GdipCreatePathIter(@Iter,Path.NativeObject);
SetNativeIterator(Iter);
end;

//!!----------------------------------------------------------------------------

destructor TGraphicsPathIterator.Destroy;
begin
GdipDeletePathIter(fNativeIterator);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.NextSubPath(StartIndex,EndIndex: PINT; IsClosed: PBOOL): INT;
begin
SetStatus(GdipPathIterNextSubpath(fNativeIterator,@Result,StartIndex,EndIndex,IsClosed));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPathIterator.NextSubPath(Path: TGraphicsPath; IsClosed: PBOOL): INT;
begin
SetStatus(GdipPathIterNextSubpathPath(fNativeIterator,@Result,Path.NativeObject,IsClosed));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.NextPathType(PathType: PBYTE; StartIndex,EndIndex: PINT): INT;
begin
SetStatus(GdipPathIterNextPathType(fNativeIterator,@Result,PathType,StartIndex,EndIndex));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.NextMarker(StartIndex,EndIndex: PINT): INT;
begin
SetStatus(GdipPathIterNextMarker(fNativeIterator,@Result,StartIndex,EndIndex));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphicsPathIterator.NextMarker(Path: TGraphicsPath): INT;
begin
SetStatus(GdipPathIterNextMarkerPath(fNativeIterator,@Result,Path.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.GetCount: INT;
begin
SetStatus(GdipPathIterGetCount(fNativeIterator,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.GetSubpathCount: INT;
begin
SetStatus(GdipPathIterGetSubpathCount(fNativeIterator,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.HasCurve: BOOL;
begin
SetStatus(GdipPathIterHasCurve(fNativeIterator,@Result));
end;

//!!----------------------------------------------------------------------------

procedure TGraphicsPathIterator.Rewind;
begin
SetStatus(GdipPathIterRewind(fNativeIterator));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.Enumerate(Points: PPointF; Types: PBYTE; Count: INT): INT;
begin
SetStatus(GdipPathIterEnumerate(fNativeIterator,@Result,PGpPointF(Points),Types,Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.CopyData(Points: PPointF; Types: PBYTE; StartIndex,EndIndex: INT): INT;
begin
SetStatus(GdipPathIterCopyData(fNativeIterator,@Result,PGpPointF(Points),Types,StartIndex,EndIndex));
end;

//!!----------------------------------------------------------------------------

Function TGraphicsPathIterator.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!=============================================================================
    TPathGradientBrush - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TPathGradientBrush - public methods
-------------------------------------------------------------------------------}

constructor TPathGradientBrush.Create(Points: PPointF; Count: INT; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush:  PGpPathGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreatePathGradient(PGpPointF(Points),Count,WrapMode,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TPathGradientBrush.Create(Points: PPoint; Count: INT; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush:  PGpPathGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreatePathGradientI(PGpPoint(Points),Count,WrapMode,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TPathGradientBrush.Create(Path: TGraphicsPath);
var
  Brush:  PGpPathGradient;
begin
inherited Create;
Brush := nil;
fLastResult := GdipCreatePathGradientFromPath(Path.NativeObject,@Brush);
SetNativeBrush(PGpBrush(Brush));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetCenterColor(Color: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(Color) then
  begin
    SetStatus(GdipGetPathGradientCenterColor(PGpPathGradient(fNativeBrush),@Argb));
    SetValue(Color^,Argb);
    Result := fLastResult;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetCenterColor(const Color: TColor): TStatus;
begin
SetStatus(GdipSetPathGradientCenterColor(PGpPathGradient(fNativeBrush),GetValue(Color)));
Result := fLastResult;
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetPointCount: INT;
begin
SetStatus(GdipGetPathGradientPointCount(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetSurroundColorCount: INT;
begin
SetStatus(GdipGetPathGradientSurroundColorCount(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetSurroundColors(Colors: PColor; Count: PINT): TStatus;
var
  Count1: INT;
  Argbs:  array of TARGB;
  i:      Integer;
begin
If Assigned(Colors) and Assigned(Count) then
  begin
    SetStatus(GdipGetPathGradientSurroundColorCount(PGpPathGradient(fNativeBrush),@Count1));
    If fLastResult = Ok then
      begin
        If (Count^ >= Count1) and (Count1 > 0) then
          begin
            Argbs := nil;
            SetLength(Argbs,Count1);
            If Length(Argbs) > 0 then
              begin
                SetStatus(GdipGetPathGradientSurroundColorsWithCount(
                  PGpPathGradient(fNativeBrush),Pointer(Argbs),@Count1));
                If fLastResult = Ok then
                  begin
                    For i := 0 to Pred(Count1) do
                      SetValue(PColorArray(Colors)^[i],Argbs[i]);
                    Count^ := Count1;
                  end;
                SetLength(Argbs,0);
                Result := fLastResult;
              end
            else Result := SetStatus(OutOfMemory);
          end
        else Result := SetStatus(InsufficientBuffer);
      end
    else Result := fLastResult;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetSurroundColors(Colors: PColor; Count: PINT): TStatus;
var
  Count1: INT;
  Argbs:  array of TARGB;
  i:      Integer;
begin
If Assigned(Colors) and Assigned(Count) then
  begin
    Count1 := GetPointCount;
    If (Count^ <= Count1) and (Count1 > 0) then
      begin
        Count1 := Count^;
        Argbs := nil;
        SetLength(Argbs,Count1);
        If Length(Argbs) > 0 then
          begin
            For i := 0 to Pred(Count1) do
              Argbs[i] := GetValue(PColorArray(Colors)^[i]);
            SetStatus(GdipSetPathGradientSurroundColorsWithCount(
              PGpPathGradient(fNativeBrush),Pointer(Argbs),@Count1));
            If fLastResult = Ok then
              Count^ := Count1;
            SetLength(Argbs,0);
            Result := fLastResult;
          end
        else Result := SetStatus(OutOfMemory);
      end
    else Result := SetStatus(InvalidParameter);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetGraphicsPath(Path: TGraphicsPath): TStatus;
begin
If Assigned(Path) then
  Result := SetStatus(GdipGetPathGradientPath(PGpPathGradient(fNativeBrush),Path.NativeObject))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetGraphicsPath(Path: TGraphicsPath): TStatus;
begin
If Assigned(Path) then
  Result := SetStatus(GdipSetPathGradientPath(PGpPathGradient(fNativeBrush),Path.NativeObject))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetCenterPoint(Point: PPointF): TStatus;
begin
Result := SetStatus(GdipGetPathGradientCenterPoint(PGpPathGradient(fNativeBrush),PGpPointF(Point)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TPathGradientBrush.GetCenterPoint(Point: PPoint): TStatus;
begin
Result := SetStatus(GdipGetPathGradientCenterPointI(PGpPathGradient(fNativeBrush),PGpPoint(Point)));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetCenterPoint(const Point: TPointF): TStatus;
begin
Result := SetStatus(GdipSetPathGradientCenterPoint(PGpPathGradient(fNativeBrush),@Point));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TPathGradientBrush.SetCenterPoint(const Point: TPoint): TStatus;
begin
Result := SetStatus(GdipSetPathGradientCenterPointI(PGpPathGradient(fNativeBrush),@Point));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetRectangle(Rect: PRectF): TStatus;
begin
Result := SetStatus(GdipGetPathGradientRect(PGpPathGradient(fNativeBrush),@Rect));
end;
//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TPathGradientBrush.GetRectangle(Rect: PRect): TStatus;
begin
Result := SetStatus(GdipGetPathGradientRectI(PGpPathGradient(fNativeBrush),@Rect));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetGammaCorrection(UseGammaCorrection: BOOL): TStatus;
begin
Result := SetStatus(GdipSetPathGradientGammaCorrection(PGpPathGradient(fNativeBrush),UseGammaCorrection));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetGammaCorrection: BOOL;
begin
SetStatus(GdipGetPathGradientGammaCorrection(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetBlendCount: INT;
begin
SetStatus(GdipGetPathGradientBlendCount(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipGetPathGradientBlend(PGpPathGradient(fNativeBrush),BlendFactors,BlendPositions,Count));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetBlend(BlendFactors,BlendPositions: PREAL; Count: INT): TStatus;
begin
Result := SetStatus(GdipSetPathGradientBlend(PGpPathGradient(fNativeBrush),BlendFactors,BlendPositions,Count));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetInterpolationColorCount: INT;
begin
SetStatus(GdipGetPathGradientPresetBlendCount(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
var
  Argbs:  array of TARGB;
  i:      Integer;
begin
Argbs := nil;
SetLength(Argbs,Count);
If Length(Argbs) > 0 then
  begin
    For i := 0 to Pred(Count) do
      Argbs[i] := GetValue(PColorArray(PresetColors)^[i]);
    Result := SetStatus(GdipSetPathGradientPresetBlend(
      PGpPathGradient(fNativeBrush),Pointer(Argbs),BlendPositions,Count));
    SetLength(Argbs,0);
  end
else Result := SetStatus(OutOfMemory);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetInterpolationColors(PresetColors: PColor; BlendPositions: PREAL; Count: INT): TStatus;
var
  Argbs:  array of TARGB;
  i:      Integer;
begin
If (Count > 0) and Assigned(PresetColors) then
  begin
    Argbs := nil;
    SetLength(Argbs,Count);
    If Length(Argbs) > 0 then
      begin
        Result := SetStatus(GdipGetPathGradientPresetBlend(
          PGpPathGradient(fNativeBrush),Pointer(Argbs),BlendPositions,Count));
        For i := 0 to Pred(Count) do
          PColorArray(PresetColors)^[i] := Color(Argbs[i]);
        SetLength(Argbs,0);
      end
    else Result := SetStatus(OutOfMemory);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetBlendBellShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
begin
Result := SetStatus(GdipSetPathGradientSigmaBlend(PGpPathGradient(fNativeBrush),Focus,Scale));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetBlendTriangularShape(Focus: REAL; Scale: REAL = 1.0): TStatus;
begin
Result := SetStatus(GdipSetPathGradientLinearBlend(PGpPathGradient(fNativeBrush),Focus,Scale));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipGetPathGradientTransform(PGpPathGradient(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipSetPathGradientTransform(PGpPathGradient(fNativeBrush),Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.ResetTransform: TStatus;
begin
Result := SetStatus(GdipResetPathGradientTransform(PGpPathGradient(fNativeBrush)));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyPathGradientTransform(PGpPathGradient(fNativeBrush),Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslatePathGradientTransform(PGpPathGradient(fNativeBrush),DX,DY,Order));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScalePathGradientTransform(PGpPathGradient(fNativeBrush),SX,SY,Order));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotatePathGradientTransform(PGpPathGradient(fNativeBrush),Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetFocusScales(XScale,YScale: PREAL): TStatus;
begin
Result := SetStatus(GdipGetPathGradientFocusScales(PGpPathGradient(fNativeBrush),XScale,YScale));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetFocusScales(XScale,YScale: REAL): TStatus;
begin
Result := SetStatus(GdipSetPathGradientFocusScales(PGpPathGradient(fNativeBrush),XScale,YScale));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.GetWrapMode: TWrapMode;
begin
SetStatus(GdipGetPathGradientWrapMode(PGpPathGradient(fNativeBrush),@Result));
end;

//!!----------------------------------------------------------------------------

Function TPathGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
Result := SetStatus(GdipSetPathGradientWrapMode(PGpPathGradient(fNativeBrush),WrapMode));
end;


{!!*****************************************************************************
    gdipluslinecaps.h
*******************************************************************************}
{!!=============================================================================
    TCustomLineCap - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TCustomLineCap - protected methods
-------------------------------------------------------------------------------}

Function TCustomLineCap.GetNativeObject: Pointer;
begin
Result := fNativeCap;
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeCap);
end;

//!!----------------------------------------------------------------------------

constructor TCustomLineCap.Create;
begin
inherited Create;
fNativeCap := nil;
fLastResult := Ok;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCustomLineCap.Create(NativeCapArg: PGpCustomLineCap; Status: TStatus);
begin
Create; //!! do not call inherited constructor
fLastResult := Status;
SetNativeCap(NativeCapArg);
end;

//!!----------------------------------------------------------------------------

procedure TCustomLineCap.SetNativeCap(NativeCapArg: PGpCustomLineCap);
begin
fNativeCap := NativeCapArg;
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TCustomLineCap - public methods
-------------------------------------------------------------------------------}

constructor TCustomLineCap.Create(FillPath,StrokePath: TGraphicsPathBase; BaseCap: TLineCap = LineCapFlat; BaseInset: REAL = 0);
begin
Create;
fNativeCap := nil;
fLastResult := GdipCreateCustomLineCap(FillPath.NativeObject,StrokePath.NativeObject,BaseCap,BaseInset,@fNativeCap);
end;

//!!----------------------------------------------------------------------------

destructor TCustomLineCap.Destroy;
begin
GdipDeleteCustomLineCap(fNativeCap);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.Clone: TCustomLineCap;
var
  NewNativeLineCap: PGpCustomLineCap;
begin
NewNativeLineCap := nil;
SetStatus(GdipCloneCustomLineCap(fNativeCap,@NewNativeLineCap));
If fLastResult = Ok then
  begin
    Result := TCustomLineCap.Create(NewNativeLineCap,fLastResult);
    If not Assigned(Result) then
      SetStatus(GdipDeleteCustomLineCap(NewNativeLineCap));
  end
else Result := nil;
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetStrokeCap(StrokeCap: TLineCap): TStatus;
begin
Result := SetStrokeCaps(StrokeCap,StrokeCap);
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetStrokeCaps(StartCap,EndCap: TLineCap): TStatus;
begin
Result := SetStatus(GdipSetCustomLineCapStrokeCaps(fNativeCap,StartCap,EndCap));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetStrokeCaps(StartCap,EndCap: PLineCap): TStatus;
begin
Result := SetStatus(GdipGetCustomLineCapStrokeCaps(fNativeCap,PGpLineCap(StartCap),PGpLineCap(EndCap)));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetStrokeJoin(LineJoin: TLineJoin): TStatus;
begin
Result := SetStatus(GdipSetCustomLineCapStrokeJoin(fNativeCap,LineJoin));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetStrokeJoin: TLineJoin;
begin
SetStatus(GdipGetCustomLineCapStrokeJoin(fNativeCap,@Result));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetBaseCap(BaseCap: TLineCap): TStatus;
begin
Result := SetStatus(GdipSetCustomLineCapBaseCap(fNativeCap,BaseCap));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetBaseCap: TLineCap;
begin
SetStatus(GdipGetCustomLineCapBaseCap(fNativeCap,@Result));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetBaseInset(Inset: REAL): TStatus;
begin
Result := SetStatus(GdipSetCustomLineCapBaseInset(fNativeCap,Inset));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetBaseInset: REAL;
begin
SetStatus(GdipGetCustomLineCapBaseInset(fNativeCap,@Result));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.SetWidthScale(WidthScale: REAL): TStatus;
begin
Result := SetStatus(GdipSetCustomLineCapWidthScale(fNativeCap,WidthScale));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetWidthScale: REAL;
begin
SetStatus(GdipGetCustomLineCapWidthScale(fNativeCap,@Result));
end;

//!!----------------------------------------------------------------------------

Function TCustomLineCap.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!=============================================================================
    TAdjustableArrowCap - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TAdjustableArrowCap - public methods
-------------------------------------------------------------------------------}

constructor TAdjustableArrowCap.Create(Height,Width: REAL; IsFilled: BOOL = True);
var
  Cap:  PGpAdjustableArrowCap;
begin
inherited Create;
Cap := nil;
fLastResult := GdipCreateAdjustableArrowCap(Height,Width,IsFilled,@Cap);
SetNativeCap(PGpCustomLineCap(Cap));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.SetHeight(Height: REAL): TStatus;
begin
Result := SetStatus(GdipSetAdjustableArrowCapHeight(PGpAdjustableArrowCap(fNativeCap),Height));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.GetHeight: REAL;
begin
SetStatus(GdipGetAdjustableArrowCapHeight(PGpAdjustableArrowCap(fNativeCap),@Result));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.SetWidth(Width: REAL): TStatus;
begin
Result := SetStatus(GdipSetAdjustableArrowCapWidth(PGpAdjustableArrowCap(fNativeCap),Width));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.GetWidth: REAL;
begin
SetStatus(GdipGetAdjustableArrowCapWidth(PGpAdjustableArrowCap(fNativeCap),@Result));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.SetMiddleInset(MiddleInset: REAL): TStatus;
begin
Result := SetStatus(GdipSetAdjustableArrowCapMiddleInset(PGpAdjustableArrowCap(fNativeCap),MiddleInset));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.GetMiddleInset: REAL;
begin
SetStatus(GdipGetAdjustableArrowCapMiddleInset(PGpAdjustableArrowCap(fNativeCap),@Result));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.SetFillState(IsFilled: BOOL): TStatus;
begin
Result := SetStatus(GdipSetAdjustableArrowCapFillState(PGpAdjustableArrowCap(fNativeCap),IsFilled));
end;

//!!----------------------------------------------------------------------------

Function TAdjustableArrowCap.IsFilled: BOOL;
begin
SetStatus(GdipGetAdjustableArrowCapFillState(PGpAdjustableArrowCap(fNativeCap),@Result));
end;


{!!*****************************************************************************
    gdiplusgraphics.h
*******************************************************************************}
{!!=============================================================================
    TGraphics - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TGraphics - protected methods
-------------------------------------------------------------------------------}

Function TGraphics.GetNativeObject: Pointer;
begin
Result := fNativeGraphics;
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeGraphics);
end;

//!!----------------------------------------------------------------------------

constructor TGraphics.Create(Graphics: PGpGraphics);
begin
inherited Create;
fLAstResult := Ok;
SetNativeGraphics(Graphics);
end;

//!!----------------------------------------------------------------------------

procedure TGraphics.SetNativeGraphics(Graphics: PGpGraphics);
begin
fNativeGraphics := Graphics;
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetNativeGraphics: PGpGraphics;
begin
Result := fNativeGraphics;
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetNativePen(Pen: TPen): PGpPen;
begin
Result := Pen.NativeObject;
end;

{!!-----------------------------------------------------------------------------
    TGraphics - public methods
-------------------------------------------------------------------------------}

class Function TGraphics.FromHDC(hDC: HDC): TGraphics;
begin
Result := TGraphics.CreateFromHDC(hDC);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TGraphics.FromHDC(hDC: HDC; hDevice: HANDLE): TGraphics;
begin
Result := TGraphics.CreateFromHDC(hDC,hDevice);
end;

//!!----------------------------------------------------------------------------

class Function TGraphics.FromHWND(hWnd: HWND; ICM: BOOL = False): TGraphics;
begin
Result := TGraphics.CreateFromHWND(hWnd,ICM);
end;

//!!----------------------------------------------------------------------------

class Function TGraphics.FromImage(Image: TImage): TGraphics;
begin
Result := TGraphics.CreateFromImage(Image);
end;

//!!----------------------------------------------------------------------------

constructor TGraphics.CreateFromHDC(hDC: HDC);
var
  Graphics: PGpGraphics;
begin
inherited Create;
Graphics := nil;
fLastResult := GdipCreateFromHDC(hDC,@Graphics);
SetNativeGraphics(Graphics);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGraphics.CreateFromHDC(hDC: HDC; hDevice: HANDLE);
var
  Graphics: PGpGraphics;
begin
inherited Create;
Graphics := nil;
fLastResult := GdipCreateFromHDC2(hDC,hDevice,@Graphics);
SetNativeGraphics(Graphics);
end;

//!!----------------------------------------------------------------------------

constructor TGraphics.CreateFromHWND(hWnd: HWND; ICM: BOOL = False);
var
  Graphics: PGpGraphics;
begin
inherited Create;
Graphics := nil;
If ICM then
  fLastResult := GdipCreateFromHWNDICM(hWnd,@Graphics)
else
  fLastResult := GdipCreateFromHWND(hWnd,@Graphics);
SetNativeGraphics(Graphics);
end;

//!!----------------------------------------------------------------------------

constructor TGraphics.CreateFromImage(Image: TImage);
var
  Graphics: PGpGraphics;
begin
inherited Create;
Graphics := nil;
If Assigned(Image) then
  fLastResult := GdipGetImageGraphicsContext(Image.NativeObject,@Graphics);
SetNativeGraphics(Graphics);
end;

//!!----------------------------------------------------------------------------

destructor TGraphics.Destroy;
begin
GdipDeleteGraphics(fNativeGraphics);
inherited;
end;

//!!----------------------------------------------------------------------------

procedure TGraphics.Flush(Intention: TFlushIntention = FlushIntentionFlush);
begin
GdipFlush(fNativeGraphics,Intention);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetHDC: HDC;
begin
SetStatus(GdipGetDC(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

procedure TGraphics.ReleaseHDC(hDC: HDC);
begin
SetStatus(GdipReleaseDC(fNativeGraphics,hDC));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetRenderingOrigin(X,Y: INT): TStatus;
begin
Result := SetStatus(GdipSetRenderingOrigin(fNativeGraphics,X,Y));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetRenderingOrigin(X,Y: PINT): TStatus;
begin
Result := SetStatus(GdipGetRenderingOrigin(fNativeGraphics,X,Y));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
begin
Result := SetStatus(GdipSetCompositingMode(fNativeGraphics,CompositingMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetCompositingMode: TCompositingMode;
begin
SetStatus(GdipGetCompositingMode(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
begin
Result := SetStatus(GdipSetCompositingQuality(fNativeGraphics,CompositingQuality));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetCompositingQuality: TCompositingQuality;
begin
SetStatus(GdipGetCompositingQuality(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetTextRenderingHint(NewMode: TTextRenderingHint): TStatus;
begin
Result := SetStatus(GdipSetTextRenderingHint(fNativeGraphics,NewMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
SetStatus(GdipGetTextRenderingHint(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetTextContrast(Contrast: UINT): TStatus;
begin
Result := SetStatus(GdipSetTextContrast(fNativeGraphics,Contrast));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetTextContrast: UINT;
begin
SetStatus(GdipGetTextContrast(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetInterpolationMode: TInterpolationMode;
begin
SetStatus(GdipGetInterpolationMode(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetInterpolationMode(InterpolationMode: TInterpolationMode): TStatus;
begin
Result := SetStatus(GdipSetInterpolationMode(fNativeGraphics,InterpolationMode));
end;

{$IF GDIPVER >= $0110}
//!!----------------------------------------------------------------------------

Function TGraphics.SetAbort(PIAbort: PGdiplusAbort): TStatus;
begin
Result := SetStatus(GdipGraphicsSetAbort(fNativeGraphics,PIAbort));
end;

{$IFEND}
//!!----------------------------------------------------------------------------

Function TGraphics.GetSmoothingMode: TSmoothingMode;
begin
SetStatus(GdipGetSmoothingMode(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetSmoothingMode(SmoothingMode: TSmoothingMode): TStatus;
begin
Result := SetStatus(GdipSetSmoothingMode(fNativeGraphics,SmoothingMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetPixelOffsetMode: TPixelOffsetMode;
begin
SetStatus(GdipGetPixelOffsetMode(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TStatus;
begin
Result := SetStatus(GdipSetPixelOffsetMode(fNativeGraphics,PixelOffsetMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipSetWorldTransform(fNativeGraphics,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.ResetTransform: TStatus;
begin
Result := SetStatus(GdipResetWorldTransform(fNativeGraphics));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.MultiplyTransform(Matrix: TMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipMultiplyWorldTransform(fNativeGraphics,Matrix.NativeObject,Order));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.TranslateTransform(DX,DY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipTranslateWorldTransform(fNativeGraphics,DX,DY,Order));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.ScaleTransform(SX,SY: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipScaleWorldTransform(fNativeGraphics,SX,SY,Order));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.RotateTransform(Angle: REAL; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
Result := SetStatus(GdipRotateWorldTransform(fNativeGraphics,Angle,Order));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetTransform(Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipGetWorldTransform(fNativeGraphics,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetPageUnit(PageUnit: TUnit): TStatus;
begin
Result := SetStatus(GdipSetPageUnit(fNativeGraphics,PageUnit));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetPageScale(Scale: REAL): TStatus;
begin
Result := SetStatus(GdipSetPageScale(fNativeGraphics,Scale));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetPageUnit: TUnit;
begin
SetStatus(GdipGetPageUnit(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetPageScale: REAL;
begin
SetStatus(GdipGetPageScale(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetDpiX: REAL;
begin
SetStatus(GdipGetDpiX(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetDpiY: REAL;
begin
SetStatus(GdipGetDpiY(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.TransformPoints(DestSpace,SrcSpace: TCoordinateSpace; Pts: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipTransformPoints(fNativeGraphics,DestSpace,SrcSpace,PGpPointF(Pts),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.TransformPoints(DestSpace,SrcSpace: TCoordinateSpace; Pts: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipTransformPointsI(fNativeGraphics,DestSpace,SrcSpace,PGpPoint(Pts),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetNearestColor(Color: PColor): TStatus;
var
  Argb: TARGB;
begin
If Assigned(Color) then
  begin
    Result := SetStatus(GdipGetNearestColor(fNativeGraphics,@Argb));
    SetValue(Color^,Argb);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawLine(Pen: TPen; X1,Y1,X2,Y2: REAL): TStatus;
begin
Result := SetStatus(GdipDrawLine(fNativeGraphics,Pen.NativeObject,X1,Y1,X2,Y2));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawLine(Pen: TPen; const Pt1,Pt2: TPointF): TStatus;
begin
Result := DrawLine(Pen,Pt1.X,Pt1.Y,Pt2.X,Pt2.Y);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawLines(Pen: TPen; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawLines(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawLine(Pen: TPen; X1,Y1,X2,Y2: INT): TStatus;
begin
Result := SetStatus(GdipDrawLineI(fNativeGraphics,Pen.NativeObject,X1,Y1,X2,Y2));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawLine(Pen: TPen; const Pt1,Pt2: TPoint): TStatus;
begin
Result := DrawLine(Pen,Pt1.X,Pt1.Y,Pt2.X,Pt2.Y);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawLines(Pen: TPen; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawLinesI(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawArc(Pen: TPen; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipDrawArc(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawArc(Pen: TPen; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := DrawArc(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawArc(Pen: TPen; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipDrawArcI(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawArc(Pen: TPen; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := DrawArc(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawBezier(Pen: TPen; X1,Y1,X2,Y2,X3,Y3,X4,Y4: REAL): TStatus;
begin
Result := SetStatus(GdipDrawBezier(fNativeGraphics,Pen.NativeObject,X1,Y1,X2,Y2,X3,Y3,X4,Y4));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawBezier(Pen: TPen; const Pt1,Pt2,Pt3,Pt4: TPointF): TStatus;
begin
Result := DrawBezier(Pen,Pt1.X,Pt1.Y,Pt2.X,Pt2.Y,Pt3.X,Pt3.Y,Pt4.X,Pt4.Y);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawBeziers(Pen: TPen; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawBeziers(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawBezier(Pen: TPen; X1,Y1,X2,Y2,X3,Y3,X4,Y4: INT): TStatus;
begin
Result := SetStatus(GdipDrawBezierI(fNativeGraphics,Pen.NativeObject,X1,Y1,X2,Y2,X3,Y3,X4,Y4));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawBezier(Pen: TPen; const Pt1,Pt2,Pt3,Pt4: TPoint): TStatus;
begin
Result := DrawBezier(Pen,Pt1.X,Pt1.Y,Pt2.X,Pt2.Y,Pt3.X,Pt3.Y,Pt4.X,Pt4.Y);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawBeziers(Pen: TPen; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawBeziersI(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawRectangle(Pen: TPen; const Rect: TRectF): TStatus;
begin
Result := DrawRectangle(PEn,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawRectangle(Pen: TPen; X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipDrawRectangle(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawRectangles(Pen: TPen; Rects: PRectF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawRectangles(fNativeGraphics,Pen.NativeObject,PGpRectF(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawRectangle(Pen: TPen; const Rect: TRect): TStatus;
begin
Result := DrawRectangle(PEn,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawRectangle(Pen: TPen; X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipDrawRectangleI(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawRectangles(Pen: TPen; Rects: PRect; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawRectanglesI(fNativeGraphics,Pen.NativeObject,PGpRect(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawEllipse(Pen: TPen; const Rect: TRectF): TStatus;
begin
Result := DrawEllipse(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawEllipse(Pen: TPen; X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipDrawEllipse(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawEllipse(Pen: TPen; const Rect: TRect): TStatus;
begin
Result := DrawEllipse(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawEllipse(Pen: TPen; X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipDrawEllipseI(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawPie(Pen: TPen; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := DrawPie(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawPie(Pen: TPen; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipDrawPie(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawPie(Pen: TPen; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := DrawPie(Pen,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawPie(Pen: TPen; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipDrawPieI(fNativeGraphics,Pen.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawPolygon(Pen: TPen; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawPolygon(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawPolygon(Pen: TPen; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawPolygonI(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawPath(Pen: TPen; Path: TGraphicsPath): TStatus;
begin
Result := SetStatus(GdipDrawPath(fNativeGraphics,Pen.NativeObject,Path.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawCurve(Pen: TPen; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawCurve(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawCurve(Pen: TPen; Points: PPointF; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipDrawCurve2(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawCurve(Pen: TPen; Points: PPointF; Count,Offset,NumberOfSegments: INT; Tension: REAL = 0.5): TStatus;
begin
Result := SetStatus(GdipDrawCurve3(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count,Offset,NumberOfSegments,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawCurve(Pen: TPen; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawCurveI(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawCurve(Pen: TPen; Points: PPoint; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipDrawCurve2I(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawCurve(Pen: TPen; Points: PPoint; Count,Offset,NumberOfSegments: INT; Tension: REAL = 0.5): TStatus;
begin
Result := SetStatus(GdipDrawCurve3I(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count,Offset,NumberOfSegments,Tension));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawClosedCurve(Pen: TPen; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawClosedCurve(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawClosedCurve(Pen: TPen; Points: PPointF; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipDrawClosedCurve2(fNativeGraphics,Pen.NativeObject,PGpPointF(Points),Count,Tension));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawClosedCurve(Pen: TPen; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipDrawClosedCurveI(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawClosedCurve(Pen: TPen; Points: PPoint; Count: INT; Tension: REAL): TStatus;
begin
Result := SetStatus(GdipDrawClosedCurve2I(fNativeGraphics,Pen.NativeObject,PGpPoint(Points),Count,Tension));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.Clear(const Color: TColor): TStatus;
begin
Result := SetStatus(GdipGraphicsClear(fNativeGraphics,GetValue(Color)));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillRectangle(Brush: TBrush; const Rect: TRectF): TStatus;
begin
Result := FillRectangle(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillRectangle(Brush: TBrush; X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipFillRectangle(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillRectangles(Brush: TBrush; Rects: PRectF; Count: INT): TStatus;
begin
Result := SetStatus(GdipFillRectangles(fNativeGraphics,Brush.NativeObject,PGpRectF(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillRectangle(Brush: TBrush; const Rect: TRect): TStatus;
begin
Result := FillRectangle(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillRectangle(Brush: TBrush; X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipFillRectangleI(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillRectangles(Brush: TBrush; Rects: PRect; Count: INT): TStatus;
begin
Result := SetStatus(GdipFillRectanglesI(fNativeGraphics,Brush.NativeObject,PGpRect(Rects),Count));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillPolygon(Brush: TBrush; Points: PPointF; Count: INT): TStatus;
begin
Result := FillPolygon(Brush,Points,Count,FillModeAlternate);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPolygon(Brush: TBrush; Points: PPointF; Count: INT; FillMode: TFillMode): TStatus;
begin
Result := SetStatus(GdipFillPolygon(fNativeGraphics,Brush.NativeObject,PGpPointF(Points),Count,FillMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPolygon(Brush: TBrush; Points: PPoint; Count: INT): TStatus;
begin
Result := FillPolygon(Brush,Points,Count,FillModeAlternate);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPolygon(Brush: TBrush; Points: PPoint; Count: INT; FillMode: TFillMode): TStatus;
begin
Result := SetStatus(GdipFillPolygonI(fNativeGraphics,Brush.NativeObject,PGpPoint(Points),Count,FillMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillEllipse(Brush: TBrush; const Rect: TRectF): TStatus;
begin
Result := FillEllipse(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillEllipse(Brush: TBrush; X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipFillEllipse(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillEllipse(Brush: TBrush; const Rect: TRect): TStatus;
begin
Result := FillEllipse(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillEllipse(Brush: TBrush; X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipFillEllipseI(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillPie(Brush: TBrush; const Rect: TRectF; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := FillPie(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPie(Brush: TBrush; X,Y,Width,Height,StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipFillPie(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPie(Brush: TBrush; const Rect: TRect; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := FillPie(Brush,Rect.X,Rect.Y,Rect.Width,Rect.Height,StartAngle,SweepAngle);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillPie(Brush: TBrush; X,Y,Width,Height: INT; StartAngle,SweepAngle: REAL): TStatus;
begin
Result := SetStatus(GdipFillPieI(fNativeGraphics,Brush.NativeObject,X,Y,Width,Height,StartAngle,SweepAngle));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillPath(Brush: TBrush; Path: TGraphicsPath): TStatus;
begin
Result := SetStatus(GdipFillPath(fNativeGraphics,Brush.NativeObject,Path.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillClosedCurve(Brush: TBrush; Points: PPointF; Count: INT): TStatus;
begin
Result := SetStatus(GdipFillClosedCurve(fNativeGraphics,Brush.NativeObject,PGpPointF(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillClosedCurve(Brush: TBrush; Points: PPointF; Count: INT; FillMode: TFillMode; Tension: REAL = 0.5): TStatus;
begin
Result := SetStatus(GdipFillClosedCurve2(fNativeGraphics,Brush.NativeObject,PGpPointF(Points),Count,Tension,FillMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillClosedCurve(Brush: TBrush; Points: PPoint; Count: INT): TStatus;
begin
Result := SetStatus(GdipFillClosedCurveI(fNativeGraphics,Brush.NativeObject,PGpPoint(Points),Count));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.FillClosedCurve(Brush: TBrush; Points: PPoint; Count: INT; FillMode: TFillMode; Tension: REAL = 0.5): TStatus;
begin
Result := SetStatus(GdipFillClosedCurve2I(fNativeGraphics,Brush.NativeObject,PGpPoint(Points),Count,Tension,FillMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.FillRegion(Brush: TBrush; Region: TRegion): TStatus;
begin
Result := SetStatus(GdipFillRegion(fNativeGraphics,Brush.NativeObject,Region.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  Brush: TBrush): TStatus;
begin
Result := SetStatus(GdipDrawString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,StringFormat.NativeObject,Brush.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  Brush: TBrush): TStatus;
begin
Result := DrawString(PWideChar(StrToWide(Str)),Length,Font,LayoutRect,StringFormat,Brush);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF;Brush: TBrush): TStatus;
var
  LayoutRect: TRectF;
begin
LayoutRect := RectF(Origin.X,Origin.Y,0.0,0.0);
Result := SetStatus(GdipDrawString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,nil,Brush.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; Brush: TBrush): TStatus;
begin
Result := DrawString(PWideChar(StrToWide(Str)),Length,Font,Origin,Brush);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
  Brush: TBrush): TStatus;
var
  LayoutRect: TRectF;
begin
LayoutRect := RectF(Origin.X,Origin.Y,0.0,0.0);
Result := SetStatus(GdipDrawString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,StringFormat.NativeObject,Brush.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
  Brush: TBrush): TStatus;
begin
Result := DrawString(PWideChar(StrToWide(Str)),Length,Font,Origin,StringFormat,Brush);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  BoundingBox: PRectF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus;
begin
Result := SetStatus(GdipMeasureString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,
  StringFormat.NativeObject,BoundingBox,CodepointsFitted,LinesFilled));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  BoundingBox: PRectF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus;
begin
Result := MeasureString(PwideChar(StrToWide(Str)),Length,Font,LayoutRect,StringFormat,BoundingBox,CodepointsFitted,LinesFilled);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRectSize: TSizeF; StringFormat: TStringFormat;
  Size: PSizeF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus;
var
  LayoutRect:     TRectF;
  BoundingBox:    TRectF;
  BoundingBoxPtr: PRectF;
begin
If Assigned(Size) then
  begin
    LayoutRect := RectF(0,0,LayoutRectSize.Width,LayoutRectSize.Height);
    BoundingBox := RectF(0,0,0,0);  //!! just some intialization so FPC is happy    
  {!!
    Following code seems to be superfluous, because the Size argument was
    already checked, but it is this way in the orginal source, so...

    ...or I have misunderstood the original. Also a posibility.
  }
    If Assigned(Size) then
      BoundingBoxPtr := @BoundingBox
    else
      BoundingBoxPtr := nil;
    Result := SetStatus(GdipMeasureString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,
      StringFormat.NativeObject,BoundingBoxPtr,CodepointsFitted,LinesFilled));
    If Assigned(Size) and (Result = Ok) then
      begin
        Size^.Width := BoundingBox.Width;
        Size^.Height := BoundingBox.Height;
      end;
  end
else Result := SetStatus(InvalidParameter);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRectSize: TSizeF; StringFormat: TStringFormat;
  Size: PSizeF; CodepointsFitted: PINT = nil; LinesFilled: PINT = nil): TStatus;
begin
Result := MeasureString(PwideChar(StrToWide(Str)),Length,Font,LayoutRectSize,StringFormat,Size,CodepointsFitted,LinesFilled);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
  BoundingBox: PRectF): TStatus;
var
  LayoutRect: TRectF;
begin
LayoutRect := RectF(Origin.X,Origin.Y,0.0,0.0);
Result := SetStatus(GdipMeasureString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,StringFormat.NativeObject,BoundingBox,nil,nil));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; StringFormat: TStringFormat;
  BoundingBox: PRectF): TStatus;
begin
Result := MeasureString(PwideChar(StrToWide(Str)),Length,Font,Origin,StringFormat,BoundingBox);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; BoundingBox: PRectF): TStatus;
begin
Result := SetStatus(GdipMeasureString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,nil,BoundingBox,nil,nil));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; BoundingBox: PRectF): TStatus;
begin
Result := MeasureString(PwideChar(StrToWide(Str)),Length,Font,LayoutRect,BoundingBox);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(Str: PWideChar; Length: INT; Font: TFont; const Origin: TPointF; BoundingBox: PRectF): TStatus;
var
  LayoutRect: TRectF;
begin
LayoutRect := RectF(Origin.X,Origin.Y,0.0,0.0);
Result := SetStatus(GdipMeasureString(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,nil,BoundingBox,nil,nil));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureString(const Str: String; Length: INT; Font: TFont; const Origin: TPointF; BoundingBox: PRectF): TStatus;
begin
Result := MeasureString(PwideChar(StrToWide(Str)),Length,Font,Origin,BoundingBox);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.MeasureCharacterRanges(Str: PWideChar; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  RegionCount: INT; Regions: PRegion): TStatus;
var
  NativeRegions:  array of PGpRegion;
  i:              Integer;
begin
If Assigned(Regions) and (RegionCount > 0) then
  begin
    NativeRegions := nil;
    SetLength(NativeRegions,RegionCount);
    If System.Length(NativeRegions) > 0 then  //!! conflict with Length argument
      begin
        For i := 0 to Pred(RegionCount) do
          NativeRegions[i] := PRegionArray(Regions)^[i].NativeObject;
        Result := SetStatus(GdipMeasureCharacterRanges(fNativeGraphics,Str,Length,Font.NativeObject,@LayoutRect,
          StringFormat.NativeObject,RegionCount,Pointer(NativeRegions)));
        SetLength(NativeRegions,0);
      end
    else Result := OutOfMemory;
  end
else Result := InvalidParameter;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.MeasureCharacterRanges(const Str: String; Length: INT; Font: TFont; const LayoutRect: TRectF; StringFormat: TStringFormat;
  RegionCount: INT; Regions: PRegion): TStatus; 
begin
Result := MeasureCharacterRanges(PWideChar(StrToWide(Str)),Length,Font,LayoutRect,StringFormat,RegionCount,Regions);
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawDriverString(Text: PUINT16; Length: INT; Font: TFont; Brush: TBrush; Positions: PPointF;
  Flags: INT; Matrix: TMatrix): TStatus;
begin
Result := SetStatus(GdipDrawDriverString(fNativeGraphics,Text,Length,Font.NativeObject,
  Brush.NativeObject,Positions,Flags,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.MeasureDriverString(Text: PUINT16; Length: INT; Font: TFont; Positions: PPointF; Flags: INT;
  Matrix: TMatrix; BoundingBox: PRectF): TStatus;
begin
Result := SetStatus(GdipMeasureDriverString(fNativeGraphics,Text,Length,Font.NativeObject,
  Positions,Flags,Matrix.NativeObject,BoundingBox));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawCachedBitmap(CB: TCachedBitmap; X,Y: INT): TStatus;
begin
Result := SetStatus(GdipDrawCachedBitmap(fNativeGraphics,CB.NativeObject,X,Y));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.DrawImage(Image: TImage; const Point: TPointF): TStatus;
begin
Result := DrawImage(Image,Point.X,Point.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y: REAL): TStatus;
begin
Result := SetStatus(GdipDrawImage(fNativeGraphics,Image.NativeObject,X,Y))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const Rect: TRectF): TStatus;
begin
Result := DrawImage(Image,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y,Width,Height: REAL): TStatus;
begin
Result := SetStatus(GdipDrawImageRect(fNativeGraphics,Image.NativeObject,X,Y,Width,Height))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const Point: TPoint): TStatus;
begin
Result := DrawImage(Image,Point.X,Point.Y);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y: INT): TStatus;
begin
Result := SetStatus(GdipDrawImageI(fNativeGraphics,Image.NativeObject,X,Y))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const Rect: TRect): TStatus;
begin
Result := DrawImage(Image,Rect.X,Rect.Y,Rect.Width,Rect.Height);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y,Width,Height: INT): TStatus;
begin
Result := SetStatus(GdipDrawImageRectI(fNativeGraphics,Image.NativeObject,X,Y,Width,Height))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; DestPoints: PPointF; Count: INT): TStatus;
begin
If Count in [3,4] then
  Result := SetStatus(GdipDrawImagePoints(fNativeGraphics,Image.NativeObject,PGpPointF(DestPoints),Count))
else
  Result := SetStatus(InvalidParameter);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; DestPoints: PPoint; Count: INT): TStatus;
begin
If Count in [3,4] then
  Result := SetStatus(GdipDrawImagePointsI(fNativeGraphics,Image.NativeObject,PGpPoint(DestPoints),Count))
else
  Result := SetStatus(InvalidParameter);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y,SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit): TStatus;
begin
Result := SetStatus(GdipDrawImagePointRect(fNativeGraphics,Image.NativeObject,X,Y,SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const DestRect: TRectF; SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit;
  ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus;
begin
Result := SetStatus(GdipDrawImageRectRect(fNativeGraphics,Image.NativeObject,DestRect.X,DestRect.Y,DestRect.Width,DestRect.Height,
  SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit,ImageAttributes.NativeObject,Callback,CallbackData));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; DestPoints: PPointF; Count: INT; SrcX,SrcY,SrcWidth,SrcHeight: REAL; SrcUnit: TUnit;
  ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus;
begin
Result := SetStatus(GdipDrawImagePointsRect(fNativeGraphics,Image.NativeObject,PGpPointF(DestPoints),Count,
  SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit,ImageAttributes.NativeObject,Callback,CallbackData));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; X,Y,SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit): TStatus;
begin
Result := SetStatus(GdipDrawImagePointRectI(fNativeGraphics,Image.NativeObject,X,Y,SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const DestRect: TRect; SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit;
  ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus;
begin
Result := SetStatus(GdipDrawImageRectRectI(fNativeGraphics,Image.NativeObject,DestRect.X,DestRect.Y,DestRect.Width,DestRect.Height,
  SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit,ImageAttributes.NativeObject,Callback,CallbackData));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; DestPoints: PPoint; Count: INT; SrcX,SrcY,SrcWidth,SrcHeight: INT; SrcUnit: TUnit;
  ImageAttributes: TImageAttributes = nil; Callback: TDrawImageAbort = nil; CallbackData: Pointer = nil): TStatus;
begin
Result := SetStatus(GdipDrawImagePointsRectI(fNativeGraphics,Image.NativeObject,PGpPoint(DestPoints),Count,
  SrcX,SrcY,SrcWidth,SrcHeight,SrcUnit,ImageAttributes.NativeObject,Callback,CallbackData));
end;

{$IF GDIPVER >= $0110}
//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const DestRect,SourceRect: TRectF; SrcUnit: TUnit;
  ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipDrawImageRectRect(fNativeGraphics,Image.NativeObject,DestRect.X,DestRect.Y,DestRect.Width,DestRect.Height,
  SourceRect.X,SourceRect.Y,SourceRect.Width,SourceRect.Height,SrcUnit,ImageAttributes.NativeObject,nil,nil))
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.DrawImage(Image: TImage; const SourceRect: TRectF; XForm: TMatrix; Effect: TEffect;
  ImageAttributes: TImageAttributes; SrcUnit: TUnit): TStatus;
begin
Result := SetStatus(GdipDrawImageFX(fNativeGraphics,Image.NativeObject,@SourceRect,XForm.NativeObject,
  Effect.NativeObject,ImageAttributes.NativeObject,SrcUnit));
end;

{$IFEND}
//!!----------------------------------------------------------------------------

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPointF; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestPoint(fNativeGraphics,MetaFile.NativeObject,
  @DestPoint,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPoint; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestPointI(fNativeGraphics,MetaFile.NativeObject,
  @DestPoint,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestRect: TRectF; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestRect(fNativeGraphics,MetaFile.NativeObject,
  @DestRect,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestRect: TRect; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestRectI(fNativeGraphics,MetaFile.NativeObject,
  @DestRect,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPointF; Count: INT; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestPoints(fNativeGraphics,MetaFile.NativeObject,
  DestPoints,Count,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPoint; Count: INT; Callback: TEnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileDestPointsI(fNativeGraphics,MetaFile.NativeObject,
  DestPoints,Count,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPointF; const SrcRect: TRectF; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoint(fNativeGraphics,MetaFile.NativeObject,
  @DestPoint,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;
  
//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestPoint: TPoint; const SrcRect: TRect; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointI(fNativeGraphics,MetaFile.NativeObject,
  @DestPoint,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestRect,SrcRect: TRectF; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestRect(fNativeGraphics,MetaFile.NativeObject,
  @DestRect,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; const DestRect,SrcRect: TRect; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestRectI(fNativeGraphics,MetaFile.NativeObject,
  @DestRect,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPointF; Count: INT; const SrcRect: TRectF; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoints(fNativeGraphics,MetaFile.NativeObject,
  DestPoints,Count,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.EnumerateMetafile(MetaFile: TMetafile; DestPoints: PPoint; Count: INT; const SrcRect: TRect; SrcUnit: TUnit;
  Callback: TEnumerateMetafileProc; CallbackData: Pointer = nil; ImageAttributes: TImageAttributes = nil): TStatus;
begin
Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointsI(fNativeGraphics,MetaFile.NativeObject,
  DestPoints,Count,@SrcRect,SrcUnit,Callback,CallbackData,ImageAttributes.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.SetClip(G: TGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipGraphics(fNativeGraphics,G.NativeObject,CombineMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.SetClip(const Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipRect(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.SetClip(const Rect: TRect; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipRectI(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.SetClip(Path: TGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipPath(fNativeGraphics,Path.NativeObject,CombineMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.SetClip(Region: TRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipRegion(fNativeGraphics,Region.NativeObject,CombineMode));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.SetClip(HRgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
Result := SetStatus(GdipSetClipHrgn(fNativeGraphics,HRgn,CombineMode));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.IntersectClip(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipSetClipRect(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineModeIntersect));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IntersectClip(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipSetClipRectI(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineModeIntersect));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IntersectClip(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipSetClipRegion(fNativeGraphics,Region.NativeObject,CombineModeIntersect));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.ExcludeClip(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipSetClipRect(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineModeExclude));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.ExcludeClip(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipSetClipRectI(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,CombineModeExclude));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.ExcludeClip(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipSetClipRegion(fNativeGraphics,Region.NativeObject,CombineModeExclude));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.ResetClip: TStatus;
begin
Result := SetStatus(GdipResetClip(fNativeGraphics));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.TranslateClip(DX,DY: REAL): TStatus;
begin
Result := SetStatus(GdipTranslateClip(fNativeGraphics,DX,DY));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.TranslateClip(DX,DY: INT): TStatus;
begin
Result := SetStatus(GdipTranslateClipI(fNativeGraphics,DX,DY));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetClip(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipGetClip(fNativeGraphics,Region.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetClipBounds(Rect: PRectF): TStatus;
begin
Result := SetStatus(GdipGetClipBounds(fNativeGraphics,PGpRectF(Rect)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.GetClipBounds(Rect: PRect): TStatus;
begin
Result := SetStatus(GdipGetClipBoundsI(fNativeGraphics,PGpRect(Rect)));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.IsClipEmpty: BOOL;
begin
SetStatus(GdipIsClipEmpty(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetVisibleClipBounds(Rect: PRectF): TStatus;
begin
Result := SetStatus(GdipGetVisibleClipBounds(fNativeGraphics,PGpRectF(Rect)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.GetVisibleClipBounds(Rect: PRect): TStatus;
begin
Result := SetStatus(GdipGetVisibleClipBoundsI(fNativeGraphics,PGpRect(Rect)));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.IsVisibleClipEmpty: BOOL;
begin
SetStatus(GdipIsVisibleClipEmpty(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.IsVisible(X,Y: INT): BOOL;
begin
Result := IsVisible(PointI(X,Y));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(const Point: TPoint): BOOL;
begin
SetStatus(GdipIsVisiblePointI(fNativeGraphics,Point.X,Point.Y,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(X,Y,Width,Height: INT): BOOL;
begin
Result := IsVisible(RectI(X,Y,Width,Height));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(const Rect: TRect): BOOL;
begin
SetStatus(GdipIsVisibleRectI(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(X,Y: REAL): BOOL;
begin
Result := IsVisible(PointF(X,Y));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(const Point: TPointF): BOOL;
begin
SetStatus(GdipIsVisiblePoint(fNativeGraphics,Point.X,Point.Y,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(X,Y,Width,Height: REAL): BOOL;
begin
Result := IsVisible(RectF(X,Y,Width,Height));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.IsVisible(const Rect: TRectF): BOOL;
begin
SetStatus(GdipIsVisibleRect(fNativeGraphics,Rect.X,Rect.Y,Rect.Width,Rect.Height,@Result));
end;
//!!----------------------------------------------------------------------------

Function TGraphics.Save: TGraphicsState;
begin
SetStatus(GdipSaveGraphics(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.Restore(GState: TGraphicsState): TStatus;
begin
Result := SetStatus(GdipRestoreGraphics(fNativeGraphics,GState));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.BeginContainer(const DstRect,SrcRect: TRectF; aUnit: TUnit): TGraphicsContainer;
begin
SetStatus(GdipBeginContainer(fNativeGraphics,@DstRect,@SrcRect,aUnit,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.BeginContainer(const DstRect,SrcRect: TRect; aUnit: TUnit): TGraphicsContainer;
begin
SetStatus(GdipBeginContainerI(fNativeGraphics,@DstRect,@SrcRect,aUnit,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TGraphics.BeginContainer: TGraphicsContainer;
begin
SetStatus(GdipBeginContainer2(fNativeGraphics,@Result));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.EndContainer(State: TGraphicsContainer): TStatus;
begin
Result := SetStatus(GdipEndContainer(fNativeGraphics,State));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.AddMetafileComment(Data: PBYTE; SizeData: UINT): TStatus;
begin
Result := SetStatus(GdipComment(fNativeGraphics,SizeData,Data));
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetHalftonePalette: HPALETTE;
begin
Result := GdipCreateHalftonePalette;
end;

//!!----------------------------------------------------------------------------

Function TGraphics.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusmetafile.h
*******************************************************************************}
{!!=============================================================================
    TMetafile - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TMetafile - protected methods
-------------------------------------------------------------------------------}

constructor TMetafile.Create;
begin
inherited Create;
SetNativeImage(nil);
fLastResult := Ok;
end;

{!!-----------------------------------------------------------------------------
    TMetafile - public methods
-------------------------------------------------------------------------------}

constructor TMetafile.Create(HWmf: HMETAFILE; WmfPlaceableFileHeader: PWmfPlaceableFileHeader; DeleteWmf: BOOL = FALSE);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipCreateMetafileFromWmf(HWmf,DeleteWmf,WmfPlaceableFileHeader,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(HEmf: HENHMETAFILE; DeleteWmf: BOOL = FALSE);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipCreateMetafileFromEmf(HEmf,DeleteWmf,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Filename: PWideChar);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipCreateMetafileFromFile(Filename,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(const Filename: String);
begin
Create(PWideChar(StrToWide(Filename)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Filename: PWideChar; WmfPlaceableFileHeader: PWmfPlaceableFileHeader);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipCreateMetafileFromWmfFile(Filename,WmfPlaceableFileHeader,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(const Filename: String; WmfPlaceableFileHeader: PWmfPlaceableFileHeader);
begin
Create(PWideChar(StrToWide(Filename)),WmfPlaceableFileHeader);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipCreateMetafileFromStream(Stream,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream);
begin
Create(IStream(TStreamAdapter.Create(Stream)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafile(ReferenceHDC,EmfType,nil,MetafileFrameUnitGdi,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; EmfType: TEmfType; const Description: String);
begin
Create(ReferenceHdc,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafile(ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileI(ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
constructor TMetafile.Create(Filename: PWideChar; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileFileName(Filename,ReferenceHDC,EmfType,nil,MetafileFrameUnitGdi,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(const Filename: String; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String);
begin
Create(PWideChar(StrToWide(Filename)),ReferenceHdc,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Filename: PWideChar; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileFileName(Filename,ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(const Filename: String; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(PWideChar(StrToWide(Filename)),ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Filename: PWideChar; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileFileNameI(Filename,ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(const Filename: String; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(PWideChar(StrToWide(Filename)),ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileStream(Stream,ReferenceHDC,EmfType,nil,MetafileFrameUnitGdi,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,EmfType,Description);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String);
begin
Create(Stream,ReferenceHdc,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; EmfType: TEmfType; const Description: String);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,EmfType,PWideChar(StrToWide(Description)));
end;      

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileStream(Stream,ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,FrameRect,FrameUnit,EmfType,Description);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(Stream,ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRectF; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
var
  Metafile: PGpMetafile;
begin
Create;
Metafile := nil;
fLastResult := GdipRecordMetafileStreamI(Stream,ReferenceHDC,EmfType,@FrameRect,FrameUnit,Description,@MetaFile);
SetNativeImage(PGpImage(MetaFile));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  EmfType: TEmfType = EmfTypeEmfPlusDual; Description: PWideChar = nil);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,FrameRect,FrameUnit,EmfType,Description);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: IStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(Stream,ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMetafile.Create(Stream: TStream; ReferenceHdc: HDC; const FrameRect: TRect; FrameUnit: TMetafileFrameUnit; EmfType: TEmfType;
  const Description: String);
begin
Create(IStream(TStreamAdapter.Create(Stream)),ReferenceHdc,FrameRect,FrameUnit,EmfType,PWideChar(StrToWide(Description)));
end;

//!!----------------------------------------------------------------------------

class Function TMetafile.GetMetafileHeader(HWmf: HMETAFILE; WmfPlaceableFileHeader: PWmfPlaceableFileHeader; Header: PMetafileHeader): TStatus;
begin
Result := GdipGetMetafileHeaderFromWmf(HWmf,WmfPlaceableFileHeader,Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TMetafile.GetMetafileHeader(HEmf: HENHMETAFILE; Header: PMetafileHeader): TStatus;
begin
Result := GdipGetMetafileHeaderFromEmf(HEmf,Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TMetafile.GetMetafileHeader(Filename: PWideChar; Header: PMetafileHeader): TStatus;
begin
Result := GdipGetMetafileHeaderFromFile(Filename,Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TMetafile.GetMetafileHeader(const Filename: String; Header: PMetafileHeader): TStatus;
begin
Result := GetMetafileHeader(PWideChar(StrToWide(FileName)),Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TMetafile.GetMetafileHeader(Stream: IStream; Header: PMetafileHeader): TStatus;
begin
Result := GdipGetMetafileHeaderFromStream(Stream,Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TMetafile.GetMetafileHeader(Stream: TStream; Header: PMetafileHeader): TStatus;
begin
Result := GetMetafileHeader(IStream(TStreamAdapter.Create(Stream)),Header);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.GetMetafileHeader(Header: PMetafileHeader): TStatus;
begin
Result := GdipGetMetafileHeaderFromMetafile(PGpMetafile(fNativeImage),Header);
end;

//!!----------------------------------------------------------------------------

Function TMetafile.GetHENHMETAFILE: HENHMETAFILE;
begin
SetStatus(GdipGetHemfFromMetafile(PGpMetafile(fNativeImage),@Result));
end;

//!!----------------------------------------------------------------------------

Function TMetafile.PlayRecord(RecordType: TEmfPlusRecordType; Flags,DataSize: UINT; Data: PBYTE): TStatus;
begin
Result := SetStatus(GdipPlayMetafileRecord(PGpMetafile(fNativeImage),RecordType,Flags,DataSize,Data));
end;

//!!----------------------------------------------------------------------------

Function TMetafile.SetDownLevelRasterizationLimit(MetafileRasterizationLimitDpi: UINT): TStatus;
begin
Result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit(PGpMetafile(fNativeImage),MetafileRasterizationLimitDpi));
end;

//!!----------------------------------------------------------------------------

Function TMetafile.GetDownLevelRasterizationLimit: UINT;
begin
SetStatus(GdipGetMetafileDownLevelRasterizationLimit(PGpMetafile(fNativeImage),@Result));
end;

//!!----------------------------------------------------------------------------

class Function TMetafile.EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT16; pData16: LPBYTE; iMapMode: INT = MM_ANISOTROPIC;
  eFlags: INT = EmfToWmfBitsFlagsDefault): UINT;
begin
Result := GdipEmfToWmfBits(hEmf,cbData16,pData16,iMapMode,eFlags);
end;

{$IF GDIPVER >= $0110}
//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; ConversionFailureFlag: PINT = nil;
  EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus;
var
  Metafile: PGpMetafile;
begin
Metafile := nil;
Result := GdipConvertToEmfPlus(RefGraphics.NativeObject,PGpMetafile(fNativeImage),
  ConversionFailureFlag,EmfType,Description,@Metafile);
If Assigned(Metafile) then
  begin
    If Result = Ok then
      begin
        GdipDisposeImage(fNativeImage);
        SetNativeImage(PGpImage(Metafile));
      end
    else GdipDisposeImage(PGpImage(Metafile));
  end;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; ConversionFailureFlag: PINT;
  EmfType: TEmfType; const Description: String): TStatus;
begin
Result := ConvertToEmfPlus(RefGraphics,ConversionFailureFlag,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; Filename: PWideChar; ConversionFailureFlag: PINT = nil;
  EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus;
var
  Metafile: PGpMetafile;
begin
Metafile := nil;
Result := GdipConvertToEmfPlusToFile(RefGraphics.NativeObject,PGpMetafile(fNativeImage),
  ConversionFailureFlag,Filename,EmfType,Description,@Metafile);
If Assigned(Metafile) then
  begin
    If Result = Ok then
      begin
        GdipDisposeImage(fNativeImage);
        SetNativeImage(PGpImage(Metafile));
      end
    else GdipDisposeImage(PGpImage(Metafile));
  end;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; const Filename: String; ConversionFailureFlag: PINT;
  EmfType: TEmfType; const Description: String): TStatus;
begin
Result := ConvertToEmfPlus(RefGraphics,PWideChar(StrToWide(Filename)),ConversionFailureFlag,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: IStream; ConversionFailureFlag: PINT = nil;
  EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus;
var
  Metafile: PGpMetafile;
begin
Metafile := nil;
Result := GdipConvertToEmfPlusToStream(RefGraphics.NativeObject,PGpMetafile(fNativeImage),
  ConversionFailureFlag,Stream,EmfType,Description,@Metafile);
If Assigned(Metafile) then
  begin
    If Result = Ok then
      begin
        GdipDisposeImage(fNativeImage);
        SetNativeImage(PGpImage(Metafile));
      end
    else GdipDisposeImage(PGpImage(Metafile));
  end;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: TStream; ConversionFailureFlag: PINT = nil;
  EmfType: TEmfType = EmfTypeEmfPlusOnly; Description: PWideChar = nil): TStatus;
begin
Result := ConvertToEmfPlus(RefGraphics,IStream(TStreamAdapter.Create(Stream)),ConversionFailureFlag,EmfType,Description);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: IStream; ConversionFailureFlag: PINT;
  EmfType: TEmfType; const Description: String): TStatus;
begin
Result := ConvertToEmfPlus(RefGraphics,Stream,ConversionFailureFlag,EmfType,PWideChar(StrToWide(Description)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMetafile.ConvertToEmfPlus(RefGraphics: TGraphicsBase; Stream: TStream; ConversionFailureFlag: PINT;
  EmfType: TEmfType; const Description: String): TStatus;
begin
Result := ConvertToEmfPlus(RefGraphics,IStream(TStreamAdapter.Create(Stream)),ConversionFailureFlag,EmfType,PWideChar(StrToWide(Description)));
end;

{$IFEND}


{!!*****************************************************************************
    gdipluscachedbitmap.h
*******************************************************************************}
{!!=============================================================================
    TCachedBitmap - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TCachedBitmap - protected methods
-------------------------------------------------------------------------------}

Function TCachedBitmap.GetNativeObject: Pointer;
begin
Result := fNativeCachedBitmap;
end;

//!!----------------------------------------------------------------------------

Function TCachedBitmap.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeCachedBitmap);
end;

{!!-----------------------------------------------------------------------------
    TCachedBitmap - public methods
-------------------------------------------------------------------------------}

constructor TCachedBitmap.Create(Bitmap: TBitmap; Graphics: TGraphicsBase);
begin
inherited Create;
fNativeCachedBitmap := nil;
fLastResult := GdipCreateCachedBitmap(Bitmap.NativeObject,Graphics.NativeObject,@fNativeCachedBitmap);
end;

//!!----------------------------------------------------------------------------

destructor TCachedBitmap.Destroy;
begin
GdipDeleteCachedBitmap(fNativeCachedBitmap);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TCachedBitmap.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusregion.h
*******************************************************************************}
{!!=============================================================================
    TRegion - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TRegion - protected methods
-------------------------------------------------------------------------------}

Function TRegion.GetNativeObject: Pointer;
begin
Result := fNativeRegion;
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeRegion);
end;

//!!----------------------------------------------------------------------------

Function TRegion.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

//!!----------------------------------------------------------------------------

constructor TRegion.Create(NativeRegion: PGpRegion);
begin
inherited Create;
SetNativeRegion(NativeRegion);
end;

//!!----------------------------------------------------------------------------

procedure TRegion.SetNativeRegion(NewNativeRegion: PGpRegion);
begin
fNativeRegion := NewNativeRegion;
end;

{!!-----------------------------------------------------------------------------
    TRegion - public methods
-------------------------------------------------------------------------------}

constructor TRegion.Create;
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegion(@Region);
SetNativeRegion(Region);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegion.Create(const Rect: TRectF);
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegionRect(@Rect,@Region);
SetNativeRegion(Region);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegion.Create(const Rect: TRect);
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegionRectI(@Rect,@Region);
SetNativeRegion(Region);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegion.Create(Path: TGraphicsPathBase);
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegionPath(Path.NativeObject,@Region);
SetNativeRegion(Region);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegion.Create(RegionData: PBYTE; Size: INT);
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegionRgnData(RegionData,Size,@Region);
SetNativeRegion(Region);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegion.Create(HRgn: HRGN);
var
  Region: PGpRegion;
begin
inherited Create;
Region := nil;
fLastResult := GdipCreateRegionHrgn(HRgn,@Region);
SetNativeRegion(Region);
end;

//!!----------------------------------------------------------------------------

class Function TRegion.FromHRGN(HRgn: HRGN): TRegion;
var
  Region: PGpRegion;
begin
Region := nil;
If GdipCreateRegionHrgn(HRgn,@Region) = Ok then
  begin
    Result := TRegion.Create(Region);
    If not Assigned(Result) then
      GdipDeleteRegion(Region);
  end
else Result := nil;
end;

//!!----------------------------------------------------------------------------

destructor TRegion.Destroy;
begin
GdipDeleteRegion(fNativeRegion);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TRegion.Clone: TRegion;
var
  Region: PGpRegion;
begin
Region := nil;
SetStatus(GdipCloneRegion(fNativeRegion,@Region));
Result := TRegion.Create(Region);
end;

//!!----------------------------------------------------------------------------

Function TRegion.MakeInfinite: TStatus;
begin
Result := SetStatus(GdipSetInfinite(fNativeRegion));
end;

//!!----------------------------------------------------------------------------

Function TRegion.MakeEmpty: TStatus;
begin
Result := SetStatus(GdipSetEmpty(fNativeRegion));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetDataSize: UINT;
begin
Result := 0;
SetStatus(GdipGetRegionDataSize(fNativeRegion,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetData(Buffer: PBYTE; BufferSize: UINT; SizeFilled: PUINT = nil): TStatus;
begin
Result := SetStatus(GdipGetRegionData(fNativeRegion,Buffer,BufferSize,SizeFilled));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Intersect(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipCombineRegionRect(fNativeRegion,@Rect,CombineModeIntersect));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Intersect(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipCombineRegionRectI(fNativeRegion,@Rect,CombineModeIntersect));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Intersect(Path: TGraphicsPathBase): TStatus;
begin
Result := SetStatus(GdipCombineRegionPath(fNativeRegion,Path.NativeObject,CombineModeIntersect));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Intersect(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipCombineRegionRegion(fNativeRegion,Region.NativeObject,CombineModeIntersect));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Union(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipCombineRegionRect(fNativeRegion,@Rect,CombineModeUnion));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Union(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipCombineRegionRectI(fNativeRegion,@Rect,CombineModeUnion));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Union(Path: TGraphicsPathBase): TStatus;
begin
Result := SetStatus(GdipCombineRegionPath(fNativeRegion,Path.NativeObject,CombineModeUnion));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Union(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipCombineRegionRegion(fNativeRegion,Region.NativeObject,CombineModeUnion));
end;

//!!----------------------------------------------------------------------------

Function TRegion.ExclusiveOR(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipCombineRegionRect(fNativeRegion,@Rect,CombineModeXor));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.ExclusiveOR(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipCombineRegionRectI(fNativeRegion,@Rect,CombineModeXor));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.ExclusiveOR(Path: TGraphicsPathBase): TStatus;
begin
Result := SetStatus(GdipCombineRegionPath(fNativeRegion,Path.NativeObject,CombineModeXor));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.ExclusiveOR(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipCombineRegionRegion(fNativeRegion,Region.NativeObject,CombineModeXor));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Exclude(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipCombineRegionRect(fNativeRegion,@Rect,CombineModeExclude));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Exclude(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipCombineRegionRectI(fNativeRegion,@Rect,CombineModeExclude));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Exclude(Path: TGraphicsPathBase): TStatus;
begin
Result := SetStatus(GdipCombineRegionPath(fNativeRegion,Path.NativeObject,CombineModeExclude));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Exclude(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipCombineRegionRegion(fNativeRegion,Region.NativeObject,CombineModeExclude));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Complement(const Rect: TRect): TStatus;
begin
Result := SetStatus(GdipCombineRegionRect(fNativeRegion,@Rect,CombineModeComplement));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Complement(const Rect: TRectF): TStatus;
begin
Result := SetStatus(GdipCombineRegionRectI(fNativeRegion,@Rect,CombineModeComplement));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Complement(Path: TGraphicsPathBase): TStatus;
begin
Result := SetStatus(GdipCombineRegionPath(fNativeRegion,Path.NativeObject,CombineModeComplement));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Complement(Region: TRegion): TStatus;
begin
Result := SetStatus(GdipCombineRegionRegion(fNativeRegion,Region.NativeObject,CombineModeComplement));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Translate(DX,DY: REAL): TStatus;
begin
Result := SetStatus(GdipTranslateRegion(fNativeRegion,DX,DY));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.Translate(DX,DY: INT): TStatus;
begin
Result := SetStatus(GdipTranslateRegionI(fNativeRegion,DX,DY));
end;

//!!----------------------------------------------------------------------------

Function TRegion.Transform(Matrix: TMatrixBase): TStatus;
begin
Result := SetStatus(GdipTransformRegion(fNativeRegion,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetBounds(Rect: PRect; G: TGraphicsBase): TStatus;
begin
Result := SetStatus(GdipGetRegionBoundsI(fNativeRegion,G.NativeObject,PGpRect(Rect)));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.GetBounds(Rect: PRectF; G: TGraphicsBase): TStatus;
begin
Result := SetStatus(GdipGetRegionBounds(fNativeRegion,G.NativeObject,PGpRectF(Rect)));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetHRGN(G: TGraphicsBase): HRGN;
begin
SetStatus(GdipGetRegionHRgn(fNativeRegion,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.IsEmpty(G: TGraphicsBase): BOOL;
begin
Result := False;
SetStatus(GdipIsEmptyRegion(fNativeRegion,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.IsInfinite(G: TGraphicsBase): BOOL;
begin
Result := False;
SetStatus(GdipIsInfiniteRegion(fNativeRegion,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.IsVisible(X,Y: INT; G: TGraphicsBase = nil): BOOL;
begin
Result := IsVisible(PointI(X,Y),G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(const Point: TPoint; G: TGraphicsBase = nil): BOOL;
begin
Result := False;
SetStatus(GdipIsVisibleRegionPointI(fNativeRegion,Point.X,Point.Y,G.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(X,Y: REAL; G: TGraphicsBase = nil): BOOL;
begin
Result := IsVisible(PointF(X,Y),G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(const Point: TPointF; G: TGraphicsBase = nil): BOOL;
begin
Result := False;
SetStatus(GdipIsVisibleRegionPoint(fNativeRegion,Point.X,Point.Y,G.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(X,Y,Width,Height: INT; G: TGraphicsBase): BOOL;
begin
Result := IsVisible(RectI(X,Y,Width,Height),G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(const Rect: TRect; G: TGraphicsBase = nil): BOOL;
begin
Result := False;
SetStatus(GdipIsVisibleRegionRectI(fNativeRegion,Rect.X,Rect.Y,Rect.Width,Rect.Height,G.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(X,Y,Width,Height: REAL; G: TGraphicsBase = nil): BOOL;
begin
Result := IsVisible(RectF(X,Y,Width,Height),G);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.IsVisible(const Rect: TRectF; G: TGraphicsBase = nil): BOOL;
begin
Result := False;
SetStatus(GdipIsVisibleRegionRect(fNativeRegion,Rect.X,Rect.Y,Rect.Width,Rect.Height,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.IsEqual(Region: TRegion; G: TGraphicsBase): BOOL;
begin
Result := False;
SetStatus(GdipIsEqualRegion(fNativeRegion,Region.NativeObject,G.NativeObject,@Result));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetRegionScansCount(Matrix: TMatrixBase): UINT;
begin
Result := 0;
SetStatus(GdipGetRegionScansCount(fNativeRegion,@Result,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetRegionScans(Matrix: TMatrixBase; Rects: PRectF; Count: PINT): TStatus;
begin
Result := SetStatus(GdipGetRegionScans(fNativeRegion,PGpRectF(Rects),Count,Matrix.NativeObject));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegion.GetRegionScans(Matrix: TMatrixBase; Rects: PRect; Count: PINT): TStatus;
begin
Result := SetStatus(GdipGetRegionScansI(fNativeRegion,PGpRect(Rects),Count,Matrix.NativeObject));
end;

//!!----------------------------------------------------------------------------

Function TRegion.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusfontcollection.h
*******************************************************************************}
{!!=============================================================================
    TFontCollection - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TFontCollection - protected methods
-------------------------------------------------------------------------------}

Function TFontCollection.GetNativeObject: Pointer;
begin
Result := fNativeFontCollection;
end;

//!!----------------------------------------------------------------------------

Function TFontCollection.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeFontCollection);
end;

//!!----------------------------------------------------------------------------

Function TFontCollection.SetStatus(Status: TStatus): TStatus;
begin
fLastResult := Status;
Result := fLastResult;
end;

{!!-----------------------------------------------------------------------------
    TFontCollection - public methods
-------------------------------------------------------------------------------}

constructor TFontCollection.Create;
begin
inherited Create;
fNativeFontCollection := nil;
end;

//!!----------------------------------------------------------------------------

Function TFontCollection.GetFamilyCount: INT;
begin
Result := 0;
fLastResult := GdipGetFontCollectionFamilyCount(fNativeFontCollection,@Result);
end;

//!!----------------------------------------------------------------------------

Function TFontCollection.GetFamilies(NumSought: INT; Gpfamilies: PFontFamily; NumFound: PINT): TStatus;
var
  NativeFamilyList: array of PGpFontFamily;
  i:                Integer;
begin
If (NumSought > 0) and Assigned(Gpfamilies) and Assigned(NumFound) then
  begin
    NumFound^ := 0;
    NativeFamilyList := nil;
    SetLength(NativeFamilyList,NumSought);
    If Length(NativeFamilyList) > 0 then
      begin
        Result := SetStatus(GdipGetFontCollectionFamilyList(fNativeFontCollection,
          NumSought,Pointer(NativeFamilyList),NumFound));
        If Result = Ok then
          For i := 0 to Pred(NumFound^) do
            GdipCloneFontFamily(NativeFamilyList[i],PFontFamilyArray(Gpfamilies)^[i].NativeObjectAddr);
        SetLength(NativeFamilyList,0);  
      end
    else Result := SetStatus(OutOfMemory);
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TFontCollection.GetLastStatus: TStatus;
begin
Result := fLastResult;
end;


{!!=============================================================================
    TInstalledFontCollection - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TInstalledFontCollection - public methods
-------------------------------------------------------------------------------}

constructor TInstalledFontCollection.Create;
begin
inherited Create;
fNativeFontCollection := nil;
fLastResult := GdipNewInstalledFontCollection(@fNativeFontCollection);
end;


{!!=============================================================================
    TPrivateFontCollection - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TPrivateFontCollection - public methods
-------------------------------------------------------------------------------}

constructor TPrivateFontCollection.Create;
begin
inherited Create;
fNativeFontCollection := nil;
fLastResult := GdipNewPrivateFontCollection(@fNativeFontCollection);
end;

//!!----------------------------------------------------------------------------

destructor TPrivateFontCollection.Destroy;
begin
GdipDeletePrivateFontCollection(@fNativeFontCollection);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TPrivateFontCollection.AddFontFile(Filename: PWideChar): TStatus;
begin
Result := SetStatus(GdipPrivateAddFontFile(fNativeFontCollection,Filename));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TPrivateFontCollection.AddFontFile(const Filename: String): TStatus;
begin
Result := AddFontFile(PWideChar(StrToWide(Filename)));
end;

//!!----------------------------------------------------------------------------

Function TPrivateFontCollection.AddMemoryFont(Memory: Pointer; Length: INT): TStatus;
begin
Result := SetStatus(GdipPrivateAddMemoryFont(fNativeFontCollection,Memory,Length));
end;


{!!*****************************************************************************
    gdiplusfontfamily.h
*******************************************************************************}
{!!=============================================================================
    TFontFamily - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TFontFamily - protected methods
-------------------------------------------------------------------------------}

Function TFontFamily.GetNativeObject: Pointer;
begin
Result := fNativeFamily;
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeFamily);
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

//!!----------------------------------------------------------------------------

constructor TFontFamily.Create(NativeFamily: PGpFontFamily; Status: TStatus);
begin
inherited Create;
fLastResult := Status;
fNativeFamily := NativeFamily;
end;

{!!-----------------------------------------------------------------------------
    TFontFamily - public methods
-------------------------------------------------------------------------------}

constructor TFontFamily.Create;
begin
inherited Create;
fNativeFamily := nil;
fLastResult := Ok;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFontFamily.Create(Name: PWideChar; FontCollection: TFontCollectionBase = nil);
begin
inherited Create;
fNativeFamily := nil;
fLastResult := GdipCreateFontFamilyFromName(Name,FontCollection.NativeObject,@fNativeFamily);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFontFamily.Create(const Name: String; FontCollection: TFontCollectionBase = nil);
begin
Create(PWideChar(StrToWide(Name)),FontCollection);
end;

//!!----------------------------------------------------------------------------

destructor TFontFamily.Destroy;
begin
GdipDeleteFontFamily(fNativeFamily);
inherited;
end;

//!!----------------------------------------------------------------------------

class Function TFontFamily.GenericSansSerif: TFontFamily;
var
  NativeFontFamily: PGpFontFamily;
  Status:           TStatus;
begin
Status := GdipGetGenericFontFamilySansSerif(@NativeFontFamily);
Result := TFontFamily.Create(NativeFontFamily,Status);
end;

//!!----------------------------------------------------------------------------

class Function TFontFamily.GenericSerif: TFontFamily;
var
  NativeFontFamily: PGpFontFamily;
  Status:           TStatus;
begin
Status := GdipGetGenericFontFamilySerif(@NativeFontFamily);
Result := TFontFamily.Create(NativeFontFamily,Status);
end;

//!!----------------------------------------------------------------------------

class Function TFontFamily.GenericMonospace: TFontFamily;
var
  NativeFontFamily: PGpFontFamily;
  Status:           TStatus;
begin
Status := GdipGetGenericFontFamilyMonospace(@NativeFontFamily);
Result := TFontFamily.Create(NativeFontFamily,Status);
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetFamilyName(Name: PWideChar; Language: LANGID = 0): TStatus;
begin
Result := SetStatus(GdipGetFamilyName(fNativeFamily,Name,Language));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFontFamily.GetFamilyName(out Name: String; Language: LANGID = 0): TStatus;
var
  Buffer: WideString;
begin
Buffer := '';
SetLength(Buffer,LF_FACESIZE);
Result := GetFamilyName(PWideChar(Buffer),Language);
Name := WideToStr(Buffer);
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.Clone: TFontFamily;
var
  ClonedFamily: PGpFontFamily;
begin
ClonedFamily := nil;
SetStatus(GdipCloneFontFamily(fNativeFamily,@ClonedFamily));
Result := TFontFamily.Create(ClonedFamily,fLastResult);
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.IsAvailable: BOOL;
begin
Result := Assigned(fNativeFamily);
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.IsStyleAvailable(Style: INT): BOOL;
begin
If SetStatus(GdipIsStyleAvailable(fNativeFamily,Style,@Result)) <> Ok then
  Result := False;
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetEmHeight(Style: INT): UINT16;
begin
SetStatus(GdipGetEmHeight(fNativeFamily,Style,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetCellAscent(Style: INT): UINT16;
begin
SetStatus(GdipGetCellAscent(fNativeFamily,Style,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetCellDescent(Style: INT): UINT16;
begin
SetStatus(GdipGetCellDescent(fNativeFamily,Style,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetLineSpacing(Style: INT): UINT16;
begin
SetStatus(GdipGetLineSpacing(fNativeFamily,Style,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFontFamily.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!*****************************************************************************
    gdiplusfont.h
*******************************************************************************}
{!!=============================================================================
    TFont - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TFont - protected methods
-------------------------------------------------------------------------------}

Function TFont.GetNativeObject: Pointer;
begin
Result := fNativeFont;
end;

//!!----------------------------------------------------------------------------

Function TFont.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeFont);
end;

//!!----------------------------------------------------------------------------

constructor TFont.Create(Font: PGpFont; Status: TStatus);
begin
inherited Create;
fLastResult := Status;
SetNativeFont(Font);
end;

//!!----------------------------------------------------------------------------

procedure TFont.SetNativeFont(Font: PGpFont);
begin
fNativeFont := Font;
end;

//!!----------------------------------------------------------------------------

Function TFont.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then
  fLastResult := Status;
Result := Status;
end;

{!!-----------------------------------------------------------------------------
    TFont - public methods
-------------------------------------------------------------------------------}

constructor TFont.Create(hDC: HDC);
var
  Font: PGpFont;
begin
inherited Create;
Font := nil;
fLastResult := GdipCreateFontFromDC(hDC,@Font);
SetNativeFont(Font);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(hDC: HDC; LogFont: PLogFontA);
var
  Font: PGpFont;
begin
inherited Create;
Font := nil;
If Assigned(LogFont) then
  fLastResult := GdipCreateFontFromLogfontA(hDC,LogFont,@Font)
else
  fLastResult := GdipCreateFontFromDC(hDC,@Font);
SetNativeFont(Font);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(hDC: HDC; LogFont: PLogFontW);
var
  Font: PGpFont;
begin
inherited Create;
Font := nil;
If Assigned(LogFont) then
  fLastResult := GdipCreateFontFromLogfontW(hDC,LogFont,@Font)
else
  fLastResult := GdipCreateFontFromDC(hDC,@Font);
SetNativeFont(Font);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(hDC: HDC; hFont: HFONT);
var
  Font:     PGpFont;
  LogFont:  TLOGFONTA;
begin
inherited Create;
Font := nil;
If hFont <> 0 then
  begin
    If GetObjectA(hFont,SizeOf(TLOGFONTA),@LogFont) <> 0 then
      fLastResult := GdipCreateFontFromLogfontA(hDC,@LogFont,@Font)
    else
      fLastResult := GdipCreateFontFromDC(hDC,@Font);
  end
else fLastResult := GdipCreateFontFromDC(hDC,@Font);
SetNativeFont(Font);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(Family: TFontFamily; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint);
var
  Font: PGpFont;
begin
inherited Create;
Font := nil;
fLastResult := GdipCreateFont(Family.NativeObject,EmSize,Style,aUnit,@Font);
SetNativeFont(Font);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(FamilyName: PWideChar; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint;
  FontCollection: TFontCollectionBase = nil);
var
  Family: TFontFamily;
begin
inherited Create;
fNativeFont := nil;
Family := TFontFamily.Create(FamilyName,FontCollection);
try
  If Family.GetLastStatus <> Ok then
    begin
      FreeAndNil(Family);
      Family := TFontFamily.GenericSansSerif;
      fLastResult := Family.GetLastStatus;
      If fLastResult <> Ok then
        Exit;
    end;
  fLastResult := GdipCreateFont(Family.NativeObject,EmSize,Style,aUnit,@fNativeFont);
  //!! following seems to be just a copy of previous lines, but it is this way in the original source too
  If fLastResult <> Ok then
    begin
      FreeAndNil(Family);
      Family := TFontFamily.GenericSansSerif;
      fLastResult := Family.GetLastStatus;
      If fLastResult <> Ok then
        Exit;
      fLastResult := GdipCreateFont(Family.NativeObject,EmSize,Style,aUnit,@fNativeFont);
    end;
finally
  FreeAndNil(Family);
end;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFont.Create(const FamilyName: String; EmSize: REAL; Style: INT = FontStyleRegular; aUnit: TUnit = UnitPoint;
  FontCollection: TFontCollectionBase = nil);
begin
Create(PWideChar(StrToWide(FamilyName)),EmSize,Style,aUnit,FontCollection);
end;

//!!----------------------------------------------------------------------------

Function TFont.GetLogFontA(G: TGraphicsBase; LogFontA: PLOGFONTA): TStatus;
begin
Result := SetStatus(GdipGetLogFontA(fNativeFont,G.NativeObject,LogFontA));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetLogFontW(G: TGraphicsBase; LogFontW: PLOGFONTW): TStatus;
begin
Result := SetStatus(GdipGetLogFontW(fNativeFont,G.NativeObject,LogFontW));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetLogFont(G: TGraphicsBase; LogFont: PLOGFONT): TStatus;
begin
Result := SetStatus(GdipGetLogFont(fNativeFont,G.NativeObject,LogFont));
end;

//!!----------------------------------------------------------------------------

Function TFont.Clone: TFont;
var
  CloneFont: PGpFont;
begin
CloneFont := nil;
SetStatus(GdipCloneFont(fNativeFont,@CloneFont));
Result := TFont.Create(CloneFont,fLastResult);
end;

//!!----------------------------------------------------------------------------

destructor TFont.Destroy;
begin
GdipDeleteFont(fNativeFont);
end;

//!!----------------------------------------------------------------------------

Function TFont.IsAvailable: BOOL;
begin
Result := Assigned(fNativeFont);
end;

//!!----------------------------------------------------------------------------

Function TFont.GetStyle: INT;
begin
SetStatus(GdipGetFontStyle(fNativeFont,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetSize: REAL;
begin
SetStatus(GdipGetFontSize(fNativeFont,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetUnit: TUnit;
begin
SetStatus(GdipGetFontUnit(fNativeFont,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetLastStatus: TStatus;
begin
Result := fLastResult;
end;

//!!----------------------------------------------------------------------------

Function TFont.GetHeight(Graphics: TGraphicsBase): REAL;
begin
SetStatus(GdipGetFontHeight(fNativeFont,Graphics.NativeObject,@Result));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFont.GetHeight(Dpi: REAL): REAL;
begin
SetStatus(GdipGetFontHeightGivenDPI(fNativeFont,Dpi,@Result));
end;

//!!----------------------------------------------------------------------------

Function TFont.GetFamily(Family: TFontFamily): TStatus;
var
  Status: TStatus;
begin
If Assigned(Family) then
  begin
    Status := GdipGetFamily(fNativeFont,Family.NativeObjectAddr);
    Family.SetStatus(Status);
    Result := SetStatus(Status);
  end
else Result := SetStatus(InvalidParameter);
end;


{!!*****************************************************************************
    gdiplusbitmap.h
*******************************************************************************}
{!!=============================================================================
    TImage - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TImage - protected methods
-------------------------------------------------------------------------------}

Function TImage.GetNativeObject: Pointer;
begin
Result := fNativeImage;
end;

//!!----------------------------------------------------------------------------

Function TImage.GetNativeObjectAddr: Pointer;
begin
Result := Addr(fNativeImage);
end;
 
//!!----------------------------------------------------------------------------

procedure TImage.SetNativeImage(NewNativeImage: PGpImage);
begin
fNativeImage := NewNativeImage;
end;

//!!----------------------------------------------------------------------------

Function TImage.SetStatus(Status: TStatus): TStatus;
begin
If Status <> Ok then   
  fLastResult := Status;
Result := Status;
end;
 
//!!----------------------------------------------------------------------------

constructor TImage.Create(NativeImage: PGpImage; Status: TStatus);
begin
inherited Create;
SetNativeImage(NativeImage);
fLastResult := Status;
end;

{!!-----------------------------------------------------------------------------
    TImage - public methods
-------------------------------------------------------------------------------}

constructor TImage.Create(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False);
begin
inherited Create;
fNativeImage := nil;
If UseEmbeddedColorManagement then
  fLastResult := GdipLoadImageFromFileICM(FileName,@fNativeImage)
else
  fLastResult := GdipLoadImageFromFile(FileName,@fNativeImage);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TImage.Create(const Filename: String; UseEmbeddedColorManagement: BOOL = False);
begin
Create(PWideChar(StrToWide(FileName)),UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TImage.Create(Stream: IStream; UseEmbeddedColorManagement: BOOL = False);
begin
inherited Create;
fNativeImage := nil;
If UseEmbeddedColorManagement then
  fLastResult := GdipLoadImageFromStreamICM(Stream,@fNativeImage)
else
  fLastResult := GdipLoadImageFromStream(Stream,@fNativeImage);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TImage.Create(Stream: TStream; UseEmbeddedColorManagement: BOOL = False);
begin
{!!
  Instance of TStreamAdapter is automatically freed since it is passed as an
  ref-counted interface.
}
Create(IStream(TStreamAdapter.Create(Stream)),UseEmbeddedColorManagement);
end;

//!!----------------------------------------------------------------------------

class Function TImage.FromFile(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False): TImage;
begin
Result := TImage.Create(Filename,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TImage.FromFile(const Filename: String; UseEmbeddedColorManagement: BOOL = False): TImage;
begin
Result := TImage.Create(Filename,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TImage.FromStream(Stream: IStream; UseEmbeddedColorManagement: BOOL = False): TImage;
begin
Result := TImage.Create(Stream,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TImage.FromStream(Stream: TStream; UseEmbeddedColorManagement: BOOL = False): TImage;
begin
Result := TImage.Create(Stream,UseEmbeddedColorManagement);
end;

//!!----------------------------------------------------------------------------

destructor TImage.Destroy;
begin
GdipDisposeImage(fNativeImage);
inherited;
end;

//!!----------------------------------------------------------------------------

Function TImage.Clone: TImage;
var
  CloneImage: PGpImage;
begin
CloneImage := nil;
SetStatus(GdipCloneImage(fNativeImage,@CloneImage));
Result := TImage.Create(CloneImage,fLastResult);
end;

//!!----------------------------------------------------------------------------

Function TImage.Save(Filename: PWideChar; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus;
begin
Result := SetStatus(GdipSaveImageToFile(fNativeImage,Filename,ClsidEncoder,EncoderParams));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImage.Save(const Filename: String; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus;
begin
Result := Save(PWideChar(StrToWide(Filename)),ClsidEncoder,EncoderParams);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImage.Save(Stream: IStream; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus;
begin
Result := SetStatus(GdipSaveImageToStream(fNativeImage,Stream,ClsidEncoder,EncoderParams));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImage.Save(Stream: TStream; ClsidEncoder: PCLSID; EncoderParams: PEncoderParameters = nil): TStatus;
begin
Result := Save(IStream(TStreamAdapter.Create(Stream)),ClsidEncoder,EncoderParams);
end;

//!!----------------------------------------------------------------------------

Function TImage.SaveAdd(EncoderParams: PEncoderParameters): TStatus;
begin
Result := SetStatus(GdipSaveAdd(fNativeImage,EncoderParams));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImage.SaveAdd(NewImage: TImage; EncoderParams: PEncoderParameters): TStatus;
begin
If Assigned(NewImage) then
  Result := SetStatus(GdipSaveAddImage(fNativeImage,NewImage.NativeObject,EncoderParams))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TImage.GetType: TImageType;
begin
Result := ImageTypeUnknown;
SetStatus(GdipGetImageType(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPhysicalDimension(Size: PSizeF): TStatus;
var
  Width, Height:  REAL;
begin
If Assigned(Size) then
  begin
    Result := SetStatus(GdipGetImageDimension(fNativeImage,@Width,@Height));
    Size^.Width := Width;
    Size^.Height := Height;
  end
else Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TImage.GetBounds(SrcRect: PRectF; SrcUnit: PUnit): TStatus;
begin
Result := SetStatus(GdipGetImageBounds(fNativeImage,PGpRectF(SrcRect),PGpUnit(SrcUnit)));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetWidth: UINT;
begin
Result := 0;
SetStatus(GdipGetImageWidth(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetHeight: UINT;
begin
Result := 0;
SetStatus(GdipGetImageHeight(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetHorizontalResolution: REAL;
begin
Result := 0.0;
SetStatus(GdipGetImageHorizontalResolution(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetVerticalResolution: REAL;
begin
Result := 0.0;
SetStatus(GdipGetImageVerticalResolution(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetFlags: UINT;
begin
Result := 0;
SetStatus(GdipGetImageFlags(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetRawFormat(Format: PGUID): TStatus;
begin
Result := SetStatus(GdipGetImageRawFormat(fNativeImage,Format));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPixelFormat: TPixelFormat;
begin
SetStatus(GdipGetImagePixelFormat(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPaletteSize: INT;
begin
Result := 0;
SetStatus(GdipGetImagePaletteSize(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPalette(Palette: PColorPalette; Size: INT): TStatus;
begin
Result := SetStatus(GdipGetImagePalette(fNativeImage,Palette,Size));
end;

//!!----------------------------------------------------------------------------

Function TImage.SetPalette(Palette: PColorPalette): TStatus;
begin
Result := SetStatus(GdipSetImagePalette(fNativeImage,Palette));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetThumbnailImage(ThumbWidth,ThumbHeight: UINT; Callback: TGetThumbnailImageAbort = nil; CallbackData: Pointer = nil): TImage;
var
  ThumbImage: PGpImage;
begin
ThumbImage := nil;
SetStatus(GdipGetImageThumbnail(fNativeImage,ThumbWidth,ThumbHeight,@ThumbImage,Callback,CallbackData));
Result := TImage.Create(ThumbImage,fLastResult);
If not Assigned(Result) then
  GdipDisposeImage(ThumbImage);
end;

//!!----------------------------------------------------------------------------

Function TImage.GetFrameDimensionsCount: UINT;
begin
Result := 0;
SetStatus(GdipImageGetFrameDimensionsCount(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetFrameDimensionsList(DimensionsIDs: PGUID; Count: UINT): TStatus;
begin
Result := SetStatus(GdipImageGetFrameDimensionsList(fNativeImage,DimensionsIDs,Count));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetFrameCount(DimensionID: PGUID): UINT;
begin
Result := 0;
SetStatus(GdipImageGetFrameCount(fNativeImage,DimensionID,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.SelectActiveFrame(DimensionID: PGUID; FrameIndex: UINT): TStatus;
begin
Result := SetStatus(GdipImageSelectActiveFrame(fNativeImage,DimensionID,FrameIndex));
end;

//!!----------------------------------------------------------------------------

Function TImage.RotateFlip(RotateFlipType: TRotateFlipType): TStatus;
begin
Result := SetStatus(GdipImageRotateFlip(fNativeImage,RotateFlipType));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPropertyCount: UINT;
begin
Result := 0;
SetStatus(GdipGetPropertyCount(fNativeImage,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPropertyIdList(NumOfProperty: UINT; List: PPROPID): TStatus;
begin
Result := SetStatus(GdipGetPropertyIdList(fNativeImage,NumOfProperty,List));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPropertyItemSize(PropId: PROPID): UINT;
begin
Result := 0;
SetStatus(GdipGetPropertyItemSize(fNativeImage,PropId,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPropertyItem(PropId: PROPID; PropSize: UINT; Buffer: PPropertyItem): TStatus;
begin
Result := SetStatus(GdipGetPropertyItem(fNativeImage,PropId,PropSize,Buffer));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetPropertySize(TotalBufferSize,NumProperties: PUINT): TStatus;
begin
Result := SetStatus(GdipGetPropertySize(fNativeImage,TotalBufferSize,NumProperties));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetAllPropertyItems(TotalBufferSize,NumProperties: UINT; AllItems: PPropertyItem): TStatus;
begin
If Assigned(AllItems) then
  Result := SetStatus(GdipGetAllPropertyItems(fNativeImage,TotalBufferSize,NumProperties,AllItems))
else
  Result := SetStatus(InvalidParameter);
end;

//!!----------------------------------------------------------------------------

Function TImage.RemovePropertyItem(PropId: PROPID): TStatus;
begin
Result := SetStatus(GdipRemovePropertyItem(fNativeImage,PropId));
end;

//!!----------------------------------------------------------------------------

Function TImage.SetPropertyItem(Item: PPropertyItem): TStatus;
begin
Result := SetStatus(GdipSetPropertyItem(fNativeImage,Item));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetEncoderParameterListSize(ClsidEncoder: PCLSID): UINT;
begin
Result := 0;
SetStatus(GdipGetEncoderParameterListSize(fNativeImage,ClsidEncoder,@Result));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetEncoderParameterList(ClsidEncoder: PCLSID; Size: UINT; Buffer: PEncoderParameters): TStatus;
begin
Result := SetStatus(GdipGetEncoderParameterList(fNativeImage,ClsidEncoder,Size,Buffer));
end;

{$IF GDIPVER >= $0110}
//!!----------------------------------------------------------------------------

Function TImage.FindFirstItem(Item: PImageItemData): TStatus;
begin
Result := SetStatus(GdipFindFirstImageItem(fNativeImage,Item));
end;

//!!----------------------------------------------------------------------------

Function TImage.FindNextItem(Item: PImageItemData): TStatus;
begin
Result := SetStatus(GdipFindNextImageItem(fNativeImage,Item));
end;

//!!----------------------------------------------------------------------------

Function TImage.GetItemData(Item: PImageItemData): TStatus;
begin
Result := SetStatus(GdipGetImageItemData(fNativeImage,Item));
end;

//!!----------------------------------------------------------------------------

Function TImage.SetAbort(PIAbort: PGdiplusAbort): TStatus;
begin
Result := SetStatus(GdipImageSetAbort(fNativeImage,PIAbort));
end;

{$IFEND}
//!!----------------------------------------------------------------------------

Function TImage.GetLastStatus: TStatus;
begin
Result := fLastResult;
fLastResult := Ok;
end;


{!!=============================================================================
    TBitmap - class implementation
===============================================================================}
{!!-----------------------------------------------------------------------------
    TBitmap - protected methods
-------------------------------------------------------------------------------}

constructor TBitmap.Create(NativeBitmap: PGpBitmap);
begin
inherited Create;
fLastResult := Ok;
SetNativeImage(PGpImage(NativeBitmap));
end;

{!!-----------------------------------------------------------------------------
    TBitmap - public methods
-------------------------------------------------------------------------------}

constructor TBitmap.Create(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
If UseEmbeddedColorManagement then
  fLastResult := GdipCreateBitmapFromFileICM(Filename,@Bitmap)
else
  fLastResult := GdipCreateBitmapFromFile(Filename,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(const Filename: String; UseEmbeddedColorManagement: BOOL = False);
begin
Create(PwideChar(StrToWide(Filename)),UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(Stream: IStream; UseEmbeddedColorManagement: BOOL = False);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
If UseEmbeddedColorManagement then
  fLastResult := GdipCreateBitmapFromStreamICM(Stream,@Bitmap)
else
  fLastResult := GdipCreateBitmapFromStream(Stream,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(Stream: TStream; UseEmbeddedColorManagement: BOOL = False);
begin
Create(IStream(TStreamAdapter.Create(Stream)),UseEmbeddedColorManagement);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromFile(Filename: PWideChar; UseEmbeddedColorManagement: BOOL = False): TBitmap;
begin
Result := TBitmap.Create(Filename,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBitmap.FromFile(const Filename: String; UseEmbeddedColorManagement: BOOL = False): TBitmap;
begin
Result := TBitmap.Create(Filename,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBitmap.FromStream(Stream: IStream; UseEmbeddedColorManagement: BOOL = False): TBitmap;
begin
Result := TBitmap.Create(Stream,UseEmbeddedColorManagement);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBitmap.FromStream(Stream: TStream; UseEmbeddedColorManagement: BOOL = False): TBitmap;
begin
Result := TBitmap.Create(Stream,UseEmbeddedColorManagement);
end;

//!!----------------------------------------------------------------------------

constructor TBitmap.Create(Width,Height,Stride: INT; Format: TPixelFormat; Scan0: PBYTE);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromScan0(Width,Height,Stride,Format,Scan0,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(Width,Height: INT; Format: TPixelFormat);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromScan0(Width,Height,0,Format,nil,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(Width,Height: INT; Target: TGraphicsBase);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromGraphics(Width,Height,Target.NativeObject,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!!----------------------------------------------------------------------------

Function TBitmap.Clone(const Rect: TRect; Format: TPixelFormat): TBitmap;
begin
Result := Clone(Rect.X,Rect.Y,Rect.Width,Rect.Height,Format);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TBitmap.Clone(X,Y,Width,Height: INT; Format: TPixelFormat): TBitmap;
var
  GpDstBitmap:  PGpBitmap;
begin
GpDstBitmap := nil;
If SetStatus(GdipCloneBitmapAreaI(X,Y,Width,Height,Format,PGpBitmap(fNativeImage),@GpDstBitmap)) = Ok then
  begin
    Result := TBitmap.Create(GpDstBitmap);
    If not Assigned(Result) then
      GdipDisposeImage(PGpImage(GpDstBitmap));
  end
else Result := nil;
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TBitmap.Clone(const Rect: TRectF; Format: TPixelFormat): TBitmap;
begin
Result := Clone(Rect.X,Rect.Y,Rect.Width,Rect.Height,Format);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TBitmap.Clone(X,Y,Width,Height: REAL; Format: TPixelFormat): TBitmap;
var
  GpDstBitmap:  PGpBitmap;
begin
GpDstBitmap := nil;
If SetStatus(GdipCloneBitmapArea(X,Y,Width,Height,Format,PGpBitmap(fNativeImage),@GpDstBitmap)) = Ok then
  begin
    Result := TBitmap.Create(GpDstBitmap);
    If not Assigned(Result) then
      GdipDisposeImage(PGpImage(GpDstBitmap));
  end
else Result := nil;
end;

//!!----------------------------------------------------------------------------

Function TBitmap.LockBits(const Rect: TRect; Flags: UINT; Format: TPixelFormat; LockedBitmapData: PBitmapData): TStatus;
begin
Result := SetStatus(GdipBitmapLockBits(PGpBitmap(fNativeImage),@Rect,Flags,Format,LockedBitmapData));
end;

//!!----------------------------------------------------------------------------

Function TBitmap.UnlockBits(LockedBitmapData: PBitmapData): TStatus;
begin
Result := SetStatus(GdipBitmapUnlockBits(PGpBitmap(fNativeImage),LockedBitmapData));
end;

//!!----------------------------------------------------------------------------

Function TBitmap.GetPixel(X,Y: INT; Color: PColor): TStatus;
var
  Argb: TARGB;
begin
Result := SetStatus(GdipBitmapGetPixel(PGpBitmap(fNativeImage),X,Y,@Argb));
If Result = Ok  then
  SetValue(Color^,Argb);
end;

//!!----------------------------------------------------------------------------

Function TBitmap.SetPixel(X,Y: INT; const Color: TColor): TStatus;
begin
Result := SetStatus(GdipBitmapSetPixel(PGpBitmap(fNativeImage),X,Y,GetValue(Color)));
end;

{$IF GDIPVER >= $0110}
//!!----------------------------------------------------------------------------

Function TBitmap.ConvertFormat(Format: TPixelFormat; DitherType: TDitherType; PaletteType: TPaletteType; Palette: PColorPalette;
  AlphaThresholdPercent: REAL): TStatus;
begin
Result := SetStatus(GdipBitmapConvertFormat(PGpBitmap(fNativeImage),Format,DitherType,
  PaletteType,Palette,AlphaThresholdPercent));
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.InitializePalette(Palette: PColorPalette; PaletteType: TPaletteType; OptimalColors: INT;
  UseTransparentColor: BOOL; Bitmap: TBitmap): TStatus;
begin
Result := GdipInitializePalette(Palette,PaletteType,OptimalColors,UseTransparentColor,Bitmap.NativeObject);
end;

//!!----------------------------------------------------------------------------

Function TBitmap.ApplyEffect(Effect: TEffect; ROI: Windows.PRECT): TStatus;
begin
If Assigned(Effect.fAuxData) then
  begin
    GdipFree(Effect.fAuxData);
    Effect.fAuxData := nil;
    Effect.fAuxDataSize := 0;
  end;
Result := GdipBitmapApplyEffect(PGpBitmap(fNativeImage),Effect.NativeObject,ROI,
  Effect.fUseAuxData,Addr(Effect.fAuxData),Addr(Effect.fAuxDataSize));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBitmap.ApplyEffect(Inputs: PBitmap; NumInputs: INT; Effect: TEffect; ROI,OutputRect: Windows.PRECT; Output: PBitmap): TStatus;
var
  OutputNative: PGpBitmap;
  NativeInputs: array of PGpBitmap;
  i:            Integer;
begin
If NumInputs >= 0 then
  begin
    OutputNative := nil;
    NativeInputs := nil;
    SetLength(NativeInputs,NumInputs);
    If Length(NativeInputs) > 0 then
      begin
        For i := 0 to Pred(NumInputs) do
          NativeInputs[i] := PBitmapArray(Inputs)^[i].NativeObject;
        If Assigned(Effect.fAuxData) then
          begin
            GdipFree(Effect.fAuxData);
            Effect.fAuxData := nil;
            Effect.fAuxDataSize := 0;
          end;
        Result := GdipBitmapCreateApplyEffect(Pointer(NativeInputs),NumInputs,Effect.NativeObject,ROI,
          OutputRect,@OutputNative,Effect.fUseAuxData,Addr(Effect.fAuxData),Addr(Effect.fAuxDataSize));
        If (Result = Ok) and Assigned(OutputNative) then
          begin
            Output^ := TBitmap.Create(OutputNative);
            If not Assigned(Output^) then
              begin
                Result := OutOfMemory;
                GdipDisposeImage(PGpImage(OutputNative));
              end;
          end
        else Output^ := nil;
        SetLength(NativeInputs,0);
      end
    else Result := OutOfMemory;
  end
else Result := InvalidParameter;
end;

//!!----------------------------------------------------------------------------

Function TBitmap.GetHistogram(Format: THistogramFormat; NumberOfEntries: UINT; Channel0,Channel1,Channel2,Channel3: PUINT): TStatus;
begin
Result := GdipBitmapGetHistogram(PGpBitmap(fNativeImage),Format,NumberOfEntries,Channel0,Channel1,Channel2,Channel3);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.GetHistogramSize(Format: THistogramFormat; NumberOfEntries: PUINT): TStatus;
begin
Result := GdipBitmapGetHistogramSize(Format,NumberOfEntries);
end;

{$IFEND}
//!!----------------------------------------------------------------------------

Function TBitmap.SetResolution(XDpi,YDpi: REAL): TStatus;
begin
Result := SetStatus(GdipBitmapSetResolution(PGpBitmap(fNativeImage),XDpi,YDpi));
end;

//!!----------------------------------------------------------------------------

constructor TBitmap.Create(Surface: IDirectDrawSurface7);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromDirectDrawSurface(Surface,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(GdiBitmapInfo: PBITMAPINFO; GdiBitmapData: Pointer);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromGdiDib(GdiBitmapInfo,GdiBitmapData,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(hBM: HBITMAP; hPal: HPALETTE);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromHBITMAP(hBM,hPal,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(hIcon: HICON);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromHICON(hIcon,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(hInstance: HINSTANCE; BitmapName: PWideChar);
var
  Bitmap: PGpBitmap;
begin
inherited Create;
Bitmap := nil;
fLastResult := GdipCreateBitmapFromResource(hInstance,BitmapName,@Bitmap);
SetNativeImage(PGpImage(Bitmap));
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitmap.Create(hInstance: HINSTANCE; const BitmapName: String);
begin
Create(hInstance,PWideChar(StrToWide(BitmapName)));
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromDirectDrawSurface7(Surface: IDirectDrawSurface7): TBitmap;
begin
Result := TBitmap.Create(Surface);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromBITMAPINFO(GdiBitmapInfo: PBITMAPINFO; GdiBitmapData: Pointer): TBitmap;
begin
Result := TBitmap.Create(GdiBitmapInfo,GdiBitmapData);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromHBITMAP(hBM: HBITMAP; hPal: HPALETTE): TBitmap;
begin
Result := TBitmap.Create(hBM,hPal);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromHICON(hIcon: HICON): TBitmap;
begin
Result := TBitmap.Create(hIcon);
end;

//!!----------------------------------------------------------------------------

class Function TBitmap.FromResource(hInstance: HINSTANCE; BitmapName: PWideChar): TBitmap;
begin
Result := TBitmap.Create(hInstance,BitmapName);
end;

//!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBitmap.FromResource(hInstance: HINSTANCE; const BitmapName: String): TBitmap;
begin
Result := TBitmap.Create(hInstance,BitmapName);
end;

//!!----------------------------------------------------------------------------

Function TBitmap.GetHBITMAP(const ColorBackground: TColor; hBMReturn: PHBITMAP): TStatus;
begin
Result := SetStatus(GdipCreateHBITMAPFromBitmap(PGpBitmap(fNativeImage),hBMReturn,GetValue(ColorBackground)));
end;

//!!----------------------------------------------------------------------------

Function TBitmap.GetHICON(hIcon: PHICON): TStatus;
begin
Result := SetStatus(GdipCreateHICONFromBitmap(PGpBitmap(fNativeImage),hIcon));
end;


{!!*****************************************************************************
    gdiplusimagecodec.h
*******************************************************************************}

Function GetImageDecodersSize(NumDecoders: PUINT; Size: PUINT): TStatus;
begin
Result := GdipGetImageDecodersSize(NumDecoders,Size);
end;

//!!----------------------------------------------------------------------------

Function GetImageDecoders(NumDecoders: UINT; Size: UINT; Decoders: PImageCodecInfo): TStatus;
begin
Result := GdipGetImageDecoders(NumDecoders,Size,Decoders);
end;

//!!----------------------------------------------------------------------------

Function GetImageEncodersSize(NumEncoders: PUINT; Size: PUINT): TStatus;
begin
Result := GdipGetImageEncodersSize(NumEncoders,Size);
end;

//!!----------------------------------------------------------------------------

Function GetImageEncoders(NumEncoders: UINT; Size: UINT; Encoders: PImageCodecInfo): TStatus;
begin
Result := GdipGetImageEncoders(NumEncoders,Size,Encoders);
end;


{!!*****************************************************************************
    Helpers - implementation
*******************************************************************************}

Function VersionSupported: Integer;
begin
If LibraryIsPresent(GDIPLIB) then
  begin
    If SymbolIsPresent(GDIPLIB,'GdipImageSetAbort') then
      Result := 2
    else
      Result := 1;
  end
else Result := 0;
end;

//!!----------------------------------------------------------------------------

Function StatusAsStr(Status: TStatus): String;
begin
case Status of
  Ok:                         Result := 'Ok';
  GenericError:               Result := 'Generic error';
  InvalidParameter:           Result := 'Invalid parameter';
  OutOfMemory:                Result := 'Out of memory';
  ObjectBusy:                 Result := 'Object busy';
  InsufficientBuffer:         Result := 'Insufficient buffer';
  NotImplemented:             Result := 'Not implemented';
  Win32Error:                 Result := 'Win32 error';
  WrongState:                 Result := 'Wrong state';
  Aborted:                    Result := 'Aborted';
  FileNotFound:               Result := 'File not found';
  ValueOverflow:              Result := 'Value overflow';
  AccessDenied:               Result := 'Access denied';
  UnknownImageFormat:         Result := 'Unknown image format';
  FontFamilyNotFound:         Result := 'Font family not found';
  FontStyleNotFound:          Result := 'Font style not found';
  NotTrueTypeFont:            Result := 'Not TrueType font';
  UnsupportedGdiplusVersion:  Result := 'Unsupported Gdiplus version';
  GdiplusNotInitialized:      Result := 'Gdiplus not initialized';
  PropertyNotFound:           Result := 'Property not found';
  PropertyNotSupported:       Result := 'Property not supported';
{$IF GDIPVER >= $0110}
  ProfileNotFound:            Result := 'Profile not found';
{$IFEND}
else
  Result := Format('Unknown error (%d).',[Ord(Status)]);
end;
end;

//!!----------------------------------------------------------------------------

Function StatusCheck(Status: TStatus): Boolean;
begin
Result := Status = Ok;
end;

//!!----------------------------------------------------------------------------

Function StatusRaise(Status: TStatus): TStatus;
begin
Result := Status;
If Status <> Ok then
  raise EGDIPlusError.CreateFmt('GDI+ failed with error %d: %s.',[Ord(Status),StatusAsStr(Status)]);
end;

//!!----------------------------------------------------------------------------

procedure GetDecoders(List: TStringList; CodecString: TCodecString = csMimeType);
var
  Count,Size: UINT;
  Decoders:   PImageCodecInfo;
  MovingPtr:  PImageCodecInfo;
  i:          Integer;
begin
List.Clear;
If StatusCheck(GetImageDecodersSize(@Count,@Size)) then
  If Size > 0 then
    begin
      Decoders := AllocMem(Size);
      try
        If StatusCheck(GetImageDecoders(Count,Size,Decoders)) then
          begin
            MovingPtr := Decoders;
            For i := 0 to Pred(Count) do
              begin
                case CodecString of
                  csName:     List.Add(WideToStr(MovingPtr^.CodecName));
                  csDLLName:  List.Add(WideToStr(MovingPtr^.DllName));
                  csFmtDescr: List.Add(WideToStr(MovingPtr^.FormatDescription));
                  csFileExt:  List.Add(WideToStr(MovingPtr^.FilenameExtension));
                  csMimeType: List.Add(WideToStr(MovingPtr^.MimeType));
                end;
                Inc(MovingPtr);
              end;
          end;
      finally
        FreeMem(Decoders,Size);
      end;
    end;
end;

//!!----------------------------------------------------------------------------

procedure GetEncoders(List: TStringList; CodecString: TCodecString = csMimeType);
var
  Count,Size: UINT;
  Encoders:   PImageCodecInfo;
  MovingPtr:  PImageCodecInfo;
  i:          Integer;
begin
List.Clear;
If StatusCheck(GetImageEncodersSize(@Count,@Size)) then
  If Size > 0 then
    begin
      Encoders := AllocMem(Size);
      try
        If StatusCheck(GetImageEncoders(Count,Size,Encoders)) then
          begin
            MovingPtr := Encoders;
            For i := 0 to Pred(Count) do
              begin
                case CodecString of
                  csName:     List.Add(WideToStr(MovingPtr^.CodecName));
                  csDLLName:  List.Add(WideToStr(MovingPtr^.DllName));
                  csFmtDescr: List.Add(WideToStr(MovingPtr^.FormatDescription));
                  csFileExt:  List.Add(WideToStr(MovingPtr^.FilenameExtension));
                  csMimeType: List.Add(WideToStr(MovingPtr^.MimeType));
                end;
                Inc(MovingPtr);
              end;
          end;
      finally
        FreeMem(Encoders,Size);
      end;
    end;
end;

//!!----------------------------------------------------------------------------

Function GetDecoderCLSID(const MimeType: String): TCLSID;
var
  Count,Size: UINT;
  Decoders:   PImageCodecInfo;
  MovingPtr:  PImageCodecInfo;
  TempMime:   WideString;
  i:          Integer;
begin
If StatusCheck(GetImageDecodersSize(@Count,@Size)) then
  If Size > 0 then
    begin
      Decoders := AllocMem(Size);
      try
        If StatusCheck(GetImageDecoders(Count,Size,Decoders)) then
          begin
            MovingPtr := Decoders;
            TempMime := StrToWide(MimeType);
            For i := 0 to Pred(Count) do
              If WideStringCompare(MovingPtr^.MimeType,TempMime,False) = 0 then
                begin
                  Result := MovingPtr^.Clsid;
                  Exit;
                end
              else Inc(MovingPtr);
          end;
      finally
        FreeMem(Decoders,Size);
      end;
    end;
raise EGDIPlusCodecNotFound.CreateFmt('GetDecoderCLSID: Decoder for mime type "%s" not found.',[MimeType]);
end;

//!!----------------------------------------------------------------------------

Function GetEncoderCLSID(const MimeType: String): TCLSID;
var
  Count,Size: UINT;
  Encoders:   PImageCodecInfo;
  MovingPtr:  PImageCodecInfo;
  TempMime:   WideString;
  i:          Integer;
begin
If StatusCheck(GetImageEncodersSize(@Count,@Size)) then
  If Size > 0 then
    begin
      Encoders := AllocMem(Size);
      try
        If StatusCheck(GetImageEncoders(Count,Size,Encoders)) then
          begin
            MovingPtr := Encoders;
            TempMime := StrToWide(MimeType);
            For i := 0 to Pred(Count) do
              If WideStringCompare(MovingPtr^.MimeType,TempMime,False) = 0 then
                begin
                  Result := MovingPtr^.Clsid;
                  Exit;
                end
              else Inc(MovingPtr);
          end;
      finally
        FreeMem(Encoders,Size);
      end;
    end;
raise EGDIPlusCodecNotFound.CreateFmt('GetEncoderCLSID: Encoder for mime type "%s" not found.',[MimeType]);
end;


{!!*****************************************************************************
    unit initialization
*******************************************************************************}

{$IF (GDIPVER >= $0110) and not Defined(NewGDIPStatic)}
initialization
  GDIPLusLibraryContext := DefaultLibraryContext;
{$IFEND}

end.
