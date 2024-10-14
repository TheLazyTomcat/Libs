{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  DevIL - header file for the ImageLib

    Direct translation of C header file il.h, a part of bindings for DevIL
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

  Version 1.0.2 (2024-10-14)

  Build against DevIL library version 1.8.0

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
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils    - github.com/TheLazyTomcat/Lib.UInt64Utils
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    WinFileInfo    - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit DevIL;

{$INCLUDE '.\DevIL_defs.inc'}

interface

uses
  SysUtils, Classes,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

const
  IL_UNICODE = {$IFDEF DevIL_Unicode}True{$ELSE}False{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EILException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EILStreamTooLarge = class(EILException);

{===============================================================================
    Basic types
===============================================================================}
type
  ILenum     = UInt32;              ILenum_p     = ^ILenum;
  ILboolean  = UInt8;               ILboolean_p  = ^ILboolean;
  ILbitfield = UInt32;              ILbitfield_p = ^ILbitfield;
  ILbyte     = Int8;                ILbyte_p     = ^ILbyte;
  ILshort    = Int16;               ILshort_p    = ^ILshort;
  ILint      = Int32;               ILint_p      = ^ILint;
  ILsizei    = PtrUInt;             ILsizei_p    = ^ILsizei;
  ILubyte    = UInt8;               ILubyte_p    = ^ILubyte;
  ILushort   = UInt16;              ILushort_p   = ^ILushort;
  ILuint     = UInt32;              ILuint_p     = ^ILuint;
  ILfloat    = Float32;             ILfloat_p    = ^ILfloat;
  ILclampf   = Float32;             ILclampf_p   = ^ILclampf;
  ILdouble   = Float64;             ILdouble_p   = ^ILdouble;
  ILclampd   = Float64;             ILclampd_p   = ^ILclampd;

  ILint64  = Int64;                 ILint64_p  = ^ILint64;
  ILuint64 = UInt64;                ILuint64_p = ^ILuint64;

{$IFDEF DevIL_Unicode}
	ILchar         = WideChar;        ILchar_p         = ^ILchar;
	ILstring       = PWideChar;       ILstring_p       = ^ILstring;
	ILconst_string = PWideChar;       ILconst_string_p = ^ILconst_string;
{$ELSE}
	ILchar         = AnsiChar;        ILchar_p         = ^ILchar;
	ILstring       = PAnsiChar;       ILstring_p       = ^ILstring;
	ILconst_string = PAnsiChar;       ILconst_string_p = ^ILconst_string;
{$ENDIF}                    

{===============================================================================
    Constants
===============================================================================}
const
  IL_FALSE = 0;
  IL_TRUE  = 1;
  

  //  Matches OpenGL's right now.
  //! Data formats \link Formats Formats\endlink
  IL_COLOUR_INDEX    = $1900;
  IL_COLOR_INDEX     = $1900;
  IL_ALPHA           = $1906;
  IL_RGB             = $1907;
  IL_RGBA            = $1908;
  IL_BGR             = $80E0;
  IL_BGRA            = $80E1;
  IL_LUMINANCE       = $1909;
  IL_LUMINANCE_ALPHA = $190A;


  //! Data types \link Types Types\endlink
  IL_BYTE           = $1400;
  IL_UNSIGNED_BYTE  = $1401;
  IL_SHORT          = $1402;
  IL_UNSIGNED_SHORT = $1403;
  IL_INT            = $1404;
  IL_UNSIGNED_INT   = $1405;
  IL_FLOAT          = $1406;
  IL_DOUBLE         = $140A;
  IL_HALF           = $140B;


  IL_MAX_BYTE           = High(Int8);
  IL_MAX_UNSIGNED_BYTE  = High(UInt8);
  IL_MAX_SHORT          = High(Int16);
  IL_MAX_UNSIGNED_SHORT = High(UInt16);
  IL_MAX_INT            = High(Int32);
  IL_MAX_UNSIGNED_INT   = High(UInt32);


  IL_VENDOR   = $1F00;
  IL_LOAD_EXT = $1F01;
  IL_SAVE_EXT = $1F02;


  //
  // IL-specific #define's
  //
  IL_VERSION = 180;
  

  // Attribute Bits
  IL_ORIGIN_BIT          = $00000001;
  IL_FILE_BIT            = $00000002;
  IL_PAL_BIT             = $00000004;
  IL_FORMAT_BIT          = $00000008;
  IL_TYPE_BIT            = $00000010;
  IL_COMPRESS_BIT        = $00000020;
  IL_LOADFAIL_BIT        = $00000040;
  IL_FORMAT_SPECIFIC_BIT = $00000080;
  IL_ALL_ATTRIB_BITS     = $000FFFFF;


  // Palette types
  IL_PAL_NONE   = $0400;
  IL_PAL_RGB24  = $0401;
  IL_PAL_RGB32  = $0402;
  IL_PAL_RGBA32 = $0403;
  IL_PAL_BGR24  = $0404;
  IL_PAL_BGR32  = $0405;
  IL_PAL_BGRA32 = $0406;


  // Image types
  IL_TYPE_UNKNOWN = $0000;
  IL_BMP          = $0420;  //!< Microsoft Windows Bitmap - .bmp extension
  IL_CUT          = $0421;  //!< Dr. Halo - .cut extension
  IL_DOOM         = $0422;  //!< DooM walls - no specific extension
  IL_DOOM_FLAT    = $0423;  //!< DooM flats - no specific extension
  IL_ICO          = $0424;  //!< Microsoft Windows Icons and Cursors - .ico and .cur extensions
  IL_JPG          = $0425;  //!< JPEG - .jpg, .jpe and .jpeg extensions
  IL_JFIF         = $0425;  //!<
  IL_ILBM         = $0426;  //!< Amiga IFF (FORM ILBM) - .iff, .ilbm, .lbm extensions
  IL_PCD          = $0427;  //!< Kodak PhotoCD - .pcd extension
  IL_PCX          = $0428;  //!< ZSoft PCX - .pcx extension
  IL_PIC          = $0429;  //!< PIC - .pic extension
  IL_PNG          = $042A;  //!< Portable Network Graphics - .png extension
  IL_PNM          = $042B;  //!< Portable Any Map - .pbm, .pgm, .ppm and .pnm extensions
  IL_SGI          = $042C;  //!< Silicon Graphics - .sgi, .bw, .rgb and .rgba extensions
  IL_TGA          = $042D;  //!< TrueVision Targa File - .tga, .vda, .icb and .vst extensions
  IL_TIF          = $042E;  //!< Tagged Image File Format - .tif and .tiff extensions
  IL_CHEAD        = $042F;  //!< C-Style Header - .h extension
  IL_RAW          = $0430;  //!< Raw Image Data - any extension
  IL_MDL          = $0431;  //!< Half-Life Model Texture - .mdl extension
  IL_WAL          = $0432;  //!< Quake 2 Texture - .wal extension
  IL_LIF          = $0434;  //!< Homeworld Texture - .lif extension
  IL_MNG          = $0435;  //!< Multiple-image Network Graphics - .mng extension
  IL_JNG          = $0435;  //!<
  IL_GIF          = $0436;  //!< Graphics Interchange Format - .gif extension
  IL_DDS          = $0437;  //!< DirectDraw Surface - .dds extension
  IL_DCX          = $0438;  //!< ZSoft Multi-PCX - .dcx extension
  IL_PSD          = $0439;  //!< Adobe PhotoShop - .psd extension
  IL_EXIF         = $043A;  //!<
  IL_PSP          = $043B;  //!< PaintShop Pro - .psp extension
  IL_PIX          = $043C;  //!< PIX - .pix extension
  IL_PXR          = $043D;  //!< Pixar - .pxr extension
  IL_XPM          = $043E;  //!< X Pixel Map - .xpm extension
  IL_HDR          = $043F;  //!< Radiance High Dynamic Range - .hdr extension
  IL_ICNS         = $0440;  //!< Macintosh Icon - .icns extension
  IL_JP2          = $0441;  //!< Jpeg 2000 - .jp2 extension
  IL_EXR          = $0442;  //!< OpenEXR - .exr extension
  IL_WDP          = $0443;  //!< Microsoft HD Photo - .wdp and .hdp extension
  IL_VTF          = $0444;  //!< Valve Texture Format - .vtf extension
  IL_WBMP         = $0445;  //!< Wireless Bitmap - .wbmp extension
  IL_SUN          = $0446;  //!< Sun Raster - .sun, .ras, .rs, .im1, .im8, .im24 and .im32 extensions
  IL_IFF          = $0447;  //!< Interchange File Format - .iff extension
  IL_TPL          = $0448;  //!< Gamecube Texture - .tpl extension
  IL_FITS         = $0449;  //!< Flexible Image Transport System - .fit and .fits extensions
  IL_DICOM        = $044A;  //!< Digital Imaging and Communications in Medicine (DICOM) - .dcm and .dicom extensions
  IL_IWI          = $044B;  //!< Call of Duty Infinity Ward Image - .iwi extension
  IL_BLP          = $044C;  //!< Blizzard Texture Format - .blp extension
  IL_FTX          = $044D;  //!< Heavy Metal: FAKK2 Texture - .ftx extension
  IL_ROT          = $044E;  //!< Homeworld 2 - Relic Texture - .rot extension
  IL_TEXTURE      = $044F;  //!< Medieval II: Total War Texture - .texture extension
  IL_DPX          = $0450;  //!< Digital Picture Exchange - .dpx extension
  IL_UTX          = $0451;  //!< Unreal (and Unreal Tournament) Texture - .utx extension
  IL_MP3          = $0452;  //!< MPEG-1 Audio Layer 3 - .mp3 extension
  IL_KTX          = $0453;  //!< Khronos Texture - .ktx extension

  IL_JASC_PAL = $0475;  //!< PaintShop Pro Palette


  // Error Types
  IL_NO_ERROR             = $0000;
  IL_INVALID_ENUM         = $0501;
  IL_OUT_OF_MEMORY        = $0502;
  IL_FORMAT_NOT_SUPPORTED = $0503;
  IL_INTERNAL_ERROR       = $0504;
  IL_INVALID_VALUE        = $0505;
  IL_ILLEGAL_OPERATION    = $0506;
  IL_ILLEGAL_FILE_VALUE   = $0507;
  IL_INVALID_FILE_HEADER  = $0508;
  IL_INVALID_PARAM        = $0509;
  IL_COULD_NOT_OPEN_FILE  = $050A;
  IL_INVALID_EXTENSION    = $050B;
  IL_FILE_ALREADY_EXISTS  = $050C;
  IL_OUT_FORMAT_SAME      = $050D;
  IL_STACK_OVERFLOW       = $050E;
  IL_STACK_UNDERFLOW      = $050F;
  IL_INVALID_CONVERSION   = $0510;
  IL_BAD_DIMENSIONS       = $0511;
  IL_FILE_READ_ERROR      = $0512;  // 05/12/2002: Addition by Sam.
  IL_FILE_WRITE_ERROR     = $0512;


  IL_LIB_GIF_ERROR  = $05E1;
  IL_LIB_JPEG_ERROR = $05E2;
  IL_LIB_PNG_ERROR  = $05E3;
  IL_LIB_TIFF_ERROR = $05E4;
  IL_LIB_MNG_ERROR  = $05E5;
  IL_LIB_JP2_ERROR  = $05E6;
  IL_LIB_EXR_ERROR  = $05E7;
  IL_UNKNOWN_ERROR  = $05FF;


  // Origin Definitions
  IL_ORIGIN_SET        = $0600;
  IL_ORIGIN_LOWER_LEFT = $0601;
  IL_ORIGIN_UPPER_LEFT = $0602;
  IL_ORIGIN_MODE       = $0603;


  // Format and Type Mode Definitions
  IL_FORMAT_SET  = $0610;
  IL_FORMAT_MODE = $0611;
  IL_TYPE_SET    = $0612;
  IL_TYPE_MODE   = $0613;


  // File definitions
  IL_FILE_OVERWRITE = $0620;
  IL_FILE_MODE      = $0621;


  // Palette definitions
  IL_CONV_PAL = $0630;


  // Load fail definitions
  IL_DEFAULT_ON_FAIL = $0632;


  // Key colour and alpha definitions
  IL_USE_KEY_COLOUR = $0635;
  IL_USE_KEY_COLOR  = $0635;
  IL_BLIT_BLEND     = $0636;


  // Interlace definitions
  IL_SAVE_INTERLACED = $0639;
  IL_INTERLACE_MODE  = $063A;


  // Quantization definitions
  IL_QUANTIZATION_MODE = $0640;
  IL_WU_QUANT          = $0641;
  IL_NEU_QUANT         = $0642;
  IL_NEU_QUANT_SAMPLE  = $0643;
  IL_MAX_QUANT_INDEXS  = $0644; //XIX : ILint : Maximum number of colors to reduce to, default of 256. and has a range of 2-256
  IL_MAX_QUANT_INDICES = $0644; // Redefined, since the above   is misspelled


  // Hints
  IL_FASTEST          = $0660;
  IL_LESS_MEM         = $0661;
  IL_DONT_CARE        = $0662;
  IL_MEM_SPEED_HINT   = $0665;
  IL_USE_COMPRESSION  = $0666;
  IL_NO_COMPRESSION   = $0667;
  IL_COMPRESSION_HINT = $0668;


  // Compression
  IL_NVIDIA_COMPRESS  = $0670;
  IL_SQUISH_COMPRESS  = $0671;


  // Subimage types
  IL_SUB_NEXT   = $0680;
  IL_SUB_MIPMAP = $0681;
  IL_SUB_LAYER  = $0682;


  // Compression definitions
  IL_COMPRESS_MODE = $0700;
  IL_COMPRESS_NONE = $0701;
  IL_COMPRESS_RLE  = $0702;
  IL_COMPRESS_LZO  = $0703;
  IL_COMPRESS_ZLIB = $0704;


  // File format-specific values
  IL_TGA_CREATE_STAMP        = $0710;
  IL_JPG_QUALITY             = $0711;
  IL_PNG_INTERLACE           = $0712;
  IL_TGA_RLE                 = $0713;
  IL_BMP_RLE                 = $0714;
  IL_SGI_RLE                 = $0715;
  IL_TGA_ID_STRING           = $0717;
  IL_TGA_AUTHNAME_STRING     = $0718;
  IL_TGA_AUTHCOMMENT_STRING  = $0719;
  IL_PNG_AUTHNAME_STRING     = $071A;
  IL_PNG_TITLE_STRING        = $071B;
  IL_PNG_DESCRIPTION_STRING  = $071C;
  IL_TIF_DESCRIPTION_STRING  = $071D;
  IL_TIF_HOSTCOMPUTER_STRING = $071E;
  IL_TIF_DOCUMENTNAME_STRING = $071F;
  IL_TIF_AUTHNAME_STRING     = $0720;
  IL_JPG_SAVE_FORMAT         = $0721;
  IL_CHEAD_HEADER_STRING     = $0722;
  IL_PCD_PICNUM              = $0723;
  IL_PNG_ALPHA_INDEX         = $0724; // currently has no effect!
  IL_JPG_PROGRESSIVE         = $0725;
  IL_VTF_COMP                = $0726;


  // DXTC definitions
  IL_DXTC_FORMAT      = $0705;
  IL_DXT1             = $0706;
  IL_DXT2             = $0707;
  IL_DXT3             = $0708;
  IL_DXT4             = $0709;
  IL_DXT5             = $070A;
  IL_DXT_NO_COMP      = $070B;
  IL_KEEP_DXTC_DATA   = $070C;
  IL_DXTC_DATA_FORMAT = $070D;
  IL_3DC              = $070E;
  IL_RXGB             = $070F;
  IL_ATI1N            = $0710;
  IL_DXT1A            = $0711;  // Normally the same as IL_DXT1, except for nVidia Texture Tools.


  // Environment map definitions
  IL_CUBEMAP_POSITIVEX = $00000400;
  IL_CUBEMAP_NEGATIVEX = $00000800;
  IL_CUBEMAP_POSITIVEY = $00001000;
  IL_CUBEMAP_NEGATIVEY = $00002000;
  IL_CUBEMAP_POSITIVEZ = $00004000;
  IL_CUBEMAP_NEGATIVEZ = $00008000;
  IL_SPHEREMAP         = $00010000;


  // Values
  IL_VERSION_NUM           = $0DE2;
  IL_IMAGE_WIDTH           = $0DE4;
  IL_IMAGE_HEIGHT          = $0DE5;
  IL_IMAGE_DEPTH           = $0DE6;
  IL_IMAGE_SIZE_OF_DATA    = $0DE7;
  IL_IMAGE_BPP             = $0DE8;
  IL_IMAGE_BYTES_PER_PIXEL = $0DE8;
  IL_IMAGE_BITS_PER_PIXEL  = $0DE9;
  IL_IMAGE_FORMAT          = $0DEA;
  IL_IMAGE_TYPE            = $0DEB;
  IL_PALETTE_TYPE          = $0DEC;
  IL_PALETTE_SIZE          = $0DED;
  IL_PALETTE_BPP           = $0DEE;
  IL_PALETTE_NUM_COLS      = $0DEF;
  IL_PALETTE_BASE_TYPE     = $0DF0;
  IL_NUM_FACES             = $0DE1;
  IL_NUM_IMAGES            = $0DF1;
  IL_NUM_MIPMAPS           = $0DF2;
  IL_NUM_LAYERS            = $0DF3;
  IL_ACTIVE_IMAGE          = $0DF4;
  IL_ACTIVE_MIPMAP         = $0DF5;
  IL_ACTIVE_LAYER          = $0DF6;
  IL_ACTIVE_FACE           = $0E00;
  IL_CUR_IMAGE             = $0DF7;
  IL_IMAGE_DURATION        = $0DF8;
  IL_IMAGE_PLANESIZE       = $0DF9;
  IL_IMAGE_BPC             = $0DFA;
  IL_IMAGE_OFFX            = $0DFB;
  IL_IMAGE_OFFY            = $0DFC;
  IL_IMAGE_CUBEFLAGS       = $0DFD;
  IL_IMAGE_ORIGIN          = $0DFE;
  IL_IMAGE_CHANNELS        = $0DFF;


  IL_SEEK_SET = 0;
  IL_SEEK_CUR = 1;
  IL_SEEK_END = 2;
  IL_EOF      = -1;

{===============================================================================
    Callback (procedural) types
===============================================================================}
type
  ILHANDLE = Pointer;

  // Callback functions for file reading
  fCloseRProc = procedure(Handle: ILHANDLE); stdcall;
  fEofProc    = Function(Handle: ILHANDLE): ILboolean; stdcall;
  fGetcProc   = Function(Handle: ILHANDLE): ILint; stdcall;
  fOpenRProc  = Function(FileName: ILconst_string): ILHANDLE; stdcall;
  fReadProc   = Function(Buffer: Pointer; Size,Number: ILuint; Handle: ILHANDLE): ILint; stdcall; // number = count
  fSeekRProc  = Function(Handle: ILHANDLE; Offset,Mode: ILint): ILint; stdcall; // mode = origin (contants above, eg. IL_SEEK_SET)
  fTellRProc  = Function(Handle: ILHANDLE): ILint; stdcall;

  // Callback functions for file writing
  fCloseWProc = procedure(Handle: ILHANDLE); stdcall;
  fOpenWProc  = Function(FileName: ILconst_string): ILHANDLE; stdcall;
  fPutcProc   = Function(C: ILubyte; Handle: ILHANDLE): ILint; stdcall;
  fSeekWProc  = Function(Handle: ILHANDLE; Offset,Mode: ILint): ILint; stdcall;
  fTellWProc  = Function(Handle: ILHANDLE): ILint; stdcall;
  fWriteProc  = Function(Buffer: Pointer; Size,Number: ILuint; Handle: ILHANDLE): ILint; stdcall;

  // Callback functions for allocation and deallocation
  mAlloc = Function(Size: ILsizei): Pointer; stdcall;
  mFree  = procedure(Ptr: Pointer); stdcall;

  // Registered format procedures
  IL_LOADPROC = Function(FileName: ILconst_string): ILenum; stdcall;
  IL_SAVEPROC = Function(FileName: ILconst_string): ILenum; stdcall;

{===============================================================================
    API functions (as procedural variables)
===============================================================================}
var
  // ImageLib Functions
  ilActiveFace:                 Function(Number: ILuint): ILboolean; stdcall = nil;
  ilActiveImage:                Function(Number: ILuint): ILboolean; stdcall = nil;
  ilActiveLayer:                Function(Number: ILuint): ILboolean; stdcall = nil;
  ilActiveMipmap:               Function(Number: ILuint): ILboolean; stdcall = nil;
  ilApplyPal:                   Function(FileName: ILconst_string): ILboolean; stdcall = nil;
  ilApplyProfile:               Function(InProfile,OutProfile: ILstring): ILboolean; stdcall = nil;
  ilBindImage:                  procedure(Image: ILuint); stdcall = nil;
  ilBlit:                       Function(Source: ILuint; DestX,DestY,DestZ: ILint; SrcX,SrcY,SrcZ,Width,Height,Depth: ILuint): ILboolean; stdcall = nil;
  ilClampNTSC:                  Function: ILboolean; stdcall = nil;
  ilClearColour:                procedure(Red,Green,Blue,Alpha: ILclampf); stdcall = nil;
  ilClearImage:                 Function: ILboolean; stdcall = nil;
  ilCloneCurImage:              Function: ILuint; stdcall = nil;
  ilCompressDXT:                Function(Data: ILubyte_p; Width,Height,Depth: ILuint; DXTCFormat: ILenum; DXTCSize: ILuint_p): ILubyte_p; stdcall = nil;
  ilCompressFunc:               Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilConvertImage:               Function(DestFormat,DestType: ILenum): ILboolean; stdcall = nil;
  ilConvertPal:                 Function(DestFormat: ILenum): ILboolean; stdcall = nil;
  ilCopyImage:                  Function(Src: ILuint): ILboolean; stdcall = nil;
  ilCopyPixels:                 Function(XOff,YOff,ZOff,Width,Height,Depth: ILuint; Format,aType: ILenum; Data: Pointer): ILuint; stdcall = nil;
  ilCreateSubImage:             Function(aType: ILenum; Num: ILuint): ILuint; stdcall = nil;
  ilDefaultImage:               Function: ILboolean; stdcall = nil;
  ilDeleteImage:                procedure(Num: ILuint); stdcall = nil;
  ilDeleteImages:               procedure(Num: ILsizei; Images: ILuint_p); stdcall = nil;  
  ilDetermineType:              Function(FileName: ILconst_string): ILenum; stdcall = nil;
  ilDetermineTypeF:             Function(aFile: ILHANDLE): ILenum; stdcall = nil;
  ilDetermineTypeL:             Function(Lump: Pointer; Size: ILuint): ILenum; stdcall = nil;
  ilDisable:                    Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilDxtcDataToImage:            Function: ILboolean; stdcall = nil;
  ilDxtcDataToSurface:          Function: ILboolean; stdcall = nil;
  ilEnable:                     Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilFlipSurfaceDxtcData:        procedure; stdcall = nil;
  ilFormatFunc:                 Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilGenImages:                  procedure(Num: ILsizei; Images: ILuint_p); stdcall = nil;
  ilGenImage:                   Function: ILuint; stdcall = nil;    
  ilGetAlpha:                   Function(aType: ILenum): ILubyte_p; stdcall = nil;
  ilGetBoolean:                 Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilGetBooleanv:                procedure(Mode: ILenum; Param: ILboolean_p); stdcall = nil;
  ilGetData:                    Function: ILubyte_p; stdcall = nil;
  ilGetDXTCData:                Function(Buffer: Pointer; BufferSize: ILuint; DXTCFormat: ILenum): ILuint; stdcall = nil;
  ilGetError:                   Function: ILenum; stdcall = nil;
  ilGetInteger:                 Function(Mode: ILenum): ILint; stdcall = nil;
  ilGetIntegerv:                procedure(Mode: ILenum; Param: ILint_p); stdcall = nil;
  ilGetLumpPos:                 Function: ILuint; stdcall = nil;
  ilGetPalette:                 Function: ILubyte_p; stdcall = nil;
  ilGetString:                  Function(StringName: ILenum): ILconst_string; stdcall = nil;
  ilHint:                       procedure(Target,Mode: ILenum); stdcall = nil;
  ilInvertSurfaceDxtcDataAlpha: Function: ILboolean; stdcall = nil;
  ilInit:                       procedure; stdcall = nil;
  ilImageToDxtcData:            Function(Format: ILenum): ILboolean; stdcall = nil;
  ilIsDisabled:                 Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilIsEnabled:                  Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilIsImage:                    Function(Image: ILuint): ILboolean; stdcall = nil;
  ilIsValid:                    Function(aType: ILenum; FileName: ILconst_string): ILboolean; stdcall = nil;
  ilIsValidF:                   Function(aType: ILenum; aFile: ILHANDLE): ILboolean; stdcall = nil;
  ilIsValidL:                   Function(aType: ILenum; Lump: Pointer; Size: ILuint): ILboolean; stdcall = nil;
  ilKeyColour:                  procedure(Red,Green,Blue,Alpha: ILclampf); stdcall = nil;
  ilLoad:                       Function(aType: ILenum; FileName: ILconst_string): ILboolean; stdcall = nil;
  ilLoadF:                      Function(aType: ILenum; aFile: ILHANDLE): ILboolean; stdcall = nil;
  ilLoadImage:                  Function(FileName: ILconst_string): ILboolean; stdcall = nil;
  ilLoadL:                      Function(aType: ILenum; Lump: Pointer; Size: ILuint): ILboolean; stdcall = nil;
  ilLoadPal:                    Function(FileName: ILconst_string): ILboolean; stdcall = nil;
  ilModAlpha:                   procedure(AlphaValue: ILdouble); stdcall = nil;
  ilOriginFunc:                 Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilOverlayImage:               Function(Source: ILuint; XCoord,YCoord,ZCoord: ILint): ILboolean; stdcall = nil;
  ilPopAttrib:                  procedure; stdcall = nil;
  ilPushAttrib:                 procedure(Bits: ILuint); stdcall = nil;
  ilRegisterFormat:             procedure(Format: ILenum); stdcall = nil;
  ilRegisterLoad:               Function(Ext: ILconst_string; Load: IL_LOADPROC): ILboolean; stdcall = nil;
  ilRegisterMipNum:             Function(Num: ILuint): ILboolean; stdcall = nil;
  ilRegisterNumFaces:           Function(Num: ILuint): ILboolean; stdcall = nil;
  ilRegisterNumImages:          Function(Num: ILuint): ILboolean; stdcall = nil;
  ilRegisterOrigin:             procedure(Origin: ILenum); stdcall = nil;
  ilRegisterPal:                procedure(Pal: Pointer; Size: ILuint; aType: ILenum); stdcall = nil;  // palette
  ilRegisterSave:               Function(Ext: ILconst_string; Save: IL_SAVEPROC): ILboolean; stdcall = nil;
  ilRegisterType:               procedure(aType: ILenum); stdcall = nil;
  ilRemoveLoad:                 Function(Ext: ILconst_string): ILboolean; stdcall = nil;
  ilRemoveSave:                 Function(Ext: ILconst_string): ILboolean; stdcall = nil;
  ilResetMemory:                procedure; stdcall = nil; // Deprecated (use ilSetMemory wiht both arguments nil)
  ilResetRead:                  procedure; stdcall = nil;
  ilResetWrite:                 procedure; stdcall = nil; 
  ilSave:                       Function(aType: ILenum; FileName: ILconst_string): ILboolean; stdcall = nil;
  ilSaveF:                      Function(aType: ILenum; aFile: ILHANDLE): ILuint; stdcall = nil;
  ilSaveImage:                  Function(FileName: ILconst_string): ILboolean; stdcall = nil;
  ilSaveL:                      Function(aType: ILenum; Lump: Pointer; Size: ILuint): ILuint; stdcall = nil;
  ilSavePal:                    Function(FileName: ILconst_string): ILboolean; stdcall = nil;
  ilSetAlpha:                   Function(AlphaValue: ILdouble): ILboolean; stdcall = nil;
  ilSetData:                    Function(Data: Pointer): ILboolean; stdcall = nil;
  ilSetDuration:                Function(Duration: ILuint): ILboolean; stdcall = nil;
  ilSetInteger:                 procedure(Mode: ILenum; Param: ILint); stdcall = nil;
  ilSetMemory:                  procedure(MemAlloc: mAlloc; MemFree: mFree); stdcall = nil;
  ilSetPixels:                  procedure(XOff,YOff,ZOff: ILint; Width,Height,Depth: ILuint; Format: ILenum; aType: ILenum; Data: Pointer); stdcall = nil;
  ilSetRead:                    procedure(OpenProc: fOpenRProc; CloseProc: fCloseRProc; EoFProc: fEofProc; GetcProc: fGetcProc; ReadProc: fReadProc; SeekProc: fSeekRProc; TellProc: fTellRProc); stdcall = nil;
  ilSetString:                  procedure(Mode: ILenum; aString: PAnsiChar); stdcall = nil;
  ilSetWrite:                   procedure(OpenProc: fOpenWProc; CloseProc: fCloseWProc; PutcProc: fPutcProc; SeekProc: fSeekWProc; TellProc: fTellWProc; WriteProc: fWriteProc); stdcall = nil;
  ilShutDown:                   procedure; stdcall = nil;
  ilSurfaceToDxtcData:          Function(Format: ILenum): ILboolean; stdcall = nil;
  ilTexImage:                   Function(Width,Height,Depth: ILuint; NumChannels: ILubyte; Format: ILenum; aType: ILenum; Data: Pointer): ILboolean; stdcall = nil;
  ilTexImageDxtc:               Function(w,h,d: ILint; DxtFormat: ILenum; Data: ILubyte_p): ILboolean; stdcall = nil;
  ilTypeFromExt:                Function(FileName: ILconst_string): ILenum; stdcall = nil;
  ilTypeFunc:                   Function(Mode: ILenum): ILboolean; stdcall = nil;
  ilLoadData:                   Function(FileName: ILconst_string; Width,Height,Depth: ILuint; Bpp: ILubyte): ILboolean; stdcall = nil;
  ilLoadDataF:                  Function(aFile: ILHANDLE; Width,Height,Depth: ILuint; Bpp: ILubyte): ILboolean; stdcall = nil;
  ilLoadDataL:                  Function(Lump: Pointer; Size: ILuint;  Width,Height,Depth: ILuint; Bpp: ILubyte): ILboolean; stdcall = nil;
  ilSaveData:                   Function(FileName: ILconst_string): ILboolean; stdcall = nil;

  // For all those weirdos that spell "colour" without the 'u'.
  ilClearColor:                 procedure(Red,Green,Blue,Alpha: ILclampf); stdcall = nil;
  ilKeyColor:                   procedure(Red,Green,Blue,Alpha: ILclampf); stdcall = nil;

{===============================================================================
    Header macros - declaration
===============================================================================}

Function IL_LIMIT(x,min,max: Integer): Integer; overload;
Function IL_LIMIT(x,min,max: Int64): Int64; overload;
Function IL_LIMIT(x,min,max: Single): Single; overload;
Function IL_LIMIT(x,min,max: Double): Double; overload;

Function IL_CLAMP(x: Single): Single; overload;
Function IL_CLAMP(x: Double): Double; overload;

procedure imemclear(var x; count: Integer);

{===============================================================================
    Library loading - declaration
===============================================================================}
const
  DevIL_LibFileName = 'DevIL.dll';

Function DevIL_Initialized: Boolean;
Function DevIL_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
procedure DevIL_Finalize(FinalLib: Boolean = True);

{===============================================================================
    Helper functions - declaration
===============================================================================}

Function ILConstStrEncode(const Str: String): ILconst_string;
Function ILConstStrDecode(Str: ILconst_string): String;

Function ILStrEncode(const Str: String): ILstring;
Function ILStrDecode(Str: ILstring): String;

Function ILBoolEncode(B: Boolean): ILboolean;
Function ILBoolDecode(B: ILboolean): Boolean;

{===============================================================================
    Implemented functions - declaration
===============================================================================}
{
  Following five functions are using DevIL's integrated system for user-
  implemented IO streaming to provide functions accepting pascal stream objects
  when loading or saving pictures.

    WARNING - given the DevIL implementation, the passed stream must not be
              larger than 2GiB minus one byte (2147483647 bytes). In case of
              saving, this limit is halved (so 1GiB - 1) to at least partially
              account for size of data being saved. If you pass larger stream,
              then an EILStreamTooLarge exception will be raised.

    NOTE - reading and writing is done from current possition in the stream.
           Position after the call is undefined, do not assume anything about
           its value.
}
Function ilIsValidS(aType: ILenum; Stream: TStream): ILboolean;
Function ilDetermineTypeS(Stream: TStream): ILenum;
Function ilLoadS(aType: ILenum; Stream: TStream): ILboolean;
Function ilSaveS(aType: ILenum; Stream: TStream): ILuint;  
Function ilLoadDataS(Stream: TStream; Width,Height,Depth: ILuint; Bpp: ILubyte): ILboolean;

implementation

uses
  DynLibUtils, StrRect;

{===============================================================================
    Header macros - implementation
===============================================================================}

Function IL_LIMIT(x,min,max: Integer): Integer;
begin
If x < min then
  Result := min
else If x > max then
  Result := max
else
  Result := x;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IL_LIMIT(x,min,max: Int64): Int64;
begin
If x < min then
  Result := min
else If x > max then
  Result := max
else
  Result := x;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IL_LIMIT(x,min,max: Single): Single;
begin
If x < min then
  Result := min
else If x > max then
  Result := max
else
  Result := x;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IL_LIMIT(x,min,max: Double): Double;
begin
If x < min then
  Result := min
else If x > max then
  Result := max
else
  Result := x;
end;

//------------------------------------------------------------------------------

Function IL_CLAMP(x: Single): Single;
begin
Result := IL_LIMIT(x,0,1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IL_CLAMP(x: Double): Double;
begin
Result := IL_LIMIT(x,0,1);
end;

//------------------------------------------------------------------------------

procedure imemclear(var x; count: Integer);
begin
FillChar(x,count,0);
end;


{===============================================================================
    Library loading - implementation
===============================================================================}
var
  DevIL_LibraryContext: TDLULibraryContext;

//------------------------------------------------------------------------------

Function DevIL_Initialized: Boolean;
begin
Result := CheckLibrary(DevIL_LibraryContext);
end;

//------------------------------------------------------------------------------

Function DevIL_Initialize(const LibPath: String = DevIL_LibFileName; InitLib: Boolean = True): Boolean;
begin
Result := OpenLibraryAndResolveSymbols(LibPath,DevIL_LibraryContext,[
  Symbol(@@ilActiveFace,                'ilActiveFace'),
  Symbol(@@ilActiveImage,               'ilActiveImage'),
  Symbol(@@ilActiveLayer,               'ilActiveLayer'),
  Symbol(@@ilActiveMipmap,              'ilActiveMipmap'),
  Symbol(@@ilApplyPal,                  'ilApplyPal'),
  Symbol(@@ilApplyProfile,              'ilApplyProfile'),
  Symbol(@@ilBindImage,                 'ilBindImage'),
  Symbol(@@ilBlit,                      'ilBlit'),
  Symbol(@@ilClampNTSC,                 'ilClampNTSC'),
  Symbol(@@ilClearColour,               'ilClearColour'),
  Symbol(@@ilClearImage,                'ilClearImage'),
  Symbol(@@ilCloneCurImage,             'ilCloneCurImage'),
  Symbol(@@ilCompressDXT,               'ilCompressDXT'),
  Symbol(@@ilCompressFunc,              'ilCompressFunc'),
  Symbol(@@ilConvertImage,              'ilConvertImage'),
  Symbol(@@ilConvertPal,                'ilConvertPal'),
  Symbol(@@ilCopyImage,                 'ilCopyImage'),
  Symbol(@@ilCopyPixels,                'ilCopyPixels'),
  Symbol(@@ilCreateSubImage,            'ilCreateSubImage'),
  Symbol(@@ilDefaultImage,              'ilDefaultImage'),
  Symbol(@@ilDeleteImage,               'ilDeleteImage'),
  Symbol(@@ilDeleteImages,              'ilDeleteImages'),
  Symbol(@@ilDetermineType,             'ilDetermineType'),
  Symbol(@@ilDetermineTypeF,            'ilDetermineTypeF'),
  Symbol(@@ilDetermineTypeL,            'ilDetermineTypeL'),
  Symbol(@@ilDisable,                   'ilDisable'),
  Symbol(@@ilDxtcDataToImage,           'ilDxtcDataToImage'),
  Symbol(@@ilDxtcDataToSurface,         'ilDxtcDataToSurface'),
  Symbol(@@ilEnable,                    'ilEnable'),
{$IF Defined(Windows) and Defined(x86)}         
  Symbol(@@ilFlipSurfaceDxtcData,       '_ilFlipSurfaceDxtcData@0'), // dunno, this is how the symbol is exported
{$ELSE}
  Symbol(@@ilFlipSurfaceDxtcData,       'ilFlipSurfaceDxtcData'),
{$IFEND}
  Symbol(@@ilFormatFunc,                'ilFormatFunc'),
  Symbol(@@ilGenImages,                 'ilGenImages'),
  Symbol(@@ilGenImage,                  'ilGenImage'),
  Symbol(@@ilGetAlpha,                  'ilGetAlpha'),
  Symbol(@@ilGetBoolean,                'ilGetBoolean'),
  Symbol(@@ilGetBooleanv,               'ilGetBooleanv'),
  Symbol(@@ilGetData,                   'ilGetData'),
  Symbol(@@ilGetDXTCData,               'ilGetDXTCData'),
  Symbol(@@ilGetError,                  'ilGetError'),
  Symbol(@@ilGetInteger,                'ilGetInteger'),
  Symbol(@@ilGetIntegerv,               'ilGetIntegerv'),
  Symbol(@@ilGetLumpPos,                'ilGetLumpPos'),
  Symbol(@@ilGetPalette,                'ilGetPalette'),
  Symbol(@@ilGetString,                 'ilGetString'),
  Symbol(@@ilHint,                      'ilHint'),
{$IF Defined(Windows) and Defined(x86)}
  Symbol(@@ilInvertSurfaceDxtcDataAlpha,'_ilInvertSurfaceDxtcDataAlpha@0'),
{$ELSE}
  Symbol(@@ilInvertSurfaceDxtcDataAlpha,'ilInvertSurfaceDxtcDataAlpha'),
{$IFEND}
  Symbol(@@ilInit,                      'ilInit'),
  Symbol(@@ilImageToDxtcData,           'ilImageToDxtcData'),
  Symbol(@@ilIsDisabled,                'ilIsDisabled'),
  Symbol(@@ilIsEnabled,                 'ilIsEnabled'),
  Symbol(@@ilIsImage,                   'ilIsImage'),
  Symbol(@@ilIsValid,                   'ilIsValid'),
  Symbol(@@ilIsValidF,                  'ilIsValidF'),
  Symbol(@@ilIsValidL,                  'ilIsValidL'),
  Symbol(@@ilKeyColour,                 'ilKeyColour'),
  Symbol(@@ilLoad,                      'ilLoad'),
  Symbol(@@ilLoadF,                     'ilLoadF'),
  Symbol(@@ilLoadImage,                 'ilLoadImage'),
  Symbol(@@ilLoadL,                     'ilLoadL'),
  Symbol(@@ilLoadPal,                   'ilLoadPal'),
  Symbol(@@ilModAlpha,                  'ilModAlpha'),
  Symbol(@@ilOriginFunc,                'ilOriginFunc'),
  Symbol(@@ilOverlayImage,              'ilOverlayImage'),
  Symbol(@@ilPopAttrib,                 'ilPopAttrib'),
  Symbol(@@ilPushAttrib,                'ilPushAttrib'),
  Symbol(@@ilRegisterFormat,            'ilRegisterFormat'),
  Symbol(@@ilRegisterLoad,              'ilRegisterLoad'),
  Symbol(@@ilRegisterMipNum,            'ilRegisterMipNum'),
  Symbol(@@ilRegisterNumFaces,          'ilRegisterNumFaces'),
  Symbol(@@ilRegisterNumImages,         'ilRegisterNumImages'),
  Symbol(@@ilRegisterOrigin,            'ilRegisterOrigin'),
  Symbol(@@ilRegisterPal,               'ilRegisterPal'),
  Symbol(@@ilRegisterSave,              'ilRegisterSave'),
  Symbol(@@ilRegisterType,              'ilRegisterType'),
  Symbol(@@ilRemoveLoad,                'ilRemoveLoad'),
  Symbol(@@ilRemoveSave,                'ilRemoveSave'),
  Symbol(@@ilResetMemory,               'ilResetMemory'),
  Symbol(@@ilResetRead,                 'ilResetRead'),
  Symbol(@@ilResetWrite,                'ilResetWrite'),
  Symbol(@@ilSave,                      'ilSave'),
  Symbol(@@ilSaveF,                     'ilSaveF'),
  Symbol(@@ilSaveImage,                 'ilSaveImage'),
  Symbol(@@ilSaveL,                     'ilSaveL'),
  Symbol(@@ilSavePal,                   'ilSavePal'),
  Symbol(@@ilSetAlpha,                  'ilSetAlpha'),
  Symbol(@@ilSetData,                   'ilSetData'),
  Symbol(@@ilSetDuration,               'ilSetDuration'),
  Symbol(@@ilSetInteger,                'ilSetInteger'),
  Symbol(@@ilSetMemory,                 'ilSetMemory'),
  Symbol(@@ilSetPixels,                 'ilSetPixels'),
  Symbol(@@ilSetRead,                   'ilSetRead'),
  Symbol(@@ilSetString,                 'ilSetString'),
  Symbol(@@ilSetWrite,                  'ilSetWrite'),
  Symbol(@@ilShutDown,                  'ilShutDown'),
  Symbol(@@ilSurfaceToDxtcData,         'ilSurfaceToDxtcData'),
  Symbol(@@ilTexImage,                  'ilTexImage'),
  Symbol(@@ilTexImageDxtc,              'ilTexImageDxtc'),
  Symbol(@@ilTypeFromExt,               'ilTypeFromExt'),
  Symbol(@@ilTypeFunc,                  'ilTypeFunc'),
  Symbol(@@ilLoadData,                  'ilLoadData'),
  Symbol(@@ilLoadDataF,                 'ilLoadDataF'),
  Symbol(@@ilLoadDataL,                 'ilLoadDataL'),
  Symbol(@@ilSaveData,                  'ilSaveData')],[optExceptionOnFailure]) = 103;
// aliasses  
ilClearColor := ilClearColour;
ilKeyColor := ilKeyColour;
{
  Note - no need to check whether the ilInit is assigned. If it is not, then
         an exception was already raised in previous step.
}
If Result and InitLib then
  ilInit;
end;

//------------------------------------------------------------------------------

procedure DevIL_Finalize(FinalLib: Boolean = True);
begin
If FinalLib and Assigned(ilShutDown) then
  ilShutDown;
CloseLibrary(DevIL_LibraryContext);
end;


{===============================================================================
    Helper functions - implementation
===============================================================================}

Function ILConstStrEncode(const Str: String): ILconst_string;
begin
{$IFDEF DevIL_Unicode}
Result := ILconst_string(StrToWide(Str));
{$ELSE}
Result := ILconst_string(StrToAnsi(Str));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function ILConstStrDecode(Str: ILconst_string): String;
begin
{$IFDEF DevIL_Unicode}
Result := WideToStr(Str);
{$ELSE}
Result := AnsiToStr(Str);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function ILStrEncode(const Str: String): ILstring;
begin
{$IFDEF DevIL_Unicode}
Result := ILconst_string(StrToWide(Str));
{$ELSE}
Result := ILconst_string(StrToAnsi(Str));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function ILStrDecode(Str: ILstring): String;
begin
{$IFDEF DevIL_Unicode}
Result := WideToStr(Str);
{$ELSE}
Result := AnsiToStr(Str);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function ILBoolEncode(B: Boolean): ILboolean;
begin
If B then
  Result := IL_TRUE
else
  Result := IL_FALSE;
end;

//------------------------------------------------------------------------------

Function ILBoolDecode(B: ILboolean): Boolean;
begin
Result := B <> IL_FALSE;
end;

{===============================================================================
    Implemented functions - implementation
===============================================================================}
type
  TILStreamData = record
    Stream:   TStream;
    InitPos:  Int64;
  end;

{-------------------------------------------------------------------------------
    Implemented functions - implementation - internals
-------------------------------------------------------------------------------}

Function Stream_OpenProc(FileName: ILconst_string): ILHANDLE; stdcall;
begin
Result := ILHANDLE(FileName); // just to prevend "unused parameter" warnings
end;

//------------------------------------------------------------------------------

procedure Stream_CloseProc(Handle: ILHANDLE); stdcall;
begin
If Assigned(Handle) then; // prevent warning
end;
 
//------------------------------------------------------------------------------

Function Stream_ReadProc(Buffer: Pointer; Size,Number: ILuint; Handle: ILHANDLE): ILint; stdcall;
begin
Result := ILint(TILStreamData(Handle^).Stream.Read(Buffer^,LongInt(Size * Number)) div LongInt(Size));
end;
 
//------------------------------------------------------------------------------

Function Stream_WriteProc(Buffer: Pointer; Size,Number: ILuint; Handle: ILHANDLE): ILint; stdcall;
begin
Result := ILint(TILStreamData(Handle^).Stream.Write(Buffer^,LongInt(Size * Number)) div LongInt(Size));
end;

//------------------------------------------------------------------------------

Function Stream_GetcProc(Handle: ILHANDLE): ILint; stdcall;
var
  Buffer: UInt8;
begin
If TILStreamData(Handle^).Stream.Read(Addr(Buffer)^,1) = 1 then
  Result := Buffer
else
  Result := IL_EOF;
end;
 
//------------------------------------------------------------------------------

Function Stream_PutcProc(C: ILubyte; Handle: ILHANDLE): ILint; stdcall;
begin
If TILStreamData(Handle^).Stream.Write(C,1) = 1 then
  Result := C
else
  Result := IL_EOF;
end;
  
//------------------------------------------------------------------------------

Function Stream_SeekProc(Handle: ILHANDLE; Offset,Mode: ILint): ILint; stdcall;
var
  SeekOrigin: TSeekOrigin;
begin
case Mode of
  IL_SEEK_CUR:  SeekOrigin := soCurrent;
  IL_SEEK_END:  SeekOrigin := soEnd;
  IL_SEEK_SET:  SeekOrigin := soBeginning;
else
  Result := -1;
  Exit;
end;
with TILStreamData(Handle^) do
  Stream.Seek(Int64(Offset + InitPos),SeekOrigin);
Result := 0;  // success
end;
  
//------------------------------------------------------------------------------

Function Stream_TellProc(Handle: ILHANDLE): ILint; stdcall;
begin
with TILStreamData(Handle^) do
  Result := ILint(Stream.Position - InitPos);
end;
  
//------------------------------------------------------------------------------

Function Stream_EofProc(Handle: ILHANDLE): ILboolean; stdcall;
begin
with TILStreamData(Handle^) do
  Result := ILBoolEncode(Stream.Position = Stream.Size);
end;

{-------------------------------------------------------------------------------
    Implemented functions - implementation - public
-------------------------------------------------------------------------------}

Function ilIsValidS(aType: ILenum; Stream: TStream): ILboolean;
var
  StreamData: TILStreamData;
begin
If Stream.Size <= High(ILint) then
  begin
    StreamData.Stream := Stream;
    StreamData.InitPos := Stream.Position;
    ilSetRead(Stream_OpenProc,Stream_CloseProc,Stream_EofProc,Stream_GetcProc,Stream_ReadProc,Stream_SeekProc,Stream_TellProc);
    try
      Result := ilIsValidF(aType,ILHANDLE(@StreamData));
    finally
      ilResetRead;
    end;
  end
else raise EILStreamTooLarge.CreateFmt('ilIsValidS: Stream too large (%d).',[Stream.Size]);
end;

//------------------------------------------------------------------------------

Function ilDetermineTypeS(Stream: TStream): ILenum;
var
  StreamData: TILStreamData;
begin
If Stream.Size <= High(ILint) then
  begin
    StreamData.Stream := Stream;
    StreamData.InitPos := Stream.Position;
    ilSetRead(Stream_OpenProc,Stream_CloseProc,Stream_EofProc,Stream_GetcProc,Stream_ReadProc,Stream_SeekProc,Stream_TellProc);
    try
      Result := ilDetermineTypeF(ILHANDLE(@StreamData));
    finally
      ilResetRead;
    end;
  end
else raise EILStreamTooLarge.CreateFmt('ilDetermineTypeS: Stream too large (%d).',[Stream.Size]);
end;

//------------------------------------------------------------------------------

Function ilLoadS(aType: ILenum; Stream: TStream): ILboolean;
var
  StreamData: TILStreamData;
begin
If Stream.Size <= High(ILint) then
  begin
    StreamData.Stream := Stream;
    StreamData.InitPos := Stream.Position;
    ilSetRead(Stream_OpenProc,Stream_CloseProc,Stream_EofProc,Stream_GetcProc,Stream_ReadProc,Stream_SeekProc,Stream_TellProc);
    try
      Result := ilLoadF(aType,ILHANDLE(@StreamData));
    finally
      ilResetRead;
    end;
  end
else raise EILStreamTooLarge.CreateFmt('ilLoadS: Stream too large (%d).',[Stream.Size]);
end;

//------------------------------------------------------------------------------

Function ilSaveS(aType: ILenum; Stream: TStream): ILuint;
var
  StreamData: TILStreamData;
begin
{
  I know the following check is not enough to fully account for the size of
  data being written. But there is no way of obtaining the resulting size
  without using temporary memory storage, which I want to avoid.
}
If Stream.Size <= (High(ILint) shr 1) then
  begin
    StreamData.Stream := Stream;
    StreamData.InitPos := Stream.Position;
    ilSetWrite(Stream_OpenProc,Stream_CloseProc,Stream_PutcProc,Stream_SeekProc,Stream_TellProc,Stream_WriteProc);
    try
      Result := ilSaveF(aType,ILHANDLE(@StreamData));
    finally
      ilResetWrite;
    end;
  end
else raise EILStreamTooLarge.CreateFmt('ilSaveS: Stream too large (%d).',[Stream.Size]);
end;

//------------------------------------------------------------------------------

Function ilLoadDataS(Stream: TStream; Width,Height,Depth: ILuint; Bpp: ILubyte): ILboolean;
var
  StreamData: TILStreamData;
begin
If Stream.Size <= High(ILint) then
  begin
    StreamData.Stream := Stream;
    StreamData.InitPos := Stream.Position;
    ilSetRead(Stream_OpenProc,Stream_CloseProc,Stream_EofProc,Stream_GetcProc,Stream_ReadProc,Stream_SeekProc,Stream_TellProc);
    try
      Result := ilLoadDataF(ILHANDLE(@StreamData),Width,Height,Depth,Bpp);
    finally
      ilResetRead;
    end;
  end
else raise EILStreamTooLarge.CreateFmt('ilLoadDataS: Stream too large (%d).',[Stream.Size]);
end;

{===============================================================================
    Global variables initialization
===============================================================================}

initialization
  DevIL_LibraryContext := DefaultLibraryContext;

end.
