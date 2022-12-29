{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  zlib bindings - bindings for dynamically linked library (DLL/SO)

    These units provides plain (no wrappers or helpers) bindings for zlib
    library. Most comments were copied directly from zlib.h header without
    any change.

    This binding is distributed with all necessary binaries (object files,
    DLLs) precompiled. For details please refer to file bin_readme.txt.

  Version 1.1.3 (2022-12-27)

  Build against zlib version 1.2.13

  Last change 2022-12-27

  ©2017-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.ZLib

  Dependencies:
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

  Library WindowsVersion is only needed when compiling for Windows OS.

===============================================================================}
unit ZLibDynamic;

{$INCLUDE '.\ZLib_defs.inc'}

interface

uses
  AuxTypes, ZLibCommon;

{===============================================================================
    Zlib functions
===============================================================================}
// for documentation, see ZLibStatic.pas

//== Procedural variables ======================================================

var
  zlibVersion:          Function: PAnsiChar; cdecl;

  deflate:              Function(strm: z_streamp; flush: int): int; cdecl;
  deflateEnd:           Function(strm: z_streamp): int; cdecl;

  inflate:              Function(strm: z_streamp; flush: int): int; cdecl;
  inflateEnd:           Function(strm: z_streamp): int; cdecl;

  deflateSetDictionary: Function(strm: z_streamp; dictionary: PByte; dictLength: uInt): int; cdecl;
  deflateGetDictionary: Function(strm: z_streamp; dictionary: PByte; dictLength: puInt): int; cdecl;
  deflateCopy:          Function(dest, source: z_streamp): int; cdecl;
  deflateReset:         Function(strm: z_streamp): int; cdecl;
  deflateParams:        Function(strm: z_streamp; level, strategy: int): int; cdecl;
  deflateTune:          Function(strm: z_streamp; good_length, max_lazy, nice_length, max_chain: int): int; cdecl;
  deflateBound:         Function(strm: z_streamp; sourceLen: uLong): uLong; cdecl;
  deflatePending:       Function(strm: z_streamp; pending: punsigned; bits: pint): int; cdecl;
  deflatePrime:         Function(strm: z_streamp; bits, value: int): int; cdecl;
  deflateSetHeader:     Function(strm: z_streamp; head: gz_headerp): int; cdecl;

  inflateSetDictionary: Function(strm: z_streamp; dictionary: PByte; dictLength: uInt): int; cdecl;
  inflateGetDictionary: Function(strm: z_streamp; dictionary: PByte; dictLength: puInt): int; cdecl;
  inflateSync:          Function(strm: z_streamp): int; cdecl;
  inflateCopy:          Function(dest, source: z_streamp): int; cdecl;
  inflateReset:         Function(strm: z_streamp): int; cdecl;
  inflateReset2:        Function(strm: z_streamp; windowBits: int): int; cdecl;
  inflatePrime:         Function(strm: z_streamp; bits, value: int): int; cdecl;
  inflateMark:          Function(strm: z_streamp): long; cdecl;
  inflateGetHeader:     Function(strm: z_streamp; head: gz_headerp): int; cdecl;

  inflateBack:          Function(strm: z_streamp; in_f: in_func; in_desc: Pointer; out_f: out_func; out_desc: Pointer): int; cdecl;
  inflateBackEnd:       Function(strm: z_streamp): int; cdecl;

  zlibCompileFlags:     Function: uLong; cdecl;

  compress:             Function(dest: PByte; destLen: puLong; source: PByte; sourceLen: uLong): int; cdecl;
  compress2:            Function(dest: PByte; destLen: puLong; source: PByte; sourceLen: uLong; level: int): int; cdecl;
  compressBound:        Function(sourceLen: uLong): uLong; cdecl;
  uncompress:           Function(dest: PByte; destLen: puLong; source: PByte; sourceLen: uLong): int; cdecl;
  uncompress2:          Function(dest: PByte; destLen: puLong; source: PByte; sourceLen: puLong): int; cdecl;

{$IFDEF GZIP_Support}
  gzopen:               Function(path: PAnsiChar; mode: PAnsiChar): gzFile; cdecl;
  gzdopen:              Function(fd: int; mode: PAnsiChar): gzFile; cdecl;
  gzbuffer:             Function(aFile: gzFile; size: unsigned): int; cdecl;
  gzsetparams:          Function(aFile: gzFile; level, strategy: int): int; cdecl;
  gzread:               Function(aFile: gzFile; buf: Pointer; len: unsigned): int; cdecl;
  gzfread:              Function(buf: Pointer; size, nitems: z_size_t; aFile: gzFile): z_size_t; cdecl;
  gzwrite:              Function(aFile: gzFile; buf: Pointer; len: unsigned): int; cdecl;
  gzfwrite:             Function(buf: Pointer; size, nintems: z_size_t; aFile: gzFile): z_size_t; cdecl;
  gzprintf:             Function(aFile: gzFile; format: PAnsiChar): int; cdecl varargs;
  gzputs:               Function(aFile: gzFile; s: PAnsiChar): int; cdecl;
  gzgets:               Function(aFile: gzFile; buf: PAnsiChar; len: int): PAnsiChar; cdecl;
  gzputc:               Function(aFile: gzFile; c: int): int; cdecl;
  gzgetc:               Function(aFile: gzFile): int; cdecl;
  gzungetc:             Function(c: int; aFile: gzFile): int; cdecl;
  gzflush:              Function(aFile: gzFile; flush: int): int; cdecl;
  gzseek:               Function(aFile: gzFile; offset: z_off_t; whence: int): z_off_t; cdecl;
  gzrewind:             Function(aFile: gzFile): int; cdecl;
  gztell:               Function(aFile: gzFile): z_off_t; cdecl;
  gzoffset:             Function(aFile: gzFile): z_off_t; cdecl;
  gzeof:                Function(aFile: gzFile): int; cdecl;
  gzdirect:             Function(aFile: gzFile): int; cdecl;
  gzclose:              Function(aFile: gzFile): int; cdecl;
  gzclose_r:            Function(aFile: gzFile): int; cdecl;
  gzclose_w:            Function(aFile: gzFile): int; cdecl;
  gzerror:              Function(aFile: gzFile; errnum: pint): PAnsiChar; cdecl;
  gzclearerr:           procedure(aFile: gzFile); cdecl;
{$ENDIF GZIP_Support}

  adler32:              Function(adler: uLong; buf: PByte; len: uInt): uLong; cdecl;
  adler32_z:            Function(adler: uLong; buf: PByte; len: z_size_t): uLong; cdecl;
  adler32_combine:      Function(adler1, adler2: uLong; len2: z_off_t): uLong; cdecl;
  crc32:                Function(crc: uLong; buf: PByte; len: uInt): uLong; cdecl;
  crc32_z:              Function(crc: uLong; buf: PByte; len: z_size_t): uLong; cdecl;
  crc32_combine:        Function(crc1, crc2: uLong; len2: z_off_t): uLong; cdecl;
  crc32_combine_gen:    Function(len2: z_off_t): uLong; cdecl;
  crc32_combine_op:     Function (crc1: uLong; crc2: uLong; op: uLong): uLong; cdecl;

  deflateInit_:         Function(strm: z_streamp; level: int; version: PAnsiChar; stream_size: int): int; cdecl;
  inflateInit_:         Function(strm: z_streamp; version: PAnsiChar; stream_size: int): int; cdecl;
  deflateInit2_:        Function(strm: z_streamp; level, method, windowBits, memLevel, strategy: int; version: PAnsiChar; stream_size: int): int; cdecl;
  inflateInit2_:        Function(strm: z_streamp; windowBits: int; version: PAnsiChar; stream_size: int): int; cdecl;
  inflateBackInit_:     Function(strm: z_streamp; windowBits: int; window: PByte; version: PAnsiChar; stream_size: int): int; cdecl;

{$IFDEF GZIP_Support}
  gzgetc_:              Function(aFile: gzFile): int; cdecl;
  gzopen64:             Function(path: PAnsiChar; mode: PAnsiChar): gzFile; cdecl;
  gzseek64:             Function(aFile: gzFile; offset: z_off64_t; whence: int): z_off64_t; cdecl;
  gztell64:             Function(aFile: gzFile): z_off64_t; cdecl;
  gzoffset64:           Function(aFile: gzFile): z_off64_t; cdecl;
{$ENDIF GZIP_Support}
  adler32_combine64:    Function(adler1, adler2: uLong; len2: z_off64_t): uLong; cdecl;
  crc32_combine64:      Function(crc1, crc2: uLong; len2: z_off64_t): uLong; cdecl;
  crc32_combine_gen64:  Function(len2: z_off64_t): uLong; cdecl;

  zError:               Function(errnum: int): PAnsiChar; cdecl;
  inflateSyncPoint:     Function(strm: z_streamp): int; cdecl;
  get_crc_table:        Function: pz_crc_t; cdecl;
  inflateUndermine:     Function(strm: z_streamp; subvert: int): int; cdecl;
  inflateValidate:      Function(strm: z_streamp; check: int): int; cdecl;
  inflateCodesUsed:     Function(strm: z_streamp): UInt32; cdecl;
  inflateResetKeep:     Function(strm: z_streamp): int; cdecl;
  deflateResetKeep:     Function(strm: z_streamp): int; cdecl;
{$IF Defined(GZIP_Support) and Defined(Windows)}
  gzopen_w:             Function(path: PWideChar; mode: PAnsiChar): gzFile; cdecl;
{$IFEND}

//== Macro functions ===========================================================

Function deflateInit(strm: z_streamp; level: int): int;{$IFDEF CanInline} inline; {$ENDIF}
Function inflateInit(strm: z_streamp): int;{$IFDEF CanInline} inline; {$ENDIF}
Function deflateInit2(strm: z_streamp; level, method, windowBits, memLevel, strategy: int): int;{$IFDEF CanInline} inline; {$ENDIF}
Function inflateInit2(strm: z_streamp; windowBits: int): int;{$IFDEF CanInline} inline; {$ENDIF}
Function inflateBackInit(strm: z_streamp; windowBits: int; window: PByte): int;{$IFDEF CanInline} inline; {$ENDIF}

//== Library initialization ====================================================

Function ZLib_Initialized: Boolean;
Function ZLib_Initialize(const LibPath: String = LibName): Boolean;
procedure ZLib_Finalize;

implementation

uses
  DynLibUtils;

//== Macro implementation ======================================================

Function deflateInit(strm: z_streamp; level: int): int;
begin
Result := deflateInit_(strm,level,PAnsiChar(ZLIB_VERSION),SizeOf(z_stream_s));
end;

//------------------------------------------------------------------------------

Function inflateInit(strm: z_streamp): int;
begin
Result := inflateInit_(strm,PAnsiChar(ZLIB_VERSION),SizeOf(z_stream_s));
end;

//------------------------------------------------------------------------------

Function deflateInit2(strm: z_streamp; level, method, windowBits, memLevel, strategy: int): int;
begin
Result := deflateInit2_(strm,level,method,windowBits,memLevel,strategy,PAnsiChar(ZLIB_VERSION),SizeOf(z_stream_s));
end;

//------------------------------------------------------------------------------

Function inflateInit2(strm: z_streamp; windowBits: int): int;
begin
Result := inflateInit2_(strm,windowBits,PAnsiChar(ZLIB_VERSION),SizeOf(z_stream_s));
end;

//------------------------------------------------------------------------------

Function inflateBackInit(strm: z_streamp; windowBits: int; window: PByte): int;
begin
Result := inflateBackInit_(strm,windowBits,window,PAnsiChar(ZLIB_VERSION),SizeOf(z_stream_s));
end;

//== Library initialization implementation =====================================

var
  ZLib_LibContext:  TDLULibraryContext;

//------------------------------------------------------------------------------

Function ZLib_Initialized: Boolean;
begin
Result := CheckLibrary(ZLib_LibContext);
end;

//------------------------------------------------------------------------------

Function ZLib_Initialize(const LibPath: String = LibName): Boolean;
begin
Result := OpenLibraryAndResolveSymbols(LibPath,ZLib_LibContext,[
  Symbol(@@zlibVersion         ,'zlibVersion'),
  // deflate
  Symbol(@@deflate             ,'deflate'),
  Symbol(@@deflateEnd          ,'deflateEnd'),
  // inflate
  Symbol(@@inflate             ,'inflate'),
  Symbol(@@inflateEnd          ,'inflateEnd'),
  // deflate - specials
  Symbol(@@deflateSetDictionary,'deflateSetDictionary'),
  Symbol(@@deflateGetDictionary,'deflateGetDictionary'),
  Symbol(@@deflateCopy         ,'deflateCopy'),
  Symbol(@@deflateReset        ,'deflateReset'),
  Symbol(@@deflateParams       ,'deflateParams'),
  Symbol(@@deflateTune         ,'deflateTune'),
  Symbol(@@deflateBound        ,'deflateBound'),
  Symbol(@@deflatePending      ,'deflatePending'),
  Symbol(@@deflatePrime        ,'deflatePrime'),
  Symbol(@@deflateSetHeader    ,'deflateSetHeader'),
  // inflate - specials
  Symbol(@@inflateSetDictionary,'inflateSetDictionary'),
  Symbol(@@inflateGetDictionary,'inflateGetDictionary'),
  Symbol(@@inflateSync         ,'inflateSync'),
  Symbol(@@inflateCopy         ,'inflateCopy'),
  Symbol(@@inflateReset        ,'inflateReset'),
  Symbol(@@inflateReset2       ,'inflateReset2'),
  Symbol(@@inflatePrime        ,'inflatePrime'),
  Symbol(@@inflateMark         ,'inflateMark'),
  Symbol(@@inflateGetHeader    ,'inflateGetHeader'),
  Symbol(@@inflateBack         ,'inflateBack'),
  Symbol(@@inflateBackEnd      ,'inflateBackEnd'),
  // itility and macro
  Symbol(@@zlibCompileFlags    ,'zlibCompileFlags'),
  Symbol(@@compress            ,'compress'),
  Symbol(@@compress2           ,'compress2'),
  Symbol(@@compressBound       ,'compressBound'),
  Symbol(@@uncompress          ,'uncompress'),
  Symbol(@@uncompress2         ,'uncompress2'),
  // gzip
{$IFDEF GZIP_Support}
  Symbol(@@gzopen              ,'gzopen'),
  Symbol(@@gzdopen             ,'gzdopen'),
  Symbol(@@gzbuffer            ,'gzbuffer'),
  Symbol(@@gzsetparams         ,'gzsetparams'),
  Symbol(@@gzread              ,'gzread'),
  Symbol(@@gzfread             ,'gzfread'),
  Symbol(@@gzwrite             ,'gzwrite'),
  Symbol(@@gzfwrite            ,'gzfwrite'),
  Symbol(@@gzprintf            ,'gzprintf'),
  Symbol(@@gzputs              ,'gzputs'),
  Symbol(@@gzgets              ,'gzgets'),
  Symbol(@@gzputc              ,'gzputc'),
  Symbol(@@gzgetc              ,'gzgetc'),
  Symbol(@@gzungetc            ,'gzungetc'),
  Symbol(@@gzflush             ,'gzflush'),
  Symbol(@@gzseek              ,'gzseek'),
  Symbol(@@gzrewind            ,'gzrewind'),
  Symbol(@@gztell              ,'gztell'),
  Symbol(@@gzoffset            ,'gzoffset'),
  Symbol(@@gzeof               ,'gzeof'),
  Symbol(@@gzdirect            ,'gzdirect'),
  Symbol(@@gzclose             ,'gzclose'),
  Symbol(@@gzclose_r           ,'gzclose_r'),
  Symbol(@@gzclose_w           ,'gzclose_w'),
  Symbol(@@gzerror             ,'gzerror'),
  Symbol(@@gzclearerr          ,'gzclearerr'),
{$ENDIF GZIP_Support}
  // checksums
  Symbol(@@adler32             ,'adler32'),
  Symbol(@@adler32_z           ,'adler32_z'),
  Symbol(@@adler32_combine     ,'adler32_combine'),
  Symbol(@@crc32               ,'crc32'),
  Symbol(@@crc32_z             ,'crc32_z'),
  Symbol(@@crc32_combine       ,'crc32_combine'),
  Symbol(@@crc32_combine_gen   ,'crc32_combine_gen'),
  Symbol(@@crc32_combine_op   ,'crc32_combine_op'),
  // macro
  Symbol(@@deflateInit_        ,'deflateInit_'),
  Symbol(@@inflateInit_        ,'inflateInit_'),
  Symbol(@@deflateInit2_       ,'deflateInit2_'),
  Symbol(@@inflateInit2_       ,'inflateInit2_'),
  Symbol(@@inflateBackInit_    ,'inflateBackInit_'),
  // large file support                    
{$IFDEF GZIP_Support}
  Symbol(@@gzgetc_             ,'gzgetc_'),
  Symbol(@@gzopen64            ,'gzopen64'),
  Symbol(@@gzseek64            ,'gzseek64'),
  Symbol(@@gztell64            ,'gztell64'),
  Symbol(@@gzoffset64          ,'gzoffset64'),
{$ENDIF GZIP_Support}
  Symbol(@@adler32_combine64   ,'adler32_combine64'),
  Symbol(@@crc32_combine64     ,'crc32_combine64'),
  Symbol(@@crc32_combine_gen64 ,'crc32_combine_gen64'),
  // undocumented
  Symbol(@@zError              ,'zError'),
  Symbol(@@inflateSyncPoint    ,'inflateSyncPoint'),
  Symbol(@@get_crc_table       ,'get_crc_table'),
  Symbol(@@inflateUndermine    ,'inflateUndermine'),
  Symbol(@@inflateValidate     ,'inflateValidate'),
  Symbol(@@inflateCodesUsed    ,'inflateCodesUsed'),
  Symbol(@@inflateResetKeep    ,'inflateResetKeep'),
  Symbol(@@deflateResetKeep    ,'deflateResetKeep')
{$IF Defined(GZIP_Support) and Defined(Windows)}
 ,Symbol(@@gzopen_w            ,'gzopen_w')
{$IFEND}
],True) = {$IFDEF GZIP_Support}{$IFDEF Windows}88{$ELSE}87{$ENDIF}{$ELSE}56{$ENDIF};
{$IFDEF CheckCompatibility}
CheckCompatibility(zlibCompileFlags);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure ZLib_Finalize;
begin
CloseLibrary(ZLib_LibContext);
end;

//== Unit initialization =======================================================

initialization
  ZLib_LibContext := DefaultLibraryContext;

end.

