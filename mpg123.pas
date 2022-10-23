{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  mpg123 library bindings

    This unit is a direct translation of C header file mpg123.h into pascal,
    and is a main part of libmpg123 library binding.

    More info about the mpg123 library can be found at: https://www.mpg123.de

  Version 1.0.4 (2020-08-11)

  Build against library version 1.25.13 (mpg123 API version 44)

  Last change 2022-09-24

  ©2018-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.mpg123

  Dependencies:
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

  Translation notes:
    - macros were expanded in-place or implemented as normal functions
    - enums were not translated to pascal enumerations, they were instead
      split into a type (an alias for int - a 32bit integer) and a set of
      constants
    - type identifiers were suffixed with _t (some types needed it to prevent
      name collisions, and to mantain consistency, it was applied to all types)
    - some constants were renamed because of symbol name collisions (added
      underscore at the start of the name)
    - some function parameters and structure fields were renamed (usually by
      adding underscore at the start of the name) because they collide with
      pascal reserved words (to, type, ...)
    - function with explicit size of offset (file handling routines) are fully
      defined and transparently available
    - large-file handling is enabled by default (symbol LARGE_FILES_SUPPORT is
      defined), meaning type off_t is an alias for off64_t and default functions
      (without _32 or _64 suffix) are silently calling 64bit-aware external
      functions
    - all comments are directly copied from the header files, no change was made
    - current translation is for Windows OS only

===============================================================================}
unit mpg123;

{$INCLUDE '.\mpg123_defs.inc'}

interface

uses
  fmt123;

(*
  libmpg123: MPEG Audio Decoder library (version 1.25.12)

  copyright 1995-2015 by the mpg123 project
  free software under the terms of the LGPL 2.1
  see COPYING and AUTHORS files in distribution or http://mpg123.org
*)

(** \file mpg123.h The header file for the libmpg123 MPEG Audio decoder *)

(** A macro to check at compile time which set of API functions to expect.
 * This should be incremented at least each time a new symbol is added
 * to the header.
 *)
const
  MPG123_API_VERSION = 44;

(* Simplified large file handling.
  I used to have a check here that prevents building for a library with conflicting large file setup
  (application that uses 32 bit offsets with library that uses 64 bits).
  While that was perfectly fine in an environment where there is one incarnation of the library,
  it hurt GNU/Linux and Solaris systems with multilib where the distribution fails to provide the
  correct header matching the 32 bit library (where large files need explicit support) or
  the 64 bit library (where there is no distinction).

  New approach: When the app defines _FILE_OFFSET_BITS, it wants non-default large file support,
  and thus functions with added suffix (mpg123_open_64).
  Any mismatch will be caught at link time because of the _FILE_OFFSET_BITS setting used when
  building libmpg123. Plus, there's dual mode large file support in mpg123 since 1.12 now.
  Link failure is not the expected outcome of any half-sane usage anymore.

  More complication: What about client code defining _LARGEFILE64_SOURCE? It might want direct access to the _64 functions, along with the ones without suffix. Well, that's possible now via defining MPG123_NO_LARGENAME and MPG123_LARGESUFFIX, respectively, for disabling or enforcing the suffix names.
*)

(*
  Now, the renaming of large file aware functions.
  By default, it appends underscore _FILE_OFFSET_BITS (so, mpg123_seek_64 for mpg123_seek), if _FILE_OFFSET_BITS is defined. You can force a different suffix via MPG123_LARGESUFFIX (that must include the underscore), or you can just disable the whole mess by defining MPG123_NO_LARGENAME.
*)
(*
********************************************************************************
        See symbol LARGE_FILES_SUPPORT and translation notes for details
********************************************************************************

#if (!defined MPG123_NO_LARGENAME) && ((defined _FILE_OFFSET_BITS) || (defined MPG123_LARGESUFFIX))

/* Need some trickery to concatenate the value(s) of the given macro(s). */
#define MPG123_MACROCAT_REALLY(a, b) a ## b
#define MPG123_MACROCAT(a, b) MPG123_MACROCAT_REALLY(a, b)
#ifndef MPG123_LARGESUFFIX
#define MPG123_LARGESUFFIX MPG123_MACROCAT(_, _FILE_OFFSET_BITS)
#endif
#define MPG123_LARGENAME(func) MPG123_MACROCAT(func, MPG123_LARGESUFFIX)

#define mpg123_open         MPG123_LARGENAME(mpg123_open)
#define mpg123_open_fd      MPG123_LARGENAME(mpg123_open_fd)
#define mpg123_open_handle  MPG123_LARGENAME(mpg123_open_handle)
#define mpg123_framebyframe_decode MPG123_LARGENAME(mpg123_framebyframe_decode)
#define mpg123_decode_frame MPG123_LARGENAME(mpg123_decode_frame)
#define mpg123_tell         MPG123_LARGENAME(mpg123_tell)
#define mpg123_tellframe    MPG123_LARGENAME(mpg123_tellframe)
#define mpg123_tell_stream  MPG123_LARGENAME(mpg123_tell_stream)
#define mpg123_seek         MPG123_LARGENAME(mpg123_seek)
#define mpg123_feedseek     MPG123_LARGENAME(mpg123_feedseek)
#define mpg123_seek_frame   MPG123_LARGENAME(mpg123_seek_frame)
#define mpg123_timeframe    MPG123_LARGENAME(mpg123_timeframe)
#define mpg123_index        MPG123_LARGENAME(mpg123_index)
#define mpg123_set_index    MPG123_LARGENAME(mpg123_set_index)
#define mpg123_position     MPG123_LARGENAME(mpg123_position)
#define mpg123_length       MPG123_LARGENAME(mpg123_length)
#define mpg123_framelength  MPG123_LARGENAME(mpg123_framelength)
#define mpg123_set_filesize MPG123_LARGENAME(mpg123_set_filesize)
#define mpg123_replace_reader MPG123_LARGENAME(mpg123_replace_reader)
#define mpg123_replace_reader_handle MPG123_LARGENAME(mpg123_replace_reader_handle)
#define mpg123_framepos MPG123_LARGENAME(mpg123_framepos)

#endif /* largefile hackery */

********************************************************************************
        See symbol LARGE_FILES_SUPPORT and translation notes for details
********************************************************************************
*)

(** \defgroup mpg123_init mpg123 library and handle setup
 *
 * Functions to initialise and shutdown the mpg123 library and handles.
 * The parameters of handles have workable defaults, you only have to tune them when you want to tune something;-)
 * Tip: Use a RVA setting...
 *
 * @{
 *)

(** Opaque structure for the libmpg123 decoder handle. *)
type
  mpg123_handle_struct_t = record end;
  mpg123_handle_struct_p = ^mpg123_handle_struct_t;

(** Opaque structure for the libmpg123 decoder handle.
 *  Most functions take a pointer to a mpg123_handle as first argument and operate on its data in an object-oriented manner.
 *)
  mpg123_handle_t = type mpg123_handle_struct_t;
  mpg123_handle_p = ^mpg123_handle_t;

(** Function to initialise the mpg123 library.
 *  This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library.
 *
 *  \return MPG123_OK if successful, otherwise an error number.
 *)
var
  mpg123_init: Function: int; cdecl;

(** Function to close down the mpg123 library.
 *  This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library. *)
  mpg123_exit: procedure; cdecl;

(** Create a handle with optional choice of decoder (named by a string, see mpg123_decoders() or mpg123_supported_decoders()).
 *  and optional retrieval of an error code to feed to mpg123_plain_strerror().
 *  Optional means: Any of or both the parameters may be NULL.
 *
 *  \param decoder optional choice of decoder variant (NULL for default)
 *  \param error optional address to store error codes
 *  \return Non-NULL pointer to fresh handle when successful.
 *)
  mpg123_new: Function(decoder: PAnsiChar; error: pint): mpg123_handle_p; cdecl;

(** Delete handle, mh is either a valid mpg123 handle or NULL.
 *  \param mh handle
 *)
  mpg123_delete: procedure(mh: mpg123_handle_p); cdecl;

(** Enumeration of the parameters types that it is possible to set/get. *)
type
  mpg123_parms_t = int;

const
  MPG123_VERBOSE       = 0;   (**< set verbosity value for enabling messages to stderr, >= 0 makes sense (integer) *)
  MPG123_FLAGS         = 1;   (**< set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX (integer) *)
  MPG123_ADD_FLAGS     = 2;   (**< add some flags (integer) *)
  MPG123_FORCE_RATE    = 3;   (**< when value > 0, force output rate to that value (integer) *)
  MPG123_DOWN_SAMPLE   = 4;   (**< 0=native rate, 1=half rate, 2=quarter rate (integer) *)
  MPG123_RVA           = 5;   (**< one of the RVA choices above (integer) *)
  MPG123_DOWNSPEED     = 6;   (**< play a frame N times (integer) *)
  MPG123_UPSPEED       = 7;   (**< play every Nth frame (integer) *)
  MPG123_START_FRAME   = 8;   (**< start with this frame (skip frames before that, integer) *)
  MPG123_DECODE_FRAMES = 9;   (**< decode only this number of frames (integer) *)
  MPG123_ICY_INTERVAL  = 10;  (**< stream contains ICY metadata with this interval (integer) *)
  MPG123_OUTSCALE      = 11;  (**< the scale for output samples (amplitude - integer or float according to mpg123 output format, normally integer) *)
  MPG123_TIMEOUT       = 12;  (**< timeout for reading from a stream (not supported on win32, integer) *)
  MPG123_REMOVE_FLAGS  = 13;  (**< remove some flags (inverse of MPG123_ADD_FLAGS, integer) *)
  MPG123_RESYNC_LIMIT  = 14;  (**< Try resync on frame parsing for that many bytes or until end of stream (<0 ... integer). This can enlarge the limit for skipping junk on beginning, too (but not reduce it).  *)
  MPG123_INDEX_SIZE    = 15;  (**< Set the frame index size (if supported). Values <0 mean that the index is allowed to grow dynamically in these steps (in positive direction, of course) -- Use this when you really want a full index with every individual frame. *)
  MPG123_PREFRAMES     = 16;  (**< Decode/ignore that many frames in advance for layer 3. This is needed to fill bit reservoir after seeking, for example (but also at least one frame in advance is needed to have all "normal" data for layer 3). Give a positive integer value, please.*)
  MPG123_FEEDPOOL      = 17;  (**< For feeder mode, keep that many buffers in a pool to avoid frequent malloc/free. The pool is allocated on mpg123_open_feed(). If you change this parameter afterwards, you can trigger growth and shrinkage during decoding. The default value could change any time. If you care about this, then set it. (integer) *)
  MPG123_FEEDBUFFER    = 18;  (**< Minimal size of one internal feeder buffer, again, the default value is subject to change. (integer) *)

(** Flag bits for MPG123_FLAGS, use the usual binary or to combine. *)
type
  mpg123_param_flags_t = int;

const
  MPG123_FORCE_MONO          = $7;     (**<     0111 Force some mono mode: This is a test bitmask for seeing if any mono forcing is active. *)
  MPG123_MONO_LEFT           = $1;     (**<     0001 Force playback of left channel only.  *)
  MPG123_MONO_RIGHT          = $2;     (**<     0010 Force playback of right channel only. *)
  MPG123_MONO_MIX            = $4;     (**<     0100 Force playback of mixed mono.         *)
  MPG123_FORCE_STEREO        = $8;     (**<     1000 Force stereo output.                  *)
  MPG123_FORCE_8BIT          = $10;    (**< 00010000 Force 8bit formats.                   *)
  MPG123_QUIET               = $20;    (**< 00100000 Suppress any printouts (overrules verbose).                    *)
  MPG123_GAPLESS             = $40;    (**< 01000000 Enable gapless decoding (default on if libmpg123 has support). *)
  MPG123_NO_RESYNC           = $80;    (**< 10000000 Disable resync stream after error.                             *)
  MPG123_SEEKBUFFER          = $100;   (**< 000100000000 Enable small buffer on non-seekable streams to allow some peek-ahead (for better MPEG sync). *)
  MPG123_FUZZY               = $200;   (**< 001000000000 Enable fuzzy seeks (guessing byte offsets or using approximate seek points from Xing TOC) *)
  MPG123_FORCE_FLOAT         = $400;   (**< 010000000000 Force floating point output (32 or 64 bits depends on mpg123 internal precision). *)
  MPG123_PLAIN_ID3TEXT       = $800;   (**< 100000000000 Do not translate ID3 text data to UTF-8. ID3 strings will contain the raw text data, with the first byte containing the ID3 encoding code. *)
  MPG123_IGNORE_STREAMLENGTH = $1000;  (**< 1000000000000 Ignore any stream length information contained in the stream, which can be contained in a 'TLEN' frame of an ID3v2 tag or a Xing tag *)
  MPG123_SKIP_ID3V2          = $2000;  (**< 10 0000 0000 0000 Do not parse ID3v2 tags, just skip them. *)
  MPG123_IGNORE_INFOFRAME    = $4000;  (**< 100 0000 0000 0000 Do not parse the LAME/Xing info frame, treat it as normal MPEG data. *)
  MPG123_AUTO_RESAMPLE       = $8000;  (**< 1000 0000 0000 0000 Allow automatic internal resampling of any kind (default on if supported). Especially when going lowlevel with replacing output buffer, you might want to unset this flag. Setting MPG123_DOWNSAMPLE or MPG123_FORCE_RATE will override this. *)
  MPG123_PICTURE             = $10000; (**< 17th bit: Enable storage of pictures from tags (ID3v2 APIC). *)
  MPG123_NO_PEEK_END         = $20000; (**< 18th bit: Do not seek to the end of
                                         *  the stream in order to probe
                                         *  the stream length and search for the id3v1 field. This also means
                                         *  the file size is unknown unless set using mpg123_set_filesize() and
                                         *  the stream is assumed as non-seekable unless overridden.
                                         *)
  MPG123_FORCE_SEEKABLE      = $40000; (**< 19th bit: Force the stream to be seekable. *)

(** choices for MPG123_RVA *)
type
  mpg123_param_rva_t = int;

const
  MPG123_RVA_OFF   = 0;                (**< RVA disabled (default).   *)
  MPG123_RVA_MIX   = 1;                (**< Use mix/track/radio gain. *)
  MPG123_RVA_ALBUM = 2;                (**< Use album/audiophile gain *)
  MPG123_RVA_MAX   = MPG123_RVA_ALBUM; (**< The maximum RVA code, may increase in future. *)

(** Set a specific parameter, for a specific mpg123_handle, using a parameter
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 *  \return MPG123_OK on success
 *)
var
  mpg123_param: Function(mh: mpg123_handle_p; atype: mpg123_parms_t; value: long; fvalue: Double): int; cdecl;

(** Get a specific parameter, for a specific mpg123_handle.
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
  mpg123_getparam: Function(mh: mpg123_handle_p; atype: mpg123_parms_t; value: plong; fvalue: PDouble): int; cdecl;

(** Feature set available for query with mpg123_feature. *)
type
  mpg123_feature_set_t = int;

const
  MPG123_FEATURE_ABI_UTF8OPEN      = 0;   (**< mpg123 expects path names to be given in UTF-8 encoding instead of plain native. *)
  MPG123_FEATURE_OUTPUT_8BIT       = 1;   (**< 8bit output   *)
  MPG123_FEATURE_OUTPUT_16BIT      = 2;   (**< 16bit output  *)
  MPG123_FEATURE_OUTPUT_32BIT      = 3;   (**< 32bit output  *)
  MPG123_FEATURE_INDEX             = 4;   (**< support for building a frame index for accurate seeking *)
  MPG123_FEATURE_PARSE_ID3V2       = 5;   (**< id3v2 parsing *)
  MPG123_FEATURE_DECODE_LAYER1     = 6;   (**< mpeg layer-1 decoder enabled *)
  MPG123_FEATURE_DECODE_LAYER2     = 7;   (**< mpeg layer-2 decoder enabled *)
  MPG123_FEATURE_DECODE_LAYER3     = 8;   (**< mpeg layer-3 decoder enabled *)
  MPG123_FEATURE_DECODE_ACCURATE   = 9;   (**< accurate decoder rounding    *)
  MPG123_FEATURE_DECODE_DOWNSAMPLE = 10;  (**< downsample (sample omit)     *)
  MPG123_FEATURE_DECODE_NTOM       = 11;  (**< flexible rate decoding       *)
  MPG123_FEATURE_PARSE_ICY         = 12;  (**< ICY support                  *)
  MPG123_FEATURE_TIMEOUT_READ      = 13;  (**< Reader with timeout (network). *)
  MPG123_FEATURE_EQUALIZER         = 14;  (**< tunable equalizer *)

(** Query libmpg123 features.
 *  \param key feature selection
 *  \return 1 for success, 0 for unimplemented functions
 *)
var
  mpg123_feature: Function(key: mpg123_feature_set_t): int; cdecl;

(* @} *)


(** \defgroup mpg123_error mpg123 error handling
 *
 * Functions to get text version of the error numbers and an enumeration
 * of the error codes returned by libmpg123.
 *
 * Most functions operating on a mpg123_handle simply return MPG123_OK (0)
 * on success and MPG123_ERR (-1) on failure, setting the internal error
 * variable of the handle to the specific error code. If there was not a valid
 * (non-NULL) handle provided to a function operating on one, MPG123_BAD_HANDLE
 * may be returned if this can not be confused with a valid positive return
 * value.
 * Meaning: A function expected to return positive integers on success will
 * always indicate error or a special condition by returning a negative one.
 *
 * Decoding/seek functions may also return message codes MPG123_DONE,
 * MPG123_NEW_FORMAT and MPG123_NEED_MORE (all negative, see below on how to
 * react). Note that calls to those can be nested, so generally watch out
 * for these codes after initial handle setup.
 * Especially any function that needs information about the current stream
 * to work will try to at least parse the beginning if that did not happen
 * yet.
 *
 * On a function that is supposed to return MPG123_OK on success and
 * MPG123_ERR on failure, make sure you check for != MPG123_OK, not
 * == MPG123_ERR, as the error code could get more specific in future,
 * or there is just a special message from a decoding routine as indicated
 * above.
 *
 * @{
 *)

(** Enumeration of the message and error codes and returned by libmpg123 functions. *)
type
  mpg123_errors_t = int;

const
  MPG123_DONE              = -12; (**< Message: Track ended. Stop decoding. *)
  MPG123_NEW_FORMAT        = -11; (**< Message: Output format will be different on next call. Note that some libmpg123 versions between 1.4.3 and 1.8.0 insist on you calling mpg123_getformat() after getting this message code. Newer verisons behave like advertised: You have the chance to call mpg123_getformat(), but you can also just continue decoding and get your data. *)
  MPG123_NEED_MORE         = -10; (**< Message: For feed reader: "Feed me more!" (call mpg123_feed() or mpg123_decode() with some new input data). *)
  MPG123_ERR               = -1;  (**< Generic Error *)
  MPG123_OK                = 0;   (**< Success *)
  MPG123_BAD_OUTFORMAT     = 1;   (**< Unable to set up output format! *)
  MPG123_BAD_CHANNEL       = 2;   (**< Invalid channel number specified. *)
  MPG123_BAD_RATE          = 3;   (**< Invalid sample rate specified.  *)
  MPG123_ERR_16TO8TABLE    = 4;   (**< Unable to allocate memory for 16 to 8 converter table! *)
  MPG123_BAD_PARAM         = 5;   (**< Bad parameter id! *)
  MPG123_BAD_BUFFER        = 6;   (**< Bad buffer given -- invalid pointer or too small size. *)
  MPG123_OUT_OF_MEM        = 7;   (**< Out of memory -- some malloc() failed. *)
  MPG123_NOT_INITIALIZED   = 8;   (**< You didn't initialize the library! *)
  MPG123_BAD_DECODER       = 9;   (**< Invalid decoder choice. *)
  MPG123_BAD_HANDLE        = 10;  (**< Invalid mpg123 handle. *)
  MPG123_NO_BUFFERS        = 11;  (**< Unable to initialize frame buffers (out of memory?). *)
  MPG123_BAD_RVA           = 12;  (**< Invalid RVA mode. *)
  MPG123_NO_GAPLESS        = 13;  (**< This build doesn't support gapless decoding. *)
  MPG123_NO_SPACE          = 14;  (**< Not enough buffer space. *)
  MPG123_BAD_TYPES         = 15;  (**< Incompatible numeric data types. *)
  MPG123_BAD_BAND          = 16;  (**< Bad equalizer band. *)
  MPG123_ERR_NULL          = 17;  (**< Null pointer given where valid storage address needed. *)
  MPG123_ERR_READER        = 18;  (**< Error reading the stream. *)
  MPG123_NO_SEEK_FROM_END  = 19;  (**< Cannot seek from end (end is not known). *)
  MPG123_BAD_WHENCE        = 20;  (**< Invalid 'whence' for seek function.*)
  MPG123_NO_TIMEOUT        = 21;  (**< Build does not support stream timeouts. *)
  MPG123_BAD_FILE          = 22;  (**< File access error. *)
  MPG123_NO_SEEK           = 23;  (**< Seek not supported by stream. *)
  MPG123_NO_READER         = 24;  (**< No stream opened. *)
  MPG123_BAD_PARS          = 25;  (**< Bad parameter handle. *)
  MPG123_BAD_INDEX_PAR     = 26;  (**< Bad parameters to mpg123_index() and mpg123_set_index() *)
  MPG123_OUT_OF_SYNC       = 27;  (**< Lost track in bytestream and did not try to resync. *)
  MPG123_RESYNC_FAIL       = 28;  (**< Resync failed to find valid MPEG data. *)
  MPG123_NO_8BIT           = 29;  (**< No 8bit encoding possible. *)
  MPG123_BAD_ALIGN         = 30;  (**< Stack aligmnent error *)
  MPG123_NULL_BUFFER       = 31;  (**< NULL input buffer with non-zero size... *)
  MPG123_NO_RELSEEK        = 32;  (**< Relative seek not possible (screwed up file offset) *)
  MPG123_NULL_POINTER      = 33;  (**< You gave a null pointer somewhere where you shouldn't have. *)
  MPG123_BAD_KEY           = 34;  (**< Bad key value given. *)
  MPG123_NO_INDEX          = 35;  (**< No frame index in this build. *)
  MPG123_INDEX_FAIL        = 36;  (**< Something with frame index went wrong. *)
  MPG123_BAD_DECODER_SETUP = 37;  (**< Something prevents a proper decoder setup *)
  MPG123_MISSING_FEATURE   = 38;  (**< This feature has not been built into libmpg123. *)
  MPG123_BAD_VALUE         = 39;  (**< A bad value has been given, somewhere. *)
  MPG123_LSEEK_FAILED      = 40;  (**< Low-level seek failed. *)
  MPG123_BAD_CUSTOM_IO     = 41;  (**< Custom I/O not prepared. *)
  MPG123_LFS_OVERFLOW      = 42;  (**< Offset value overflow during translation of large file API calls -- your client program cannot handle that large file. *)
  MPG123_INT_OVERFLOW      = 43;  (**< Some integer overflow. *)

(** Look up error strings given integer code.
 *  \param errcode integer error code
 *  \return string describing what that error error code means
 *)
var
  mpg123_plain_strerror: Function(errcode: int): PAnsiChar; cdecl;

(** Give string describing what error has occured in the context of handle mh.
 *  When a function operating on an mpg123 handle returns MPG123_ERR, you should check for the actual reason via
 *  char *errmsg = mpg123_strerror(mh)
 *  This function will catch mh == NULL and return the message for MPG123_BAD_HANDLE.
 *  \param mh handle
 *  \return error message
 *)
  mpg123_strerror: Function(mh: mpg123_handle_p): PAnsiChar; cdecl;

(** Return the plain errcode intead of a string.
 *  \param mh handle
 *  \return error code recorded in handle or MPG123_BAD_HANDLE
 *)
  mpg123_errcode: Function(mh: mpg123_handle_p): int; cdecl;

(*@}*)


(** \defgroup mpg123_decoder mpg123 decoder selection
 *
 * Functions to list and select the available decoders.
 * Perhaps the most prominent feature of mpg123: You have several (optimized) decoders to choose from (on x86 and PPC (MacOS) systems, that is).
 *
 * @{
 *)

(** Get available decoder list.
 *  \return NULL-terminated array of generally available decoder names (plain 8bit ASCII)
 *)
  mpg123_decoders: Function: PPAnsiChar; cdecl;

(** Get supported decoder list.
 *  \return NULL-terminated array of the decoders supported by the CPU (plain 8bit ASCII)
 *)
  mpg123_supported_decoders: Function: PPAnsiChar; cdecl;

(** Set the active decoder.
 *  \param mh handle
 *  \param decoder_name name of decoder
 *  \return MPG123_OK on success
 *)
  mpg123_decoder: Function(mh: mpg123_handle_p; decoder_name: PAnsiChar): int; cdecl;

(** Get the currently active decoder name.
 *  The active decoder engine can vary depening on output constraints,
 *  mostly non-resampling, integer output is accelerated via 3DNow & Co. but for
 *  other modes a fallback engine kicks in.
 *  Note that this can return a decoder that is only active in the hidden and not
 *  available as decoder choice from the outside.
 *  \param mh handle
 *  \return The decoder name or NULL on error.
 *)
  mpg123_current_decoder: Function(mh: mpg123_handle_p): PAnsiChar; cdecl;

(*@}*)


(** \defgroup mpg123_output mpg123 output audio format
 *
 * Functions to get and select the format of the decoded audio.
 *
 * Before you dive in, please be warned that you might get confused by this. This seems to happen a lot, therefore I am trying to explain in advance.
 *
 * The mpg123 library decides what output format to use when encountering the first frame in a stream, or actually any frame that is still valid but differs from the frames before in the prompted output format. At such a deciding point, an internal table of allowed encodings, sampling rates and channel setups is consulted. According to this table, an output format is chosen and the decoding engine set up accordingly (including optimized routines for different output formats). This might seem unusual but it just follows from the non-existence of "MPEG audio files" with defined overall properties. There are streams, streams are concatenations of (semi) independent frames. We store streams on disk and call them "MPEG audio files", but that does not change their nature as the decoder is concerned (the LAME/Xing header for gapless decoding makes things interesting again).
 *
 * To get to the point: What you do with mpg123_format() and friends is to fill the internal table of allowed formats before it is used. That includes removing support for some formats or adding your forced sample rate (see MPG123_FORCE_RATE) that will be used with the crude internal resampler. Also keep in mind that the sample encoding is just a question of choice -- the MPEG frames do only indicate their native sampling rate and channel count. If you want to decode to integer or float samples, 8 or 16 bit ... that is your decision. In a "clean" world, libmpg123 would always decode to 32 bit float and let you handle any sample conversion. But there are optimized routines that work faster by directly decoding to the desired encoding / accuracy. We prefer efficiency over conceptual tidyness.
 *
 * People often start out thinking that mpg123_format() should change the actual decoding format on the fly. That is wrong. It only has effect on the next natural change of output format, when libmpg123 will consult its format table again. To make life easier, you might want to call mpg123_format_none() before any thing else and then just allow one desired encoding and a limited set of sample rates / channel choices that you actually intend to deal with. You can force libmpg123 to decode everything to 44100 KHz, stereo, 16 bit integer ... it will duplicate mono channels and even do resampling if needed (unless that feature is disabled in the build, same with some encodings). But I have to stress that the resampling of libmpg123 is very crude and doesn't even contain any kind of "proper" interpolation.
 *
 * In any case, watch out for MPG123_NEW_FORMAT as return message from decoding routines and call mpg123_getformat() to get the currently active output format.
 *
 * @{
 *)

(** They can be combined into one number (3) to indicate mono and stereo... *)
type
  mpg123_channelcount_t = int;

const
  MPG123_MONO   = 1;  (**< mono *)
  MPG123_STEREO = 2;  (**< stereo *)

(** An array of supported standard sample rates
 *  These are possible native sample rates of MPEG audio files.
 *  You can still force mpg123 to resample to a different one, but by default you will only get audio in one of these samplings.
 *  \param list Store a pointer to the sample rates array there.
 *  \param number Store the number of sample rates there. *)
var
  mpg123_rates: procedure(list: pplong; number: psize_t); cdecl;

(** An array of supported audio encodings.
 *  An audio encoding is one of the fully qualified members of mpg123_enc_enum (MPG123_ENC_SIGNED_16, not MPG123_SIGNED).
 *  \param list Store a pointer to the encodings array there.
 *  \param number Store the number of encodings there. *)
  mpg123_encodings: procedure(list: ppint; number: psize_t); cdecl;

(** Return the size (in bytes) of one mono sample of the named encoding.
 * \param encoding The encoding value to analyze.
 * \return positive size of encoding in bytes, 0 on invalid encoding. *)
  mpg123_encsize: Function(encoding: int): int; cdecl;

(** Configure a mpg123 handle to accept no output format at all,
 *  use before specifying supported formats with mpg123_format
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_format_none: Function(mh: mpg123_handle_p): int; cdecl;

(** Configure mpg123 handle to accept all formats
 *  (also any custom rate you may set) -- this is default.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_format_all: Function(mh: mpg123_handle_p): int; cdecl;

(** Set the audio format support of a mpg123_handle in detail:
 *  \param mh handle
 *  \param rate The sample rate value (in Hertz).
 *  \param channels A combination of MPG123_STEREO and MPG123_MONO.
 *  \param encodings A combination of accepted encodings for rate and channels, p.ex MPG123_ENC_SIGNED16 | MPG123_ENC_ULAW_8 (or 0 for no support). Please note that some encodings may not be supported in the library build and thus will be ignored here.
 *  \return MPG123_OK on success, MPG123_ERR if there was an error. *)
  mpg123_format: Function(mh: mpg123_handle_p; rate: long; channels, encodings: int): int; cdecl;

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_handle.
 *  \param mh handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO, 
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
  mpg123_format_support: Function(mh: mpg123_handle_p; rate: long; encoding: int): int; cdecl;

(** Get the current output format written to the addresses given.
 *  If the stream is freshly loaded, this will try to parse enough
 *  of it to give you the format to come. This clears the flag that
 *  would otherwise make the first decoding call return
 *  MPG123_NEW_FORMAT.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \return MPG123_OK on success
 *)
  mpg123_getformat: Function(mh: mpg123_handle_p; rate: plong; channels, encoding: pint): int; cdecl;

(** Get the current output format written to the addresses given.
 *  This differs from plain mpg123_getformat() in that you can choose
 *  _not_ to clear the flag that would trigger the next decoding call
 *  to return MPG123_NEW_FORMAT in case of a new format arriving.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \param clear_flag if true, clear internal format flag
 *  \return MPG123_OK on success
 *)
  mpg123_getformat2: Function(mh: mpg123_handle_p; rate: plong; channels, encoding: pint; clear_flag: int): int; cdecl;

(*@}*)


(** \defgroup mpg123_input mpg123 file input and decoding
 *
 * Functions for input bitstream and decoding operations.
 * Decoding/seek functions may also return message codes MPG123_DONE, MPG123_NEW_FORMAT and MPG123_NEED_MORE (please read up on these on how to react!).
 * @{
 *)

(* reading samples / triggering decoding, possible return values: *)
(** Enumeration of the error codes returned by libmpg123 functions. *)

(** Open and prepare to decode the specified file by filesystem path.
 *  This does not open HTTP urls; libmpg123 contains no networking code.
 *  If you want to decode internet streams, use mpg123_open_fd() or mpg123_open_feed().
 *  \param mh handle
 *  \param path filesystem path
 *  \return MPG123_OK on success
 *)
  mpg123_open: Function(mh: mpg123_handle_p; path: PAnsiChar): int; cdecl;
  mpg123_open_32: Function(mh: mpg123_handle_p; path: PAnsiChar): int; cdecl;
  mpg123_open_64: Function(mh: mpg123_handle_p; path: PAnsiChar): int; cdecl;

(** Use an already opened file descriptor as the bitstream input
 *  mpg123_close() will _not_ close the file descriptor.
 *  \param mh handle
 *  \param fd file descriptor
 *  \return MPG123_OK on success
 *)
  mpg123_open_fd: Function(mh: mpg123_handle_p; fd: int): int; cdecl;
  mpg123_open_fd_32: Function(mh: mpg123_handle_p; fd: int): int; cdecl;
  mpg123_open_fd_64: Function(mh: mpg123_handle_p; fd: int): int; cdecl;

(** Use an opaque handle as bitstream input. This works only with the
 *  replaced I/O from mpg123_replace_reader_handle()!
 *  mpg123_close() will call the cleanup callback for your handle (if you gave one).
 *  \param mh handle
 *  \param iohandle your handle
 *  \return MPG123_OK on success
 *)
  mpg123_open_handle: Function(mh: mpg123_handle_p; iohandle: Pointer): int; cdecl;
  mpg123_open_handle_32: Function(mh: mpg123_handle_p; iohandle: Pointer): int; cdecl;
  mpg123_open_handle_64: Function(mh: mpg123_handle_p; iohandle: Pointer): int; cdecl;

(** Open a new bitstream and prepare for direct feeding
 *  This works together with mpg123_decode(); you are responsible for reading and feeding the input bitstream.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_open_feed: Function(mh: mpg123_handle_p): int; cdecl;

(** Closes the source, if libmpg123 opened it.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_close: Function(mh: mpg123_handle_p): int; cdecl;

(** Read from stream and decode up to outmemsize bytes.
 *  \param mh handle
 *  \param outmemory address of output buffer to write to
 *  \param outmemsize maximum number of bytes to write
 *  \param done address to store the number of actually decoded bytes to
 *  \return MPG123_OK or error/message code
 *)
  mpg123_read: Function(mh: mpg123_handle_p; outmemory: PByte; outmemsize: size_t; done: psize_t): int; cdecl;

(** Feed data for a stream that has been opened with mpg123_open_feed().
 *  It's give and take: You provide the bytestream, mpg123 gives you the decoded samples.
 *  \param mh handle
 *  \param in input buffer
 *  \param size number of input bytes
 *  \return MPG123_OK or error/message code.
 *)
  mpg123_feed: Function(mh: mpg123_handle_p; inbuff: PByte; size: size_t): int; cdecl;

(** Decode MPEG Audio from inmemory to outmemory. 
 *  This is very close to a drop-in replacement for old mpglib.
 *  When you give zero-sized output buffer the input will be parsed until 
 *  decoded data is available. This enables you to get MPG123_NEW_FORMAT (and query it) 
 *  without taking decoded data.
 *  Think of this function being the union of mpg123_read() and mpg123_feed() (which it actually is, sort of;-).
 *  You can actually always decide if you want those specialized functions in separate steps or one call this one here.
 *  \param mh handle
 *  \param inmemory input buffer
 *  \param inmemsize number of input bytes
 *  \param outmemory output buffer
 *  \param outmemsize maximum number of output bytes
 *  \param done address to store the number of actually decoded bytes to
 *  \return error/message code (watch out especially for MPG123_NEED_MORE)
 *)
  mpg123_decode: Function(mh: mpg123_handle_p; inmemory: PByte; inmemsize: size_t; outmemory: PByte; outmemsize: size_t; done: psize_t): int; cdecl;

(** Decode next MPEG frame to internal buffer
 *  or read a frame and return after setting a new format.
 *  \param mh handle
 *  \param num current frame offset gets stored there
 *  \param audio This pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
  mpg123_decode_frame: Function(mh: mpg123_handle_p; num: poff_t; audio: PPByte; bytes: psize_t): int; cdecl;
  mpg123_decode_frame_32: Function(mh: mpg123_handle_p; num: poff32_t; audio: PPByte; bytes: psize_t): int; cdecl;
  mpg123_decode_frame_64: Function(mh: mpg123_handle_p; num: poff64_t; audio: PPByte; bytes: psize_t): int; cdecl;

(** Decode current MPEG frame to internal buffer.
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \param num last frame offset gets stored there
 *  \param audio this pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
  mpg123_framebyframe_decode: Function(mh: mpg123_handle_p; num: poff_t; audio: PPByte; bytes: psize_t): int; cdecl;
  mpg123_framebyframe_decode_32: Function(mh: mpg123_handle_p; num: poff32_t; audio: PPByte; bytes: psize_t): int; cdecl;
  mpg123_framebyframe_decode_64: Function(mh: mpg123_handle_p; num: poff64_t; audio: PPByte; bytes: psize_t): int; cdecl;

(** Find, read and parse the next mp3 frame
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \return MPG123_OK or error/message code
 *)
  mpg123_framebyframe_next: Function(mh: mpg123_handle_p): int; cdecl;

(** Get access to the raw input data for the last parsed frame.
 * This gives you a direct look (and write access) to the frame body data.
 * Together with the raw header, you can reconstruct the whole raw MPEG stream without junk and meta data, or play games by actually modifying the frame body data before decoding this frame (mpg123_framebyframe_decode()).
 * A more sane use would be to use this for CRC checking (see mpg123_info() and MPG123_CRC), the first two bytes of the body make up the CRC16 checksum, if present.
 * You can provide NULL for a parameter pointer when you are not interested in the value.
 *
 * \param mh handle
 * \param header the 4-byte MPEG header
 * \param bodydata pointer to the frame body stored in the handle (without the header)
 * \param bodybytes size of frame body in bytes (without the header)
 * \return MPG123_OK if there was a yet un-decoded frame to get the
 *    data from, MPG123_BAD_HANDLE or MPG123_ERR otherwise (without further
 *    explanation, the error state of the mpg123_handle is not modified by
 *    this function).
 *)
  mpg123_framedata: Function(mh: mpg123_handle_p; header: pulong; bodydata: PPByte; bodybytes: psize_t): int; cdecl;

(** Get the input position (byte offset in stream) of the last parsed frame.
 *  This can be used for external seek index building, for example.
 *  It just returns the internally stored offset, regardless of validity --
 *  you ensure that a valid frame has been parsed before!
 * \param mh handle
 * \return byte offset in stream
 *)
  mpg123_framepos: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_framepos_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_framepos_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(*@}*)


(** \defgroup mpg123_seek mpg123 position and seeking
 *
 * Functions querying and manipulating position in the decoded audio bitstream.
 * The position is measured in decoded audio samples, or MPEG frame offset for the specific functions.
 * If gapless code is in effect, the positions are adjusted to compensate the skipped padding/delay - meaning, you should not care about that at all and just use the position defined for the samples you get out of the decoder;-)
 * The general usage is modelled after stdlib's ftell() and fseek().
 * Especially, the whence parameter for the seek functions has the same meaning as the one for fseek() and needs the same constants from stdlib.h: 
 * - SEEK_SET: set position to (or near to) specified offset
 * - SEEK_CUR: change position by offset from now
 * - SEEK_END: set position to offset from end
 *
 * Note that sample-accurate seek only works when gapless support has been enabled at compile time; seek is frame-accurate otherwise.
 * Also, really sample-accurate seeking (meaning that you get the identical sample value after seeking compared to plain decoding up to the position) is only guaranteed when you do not mess with the position code by using MPG123_UPSPEED, MPG123_DOWNSPEED or MPG123_START_FRAME. The first two mainly should cause trouble with NtoM resampling, but in any case with these options in effect, you have to keep in mind that the sample offset is not the same as counting the samples you get from decoding since mpg123 counts the skipped samples, too (or the samples played twice only once)!
 * Short: When you care about the sample position, don't mess with those parameters;-)
 * Also, seeking is not guaranteed to work for all streams (underlying stream may not support it).
 * And yet another caveat: If the stream is concatenated out of differing pieces (Frankenstein stream), seeking may suffer, too.
 *
 * @{
 *)

(** Returns the current position in samples.
 *  On the next successful read, you'd get that sample.
 *  \param mh handle
 *  \return sample offset or MPG123_ERR (null handle)
 *)
  mpg123_tell: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_tell_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_tell_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(** Returns the frame number that the next read will give you data from.
 *  \param mh handle
 *  \return frame offset or MPG123_ERR (null handle)
 *)
  mpg123_tellframe: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_tellframe_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_tellframe_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(** Returns the current byte offset in the input stream.
 *  \param mh handle
 *  \return byte offset or MPG123_ERR (null handle)
 *)
  mpg123_tell_stream: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_tell_stream_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_tell_stream_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(** Seek to a desired sample offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param sampleoff offset in PCM samples
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code
 *)
  mpg123_seek: Function(mh: mpg123_handle_p; samleoff: off_t; whence: int): off_t; cdecl;
  mpg123_seek_32: Function(mh: mpg123_handle_p; samleoff: off32_t; whence: int): off32_t; cdecl;
  mpg123_seek_64: Function(mh: mpg123_handle_p; samleoff: off64_t; whence: int): off64_t; cdecl;

(** Seek to a desired sample offset in data feeding mode. 
 *  This just prepares things to be right only if you ensure that the next chunk of input data will be from input_offset byte position.
 *  \param mh handle
 *  \param sampleoff offset in PCM samples
 *  \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 *  \param input_offset The position it expects to be at the 
 *                      next time data is fed to mpg123_decode().
 *  \return The resulting offset >= 0 or error/message code *)
  mpg123_feedseek: Function(mh: mpg123_handle_p; samleoff: off_t; whence: int; input_offset: poff_t): off_t; cdecl;
  mpg123_feedseek_32: Function(mh: mpg123_handle_p; samleoff: off32_t; whence: int; input_offset: poff32_t): off32_t; cdecl;
  mpg123_feedseek_64: Function(mh: mpg123_handle_p; samleoff: off64_t; whence: int; input_offset: poff64_t): off64_t; cdecl;

(** Seek to a desired MPEG frame offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param frameoff offset in MPEG frames
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code *)
  mpg123_seek_frame: Function(mh: mpg123_handle_p; frameoff: off_t; whence: int): off_t; cdecl;
  mpg123_seek_frame_32: Function(mh: mpg123_handle_p; frameoff: off32_t; whence: int): off32_t; cdecl;
  mpg123_seek_frame_64: Function(mh: mpg123_handle_p; frameoff: off64_t; whence: int): off64_t; cdecl;

(** Return a MPEG frame offset corresponding to an offset in seconds.
 *  This assumes that the samples per frame do not change in the file/stream, which is a good assumption for any sane file/stream only.
 *  \return frame offset >= 0 or error/message code *)
  mpg123_timeframe: Function(mh: mpg123_handle_p; dec: Double): off_t; cdecl;
  mpg123_timeframe_32: Function(mh: mpg123_handle_p; dec: Double): off32_t; cdecl;
  mpg123_timeframe_64: Function(mh: mpg123_handle_p; dec: Double): off64_t; cdecl;

(** Give access to the frame index table that is managed for seeking.
 *  You are asked not to modify the values... Use mpg123_set_index to set the
 *  seek index
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step one index byte offset advances this many MPEG frames
 *  \param fill number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
  mpg123_index: Function(mh: mpg123_handle_p; offsets: ppoff_t; step: poff_t; fill: psize_t): int; cdecl;
  mpg123_index_32: Function(mh: mpg123_handle_p; offsets: ppoff32_t; step: poff32_t; fill: psize_t): int; cdecl;
  mpg123_index_64: Function(mh: mpg123_handle_p; offsets: ppoff64_t; step: poff64_t; fill: psize_t): int; cdecl;

(** Set the frame index table
 *  Setting offsets to NULL and fill > 0 will allocate fill entries. Setting offsets
 *  to NULL and fill to 0 will clear the index and free the allocated memory used by the index.
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step    one index byte offset advances this many MPEG frames
 *  \param fill    number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
  mpg123_set_index: Function(mh: mpg123_handle_p; offsets: poff_t; step: off_t; fill: size_t): int; cdecl;
  mpg123_set_index_32: Function(mh: mpg123_handle_p; offsets: poff32_t; step: off32_t; fill: size_t): int; cdecl;
  mpg123_set_index_64: Function(mh: mpg123_handle_p; offsets: poff64_t; step: off64_t; fill: size_t): int; cdecl;

(** An old crutch to keep old mpg123 binaries happy.
 *  WARNING: This function is there only to avoid runtime linking errors with
 *  standalone mpg123 before version 1.23.0 (if you strangely update the
 *  library but not the end-user program) and actually is broken
 *  for various cases (p.ex. 24 bit output). Do never use. It might eventually
 *  be purged from the library.
 *)
  mpg123_position: Function(mh: mpg123_handle_p; frame_offset, buffered_bytes: off_t; current_frame, frames_left: poff_t; current_seconds, seconds_left: PDouble): int; cdecl;
  mpg123_position_32: Function(mh: mpg123_handle_p; frame_offset, buffered_bytes: off32_t; current_frame, frames_left: poff32_t; current_seconds, seconds_left: PDouble): int; cdecl;
  mpg123_position_64: Function(mh: mpg123_handle_p; frame_offset, buffered_bytes: off64_t; current_frame, frames_left: poff64_t; current_seconds, seconds_left: PDouble): int; cdecl;

(*@}*)


(** \defgroup mpg123_voleq mpg123 volume and equalizer
 *
 * @{
 *)

(** another channel enumeration, for left/right choice *)
type
  mpg123_channels_t = int;

const
  MPG123_LEFT  = $1;  (**< The Left Channel. *)
  MPG123_RIGHT = $2;  (**< The Right Channel. *)
  MPG123_LR    = $3;  (**< Both left and right channel; same as MPG123_LEFT|MPG123_RIGHT *)

(** Set the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \param val The (linear) adjustment factor.
 *  \return MPG123_OK on success
 *)
var
  mpg123_eq: Function(mh: mpg123_handle_p; channel: mpg123_channels_t; band: int; val: Double): int; cdecl;

(** Get the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for (arithmetic mean of) both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \return The (linear) adjustment factor (zero for pad parameters) *)
  mpg123_geteq: Function(mh: mpg123_handle_p; channel: mpg123_channels_t; band: int): Double; cdecl;

(** Reset the 32 Band Audio Equalizer settings to flat
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_reset_eq: Function(mh: mpg123_handle_p): int; cdecl;

(** Set the absolute output volume including the RVA setting,
 *  vol<0 just applies (a possibly changed) RVA setting.
 *  \param mh handle
 *  \param vol volume value (linear factor)
 *  \return MPG123_OK on success
 *)
  mpg123_volume: Function(mh: mpg123_handle_p; vol: Double): int; cdecl;

(** Adjust output volume including the RVA setting by chosen amount
 *  \param mh handle
 *  \param change volume value (linear factor increment)
 *  \return MPG123_OK on success
 *)
  mpg123_volume_change: Function(mh: mpg123_handle_p; change: Double): int; cdecl;

(** Return current volume setting, the actual value due to RVA, and the RVA
 *  adjustment itself. It's all as double float value to abstract the sample 
 *  format. The volume values are linear factors / amplitudes (not percent) 
 *  and the RVA value is in decibels.
 *  \param mh handle
 *  \param base return address for base volume (linear factor)
 *  \param really return address for actual volume (linear factor)
 *  \param rva_db return address for RVA value (decibels)
 *  \return MPG123_OK on success
 *)
  mpg123_getvolume: Function(mh: mpg123_handle_p; base, really, rva_db: PDouble): int; cdecl;

(* TODO: Set some preamp in addition / to replace internal RVA handling? *)

(*@}*)


(** \defgroup mpg123_status mpg123 status and information
 *
 * @{
 *)

(** Enumeration of the mode types of Variable Bitrate *)
type
  mpg123_vbr_t = int;

const
  MPG123_CBR = 0; (**< Constant Bitrate Mode (default) *)
  MPG123_VBR = 1; (**< Variable Bitrate Mode *)
  MPG123_ABR = 2; (**< Average Bitrate Mode *)

(** Enumeration of the MPEG Versions *)
type
  mpg123_version_t = int;

const
  MPG123_1_0 = 0; (**< MPEG Version 1.0 *)
  MPG123_2_0 = 1; (**< MPEG Version 2.0 *)
  MPG123_2_5 = 2; (**< MPEG Version 2.5 *)


(** Enumeration of the MPEG Audio mode.
 *  Only the mono mode has 1 channel, the others have 2 channels. *)
type
  mpg123_mode_t = int;

const
  MPG123_M_STEREO = 0;  (**< Standard Stereo. *)
  MPG123_M_JOINT  = 1;  (**< Joint Stereo. *)
  MPG123_M_DUAL   = 2;  (**< Dual Channel. *)
  MPG123_M_MONO   = 3;  (**< Single Channel. *)


(** Enumeration of the MPEG Audio flag bits *)
type
  mpg123_flags_t = int;

const
  MPG123_CRC       = $1;  (**< The bitstream is error protected using 16-bit CRC. *)
  MPG123_COPYRIGHT = $2;  (**< The bitstream is copyrighted. *)
  MPG123_PRIVATE   = $4;  (**< The private bit has been set. *)
  MPG123_ORIGINAL  = $8;  (**< The bitstream is an original, not a copy. *)

(** Data structure for storing information about a frame of MPEG Audio *)
type
  mpg123_frameinfo_t = record
    version:    mpg123_version_t; (**< The MPEG version (1.0/2.0/2.5). *)
    layer:      int;              (**< The MPEG Audio Layer (MP1/MP2/MP3). *)
    rate:       long;             (**< The sampling rate in Hz. *)
    mode:       mpg123_mode_t;    (**< The audio mode (Mono, Stereo, Joint-stero, Dual Channel). *)
    mode_ext:   int;              (**< The mode extension bit flag. *)
    framesize:  int;              (**< The size of the frame (in bytes, including header). *)
    flags:      mpg123_flags_t;   (**< MPEG Audio flag bits. Just now I realize that it should be declared as int, not enum. It's a bitwise combination of the enum values. *)
    emphasis:   int;              (**< The emphasis type. *)
    bitrate:    int;              (**< Bitrate of the frame (kbps). *)
    abr_rate:   int;              (**< The target average bitrate. *)
    vbr:        mpg123_vbr_t;     (**< The VBR mode. *)
  end;
  mpg123_frameinfo_p = ^mpg123_frameinfo_t;

(** Get frame information about the MPEG audio bitstream and store it in a mpg123_frameinfo structure.
 *  \param mh handle
 *  \param mi address of existing frameinfo structure to write to
 *  \return MPG123_OK on success
 *)
var
  mpg123_info: Function(mh: mpg123_handle_p; mi: mpg123_frameinfo_p): int; cdecl;

(** Get the safe output buffer size for all cases
 *  (when you want to replace the internal buffer)
 *  \return safe buffer size
 *)
  mpg123_safe_buffer: Function: size_t; cdecl;

(** Make a full parsing scan of each frame in the file. ID3 tags are found. An
 *  accurate length value is stored. Seek index will be filled. A seek back to
 *  current position is performed. At all, this function refuses work when
 *  stream is not seekable.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
  mpg123_scan: Function(mh: mpg123_handle_p): int; cdecl;

(** Return, if possible, the full (expected) length of current track in frames.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
  mpg123_framelength: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_framelength_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_framelength_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(** Return, if possible, the full (expected) length of current track in samples.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
  mpg123_length: Function(mh: mpg123_handle_p): off_t; cdecl;
  mpg123_length_32: Function(mh: mpg123_handle_p): off32_t; cdecl;
  mpg123_length_64: Function(mh: mpg123_handle_p): off64_t; cdecl;

(** Override the value for file size in bytes.
 *  Useful for getting sensible track length values in feed mode or for HTTP streams.
 *  \param mh handle
 *  \param size file size in bytes
 *  \return MPG123_OK on success
 *)
  mpg123_set_filesize: Function(mh: mpg123_handle_p; size: off_t): int; cdecl;
  mpg123_set_filesize_32: Function(mh: mpg123_handle_p; size: off32_t): int; cdecl;
  mpg123_set_filesize_64: Function(mh: mpg123_handle_p; size: off64_t): int; cdecl;

(** Get MPEG frame duration in seconds.
 *  \param mh handle
 *  \return frame duration in seconds, <0 on error
 *)
  mpg123_tpf: Function(mh: mpg123_handle_p): Double; cdecl;

(** Get MPEG frame duration in samples.
 *  \param mh handle
 *  \return samples per frame for the most recently parsed frame; <0 on errors
 *)
  mpg123_spf: Function(mh: mpg123_handle_p): int; cdecl;

(** Get and reset the clip count.
 *  \param mh handle
 *  \return count of clipped samples
 *)
  mpg123_clip: Function(mh: mpg123_handle_p): long; cdecl;

(** The key values for state information from mpg123_getstate(). *)
type
  mpg123_state_t = int;

const
  MPG123_ACCURATE      = 1; (**< Query if positons are currently accurate (integer value, 0 if false, 1 if true). *)
  MPG123_BUFFERFILL    = 2; (**< Get fill of internal (feed) input buffer as integer byte count returned as long and as double. An error is returned on integer overflow while converting to (signed) long, but the returned floating point value shold still be fine. *)
  MPG123_FRANKENSTEIN  = 3; (**< Stream consists of carelessly stitched together files. Seeking may yield unexpected results (also with MPG123_ACCURATE, it may be confused). *)
  MPG123_FRESH_DECODER = 4; (**< Decoder structure has been updated, possibly indicating changed stream (integer value, 0 if false, 1 if true). Flag is cleared after retrieval. *)


(** Get various current decoder/stream state information.
 *  \param mh handle
 *  \param key the key to identify the information to give.
 *  \param val the address to return (long) integer values to
 *  \param fval the address to return floating point values to
 *  \return MPG123_OK on success
 *)
var
  mpg123_getstate: Function(mh: mpg123_handle_p; key: mpg123_state_t; val: plong; fval: PDouble): int; cdecl;

(*@}*)


(** \defgroup mpg123_metadata mpg123 metadata handling
 *
 * Functions to retrieve the metadata from MPEG Audio files and streams.
 * Also includes string handling functions.
 *
 * @{
 *)

(** Data structure for storing strings in a safer way than a standard C-String.
 *  Can also hold a number of null-terminated strings. *)
type
  mpg123_string_t = record
    p:    PAnsiChar;  (**< pointer to the string data *)
    size: size_t;     (**< raw number of bytes allocated *)
    fill: size_t;     (**< number of used bytes (including closing zero byte) *)
  end;
  mpg123_string_p = ^mpg123_string_t;

(** Create and allocate memory for a new mpg123_string
 *  \param sb string handle (address of existing structure on your side)
 *)
var
  mpg123_init_string: procedure(sb: mpg123_string_p); cdecl;

(** Free-up mempory for an existing mpg123_string
 *  \param sb string handle
 *)
  mpg123_free_string: procedure(sb: mpg123_string_p); cdecl;

(** Change the size of a mpg123_string
 *  \param sb string handle
 *  \param news new size in bytes
 *  \return 0 on error, 1 on success
 *)
  mpg123_resize_string: Function(sb: mpg123_string_p; news: size_t): int; cdecl;

(** Increase size of a mpg123_string if necessary (it may stay larger).
 *  Note that the functions for adding and setting in current libmpg123
 *  use this instead of mpg123_resize_string().
 *  That way, you can preallocate memory and safely work afterwards with
 *  pieces.
 *  \param sb string handle
 *  \param news new minimum size
 *  \return 0 on error, 1 on success
 *)
  mpg123_grow_string: Function(sb: mpg123_string_p; news: size_t): int; cdecl;

(** Copy the contents of one mpg123_string string to another.
 *  Yes the order of arguments is reversed compated to memcpy().
 *  \param from string handle
 *  \param to string handle
 *  \return 0 on error, 1 on success
 *)
  mpg123_copy_string: Function(from, _to: mpg123_string_p): int; cdecl;

(** Append a C-String to an mpg123_string
 *  \param sb string handle
 *  \param stuff to append
 *  \return 0 on error, 1 on success
 *)
  mpg123_add_string: Function(sb: mpg123_string_p; stuff: PAnsiChar): int; cdecl;

(** Append a C-substring to an mpg123 string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
  mpg123_add_substring: Function(sb: mpg123_string_p; stuff: PAnsiChar; from, count: size_t): int; cdecl;

(** Set the content of a mpg123_string to a C-string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \return 0 on error, 1 on success
 *)
  mpg123_set_string: Function(sb: mpg123_string_p; stuff: PAnsiChar): int; cdecl;

(** Set the content of a mpg123_string to a C-substring
 *  \param sb string handle
 *  \param stuff the future content
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
  mpg123_set_substring: Function(sb: mpg123_string_p; stuff: PAnsiChar; from, count: size_t): int; cdecl;

(** Count characters in a mpg123 string (non-null bytes or UTF-8 characters).
 *  Even with the fill property, the character count is not obvious as there could be multiple trailing null bytes.
 *  \param sb string handle
 *  \param utf8 a flag to tell if the string is in utf8 encoding
 *  \return character count
*)
  mpg123_strlen: Function(sb: mpg123_string_p; utf8: int): size_t; cdecl;

(** Remove trailing \\r and \\n, if present.
 *  \param sb string handle
 *  \return 0 on error, 1 on success
 *)
  mpg123_chomp_string: Function(sb: mpg123_string_p): int; cdecl;

(** The mpg123 text encodings. This contains encodings we encounter in ID3 tags or ICY meta info. *)
type
  mpg123_text_encoding_t = int;

const
  mpg123_text_unknown  = 0; (**< Unkown encoding... mpg123_id3_encoding can return that on invalid codes. *)
  mpg123_text_utf8     = 1; (**< UTF-8 *)
  mpg123_text_latin1   = 2; (**< ISO-8859-1. Note that sometimes latin1 in ID3 is abused for totally different encodings. *)
  mpg123_text_icy      = 3; (**< ICY metadata encoding, usually CP-1252 but we take it as UTF-8 if it qualifies as such. *)
  mpg123_text_cp1252   = 4; (**< Really CP-1252 without any guessing. *)
  mpg123_text_utf16    = 5; (**< Some UTF-16 encoding. The last of a set of leading BOMs (byte order mark) rules.
                             *   When there is no BOM, big endian ordering is used. Note that UCS-2 qualifies as UTF-8 when
                             *   you don't mess with the reserved code points. If you want to decode little endian data
                             *   without BOM you need to prepend 0xff 0xfe yourself. *)
  mpg123_text_utf16bom = 6; (**< Just an alias for UTF-16, ID3v2 has this as distinct code. *)
  mpg123_text_utf16be  = 7; (**< Another alias for UTF16 from ID3v2. Note, that, because of the mess that is reality,
                             *   BOMs are used if encountered. There really is not much distinction between the UTF16 types for mpg123
                             *   One exception: Since this is seen in ID3v2 tags, leading null bytes are skipped for all other UTF16
                             *   types (we expect a BOM before real data there), not so for utf16be!*)
  mpg123_text_max      = 7; (**< Placeholder for the maximum encoding value. *)

(** The encoding byte values from ID3v2. *)
type
  mpg123_id3_enc_t = int;

const
  mpg123_id3_latin1   = 0;  (**< Note: This sometimes can mean anything in practice... *)
  mpg123_id3_utf16bom = 1;  (**< UTF16, UCS-2 ... it's all the same for practical purposes. *)
  mpg123_id3_utf16be  = 2;  (**< Big-endian UTF-16, BOM see note for mpg123_text_utf16be. *)
  mpg123_id3_utf8     = 3;  (**< Our lovely overly ASCII-compatible 8 byte encoding for the world. *)
  mpg123_id3_enc_max  = 3;  (**< Placeholder to check valid range of encoding byte. *)

(** Convert ID3 encoding byte to mpg123 encoding index.
 *  \param id3_enc_byte the ID3 encoding code
 *  \return the mpg123 encoding index
 *)
var
  mpg123_enc_from_id3: Function(id3_enc_byte: Byte): mpg123_text_encoding_t; cdecl;

(** Store text data in string, after converting to UTF-8 from indicated encoding
 *  A prominent error can be that you provided an unknown encoding value, or this build of libmpg123 lacks support for certain encodings (ID3 or ICY stuff missing).
 *  Also, you might want to take a bit of care with preparing the data; for example, strip leading zeroes (I have seen that).
 *  \param sb  target string
 *  \param enc mpg123 text encoding value
 *  \param source source buffer with plain unsigned bytes (you might need to cast from signed char)
 *  \param source_size number of bytes in the source buffer
 *  \return 0 on error, 1 on success (on error, mpg123_free_string is called on sb)
 *)
  mpg123_store_utf8: Function(sb: mpg123_string_p; enc: mpg123_text_encoding_t; source: PByte; source_size: size_t): int; cdecl;

(** Sub data structure for ID3v2, for storing various text fields (including comments).
 *  This is for ID3v2 COMM, TXXX and all the other text fields.
 *  Only COMM and TXXX have a description, only COMM and USLT have a language.
 *  You should consult the ID3v2 specification for the use of the various text fields ("frames" in ID3v2 documentation, I use "fields" here to separate from MPEG frames). *)
type
  mpg123_text_t = record
    lang:         array[0..2] of AnsiChar;  (**< Three-letter language code (not terminated). *)
    id:           array[0..3] of AnsiChar;  (**< The ID3v2 text field id, like TALB, TPE2, ... (4 characters, no string termination). *)
    description:  mpg123_string_t;          (**< Empty for the generic comment... *)
    text:         mpg123_string_t;          (**< ... *)
  end;
  mpg123_text_p = ^mpg123_text_t;

(** The picture type values from ID3v2. *)
  mpg123_id3_pic_type_t = int;

const
  mpg123_id3_pic_other          =  0; (**< see ID3v2 docs *)
  mpg123_id3_pic_icon           =  1; (**< see ID3v2 docs *)
  mpg123_id3_pic_other_icon     =  2; (**< see ID3v2 docs *)
  mpg123_id3_pic_front_cover    =  3; (**< see ID3v2 docs *)
  mpg123_id3_pic_back_cover     =  4; (**< see ID3v2 docs *)
  mpg123_id3_pic_leaflet        =  5; (**< see ID3v2 docs *)
  mpg123_id3_pic_media          =  6; (**< see ID3v2 docs *)
  mpg123_id3_pic_lead           =  7; (**< see ID3v2 docs *)
  mpg123_id3_pic_artist         =  8; (**< see ID3v2 docs *)
  mpg123_id3_pic_conductor      =  9; (**< see ID3v2 docs *)
  mpg123_id3_pic_orchestra      = 10; (**< see ID3v2 docs *)
  mpg123_id3_pic_composer       = 11; (**< see ID3v2 docs *)
  mpg123_id3_pic_lyricist       = 12; (**< see ID3v2 docs *)
  mpg123_id3_pic_location       = 13; (**< see ID3v2 docs *)
  mpg123_id3_pic_recording      = 14; (**< see ID3v2 docs *)
  mpg123_id3_pic_performance    = 15; (**< see ID3v2 docs *)
  mpg123_id3_pic_video          = 16; (**< see ID3v2 docs *)
  mpg123_id3_pic_fish           = 17; (**< see ID3v2 docs *)
  mpg123_id3_pic_illustration   = 18; (**< see ID3v2 docs *)
  mpg123_id3_pic_artist_logo    = 19; (**< see ID3v2 docs *)
  mpg123_id3_pic_publisher_logo = 20; (**< see ID3v2 docs *)

(** Sub data structure for ID3v2, for storing picture data including comment.
 *  This is for the ID3v2 APIC field. You should consult the ID3v2 specification
 *  for the use of the APIC field ("frames" in ID3v2 documentation, I use "fields"
 *  here to separate from MPEG frames). *)
type
  mpg123_picture_t = record
    _type:        Byte;             (**< mpg123_id3_pic_type value *)
    description:  mpg123_string_t;  (**< description string *)
    mime_type:    mpg123_string_t;  (**< MIME type *)
    size:         size_t;           (**< size in bytes *)
    data:         PByte;            (**< pointer to the image data *)
  end;
  mpg123_picture_p = ^mpg123_picture_t;

(** Data structure for storing IDV3v2 tags.
 *  This structure is not a direct binary mapping with the file contents.
 *  The ID3v2 text frames are allowed to contain multiple strings.
 *  So check for null bytes until you reach the mpg123_string fill.
 *  All text is encoded in UTF-8. *)
  mpg123_id3v2_t = record
    version:      Byte;             (**< 3 or 4 for ID3v2.3 or ID3v2.4. *)
    title:        mpg123_string_p;  (**< Title string (pointer into text_list). *)
    artist:       mpg123_string_p;  (**< Artist string (pointer into text_list). *)
    album:        mpg123_string_p;  (**< Album string (pointer into text_list). *)
    year:         mpg123_string_p;  (**< The year as a string (pointer into text_list). *)
    genre:        mpg123_string_p;  (**< Genre String (pointer into text_list). The genre string(s) may very well need postprocessing, esp. for ID3v2.3. *)
    comment:      mpg123_string_p;  (**< Pointer to last encountered comment text with empty description. *)
    (* Encountered ID3v2 fields are appended to these lists.
       There can be multiple occurences, the pointers above always point to the last encountered data. *)
    comment_list: mpg123_text_p;    (**< Array of comments. *)
    comments:     size_t;           (**< Number of comments. *)
    text:         mpg123_text_p;    (**< Array of ID3v2 text fields (including USLT) *)
    texts:        size_t;           (**< Numer of text fields. *)
    extra:        mpg123_text_p;    (**< The array of extra (TXXX) fields. *)
    extras:       size_t;           (**< Number of extra text (TXXX) fields. *)
    picture:      mpg123_picture_p; (**< Array of ID3v2 pictures fields (APIC). *)
    pictures:     size_t;           (**< Number of picture (APIC) fields. *)
  end;
  mpg123_id3v2_p  = ^mpg123_id3v2_t;
  mpg123_id3v2_pp = ^mpg123_id3v2_p;

(** Data structure for ID3v1 tags (the last 128 bytes of a file).
 *  Don't take anything for granted (like string termination)!
 *  Also note the change ID3v1.1 did: comment[28] = 0; comment[29] = track_number
 *  It is your task to support ID3v1 only or ID3v1.1 ...*)
  mpg123_id3v1_t = record
    tag:      array[0..2] of AnsiChar;    (**< Always the string "TAG", the classic intro. *)
    title:    array[0..29] of AnsiChar;   (**< Title string.  *)
    artist:   array[0..29] of AnsiChar;   (**< Artist string. *)
    album:    array[0..29] of AnsiChar;   (**< Album string. *)
    year:     array[0..3] of AnsiChar;    (**< Year string. *)
    comment:  array[0..29] of AnsiChar;   (**< Comment string. *)
    genre:    Byte;                       (**< Genre index. *)
  end;
  mpg123_id3v1_p  = ^mpg123_id3v1_t;
  mpg123_id3v1_pp = ^mpg123_id3v1_p;

const
  // added underscores to prevent name collisions
  _MPG123_ID3     = $3;  (**< 0011 There is some ID3 info. Also matches 0010 or NEW_ID3. *)
  _MPG123_NEW_ID3 = $1;  (**< 0001 There is ID3 info that changed since last call to mpg123_id3. *)
  _MPG123_ICY     = $c;  (**< 1100 There is some ICY info. Also matches 0100 or NEW_ICY.*)
  _MPG123_NEW_ICY = $4;  (**< 0100 There is ICY info that changed since last call to mpg123_icy. *)

(** Query if there is (new) meta info, be it ID3 or ICY (or something new in future).
 *  \param mh handle
 *  \return combination of flags, 0 on error (same as "nothing new")
 *)
var
  mpg123_meta_check: Function(mh: mpg123_handle_p): int; cdecl;

(** Clean up meta data storage (ID3v2 and ICY), freeing memory.
 *  \param mh handle
 *)
  mpg123_meta_free: procedure(mh: mpg123_handle_p); cdecl;

(** Point v1 and v2 to existing data structures wich may change on any next read/decode function call.
 *  v1 and/or v2 can be set to NULL when there is no corresponding data.
 *  \return MPG123_OK on success
 *)
  mpg123_id3: Function(mh: mpg123_handle_p; v1: mpg123_id3v1_pp; v2: mpg123_id3v2_pp): int; cdecl;

(** Point icy_meta to existing data structure wich may change on any next read/decode function call.
 *  \param mh handle
 *  \param icy_meta return address for ICY meta string (set to NULL if nothing there)
 *  \return MPG123_OK on success
 *)
  mpg123_icy: Function(mh: mpg123_handle_p; icy_meta: PPAnsiChar): int; cdecl;

(** Decode from windows-1252 (the encoding ICY metainfo used) to UTF-8.
 *  Note that this is very similar to mpg123_store_utf8(&sb, mpg123_text_icy, icy_text, strlen(icy_text+1)) .
 *  \param icy_text The input data in ICY encoding
 *  \return pointer to newly allocated buffer with UTF-8 data (You free() it!) *)
  mpg123_icy2utf8: Function(icy_text: PAnsiChar): PAnsiChar; cdecl;
{$IFDEF LEAK_WARNINGS}
  {$MESSAGE WARN 'Function mpg123_icy2utf8 is allocating buffer that cannot be freed. Use it with caution!'}
{$ENDIF}

(* @} *)


(** \defgroup mpg123_advpar mpg123 advanced parameter API
 *
 *  Direct access to a parameter set without full handle around it.
 *  Possible uses:
 *    - Influence behaviour of library _during_ initialization of handle (MPG123_VERBOSE).
 *    - Use one set of parameters for multiple handles.
 *
 *  The functions for handling mpg123_pars (mpg123_par() and mpg123_fmt() 
 *  family) directly return a fully qualified mpg123 error code, the ones 
 *  operating on full handles normally MPG123_OK or MPG123_ERR, storing the 
 *  specific error code itseld inside the handle. 
 *
 * @{
 *)

(** Opaque structure for the libmpg123 decoder parameters. *)
type
  mpg123_pars_struct_t = record end;
  mpg123_pars_struct_p = ^mpg123_pars_struct_t;

(** Opaque structure for the libmpg123 decoder parameters. *)
  mpg123_pars_t = type mpg123_pars_struct_t;
  mpg123_pars_p = ^mpg123_pars_t;

(** Create a handle with preset parameters.
 *  \param mp parameter handle
 *  \param decoder decoder choice
 *  \param error error code return address
 *  \return mpg123 handle
 *)
var
  mpg123_parnew: Function(mp: mpg123_pars_p; decoder: PAnsiChar; error: pint): mpg123_handle_p; cdecl;

(** Allocate memory for and return a pointer to a new mpg123_pars
 *  \param error error code return address
 *  \return new parameter handle
 *)
  mpg123_new_pars: Function(error: pint): mpg123_pars_p; cdecl;

(** Delete and free up memory used by a mpg123_pars data structure
 *  \param mp parameter handle
 *)
  mpg123_delete_pars: procedure(mp: mpg123_pars_p); cdecl;

(** Configure mpg123 parameters to accept no output format at all, 
 *  use before specifying supported formats with mpg123_format
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
  mpg123_fmt_none: Function(mp: mpg123_pars_p): int; cdecl;

(** Configure mpg123 parameters to accept all formats 
 *  (also any custom rate you may set) -- this is default. 
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
  mpg123_fmt_all: Function(mp: mpg123_pars_p): int; cdecl;

(** Set the audio format support of a mpg123_pars in detail:
 * \param mp parameter handle
 * \param rate The sample rate value (in Hertz).
 * \param channels A combination of MPG123_STEREO and MPG123_MONO.
 * \param encodings A combination of accepted encodings for rate and channels,
 *                  p.ex MPG123_ENC_SIGNED16|MPG123_ENC_ULAW_8 (or 0 for no
 *                  support).
 * \return MPG123_OK on success
*)
  mpg123_fmt: Function(mp: mpg123_pars_p; rate: long; channels, encodings: int): int; cdecl;

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_pars.
 *  \param mp parameter handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO, 
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
  mpg123_fmt_support: Function(mp: mpg123_pars_p; rate: long; encoding: int): int; cdecl;

(** Set a specific parameter, for a specific mpg123_pars, using a parameter 
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 *  \return MPG123_OK on success
 *)
  mpg123_par: Function(mp: mpg123_pars_p; _type: mpg123_parms_t; value: long; fvalue: Double): int; cdecl;

(** Get a specific parameter, for a specific mpg123_pars. 
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
  mpg123_getpar: Function(mp: mpg123_pars_p; _type: mpg123_parms_t; value: plong; fvalue: PDouble): int; cdecl;

(* @} *)


(** \defgroup mpg123_lowio mpg123 low level I/O
  * You may want to do tricky stuff with I/O that does not work with mpg123's default file access or you want to make it decode into your own pocket...
  *
  * @{ *)

// callbacks procedural types
type
  TIntRead   = Function(DataSource: int; Buffer: Pointer; Size: size_t): ssize_t; cdecl;
  TIntSeek   = Function(DataSource: int; Offset: off_t; Whence: int): off_t; cdecl;
  TIntSeek32 = Function(DataSource: int; Offset: off32_t; Whence: int): off32_t; cdecl;
  TIntSeek64 = Function(DataSource: int; Offset: off64_t; Whence: int): off64_t; cdecl;

  TPtrRead   = Function(DataSource: Pointer; Buffer: Pointer; Size: size_t): ssize_t; cdecl;
  TPtrSeek   = Function(DataSource: Pointer; Offset: off_t; Whence: int): off_t; cdecl;
  TPtrSeek32 = Function(DataSource: Pointer; Offset: off32_t; Whence: int): off32_t; cdecl;
  TPtrSeek64 = Function(DataSource: Pointer; Offset: off64_t; Whence: int): off64_t; cdecl;
  TPtrClean  = procedure(DataSource: Pointer); cdecl;

(** Replace default internal buffer with user-supplied buffer.
  * Instead of working on it's own private buffer, mpg123 will directly use the one you provide for storing decoded audio.
  * Note that the required buffer size could be bigger than expected from output
  * encoding if libmpg123 has to convert from primary decoder output (p.ex. 32 bit
  * storage for 24 bit output).
  * \param mh handle
  * \param data pointer to user buffer
  * \param size of buffer in bytes
  * \return MPG123_OK on success
  *)
var
  mpg123_replace_buffer: Function(mh: mpg123_handle_p; data: PByte; size: size_t): int; cdecl;

(** The max size of one frame's decoded output with current settings.
 *  Use that to determine an appropriate minimum buffer size for decoding one frame.
 *  \param mh handle
 *  \return maximum decoded data size in bytes
 *)
  mpg123_outblock: Function(mh: mpg123_handle_p): size_t; cdecl;

(** Replace low-level stream access functions; read and lseek as known in POSIX.
 *  You can use this to make any fancy file opening/closing yourself, 
 *  using mpg123_open_fd() to set the file descriptor for your read/lseek
 *  (doesn't need to be a "real" file descriptor...).
 *  Setting a function to NULL means that the default internal read is 
 *  used (active from next mpg123_open call on).
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this implies mpg123_close().
 * \param mh handle
 * \param r_read callback for reading (behaviour like POSIX read)
 * \param r_lseek callback for seeking (like POSIX lseek)
 * \return MPG123_OK on success
 *)
  mpg123_replace_reader: Function(mh: mpg123_handle_p; r_read: TIntRead; r_lseek: TIntSeek): int; cdecl;
  mpg123_replace_reader_32: Function(mh: mpg123_handle_p; r_read: TIntRead; r_lseek: TIntSeek32): int; cdecl;
  mpg123_replace_reader_64: Function(mh: mpg123_handle_p; r_read: TIntRead; r_lseek: TIntSeek64): int; cdecl;

(** Replace I/O functions with your own ones operating on some kind of
 *  handle instead of integer descriptors.
 *  The handle is a void pointer, so you can pass any data you want...
 *  mpg123_open_handle() is the call you make to use the I/O defined here.
 *  There is no fallback to internal read/seek here.
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this mpg123_close() is implied here.
 *  \param mh handle
 *  \param r_read callback for reading (behaviour like POSIX read)
 *  \param r_lseek callback for seeking (like POSIX lseek)
 *  \param cleanup A callback to clean up an I/O handle on mpg123_close,
 *         can be NULL for none (you take care of cleaning your handles).
 * \return MPG123_OK on success
 *)
  mpg123_replace_reader_handle: Function(mh: mpg123_handle_p; r_read: TPtrRead; r_lseek: TPtrSeek; cleanup: TPtrClean): int; cdecl;
  mpg123_replace_reader_handle_32: Function(mh: mpg123_handle_p; r_read: TPtrRead; r_lseek: TPtrSeek32; cleanup: TPtrClean): int; cdecl;
  mpg123_replace_reader_handle_64: Function(mh: mpg123_handle_p; r_read: TPtrRead; r_lseek: TPtrSeek64; cleanup: TPtrClean): int; cdecl;

(* @} *)

//==============================================================================

const
  mpg123_LibFileName = 'mpg123.dll';

Function mpg123_Initialize(const LibPath: String = mpg123_LibFileName; InitLib: Boolean = True): Boolean;
procedure mpg123_Finalize(FinalLib: Boolean = True);

implementation

uses
  DynLibUtils;

var
  Libcontext: TDLULibraryContext;

//------------------------------------------------------------------------------

Function mpg123_Initialize(const LibPath: String = mpg123_LibFileName; InitLib: Boolean = True): Boolean;
const
  // function name suffix for large files support  
  LFS_DEF_SUFFIX = {$IFDEF LARGE_FILES_SUPPORT}'_64'{$ELSE}''{$ENDIF};  
begin
Result := OpenLibraryAndResolveSymbols(LibPath,LibContext,[
  // mpg123 library and handle setup - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_init                    ,'mpg123_init'),
  Symbol(@@mpg123_exit                    ,'mpg123_exit'),
  Symbol(@@mpg123_new                     ,'mpg123_new'),
  Symbol(@@mpg123_delete                  ,'mpg123_delete'),
  // mpg123 params and features  - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_param                   ,'mpg123_param'),
  Symbol(@@mpg123_getparam                ,'mpg123_getparam'),
  Symbol(@@mpg123_feature                 ,'mpg123_feature'),
  // mpg123 error handling - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_plain_strerror          ,'mpg123_plain_strerror'),
  Symbol(@@mpg123_strerror                ,'mpg123_strerror'),
  Symbol(@@mpg123_errcode                 ,'mpg123_errcode'),
  // mpg123 decoder selection  - - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_decoders                ,'mpg123_decoders'),
  Symbol(@@mpg123_supported_decoders      ,'mpg123_supported_decoders'),
  Symbol(@@mpg123_decoder                 ,'mpg123_decoder'),
  Symbol(@@mpg123_current_decoder         ,'mpg123_current_decoder'),
  // mpg123 output audio format  - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_rates                   ,'mpg123_rates'),
  Symbol(@@mpg123_encodings               ,'mpg123_encodings'),
  Symbol(@@mpg123_encsize                 ,'mpg123_encsize'),
  Symbol(@@mpg123_format_none             ,'mpg123_format_none'),
  Symbol(@@mpg123_format_all              ,'mpg123_format_all'),
  Symbol(@@mpg123_format                  ,'mpg123_format'),
  Symbol(@@mpg123_format_support          ,'mpg123_format_support'),
  Symbol(@@mpg123_getformat               ,'mpg123_getformat'),
  Symbol(@@mpg123_getformat2              ,'mpg123_getformat2'),
  // mpg123 file input and decoding  - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_open                    ,'mpg123_open' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_open_32                 ,'mpg123_open_32'),
  Symbol(@@mpg123_open_64                 ,'mpg123_open_64'),
  Symbol(@@mpg123_open_fd                 ,'mpg123_open_fd' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_open_fd_32              ,'mpg123_open_fd_32'),
  Symbol(@@mpg123_open_fd_64              ,'mpg123_open_fd_64'),
  Symbol(@@mpg123_open_handle             ,'mpg123_open_handle' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_open_handle_32          ,'mpg123_open_handle_32'),
  Symbol(@@mpg123_open_handle_64          ,'mpg123_open_handle_64'),
  Symbol(@@mpg123_open_feed               ,'mpg123_open_feed'),
  Symbol(@@mpg123_close                   ,'mpg123_close'),
  Symbol(@@mpg123_read                    ,'mpg123_read'),
  Symbol(@@mpg123_feed                    ,'mpg123_feed'),
  Symbol(@@mpg123_decode                  ,'mpg123_decode'),
  Symbol(@@mpg123_decode_frame            ,'mpg123_decode_frame' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_decode_frame_32         ,'mpg123_decode_frame_32'),
  Symbol(@@mpg123_decode_frame_64         ,'mpg123_decode_frame_64'),
  Symbol(@@mpg123_framebyframe_decode     ,'mpg123_framebyframe_decode' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_framebyframe_decode_32  ,'mpg123_framebyframe_decode_32'),
  Symbol(@@mpg123_framebyframe_decode_64  ,'mpg123_framebyframe_decode_64'),
  Symbol(@@mpg123_framebyframe_next       ,'mpg123_framebyframe_next'),
  Symbol(@@mpg123_framedata               ,'mpg123_framedata'),
  Symbol(@@mpg123_framepos                ,'mpg123_framepos' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_framepos_32             ,'mpg123_framepos_32'),
  Symbol(@@mpg123_framepos_64             ,'mpg123_framepos_64'),
  // mpg123 position and seeking - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_tell                    ,'mpg123_tell' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_tell_32                 ,'mpg123_tell_32'),
  Symbol(@@mpg123_tell_64                 ,'mpg123_tell_64'),
  Symbol(@@mpg123_tellframe               ,'mpg123_tellframe' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_tellframe_32            ,'mpg123_tellframe_32'),
  Symbol(@@mpg123_tellframe_64            ,'mpg123_tellframe_64'),
  Symbol(@@mpg123_tell_stream             ,'mpg123_tell_stream' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_tell_stream_32          ,'mpg123_tell_stream_32'),
  Symbol(@@mpg123_tell_stream_64          ,'mpg123_tell_stream_64'),
  Symbol(@@mpg123_seek                    ,'mpg123_seek' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_seek_32                 ,'mpg123_seek_32'),
  Symbol(@@mpg123_seek_64                 ,'mpg123_seek_64'),
  Symbol(@@mpg123_feedseek                ,'mpg123_feedseek' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_feedseek_32             ,'mpg123_feedseek_32'),
  Symbol(@@mpg123_feedseek_64             ,'mpg123_feedseek_64'),
  Symbol(@@mpg123_seek_frame              ,'mpg123_seek_frame' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_seek_frame_32           ,'mpg123_seek_frame_32'),
  Symbol(@@mpg123_seek_frame_64           ,'mpg123_seek_frame_64'),
  Symbol(@@mpg123_timeframe               ,'mpg123_timeframe' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_timeframe_32            ,'mpg123_timeframe_32'),
  Symbol(@@mpg123_timeframe_64            ,'mpg123_timeframe_64'),
  Symbol(@@mpg123_index                   ,'mpg123_index' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_index_32                ,'mpg123_index_32'),
  Symbol(@@mpg123_index_64                ,'mpg123_index_64'),
  Symbol(@@mpg123_set_index               ,'mpg123_set_index' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_set_index_32            ,'mpg123_set_index_32'),
  Symbol(@@mpg123_set_index_64            ,'mpg123_set_index_64'),
  Symbol(@@mpg123_position                ,'mpg123_position' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_position_32             ,'mpg123_position_32'),
  Symbol(@@mpg123_position_64             ,'mpg123_position_64'),
  // mpg123 volume and equalizer - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_eq                      ,'mpg123_eq'),
  Symbol(@@mpg123_geteq                   ,'mpg123_geteq'),
  Symbol(@@mpg123_reset_eq                ,'mpg123_reset_eq'),
  Symbol(@@mpg123_volume                  ,'mpg123_volume'),
  Symbol(@@mpg123_volume_change           ,'mpg123_volume_change'),
  Symbol(@@mpg123_getvolume               ,'mpg123_getvolume'),
  // mpg123 status and information - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_info                    ,'mpg123_info'),
  Symbol(@@mpg123_safe_buffer             ,'mpg123_safe_buffer'),
  Symbol(@@mpg123_scan                    ,'mpg123_scan'),
  Symbol(@@mpg123_framelength             ,'mpg123_framelength' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_framelength_32          ,'mpg123_framelength_32'),
  Symbol(@@mpg123_framelength_64          ,'mpg123_framelength_64'),
  Symbol(@@mpg123_length                  ,'mpg123_length' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_length_32               ,'mpg123_length_32'),
  Symbol(@@mpg123_length_64               ,'mpg123_length_64'),
  Symbol(@@mpg123_set_filesize            ,'mpg123_set_filesize' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_set_filesize_32         ,'mpg123_set_filesize_32'),
  Symbol(@@mpg123_set_filesize_64         ,'mpg123_set_filesize_64'),
  Symbol(@@mpg123_tpf                     ,'mpg123_tpf'),
  Symbol(@@mpg123_spf                     ,'mpg123_spf'),
  Symbol(@@mpg123_clip                    ,'mpg123_clip'),
  // mpg123 decoder/stream state information - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_getstate                ,'mpg123_getstate'),
  // mpg123 string handling functions  - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_init_string             ,'mpg123_init_string'),
  Symbol(@@mpg123_free_string             ,'mpg123_free_string'),
  Symbol(@@mpg123_resize_string           ,'mpg123_resize_string'),
  Symbol(@@mpg123_grow_string             ,'mpg123_grow_string'),
  Symbol(@@mpg123_copy_string             ,'mpg123_copy_string'),
  Symbol(@@mpg123_add_string              ,'mpg123_add_string'),
  Symbol(@@mpg123_add_substring           ,'mpg123_add_substring'),
  Symbol(@@mpg123_set_string              ,'mpg123_set_string'),
  Symbol(@@mpg123_set_substring           ,'mpg123_set_substring'),
  Symbol(@@mpg123_strlen                  ,'mpg123_strlen'),
  Symbol(@@mpg123_chomp_string            ,'mpg123_chomp_string'),
  // mpg123 text encodings - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_enc_from_id3            ,'mpg123_enc_from_id3'),
  Symbol(@@mpg123_store_utf8              ,'mpg123_store_utf8'),
  // mpg123 metadata handling  - - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_meta_check              ,'mpg123_meta_check'),
  Symbol(@@mpg123_meta_free               ,'mpg123_meta_free'),
  Symbol(@@mpg123_id3                     ,'mpg123_id3'),
  Symbol(@@mpg123_icy                     ,'mpg123_icy'),
  Symbol(@@mpg123_icy2utf8                ,'mpg123_icy2utf8'),
  // mpg123 advanced parameter API - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_parnew                  ,'mpg123_parnew'),
  Symbol(@@mpg123_new_pars                ,'mpg123_new_pars'),
  Symbol(@@mpg123_delete_pars             ,'mpg123_delete_pars'),
  Symbol(@@mpg123_fmt_none                ,'mpg123_fmt_none'),
  Symbol(@@mpg123_fmt_all                 ,'mpg123_fmt_all'),
  Symbol(@@mpg123_fmt                     ,'mpg123_fmt'),
  Symbol(@@mpg123_fmt_support             ,'mpg123_fmt_support'),
  Symbol(@@mpg123_par                     ,'mpg123_par'),
  Symbol(@@mpg123_getpar                  ,'mpg123_getpar'),
  // mpg123 low level I/O  - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Symbol(@@mpg123_replace_buffer          ,'mpg123_replace_buffer'),
  Symbol(@@mpg123_outblock                ,'mpg123_outblock'),
  Symbol(@@mpg123_replace_reader          ,'mpg123_replace_reader' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_replace_reader_32       ,'mpg123_replace_reader_32'),
  Symbol(@@mpg123_replace_reader_64       ,'mpg123_replace_reader_64'),
  Symbol(@@mpg123_replace_reader_handle   ,'mpg123_replace_reader_handle' + LFS_DEF_SUFFIX),
  Symbol(@@mpg123_replace_reader_handle_32,'mpg123_replace_reader_handle_32'),
  Symbol(@@mpg123_replace_reader_handle_64,'mpg123_replace_reader_handle_64')],True) = 135;
// init library
If Result and InitLib then
  Result := mpg123_init() = MPG123_OK;
end;

//------------------------------------------------------------------------------

procedure mpg123_Finalize(FinalLib: Boolean = True);
begin   
If FinalLib then
  mpg123_exit;
CloseLibrary(LibContext);
end;

//==============================================================================

initialization
  LibContext := DefaultLibraryContext;

end.
