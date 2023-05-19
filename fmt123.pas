{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  mpg123 library bindings

    This unit is a direct translation of C header file fmt123.h, which is part
    of bindings for libmpg123 and libout123 libraries, into pascal.

    More info about the mpg123 library can be found at: https://www.mpg123.de

  Version 1.0.5 (2023-05-16)

  Build against library version 1.25.13

  Last change 2023-05-16

  ©2018-2023 František Milt

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
unit fmt123;

{$INCLUDE '.\mpg123_defs.inc'}

interface

uses
  SysUtils,
  AuxTypes;

(* some new types and constants for easier translation from C header *)
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

type
  PPByte      = ^PByte;
  PPPAnsiChar = ^PPAnsiChar;

  int     = Int32;      pint     = ^int;        ppint     = ^pint;
  long    = Int32;      plong    = ^long;       pplong    = ^plong;
  ulong   = UInt32;     pulong   = ^ulong;
  size_t  = PtrUInt;    psize_t  = ^size_t;
  ssize_t = PtrInt;
  off32_t = Int32;      poff32_t = ^off32_t;    ppoff32_t = ^poff32_t;
  off64_t = Int64;      poff64_t = ^off64_t;    ppoff64_t = ^poff64_t;
{$IFDEF LARGE_FILES_SUPPORT}
  off_t   = off64_t;
{$ELSE}
  off_t   = long;
{$ENDIF}                poff_t   = ^off_t;      ppoff_t   = ^poff_t;

type
  // binding-specific exception
  EMPG123Exception = class(Exception);

//==============================================================================

(*
  libmpg123: MPEG Audio Decoder library

  separate header just for audio format definitions not tied to
  library code

  copyright 1995-2015 by the mpg123 project
  free software under the terms of the LGPL 2.1
  see COPYING and AUTHORS files in distribution or http://mpg123.org
*)

(** \file fmt123.h Audio format definitions. *)

(** \defgroup mpg123_enc mpg123 PCM sample encodings
 *  These are definitions for audio formats used by libmpg123 and
 *  libout123.
 *
 * @{
 *)

(** An enum over all sample types possibly known to mpg123.
 *  The values are designed as bit flags to allow bitmasking for encoding
 *  families.
 *  This is also why the enum is not used as type for actual encoding variables,
 *  plain integers (at least 16 bit, 15 bit being used) cover the possible
 *  combinations of these flags.
 *
 *  Note that (your build of) libmpg123 does not necessarily support all these.
 *  Usually, you can expect the 8bit encodings and signed 16 bit.
 *  Also 32bit float will be usual beginning with mpg123-1.7.0 .
 *  What you should bear in mind is that (SSE, etc) optimized routines may be
 *  absent for some formats. We do have SSE for 16, 32 bit and float, though.
 *  24 bit integer is done via postprocessing of 32 bit output -- just cutting
 *  the last byte, no rounding, even. If you want better, do it yourself.
 *
 *  All formats are in native byte order. If you need different endinaness, you
 *  can simply postprocess the output buffers (libmpg123 wouldn't do anything
 * else). The macro MPG123_SAMPLESIZE() can be helpful there.
 *)
type
  mpg123_enc_enum_t = int;

const
  MPG123_ENC_8           = $00f;                                                (* 0000 0000 0000 1111 Some 8 bit  integer encoding. *)
  MPG123_ENC_16          = $040;                                                (* 0000 0000 0100 0000 Some 16 bit integer encoding. *)
  MPG123_ENC_24          = $4000;                                               (* 0100 0000 0000 0000 Some 24 bit integer encoding. *)
  MPG123_ENC_32          = $100;                                                (* 0000 0001 0000 0000 Some 32 bit integer encoding. *)
  MPG123_ENC_SIGNED      = $080;                                                (* 0000 0000 1000 0000 Some signed integer encoding. *)
  MPG123_ENC_FLOAT       = $e00;                                                (* 0000 1110 0000 0000 Some float encoding. *)
  MPG123_ENC_SIGNED_16   = (MPG123_ENC_16 or MPG123_ENC_SIGNED or $10);         (* 0000 0000 1101 0000 signed 16 bit *)
  MPG123_ENC_UNSIGNED_16 = (MPG123_ENC_16 or $20);                              (* 0000 0000 0110 0000 unsigned 16 bit *)
  MPG123_ENC_UNSIGNED_8  = $01;                                                 (* 0000 0000 0000 0001 unsigned 8 bit *)
  MPG123_ENC_SIGNED_8    = (MPG123_ENC_SIGNED or $02);                          (* 0000 0000 1000 0010 signed 8 bit *)
  MPG123_ENC_ULAW_8      = $04;                                                 (* 0000 0000 0000 0100 ulaw 8 bit *)
  MPG123_ENC_ALAW_8      = $08;                                                 (* 0000 0000 0000 1000 alaw 8 bit *)
  MPG123_ENC_SIGNED_32   = (MPG123_ENC_32 or MPG123_ENC_SIGNED or $1000);       (* 0001 0001 1000 0000 signed 32 bit *)
  MPG123_ENC_UNSIGNED_32 = (MPG123_ENC_32 or $2000);                            (* 0010 0001 0000 0000 unsigned 32 bit *)
  MPG123_ENC_SIGNED_24   = (MPG123_ENC_24 or MPG123_ENC_SIGNED or $1000);       (* 0101 0000 1000 0000 signed 24 bit *)
  MPG123_ENC_UNSIGNED_24 = (MPG123_ENC_24 or $2000);                            (* 0110 0000 0000 0000 unsigned 24 bit *)
  MPG123_ENC_FLOAT_32    = $200;                                                (* 0000 0010 0000 0000 32bit float *)
  MPG123_ENC_FLOAT_64    = $400;                                                (* 0000 0100 0000 0000 64bit float *)    
  MPG123_ENC_ANY         = (MPG123_ENC_SIGNED_16  or MPG123_ENC_UNSIGNED_16 or  (* Any possibly known encoding from the list above. *)
                            MPG123_ENC_UNSIGNED_8 or MPG123_ENC_SIGNED_8 or
                            MPG123_ENC_ULAW_8     or MPG123_ENC_ALAW_8 or
                            MPG123_ENC_SIGNED_32  or MPG123_ENC_UNSIGNED_32 or
                            MPG123_ENC_SIGNED_24  or MPG123_ENC_UNSIGNED_24 or
                            MPG123_ENC_FLOAT_32   or MPG123_ENC_FLOAT_64);

(** Get size of one PCM sample with given encoding.
 *  This is included both in libmpg123 and libout123. Both offer
 *  an API function to provide the macro results from library
 *  compile-time, not that of you application. This most likely
 *  does not matter as I do not expect any fresh PCM sample
 *  encoding to appear. But who knows? Perhaps the encoding type
 *  will be abused for funny things in future, not even plain PCM.
 *  And, by the way: Thomas really likes the ?: operator.
 * \param enc the encoding (mpg123_enc_enum value)
 * \return size of one sample in bytes
 *)
Function MPG123_SAMPLESIZE(enc: mpg123_enc_enum_t): Integer;

(** Structure defining an audio format.
 *  Providing the members as individual function arguments to define a certain
 *  output format is easy enough. This struct makes is more comfortable to deal
 *  with a list of formats.
 *  Negative values for the members might be used to communicate use of default
 *  values.
 *)
type
  mpg123_fmt_t = record
    rate:     long;   (**< sampling rate in Hz  *)
    channels: int;    (**< channel count *)
    encoding: int;    (** encoding code, can be single value or bitwise or of members of
                       *  mpg123_enc_enum *)
  end;
  mpg123_fmt_p  = ^mpg123_fmt_t;
  mpg123_fmt_pp = ^mpg123_fmt_p;

(* @} *)

implementation

//== Macro implementation ======================================================

Function MPG123_SAMPLESIZE(enc: mpg123_enc_enum_t): Integer;
begin
If (enc and MPG123_ENC_8) <> 0 then
  Result := 1
else If (enc and MPG123_ENC_16) <> 0 then
  Result := 2
else If (enc and MPG123_ENC_24) <> 0 then
  Result := 3
else If ((enc and MPG123_ENC_32) <> 0) or (enc = MPG123_ENC_FLOAT_32) then
  Result := 4
else If enc = MPG123_ENC_FLOAT_64 then
  Result := 8
else
  Result := 0;
end;

end.
