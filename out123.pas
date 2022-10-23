{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  mpg123 library bindings

    This unit is a direct translation of C header file out123.h into pascal,
    and is a main part of libout123 library binding.

    More info about the mpg123 library can be found at: https://www.mpg123.de

  Version 1.0.4 (2020-08-11)

  Build against library version 1.25.13 (libout123 API version 2)

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
unit out123;

{$INCLUDE '.\mpg123_defs.inc'}

interface

uses
  fmt123;

(*
  out123: audio output interface

  copyright 1995-2016 by the mpg123 project,
  free software under the terms of the LGPL 2.1

  see COPYING and AUTHORS files in distribution or http://mpg123.org
  initially written as audio.h by Michael Hipp, reworked into out123 API
  by Thomas Orgis
*)

(** A macro to check at compile time which set of API functions to expect.
 * This should be incremented at least each time a new symbol is added
 * to the header.
 *)
const
  OUT123_API_VERSION = 2;

(** \defgroup out123_api out123 library API
 *  This is out123, a library focused on continuous playback of audio streams
 *  via various platform-specific output methods. It glosses over details of
 *  the native APIs to give an interface close to simply writing data to a
 *  file. There might be the option to tune details like buffer (period) sizes
 *  and the number of them on the device side in future, but the focus of the
 *  library is to ease the use case of just getting that raw audio data out
 *  there, without interruptions.
 *
 *  The basic idea is to create a handle with out123_new() and open a certain
 *  output device (using a certain driver module, possibly build-time defaults)
 *  with out123_open(). Now, you can query the output device for supported
 *  encodings for given rate and channel count with out123_get_encodings() and
 *  decide what to use for actually starting playback with out123_start().
 *
 *  Then, you just need to provide (interleaved pcm) data for playback with
 *  out123_play(), which will block when the device's buffers are full. You get
 *  your timing from that (instead of callbacks). If your program does the
 *  production of the audio data just a little bit faster than the playback,
 *  causing out123_play() to block ever so briefly, you're fine.
 *
 *  You stop playback with out123_stop(), or just close the device and driver
 *  via out123_close(), or even just decide to drop it all and do out123_del()
 *  right away when you're done.
 *
 *  There are other functions for specific needs, but the basic idea should be
 *  covered by the above.
 @{
 *)

(** Opaque structure for the libout123 handle. *)
type
  out123_struct_t = record end;
  out123_struct_p = ^out123_struct_t;

(** Typedef shortcut as preferrend name for the handle type. *)
  out123_handle_t = type out123_struct_t;
  out123_handle_p = ^out123_handle_t;


(** Enumeration of codes for the parameters that it is possible to set/get. *)
type
  out123_parms_t = int;

const
  OUT123_FLAGS        = 1;  (**< integer, various flags, see enum out123_flags *)
  OUT123_PRELOAD      = 2;  (**< float, fraction of buffer to fill before playback *)
  OUT123_GAIN         = 3;  (**< integer, output device gain (module-specific) *)
  OUT123_VERBOSE      = 4;  (**< integer, verbosity to stderr, >= 0 *)
  OUT123_DEVICEBUFFER = 5;  (**<
   *  float, length of device buffer in seconds;
   *  This might be ignored, might have only a loose relation to actual
   *  buffer sizes and latency, depending on output driver. Try to tune
   *  this before opening a device if you want to influcence latency or reduce
   *  dropouts. Value <= 0 uses some default, usually favouring stable playback
   *  over low latency. Values above 0.5 are probably too much.
   *)
  OUT123_PROPFLAGS    = 6;  (**< integer, query driver/device property flags (r/o) *)
  OUT123_NAME         = 7;  (**< string, name of this instance (NULL restores default);
   * The value returned by out123_getparam() might be different if the audio
   * backend changed it (to be unique among clients, p.ex.).
   * TODO: The name provided here is used as prefix in diagnostic messages. *)
  OUT123_BINDIR       = 8;(**< string, path to a program binary directory to use
   * as starting point in the search for the output module directory
   * (e.g. ../lib/mpg123 or ./plugins). The environment variable MPG123_MODDIR
   * is always tried first and the in-built installation path last.
   *)

(** Flags to tune out123 behaviour *)
type
  out123_flags_t = int;

const
  OUT123_HEADPHONES       = $01; (**< output to headphones (if supported) *)
  OUT123_INTERNAL_SPEAKER = $02; (**< output to speaker (if supported) *)
  OUT123_LINE_OUT         = $04; (**< output to line out (if supported) *)
  OUT123_QUIET            = $08; (**< no printouts to standard error *)
  OUT123_KEEP_PLAYING     = $10; (**<
   *  When this is set (default), playback continues in a loop when the device
   *  does not consume all given data at once. This happens when encountering
   *  signals (like SIGSTOP, SIGCONT) that cause interruption of the underlying
   *  functions.
   *  Note that this flag is meaningless when the optional buffer is employed,
   *  There, your program will always block until the buffer completely took
   *  over the data given to it via out123_play(), unless a communication error
   *  arises.
   *)


(** Read-only output driver/device property flags (OUT123_PROPFLAGS). *)
type
  out123_propflags_t = int;

const
  OUT123_PROP_LIVE       = $01;  (**< This is a live output, meaning that
   *  special care might be needed for pauses in playback (p.ex. stream
   *  of silence instead of interruption), as opposed to files on disk.
   *)
  OUT123_PROP_PERSISTENT = $02;  (**< This (live) output does not need
   *  special care for pauses (continues with silence itself),
   *  out123_pause() does nothing to the device.
   *)

(** Create a new output handle.
 *  This only allocates and initializes memory, so the only possible
 *  error condition is running out of memory.
 * \return pointer to new handle or NULL on error
 *)
var
  out123_new: Function: out123_handle_p; cdecl;

(** Delete output handle.
 *  This implies out123_close().
 *)
  out123_del: procedure(ao: out123_handle_p); cdecl;

(** Error code enumeration
 * API calls return a useful (positve) value or zero (OUT123_OK) on simple
 * success. A negative value (-1 == OUT123_ERR) usually indicates that some
 * error occured. Which one, that can be queried using out123_errcode()
 * and friends.
 *)
type
  out123_error_t = int;

const
  OUT123_ERR             = -1;(**< generic alias for verbosity, always == -1 *)
  OUT123_OK              = 0; (**< just a name for zero, not going to change *)
  OUT123_DOOM            = 1; (**< dazzled, out of memory *)
  OUT123_BAD_DRIVER_NAME = 2; (**< bad driver name given *)
  OUT123_BAD_DRIVER      = 3; (**< unspecified issue loading a driver *)
  OUT123_NO_DRIVER       = 4; (**< no driver loaded *)
  OUT123_NOT_LIVE        = 5; (**< no active audio device *)
  OUT123_DEV_PLAY        = 6; (**< some device playback error *)
  OUT123_DEV_OPEN        = 7; (**< error opening device *)
  OUT123_BUFFER_ERROR    = 8; (**<
   * Some (really unexpected) error in buffer infrastructure.
   *)
  OUT123_MODULE_ERROR    = 9;  (**< basic failure in module loading *)
  OUT123_ARG_ERROR       = 10;  (**< some bad function arguments supplied *)
  OUT123_BAD_PARAM       = 11;  (**< unknown parameter code *)
  OUT123_SET_RO_PARAM    = 12;  (**< attempt to set read-only parameter *)
  OUT123_BAD_HANDLE      = 13;  (**< bad handle pointer (NULL, usually) *)
  OUT123_ERRCOUNT        = 14;  (**< placeholder for shaping arrays *)

(** Get string representation of last encountered error in the
 *  context of given handle.
 * \param ao handle
 * \return error string
 *)
var
  out123_strerror: Function(ao: out123_handle_p): PAnsiChar; cdecl;

(** Get the plain errcode intead of a string.
 * Note that this used to return OUT123_ERR instead of
 * OUT123_BAD_HANDLE in case of ao==NULL before mpg123-1.23.5 .
 * \param ao handle
 * \return error code recorded in handle or OUT123_BAD_HANDLE
 *)
  out123_errcode: Function(ao: out123_handle_p): int; cdecl;

(** Return the error string for a given error code.
 * \param errcode the integer error code
 * \return error string
 *)
  out123_plain_strerror: Function(errcode: int): PAnsiChar; cdecl;

(** Set a desired output buffer size.
 *  This starts a separate process that handles the audio output, decoupling
 *  the latter from the main process with a memory buffer and saving you the
 *  burden to ensure sparing CPU cycles for actual playback.
 *  This is for applicatons that prefer continuous playback over small latency.
 *  In other words: The kind of applications that out123 is designed for.
 *  This routine always kills off any currently active audio output module /
 *  device, even if you just disable the buffer when there is no buffer.
 *
 *  Keep this in mind for memory-constrainted systems: Activating the
 *  buffer causes a fork of the calling process, doubling the virtual memory
 *  use. Depending on your operating system kernel's behaviour regarding
 *  memory overcommit, it might be wise to call out123_set_buffer() very
 *  early in your program before allocating lots of memory.
 *
 *  There _might_ be a change to threads in future, but for now this is
 *  classic fork with shared memory, working without any threading library.
 *  If your platform or build does not support that, you will always get an
 *  error on trying to set up a non-zero buffer (but the API call will be
 *  present).
 *
 *  Also, if you do intend to use this from a multithreaded program, think
 *  twice and make sure that your setup is happy with forking full-blown
 *  processes off threaded programs. Probably you are better off spawning a
 *  buffer thread yourself.
 *
 * \param ao handle
 * \param buffer_bytes size (bytes) of a memory buffer for decoded audio,
 *    a value of zero disables the buffer.
 * \return 0 on success, OUT123_ERR on error
 *)
  out123_set_buffer: Function(ao: out123_handle_p; buffer_bytes: size_t): int; cdecl;

(** Set a specific parameter, for a specific out123_handle, using a parameter 
 *  code chosen from the out123_parms enumeration, to the specified value.
 *  The parameters usually only change what happens on next out123_open, not
 *  incfluencing running operation.
 * \param ao handle
 * \param code parameter code
 * \param value input value for integer parameters
 * \param fvalue input value for floating point parameters
 * \param svalue input value for string parameters (contens are copied)
 * \return 0 on success, OUT123_ERR on error.
 *)
  out123_param: Function(ao: out123_handle_p; code: out123_parms_t; value: long; fvalue: Double; svalue: PAnsiChar): int; cdecl;

  Function out123_param_int(ao: out123_handle_p; code: out123_parms_t; value: long): int;
  Function out123_param_float(ao: out123_handle_p; code: out123_parms_t; value: Double): int;
  Function out123_param_string(ao: out123_handle_p; code: out123_parms_t; value: PAnsiChar): int;

(** Get a specific parameter, for a specific out123_handle, using a parameter
 *  code chosen from the out123_parms enumeration, to the specified value.
 * \param ao handle
 * \param code parameter code
 * \param ret_value output address for integer parameters
 * \param ret_fvalue output address for floating point parameters
 * \param ret_svalue output address for string parameters (pointer to
 *        internal memory, so no messing around, please)
 * \return 0 on success, OUT123_ERR on error (bad parameter name or bad handle).
 *)
var
  out123_getparam: Function(ao: out123_handle_p; code: out123_parms_t; ret_value: plong; ret_fvalue: PDouble; ret_svalue: PPAnsiChar): int; cdecl;

  Function out123_getparam_int(ao: out123_handle_p; code: out123_parms_t; ret_value: plong): int;
  Function out123_getparam_float(ao: out123_handle_p; code: out123_parms_t; ret_value: PDouble): int;
  Function out123_getparam_string(ao: out123_handle_p; code: out123_parms_t; ret_value: PPAnsiChar): int;

(** Copy parameters from another out123_handle.
 * \param ao handle
 * \param from_ao the handle to copy parameters from
 * \return 0 in success, -1 on error
 *)
var
  out123_param_from: Function(ao, from_ao: out123_handle_p): int; cdecl;

(** Get list of driver modules reachable in system in C argv-style format.
 *  The client is responsible for freeing the memory of both the individual
 *  strings and the lists themselves.
 *  A module that is not loadable because of missing libraries is simply
 *  skipped. You will get stderr messages about that unless OUT123_QUIET was
 *  was set, though. Failure to open the module directory is a serious error,
 *  resulting in negative return value.
 * \param ao handle
 * \param names address for storing list of names
 * \param descr address for storing list of descriptions
 * \return number of drivers found, -1 on error
 *)
  out123_drivers: Function(ao: out123_handle_p; names, descr: PPPAnsiChar): int; cdecl;
{$IFDEF LEAK_WARNINGS}
  {$MESSAGE WARN 'Function out123_drivers is allocating buffers that cannot be freed. Use it with caution!'}
{$ENDIF}

(** Open an output device with a certain driver
 *  Note: Opening means that the driver code is loaded and the desired
 *  device name recorded, possibly tested for availability or tentatively
 *  opened. After out123_open(), you can ask for supported encodings
 *  and then really open the device for playback with out123_start().
 * \param ao handle
 * \param driver (comma-separated list of) output driver name(s to try),
 *               NULL for default (stdout for file-based drivers)
 * \param device device name to open, NULL for default
 * \return 0 on success, -1 on error.
 *)
  out123_open: Function(ao: out123_handle_p; driver, device: PAnsiChar): int; cdecl;

(** Give info about currently loaded driver and device
 *  Any of the return addresses can be NULL if you are not interested in
 *  everything. You get pointers to internal storage. They are valid
 *  as long as the driver/device combination is opened.
 *  The device may be NULL indicating some unnamed default.
 *  TODO: Make the driver modules return names for such defaults.
 * \param ao handle
 * \param driver return address for driver name
 * \param device return address for device name
 * \return 0 on success, -1 on error (i.e. no driver loaded)
 *)
  out123_driver_info: Function(ao: out123_handle_p; driver, device: PPAnsiChar): int; cdecl;

(** Close the current output device and driver.
 *  This implies out123_drain() to ensure no data is lost.
 *  With a buffer, that might cause considerable delay during
 *  which your main application is blocked waiting.
 *  Call out123_drop() beforehand if you want to end things
 *  quickly.
 * \param ao handle
 *)
  out123_close: procedure(ao: out123_handle_p); cdecl;

(** Get supported audio encodings for given rate and channel count,
 *  for the currently openend audio device.
 *  TODO: Reopening the underlying audio device for each query
 *        is dumb, at least when dealing with JACK. It takes
 *        a long time and is just a waste. Reconsider that.
 *        Make sure that all output modules are fine with it, though!
 *  Usually, a wider range of rates is supported, but the number
 *  of sample encodings is limited, as is the number of channels.
 *  So you can call this with some standard rate and hope that the
 *  returned encodings work also for others, with the tested channel
 *  count.
 *  The return value of -1 on some encountered error conveniently also
 *  does not match any defined format (only 15 bits used for encodings,
 *  so this would even work with 16 bit integers).
 *  This implies out123_stop() to enter query mode.
 * \param ao handle
 * \param rate sampling rate
 * \param channels number of channels
 * \return supported encodings combined with bitwise or, to be checked
 *         against your favourite bitmask, -1 on error
 *)
  out123_encodings: Function(ao: out123_handle_p; rate: long; channels: int): int; cdecl;

(** Return the size (in bytes) of one mono sample of the named encoding.
 * \param encoding The encoding value to analyze.
 * \return positive size of encoding in bytes, 0 on invalid encoding. *)
  out123_encsize: Function(encoding: int): int; cdecl;

(** Get list of supported formats for currently opened audio device.
 *  Given a list of sampling rates and minimal/maximal channel count,
 *  this quickly checks what formats are supported with these
 *  constraints. The first entry is always reserved for a default
 *  format for the output device. If there is no such default,
 *  all values of the format are -1.
 *  For each requested combination of rate and channels, a format entry is
 *  created, possible with encoding value 0 to indicate that this combination
 *  has been tested and rejected. So, when there is no basic error, the
 *  number of returned format entries should be
 *     (ratecount*(maxchannels-minchannels+1)+1)
 *  . But instead of forcing you to guess, this will be allocated by
 *  successful run.
 *  For the first entry, the encoding member is supposed to be a definite
 *  encoding, for the others it is a bitwise combination of all possible
 *  encodings.
 *  This function is more efficient than many calls to out123_encodings().
 * \param ao handle
 * \param rates pointer to an array of sampling rates, may be NULL for none
 * \param ratecount number of provided sampling rates
 * \param minchannels minimal channel count
 * \param maxchannels maximal channel count
 * \param fmtlist return address for array of supported formats
 *        the encoding field of each entry is a combination of all
 *        supported encodings at this rate and channel count;
 *        Memory shall be freed by user.
 * \return number of returned format enries, -1 on error
 *)
  out123_formats: Function(ao: out123_handle_p; rates: plong; ratecount, minchannels, maxchannels: int; fmtlist: mpg123_fmt_pp): int; cdecl;
{$IFDEF LEAK_WARNINGS}
  {$MESSAGE WARN 'Function out123_formats is allocating buffer that cannot be freed. Use it with caution!'}
{$ENDIF}

(** Get list of encodings known to the library.
 *  You are responsible for freeing the allocated array.
 * \param enclist return address for allocated array of encoding codes
 * \return number of encodings, -1 on error
 *)
  out123_enc_list: Function(enclist: ppint): int; cdecl;
{$IFDEF LEAK_WARNINGS}
  {$MESSAGE WARN 'Function out123_enc_list is allocating buffer that cannot be freed. Use it with caution!'}
{$ENDIF}

(** Find encoding code by name.
 * \param name short or long name to find encoding code for
 * \return encoding if found (enum mpg123_enc_enum), else 0
 *)
  out123_enc_byname: Function(name: PAnsiChar): int; cdecl;

(** Get name of encoding.
 * \param encoding code (enum mpg123_enc_enum)
 * \return short name for valid encodings, NULL otherwise
 *)
  out123_enc_name: Function(encoding: int): PAnsiChar; cdecl;

(** Get long name of encoding.
 * \param encoding code (enum mpg123_enc_enum)
 * \return long name for valid encodings, NULL otherwise
 *)
  out123_enc_longname: Function(encoding: int): PAnsiChar; cdecl;

(** Start playback with a certain output format
 *  It might be a good idea to have audio data handy to feed after this
 *  returns with success.
 *  Rationale for not taking a pointer to struct mpg123_fmt: This would
 *  always force you to deal with that type and needlessly enlarge the
 *  shortest possible program.
 * \param ao handle
 * \param encoding sample encoding (values matching libmpg123 API)
 * \param channels number of channels (1 or 2, usually)
 * \param rate sampling rate
 * \return 0 on success, negative on error (bad format, usually)
 *)
  out123_start: Function(ao: out123_handle_p; rate: long; channels, encoding: int): int; cdecl;

(** Pause playback
 *  Interrupt playback, holding any data in the optional buffer.
 *
 *  This closes the audio device if it is a live sink, ready to be re-opened
 *  by out123_continue() or out123_play() with the existing parameters.
 * \param ao handle
 *)
  out123_pause: procedure(ao: out123_handle_p); cdecl;

(** Continue playback
 *  The counterpart to out123_pause(). Announce to the driver that playback
 *  shall continue.
 *
 *  Playback might not resume immediately if the optional buffer is configured
 *  to wait for a minimum fill and close to being empty. You can force playback
 *  of the last scrap with out123_drain(), or just by feeding more data with
 *  out123_play(), which will trigger out123_continue() for you, too.
 * \param ao handle
 *)
  out123_continue: procedure(ao: out123_handle_p); cdecl;

(** Stop playback.
 *  This waits for pending audio data to drain to the speakers.
 *  You might want to call out123_drop() before stopping if you want
 *  to end things right away.
 * \param ao handle
 *)
  out123_stop: procedure(ao: out123_handle_p); cdecl;

(** Hand over data for playback and wait in case audio device is busy.
 *  This survives non-fatal signals like SIGSTOP/SIGCONT and keeps on
 *  playing until the buffer is done with if the flag
 *  OUT123_KEEP_PLAYING ist set (default). So, per default, if
 *  you provided a byte count divisible by the PCM frame size, it is an
 *  error when less bytes than given are played.
 *  To be sure if an error occured, check out123_errcode().
 *  Also note that it is no accident that the buffer parameter is not marked
 *  as constant. Some output drivers might need to do things like swap
 *  byte order. This is done in-place instead of wasting memory on yet
 *  another copy. 
 * \param ao handle
 * \param buffer pointer to raw audio data to be played
 * \param bytes number of bytes to read from the buffer
 * \return number of bytes played (might be less than given, even zero)
 *)
  out123_play: Function(ao: out123_handle_p; buffer: Pointer; bytes: size_t): size_t; cdecl;

(** Drop any buffered data, making next provided data play right away.
 *  This does not imply an actual pause in playback.
 *  You are expected to play something, unless you called out123_pause().
 *  Feel free to call out123_stop() afterwards instead for a quicker
 *  exit than the implied out123_drain().
 *  For live sinks, this may include dropping data from their buffers.
 *  For others (files), this only concerns data in the optional buffer.
 * \param ao handle
 *)
  out123_drop: procedure(ao: out123_handle_p); cdecl;

(** Drain the output, waiting until all data went to the hardware.
 * This does imply out123_continue() before and out123_pause()
 * after draining.
 * This might involve only the optional buffer process, or the
 * buffers on the audio driver side, too.
 * \param ao handle
 *)
  out123_drain: procedure(ao: out123_handle_p); cdecl;

(** Drain the output, but only partially up to the given number of
 *  bytes. This gives you the opportunity to do something while
 *  the optional buffer is writing remaining data instead of having
 *  one atomic API call for it all.
 *
 *  It is wholly expected that the return value of out123_buffered()
 *  before and after calling this has a bigger difference than the
 *  provided limit, as the buffer is writing all the time in the
 *  background.
 *
 *  This is just a plain out123_drain() if the optional buffer is not
 *  in use. Also triggers out123_continue(), but only out123_pause()
 *  if there is no buffered data anymore.
 * \param ao handle
 * \param bytes limit of buffered bytes to drain
 * \return number of bytes drained from buffer
 *)
  out123_ndrain: procedure(ao: out123_handle_p; bytes: size_t); cdecl;

(** Get an indication of how many bytes reside in the optional buffer.
 * This might get extended to tell the number of bytes queued up in the
 * audio backend, too.
 * \param ao handle
 * \return number of bytes in out123 library buffer
 *)
  out123_buffered: procedure(ao: out123_handle_p); cdecl;

(** Extract currently used audio format from handle.
 *  matching mpg123_getformat().
 *  Given return addresses may be NULL to indicate no interest.
 * \param ao handle
 * \param rate address for sample rate
 * \param channels address for channel count
 * \param encoding address for encoding
 * \param framesize size of a full PCM frame (for convenience)
 * \return 0 on success, -1 on error
 *)
  out123_getformat: Function(ao: out123_handle_p; rate: plong; channels, encoding, framesize: pint): int; cdecl;

(* @} *)

//==============================================================================

const
  out123_LibFileName = 'out123.dll';

Function out123_Initialize(const LibPath: String = out123_LibFileName): Boolean;
procedure out123_Finalize;

implementation

uses
  DynLibUtils;

//== Macro implementation ======================================================

Function out123_param_int(ao: out123_handle_p; code: out123_parms_t; value: fmt123.long): int;
begin
Result := out123_param(ao,code,value,0.0,nil);
end;

//------------------------------------------------------------------------------

Function out123_param_float(ao: out123_handle_p; code: out123_parms_t; value: Double): int;
begin
Result := out123_param(ao,code,0,value,nil);
end;

//------------------------------------------------------------------------------

Function out123_param_string(ao: out123_handle_p; code: out123_parms_t; value: PAnsiChar): int;
begin
Result := out123_param(ao,code,0,0.0,value);
end;

//------------------------------------------------------------------------------

Function out123_getparam_int(ao: out123_handle_p; code: out123_parms_t; ret_value: fmt123.plong): int;
begin
Result := out123_getparam(ao,code,ret_value,nil,nil);
end;

//------------------------------------------------------------------------------

Function out123_getparam_float(ao: out123_handle_p; code: out123_parms_t; ret_value: PDouble): int;
begin
Result := out123_getparam(ao,code,nil,ret_value,nil);
end;

//------------------------------------------------------------------------------

Function out123_getparam_string(ao: out123_handle_p; code: out123_parms_t; ret_value: PPAnsiChar): int;
begin
Result := out123_getparam(ao,code,nil,nil,ret_value);
end;

//==============================================================================

var
  LibContext: TDLULibraryContext;

//------------------------------------------------------------------------------  

Function out123_Initialize(const LibPath: String = out123_LibFileName): Boolean;
begin
Result := OpenLibraryAndResolveSymbols(LibPath,LibContext,[
  Symbol(@@out123_new           ,'out123_new'),
  Symbol(@@out123_del           ,'out123_del'),
  Symbol(@@out123_strerror      ,'out123_strerror'),
  Symbol(@@out123_errcode       ,'out123_errcode'),
  Symbol(@@out123_plain_strerror,'out123_plain_strerror'),
  Symbol(@@out123_set_buffer    ,'out123_set_buffer'),
  Symbol(@@out123_param         ,'out123_param'),
  Symbol(@@out123_getparam      ,'out123_getparam'),
  Symbol(@@out123_param_from    ,'out123_param_from'),
  Symbol(@@out123_drivers       ,'out123_drivers'),
  Symbol(@@out123_open          ,'out123_open'),
  Symbol(@@out123_driver_info   ,'out123_driver_info'),
  Symbol(@@out123_close         ,'out123_close'),
  Symbol(@@out123_encodings     ,'out123_encodings'),
  Symbol(@@out123_encsize       ,'out123_encsize'),
  Symbol(@@out123_formats       ,'out123_formats'),
  Symbol(@@out123_enc_list      ,'out123_enc_list'),
  Symbol(@@out123_enc_byname    ,'out123_enc_byname'),
  Symbol(@@out123_enc_name      ,'out123_enc_name'),
  Symbol(@@out123_enc_longname  ,'out123_enc_longname'),
  Symbol(@@out123_start         ,'out123_start'),
  Symbol(@@out123_pause         ,'out123_pause'),
  Symbol(@@out123_continue      ,'out123_continue'),
  Symbol(@@out123_stop          ,'out123_stop'),
  Symbol(@@out123_play          ,'out123_play'),
  Symbol(@@out123_drop          ,'out123_drop'),
  Symbol(@@out123_drain         ,'out123_drain'),
  Symbol(@@out123_ndrain        ,'out123_ndrain'),
  Symbol(@@out123_buffered      ,'out123_buffered'),
  Symbol(@@out123_getformat     ,'out123_getformat')],True) = 30;
end;

//------------------------------------------------------------------------------

procedure out123_Finalize;
begin
CloseLibrary(LibContext);
end;

//==============================================================================

initialization
  LibContext := DefaultLibraryContext;

end.
