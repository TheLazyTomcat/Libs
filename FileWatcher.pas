{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  FileWatcher

    This library provides a simple TFileWatcher class that can be used to
    watch for changes in files and directories (eg. to watch when a certain
    file is changed or when something is deleted from a selected directory).

    As it is only a wrapper around system-provided services, its behaviour
    and potential limitations will differ between operating systems - refer
    to description of individual types and methods for more information.

    To use it, simply create an TFileWatcher instance, providing path to
    file or directory to be watched and approptiate watch options. Then
    assign handler to one of the OnChange* event/callback property - this
    handler will be notified when a change is catched. Finally, repeatedly
    call WaitForChanges or GetChanges methods - these are reading changes
    buffered by the system and pass them to abovementioned handler for
    further processing.

  Version 1.0 (2026-08-01)

  Last change 2026-08-01

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.FileWatcher

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
  * MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol FileWatcher_UseAuxExceptions for details).

  Library MulticastEvent is required only when compiling for Linux operating
  system.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    ListUtils   - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit FileWatcher;
{
  FileWatcher_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  FileWatcher_UseAuxExceptions to achieve this.
}
{$IF Defined(FileWatcher_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFpc}
  {$MODESWITCH ClassicProcVars+}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, {$IFDEF Windows}Windows,{$ELSE}BaseUnix,{$ENDIF}
  {$IFNDEF Windows}MulticastEvent,{$ENDIF} AuxTypes, AuxClasses, StrRect
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EFWException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EFWSystemError  = class(EFWException);
  EFWInvalidValue = class(EFWException);

{$IFNDEF Windows}
{===============================================================================
--------------------------------------------------------------------------------
                                TChangeDispatcher
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TChangeDispatchEvent

  Internal-only, do not use.
}
  TChangeDispatchEvent = Function(Sender: TObject; Watch: cint; Mask,Cookie: UInt32; const Name: TSysString): Boolean of object;

{===============================================================================
    TChangeDispatcher - class declaration
===============================================================================}
type
{
  TChangeDispatcher

  Internal-only, do not use.
}
  TChangeDispatcher = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TChangeDispatchEvent): Integer; reintroduce;
    Function Find(const Handler: TChangeDispatchEvent; out Index: Integer): Boolean; reintroduce;
    Function Add(const Handler: TChangeDispatchEvent; AllowDuplicity: Boolean = False): Integer; reintroduce;
    Function Remove(const Handler: TChangeDispatchEvent; RemoveAll: Boolean = True): Integer; reintroduce;
    procedure Call(Sender: TObject; Watch: cint; Mask,Cookie: UInt32; Name: TSysString); reintroduce;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TINotify
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TINotify - class declaration
===============================================================================}
{
  TINotify

  Do not directly use instances of this class - see description of TFileWatcher
  constructors for information on how to use it.
}
type
  TINotify = class(TCustomRefCountedObject)
  protected
    fInstance:    cint;
    fReadBuffer:  Pointer;
    fOnChange:    TChangeDispatcher;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    property OnChange: TChangeDispatcher read fOnChange;
  public
    class procedure TimeGetActual(out Time: TTimeSpec); virtual;
    class Function TimeGetElapsedMillis(const From: TTimeSpec): UInt32; virtual;
    constructor Create;
    destructor Destroy; override;
    Function WaitForChanges(Timeout: UInt32): Boolean; virtual;
    procedure ReadAndDispatchChanges; virtual;
    property Instance: cint read fInstance;
  end;

{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                  TFileWatcher
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TFWWatchOptions

  Used in TFileWatcher constructors to select how and what should be watched
  for changes.

    optWatchFile    - when active, it instructs the watcher that the watched
                      path points to a file (make sure it does, otherwise an
                      exception will likely be raised). If not active then the
                      path is assumed to be a directory (again, it must exist).
                      It does not change much externally, but it has effects
                      on internal workings, so make sure to use it properly.

    optWatchSubtree - observed only when watching a directory. Changes made
                      to all subdirectories withing the watched directory are
                      also reported when this option is active, otherwise only
                      changes directly in it are reported, change in subdirs
                      are ignored.

                        WARNING - this option works only on Windows OS, it is
                                  ignored in Linux (inotify does not provide
                                  this functionality and simulating it could
                                  be very problematic for large trees).

    optWatchName    - watch for file creation, deletion, renaming or move.
                      If watching a directory, only its content is watched,
                      not the directory itself.

    optWatchState   - watch for changes in states, eg. change in file size or
                      attributes (this includes if the file is written into).
                      If more than one attribute is changed, then each attribute
                      change is reported separately.
                      If directory is watched, then only changes to its content
                      are reported, not to the directory itself.

  At least one of optWatchName or optWatchState must be active, otherwise the
  constructor will fail with EFWInvalidValue exception.
}
  TFWWatchOptions = set of (optWatchFile,optWatchSubtree,optWatchName,optWatchState);

const
{
  WATCH_ALL_CHANGES

  Use this constant for WatchOptions if zou want to be informed about all
  supported changes to watched file or directory.
}
  WATCH_ALL_CHANGES = [optWatchName,optWatchState];

{
  INFINITE

  When used for Timeout, it forces the waiting function to wait for indefinite
  time, returning only when change is encountered.

    WARNING - it is NOT recommended to use infinite waiting, as it is
              very easy to get into situation where no change will ever
              be reported, effectively dead-locking the calling thread.
}
  INFINITE = UInt32(-1);

type
{
  TFWChangeWaitResult

  Used to indicate result of waiting for changes (method WaitForChanges).

    wrChange   - at least one change occured and was passed to handler for
                 processing

    wrTimeout  - no change was signaled in the prescribed timeout interval

    wrError    - an erroneous condition was encountered (returned when the
                 WaitForChanges method is called from within change handler)
}
  TFWChangeWaitResult = (wrChange,wrTimeout,wrError);

type
{
  TFWChange

  This enumeration is used to inform handler of OnChange* event/callback about
  what change to watched path was detected.

    chnAdded       - as file or directory was create in watched directory,
                     in Windows this is also reported when a file matching
                     watched file is created

    chnRemoved     - file or directory was removed from watched directory,
                     in Windows this is also reported when watched file is
                     deleted

    chnRenamedFrom - reported for a file that is being renamed or moved,
                     it migh but also might not be followed by chnRenamedTo,
                     depending on where the moved/renamed file ends-up,
                     FileName will contain the original name

    chnRenamedTo   - reported for the target when a file is renamed or moved,
                     it migh but also might not be preceded by chnRenamedFrom,
                     depending on where the source file resided,
                     FileName will contain the new name

    chnChanged     - reported for any other change to watched file or files
                     and directories in watched directory (eg. change in size,
                     access/write/creation time or other attributes)

    chnUnknown     - this should never be reported as it is used only
                     internally - if you encounter it, ignore it
}
  TFWChange = (chnAdded,chnRemoved,chnRenamedFrom,chnRenamedTo,chnChanged,chnUnknown);

{
  TFWChangeEvent
  TFWChangeCallback

  Used to report occuring changes to watched file or directory

  Change will indicate what change occured, FileName will contain name of the
  file or directory that was changed.
}
  TFWChangeEvent    = procedure(Sender: TObject; Change: TFWChange; const FileName: String) of object;
  TFWChangeCallback = procedure(Sender: TObject; Change: TFWChange; const FileName: String);

{===============================================================================
    TFileWatcher - class declaration
===============================================================================}
type
  TFileWatcher = class(TCustomObject)
  protected
    fWatchOptions:        TFWWatchOptions;
    fWatchedPath:         String;
    fOnChangeEvent:       TFWChangeEvent;
    fOnChangeCallback:    TFWChangeCallback;
    fLostChanges:         Boolean;
 {$IFDEF Windows}
    fClockFrequency:      Int64;
    fWatchingFile:        Boolean;
    fWatchedDir:          String;
    fWatchedFile:         String;
    fWatchFilter:         DWORD;
    fCompletionEvent:     THandle;
    fDirectoryHandle:     THandle;
    fChangeBufferBase:    Pointer;
    fChangeBufferSize:    TMemSize;
    fChangeBuffer:        Pointer;
    fOverlapped:          TOverlapped;
    fOverlappedPending:   Boolean;
 {$ELSE}
    fWatchedFile:         String;
    fINotifyInstance:     TINotify;
    fINotifyWatch:        cint;
    fReceivedChanges:     Integer;
 {$ENDIF}
    fInChangeHandler:     Boolean;
    Function GetLostChanges: Boolean; virtual;
  {$IFDEF Windows}
    Function ClockGetActual: Int64; virtual;
    Function ClockMillisFrom(const Value: Int64): Int64; virtual;
    procedure ChangesReadStart; virtual;
    Function ChangesReadWait(Timeout: UInt32): Boolean; virtual;
    Function ChangesProcess: Boolean; virtual;
 {$ELSE}
    Function OnChangeHandler(Sender: TObject; Watch: cint; Mask,Cookie: UInt32; const Name: TSysString): Boolean; virtual;
 {$ENDIF}
    procedure Initialize({$IFNDEF Windows}INotifyInstance: TINotify; {$ENDIF}const WatchedPath: String; WatchOptions: TFWWatchOptions); virtual;
    procedure Finalize; virtual;
    procedure DoChange(Change: TFWChange; const Path: String); virtual;
  public
  {
    Create
    WatchDirectory
    WatchFile

    WatchDirectory is only a macro calling the Create constructor while
    ensuring the WatchOptions contains optWatchFile option. Similarly,
    WatchFile calls Create while ensuring the WatchOptions does NOT cantain
    optWatchFile option. For more information regarding watch options, refer
    to description of type TFWWatchOptions.

      NOTE - in Windows, it is not possible to watch a file for changes. Here
             it is implemented as a watch for changes in containing directory
             while filtering occuring changes and reporting only those that
             happen on the watched file.

      WARNING - watched file or directory must exist, otherwise an exception
                is raised in the constructor.

    Overloads accepting INotifyInstance argument (present only in Linux) are
    here one reason - in Linux, the implementation uses inotify instance to
    watch for changes, but number of inotify instances is severely limited
    per user (128 in systems I have tried). Therefore the inotify instance is
    created separately from TFileWatcher object so it can be used multiple-
    times.

      NOTE - use the TINotify instance only within a single thread, as it
             is not thread safe. Create new instance(s) for other thread(s).

    If you use constructor that does not accept TINotify object, then it will
    be created automatically during initialization. This internally created
    instance can be used for other objects too (it is publicly available trough
    INotifyInstance property).

    Note that TINotify objects are reference counted. If the instance is
    created internally, it is also immediately acquired - no matter how many
    times you then use this instance, it will be automatically destroyed when
    last object using it is freed.
    For manually created instances, it is more complicated - if you just create
    the instance and not use it (you do not pass it to constructor), then you
    are responsible for freeing it. But the moment is is used (passed to any
    constructor), it is acquired and from this moment reference counted (will
    be automatically freed when last object that is using it is destroyed).
  }
  {$IFNDEF Windows}
    constructor Create(INotifyInstance: TINotify; const WatchedPath: String; WatchOptions: TFWWatchOptions); overload;
    constructor WatchDirectory(INotifyInstance: TINotify; const DirectoryPath: String; WatchOptions: TFWWatchOptions); overload;
    constructor WatchFile(INotifyInstance: TINotify; const FilePath: String; WatchOptions: TFWWatchOptions); overload;
  {$ENDIF}
    constructor Create(const WatchedPath: String; WatchOptions: TFWWatchOptions); {$IFNDEF Windows} overload;{$ENDIF}
    constructor WatchDirectory(const DirectoryPath: String; WatchOptions: TFWWatchOptions{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); {$IFNDEF Windows} overload;{$ENDIF}
    constructor WatchFile(const FilePath: String; WatchOptions: TFWWatchOptions{$IFNDEF FPC}; Dummy: LongWord = 0{$ENDIF}); {$IFNDEF Windows} overload;{$ENDIF}
    destructor Destroy; override;
  {
    WaitForChanges

    Waits for a change to occur in watched directory or file, but for at most
    Timeout milliseconds. The function will block until either the timeout
    elapses (in which case wrTimeout is returned), a change in watched path
    occurs (returns wrChange) or an erroneous state is encountered (wrError).

    Timeout can be set to INIFINITE, but I would STRONGLY discourage you from
    doing that - it is very easy to get into situation where that would block
    the calling thread indefinitely for real.

    If a change is catched by the call (it will see only changes that occured
    from last call to WaitForChanges or GetChanges, or from object creation),
    it is passed to handler of OnChange* event property for processing. Note
    that more (and possibly many more) than one change can be passed within
    a single call to this method. After the handler returns, this function
    will also exit, returning wrChange.

      NOTE - you should check property LostChanges after each call to this
             method (see there for details), irrespective of result value.

    Do not call this method from handler of OnChange* event - If you do, it
    will immediately exit without doing anything, returning wrError.
  }
    Function WaitForChanges(Timeout: UInt32): TFWChangeWaitResult; virtual;
  {
    GetChanges

    Works exactly the same as WaitForChanges (see its description for more
    details), except it does not wait for changes to occur - it checks whether
    there were any from previous call to GetChanges or WaitForChanges and
    processes those that occured. It is not completely equivalent to calling
    WaitForChanges with zero timeout, but functionally there is not much
    difference.

    Returns true if any change occured and was passed to handlers, false
    otherwise.

      NOTE - you should check property LostChanges after each call to this
             method (see there for details), irrespective of result value.

    Do not call this method from handler of OnChange* event - If you do, it
    will immediately exit without doing anything, returning false.
  }
    Function GetChanges: Boolean; virtual;
  {
    WatchOptions

    These are wait options used internally by the object - they might not be
    completely the same as watch options passed to constructor because they
    are rectified before use.
  }
    property WatchOptions: TFWWatchOptions read fWatchOptions;
  {
    WatchedPath

    Same as property WatchOptions - this is path used internally. It is based
    on path passed to constructor, but might not completely match it.
  }
    property WatchedPath: String read fWatchedPath;
  {
    OnChange[Event/Callback]

    This event/callback is a way how the object informs outer code about
    change occurence. So, to get notified about change in watched path,
    assign handler to one of the provided OnChange* property (note that
    OnChange and OnChangeEvent are aliases to the same property).

    Generally, assigned handlers are called from within WaitForChanges and
    GetChanges methods when they pick-up a change, but...

      NOTE - in Linux, when using shared inotify instance (see constructors
             for more info). the assigned handler can be called even if you
             do not call WaitForChanges or GetChanges. This is because the
             change dispatching is controlled by the shared inotify instance,
             and if some other TFileWatcher uses it to dispatch changes, they
             will be dispatched to all existing objects using the same inotify
             instance (the object internally checks whether the change belongs
             to it before passing it, so you will not get changes registered
             elsewhere).

    If both event and callback are assigned, then only the event is called.
    Even if no handler is assigned, the processing goes as if it would be
    (the changes are "consumed", but not reported anywhere).

      WARNING - do not call methods WaitForChanges and GetChanges from
                within the handler. That could create infinite indirect
                recursion, which would eventually lead to stack overflow
                and crash.
                Mentioned methods are protected against this - see their
                descriptions for more information.
  }
    property OnChange: TFWChangeEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TFWChangeEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TFWChangeCallback read fOnChangeCallback write fOnChangeCallback;
  {
    LostChanges

    This property indicates whether some changes to watched path were lost
    (this happens due to limitations in operating systems, both Windows and
    Linux). It reads as false if no changes were lost, if true then it means
    at least one, possibly/probably more, change was lost and will not be
    reported.

      WARNING - reading this property always resets it (sets it to false),
                so you have to store its value if you want to work with it
                any further. This is to allow discerning time when the loss
                happened.

    You should check this property after every call to methods WaitForChanges
    and GetChanges, as both can set it, irrespective of their result values.
  }
    property LostChanges: Boolean read GetLostChanges;
  {$IFNDEF Windows}
    property INotifyInstance: TINotify read fINotifyInstance;
  {$ENDIF}
  end;

implementation

{$IFNDEF Windows}
uses
  Linux;
{$ENDIF}

{$IFNDEF Windows}
  {$LINKLIB C}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

{===============================================================================
    Auxiliary stuff
===============================================================================}
const
  FW_CHANGEBUFFER_SIZE = 64 * 1024; // 64KiB

{$IFNDEF Windows}
//------------------------------------------------------------------------------

Function ConsumeArg(const Arg: UInt32): Integer; overload;
begin
Result := Integer(Arg);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(const Arg: TObject): Integer; overload;
begin
Result := Integer(Arg.InstanceSize);
end;

//------------------------------------------------------------------------------

Function Ptr2UInt(const Ptr: Pointer): PtrUInt;
var
  iPtr: PtrInt absolute Ptr;
begin
Result := iPtr;
end;

{$ENDIF}
{===============================================================================
    System calls, constants, types, ...
===============================================================================}
{$IFDEF Windows}

{$IFDEF FPC}
const
  FILE_ACTION_ADDED            = $00000001;
  FILE_ACTION_REMOVED          = $00000002;
  FILE_ACTION_MODIFIED         = $00000003;
  FILE_ACTION_RENAMED_OLD_NAME = $00000004;
  FILE_ACTION_RENAMED_NEW_NAME = $00000005;

  FILE_NOTIFY_CHANGE_LAST_ACCESS = $00000020;
  FILE_NOTIFY_CHANGE_CREATION    = $00000040;

type
  TFileIOCompletionRoutine = procedure(dwErrorCode: DWORD; dwNumberOfBytesTransfered: DWORD; lpOverlapped: POverlapped); stdcall;

Function ReadDirectoryChangesW(
  hDirectory:           THandle;
  lpBuffer:             Pointer;
  nBufferLength:        DWORD;
  bWatchSubtree:        BOOL;
  dwNotifyFilter:       DWORD;
  lpBytesReturned:      PDWORD;
  lpOverlapped:         POverlapped;
  lpCompletionRoutine:  TFileIOCompletionRoutine
): BOOL; stdcall; external kernel32;
{$ENDIF}

{$ELSE}//-----------------------------------------------------------------------

Function errno_ptr: pcInt; cdecl; external name '__errno_location';
Function close(fd: cint): cint; cdecl; external;

type
  TINotifyEvent = record
    wd:     cint;       // Watch descriptor
    mask:   UInt32;     // Mask describing event
    cookie: UInt32;     // Unique cookie associating related events (for rename(2))
    len:    UInt32;     // Size of name field
    name:   record end; // Optional null-terminated name (here zero-size placeholder)
  end;
  PINotifyEvent = ^TINotifyEvent;

Function inotify_init: cint; cdecl; external;

Function inotify_add_watch(fd: cint; pathname: PSysChar; mask: UInt32): cint; cdecl; external;
Function inotify_rm_watch(fd: cint; wd: cint): cint; cdecl; external;

Function clock_gettime(clockid: clockid_t; tp: PTimeSpec): cint; cdecl; external;

type
  TPollFD = record
    fd:       cint;   // file descriptor
    events:   cshort; // requested events
    revents:  cshort; // returned events
  end;
  PPollFD = ^TpollFD;

  nfds_t = culong;

Function poll(fds: PPollFD; nfds: nfds_t; timeout: cint): cint; cdecl; external;

Function read(fd: cint; buf: Pointer; count: size_t): ssize_t; cdecl; external;

{===============================================================================
--------------------------------------------------------------------------------
                                TChangeDispatcher
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TChangeDispatcher - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TChangeDispatcher - public methods
-------------------------------------------------------------------------------}

Function TChangeDispatcher.IndexOf(const Handler: TChangeDispatchEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TChangeDispatcher.Find(const Handler: TChangeDispatchEvent; out Index: Integer): Boolean;
begin
Result := inherited Find(TEvent(Handler),Index);
end;

//------------------------------------------------------------------------------

Function TChangeDispatcher.Add(const Handler: TChangeDispatchEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TChangeDispatcher.Remove(const Handler: TChangeDispatchEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TChangeDispatcher.Call(Sender: TObject; Watch: cint; Mask,Cookie: UInt32; Name: TSysString);
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If fEntries[i].IsMethod then
    If TChangeDispatchEvent(fEntries[i].HandlerMethod)(Sender,Watch,Mask,Cookie,Name) then
      Break{For i};
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TINotify
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TINotify - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TINotify - protected methods
-------------------------------------------------------------------------------}

procedure TINotify.Initialize;
begin
FreeOnRelease := True;
fInstance := inotify_init;
If fInstance = -1 then
  raise EFWSystemError.CreateFmt('TINotify.Initialize: Failed to create inotify instance (%d).',[errno_ptr^]);
fReadBuffer := AllocMem(FW_CHANGEBUFFER_SIZE);
fOnChange := TChangeDispatcher.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TINotify.Finalize;
begin
FreeAndNil(fOnChange);
FreeMem(fReadBuffer,FW_CHANGEBUFFER_SIZE);
If close(fInstance) = -1 then
  raise EFWSystemError.CreateFmt('TINotify.Finalize: Failed to close inotify instance (%d).',[errno_ptr^]);
end;

{-------------------------------------------------------------------------------
    TINotify - public methods
-------------------------------------------------------------------------------}

class procedure TINotify.TimeGetActual(out Time: TTimeSpec);
begin
If clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = -1 then
  raise EFWSystemError.CreateFmt('TINotify.TimeGetActual: Unable to obtain time (%d).',[errno_ptr^]);
end;

//------------------------------------------------------------------------------
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}

class Function TINotify.TimeGetElapsedMillis(const From: TTimeSpec): UInt32;
var
  ActualTime: TTimeSpec;
  Temp:       Int64;
begin
TimeGetActual(ActualTime);
Temp := ((Int64(ActualTime.tv_sec) - From.tv_sec) * 1000) +
        ((Int64(ActualTime.tv_nsec) - From.tv_nsec) div 1000000);
If (Temp < 0) or (Temp > High(cint)) then
  Result := High(cint)
else
  Result := Uint32(Temp);
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
//------------------------------------------------------------------------------

constructor TINotify.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TINotify.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TINotify.WaitForChanges(Timeout: UInt32): Boolean;
var
  ActualTimeout:  cint;
  PollFD:         TPollFD;
  StartTime:      TTimeSpec;
  ContinueWait:   Boolean;
  PollRes:        cint;
  ErrNum:         cint;
  ElapsedMillis:  UInt32;
begin
Result := False;
If Timeout <> INFINITE then
  ActualTimeout := cint(Timeout and (UInt32(-1) shr 1))
else
  ActualTimeout := cint(-1);
FillChar(Addr(PollFD)^,SizeOf(PollFD),0);
PollFD.fd := fInstance;
PollFD.events := POLLIN;
TimeGetActual(StartTime);
repeat
  ContinueWait := False;
  PollFD.revents := 0;
  PollRes := poll(@PollFD,1,ActualTimeout);
  If PollRes <> 0 then
    begin
      ErrNum := errno_ptr^;
      If (PollRes = -1) and (ErrNum <> ESysEINTR) then
        raise EFWSystemError.CreateFmt('TINotify.WaitForChanges: Failed to wait on inotify (%d).',[ErrNum])
      else If (PollRes > 0) and ((PollFD.revents and POLLIN) <> 0) then
        Result := True
      else
        begin
        {
          Poll was interrupted by signal or some unexpected event ended the
          wait (should not happen, but...), recalculate timeout and re-enter
          waiting.
        }
          ContinueWait := True;
          ElapsedMillis := TimeGetElapsedMillis(StartTime);
          If ElapsedMillis < Timeout then
            ActualTimeout := Timeout - ElapsedMillis
          else
            Break{repeat}; // result is already set to false
        end;
    end
  else Result := False; // result of zero means timeout
until not ContinueWait;
end;

//------------------------------------------------------------------------------

procedure TINotify.ReadAndDispatchChanges;
var
  BytesRead:  ssize_t;
  EventPtr:   PINotifyEvent;
  NameStr:    TSysString;
begin
BytesRead := read(fInstance,fReadBuffer,FW_CHANGEBUFFER_SIZE);
EventPtr := PINotifyEvent(fReadBuffer);
NameStr := '';
while BytesRead > 0 do
  begin
    If EventPtr^.len > 0 then
      begin
        SetLength(NameStr,StrLen(Addr(EventPtr^.name)));
        Move(EventPtr^.name,PSysChar(NameStr)^,Length(NameStr) * SizeOf(TSysChar));
      end
    else NameStr := '';
    // IN_Q_OVERFLOW is managed by watchers
    fOnChange.Call(Self,EventPtr^.wd,EventPtr^.mask,EventPtr^.cookie,NameStr);
    Dec(BytesRead,SizeOf(TINotifyEvent) + Integer(EventPtr^.len));
    Inc(PByte(EventPtr),SizeOf(TINotifyEvent) + Integer(EventPtr^.len));
  end;
end;

{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                  TFileWatcher
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFileWatcher - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFileWatcher - protected methods
-------------------------------------------------------------------------------}

Function TFileWatcher.GetLostChanges: Boolean;
begin
Result := fLostChanges;
fLostChanges := False;
end;

{$IFDEF Windows}
//------------------------------------------------------------------------------

Function TFileWatcher.ClockGetActual: Int64;
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise EFWSystemError.CreateFmt('TFileWatcher.ClockGetActual: Failed to query clock (%u).',[GetLastError]);
Result := Result and (Int64(-1) shr 1);
end;

//------------------------------------------------------------------------------

Function TFileWatcher.ClockMillisFrom(const Value: Int64): Int64;
var
  ActualClock:  Int64;
begin
ActualClock := ClockGetActual;
If ActualClock >= Value then
  Result := ((ActualClock - Value) * 1000) div fClockFrequency
else
  Result := (((High(Int64) - Value) + Succ(ActualClock)) * 1000) div fClockFrequency;
end;

//------------------------------------------------------------------------------

procedure TFileWatcher.ChangesReadStart;
var
  ErrorCode:  DWORD;
begin
If not fOverlappedPending then
  begin
    FillChar(fOverlapped,SizeOf(fOverlapped),0);
    fOverlapped.hEvent := fCompletionEvent; // other fields stay at zero
  {
    ReadDirectoryChangesW blocks until there are changes to be read, and since
    there is no timeout, we have to use asynchronous reading.
  }
    If not ReadDirectoryChangesW(fDirectoryHandle,fChangeBuffer,fChangeBufferSize,
      BOOL(optWatchSubtree in fWatchOptions),fWatchFilter,nil,@fOverlapped,nil) then
      begin
        ErrorCode := GetLastError;
        If ErrorCode <> ERROR_NOTIFY_ENUM_DIR then
          raise EFWSystemError.CreateFmt('TFileWatcher.ChangesReadStart: Failed to read changes (%u).',[ErrorCode])
        else
          fLostChanges := True;
      end
    else fOverlappedPending := True;
  end;
end;

//------------------------------------------------------------------------------

Function TFileWatcher.ChangesReadWait(Timeout: UInt32): Boolean;
begin
If fOverlappedPending then
  case WaitForSingleObject(fCompletionEvent,Timeout) of
    WAIT_ABANDONED: raise EFWSystemError.Create('TFileWatcher.ChangesReadWait: Unexpected wait result (abandoned).');
    WAIT_OBJECT_0:  Result := True;
    WAIT_TIMEOUT:   Result := False;
  else
   {WAIT_FAILED}
    raise EFWSystemError.CreateFmt('TFileWatcher.ChangesReadWait: Wait failed (%u).',[GetLastError]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TFileWatcher.ChangesProcess: Boolean;

  Function ActionDecode(Action: DWORD): TFWChange;
  begin
    case Action of
      FILE_ACTION_ADDED:            Result := chnAdded;
      FILE_ACTION_REMOVED:          Result := chnRemoved;
      FILE_ACTION_MODIFIED:         Result := chnChanged;
      FILE_ACTION_RENAMED_OLD_NAME: Result := chnRenamedFrom;
      FILE_ACTION_RENAMED_NEW_NAME: Result := chnRenamedTo;
    else
      Result := chnUnknown;
    end;
  end;

type
  TFWFileNotifyInfo = record
    NextEntryOffset:  DWORD;
    Action:           DWORD;
    FileNameLength:   DWORD;
    FileName:         WideChar;
  end;
  PFWFileNotifyInfo = ^TFWFileNotifyInfo;
var
  StoredBytes:  DWORD;
  ChangeInfo:   PFWFileNotifyInfo;
  TempStr:      UnicodeString;
begin
Result := False;
StoredBytes := 0;
TempStr := '';  // so the compiler is happy
fOverlappedPending := False;
ResetEvent(fCompletionEvent);
If not GetOverlappedResult(fDirectoryHandle,fOverlapped,StoredBytes,False) then
  raise EFWSystemError.CreateFmt('TFileWatcher.ChangesProcess: Failed to get overlapped result (%u).',[GetLastError]);
{
  If the read returned with 0 bytes stored, it means the buffer was too small
  and all buffered changes were discarded.
}
If StoredBytes <= 0 then
  fLostChanges := True;
ChangeInfo := PFWFileNotifyInfo(fChangeBuffer);
while StoredBytes > 0 do
  begin
    SetLength(TempStr,ChangeInfo^.FileNameLength div 2);
    Move(ChangeInfo^.FileName,PWideChar(TempStr)^,Length(TempStr) * SizeOf(WideChar));
    fInChangeHandler := True;
    try
      If not fWatchingFile or AnsiSameText(ExtractFileName(UnicodeToStr(TempStr)),fWatchedFile) then
        begin
          DoChange(ActionDecode(ChangeInfo^.Action),UnicodeToStr(TempStr));
          Result := True;
        end;
    finally
      fInChangeHandler := False;
    end;
    If ChangeInfo^.NextEntryOffset <= 0 then
      Break{while}
    else If StoredBytes >= ChangeInfo^.NextEntryOffset then
      Dec(StoredBytes,ChangeInfo^.NextEntryOffset)
    else
      StoredBytes := 0;
    Inc(PByte(ChangeInfo),ChangeInfo^.NextEntryOffset);
  end;
end;

//------------------------------------------------------------------------------
{$ELSE}

Function TFileWatcher.OnChangeHandler(Sender: TObject; Watch: cint; Mask,Cookie: UInt32; const Name: TSysString): Boolean;
var
  ReportedPath: String;
begin
ConsumeArg(Sender);
ConsumeArg(Cookie);
If Watch = fINotifyWatch then
  begin
    Result := True;
    fInChangeHandler := True;
    try
      If (Length(Name) <= 0) and (optWatchFile in fWatchOptions) then
        ReportedPath := fWatchedFile
      else
        ReportedPath := SysToStr(Name);
      // decode changes
      If (IN_CREATE and Mask) <> 0 then
        DoChange(chnAdded,ReportedPath);
      If ((IN_DELETE or IN_DELETE_SELF) and Mask) <> 0 then
        DoChange(chnRemoved,ReportedPath);
      If ((IN_MOVED_FROM or IN_MOVE_SELF) and Mask) <> 0 then
        DoChange(chnRenamedFrom,ReportedPath);
      If (IN_MOVED_TO and Mask) <> 0 then
        DoChange(chnRenamedTo,ReportedPath);
      If ((IN_ACCESS or IN_MODIFY or IN_ATTRIB) and Mask) <> 0 then
        DoChange(chnChanged,ReportedPath);
    finally
      fInChangeHandler := False;
    end;
  end
else
  begin
    // watch of -1 is passed when IN_Q_OVERFLOW is encountered
    If Watch = -1 then
      fLostChanges := True;
    Result := False;
  end;
end;

{$ENDIF}
//------------------------------------------------------------------------------

procedure TFileWatcher.Initialize({$IFNDEF Windows}INotifyInstance: TINotify; {$ENDIF}const WatchedPath: String; WatchOptions: TFWWatchOptions);
{$IFDEF Windows}
const
  FILE_LIST_DIRECTORY = $0001;

  Function AlignBuffer(var Addr: Pointer): PtrUInt;
  var
    IntAddr:  PtrUInt absolute Addr;
  begin
    Result := IntAddr and 3;
    IntAddr := (IntAddr + 3) and not PtrUInt(3);
  end;
{$ELSE}
var
  WatchMask:  UInt32;
{$ENDIF}
begin
fWatchOptions := WatchOptions;
fWatchedPath := ExpandFileName(WatchedPath);
fOnChangeEvent := nil;
fOnChangeCallback := nil;
fLostChanges := False;
{$IFDEF Windows}
If not QueryPerformanceFrequency(fClockFrequency) then
  raise EFWSystemError.CreateFmt('TFileWatcher.Initialize: Failed to query clock frequency (%u).',[GetLastError]);
fClockFrequency := fClockFrequency and (Int64(-1) shr 1);
fWatchingFile := optWatchFile in fWatchOptions;
If fWatchingFile then
  begin
    If not FileExists(fWatchedPath) then
      raise EFWInvalidValue.CreateFmt('TFileWatcher.Initialize: Wathed file (%s) does not exist.',[fWatchedPath]);
    Exclude(fWatchOptions,optWatchSubtree);
    fWatchedDir := ExtractFileDir(fWatchedPath);
    fWatchedFile := ExtractFileName(fWatchedPath);
  end
else fWatchedDir := fWatchedPath;
fWatchFilter := 0;
If optWatchName in fWatchOptions then
  begin
    If fWatchingFile then
      fWatchFilter := fWatchFilter or FILE_NOTIFY_CHANGE_FILE_NAME
    else
      fWatchFilter := fWatchFilter or FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME;
  end;
If optWatchState in fWatchOptions then
  fWatchFilter := fWatchFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES or FILE_NOTIFY_CHANGE_SIZE or FILE_NOTIFY_CHANGE_SECURITY or
    {following are file times}FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_LAST_ACCESS or FILE_NOTIFY_CHANGE_CREATION;
If fWatchFilter <= 0 then
  raise EFWInvalidValue.Create('TFileWatcher.Initialize: Empty watch filter not allowed.');
fCompletionEvent := CreateEvent(nil,True,False,nil);
If fCompletionEvent = 0 then
  raise EFWSystemError.CreateFmt('TFileWatcher.Initialize: Unable to create completion event (%u).',[GetLastError]);
fDirectoryHandle := CreateFile(PChar(StrToSys(fWatchedDir)),GENERIC_READ or FILE_LIST_DIRECTORY, FILE_SHARE_READ or
  FILE_SHARE_WRITE or FILE_SHARE_DELETE,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,0);
If fDirectoryHandle = INVALID_HANDLE_VALUE then
  raise EFWSystemError.CreateFmt('TFileWatcher.Initialize: Unable to open directory "%s" (%u).',[fWatchedDir,GetLastError]);
{
  Buffer for ReadDirectoryChangesW must be aligned on DWORD boundary, but we
  cannot rely on memory manager that it will be - need to ensure it.
}
fChangeBufferBase := AllocMem(FW_CHANGEBUFFER_SIZE);
fChangeBufferSize := FW_CHANGEBUFFER_SIZE;
fChangeBuffer := fChangeBufferBase;
fChangeBufferSize := fChangeBufferSize - AlignBuffer(fChangeBuffer);
FillChar(fOverlapped,SizeOf(fOverlapped),0);
fOverlappedPending := False;
// establish file-system watch
ChangesReadStart;
{$ELSE}
Exclude(fWatchOptions,optWatchSubtree); // does not work in Linux
fINotifyWatch := -1;  // do not touch
If optWatchFile in fWatchOptions then
  fWatchedFile := ExtractFileName(fWatchedPath)
else
  fWatchedFile := '';
If Assigned(INotifyInstance) then
  begin
    INotifyInstance.Acquire;
    fINotifyInstance := INotifyInstance;
  end
else
  begin
    fINotifyInstance := TINotify.Create;
    fINotifyInstance.Acquire;
  end;
fINotifyInstance.OnChange.Add(OnChangeHandler);
If optWatchFile in fWatchOptions then
  WatchMask := 0
else
  WatchMask := IN_ONLYDIR;
If optWatchName in fWatchOptions then
  begin
    WatchMask := WatchMask or IN_CREATE or IN_DELETE or IN_MOVED_FROM or IN_MOVED_TO;
    If optWatchFile in fWatchOptions then
      WatchMask := WatchMask or IN_DELETE_SELF or IN_MOVE_SELF;
  end;
If optWatchState in fWatchOptions then
  WatchMask := WatchMask or IN_ATTRIB or IN_ACCESS or IN_MODIFY;
If WatchMask <= 0 then
  raise EFWInvalidValue.Create('TFileWatcher.Initialize: Empty watch mask not allowed.');
fINotifyWatch := inotify_add_watch(fINotifyInstance.Instance,PSysChar(StrToSys(WatchedPath)),WatchMask);
If fINotifyWatch = -1 then
  raise EFWSystemError.CreateFmt('TFileWatcher.Initialize: Failed to add inotify watch (%d).',[errno_ptr^]);
fReceivedChanges := 0;
{$ENDIF}
fInChangeHandler := False;
end;

//------------------------------------------------------------------------------

procedure TFileWatcher.Finalize;
begin
{$IFDEF Windows}
FreeMem(fChangeBuffer,FW_CHANGEBUFFER_SIZE);
If fOverlappedPending then
  CancelIo(fDirectoryHandle);
CloseHandle(fDirectoryHandle);
CloseHandle(fCompletionEvent);
{$ELSE}
If fINotifyWatch <> -1 then
  If inotify_rm_watch(fINotifyInstance.Instance,fINotifyWatch) = -1 then
    raise EFWSystemError.CreateFmt('TFileWatcher.Finalize: Failed to remove inotify watch (%d).',[errno_ptr^]);
fINotifyInstance.OnChange.Remove(OnChangeHandler);
fINotifyInstance.Release;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFileWatcher.DoChange(Change: TFWChange; const Path: String);
begin
{$IFNDEF Windows}
Inc(fReceivedChanges);
{$ENDIF}
If Assigned(fOnChangeEvent) then
  fOnChangeEvent(Self,Change,Path)
else If Assigned(fOnChangeCallback) then
  fOnChangeCallback(Self,Change,Path);
end;

{-------------------------------------------------------------------------------
    TFileWatcher - public methods
-------------------------------------------------------------------------------}
{$IFNDEF Windows}
constructor TFileWatcher.Create(INotifyInstance: TINotify; const WatchedPath: String; WatchOptions: TFWWatchOptions); overload;
begin
inherited Create;
Initialize(INotifyInstance,WatchedPath,WatchOptions);
end;

//------------------------------------------------------------------------------

constructor TFileWatcher.WatchDirectory(INotifyInstance: TINotify; const DirectoryPath: String; WatchOptions: TFWWatchOptions); overload;
begin
Create(INotifyInstance,DirectoryPath,WatchOptions - [optWatchFile]);
end;

//------------------------------------------------------------------------------

constructor TFileWatcher.WatchFile(INotifyInstance: TINotify; const FilePath: String; WatchOptions: TFWWatchOptions); overload;
begin
Create(INotifyInstance,FilePath,WatchOptions + [optWatchFile]);
end;

//------------------------------------------------------------------------------
{$ENDIF}

constructor TFileWatcher.Create(const WatchedPath: String; WatchOptions: TFWWatchOptions);
begin
inherited Create;
Initialize({$IFNDEF Windows}nil,{$ENDIF}WatchedPath,WatchOptions);
end;

//------------------------------------------------------------------------------

constructor TFileWatcher.WatchDirectory(const DirectoryPath: String; WatchOptions: TFWWatchOptions{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Create(DirectoryPath,WatchOptions - [optWatchFile]);
end;

//------------------------------------------------------------------------------

constructor TFileWatcher.WatchFile(const FilePath: String; WatchOptions: TFWWatchOptions{$IFNDEF FPC}; Dummy: LongWord = 0{$ENDIF});
begin
Create(FilePath,WatchOptions + [optWatchFile]);
end;

//------------------------------------------------------------------------------

destructor TFileWatcher.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TFileWatcher.WaitForChanges(Timeout: UInt32): TFWChangeWaitResult;
var
  ActualTimeout:  UInt32;
  ContinueWait:   Boolean;
  WaitingStart:   {$IFDEF Windows}Int64{$ELSE}TTimeSpec{$ENDIF};
  ElapsedMillis:  {$IFDEF Windows}Int64{$ELSE}UInt32{$ENDIF};
begin
If not fInChangeHandler then
  begin
  {$IFDEF Windows}
    Result := wrTimeout;
    // deal with possible pending read
    while ChangesReadWait(0) do
      begin
        If ChangesProcess then
          Result := wrChange;
        ChangesReadStart;
      end;
    If Result = wrChange then
      Exit;
    // no unprocessed changes, do full read-check-process cycle
    ActualTimeout := Timeout;
    WaitingStart := ClockGetActual;
    repeat
      ContinueWait := False;
      ChangesReadStart;
    {
      Overlapped pending is always true after ChangesReadStart, so
      ChangesReadWait can return false only for timeout.
    }
      If ChangesReadWait(ActualTimeout) then
        begin
          If not ChangesProcess then
            begin
              // recalculate timeout and re-enter waiting
              ContinueWait := True;
              If Timeout <> INFINITE then
                begin
                  ElapsedMillis := ClockMillisFrom(WaitingStart);
                  If ElapsedMillis < Int64(Timeout) then
                    ActualTimeout := Timeout - ElapsedMillis
                  else
                    Break{repeat}; // result is already set to wrTimeout
                end;
            end
          else Result := wrChange;
        end;
    until not ContinueWait;
  {$ELSE}
    Result := wrTimeout;
    ActualTimeout := Timeout;
    fINotifyInstance.TimeGetActual(WaitingStart);
    repeat
      ContinueWait := False;
      If fINotifyInstance.WaitForChanges(ActualTimeout) then
        begin
          fReceivedChanges := 0;
          fINotifyInstance.ReadAndDispatchChanges;
          If fReceivedChanges <= 0 then
            begin
            {
              No change was received, meaning all changes were dispatched to
              another watchers - recalculate timeout and re-enter waiting.
            }
              ContinueWait := True;
              ElapsedMillis := fINotifyInstance.TimeGetElapsedMillis(WaitingStart);
              If ElapsedMillis < Timeout then
                ActualTimeout := Timeout - ElapsedMillis
              else
                Break{repeat}; // result is already set to wrTimeout
            end
          else Result := wrChange;
        end
      else Result := wrTimeout;
    until not ContinueWait;
  {$ENDIF}
  end
else Result := wrError;
end;

//------------------------------------------------------------------------------

Function TFileWatcher.GetChanges: Boolean;
begin
Result := False;
If not fInChangeHandler then
  begin
  {$IFDEF Windows}
    while ChangesReadWait(0) do
      begin
        If ChangesProcess then
          Result := True;
        ChangesReadStart;
      end;
  {$ELSE}
    If fINotifyInstance.WaitForChanges(0) then
      begin
        fReceivedChanges := 0;
        fINotifyInstance.ReadAndDispatchChanges;
        Result := fReceivedChanges > 0;
      end;
  {$ENDIF}
  end;
end;

end.
