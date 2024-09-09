{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple pipes

    This library provides wrappers for pipes available in different operating
    systems (currently only Windows and Linux systems are supported). To keep
    things simple, all pipes provided here are unidirectional (no duplex pipes)
    and blocking.

    To properly use the pipes, first create a pipe server using a Create*
    constructor and then create a pipe client using Connect* constructor (note
    that the ends can be in different threads and even processes). The ends
    must be in opposite access mode (read-write), so if you create write
    server, the client must be in read mode, and vice-versa. Failure to do so
    will result in undefined behaviour and unexpected results.

    Three types of pipes are currently implemented - anonymous pipe (TPipe or
    TAnonymousPipe), shared pipe (TSharedPipe) and named pipe (TNamedPipe).

      Anonymous pipes do no have name. To connect to an anonymous server, you
      have to provide binary connection data to the Connect* constructor. These
      data are exposed on the server via ConnectionData property. How to pass
      them to the client thread or process is up to you (global memory, shared
      memory, messages, ...).

      Shared pipes are based on anonymous pipes, but they have a global name
      (case-insensitive). When you create a server, it stores connection data
      to a shared memory under its own name. When you want to connect to it,
      pass the same name to client constructor and it will get the connection
      data automatically from this shared memory - so you don't have to manage
      any IPC or other means of data sharing.

      Named pipes works similarly to shared pipes (on the outside, not
      internally), but they are managed by the operating system. Simply pass
      the same name to both server and client and you are golden.

        NOTE - names of named pipes are case-insensitive on all systems.

        WARNING - named pipes will not exit their constructors (both server and
                  client) - ie. will block - until the other end is connected.
                  This can lead to a deadlocks (especially if you try to create
                  both ends in the same thread), so be careful.

  Version 1.1.2 (2024-05-03)

  Last change 2024-09-09

  ©2022-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimplePipes

  Dependencies:
    AuxClasses       - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions    - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes         - github.com/TheLazyTomcat/Lib.AuxTypes
    InterlockedOps   - github.com/TheLazyTomcat/Lib.InterlockedOps
    NamedSharedItems - github.com/TheLazyTomcat/Lib.NamedSharedItems
  * StrRect          - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimplePipes_UseAuxExceptions for details).

  Library StrRect is required only when compiling for Windows OS.

  Libraries AuxExceptions and StrRect might also be required as an indirect
  dependencies.

  Indirect dependencies:
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    SHA1               - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SimplePipes;
{
  SimplePipes_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimplePipes_UseAuxExceptions to achieve this.
}
{$IF Defined(SimplePipes_UseAuxExceptions)}
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
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,{$IFNDEF Windows} baseunix,{$ENDIF}
  AuxTypes, AuxClasses, NamedSharedItems
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESPException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESPSystemError    = class(ESPException);
  ESPNoServer       = class(ESPException);
  ESPHandleDupError = class(ESPException);

  ESPInvalidMode = class(ESPException);
  ESPPeekError   = class(ESPException);
  ESPReadError   = class(ESPException);
  ESPWriteError  = class(ESPException);

{===============================================================================
--------------------------------------------------------------------------------
                                    TPipeBase
--------------------------------------------------------------------------------
===============================================================================}
type
  TSPEndpointMode = (emRead,emWrite);

  TSPEndpointHandle = {$IFDEF Windows}THandle{$ELSE}cInt{$ENDIF};

{===============================================================================
    TPipeBase - class declaration
===============================================================================}
type
  TPipeBase = class(TCustomObject)
  protected
    fISServer:      Boolean;
    fEndpointMode:  TSPEndpointMode;
    fReadHandle:    TSPEndpointHandle;
    fWriteHandle:   TSPEndpointHandle;
    procedure CreatePipe; virtual; abstract;
    procedure DestroyPipe; virtual; abstract;
    procedure ConnectPipe; virtual; abstract;
    procedure DisconnectPipe; virtual; abstract;
    procedure Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode); virtual;
    procedure Finalize; virtual;
  public
    destructor Destroy; override;
    Function PeekBytes: TMemSize; virtual;  // returns number of unread bytes in the pipe
    Function Read(out Buffer; Count: TMemSize): TMemSize; virtual;
    Function Write(const Buffer; Count: TMemSize): TMemSize; virtual;
    property IsServer: Boolean read fIsServer;
    property EndpointMode: TSPEndpointMode read fEndpointMode;
    property ReadHandle: TSPEndpointHandle read fReadHandle;
    property WriteHandle: TSPEndpointHandle read fWriteHandle;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSimplePipe
--------------------------------------------------------------------------------
===============================================================================}
type
  TSPConnectionData = packed record
    ValidData:      UInt32; // 0 = invalid, !0 = valid
    CreatorPID:     UInt32;
    EndpointHandle: Int64;
  end;
  PSPConnectionData = ^TSPConnectionData;

{===============================================================================
    TSimplePipe - class declaration
===============================================================================}
type
  TSimplePipe = class(TPipeBase)
  protected
    fConnectionDataPtr: PSPConnectionData;
    procedure CreatePipe; override;
    procedure DestroyPipe; override;
    procedure ConnectPipe; override;
    procedure DisconnectPipe; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TAnonymousPipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TAnonymousPipe - class declaration
===============================================================================}
type
  TAnonymousPipe = class(TSimplePipe)
  protected
    fConnectionData:  TSPConnectionData;
    procedure Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode); override;
  public
    constructor CreateReadEnd;
    constructor CreateWriteEnd{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF};
    constructor ConnectReadEnd(ConnectionData: TSPConnectionData);
    constructor ConnectWriteEnd(ConnectionData: TSPConnectionData{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    property ConnectionData: TSPConnectionData read fConnectionData;
  end;

  TPipe = class(TAnonymousPipe);

{===============================================================================
--------------------------------------------------------------------------------
                                   TSharedPipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSharedPipe - class declaration
===============================================================================}
type
  TSharedPipe = class(TSimplePipe)
  protected
    fName:                String;
    fConnectionDataItem:  TNamedSharedItem;
    procedure Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode); override;
    procedure Finalize; override;
  public
    constructor CreateReadEnd(const Name: String);
    constructor CreateWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor ConnectReadEnd(const Name: String{$IFNDEF FPC}; Dummy: Single = 0.0{$ENDIF});
    constructor ConnectWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: String = ''{$ENDIF});
    property Name: String read fName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TNamedPipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TNamedPipe - class declaration
===============================================================================}
type
  TNamedPipe = class(TPipeBase)
  protected
    fName:  String;
    procedure CreatePipe; override;
    procedure DestroyPipe; override;
    procedure ConnectPipe; override;
    procedure DisconnectPipe; override;
    procedure InitializeName(IsServer: Boolean; EndpointMode: TSPEndpointMode; const Name: String); virtual;
  public
    constructor CreateReadEnd(const Name: String);
    constructor CreateWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor ConnectReadEnd(const Name: String{$IFNDEF FPC}; Dummy: Single = 0.0{$ENDIF});
    constructor ConnectWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: String = ''{$ENDIF});
    property Name: String read fName;
  end;

implementation

uses
{$IFDEF Windows}
  Windows, StrUtils,
  StrRect,
{$ENDIF}
  InterlockedOps;

{===============================================================================
    Externals
===============================================================================}
{$IFNDEF Windows}
const
  O_CLOEXEC = $80000;
  FIONREAD  = $541B;

type
  TFDPair = array[0..1] of cInt;
  PFDPair = ^TFDPair;

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function getpid: pid_t; cdecl; external;

Function pipe2(pipefd: PFDPair; flags: cInt): cInt; cdecl; external;

Function mkfifo(pathname: PChar; mode: mode_t): cInt; cdecl; external;
Function unlink(pathname: PChar): cInt; cdecl; external;

Function open(pathname: PChar; flags: cInt; mode: mode_t): cInt; cdecl; external;
Function close(fd: cInt): cInt; cdecl; external;

Function sys_read(fd: cInt; buf: Pointer; count: size_t): ssize_t; cdecl; external name 'read';
Function sys_write(fd: cInt; buf: Pointer; count: size_t): ssize_t; cdecl; external name 'write';

Function ioctl(fd: cInt; request: cULong): cInt; cdecl; external; varargs;

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                    TPipeBase
--------------------------------------------------------------------------------
===============================================================================}
type
  TSPParamInt = {$IFDEF Windows}DWORD{$ELSE}cInt{$ENDIF};

{===============================================================================
    TPipeBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TPipeBase - protected methods
-------------------------------------------------------------------------------}

procedure TPipeBase.Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode);
begin
fIsServer := IsServer;
fEndpointMode := EndpointMode;
fReadHandle := {$IFDEF Windows}INVALID_HANDLE_VALUE{$ELSE}-1{$ENDIF};
fWriteHandle := {$IFDEF Windows}INVALID_HANDLE_VALUE{$ELSE}-1{$ENDIF};
If fISServer then
  CreatePipe
else
  ConnectPipe;
end;

//------------------------------------------------------------------------------

procedure TPipeBase.Finalize;
begin
If fISServer then
  DestroyPipe
else
  DisconnectPipe;
end;

{-------------------------------------------------------------------------------
    TPipeBase - public methods
-------------------------------------------------------------------------------}

destructor TPipeBase.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TPipeBase.PeekBytes: TMemSize;
var
  BytesInPipe:  TSPParamInt;
begin
If fEndpointMode = emRead then
  begin
  {$IFDEF Windows}
    If PeekNamedPipe(fReadHandle,nil,0,nil,@BytesInPipe,nil) then
      Result := TMemSize(BytesInPipe)
    else
      raise ESPPeekError.CreateFmt('TPipeBase.PeekBytes: Failed to peed pipe (%d).',[GetLastError]);
  {$ELSE}
    If ioctl(fReadHandle,FIONREAD,@BytesInPipe) = 0 then
      Result := TMemSize(BytesInPipe)
    else
      raise ESPPeekError.CreateFmt('TPipeBase.PeekBytes: Failed to peed pipe (%d).',[errno_ptr^]);
  {$ENDIF}
  end
else raise ESPInvalidMode.Create('TPipeBase.PeekBytes: Cannot peek from write end.');
end;

//------------------------------------------------------------------------------

Function TPipeBase.Read(out Buffer; Count: TMemSize): TMemSize;
var
  BytesRead:  TSPParamInt;
begin
If fEndpointMode = emRead then
  begin
    BytesRead := 0;
  {$IFDEF Windows}
    If Windows.ReadFile(fReadHandle,Addr(Buffer)^,DWORD(Count),BytesRead,nil) then
      Result := TMemSize(BytesRead)
    else
      raise ESPReadError.CreateFmt('TPipeBase.Read: Read failed (%d).',[GetLastError]);
  {$ELSE}
    BytesRead := sys_read(fReadHandle,@Buffer,size_t(Count));
    If BytesRead >= 0 then
      Result := TMemSize(BytesRead)
    else
      raise ESPReadError.CreateFmt('TPipeBase.Read: Read failed (%d).',[errno_ptr^]);
  {$ENDIF}
  end
else raise ESPInvalidMode.Create('TPipeBase.Read: Cannot read from write end.');
end;

//------------------------------------------------------------------------------

Function TPipeBase.Write(const Buffer; Count: TMemSize): TMemSize;
var
  BytesWritten: TSPParamInt;
begin
If fEndpointMode = emWrite then
  begin
    BytesWritten := 0;
  {$IFDEF Windows}
    If Windows.WriteFile(fWriteHandle,Buffer,DWORD(Count),BytesWritten,nil) then
      Result := TMemSize(BytesWritten)
    else
      raise ESPWriteError.CreateFmt('TPipeBase.Write: Write failed (%d).',[GetLastError]);
  {$ELSE}
    BytesWritten := sys_write(fWriteHandle,@Buffer,size_t(Count));
    If BytesWritten >= 0 then
      Result := TMemSize(BytesWritten)
    else
      raise ESPWriteError.CreateFmt('TPipeBase.Write: Write failed (%d).',[errno_ptr^]);
  {$ENDIF}
  end
else raise ESPInvalidMode.Create('TPipeBase.Write: Cannot write to read end.');
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSimplePipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimplePipe - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimplePipe - protected methods
-------------------------------------------------------------------------------}

procedure TSimplePipe.CreatePipe;
{$IFDEF Windows}
var
  SecAttr:  TSecurityAttributes;
begin
SecAttr.nLength := SizeOf(TSecurityAttributes);
SecAttr.lpSecurityDescriptor := nil;
SecAttr.bInheritHandle := True;
If not Windows.CreatePipe(fReadHandle,fWriteHandle,@SecAttr,0) then
  raise ESPSystemError.CreateFmt('TSimplePipe.CreatePipe: Failed to create pipe (%d).',[GetLastError]);
fConnectionDataPtr^.CreatorPID := UInt32(GetCurrentProcessID);
{$ELSE}
var
  Handles:  TFDPair;
begin
If pipe2(@Handles,O_CLOEXEC) <> 0 then
  raise ESPSystemError.CreateFmt('TSimplePipe.CreatePipe: Failed to create pipe (%d).',[errno_ptr^]);
fReadHandle := Handles[0];
fWriteHandle := Handles[1];
fConnectionDataPtr^.CreatorPID := UInt32(getpid);
{$ENDIF}
// note the reverse - when in read mode, store write handle, and vice-versa
case fEndpointMode of
  emRead:   fConnectionDataPtr^.EndpointHandle := Int64(PtrInt(fWriteHandle));
  emWrite:  fConnectionDataPtr^.EndpointHandle := Int64(PtrInt(fReadHandle));
else
  raise ESPInvalidMode.CreateFmt('TSimplePipe.CreatePipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
InterlockedStore(fConnectionDataPtr^.ValidData,1);
end;

//------------------------------------------------------------------------------

procedure TSimplePipe.DestroyPipe;
begin
InterlockedStore(fConnectionDataPtr^.ValidData,0);
FillChar(fConnectionDataPtr^,SizeOf(TSPConnectionData),0);
{$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fReadHandle);
{$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fWriteHandle);
end;

//------------------------------------------------------------------------------

procedure TSimplePipe.ConnectPipe;

  Function ConnectPipeInternal({$IFNDEF Windows}Param: TSPParamInt{$ENDIF}): TSPEndpointHandle;
  {$IFDEF Windows}
  var
    SourceProcess:  THandle;
  begin
    SourceProcess := OpenProcess(PROCESS_DUP_HANDLE,False,fConnectionDataPtr^.CreatorPID);
    If SourceProcess <> 0 then
      try
        If not Windows.DuplicateHandle(SourceProcess,THandle(PtrInt(fConnectionDataPtr^.EndpointHandle)),GetCurrentProcess,@Result,0,True,DUPLICATE_SAME_ACCESS) then
          raise ESPHandleDupError.CreateFmt('TSimplePipe.ConnectPipe.ConnectPipeInternal: Failed to duplicate handle (%d).',[GetLastError]);
      finally
        CloseHandle(SourceProcess);
      end
    else raise ESPHandleDupError.CreateFmt('TSimplePipe.ConnectPipe.ConnectPipeInternal: Failed to open source process (%d).',[GetLastError]);

  {$ELSE}
  begin
  {
    Open symlink file "/proc/[pid]/fd/[file_descriptor]" - it links to the
    anonymous pipe end given in the connection data.
  }
    Result := open(PChar(Format('/proc/%d/fd/%d',[fConnectionDataPtr^.CreatorPID,fConnectionDataPtr^.EndpointHandle])),O_CLOEXEC or Param,0);
    If Result < 0 then
      raise ESPSystemError.CreateFmt('TSimplePipe.ConnectPipe.ConnectPipeInternal: Failed to connect named pipe (%d).',[errno_ptr^]);
  {$ENDIF}
  end;

begin
If InterlockedLoad(fConnectionDataPtr^.ValidData) <> 0 then
  begin
    case fEndpointMode of
      emRead:   fReadHandle := ConnectPipeInternal({$IFNDEF Windows}O_RDONLY{$ENDIF});
      emWrite:  fWriteHandle := ConnectPipeInternal({$IFNDEF Windows}O_WRONLY{$ENDIF});
    else
      raise ESPInvalidMode.CreateFmt('TSimplePipe.ConnectPipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
    end;
  end
else raise ESPNoServer.Create('TSimplePipe.ConnectPipe: No server to connect to.');
end;

//------------------------------------------------------------------------------

procedure TSimplePipe.DisconnectPipe;
begin
case fEndpointMode of
  emRead:   {$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fReadHandle);
  emWrite:  {$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fWriteHandle);
else
  raise ESPInvalidMode.CreateFmt('TSimplePipe.DisconnectPipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TAnonymousPipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TAnonymousPipe - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TAnonymousPipe - protected methods
-------------------------------------------------------------------------------}

procedure TAnonymousPipe.Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode);
begin
fConnectionDataPtr := @fConnectionData;
inherited Initialize(IsServer,EndpointMode);
end;

{-------------------------------------------------------------------------------
    TAnonymousPipe - public methods
-------------------------------------------------------------------------------}

constructor TAnonymousPipe.CreateReadEnd;
begin
inherited Create;
Initialize(True,emRead);
end;

//------------------------------------------------------------------------------

constructor TAnonymousPipe.CreateWriteEnd{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF};
begin
inherited Create;
Initialize(True,emWrite);
end;

//------------------------------------------------------------------------------

constructor TAnonymousPipe.ConnectReadEnd(ConnectionData: TSPConnectionData);
begin
inherited Create;
fConnectionData := ConnectionData;
Initialize(False,emRead);
end;

//------------------------------------------------------------------------------

constructor TAnonymousPipe.ConnectWriteEnd(ConnectionData: TSPConnectionData{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fConnectionData := ConnectionData;
Initialize(False,emWrite);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSharedPipe
--------------------------------------------------------------------------------
===============================================================================}
const
  SP_SHARED_NAMESPACE = 'shared_pipes';

{===============================================================================
    TSharedPipe - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSharedPipe - protected methods
-------------------------------------------------------------------------------}

procedure TSharedPipe.Initialize(IsServer: Boolean; EndpointMode: TSPEndpointMode);
begin
fConnectionDataItem := TNamedSharedItem.Create(fName,SizeOf(TSPConnectionData),SP_SHARED_NAMESPACE);
fConnectionDataPtr := PSPConnectionData(fConnectionDataItem.Memory);
inherited Initialize(IsServer,EndpointMode);
end;

//------------------------------------------------------------------------------

procedure TSharedPipe.Finalize;
begin
inherited;
fConnectionDataItem.Free;
end;

{-------------------------------------------------------------------------------
    TSharedPipe - public methods
-------------------------------------------------------------------------------}

constructor TSharedPipe.CreateReadEnd(const Name: String);
begin
inherited Create;
fName := Name;
Initialize(True,emRead);
end;

//------------------------------------------------------------------------------

constructor TSharedPipe.CreateWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fName := Name;
Initialize(True,emWrite);
end;

//------------------------------------------------------------------------------

constructor TSharedPipe.ConnectReadEnd(const Name: String{$IFNDEF FPC}; Dummy: Single = 0.0{$ENDIF});
begin
inherited Create;
fName := Name;
Initialize(False,emRead);
end;

//------------------------------------------------------------------------------

constructor TSharedPipe.ConnectWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: String = ''{$ENDIF});
begin
inherited Create;
fName := Name;
Initialize(False,emWrite);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TNamedPipe
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TNamedPipe - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TNamedPipe - protected methods
-------------------------------------------------------------------------------}

procedure TNamedPipe.CreatePipe;

  procedure CreatePipeInternal(Param: TSPParamInt; out PipeHandle: TSPEndpointHandle);
  {$IFDEF Windows}
  var
    ErrCode:  DWORD;
  begin
    PipeHandle := CreateNamedPipeW(PWideChar(StrToWinW(fName)),Param,PIPE_TYPE_BYTE,1,0,0,INFINITE,nil);
    If PipeHandle <> INVALID_HANDLE_VALUE then
      begin
        If not ConnectNamedPipe(PipeHandle,nil) then
          begin
            ErrCode := GetLastError;
            If ErrCode <> ERROR_PIPE_CONNECTED then
              raise ESPSystemError.CreateFmt('TNamedPipe.CreatePipe.CreatePipeInternal: Failed to connect named pipe (%d).',[ErrCode]);
          end;
      end
    else raise ESPSystemError.CreateFmt('TNamedPipe.CreatePipe.CreatePipeInternal: Failed to create named pipe (%d).',[GetLastError]);
  {$ELSE}
  begin
    If mkfifo(PChar(fName),S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH) <> 0 then
      raise ESPSystemError.CreateFmt('TNamedPipe.CreatePipe.CreatePipeInternal: Failed to create named pipe (%d).',[errno_ptr^]);
    PipeHandle := open(PChar(fName),O_CLOEXEC or Param,0);
    If PipeHandle < 0 then
      raise ESPSystemError.CreateFmt('TNamedPipe.CreatePipe.CreatePipeInternal: Failed to connect named pipe (%d).',[errno_ptr^]);
  {$ENDIF}
  end;

begin
case fEndpointMode of
  emRead:   CreatePipeInternal({$IFDEF Windows}PIPE_ACCESS_INBOUND{$ELSE}O_RDONLY{$ENDIF},fReadHandle);
  emWrite:  CreatePipeInternal({$IFDEF Windows}PIPE_ACCESS_OUTBOUND{$ELSE}O_WRONLY{$ENDIF},fWriteHandle);
else
  raise ESPInvalidMode.CreateFmt('TNamedPipe.CreatePipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TNamedPipe.DestroyPipe;

  procedure DestroyPipeInternal(PipeHandle: TSPEndpointHandle);
  begin
  {$IFDEF Windows}
    FlushFileBuffers(PipeHandle);
    DisconnectNamedPipe(PipeHandle);
    CloseHandle(PipeHandle);
  {$ELSE}
    close(PipeHandle);
    unlink(PChar(fName));
  {$ENDIF}
  end;

begin
case fEndpointMode of
  emRead:   DestroyPipeInternal(fReadHandle);
  emWrite:  DestroyPipeInternal(fWriteHandle);
else
  raise ESPInvalidMode.CreateFmt('TNamedPipe.DestroyPipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TNamedPipe.ConnectPipe;

  procedure ConnectPipeInternal(Param: TSPParamInt; out PipeHandle: TSPEndpointHandle);
  begin
  {$IFDEF Windows}
    PipeHandle := CreateFileW(PWideChar(StrToWinW(fName)),Param,0,nil,OPEN_EXISTING,0,0);
    If PipeHandle = INVALID_HANDLE_VALUE then
      raise ESPSystemError.CreateFmt('TNamedPipe.ConnectPipe.ConnectPipeInternal: Failed to connect named pipe (%d).',[GetLastError]);
  {$ELSE}
    PipeHandle := open(PChar(fName),O_CLOEXEC or Param,0);
    If PipeHandle < 0 then
      raise ESPSystemError.CreateFmt('TNamedPipe.ConnectPipe.ConnectPipeInternal: Failed to connect named pipe (%d).',[errno_ptr^]);
  {$ENDIF}
  end;

begin
case fEndpointMode of
  emRead:   ConnectPipeInternal({$IFDEF Windows}GENERIC_READ{$ELSE}O_RDONLY{$ENDIF},fReadHandle);
  emWrite:  ConnectPipeInternal({$IFDEF Windows}GENERIC_WRITE{$ELSE}O_WRONLY{$ENDIF},fWriteHandle);
else
  raise ESPInvalidMode.CreateFmt('TNamedPipe.ConnectPipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TNamedPipe.DisconnectPipe;
begin
case fEndpointMode of
  emRead:   {$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fReadHandle);
  emWrite:  {$IFDEF Windows}CloseHandle{$ELSE}close{$ENDIF}(fWriteHandle);
else
  raise ESPInvalidMode.CreateFmt('TNamedPipe.DisconnectPipe: Invalid endpoint mode (%d).',[Ord(fEndpointMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TNamedPipe.InitializeName(IsServer: Boolean; EndpointMode: TSPEndpointMode; const Name: String);
var
  i:  TStrOff;
begin
// assign and rectify name
fName := AnsiLowerCase(Name);
{$IFDEF Windows}
// replace all backslashes (not allowed) with underscores
For i := 1 to Length(fName) do
  If fName[i] = '\' then
    fName[i] := '_';
// prepend required string
If not AnsiStartsText('\\.\pipe\',fName) then
  fName := '\\.\pipe\' + fName;
// limit length
If Length(fName) > 256 then
  SetLength(fName,256);
{$ELSE}
// replace all path delimiters with underscores
For i := 1 to Length(fName) do
  If fName[i] = '/' then
    fName[i] := '_';
fName := '/tmp/fifo_' + fName;
{$ENDIF}
Initialize(IsServer,EndpointMode);
end;

{-------------------------------------------------------------------------------
    TNamedPipe - public methods
-------------------------------------------------------------------------------}


constructor TNamedPipe.CreateReadEnd(const Name: String);
begin
inherited Create;
InitializeName(True,emRead,Name);
end;

//------------------------------------------------------------------------------

constructor TNamedPipe.CreateWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
InitializeName(True,emWrite,Name);
end;

//------------------------------------------------------------------------------

constructor TNamedPipe.ConnectReadEnd(const Name: String{$IFNDEF FPC}; Dummy: Single = 0.0{$ENDIF});
begin
inherited Create;
InitializeName(False,emRead,Name);
end;

//------------------------------------------------------------------------------

constructor TNamedPipe.ConnectWriteEnd(const Name: String{$IFNDEF FPC}; Dummy: String = ''{$ENDIF});
begin
inherited Create;
InitializeName(False,emWrite,Name);
end;

end.
