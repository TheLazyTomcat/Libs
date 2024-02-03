{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Concurrent tasks

    Small library providing means for running and controlling multiple separate
    tasks run in threads (each task in its own thread).

    To use this unit, create a descendant of class TCNTSTask and put the
    main processing code into method Main (override it). Then pass instance of
    this class to an instance of TCNTSManager. Manager will automatically start
    the task when running resources get available, or you can start the task
    manually (this can pause other task if there are no running slots
    available).
    You can call any public method of TCNTSTask from Main, but remember that
    the code is running in its own thread, so protect any shared data you want
    to access, tasks don't have any mean of thread safety and data integrity
    protection.

    In Main, you should call method Cycle regularly, this ensures the task
    responds to requests from the manager and also enables the intergrated
    messaging system (which is also used for progress signalling). If you want
    to react to incoming messages, override task's protected method
    ProcessMessage.
    Remember that the messaging system is strictly server-client in nature.
    Meaning any message sent from a task will arrive to the manager. To enable
    message processing and progress reporting in the manager, make sure to call
    method Update regularly (this method receives and dispatches messages sent
    from all individual tasks).

    The implementation of method Main is entirely up to you, but suggested
    template would be as follows:

        Function TTestTask.Main: Boolean;
        begin
          while not Terminated do
            begin
              Cycle;
              [user code]
              [progress signalling]
            end;
          Result := not Terminated;
        end;

  Version 1.2 (2022-09-30)

  Last change 2024-02-03

  ©2017-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.ConcurrentTasks

  Dependencies:
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    BitOps              - github.com/TheLazyTomcat/Lib.BitOps
  * BitVector           - github.com/TheLazyTomcat/Lib.BitVector
    CrossSyncObjs       - github.com/TheLazyTomcat/Lib.CrossSyncObjs
    HashBase            - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps      - github.com/TheLazyTomcat/Lib.InterlockedOps
  * LinSyncObjs         - github.com/TheLazyTomcat/Lib.LinSyncObjs
    ListSorters         - github.com/TheLazyTomcat/Lib.ListSorters
    MemVector           - github.com/TheLazyTomcat/Lib.MemVector
    Messanger           - github.com/TheLazyTomcat/Lib.Messanger
    NamedSharedItems    - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1                - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream  - github.com/TheLazyTomcat/Lib.SharedMemoryStream
  * SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * SimpleFutex         - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream  - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
  * WinSyncObjs         - github.com/TheLazyTomcat/Lib.WinSyncObjs

  Libraries UInt64Utils and WinSyncObjs are required only when compiling for
  Windows OS.

  Libraries BitVector, LinSyncObjs and SimpleFutex are required only when
  compiling for Linux OS.

  Library SimpleCPUID might not be required when compiling for Windows OS,
  depending on defined symbols in InterlockedOps and BitOps libraries.

  BinaryStreamingLite can be replaced by full BinaryStreaming.

===============================================================================}
unit ConcurrentTasks;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses, Messanger, CrossSyncObjs;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ECNSTException = class(Exception);

  ECNSTIndexOutOfBounds = class(ECNSTException);
  ECNTSInvalidValue     = class(ECNSTException);
  ECNTSTaskRunning      = class(ECNSTException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TCNTSTask
--------------------------------------------------------------------------------
===============================================================================}
type
  TCNTSTaskID = Integer;

  // messaging types
  TCNTSMessageParam  = TMsgrParam;
  TCNTSMessageResult = TMsgrParam;

  TCNTSMessage = record
    Param1: TCNTSMessageParam;
    Param2: TCNTSMessageParam;
    Result: TCNTSMessageResult;
  end;

{===============================================================================
    TCNTSTask - class declaration
===============================================================================}
type
  TCNTSTask = class(TCustomObject)
  protected
    fTaskID:        TCNTSTaskID;
    fCommEndpoint:  TMessangerEndpoint;
    fPauseEvent:    TEvent;
    fTerminated:    Integer;
    // getters, setters
    Function GetTerminated: Boolean;
    procedure SetTerminated(Value: Boolean);
    // internal methods (not to be called from user code)
    procedure InternalMessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags); virtual;
    procedure InternalSetup(TaskID: TCNTSTaskID; CommEndpoint: TMessangerEndpoint; PauseEvent: TEvent); virtual;
    procedure InternalExecute; virtual;
    // protected user methods
    procedure ProcessMessage(var Msg: TCNTSMessage); virtual;
  public
    // public user methods
    Function Main: Boolean; virtual; abstract;
    // other public methods
    procedure Terminate; virtual;    
    Function PostMessage(Param1,Param2: TCNTSMessageParam): Boolean; virtual;
    Function SendMessage(Param1,Param2: TCNTSMessageParam): TCNTSMessageResult; virtual;
    procedure SignalStageProgress(Stage: Integer; Progress: Double); virtual;
    procedure SignalProgress(Progress: Double); virtual;
    procedure Cycle(Timeout: UInt32 = 0); virtual;
    // properties
    property TaskID: TCNTSTaskID read fTaskID;
    property Terminated: Boolean read GetTerminated write SetTerminated;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCNTSThread - class declaration
===============================================================================}
type
  TCNTSThread = class(TThread)
  protected
    fTaskObject:  TCNTSTask;
    procedure Execute; override;
  public
    constructor Create(TaskObject: TCNTSTask);
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSManager
--------------------------------------------------------------------------------
===============================================================================}
type
  TCNTSTaskState = (
    tsReady,        // never ran, can be automatically started, initial state
    tsInactive,     // never ran, paused by user, cannot be automatically started
    tsRunning,      // running
    tsPaused,       // running, paused by user
    tsQueued,       // running, paused by manager, can be automatically unpaused
    tsStopping,     // running, task was stopped by the user
    tsTerminating,  // running, manager is waiting for the thread to end
    tsCompleted,    // completed, returned true
    tsAborted);     // completed, returned false

  TCNTSTaskItem = record
    TaskID:       TCNTSTaskID;  
    TaskObject:   TCNTSTask;
    TaskState:    TCNTSTaskState;
    TaskStage:    Integer;
    TaskProgress: Double;
  end;

  TCNTSTaskItemFull = record
    PublicPart:     TCNTSTaskItem;
    CommEndpoint:   TMessangerEndpoint;
    CommEndpointID: TMsgrEndpointID;
    PauseEvent:     TEvent;
    AssignedThread: TCNTSThread;
  end;

  TCNTSTaskEvent = procedure(Sender: TObject; TaskIndex: Integer) of object;
  TCNTSTaskCallback = procedure(Sender: TObject; TaskIndex: Integer);

  // task index points to a sender
  TCNTSMessageEvent = procedure(Sender: TObject; TaskIndex: Integer; var Msg: TCNTSMessage) of object;
  TCNTSMessageCallback = procedure(Sender: TObject; TaskIndex: Integer; var Msg: TCNTSMessage);

{===============================================================================
    TCNTSManager - class declaration
===============================================================================}
type
  TCNTSManager = class(TCustomListObject)
  protected
    fTasks:                       array of TCNTSTaskItemFull;
    fTaskCount:                   Integer;
    fOwnsTaskObjects:             Boolean;
    fMaxConcurrentTasks:          Integer;
    fMessanger:                   TMessanger;
    fCommEndpoint:                TMessangerEndpoint;
    // events
    fOnTaskStateEvent:            TCNTSTaskEvent;
    fOnTaskStateCallback:         TCNTSTaskCallback;
    fOnTaskStageProgressEvent:    TCNTSTaskEvent;
    fOnTaskStageProgressCallback: TCNTSTaskCallback;
    fOnTaskProgressEvent:         TCNTSTaskEvent;
    fOnTaskProgressCallback:      TCNTSTaskCallback;
    fOnTaskCompleteEvent:         TCNTSTaskEvent;
    fOnTaskCompleteCallback:      TCNTSTaskCallback;
    fOnTaskRemovingEvent:         TCNTSTaskEvent;
    fOnTaskRemovingCallback:      TCNTSTaskCallback;
    fOnMessageEvent:              TCNTSMessageEvent;
    fOnMessageCallback:           TCNTSMessageCallback;
    fOnChangeEvent:               TNotifyEvent;
    fOnChangeCallback:            TNotifyCallback;
    // getters, setters
    Function GetTask(Index: Integer): TCNTSTaskItem; virtual;
    procedure SetMaxConcurrentTasks(Value: Integer); virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // events firing
    procedure DoTaskState(TaskIndex: Integer); virtual;
    procedure DoTaskStageProgress(TaskIndex: Integer); virtual;
    procedure DoTaskProgress(TaskIndex: Integer); virtual;
    procedure DoTaskComplete(TaskIndex: Integer); virtual;
    procedure DoTaskRemoving(TaskIndex: Integer); virtual;
    procedure DoMessage(TaskIndex: Integer; var Msg: TCNTSMessage); virtual;
    procedure DoChange; virtual;
    // internal processing methods
    Function FindCommEndpoint(CommIndpointID: TMsgrEndpointID; out TaskIndex: Integer): Boolean; virtual;
    procedure MessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags); virtual;
    procedure ManageRunningTasks(IgnoredTask: Integer = -1); virtual;
    procedure TerminateTask(TaskIndex: Integer); virtual;
    procedure WaitAndFreeTask(TaskIndex: Integer); virtual;
    procedure ManagedStartTask(TaskIndex: Integer); virtual;
    procedure ManagedStopTask(TaskIndex: Integer); virtual;
    Function InternalExtractTask(TaskIndex: Integer): TCNTSTask; virtual;
    // init, final
    procedure Initialize(OwnsTaskObjects: Boolean); virtual;
    procedure Finalize; virtual;
  public
    class Function ProcessorCount: Integer; virtual;
    constructor Create(OwnsTaskObjects: Boolean = True);
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function First: TCNTSTaskItem; virtual;
    Function Last: TCNTSTaskItem; virtual;
    // list management
    Function IndexOfTask(TaskID: TCNTSTaskID): Integer; overload; virtual;
    Function IndexOfTask(TaskObject: TCNTSTask): Integer; overload; virtual;
    Function FindTask(TaskID: TCNTSTaskID; out TaskIndex: Integer): Boolean; overload; virtual;
    Function FindTask(TaskObject: TCNTSTask; out TaskIndex: Integer): Boolean; overload; virtual;
    Function AddTask(TaskID: TCNTSTaskID; TaskObject: TCNTSTask; Active: Boolean = True): Integer; overload; virtual;
    Function AddTask(TaskObject: TCNTSTask; Active: Boolean = True): Integer; overload; virtual;
    procedure InsertTask(Index: Integer; TaskID: TCNTSTaskID; TaskObject: TCNTSTask; Active: Boolean = True); overload; virtual;
    procedure InsertTask(Index: Integer; TaskObject: TCNTSTask; Active: Boolean = True); overload; virtual;
    procedure MoveTask(SrcIdx,DstIdx: Integer); virtual;
    procedure ExchangeTask(Index1,Index2: Integer); virtual;
    Function ExtractTask(TaskID: TCNTSTaskID): TCNTSTask; overload; virtual;
    Function ExtractTask(TaskObject: TCNTSTask): TCNTSTask; overload; virtual;  
    Function RemoveTask(TaskID: TCNTSTaskID): Integer; overload; virtual;
    Function RemoveTask(TaskObject: TCNTSTask): Integer; overload; virtual;
    procedure DeleteTask(TaskIndex: Integer); virtual;
    procedure ClearTasks; virtual;
    procedure ClearCompletedTasks; virtual;
    // task running management
    procedure StartTask(TaskIndex: Integer); virtual;
    procedure PauseTask(TaskIndex: Integer); virtual;
    procedure ResumeTask(TaskIndex: Integer); virtual;
    procedure StopTask(TaskIndex: Integer); virtual;
    // task properties
    Function GetTaskThreadPriority(TaskIndex: Integer): TThreadPriority; virtual;
    Function SetTaskThreadPriority(TaskIndex: Integer; NewPriority: TThreadPriority): TThreadPriority; virtual;
    // other task methods
    Function RunningTasksCount: Integer; virtual;
    procedure RunningTasksWaitFor; virtual;
    Function UnfinishedTasksCount(CountPaused: Boolean = True): Integer; virtual;
    // messaging
    Function PostMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): Boolean; virtual;
    Function SendMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): TCNTSMessageResult; virtual;
    procedure Update(Timeout: UInt32 = 0); virtual;
    // properties
    property Tasks[Index: Integer]: TCNTSTaskItem read GetTask; default;
    property TaskCount: Integer read GetCount;
    property OwnsTaskObjects: Boolean read fOwnsTaskObjects write fOwnsTaskObjects;
    property MaxConcurrentTasks: Integer read fMaxConcurrentTasks write SetMaxConcurrentTasks;
    // events
    property OnTaskStateCallback: TCNTSTaskCallback read fOnTaskStateCallback write fOnTaskStateCallback;
    property OnTaskStateEvent: TCNTSTaskEvent read fOnTaskStateEvent write fOnTaskStateEvent;
    property OnTaskState: TCNTSTaskEvent read fOnTaskStateEvent write fOnTaskStateEvent;
    property OnTaskStageProgressCallback: TCNTSTaskCallback read fOnTaskProgressCallback write fOnTaskProgressCallback;
    property OnTaskStageProgressEvent: TCNTSTaskEvent read fOnTaskProgressEvent write fOnTaskProgressEvent;
    property OnTaskStageProgress: TCNTSTaskEvent read fOnTaskProgressEvent write fOnTaskProgressEvent;
    property OnTaskProgressEventCallback: TCNTSTaskCallback read fOnTaskProgressCallback write fOnTaskProgressCallback;
    property OnTaskProgressEvent: TCNTSTaskEvent read fOnTaskProgressEvent write fOnTaskProgressEvent;
    property OnTaskProgress: TCNTSTaskEvent read fOnTaskProgressEvent write fOnTaskProgressEvent;
    property OnTaskCompleteCallback: TCNTSTaskCallback read fOnTaskCompleteCallback write fOnTaskCompleteCallback;
    property OnTaskCompleteEvent: TCNTSTaskEvent read fOnTaskCompleteEvent write fOnTaskCompleteEvent;
    property OnTaskComplete: TCNTSTaskEvent read fOnTaskCompleteEvent write fOnTaskCompleteEvent;
    property OnTaskRemovingCallback: TCNTSTaskCallback read fOnTaskRemovingCallback write fOnTaskRemovingCallback;
    property OnTaskRemovingEvent: TCNTSTaskEvent read fOnTaskRemovingEvent write fOnTaskRemovingEvent;
    property OnTaskRemoving: TCNTSTaskEvent read fOnTaskRemovingEvent write fOnTaskRemovingEvent;
    property OnMessageCallback: TCNTSMessageCallback read fOnMessageCallback write fOnMessageCallback;
    property OnMessageEvent: TCNTSMessageEvent read fOnMessageEvent write fOnMessageEvent;
    property OnMessage: TCNTSMessageEvent read fOnMessageEvent write fOnMessageEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
  end;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}BaseUnix,{$ENDIF}
  InterlockedOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    External functions
===============================================================================}
{$IFDEF Windows}

procedure GetNativeSystemInfo(lpSystemInfo: PSystemInfo); stdcall; external kernel32;

{$ELSE}

const
//_SC_NPROCESSORS_CONF = 83;  // not used anywhere
  _SC_NPROCESSORS_ONLN = 84;

Function sysconf(name: cInt): cLong; cdecl; external;

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TCNTSTask
--------------------------------------------------------------------------------
===============================================================================}
const
  CNTS_MSGR_ENDPOINT_MANAGER = 0;

  CNTS_MSG_USER           = 0;
  CNTS_MSG_TERMINATE      = 1;
  CNTS_MSG_PROGRESS       = 2;
  CNTS_MSG_STAGEPROGRESS  = 3;
  CNTS_MSG_COMPLETED      = 100;

{===============================================================================
    TCNTSTask - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCNTSTask - protected methods
-------------------------------------------------------------------------------}

Function TCNTSTask.GetTerminated: Boolean;
begin
Result := InterlockedLoad(fTerminated) <> 0;
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.SetTerminated(Value: Boolean);
begin
If Value then
  InterlockedStore(fTerminated,-1)
else
  InterlockedStore(fTerminated,0);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W4055 W5024{$ENDIF}
procedure TCNTSTask.InternalMessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags);
var
  TempMessage:  TCNTSMessage;
begin
case Msg.Parameter1 of
  CNTS_MSG_USER:
    begin
      TempMessage.Param1 := Msg.Parameter2;
      TempMessage.Param2 := Msg.Parameter3;
      TempMessage.Result := 0;
      ProcessMessage(TempMessage);
      If mdfSentMessage in Flags then
        TCNTSMessageResult(Pointer(Msg.Parameter4)^) := TempMessage.Result;
    end;
  CNTS_MSG_TERMINATE:
    Terminate;
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TCNTSTask.InternalSetup(TaskID: TCNTSTaskID; CommEndpoint: TMessangerEndpoint; PauseEvent: TEvent);
begin
fTaskID := TaskID;
fCommEndpoint := CommEndpoint;
fCommEndpoint.OnMessageEvent := InternalMessageHandler;
fPauseEvent := PauseEvent;
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.InternalExecute;
var
  MainProcResult: Integer;
begin
Cycle(0); // process all messages received to this point
If Main then // main processing
  MainProcResult := -1
else
  MainProcResult := 0;
fCommEndpoint.PostMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_COMPLETED,TMsgrParam(MainProcResult),0,0);
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.ProcessMessage(var Msg: TCNTSMessage);
begin
Msg.Result := 0;
end;

{-------------------------------------------------------------------------------
    TCNTSTask - public methods
-------------------------------------------------------------------------------}

procedure TCNTSTask.Terminate;
begin
InterlockedStore(fTerminated,-1);
end;

//------------------------------------------------------------------------------

Function TCNTSTask.PostMessage(Param1,Param2: TCNTSMessageParam): Boolean;
begin
Result := fCommEndpoint.PostMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_USER,Param1,Param2,0);
end;

//------------------------------------------------------------------------------

Function TCNTSTask.SendMessage(Param1,Param2: TCNTSMessageParam): TCNTSMessageResult;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If not fCommEndpoint.SendMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_USER,Param1,Param2,TMsgrParam(Addr(Result))) then
 Result := 0;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.SignalStageProgress(Stage: Integer; Progress: Double);
var
  ValueLo:  TCNTSMessageParam;
  ValueHi:  TCNTSMessageParam;
begin
ValueLo := TCNTSMessageParam(UInt32(Addr(Progress)^));
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
ValueHi := TCNTSMessageParam(PUInt32(PtrUInt(Addr(Progress)) + SizeOf(UInt32))^);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
fCommEndpoint.PostMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_STAGEPROGRESS,ValueLo,ValueHi,TCNTSMessageParam(Stage));
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.SignalProgress(Progress: Double);
var
  ValueLo:  TCNTSMessageParam;
  ValueHi:  TCNTSMessageParam;
begin
ValueLo := TCNTSMessageParam(UInt32(Addr(Progress)^));
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
ValueHi := TCNTSMessageParam(PUInt32(PtrUInt(Addr(Progress)) + SizeOf(UInt32))^);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
fCommEndpoint.PostMessage(CNTS_MSGR_ENDPOINT_MANAGER,CNTS_MSG_PROGRESS,ValueLo,ValueHi,0);
end;

//------------------------------------------------------------------------------

procedure TCNTSTask.Cycle(Timeout: UInt32 = 0);
begin
fCommEndpoint.Cycle(Timeout);
fPauseEvent.Wait(INFINITE);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCNTSThread - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCNTSThread - protected methods
-------------------------------------------------------------------------------}

procedure TCNTSThread.Execute;
begin
fTaskObject.InternalExecute;
end;

{-------------------------------------------------------------------------------
    TCNTSThread - public methods
-------------------------------------------------------------------------------}

constructor TCNTSThread.Create(TaskObject: TCNTSTask);
begin
inherited Create(False);
Priority := tpLower;
FreeOnTerminate := False;
fTaskObject := TaskObject;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCNTSManager
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCNTSManager - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCNTSManager - protected methods
-------------------------------------------------------------------------------}

Function TCNTSManager.GetTask(Index: Integer): TCNTSTaskItem;
begin
If CheckIndex(Index) then
  Result := fTasks[Index].PublicPart
else
  raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.GetTask: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.SetMaxConcurrentTasks(Value: Integer);
begin
If Value >= 1 then
  begin
    fMaxConcurrentTasks := Value;
    ManageRunningTasks;
  end
else raise ECNTSInvalidValue.CreateFmt('TCNTSManager.SetMaxConcurrentTasks: Cannot assign value smaller than 1 (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetCapacity: Integer;
begin
Result := Length(fTasks);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.SetCapacity(Value: Integer);
begin
SetLength(fTasks,Value);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetCount: Integer;
begin
Result := fTaskCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TCNTSManager.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TCNTSManager.DoTaskState(TaskIndex: Integer);
begin
If Assigned(fOnTaskStateEvent) then
  fOnTaskStateEvent(Self,TaskIndex)
else If Assigned(fOnTaskStateCallback) then
  fOnTaskStateCallback(Self,TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoTaskStageProgress(TaskIndex: Integer);
begin
If Assigned(fOnTaskStageProgressEvent) then
  fOnTaskStageProgressEvent(Self,TaskIndex)
else If Assigned(fOnTaskStageProgressCallback) then
  fOnTaskStageProgressCallback(Self,TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoTaskProgress(TaskIndex: Integer);
begin
If Assigned(fOnTaskProgressEvent) then
  fOnTaskProgressEvent(Self,TaskIndex)
else If Assigned(fOnTaskProgressCallback) then
  fOnTaskProgressCallback(Self,TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoTaskComplete(TaskIndex: Integer);
begin
If Assigned(fOnTaskCompleteEvent) then
  fOnTaskCompleteEvent(Self,TaskIndex)
else If Assigned(fOnTaskCompleteCallback) then
  fOnTaskCompleteCallback(Self,TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoTaskRemoving(TaskIndex: Integer);
begin
If Assigned(fOnTaskRemovingEvent) then
  fOnTaskRemovingEvent(Self,TaskIndex)
else If Assigned(fOnTaskRemovingCallback) then
  fOnTaskRemovingCallback(Self,TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoMessage(TaskIndex: Integer; var Msg: TCNTSMessage);
begin
If Assigned(fOnMessageEvent) then
  fOnMessageEvent(Self,TaskIndex,Msg)
else If Assigned(fOnMessageCallback) then
  fOnMessageCallback(Self,TaskIndex,Msg);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DoChange;
begin
If Assigned(fOnChangeEvent) then
  fOnChangeEvent(Self)
else If Assigned(fOnChangeCallback) then
  fOnChangeCallback(Self);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.FindCommEndpoint(CommIndpointID: TMsgrEndpointID; out TaskIndex: Integer): Boolean;
var
  i:  Integer;
begin
Result := False;
TaskIndex := -1;
For i := LowIndex to HighIndex do
  If Assigned(fTasks[i].CommEndpoint) then
    If fTasks[i].CommEndpoint.EndpointID = CommIndpointID then
      begin
        TaskIndex := i;
        Result := True;
        Break{For i};
      end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W4055 W5024{$ENDIF}
procedure TCNTSManager.MessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags);
var
  TempMessage:  TCNTSMessage;
  TaskIndex:    Integer;
  PrgsBuffer:   Int64;
begin
case Msg.Parameter1 of
  CNTS_MSG_USER:
    If FindCommEndpoint(Msg.Sender,TaskIndex) then
      begin
        TempMessage.Param1 := Msg.Parameter2;
        TempMessage.Param2 := Msg.Parameter3;
        TempMessage.Result := 0;
        DoMessage(TaskIndex,TempMessage);
        If mdfSentMessage in Flags then
          TCNTSMessageResult(Pointer(Msg.Parameter4)^) := TempMessage.Result;
      end;
  CNTS_MSG_STAGEPROGRESS:
    If FindCommEndpoint(Msg.Sender,TaskIndex) then
      begin
        Int64Rec(PrgsBuffer).Lo := UInt32(Msg.Parameter2);
        Int64Rec(PrgsBuffer).Hi := UInt32(Msg.Parameter3);
        fTasks[TaskIndex].PublicPart.TaskProgress := Double(Addr(PrgsBuffer)^);
        fTasks[TaskIndex].PublicPart.TaskStage := Integer(Msg.Parameter4);
        DoTaskStageProgress(TaskIndex);
      end;
  CNTS_MSG_PROGRESS:
    If FindCommEndpoint(Msg.Sender,TaskIndex) then
      begin
        Int64Rec(PrgsBuffer).Lo := UInt32(Msg.Parameter2);
        Int64Rec(PrgsBuffer).Hi := UInt32(Msg.Parameter3);
        fTasks[TaskIndex].PublicPart.TaskProgress := Double(Addr(PrgsBuffer)^);
        DoTaskProgress(TaskIndex);
      end;
  CNTS_MSG_COMPLETED:
    begin
      If FindCommEndpoint(Msg.Sender,TaskIndex) then
        begin
          If Assigned(fTasks[TaskIndex].AssignedThread) then
            fTasks[TaskIndex].AssignedThread.WaitFor;
          If Msg.Parameter2 <> 0 then
            fTasks[TaskIndex].PublicPart.TaskState := tsCompleted
          else
            fTasks[TaskIndex].PublicPart.TaskState := tsAborted;
          DoTaskState(TaskIndex);
          DoTaskComplete(TaskIndex);
          ManageRunningTasks;
        end
      else ManageRunningTasks;
    end;
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TCNTSManager.ManageRunningTasks(IgnoredTask: Integer = -1);
var
  RunCount: Integer;
  i:        Integer;
begin
RunCount := fMaxConcurrentTasks - RunningTasksCount;
If RunCount > 0 then
  begin
    For i := LowIndex to HighIndex do
      If RunCount > 0 then
        begin
          If (fTasks[i].PublicPart.TaskState in [tsReady,tsQueued]) and (i <> IgnoredTask) then
            begin
              ManagedStartTask(i);
              Dec(RunCount);
            end;
        end
      else Break{For i};
  end
else If RunCount < 0 then
  begin
    For i := HighIndex downto LowIndex do
      If RunCount < 0 then
        begin
          If (fTasks[i].PublicPart.TaskState = tsRunning) and (i <> IgnoredTask) then
            begin
              ManagedStopTask(i);
              Inc(RunCount);
            end;
        end
      else Break{For i};
  end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.TerminateTask(TaskIndex: Integer);
begin
case fTasks[TaskIndex].PublicPart.TaskState of
  tsReady,
  tsInactive:
    fTasks[TaskIndex].PublicPart.TaskState := tsAborted;
  tsRunning:
    begin
      fCommEndpoint.PostMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_TERMINATE,0,0,0,MSGR_PRIORITY_ABSOLUTE);
      fTasks[TaskIndex].PublicPart.TaskState := tsTerminating;
    end;
  tsPaused,
  tsQueued:
    begin
      fCommEndpoint.PostMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_TERMINATE,0,0,0,MSGR_PRIORITY_ABSOLUTE);
      fTasks[TaskIndex].PauseEvent.Unlock;
      fTasks[TaskIndex].PublicPart.TaskState := tsTerminating;
    end;
end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.WaitAndFreeTask(TaskIndex: Integer);
begin
If Assigned(fTasks[TaskIndex].AssignedThread) then
  begin
    // wait for task to truly exit
    fTasks[TaskIndex].AssignedThread.WaitFor;
    fTasks[TaskIndex].AssignedThread.Free;
  end;
fTasks[TaskIndex].CommEndpoint.Free;
fTasks[TaskIndex].PauseEvent.Free;
If fOwnsTaskObjects then
  fTasks[TaskIndex].PublicPart.TaskObject.Free
else
  DoTaskRemoving(TaskIndex);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ManagedStartTask(TaskIndex: Integer);
begin
case fTasks[TaskIndex].PublicPart.TaskState of
  tsReady:  begin
              fTasks[TaskIndex].PublicPart.TaskObject.InternalSetup(
                fTasks[TaskIndex].PublicPart.TaskID,
                fTasks[TaskIndex].CommEndpoint,
                fTasks[TaskIndex].PauseEvent);
              fTasks[TaskIndex].AssignedThread := TCNTSThread.Create(fTasks[TaskIndex].PublicPart.TaskObject);
              fTasks[TaskIndex].PublicPart.TaskState := tsRunning;
              DoTaskState(TaskIndex);
            end;
  tsQueued: begin
              fTasks[TaskIndex].PauseEvent.Unlock;
              fTasks[TaskIndex].PublicPart.TaskState := tsRunning;
              DoTaskState(TaskIndex);
            end;
end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ManagedStopTask(TaskIndex: Integer);
begin
If fTasks[TaskIndex].PublicPart.TaskState = tsRunning then
  begin
    fTasks[TaskIndex].PauseEvent.Lock;
    fTasks[TaskIndex].PublicPart.TaskState := tsQueued;
    DoTaskState(TaskIndex);
  end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.InternalExtractTask(TaskIndex: Integer): TCNTSTask;
var
  i:  Integer;
begin
If fTasks[TaskIndex].PublicPart.TaskState in [tsReady,tsInactive,tsCompleted,tsAborted] then
  begin
    If Assigned(fTasks[TaskIndex].AssignedThread) then
      begin
        fTasks[TaskIndex].AssignedThread.WaitFor;
        fTasks[TaskIndex].AssignedThread.Free;
      end;
    fTasks[TaskIndex].CommEndpoint.Free;
    fTasks[TaskIndex].PauseEvent.Free;
    Result := fTasks[TaskIndex].PublicPart.TaskObject;
    For i := TaskIndex to Pred(HighIndex) do
      fTasks[i] := fTasks[i + 1];
    Dec(fTaskCount);
    Shrink;
    DoChange;  
  end
else raise ECNTSTaskRunning.CreateFmt('TCNTSManager.InternalExtractTask: Cannot extract running task (#%d).',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Initialize(OwnsTaskObjects: Boolean);
begin
SetLength(fTasks,0);
fTaskCount := 0;
fOwnsTaskObjects := OwnsTaskObjects;
fMaxConcurrentTasks := ProcessorCount;
fMessanger := TMessanger.Create(1024);
fCommEndpoint := fMessanger.CreateEndpoint(CNTS_MSGR_ENDPOINT_MANAGER);
fCommEndpoint.OnMessage := MessageHandler;
// init events
fOnTaskStateEvent := nil;
fOnTaskStateCallback := nil;
fOnTaskStageProgressEvent := nil;
fOnTaskStageProgressCallback := nil;
fOnTaskProgressEvent := nil;
fOnTaskProgressCallback := nil;
fOnTaskCompleteEvent := nil;
fOnTaskCompleteCallback := nil;
fOnTaskRemovingEvent := nil;
fOnTaskRemovingCallback := nil;
fOnMessageEvent := nil;
fOnMessageCallback := nil;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Finalize;
begin
// prevent events firing (except for task removal)
fOnTaskStateEvent := nil;
fOnTaskStateCallback := nil;
fOnTaskStageProgressEvent := nil;
fOnTaskStageProgressCallback := nil;
fOnTaskProgressEvent := nil;
fOnTaskProgressCallback := nil;
fOnTaskCompleteEvent := nil;
fOnTaskCompleteCallback := nil;
fOnMessageEvent := nil;
fOnMessageCallback := nil;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
// clear
ClearTasks;
fCommEndpoint.Free;
fMessanger.Free;
end;

{-------------------------------------------------------------------------------
    TCNTSManager - public methods
-------------------------------------------------------------------------------}

class Function TCNTSManager.ProcessorCount: Integer;
{$IFDEF Windows}
var
  SysInfo:  TSystemInfo;
begin
GetNativeSystemInfo(@SysInfo);
Result := Integer(SysInfo.dwNumberOfProcessors);
If Result < 1 then
  Result := 1;
end;
{$ELSE}
begin
Result := sysconf(_SC_NPROCESSORS_ONLN);
If Result < 1 then
  Result := 1;
end;
{$ENDIF}

//------------------------------------------------------------------------------

constructor TCNTSManager.Create(OwnsTaskObjects: Boolean = True);
begin
inherited Create;
Initialize(OwnsTaskObjects);
end;

//------------------------------------------------------------------------------

destructor TCNTSManager.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.LowIndex: Integer;
begin
Result := Low(fTasks);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.HighIndex: Integer;
begin
Result := Pred(fTaskCount);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.First: TCNTSTaskItem;
begin
Result := GetTask(LowIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.Last: TCNTSTaskItem;
begin
Result := GetTask(HighIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.IndexOfTask(TaskID: TCNTSTaskID): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fTasks[i].PublicPart.TaskID = TaskID then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.IndexOfTask(TaskObject: TCNTSTask): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fTasks[i].PublicPart.TaskObject = TaskObject then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.FindTask(TaskID: TCNTSTaskID; out TaskIndex: Integer): Boolean;
begin
TaskIndex := IndexOfTask(TaskID);
Result := CheckIndex(TaskIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.FindTask(TaskObject: TCNTSTask; out TaskIndex: Integer): Boolean;
begin
TaskIndex := IndexOfTask(TaskObject);
Result := CheckIndex(TaskIndex);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.AddTask(TaskID: TCNTSTaskID; TaskObject: TCNTSTask; Active: Boolean = True): Integer;
var
  NewTaskItem:  TCNTSTaskItemFull;
begin
NewTaskItem.PublicPart.TaskID := TaskID;
NewTaskItem.PublicPart.TaskObject := TaskObject;
If Active then
  NewTaskItem.PublicPart.TaskState := tsReady
else
  NewTaskItem.PublicPart.TaskState := tsInactive;
NewTaskItem.PublicPart.TaskStage := 0;
NewTaskItem.PublicPart.TaskProgress := 0.0;
NewTaskItem.CommEndpoint := fMessanger.CreateEndpoint;
NewTaskItem.CommEndpointID := NewTaskItem.CommEndpoint.EndpointID;
NewTaskItem.PauseEvent := TEvent.Create(True,True);
// thread is created only when the task is started
NewTaskItem.AssignedThread := nil;
Grow;
Result := fTaskCount;
fTasks[Result] := NewTaskItem;
Inc(fTaskCount);
DoChange;
DoTaskState(Result);
ManageRunningTasks;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TCNTSManager.AddTask(TaskObject: TCNTSTask; Active: Boolean = True): Integer;
begin
Result := AddTask(0,TaskObject,Active);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.InsertTask(Index: Integer; TaskID: TCNTSTaskID; TaskObject: TCNTSTask; Active: Boolean = True);
var
  NewTaskItem:  TCNTSTaskItemFull;
  i:            Integer;
begin
If CheckIndex(Index) then
  begin
    NewTaskItem.PublicPart.TaskID := TaskID;
    NewTaskItem.PublicPart.TaskObject := TaskObject;
    If Active then
      NewTaskItem.PublicPart.TaskState := tsReady
    else
      NewTaskItem.PublicPart.TaskState := tsInactive;
    NewTaskItem.PublicPart.TaskStage := 0;
    NewTaskItem.PublicPart.TaskProgress := 0.0;
    NewTaskItem.CommEndpoint := fMessanger.CreateEndpoint;
    NewTaskItem.CommEndpointID := NewTaskItem.CommEndpoint.EndpointID;
    NewTaskItem.PauseEvent := TEvent.Create(True,True);
    NewTaskItem.AssignedThread := nil;
    Grow;
    For i := HighIndex downto Succ(Index) do
      fTasks[i] := fTasks[i - 1];
    fTasks[Index] := NewTaskItem;
    DoChange;
    DoTaskState(Index);
    ManageRunningTasks;
  end
else If Index = TaskCount then
  AddTask(TaskID,TaskObject,Active)
else
  raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.InsertTask: Index (%d) out of bounds.',[Index]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCNTSManager.InsertTask(Index: Integer; TaskObject: TCNTSTask; Active: Boolean = True);
begin
InsertTask(Index,0,TaskObject,Active);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.MoveTask(SrcIdx,DstIdx: Integer);
var
  TempItem: TCNTSTaskItemFull;
  i:        Integer;
begin
If SrcIdx <> DstIdx then
  begin
    If not CheckIndex(SrcIdx) then
      raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.MoveTask: Source index (%d) out of bounds.',[SrcIdx]);
    If not CheckIndex(DstIdx) then
      raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.MoveTask: Destination index (%d) out of bounds.',[DstIdx]);
    TempItem := fTasks[SrcIdx];
    If DstIdx > SrcIdx then
      For i := SrcIdx to Pred(DstIdx) do
        fTasks[i] := fTasks[i + 1]
    else
      For i := SrcIdx downto Succ(DstIdx) do
        fTasks[i] := fTasks[i - 1];
    fTasks[DstIdx] := TempItem;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ExchangeTask(Index1,Index2: Integer);
var
  TempItem: TCNTSTaskItemFull;
begin
If Index1 <> Index2 then
  begin
    If not CheckIndex(Index1) then
      raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.MoveTask: First index (%d) out of bounds.',[Index1]);
    If not CheckIndex(Index2) then
      raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.MoveTask: Second index (%d) out of bounds.',[Index2]);
    TempItem := fTasks[Index1];
    fTasks[Index1] := fTasks[Index2];
    fTasks[Index2] := TempItem;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.ExtractTask(TaskID: TCNTSTaskID): TCNTSTask;
var
  Index:  Integer;
begin
If FindTask(TaskID,Index) then
  Result := InternalExtractTask(Index)
else
  Result := nil;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TCNTSManager.ExtractTask(TaskObject: TCNTSTask): TCNTSTask;
var
  Index:  Integer;
begin
If FindTask(TaskObject,Index) then
  Result := InternalExtractTask(Index)
else
  Result := nil;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.RemoveTask(TaskID: TCNTSTaskID): Integer;
begin
If FindTask(TaskID,Result) then
  DeleteTask(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TCNTSManager.RemoveTask(TaskObject: TCNTSTask): Integer;
begin
If FindTask(TaskObject,Result) then
  DeleteTask(Result);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.DeleteTask(TaskIndex: Integer);
var
  i:  Integer;
begin
If CheckIndex(TaskIndex) then
  begin
    TerminateTask(TaskIndex);
    WaitAndFreeTask(TaskIndex);
    For i := TaskIndex to Pred(HighIndex) do
      fTasks[i] := fTasks[i + 1];
    Dec(fTaskCount);
    Shrink;
    DoChange;
    // dipose of received but unprocessed messages sent by the deleted task
    Update;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.DeleteTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ClearTasks;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  TerminateTask(i);
For i := HighIndex downto LowIndex do
  WaitAndFreeTask(i);
fTaskCount := 0;
Shrink;
DoChange;
Update;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ClearCompletedTasks;
var
  i,j:  Integer;
begin
For i := HighIndex downto LowIndex do
  If fTasks[i].PublicPart.TaskState = tsCompleted then
    begin
      WaitAndFreeTask(i);
      For j := i to Pred(HighIndex) do
        fTasks[j] := fTasks[j + 1];
      Dec(fTaskCount);
    end;
Shrink;
DoChange;
Update;
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.StartTask(TaskIndex: Integer);
begin
If CheckIndex(TaskIndex) then
  begin
    If fTasks[TaskIndex].PublicPart.TaskState in [tsReady,tsInactive] then
      begin
        fTasks[TaskIndex].PublicPart.TaskObject.InternalSetup(
          fTasks[TaskIndex].PublicPart.TaskID,
          fTasks[TaskIndex].CommEndpoint,
          fTasks[TaskIndex].PauseEvent);
        fTasks[TaskIndex].AssignedThread := TCNTSThread.Create(fTasks[TaskIndex].PublicPart.TaskObject);
        fTasks[TaskIndex].PublicPart.TaskState := tsRunning;
        DoTaskState(TaskIndex);
        ManageRunningTasks(TaskIndex);
      end;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.StartTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.PauseTask(TaskIndex: Integer);
begin
If CheckIndex(TaskIndex) then
  case fTasks[TaskIndex].PublicPart.TaskState of
    tsReady:
      begin
        fTasks[TaskIndex].PublicPart.TaskState := tsInactive;
        DoTaskState(TaskIndex);
      end;
    tsRunning:
      begin
        fTasks[TaskIndex].PublicPart.TaskState := tsPaused;
        fTasks[TaskIndex].PauseEvent.Lock;
        DoTaskState(TaskIndex);
        ManageRunningTasks(TaskIndex);
      end;
    tsQueued:
      begin
        fTasks[TaskIndex].PublicPart.TaskState := tsPaused;
        DoTaskState(TaskIndex);
      end;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.PauseTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.ResumeTask(TaskIndex: Integer);
begin
If CheckIndex(TaskIndex) then
  case fTasks[TaskIndex].PublicPart.TaskState of
    tsInactive:
      begin
        fTasks[TaskIndex].PublicPart.TaskState := tsReady;
        DoTaskState(TaskIndex);
        ManageRunningTasks;
      end;
    tsPaused,
    tsQueued:
      begin
        fTasks[TaskIndex].PublicPart.TaskState := tsRunning;
        fTasks[TaskIndex].PauseEvent.Unlock;
        DoTaskState(TaskIndex);
        ManageRunningTasks(TaskIndex);
      end;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.ResumeTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.StopTask(TaskIndex: Integer);
begin
If CheckIndex(TaskIndex) then
  case fTasks[TaskIndex].PublicPart.TaskState of
    tsRunning:
      begin
        fCommEndpoint.PostMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_TERMINATE,0,0,0);
        fTasks[TaskIndex].PublicPart.TaskState := tsStopping;
        DoTaskState(TaskIndex);
      end;
    tsPaused,
    tsQueued:
      begin
        fCommEndpoint.PostMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_TERMINATE,0,0,0);
        fTasks[TaskIndex].PublicPart.TaskState := tsStopping;
        fTasks[TaskIndex].PauseEvent.Unlock;
        DoTaskState(TaskIndex);
      end;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.StopTask: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.GetTaskThreadPriority(TaskIndex: Integer): TThreadPriority;
begin
If CheckIndex(TaskIndex) then
  begin
    If Assigned(fTasks[TaskIndex].AssignedThread) then
      Result := fTasks[TaskIndex].AssignedThread.Priority
    else
      Result := tpLower;  // this is the default with which the assigned threads are created
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.GetTaskThreadPriority: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.SetTaskThreadPriority(TaskIndex: Integer; NewPriority: TThreadPriority): TThreadPriority;
begin
If CheckIndex(TaskIndex) then
  begin
    If Assigned(fTasks[TaskIndex].AssignedThread) then
      begin
        Result := fTasks[TaskIndex].AssignedThread.Priority;
        fTasks[TaskIndex].AssignedThread.Priority := NewPriority;
      end
    else Result := tpLower;
  end
else raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.SetTaskThreadPriority: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.RunningTasksCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := LowIndex to HighIndex do
  If fTasks[i].PublicPart.TaskState = tsRunning then
    Inc(Result);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.RunningTasksWaitFor;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If (fTasks[i].PublicPart.TaskState = tsRunning) and Assigned(fTasks[i].AssignedThread) then
    fTasks[i].AssignedThread.WaitFor;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.UnfinishedTasksCount(CountPaused: Boolean = True): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := LowIndex to HighIndex do
  begin
    If fTasks[i].PublicPart.TaskState in [tsReady,tsRunning,tsStopping,tsTerminating] then
      Inc(Result);
    If CountPaused and (fTasks[i].PublicPart.TaskState in [tsInactive,tsPaused,tsQueued]) then
      Inc(Result);
  end;
end;

//------------------------------------------------------------------------------

Function TCNTSManager.PostMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): Boolean;
begin
If CheckIndex(TaskIndex) then
  Result := fCommEndpoint.PostMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_USER,Param1,Param2,0)
else
  raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.PostMessage: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

Function TCNTSManager.SendMessage(TaskIndex: Integer; Param1,Param2: TCNTSMessageParam): TCNTSMessageResult;
begin
If CheckIndex(TaskIndex) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  fCommEndpoint.SendMessage(fTasks[TaskIndex].CommEndpointID,CNTS_MSG_USER,Param1,Param2,TMsgrParam(Addr(Result)))
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise ECNSTIndexOutOfBounds.CreateFmt('TCNTSManager.SendMessage: Index (%d) out of bounds.',[TaskIndex]);
end;

//------------------------------------------------------------------------------

procedure TCNTSManager.Update(Timeout: UInt32 = 0);
begin
fCommEndpoint.Cycle(Timeout);
end;


end.
