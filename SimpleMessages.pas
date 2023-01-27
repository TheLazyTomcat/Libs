{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple messages

    This unit provides means of simple inter-thread and also inter-process
    message-based communication.

    It was created for use in Linux as a functional replacement of Windows
    message system (SendMessage, PostMessage, GetMessage, ...).

    There are, at this point, no performance optimizations in place, therefore
    its use should be limited only to sporadic notifications and exchange of
    very small amount of data (what can fit into the parameters).

    Both asynchronous (PostMessage) and synchronous (SendMessage) sending is
    fully supported.

      WARNING - sending a message from handler that is processing received
                synchronous (sent) message is possible, but it creates a
                complex indirect recursion. This, when not checked, can lead to
                a stack overflow. Be aware of this.

    To fetch and dispatch/process any incoming messages, call PeekMessages
    (non-blocking) or GetMessages (blocks until a message is received or a
    given timeout elapses).

    Also, there are two ways how to use this unit - either create an instance
    of TSimpleMessagesClient class and call its methods, or use provided
    procedural interface (standalone functions InitMessages, SendMessage, ...).

      WARNING - when using an TSimpleMessagesClient instance, do creation and
                all method calls within a single thread.

    For more information on this unit, contact the author or consult with the
    source code.

  Version 1.0 alpha 2 (requires extensive testing) (2022-10-20)

  Last change 2023-01-26

  ©2022-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleMessages

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BinaryStreaming    - github.com/TheLazyTomcat/Lib.BinaryStreaming
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    BitVector          - github.com/TheLazyTomcat/Lib.BitVector
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
  * LinSyncObjs        - github.com/TheLazyTomcat/Lib.LinSyncObjs
    ListSorters        - github.com/TheLazyTomcat/Lib.ListSorters
    MemVector          - github.com/TheLazyTomcat/Lib.MemVector 
    NamedSharedItems   - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1               - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
  * WinSyncObjs        - github.com/TheLazyTomcat/Lib.WinSyncObjs

  Libraries UInt64Utils and WinSyncObjs are required only when compiling for
  Windows OS.

  Libraries LinSyncObjs and SimpleFutex are required only when compiling for
  Linux OS.

  Library SimpleCPUID might not be required when compiling for Windows OS,
  depending on defined symbols in InterlockedOps and BitOps libraries.  

===============================================================================}
unit SimpleMessages;

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
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, {$IFNDEF Windows}BaseUnix, {$ENDIF}
  AuxTypes, AuxClasses, BitVector, MemVector, SharedMemoryStream,
{$IFDEF Windows}
  WinSyncObjs
{$ELSE}
  LinSyncObjs
{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESMException = class(Exception);

  ESMSystemError      = class(ESMException);
  ESMInvalidValue     = class(ESMException);
  ESMLimitMismatch    = class(ESMException);
  ESMOutOfResources   = class(ESMException);
  ESMIndexOutOfBounds = class(ESMException);
  ESMNoMessageClient  = class(ESMException);

{===============================================================================
    Common types and constants
===============================================================================}
{-------------------------------------------------------------------------------
    Common types and constants - public
-------------------------------------------------------------------------------}
type
  TSMClientID      = UInt16;
  TSMMessageParam  = UInt64;
  TSMMessageResult = TSMMessageParam; // must be the same type as for parameter

  TSMMessage = record
    Sender: TSMClientID;
    Param1: TSMMessageParam;
    Param2: TSMMessageParam;
    Result: TSMMessageResult;
  end;

  TSMDispatchFlag = (dfSentMessage,dfBroadcastedMessage,dfDirectDispatch,
                     dfStopDispatching);

  TSMDispatchFlags = set of TSMDispatchFlag;

type
  TSMMessageEvent    = procedure(Sender: TObject; var Msg: TSMMessage; var Flags: TSMDispatchFlags) of object;
  TSMMessageCallback = procedure(Sender: TObject; var Msg: TSMMessage; var Flags: TSMDispatchFlags);

const
  SM_MAXCLIENTS_DEF  = 128;
  SM_MAXMESSAGES_DEF = 8192;  // this must be larger than or equal to SM_MAXCLIENTS_DEF

  CLIENTID_BROADCAST = $FFFF;

  INFINITE = UInt32(-1);  // infinite timeout

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TSMExtraInfo = record
    ClientCount:                Integer;
    MaxClients:                 Integer;
    MessageCount:               Integer;
    MaxMessages:                Integer;
    SharedMemorySize:           TMemSize;
    FetchedSentMessagesCount:   Integer;
    FetchedPostedMessagesCount: Integer;
  end;       

{-------------------------------------------------------------------------------
    Common types and constants - internal
-------------------------------------------------------------------------------}
type
{$IFDEF Windows}
  TSMCrossHandle = UInt64;
{$ENDIF}

  TSMMessageIndex     = Int32;
  TSMMessageFlags     = UInt32;
  TSMMessageTimestamp = Int64;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TSMShMemClients = packed record
    Count:                Int32;
    MessageSlotWaitCount: Int32;
    MapOffset:            UInt32;
    ArrayOffset:          UInt32;
  end;
  PSMShMemClients = ^TSMShMemClients;

  TSMShMemMessages = packed record
    Count:        Int32;
    ArrayOffset:  UInt32;
    // linked list indices
    FirstFree:    TSMMessageIndex;
    LastFree:     TSMMessageIndex;
    FirstUsed:    TSMMessageIndex;
    LastUsed:     TSMMessageIndex;
  end;
  PSMShMemMessages = ^TSMShMemMessages;

  TSMShMemHead = packed record
    Initialized:  Boolean;
    MaxClients:   Int32;
    MaxMessages:  Int32;
    Clients:      TSMShMemClients;
    Messages:     TSMShMemMessages;
  end;
  PSMShMemHead = ^TSMShMemHead;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TSMShMemClient = packed record
    Flags:          UInt32;
  {$IFDEF Windows}
    Identifier:     TGUID;
    ProcessID:      DWORD;
    Synchronizer:   TSMCrossHandle;
    ThreadID:       DWORD;
  {$ELSE}
    Synchronizer:   TLSOSimpleEvent;
    ThreadID:       pid_t;
  {$ENDIF}
    T2TWakeupCode:  UInt64;
    T2TWakeupData:  UInt64;
  end;
  PSMShMemClient = ^TSMShMemClient;

  TSMShMemMessage = packed record
    Sender:     TSMClientID;
    Recipient:  TSMClientID;
    Flags:      TSMMessageFlags;
    Timestamp:  TSMMessageTimestamp;
    P1_Result:  TSMMessageParam;
    P2_Counter: TSMMessageParam;
    MasterMsg:  TSMMessageIndex;
    // linked list indices...
    Index:      TSMMessageIndex;
    Prev:       TSMMessageIndex;
    Next:       TSMMessageIndex;
  end;
  PSMShMemMessage = ^TSMShMemMessage;

{$IFDEF Windows}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TSMClientSynchonizer = record
    Assigned:     Boolean;
    Identifier:   TGUID;
    Synchronizer: TEvent;
  end;

  TSMClientSynchonizers = array of TSMClientSynchonizer;
{$ENDIF}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TSMFetchMessagesResult = (lmrSentMessage,lmrPostedMessage);

  TSMFetchMessagesResults = set of TSMFetchMessagesResult;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSMMessageVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSMMessageVector - class declaration
===============================================================================}
type
  TSMMessageVector = class(TMemVector)
  protected
    Function GetItem(Index: Integer): TSMShMemMessage; virtual;
    procedure SetItem(Index: Integer; Value: TSMShMemMessage); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(Memory: Pointer; Count: Integer); overload;
    Function First: TSMShMemMessage; reintroduce;
    Function Last: TSMShMemMessage; reintroduce;
    Function IndexOf(Item: TSMShMemMessage): Integer; reintroduce;
    Function Add(Item: TSMShMemMessage): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: TSMShMemMessage); reintroduce;
    Function Remove(Item: TSMShMemMessage): Integer; reintroduce;
    Function Extract(Item: TSMShMemMessage): TSMShMemMessage; reintroduce;
    property Items[Index: Integer]: TSMShMemMessage read GetItem write SetItem; default;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleMessagesClient
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleMessagesClient - class declaration
===============================================================================}
type
  TSimpleMessagesClient = class(TCustomObject)
  protected
    fThreadID:              {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};
    fIsFounder:             Boolean;
    fClientID:              TSMClientID;
    fSharedMemory:          TSharedMemory;
    fShMemHead:             PSMShMemHead;
    fShMemClient:           PSMShMemClient;
    fShMemClientMap:        Pointer;
    fShMemClientArr:        Pointer;
    fShMemMessageArr:       Pointer;
    fClientMap:             TBitVectorStatic;
    fSynchronizer:          {$IFDEF Windows}TEvent{$ELSE}PLSOSimpleEvent{$ENDIF};
    fFetchedSentMessages:   TSMMessageVector;
    fFetchedPostedMessages: TSMMessageVector;
  {$IFDEF Windows}
    fClientSyncs:           TSMClientSynchonizers;
  {$ENDIF}
    fOnMessageEvent:        TSMMessageEvent;
    fOnMessageCallback:     TSMMessageCallback;
    // geting item pointers
    Function GetClientArrayItemPtr(ClientIndex: Integer): PSMShMemClient; virtual;
    Function GetMessageArrayItemPtr(MessageIndex: TSMMessageIndex): PSMShMemMessage; virtual;
    Function GetMessageArrayNextItemPtr(MessageItem: PSMShMemMessage): PSMShMemMessage; virtual;  // does not check bounds
    // internal workings
    procedure DecoupleMessage(MessageIndex: TSMMessageIndex); virtual;          // NL (= does not lock shared memory, but expects it to be locked)
    Function AddMessage(const Msg: TSMShMemMessage): TSMMessageIndex; virtual;  // NL
    procedure RemoveMessage(MessageIndex: TSMMessageIndex); virtual;            // NL
    procedure ReleaseSentMessage(MessageIndex: TSMMessageIndex; Processed: Boolean; MsgResult: TSMMessageResult); virtual;  // NL
    procedure WakeClient(ClientIndex: Integer; SetFlags: UInt32); virtual;      // NL
    procedure ThreadToThreadWakeupCall(ClientIndex: Integer); virtual;          // NL
    procedure WakeClientsMsgSlots; virtual;                                     // NL
    procedure WaitMessageSlots(ClientsCount: Boolean); virtual;                 // NL
    Function SendSinglecast(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult; virtual;
    Function SendBroadcast(Param1, Param2: TSMMessageParam): TSMMessageResult; virtual;
    Function PostSinglecast(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean; virtual;
    Function PostBroadcast(Param1, Param2: TSMMessageParam): Boolean; virtual;
    Function WaitMessages(Timeout: UInt32): Boolean; virtual;
    Function FetchMessages: TSMFetchMessagesResults; virtual;
    Function DirectDispatchMessage(var Msg: TSMShMemMessage): TSMMessageResult; virtual;
    procedure DispatchSentMessages; virtual;
    procedure DispatchPostedMessages; virtual;
    Function InternalPeekMessages: Boolean; virtual;
    procedure ThreadToThreadWakeup; virtual;
    // events firing
    procedure DoMessage(var Msg: TSMMessage; var Flags: TSMDispatchFlags); virtual;
    // object init/final
    procedure Initialize(MaxClients,MaxMessages: Integer; const NameSpace: String); virtual;
    procedure Finalize; virtual;
    // some utilities
    Function LowClientIndex: Integer; virtual;
    Function HighClientIndex: Integer; virtual;
    Function CheckClientIndex(ClientIndex: Integer): Boolean; virtual;
    Function CheckClientID(ClientID: TSMClientID): Boolean; virtual;
    Function LowMessageIndex: TSMMessageIndex; virtual;
    Function HighMessageIndex: TSMMessageIndex; virtual;
    Function CheckMessageIndex(MessageIndex: TSMMessageIndex): Boolean; virtual;
  public
    constructor Create(MaxClients,MaxMessages: Integer; const NameSpace: String = ''); overload;
    constructor Create(const NameSpace: String = ''); overload;
    destructor Destroy; override;
    Function SendMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult; virtual;
    Function PostMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean; virtual;
    procedure GetMessages(Timeout: UInt32 = INFINITE); virtual;
    procedure PeekMessages; virtual;
    Function GetExtraInfo: TSMExtraInfo; virtual;
    property IsFounder: Boolean read fIsFounder;
    property ClientID: TSMClientID read fClientID;
    property OnMessageCallback: TSMMessageCallback read fOnMessageCallback write fOnMessageCallback;
    property OnMessageEvent: TSMMessageEvent read fOnMessageEvent write fOnMessageEvent;
    property OnMessage: TSMMessageEvent read fOnMessageEvent write fOnMessageEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

procedure InitMessages(Handler: TSMMessageCallback; MaxClients,MaxMessages: Integer; const NameSpace: String = ''); overload;
procedure InitMessages(Handler: TSMMessageEvent; MaxClients,MaxMessages: Integer; const NameSpace: String = ''); overload;
procedure InitMessages(Handler: TSMMessageCallback; const NameSpace: String = ''); overload;
procedure InitMessages(Handler: TSMMessageEvent; const NameSpace: String = ''); overload;

procedure FinalMessages;

procedure SetMessageHandler(Handler: TSMMessageCallback); overload;
procedure SetMessageHandler(Handler: TSMMessageEvent); overload;

Function ActiveMessages: Boolean;

//------------------------------------------------------------------------------

Function SendMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult;
Function PostMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean;

procedure GetMessages(Timeout: UInt32 = INFINITE);
procedure PeekMessages;


implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}Linux, SysCall, {$ENDIF} Math;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
    Auxiliary functions
===============================================================================}

Function GetTimestamp: TSMMessageTimestamp;
{$IFDEF Windows}
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise ESMSystemError.CreateFmt('GetTimestamp: Cannot obtain time stamp (%d).',[GetLastError]);
{$ELSE}
var
  Time: TTimeSpec;
begin
If clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = 0 then
  Result := (Int64(Time.tv_sec) * 1000000000) + Time.tv_nsec
else
  raise ESMSystemError.CreateFmt('GetTimestamp: Cannot obtain time stamp (%d).',[errno]);
{$ENDIF}
Result := Result and $7FFFFFFFFFFFFFFF; // mask out sign bit
end;

//------------------------------------------------------------------------------

Function GetElapsedMillis(StartTime: TSMMessageTimestamp): UInt32;
var
  CurrentTime:  TSMMessageTimestamp;
  Temp:         Int64;
begin
CurrentTime := GetTimestamp;
If CurrentTime >= StartTime then
  begin
  {$IFDEF Windows}
    Temp := 1;
    If QueryPerformanceFrequency(Temp) then
      Temp := Trunc(((CurrentTime - StartTime) / Temp) * 1000)
    else
      raise ESMSystemError.CreateFmt('GetElapsedMillis: Cannot obtain timer frequency (%d).',[GetLastError]);
  {$ELSE}
    Temp := (CurrentTime - StartTime) div 1000000;  // stamps are in ns, convert to ms
  {$ENDIF}
    If Temp < INFINITE then
      Result := UInt32(Temp)
    else
      Result := INFINITE;
  end
else Result := INFINITE;
end;

//------------------------------------------------------------------------------

Function ConstructMessage(Sender,Recipient: TSMClientID; Flags: TSMMessageFlags; P1,P2: TSMMessageParam; MasterMsg: TSMMessageIndex): TSMShMemMessage;
begin
Result.Sender := Sender;
Result.Recipient := Recipient;
Result.Flags := Flags;
Result.Timestamp := GetTimestamp;
Result.P1_Result := P1;
Result.P2_Counter := P2;
Result.MasterMsg := MasterMsg;
Result.Index := -1;
Result.Prev := -1;
Result.Next := -1;
end;

//------------------------------------------------------------------------------

{$IFNDEF Windows}
Function gettid: pid_t;
begin
Result := do_syscall(syscall_nr_gettid);
end;
{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TSMMessageVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSMMessageVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSMMessageVector - protected methods
-------------------------------------------------------------------------------}

Function TSMMessageVector.GetItem(Index: Integer): TSMShMemMessage;
begin
Result := TSMShMemMessage(GetItemPtr(Index)^);
end;

//------------------------------------------------------------------------------

procedure TSMMessageVector.SetItem(Index: Integer; Value: TSMShMemMessage);
begin
SetItemPtr(Index,@Value);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{
  Order messages only by time.
  
  Because messages are traversed from high index to low, the sorting is done in
  reverse (from higher timestamp to lower, so the older are dispatched first).
}
If TSMShMemMessage(Item1^).Timestamp < TSMShMemMessage(Item2^).Timestamp then
  Result := +1
else If TSMShMemMessage(Item1^).Timestamp > TSMShMemMessage(Item2^).Timestamp then
  Result := -1
else
  Result := 0;
end;

{-------------------------------------------------------------------------------
    TSMMessageVector - public methods
-------------------------------------------------------------------------------}

constructor TSMMessageVector.Create;
begin
inherited Create(SizeOf(TSMShMemMessage));
ShrinkMode := smKeepCap;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TSMMessageVector.Create(Memory: Pointer; Count: Integer);
begin
inherited Create(Memory,Count,SizeOf(TSMShMemMessage));
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.First: TSMShMemMessage;
begin
Result := TSMShMemMessage(inherited First^);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.Last: TSMShMemMessage;
begin
Result := TSMShMemMessage(inherited Last^);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.IndexOf(Item: TSMShMemMessage): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.Add(Item: TSMShMemMessage): Integer;
begin
Result := inherited Add(@Item);
end;

//------------------------------------------------------------------------------

procedure TSMMessageVector.Insert(Index: Integer; Item: TSMShMemMessage);
begin
inherited Insert(Index,@Item);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.Remove(Item: TSMShMemMessage): Integer;
begin
Result := inherited Remove(@Item);
end;

//------------------------------------------------------------------------------

Function TSMMessageVector.Extract(Item: TSMShMemMessage): TSMShMemMessage;
var
  TempPtr:  Pointer;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := TSMShMemMessage(TempPtr^)
else
  FillChar(Addr(Result)^,SizeOf(Result),0);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleMessagesClient
--------------------------------------------------------------------------------
===============================================================================}
const
  SM_SHAREDMEM_PREFIX = 'sm_shrdmem_';

  SM_CLIENTFLAG_RECVSMSG = UInt32($00000001); // received sent message(s)
  SM_CLIENTFLAG_RECVPMSG = UInt32($00000002); // received posted message(s)
  SM_CLIENTFLAG_RECVMSG  = UInt32(SM_CLIENTFLAG_RECVSMSG or SM_CLIENTFLAG_RECVPMSG);  // received a message
  SM_CLIENTFLAG_MSGSLTWT = UInt32($00000004); // client is waiting for a free message slot

  SM_MSGFLAG_SENT      = UInt32($00000001); // message was sent, not posted
  SM_MSGFLAG_BROADCAST = UInt32($00000002); // broadcasted message
  SM_MSGFLAG_MASTER    = UInt32($00000004); // sent broadcasted master message
  SM_MSGFLAG_FETCHED   = UInt32($00000008); // message is fetched but not removed (sent messages, prevents refetching)
  SM_MSGFLAG_RELEASED  = UInt32($00000010); // sender is released from waiting
  SM_MSGFLAG_PROCESSED = UInt32($00000020); // message was processed by recipient

{===============================================================================
    TSimpleMessagesClient - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleMessagesClient - protected methods
-------------------------------------------------------------------------------}

Function TSimpleMessagesClient.GetClientArrayItemPtr(ClientIndex: Integer): PSMShMemClient;
begin
If CheckClientIndex(ClientIndex) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PSMShMemClient(Pointer(PtrUInt(fShMemClientArr) + PtrUInt(ClientIndex * SizeOf(TSMShMemClient))))
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.GetClientArrayItemPtr: Index (%d) out of bounds.',[ClientIndex]);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.GetMessageArrayItemPtr(MessageIndex: TSMMessageIndex): PSMShMemMessage;
begin
If CheckMessageIndex(MessageIndex) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PSMShMemMessage(Pointer(PtrUInt(fShMemMessageArr) + PtrUInt(MessageIndex * SizeOf(TSMShMemMessage))))
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.GetMessageArrayItemPtr: Index (%d) out of bounds.',[MessageIndex]);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.GetMessageArrayNextItemPtr(MessageItem: PSMShMemMessage): PSMShMemMessage;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PSMShMemMessage(Pointer(PtrUInt(MessageItem) + PtrUInt(SizeOf(TSMShMemMessage))));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.DecoupleMessage(MessageIndex: TSMMessageIndex);
var
  MessageItemPtr: PSMShMemMessage;
begin
If CheckMessageIndex(MessageIndex) then
  begin
    MessageItemPtr := GetMessageArrayItemPtr(MessageIndex);
    If CheckMessageIndex(MessageItemPtr^.Prev) then
      GetMessageArrayItemPtr(MessageItemPtr^.Prev)^.Next := MessageItemPtr^.Next;
    If CheckMessageIndex(MessageItemPtr^.Next) then
      GetMessageArrayItemPtr(MessageItemPtr^.Next)^.Prev := MessageItemPtr^.Prev;
    If MessageIndex = fShMemHead^.Messages.FirstFree then
      fShMemHead^.Messages.FirstFree := MessageItemPtr^.Next;
    If MessageIndex = fShMemHead^.Messages.LastFree then
      fShMemHead^.Messages.LastFree := MessageItemPtr^.Prev;
    If MessageIndex = fShMemHead^.Messages.FirstUsed then
      fShMemHead^.Messages.FirstUsed := MessageItemPtr^.Next;
    If MessageIndex = fShMemHead^.Messages.LastUsed then
      fShMemHead^.Messages.LastUsed := MessageItemPtr^.Prev;
    MessageItemPtr^.Prev := -1;
    MessageItemPtr^.Next := -1;
  end
else raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.DecoupleMessage: Message index (%d) out of bounds.',[MessageIndex]);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.AddMessage(const Msg: TSMShMemMessage): TSMMessageIndex;
var
  MessageItemPtr: PSMShMemMessage;
begin
If CheckMessageIndex(fShMemHead^.Messages.FirstFree) then
  begin
    Result := fShMemHead^.Messages.FirstFree;
    MessageItemPtr := GetMessageArrayItemPtr(Result);
    // first move the item from free to used list
    DecoupleMessage(MessageItemPtr^.Index);
    If not CheckMessageIndex(fShMemHead^.Messages.FirstUsed) then
      fShMemHead^.Messages.FirstUsed := MessageItemPtr^.Index;
    If CheckMessageIndex(fShMemHead^.Messages.LastUsed) then
      GetMessageArrayItemPtr(fShMemHead^.Messages.LastUsed)^.Next := MessageItemPtr^.Index;
    MessageItemPtr^.Prev := fShMemHead^.Messages.LastUsed;
    MessageItemPtr^.Next := -1;
    fShMemHead^.Messages.LastUsed := MessageItemPtr^.Index;
    // now copy the data
    MessageItemPtr^.Sender := Msg.Sender;
    MessageItemPtr^.Recipient := Msg.Recipient;
    MessageItemPtr^.Flags := Msg.Flags;
    MessageItemPtr^.Timestamp := Msg.Timestamp;
    MessageItemPtr^.P1_Result := Msg.P1_Result;
    MessageItemPtr^.P2_Counter := Msg.P2_Counter;
    MessageItemPtr^.MasterMsg := Msg.MasterMsg;
    Inc(fShMemHead^.Messages.Count);
  end
else raise ESMOutOfResources.Create('TSimpleMessagesClient.AddMessage: No free message slot.');
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.RemoveMessage(MessageIndex: TSMMessageIndex);
var
  MessageItemPtr: PSMShMemMessage;
begin
If CheckMessageIndex(MessageIndex) then
  begin
    MessageItemPtr := GetMessageArrayItemPtr(MessageIndex);
    // just move the item to the list of free
    DecoupleMessage(MessageIndex);
    If not CheckMessageIndex(fShMemHead^.Messages.FirstFree) then
      fShMemHead^.Messages.FirstFree := MessageIndex;
    If CheckMessageIndex(fShMemHead^.Messages.LastFree) then
      GetMessageArrayItemPtr(fShMemHead^.Messages.LastFree)^.Next := MessageIndex;
    MessageItemPtr^.Prev := fShMemHead^.Messages.LastFree;
    MessageItemPtr^.Next := -1;
    fShMemHead^.Messages.LastFree := MessageIndex;
    Dec(fShMemHead^.Messages.Count);
  end
else raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.RemoveMessage: Message index (%d) out of bounds.',[MessageIndex]);
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.ReleaseSentMessage(MessageIndex: TSMMessageIndex; Processed: Boolean; MsgResult: TSMMessageResult);
var
  MessageItemPtr: PSMShMemMessage;
begin
If CheckMessageIndex(MessageIndex) then
  begin
    MessageItemPtr := GetMessageArrayItemPtr(MessageIndex);
    If MessageItemPtr^.Flags and SM_MSGFLAG_SENT <> 0 then
      begin
        If Processed then
          begin
            MessageItemPtr^.Flags := MessageItemPtr^.Flags or SM_MSGFLAG_PROCESSED;
            MessageItemPtr^.P1_Result := MsgResult;
          end;
        If MessageItemPtr^.Flags and SM_MSGFLAG_MASTER <> 0 then
          begin
            // broadcasted master message; decrement the counter
            MessageItemPtr^.P2_Counter := TSMMEssageParam(Int64(MessageItemPtr^.P2_Counter) - 1);
            If Int64(MessageItemPtr^.P2_Counter) <= 0 then
              begin
                MessageItemPtr^.Flags := MessageItemPtr^.Flags or SM_MSGFLAG_RELEASED;
                WakeClient(Integer(MessageItemPtr^.Sender),0);
              end;
          end
        else
          begin
            // singlecast message
            MessageItemPtr^.Flags := MessageItemPtr^.Flags or SM_MSGFLAG_RELEASED;
            WakeClient(Integer(MessageItemPtr^.Sender),0);
          end;
      end;
  end
else raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.ReleaseMessage: Message index (%d) out of bounds.',[MessageIndex]);
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.WakeClient(ClientIndex: Integer; SetFlags: UInt32);
var
  ClientItemPtr:  PSMShMemClient;
{$IFNDEF Windows}
  CallResult:     cInt;
{$ENDIF}

{$IFDEF Windows}
  procedure DuplicateClientSynchronizer;
  begin
    fClientSyncs[ClientIndex].Assigned := True;
    fClientSyncs[ClientIndex].Identifier := ClientItemPtr^.Identifier;
    fClientSyncs[ClientIndex].Synchronizer :=
      TEvent.DuplicateFromProcessID(ClientItemPtr^.ProcessID,THandle(ClientItemPtr^.Synchronizer));
  end;
{$ENDIF}

begin
If CheckClientIndex(ClientIndex) then
  begin
    ClientItemPtr := GetClientArrayItemPtr(ClientIndex);
  {$IFDEF Windows}
    If fClientSyncs[ClientIndex].Assigned then
      begin
        If not IsEqualGUID(fClientSyncs[ClientIndex].Identifier,ClientItemPtr^.Identifier) then
          begin
            fClientSyncs[ClientIndex].Synchronizer.Free;
            DuplicateClientSynchronizer;
          end;
      end
    else DuplicateClientSynchronizer;
  {$ENDIF}
    // set flags and release the event
    ClientItemPtr^.Flags := ClientItemPtr^.Flags or SetFlags;
  {$IFDEF Windows}
    fClientSyncs[ClientIndex].Synchronizer.SetEventStrict;
  {$ELSE}
    CallResult := event_auto_unlock(Addr(GetClientArrayItemPtr(ClientIndex)^.Synchronizer));
    If CallResult <> 0 then
      raise ESMSystemError.CreateFmt('TSimpleMessagesClient.WakeClient: Failed to unlock event (%d)',[CallResult]);
  {$ENDIF}
  end
else raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.WakeClient: Client index (%d) out of bounds.',[ClientIndex]);
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.ThreadToThreadWakeupCall(ClientIndex: Integer);
type
  TObjProc = procedure of object;
var
  ClientItemPtr:  PSMShMemClient;
  MethodToCall:   TMethod;
begin
If CheckClientIndex(ClientIndex) then
  begin
    ClientItemPtr := GetClientArrayItemPtr(ClientIndex);
    If ClientItemPtr^.ThreadID = fThreadID then
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        MethodToCall.Code := Pointer(PtrUInt(ClientItemPtr^.T2TWakeupCode));
        MethodToCall.Data := Pointer(PtrUInt(ClientItemPtr^.T2TWakeupData));
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        // call the method (unlock data so the called client can safely lock the for itself)
        fSharedMemory.Unlock;
        try
          TObjProc(MethodToCall);
        finally
          fSharedMemory.Lock;
        end;
      end;
  end
else raise ESMIndexOutOfBounds.CreateFmt('TSimpleMessagesClient.ThreadToThreasWakeupCall: Client index (%d) out of bounds.',[ClientIndex]);
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.WakeClientsMsgSlots;
var
  FreeMsgSlots: Integer;
  i:            Integer;
begin
// wake clients waiting for free message slot (as many as there is free slots)
If fShMemHead^.Clients.MessageSlotWaitCount > 0 then
  begin
    FreeMsgSlots := fShMemHead^.MaxMessages - fShMemHead^.Messages.Count;
    If FreeMsgSlots > 0 then
      For i := LowClientIndex to HighClientIndex do
        If (i <> Integer(fClientID)) and fClientMap[i] and
          (GetClientArrayItemPtr(i)^.Flags and SM_CLIENTFLAG_MSGSLTWT <> 0) then
          begin
            WakeClient(i,0);
            If FreeMsgSlots > 1 then
              Dec(FreeMsgSlots)
            else
              Break{For i};
          end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.WaitMessageSlots(ClientsCount: Boolean);

  Function TrueReqCount: Integer;
  begin
    If ClientsCount then
      Result := fShMemHead^.Clients.Count
    else
      Result := 1;
  end;

begin
If fShMemHead^.MaxMessages < (fShMemHead^.Messages.Count + TrueReqCount) then
  begin
    fShMemClient^.Flags := fShMemClient^.Flags or SM_CLIENTFLAG_MSGSLTWT;
    Inc(fShMemHead^.Clients.MessageSlotWaitCount);
    try
      while fShMemHead^.MaxMessages < (fShMemHead^.Messages.Count + TrueReqCount) do
        begin
          fSharedMemory.Unlock;
          try
          {$IFDEF Windows}
            fSynchronizer.WaitFor; // infinite wait
          {$ELSE}
            event_auto_wait(fSynchronizer);
          {$ENDIF}
          finally
            fSharedMemory.Lock;
          end;
        end;
    finally
      Dec(fShMemHead^.Clients.MessageSlotWaitCount);
      fShMemClient^.Flags := fShMemClient^.Flags and not SM_CLIENTFLAG_MSGSLTWT;
    end;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.SendSinglecast(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult;
var
  TempMessage:    TSMShMemMessage;
  MessageItemPtr: PSMShMemMessage;
begin
Result := 0;
TempMessage := ConstructMessage(fClientID,Recipient,SM_MSGFLAG_SENT,Param1,Param2,-1);
// do actual sending only when the recipiend is not this instance
If Recipient <> fClientID then
  begin
    fSharedMemory.Lock;
    try
      If fClientMap[Integer(Recipient)] then
        begin
          // check if there is a place in the message queue, if not, wait for it
          WaitMessageSlots(False);
        {
          Check recipient validity again (it might have been destroyed during
          the wait for a free msg slot).
        }
          If fClientMap[Integer(Recipient)] then
            begin  
              MessageItemPtr := GetMessageArrayItemPtr(AddMessage(TempMessage));
              WakeClient(Integer(Recipient),SM_CLIENTFLAG_RECVSMSG);
              ThreadToThreadWakeupCall(Integer(Recipient));
              // wait for message processing, also process incoming sent messages
              while MessageItemPtr^.Flags and SM_MSGFLAG_RELEASED = 0 do
                begin
                  fSharedMemory.Unlock;
                  try
                  {$IFDEF Windows}
                    fSynchronizer.WaitFor; // infinite wait
                  {$ELSE}
                    event_auto_wait(fSynchronizer);
                  {$ENDIF}
                    If lmrSentMessage in FetchMessages then
                      DispatchSentMessages;
                  finally
                    fSharedMemory.Lock;
                  end;
                end;
              // get result and remove the message
              If MessageItemPtr^.Flags and SM_MSGFLAG_PROCESSED <> 0 then
                Result := TSMMessageResult(MessageItemPtr^.P1_Result)
              else
                Result := 0;
              RemoveMessage(MessageItemPtr^.Index);
              WakeClientsMsgSlots;
            end;
        end;
    finally
      fSharedMemory.Unlock;
    end;
  end
// sending to itself
else Result := DirectDispatchMessage(TempMessage);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.SendBroadcast(Param1, Param2: TSMMessageParam): TSMMessageResult;
var
  TempMessage:          TSMShMemMessage;
  DirectDispatchResult: TSMMEssageResult;
  MasterMessagePtr:     PSMShMemMessage;
  i:                    Integer;
begin
// first do direct dispatch
TempMessage := ConstructMessage(fClientID,fClientID,SM_MSGFLAG_SENT or SM_MSGFLAG_BROADCAST,Param1,Param2,-1);
DirectDispatchResult := DirectDispatchMessage(TempMessage);
// and now the ugly stuff...
fSharedMemory.Lock;
try
  If fShMemHead^.Clients.Count > 1 then
    begin
      // first make sure we have enought message slots (number of clients - 1, plus one for master message)
      WaitMessageSlots(True);
      If fShMemHead^.Clients.Count > 1 then
        begin
          // add master message to the queue (prevent its fetching by setting fetched flag)
          TempMessage := ConstructMessage(fClientID,CLIENTID_BROADCAST,
            SM_MSGFLAG_SENT or SM_MSGFLAG_BROADCAST or SM_MSGFLAG_MASTER,
            DirectDispatchResult,TSMMessageParam(Pred(fShMemHead^.Clients.Count)){minus self},-1);
          MasterMessagePtr := GetMessageArrayItemPtr(AddMessage(TempMessage));
          // create and add all submessages
          TempMessage := ConstructMessage(fClientID,CLIENTID_BROADCAST,
            SM_MSGFLAG_SENT or SM_MSGFLAG_BROADCAST,
            Param1,Param2,MasterMessagePtr^.Index);
          For i := LowClientIndex to HighClientIndex do
            If (i <> Integer(fClientID)) and fClientMap[i] then
              begin
                TempMessage.Recipient := TSMClientID(i);
                AddMessage(TempMessage);
                WakeClient(Integer(TempMessage.Recipient),SM_CLIENTFLAG_RECVSMSG);
                ThreadToThreadWakeupCall(Integer(TempMessage.Recipient));
              end;
          // wait on master message
          while MasterMessagePtr^.Flags and SM_MSGFLAG_RELEASED = 0 do
            begin
              fSharedMemory.Unlock;
              try
              {$IFDEF Windows}
                fSynchronizer.WaitFor; // infinite wait
              {$ELSE}
                event_auto_wait(fSynchronizer);
              {$ENDIF}
                If lmrSentMessage in FetchMessages then
                  DispatchSentMessages;
              finally
                fSharedMemory.Lock;
              end;
            end;
          // get result and remove the master message
          Result := TSMMessageResult(MasterMessagePtr^.P1_Result);
          RemoveMessage(MasterMessagePtr^.Index);
          WakeClientsMsgSlots;
        end
      else Result := DirectDispatchResult;
    end
  else Result := DirectDispatchResult;
finally
  fSharedMemory.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.PostSinglecast(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean;
var
  TempMessage:  TSMShMemMessage;
begin
Result := False;
fSharedMemory.Lock;
try
  // continue only if the recipient exists and there is a room for the message
  If fClientMap[Integer(Recipient)] and (fShMemHead^.Messages.Count < fShMemHead^.MaxMessages) then
    begin
      TempMessage := ConstructMessage(fClientID,Recipient,0,Param1,Param2,-1);
      AddMessage(TempMessage);
      WakeClient(Integer(Recipient),SM_CLIENTFLAG_RECVPMSG);
      Result := True;
    end;
finally
  fSharedMemory.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.PostBroadcast(Param1, Param2: TSMMessageParam): Boolean;
var
  TempMessage:  TSMShMemMessage;
  i:            Integer;
begin
Result := False;
fSharedMemory.Lock;
try
  If (fShMemHead^.Clients.Count > 0) and
     (fShMemHead^.Clients.Count <= (fShMemHead^.MaxMessages - fShMemHead^.Messages.Count)) then
    begin
      TempMessage := ConstructMessage(fClientID,CLIENTID_BROADCAST,SM_MSGFLAG_BROADCAST,Param1,Param2,-1);
      For i := LowClientIndex to HighClientIndex do
        If fClientMap[i] then // does this client exist?
          begin
            TempMessage.Recipient := TSMClientID(i);
            AddMessage(TempMessage);
            WakeClient(Integer(TempMessage.Recipient),SM_CLIENTFLAG_RECVPMSG);
          end;
      Result := True;
    end;
finally
  fSharedMemory.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.WaitMessages(Timeout: UInt32): Boolean;

  Function CheckMessages: Boolean;
  begin
    fSharedMemory.Lock;
    try
      Result := fShMemClient^.Flags and SM_CLIENTFLAG_RECVMSG <> 0;
    finally
      fSharedMemory.Unlock;
    end;
  end;

var
  StartTime:        TSMMessageTimestamp;
  TimeoutRemaining: UInt32;
  ElapsedMillis:    UInt32;
  ExitWait:         Boolean;
begin
Result := True;
StartTime := GetTimestamp;
TimeoutRemaining := Timeout;
If (fFetchedSentMessages.Count <= 0) and (fFetchedPostedMessages.Count <= 0) then
  repeat
    ExitWait := True;
  {$IFDEF Windows}
    If fSynchronizer.WaitFor(TimeoutRemaining) in [wrSignaled,wrAbandoned,wrIOCompletion,wrMessage] then
  {$ELSE}
    If event_auto_timedwait(fSynchronizer,TimeoutRemaining) = 0 then
  {$ENDIF}
      begin
        If not CheckMessages then
          begin
            // no message received, recalculate timeout and re-enter waiting
            If Timeout <> INFINITE then
              begin
                ElapsedMillis := GetElapsedMillis(StartTime);
                If Timeout <= ElapsedMillis then
                  begin
                    Result := False;
                    Break{repeat};
                  end
                else TimeoutRemaining := Timeout - ElapsedMillis;
              end;
            ExitWait := False;
          end;
      end
    else Result := False;
  until ExitWait;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.FetchMessages: TSMFetchMessagesResults;
var
  MessageIndex:   TSMMessageIndex;
  MessageItemPtr: PSMShMemMessage;
begin
fSharedMemory.Lock;
try
  If fShMemClient^.Flags and SM_CLIENTFLAG_RECVMSG <> 0 then
    begin
      MessageIndex := fShMemHead^.Messages.FirstUsed;
      while CheckMessageIndex(MessageIndex) do
        begin
          MessageItemPtr := GetMessageArrayItemPtr(MessageIndex);
          MessageIndex := MessageItemPtr^.Next;
          If MessageItemPtr^.Recipient = fClientID then
            begin
              If MessageItemPtr^.Flags and SM_MSGFLAG_SENT = 0 then
                begin
                  // posted message
                  fFetchedPostedMessages.Add(MessageItemPtr^);
                  RemoveMessage(MessageItemPtr^.Index);
                end
              else
                begin
                  // sent message (must not be master or fetched)
                  If (MessageItemPtr^.Flags and SM_MSGFLAG_FETCHED = 0) and
                     (MessageItemPtr^.Flags and SM_MSGFLAG_MASTER = 0) then
                    begin
                      MessageItemPtr^.Flags := MessageItemPtr^.Flags or SM_MSGFLAG_FETCHED;
                      fFetchedSentMessages.Add(MessageItemPtr^);
                      If MessageItemPtr^.Flags and SM_MSGFLAG_BROADCAST <> 0 then
                        RemoveMessage(MessageItemPtr^.Index);
                    end;
                end;
            end;
        end;
      fShMemClient^.Flags := fShMemClient^.Flags and not SM_CLIENTFLAG_RECVMSG;
      WakeClientsMsgSlots;
    end;
finally
  fSharedMemory.Unlock;
end;
fFetchedSentMessages.Sort;
fFetchedPostedMessages.Sort;
Result := [];
If fFetchedSentMessages.Count > 0 then
  Include(Result,lmrSentMessage);
If fFetchedPostedMessages.Count > 0 then
  Include(Result,lmrPostedMessage);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.DirectDispatchMessage(var Msg: TSMShMemMessage): TSMMessageResult;
var
  TempMessage:    TSMMessage;
  DispatchFlags:  TSMDispatchFlags;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    TempMessage.Sender := Msg.Sender;
    TempMessage.Param1 := Msg.P1_Result;
    TempMessage.Param2 := Msg.P2_Counter;
    TempMessage.Result := 0;
    DispatchFlags := [dfSentMessage,dfDirectDispatch];
    If Msg.Flags and SM_MSGFLAG_BROADCAST <> 0 then
      Include(DispatchFlags,dfBroadcastedMessage);
    DoMessage(TempMessage,DispatchFlags); // <<<
    Result := TempMessage.Result;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.DispatchSentMessages;
var
  TempShMemMessage: TSMShMemMessage;
  TempMessage:      TSMMessage;
  DispatchFlags:    TSMDispatchFlags;
  i:                Integer;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    while fFetchedSentMessages.Count > 0 do
      begin
        TempShMemMessage := fFetchedSentMessages[fFetchedSentMessages.HighIndex];
        fFetchedSentMessages.Delete(fFetchedSentMessages.HighIndex);
        TempMessage.Sender := TempShMemMessage.Sender;
        TempMessage.Param1 := TempShMemMessage.P1_Result;
        TempMessage.Param2 := TempShMemMessage.P2_Counter;
        TempMessage.Result := 0;
        DispatchFlags := [dfSentMessage];
        If TempShMemMessage.Flags and SM_MSGFLAG_BROADCAST <> 0 then
          Include(DispatchFlags,dfBroadcastedMessage);
        DoMessage(TempMessage,DispatchFlags); // <<<
        fSharedMemory.Lock;
        try
          If TempShMemMessage.Flags and SM_MSGFLAG_BROADCAST <> 0 then
            ReleaseSentMessage(TempShMemMessage.MasterMsg,True,TempMessage.Result)
          else
            ReleaseSentMessage(TempShMemMessage.Index,True,TempMessage.Result);
        finally
          fSharedMemory.Unlock;
        end;
        If dfStopDispatching in DispatchFlags then
          Break{while};
      end;
  end
else
  begin
    fSharedMemory.Lock;
    try
      For i := fFetchedSentMessages.LowIndex to fFetchedSentMessages.HighIndex do
        If fFetchedSentMessages[i].Flags and SM_MSGFLAG_BROADCAST <> 0 then
          ReleaseSentMessage(fFetchedSentMessages[i].MasterMsg,False,0)
        else
          ReleaseSentMessage(fFetchedSentMessages[i].Index,False,0);
    finally
      fSharedMemory.Unlock;
    end;
    fFetchedSentMessages.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.DispatchPostedMessages;
var
  TempShMemMessage: TSMShMemMessage;
  TempMessage:      TSMMessage;
  DispatchFlags:    TSMDispatchFlags;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    while fFetchedPostedMessages.Count > 0 do
      begin
        TempShMemMessage := fFetchedPostedMessages[fFetchedPostedMessages.HighIndex];
        fFetchedPostedMessages.Delete(fFetchedPostedMessages.HighIndex);
        TempMessage.Sender := TempShMemMessage.Sender;
        TempMessage.Param1 := TempShMemMessage.P1_Result;
        TempMessage.Param2 := TempShMemMessage.P2_Counter;
        TempMessage.Result := 0;
        DispatchFlags := [];
        If TempShMemMessage.Flags and SM_MSGFLAG_BROADCAST <> 0 then
          Include(DispatchFlags,dfBroadcastedMessage);
        DoMessage(TempMessage,DispatchFlags); // <<<
        If dfStopDispatching in DispatchFlags then
          Break{while};
      end;
  end
else fFetchedPostedMessages.Clear;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.InternalPeekMessages: Boolean;
begin
If FetchMessages <> [] then
  begin
    DispatchSentMessages;
    DispatchPostedMessages;
    Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.ThreadToThreadWakeup;
begin
{
  This method is called directly from other client that is running in the same
  thread as this one when he sents a message here.
  This prevents a deadlock when sending messages between clients in one thread
  (sender is waiting while recipient cannot react).
}
If lmrSentMessage in FetchMessages then
  DispatchSentMessages;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.DoMessage(var Msg: TSMMessage; var Flags: TSMDispatchFlags);
begin
If Assigned(fOnMessageEvent) then
  fOnMessageEvent(Self,Msg,Flags)
else If Assigned(fOnMessageCallback) then
  fOnMessageCallback(Self,Msg,Flags);
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.Initialize(MaxClients,MaxMessages: Integer; const NameSpace: String);
var
  MapFreeIdx:     Integer;
  MessageItemPtr: PSMShMemMessage;
  i:              TSMMessageIndex;
{$IFDEF Windows}
  j:              Integer;
{$ELSE}
  CallResult:     cInt;
{$ENDIF}
begin
// sanity checks
If (MaxClients <= 0) or (MaxClients >= CLIENTID_BROADCAST) then
  raise ESMInvalidValue.CreateFmt('TSimpleMessagesClient.Initialize: Invalid client limit (%d).',[MaxClients]);
If (MaxMessages <= 0) or (MaxMessages < MaxClients) then
  raise ESMInvalidValue.CreateFmt('TSimpleMessagesClient.Initialize: Invalid message limit (%d).',[MaxMessages]);
fThreadID := {$IFDEF Windows}GetCurrentThreadID{$ELSE}gettid{$ENDIF};
// create shared memory
fSharedMemory := TSharedMemory.Create(
  // calculate expected shared memory size...
  (TMemSize(SizeOf(TSMShMemHead) + 31) and not TMemSize(31)) +                  // head
  (TMemSize(Ceil(MaxClients / 8) + 31) and not TMemSize(31)) +                  // client map
  (TMemSize((MaxClients * SizeOf(TSMShMemClient)) + 31) and not TMemSize(31)) + // client array
  (TMemSize(MaxMessages * SizeOf(TSMShMemMessage)))                             // message array
  ,SM_SHAREDMEM_PREFIX + NameSpace);
fSharedMemory.Lock;
try
  fShMemHead := PSMShMemHead(fSharedMemory.Memory);
  If fShMemHead^.Initialized then
    begin
      fIsFounder := False;
      // not a first access to this memory, check limits
      If fShMemHead^.MaxClients <> MaxClients then
        raise ESMLimitMismatch.CreateFmt('TSimpleMessagesClient.Initialize: Client limit does not match (%d/%d).',
          [MaxClients,fShMemHead^.MaxClients]);
      If fShMemHead^.MaxMessages <> MaxMessages then
        raise ESMLimitMismatch.CreateFmt('TSimpleMessagesClient.Initialize: Message limit does not match (%d/%d).',
          [MaxMessages,fShMemHead^.MaxMessages]);
    end
  else
    begin
      fIsFounder := True;
      // first access to this memory, initialize everything
      fShMemHead^.Initialized := True;
      fShMemHead^.MaxClients := MaxClients;
      fShMemHead^.MaxMessages := MaxMessages;
      fShMemHead^.Clients.MapOffset := TMemSize(SizeOf(TSMShMemHead) + 31) and not TMemSize(31);
      fShMemHead^.Clients.ArrayOffset := fShMemHead^.Clients.MapOffset +
        (TMemSize(Ceil(MaxClients / 8) + 31) and not TMemSize(31));
      fShMemHead^.Messages.ArrayOffset := fShMemHead^.Clients.ArrayOffset +
        (TMemSize((MaxClients * SizeOf(TSMShMemClient)) + 31) and not TMemSize(31));
    end;
  // calculate pointers from offsets
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  fShMemClientMap := Pointer(PtrUInt(fShMemHead) + PtrUInt(fShMemHead^.Clients.MapOffset));
  fShMemClientArr := Pointer(PtrUInt(fShMemHead) + PtrUInt(fShMemHead^.Clients.ArrayOffset));
  fShMemMessageArr := Pointer(PtrUInt(fShMemHead) + PtrUInt(fShMemHead^.Messages.ArrayOffset));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  // init the linked list (must be after pointers setup)
  If fIsFounder then
    begin
      fShMemHead^.Messages.FirstFree := LowMessageIndex;
      fShMemHead^.Messages.LastFree := HighMessageIndex;
      fShMemHead^.Messages.FirstUsed := -1;
      fShMemHead^.Messages.LastUsed := -1;
      MessageItemPtr := GetMessageArrayItemPtr(0);
      For i := LowMessageIndex to HighMessageIndex do
        begin
          MessageItemPtr^.Index := i;
          If i <= LowMessageIndex then
            MessageItemPtr^.Prev := -1
          else
            MessageItemPtr^.Prev := Pred(i);
          If i >= HighMessageIndex then
            MessageItemPtr^.Next := -1
          else
            MessageItemPtr^.Next := Succ(i);
          MessageItemPtr := GetMessageArrayNextItemPtr(MessageItemPtr);
        end;
    end;
  // add self to the client array
  fClientID := CLIENTID_BROADCAST;  
  fClientMap := TBitVectorStatic.Create(fShMemClientMap,MaxClients);
  MapFreeIdx := fClientMap.FirstClean;
  If fClientMap.CheckIndex(MapFreeIdx) then
    begin
    {$IFDEF Windows}
      fSynchronizer := TEvent.Create(False,False);
    {$ENDIF}
      fShMemClient := GetClientArrayItemPtr(MapFreeIdx);
      fShMemClient^.Flags := 0;
    {$IFDEF Windows}
      If CreateGUID(fShMemClient^.Identifier) <> S_OK then
        raise ESMOutOfResources.Create('TSimpleMessagesClient.Initialize: Cannot generate client GUID.');
      fShMemClient^.ProcessID := GetCurrentProcessID;
      fShMemClient^.Synchronizer := TSMCrossHandle(fSynchronizer.Handle);
    {$ELSE}
      fSynchronizer := Addr(fShMemClient^.Synchronizer);
      CallResult := event_auto_init(fSynchronizer);
      If CallResult <> 0 then
        raise ESMSystemError.CreateFmt('TSimpleMessagesClient.Initialize: Failed to initialize event (%d).',[CallResult]);
    {$ENDIF}
      fShMemClient^.ThreadID := fThreadID;
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      fShMemClient^.T2TWakeupCode := UInt64(PtrUInt(@TSimpleMessagesClient.ThreadToThreadWakeup));
      fShMemClient^.T2TWakeupData := UInt64(PtrUInt(Self));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
      fClientMap[MapFreeIdx] := True;
      Inc(fShMemHead^.Clients.Count);
      fClientID := TSMClientID(MapFreeIdx);
    end
  else raise ESMOutOfResources.Create('TSimpleMessagesClient.Initialize: No free client slot.');
{$IFDEF Windows}
  // client synchronizers (make sure they are properly initialized)
  SetLength(fClientSyncs,MaxClients);
  For j := Low(fClientSyncs) to High(fClientSyncs) do
    If j <> Integer(fClientID) then
      begin
        fClientSyncs[j].Assigned := False;
        FillChar(fClientSyncs[j].Identifier,SizeOf(TGUID),0);
        fClientSyncs[j].Synchronizer := nil;
      end
    else
      begin
        fClientSyncs[j].Assigned := True;
        fClientSyncs[j].Identifier := fShMemClient^.Identifier;
        fClientSyncs[j].Synchronizer := fSynchronizer;
      end;
{$ENDIF}
  // vectors for received messages
  fFetchedSentMessages := TSMMessageVector.Create;
  fFetchedPostedMessages := TSMMessageVector.Create;
finally
  fSharedMemory.Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.Finalize;
var
  i:  Integer;
begin
If Assigned(fSharedMemory) then
  begin
    fSharedMemory.Lock;
    try
      If Assigned(fFetchedSentMessages) and Assigned(fFetchedPostedMessages) then
        begin
          // get and release all sent messages (also remove the posted ones)
          FetchMessages;
          For i := fFetchedSentMessages.LowIndex to fFetchedSentMessages.HighIndex do
            If fFetchedSentMessages[i].Flags and SM_MSGFLAG_BROADCAST <> 0 then
              ReleaseSentMessage(fFetchedSentMessages[i].MasterMsg,False,0)
            else
              ReleaseSentMessage(fFetchedSentMessages[i].Index,False,0);
         end;
    {$IFDEF Windows}
      // free synchronizers (do not use Low/HighClientIndex)
      For i := Low(fClientSyncs) to High(fClientSyncs) do
        If fClientSyncs[i].Assigned and (i <> Integer(fClientID)) then
          fClientSyncs[i].Synchronizer.Free;
    {$ENDIF}
      // remove self from clients
      If Assigned(fShMemClient) and (fClientID <> CLIENTID_BROADCAST) then
        begin
          fShMemClient^.Flags := 0;  // to be sure
          fClientMap[Integer(fClientID)] := False;
          Dec(fShMemHead^.Clients.Count);
        end;
      // cleanup
      fFetchedPostedMessages.Free;
      fFetchedSentMessages.Free;
    {$IFDEF Windows}
      fSynchronizer.Free;
    {$ELSE}
      event_auto_destroy(fSynchronizer);
    {$ENDIF}
      fClientMap.Free;
    finally
      fSharedMemory.Unlock;
    end;
    fSharedMemory.Free;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.LowClientIndex: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.HighClientIndex: Integer;
begin
Result := Pred(fShMemHead^.MaxClients);
end;
 
//------------------------------------------------------------------------------

Function TSimpleMessagesClient.CheckClientIndex(ClientIndex: Integer): Boolean;
begin
Result := (ClientIndex >= LowClientIndex) and (ClientIndex <= HighClientIndex);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.CheckClientID(ClientID: TSMClientID): Boolean;
begin
Result := CheckClientIndex(Integer(ClientID)) or (ClientID = CLIENTID_BROADCAST);
end;
 
//------------------------------------------------------------------------------

Function TSimpleMessagesClient.LowMessageIndex: TSMMessageIndex;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.HighMessageIndex: TSMMessageIndex;
begin
Result := Pred(fShMemHead^.MaxMessages);
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.CheckMessageIndex(MessageIndex: TSMMessageIndex): Boolean;
begin
Result := (MessageIndex >= LowMessageIndex) and (MessageIndex <= HighMessageIndex);
end;

{-------------------------------------------------------------------------------
    TSimpleMessagesClient - public methods
-------------------------------------------------------------------------------}

constructor TSimpleMessagesClient.Create(MaxClients,MaxMessages: Integer; const NameSpace: String = '');
begin
inherited Create;
Initialize(MaxClients,MaxMessages,NameSpace);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleMessagesClient.Create(const NameSpace: String = '');
begin
Create(SM_MAXCLIENTS_DEF,SM_MAXMESSAGES_DEF,NameSpace);
end;

//------------------------------------------------------------------------------

destructor TSimpleMessagesClient.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.SendMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult;
begin
If CheckClientID(Recipient) then
  begin
    If Recipient = CLIENTID_BROADCAST then
      Result := SendBroadcast(Param1,Param2)
    else
      Result := SendSinglecast(Recipient,Param1,Param2);
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.PostMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean;
begin
If CheckClientID(Recipient) then
  begin
    If Recipient = CLIENTID_BROADCAST then
      Result := PostBroadcast(Param1,Param2)
    else
      Result := PostSinglecast(Recipient,Param1,Param2);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.GetMessages(Timeout: UInt32 = INFINITE);
begin
// enter waiting only if there is no message already received
If not InternalPeekMessages then
  If WaitMessages(Timeout) then
    InternalPeekMessages;
end;

//------------------------------------------------------------------------------

procedure TSimpleMessagesClient.PeekMessages;
begin
InternalPeekMessages;
end;

//------------------------------------------------------------------------------

Function TSimpleMessagesClient.GetExtraInfo: TSMExtraInfo;
begin
fSharedMemory.Lock;
try
  Result.ClientCount := fShMemHead^.Clients.Count;
  Result.MaxClients := fShMemHead^.MaxClients;
  Result.MessageCount := fShMemHead^.Messages.Count;
  Result.MaxMessages := fShMemHead^.MaxMessages;
  Result.SharedMemorySize := fSharedMemory.Size;
  Result.FetchedSentMessagesCount := fFetchedSentMessages.Count;
  Result.FetchedPostedMessagesCount := fFetchedPostedMessages.Count;
finally
  fSharedMEmory.Unlock;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - implementation
===============================================================================}
threadvar
  ThreadMsgClient:  TSimpleMessagesClient;  // automatically initialized to nil

//------------------------------------------------------------------------------

procedure InitMessages(Handler: TSMMessageCallback; MaxClients,MaxMessages: Integer; const NameSpace: String = '');
begin
If Assigned(ThreadMsgClient) then
  FreeAndNil(ThreadMsgClient);
ThreadMsgClient := TSimpleMessagesClient.Create(MaxClients,MaxMessages,NameSpace);
ThreadMsgClient.OnMessageCallback := Handler;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure InitMessages(Handler: TSMMessageEvent; MaxClients,MaxMessages: Integer; const NameSpace: String = '');
begin
If Assigned(ThreadMsgClient) then
  FreeAndNil(ThreadMsgClient);
ThreadMsgClient := TSimpleMessagesClient.Create(MaxClients,MaxMessages,NameSpace);
ThreadMsgClient.OnMessageEvent := Handler;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure InitMessages(Handler: TSMMessageCallback; const NameSpace: String = '');
begin
InitMessages(Handler,SM_MAXCLIENTS_DEF,SM_MAXMESSAGES_DEF,NameSpace);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure InitMessages(Handler: TSMMessageEvent; const NameSpace: String = '');
begin
InitMessages(Handler,SM_MAXCLIENTS_DEF,SM_MAXMESSAGES_DEF,NameSpace);
end;

//------------------------------------------------------------------------------

procedure FinalMessages;
begin
FreeAndNil(ThreadMsgClient);
end;

//------------------------------------------------------------------------------

procedure SetMessageHandler(Handler: TSMMessageCallback);
begin
If Assigned(ThreadMsgClient) then
  begin
    If Assigned(ThreadMsgClient.OnMessageEvent) then
      ThreadMsgClient.OnMessageEvent := nil;
    ThreadMsgClient.OnMessageCallback := Handler;
  end
else raise ESMNoMessageClient.Create('SetMessageHandler: No message client for this thread.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetMessageHandler(Handler: TSMMessageEvent);
begin
If Assigned(ThreadMsgClient) then
  begin
    If Assigned(ThreadMsgClient.OnMessageCallback) then
      ThreadMsgClient.OnMessageCallback := nil;
    ThreadMsgClient.OnMessageEvent := Handler;
  end
else raise ESMNoMessageClient.Create('SetMessageHandler: No message client for this thread.');
end;

//------------------------------------------------------------------------------

Function ActiveMessages: Boolean;
begin
Result := Assigned(ThreadMsgClient);
end;

//------------------------------------------------------------------------------

Function SendMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): TSMMessageResult;
begin
If Assigned(ThreadMsgClient) then
  Result := ThreadMsgClient.SendMessage(Recipient,Param1,Param2)
else
  raise ESMNoMessageClient.Create('SendMessage: No message client for this thread.');
end;

//------------------------------------------------------------------------------

Function PostMessage(Recipient: TSMClientID; Param1, Param2: TSMMessageParam): Boolean;
begin
If Assigned(ThreadMsgClient) then
  Result := ThreadMsgClient.PostMessage(Recipient,Param1,Param2)
else
  raise ESMNoMessageClient.Create('PostMessage: No message client for this thread.');
end;

//------------------------------------------------------------------------------

procedure GetMessages(Timeout: UInt32 = INFINITE);
begin
If Assigned(ThreadMsgClient) then
  ThreadMsgClient.GetMessages(Timeout)
else
  raise ESMNoMessageClient.Create('GetMessages: No message client for this thread.');
end;

//------------------------------------------------------------------------------

procedure PeekMessages;
begin
If Assigned(ThreadMsgClient) then
  ThreadMsgClient.PeekMessages
else
  raise ESMNoMessageClient.Create('PeekMessages: No message client for this thread.');
end;

end.
