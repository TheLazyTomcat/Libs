{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Messanger

    Small library for message-based intraprocess (ie. between threads of single
    process) communication.

    To use this library, create an instance of TMessanger class. This object
    is managing all communication endpoints and their connections. It should be
    created and completely managed only in one thread, do not access it in
    other threads.

    For each thread that wants to communicate, create exactly one endpoint
    instance (instance of class TMessangerEndpoint) - refrain from using more
    than one endpoint in a single thread.
    Do NOT create these endpoints directly, always use TMessanger instance and
    its method CreateEndpoint to create the endpoint object - do this in the
    thread that is managing the messanger. Then pass this object by whatever
    means to the thread that will be using it.

    On the other hand, always free the endpoint from the thread that is using
    it, do not pass it back to thread managing the messanger.

      WARNING - all endpoint objects must be freed before you destroy the
                messanger, otherwise an exception will be raised while freeing
                the messanger instance.

    To communicate, use methods of endpoints to do so. SendMessage for
    synchronous sending, PostMessage for asynchronous sending and so on.
    To receive messages, you have to call methods WaitForMessage,
    FetchMessages, and DispatchMessages repeatedly - or call method Cycle to
    do so in one call.
    If you want to process the incoming messages, assign one of the OnMessage*
    handlers - the endpoint will be dispatching all received messages to the
    handler for processing.

  Version 2.0.3 (2022-10-26)

  Last change 2024-02-03

  ©2016-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Messanger

  Dependencies:
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
    BitOps              - github.com/TheLazyTomcat/Lib.BitOps
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
  * BitVector           - github.com/TheLazyTomcat/Lib.BitVector
    CrossSyncObjs       - github.com/TheLazyTomcat/Lib.CrossSyncObjs
    HashBase            - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps      - github.com/TheLazyTomcat/Lib.InterlockedOps
  * LinSyncObjs         - github.com/TheLazyTomcat/Lib.LinSyncObjs
    ListSorters         - github.com/TheLazyTomcat/Lib.ListSorters
    MemVector           - github.com/TheLazyTomcat/Lib.MemVector
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
unit Messanger;

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
  SysUtils, Classes,{$IFNDEF Windows} BaseUnix,{$ENDIF}
  AuxTypes, AuxClasses, MemVector, CrossSyncObjs;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMsgrException = class(Exception);

  EMsgrTimestampError   = class(EMsgrException);
  EMsgrIndexOutOfBounds = class(EMsgrException);
  EMsgrInvalidValue     = class(EMsgrException);
  EMsgrInvalidState     = class(EMsgrException);
  EMsgrNoResources      = class(EMsgrException);

{===============================================================================
    Common types and constants
===============================================================================}
type
  TMsgrEndpointID = UInt16;       PMsgrEndpointID = ^TMsgrEndpointID;
  TMsgrPriority   = Int32;        PMsgrPriority   = ^TMsgrPriority;
  TMsgrTimestamp  = Int64;        PMsgrTimestamp  = ^TMsgrTimestamp;
  TMsgrParam      = PtrInt;       PMsgrParam      = ^TMsgrParam;
  TMsgrSendParam  = Pointer;      PMsgrSendParam  = ^TMsgrSendParam;

type
{
  TMsgrMessage is used only internally, do not use it anywhere else.
}
  TMsgrMessage = packed record
    Sender:     TMsgrEndpointID;
    Recipient:  TMsgrEndpointID;
    Priority:   TMsgrPriority;
    Timestamp:  TMsgrTimestamp;
    Parameter1: TMsgrParam;
    Parameter2: TMsgrParam;
    Parameter3: TMsgrParam;
    Parameter4: TMsgrParam;
    SendParam:  TMsgrSendParam;  // used for synchronous communication
  end;
  PMsgrMessage = ^TMsgrMessage;

{
  TMsgrMessageIn is used in places where an incoming messages is passed for
  processing by the user.
}
  TMsgrMessageIn = record
    Sender:     TMsgrEndpointID;
    Parameter1: TMsgrParam;
    Parameter2: TMsgrParam;
    Parameter3: TMsgrParam;
    Parameter4: TMsgrParam;
  end;

{
  TMsgrMessageOut is used in sending of messages, where user can pass this
  structure in place of number of individual parameters.
}
  TMsgrMessageOut = record
    Recipient:  TMsgrEndpointID;
    Priority:   TMsgrPriority;
    Parameter1: TMsgrParam;
    Parameter2: TMsgrParam;
    Parameter3: TMsgrParam;
    Parameter4: TMsgrParam;    
  end;

const
{
  When selecting MSGR_ID_BROADCAST as a recipient, the message will be posted
  to all existing endpoints (including the sender).

    WARNING - this is only possible for posted messages, trying to broadcast
              synchronous message will always fail.
}
  MSGR_ID_BROADCAST = TMsgrEndpointID(High(TMsgrEndpointID)); // $FFFF

{
  Note that, when received messages are sorted for processing, the priority
  takes precedence over timestamp, meaning messages with higher priority are
  processed sooner than messages sent before them but with lower priority.
}
  MSGR_PRIORITY_MINIMAL       = TMsgrPriority(-100000);
  MSGR_PRIORITY_EXTREME_LOW   = TMsgrPriority(-10000);
  MSGR_PRIORITY_VERY_LOW      = TMsgrPriority(-1000);
  MSGR_PRIORITY_LOW           = TMsgrPriority(-100);
  MSGR_PRIORITY_BELOW_NORMAL  = TMsgrPriority(-10);
  MSGR_PRIORITY_NORMAL        = TMsgrPriority(0);
  MSGR_PRIORITY_ABOVE_NORMAL  = TMsgrPriority(10);
  MSGR_PRIORITY_HIGH          = TMsgrPriority(100);
  MSGR_PRIORITY_VERY_HIGH     = TMsgrPriority(1000);
  MSGR_PRIORITY_EXTREME_HIGH  = TMsgrPriority(10000);
  MSGR_PRIORITY_ABSOLUTE      = TMsgrPriority(100000);

  MSGR_PRIORITY_MIN = MSGR_PRIORITY_MINIMAL;
  MSGR_PRIORITY_MAX = MSGR_PRIORITY_ABSOLUTE;

  // infinite timeout
  INFINITE = UInt32(-1);

{===============================================================================
--------------------------------------------------------------------------------
                               TMsgrMessageVector
--------------------------------------------------------------------------------
===============================================================================}
{
  Class TMsgrMessageVector is only for internal purposes.
}
{===============================================================================
    TMsgrMessageVector - class declaration
===============================================================================}
type
  TMsgrMessageVector = class(TMemVector)
  protected
    Function GetItem(Index: Integer): TMsgrMessage; virtual;
    procedure SetItem(Index: Integer; Value: TMsgrMessage); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(Memory: Pointer; Count: Integer); overload;
    Function First: TMsgrMessage; reintroduce;
    Function Last: TMsgrMessage; reintroduce;
    Function IndexOf(Item: TMsgrMessage): Integer; reintroduce;
    Function Add(Item: TMsgrMessage): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: TMsgrMessage); reintroduce;
    Function Remove(Item: TMsgrMessage): Integer; reintroduce;
    Function Extract(Item: TMsgrMessage): TMsgrMessage; reintroduce;
    property Items[Index: Integer]: TMsgrMessage read GetItem write SetItem; default;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                           TMsgrBufferedMessagesVector
--------------------------------------------------------------------------------
===============================================================================}
{
  Class TMsgrBufferedMessagesVector is only for internal purposes.
}
{===============================================================================
    TMsgrBufferedMessagesVector - class declaration
===============================================================================}
type
  TMsgrBufferedMessageVector = class(TMsgrMessageVector)
  protected
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TMsgrWaiter
--------------------------------------------------------------------------------
===============================================================================}
{
  Class TMsgrWaiter is only for internal purposes.
}
type
  TMsgrWaiterResult = (wtrMessage,wtrTimeout,wtrError,wtrSentMessage,
                       wtrSendReleased,wtrSendProcessed);

  TMsgrWaiterResults = set of TMsgrWaiterResult;

{===============================================================================
    TMsgrWaiter - class declaration
===============================================================================}
type
  TMsgrWaiter = class(TObject)
  protected
    fInterlock: TCriticalSectionRTL;
    fEvent:     TEvent;
    fFlags:     UInt32;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SentMessageReceived; virtual;
    procedure PostedMessageReceived; virtual;
    procedure ReleaseSendMessage(var LocalFlags: UInt32; Processed: Boolean); virtual;
    Function WaitForMessage(Timeout: UInt32): TMsgrWaiterResult; virtual;
    Function WaitForRelease(var LocalFlags: UInt32): TMsgrWaiterResults; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TMessangerEndpoint
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TMsgrDispatchFlag

  Dispatch flags are serving two purposes - on enter to the handler, they can
  contain flags the handler can use to discern details about the currently
  processed message and also about the state of the messanger endpoint.
  Secondly, the handler can include or exclude some flags from the set - some
  flags are checked after the handle returns, and their presence or absence is
  used to control further dispatching or workings of the endpoint.

  Individual flags can be in, out, or in-out in nature.

    In flags are included before the handler is called, so it can probe them.
    Removing or adding them has no effect as they are ignored after the handler
    returns.

    Out flags are never included before the call, but handler can include them
    to control further processing.

    In-out flags can be both included before the call and also excluded or
    included to the set by the handler.


  mdfSentMessage (in)         - informs the handler that the current message
                                was sent synchronously (ie. using Send* method,
                                not Post* methods) and sender is waiting for
                                its processing

  mdfUndeliveredMessage (in)  - currently processed message is an undelivered
                                buffered message

  mdfSendBlocked (in)         - any sending or posting is blocked and will fail

  mdfStopDispatching (out)    - when added by the handler, orders the dispatcher
                                to stop dispatching further messages after the
                                current one

  mdfAutoCycle (in-out)       - when included on enter, informs the handler
                                that the endpoint is running the autocycle, by
                                removing this flag, handler can break the
                                autocycle, note that adding this flag when it
                                is not present will NOT activate the autocycle

  mdfDirectDispatch (in)      - processed message was sent to this endpoint
                                (ie. the sender is the same as recipient)                                 
}
  TMsgrDispatchFlag = (mdfSentMessage,mdfUndeliveredMessage,mdfSendBlocked,
                       mdfStopDispatching,mdfAutoCycle,mdfDirectDispatch);

  TMsgrDispatchFlags = set of TMsgrDispatchFlag;

  TMsgrWaitResult = (mwrMessage,mwrTimeout,mwrError);

  TMsgrMessageInEvent    = procedure(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags) of object;
  TMsgrMessageInCallback = procedure(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags);

  TMsgrMessageOutEvent    = procedure(Sender: TObject; Msg: TMsgrMessageOut; var Flags: TMsgrDispatchFlags) of object;
  TMsgrMessageOutCallback = procedure(Sender: TObject; Msg: TMsgrMessageOut; var Flags: TMsgrDispatchFlags);

{===============================================================================
    TMessangerEndpoint - class declaration
===============================================================================}
type
  TMessangerEndpoint = class(TCustomObject)
  protected
    fWorkingThread:           {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};
    fMessanger:               TObject;              // should be TMessanger class
    fEndpointID:              TMsgrEndpointID;
    fAutoBuffSend:            Boolean;
    fSendLevelMax:            Integer;
    // runtime variables
    fSendLevel:               Integer;
    fAutoCycle:               Boolean;
    fSendBlocked:             Boolean;
    // synchronizers
    fIncomingSynchronizer:    TCriticalSectionRTL;  // protects vector of incoming messages
    fWaiter:                  TMsgrWaiter;
    // message storage vectors
    fIncomingSentMessages:    TMsgrMessageVector;
    fIncomingPostedMessages:  TMsgrMessageVector;
    fReceivedSentMessages:    TMsgrMessageVector;
    fReceivedPostedMessages:  TMsgrMessageVector;
    fBufferedMessages:        TMsgrBufferedMessageVector;
    fUndeliveredMessages:     TMsgrMessageVector;   // stores undelivered buffered messages
    fVectorsReady:            Boolean;              // used to indicate successful creation
    // events
    fOnMessageEvent:          TMsgrMessageInEvent;
    fOnMessageCallback:       TMsgrMessageInCallback;
    fOnUndeliveredEvent:      TMsgrMessageOutEvent;
    fOnUndeliveredCallback:   TMsgrMessageOutCallback;
    fOnDestroyingEvent:       TNotifyEvent;
    fOnDestroyingCallback:    TNotifyCallback;
    // getters, setters
    Function GetMessageCount: Integer;
    Function GetMessage(Index: Integer): TMsgrMessage;
    // methods called from other (sender) threads
    procedure ReceiveSentMessages(Messages: PMsgrMessage; Count: Integer); virtual;
    procedure ReceivePostedMessages(Messages: PMsgrMessage; Count: Integer); virtual;
    // message fetching
    procedure SentMessagesFetch; virtual;
    // message dispatching
    procedure SentMessagesDispatch; virtual;
    procedure PostedMessagesDispatch; virtual;
    procedure UndeliveredDispatch; virtual;
    Function DirectDispatch(Msg: TMsgrMessage): Boolean; virtual;
    // events firing
    procedure DoMessage(Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags); virtual;
    procedure DoUndelivered(Msg: TMsgrMessageOut; var Flags: TMsgrDispatchFlags); virtual;
    procedure DoDestroying; virtual;
    // init/final
    procedure Initialize(EndpointID: TMsgrEndpointID; Messanger: TObject); virtual;
    procedure Finalize; virtual;
    // utility
    Function MsgOutToMsg(Msg: TMsgrMessageOut; SendParamPtr: PMsgrSendParam): TMsgrMessage; virtual;
    Function MsgToMsgIn(Msg: TMsgrMessage): TMsgrMessageIn; virtual;
    Function MsgToMsgOut(Msg: TMsgrMessage): TMsgrMessageOut; virtual;
  public
  {
    Create

    Do not call the constructor, use TMessanger instance to create a new
    endpoint object.
  }
    constructor Create(EndpointID: TMsgrEndpointID; Messanger: TObject);
  {
    Destroy

    During the object destruction, it is possible that events/callbacks
    OnMessage* and OnUndelivered* will be fired. It is to process messages
    received from last dispatching and unposted buffered messages respectively.

    Note that for both the SendBlocked is true.

    After that, OnDestroying* event/callback is fired - if both are assigned,
    then only the event will be called.
  }
    destructor Destroy; override;
  {
    ThreadInit

    If you want to use more than one instance of TMessangerEndpoint in a single
    thread, call this method as soon as possible at least once on all intances
    within the context of the thread that will be using them.
  }
    procedure ThreadInit; virtual;
    // message sending methods
  {
    SendMessage

    SendMessage will send the passed message and then waits - it will not
    return until the message has been processed by the recipient.

    It will return true only when the message was received AND processed
    (passed to message handler) by the recipient. So, in case the message was
    delivered, but not processed (eg. because the recipient was destroyed),
    it will return false.

    The endpoint can still receive and dispatch other sent messages (but not
    the posted ones).
    This means that, while you are waiting for the SendMessage to return, the
    endpoint can and will call message handler.
    It is to prevent a deadlock when the recipient responds by sending another
    synchronous message back to the sender within its message dispatch.

    It is allowed to send another synchronous message from a message handler,
    but as this creates a complex recursion, it is limited - see SendLevel and
    SendLevelMaximum properties.

    It is not allowed to use MSGR_ID_BROADCAST as a recipient - synchronous
    messages cannot be broadcasted.

    If AutomaticBufferedSend is set to true and message sending is not blocked
    (see property SendBlocked), then all buffered messages are posted before
    the actual sending is performed.
  }
    Function SendMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean; overload; virtual;
    Function SendMessage(Msg: TMsgrMessageOut): Boolean; overload; virtual;
  {
    PostMessage

    PostMessage adds the passed message to the recipient's incoming messages
    and immediately exits.

    Use MSGR_ID_BROADCAST as a recipient to post the message to all existing
    endpoints (sender also receives a copy).

    If AutomaticBufferedSend is set to true and message sending is not blocked
    (see property SendBlocked), then all buffered messages are posted before
    the actual posting is performed.
  }
    Function PostMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean; overload; virtual;
    Function PostMessage(Msg: TMsgrMessageOut): Boolean; overload; virtual;
  {
    BufferMessage

    BufferMessage merely adds the passed message to internal storage, preparing
    it for future posting.

    This is here to optimize rapid posting of large number of messages - they
    can be buffered and then posted all at once, significantly reducing the
    operation overhead.

    MSGR_ID_BROADCAST can be used as s recipient to post the message to all
    existing endpoints (including the sender).
  }
    procedure BufferMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL); overload; virtual;
    procedure BufferMessage(Msg: TMsgrMessageOut); overload; virtual;
  {
    PostBufferedMessages

    PostBufferedMessages posts all buffered messages at once to their respective
    recipients.

    If some of the messages cannot be delivered, they are passed at the end of
    the processing to the OnUndelivered* event/callback.
    If both OnUndeliveredCallback and OnUndeliveredEvent (equivalent to
    OnUndelivered) are assigned, then only the event is called. If none is
    assigned, then the messages are dropped an deleted.

    Note that while in the OnUndelivered* handler, all message sending and
    posting is blocked (property SendBlocked is true).
  }
    procedure PostBufferedMessages; virtual;
    // operation methods
  {
    WaitForMessage

    WaitForMessage waits until a new message is received, timeout elapses (you
    can use INFINITE constant for infinite wait) or an error occurs.
    Whichever happened is indicated by the result.
  }
    Function WaitForMessage(Timeout: UInt32): TMsgrWaitResult; virtual;
  {
    FetchMessages

    FetchMessages moves incoming messages (those deposited to a shared location
    by other endpoints) into local storage (received messages), where they can
    be accessed without the overhead of thread synchronization and locking.
  }
    procedure FetchMessages; virtual;
  {
    DispatchMessages

    When any of OnMessage(Event) or OnMessageCallback is assigned, it will
    traverse all received messages, passing each of them to the handler for
    processing (this process is called dispatching).

    All sent messages are dispatched first, the posted right after them. They
    are dispatched in order of ther priority (from higher to lower) and time of
    sending/posting (from oldest to newest). Note that priority takes precedence
    over the time.

    If both OnMessageEvent (which is equivalent to OnMessage) and
    OnMessageCallback are assigned, then only the OnMessageEvent is called.

    When no event or callback is assigned, it only clears all received messages.
  }
    procedure DispatchMessages; virtual;
  {
    ClearMessages

    Deletes all received messags without dispatching them.

    Note that senders waiting for sent messages to be processed will be
    released but the messages will NOT be marked as processed, meaning the
    respective SendMessage methods will return false.
  }
    procedure ClearMessages; virtual;
  {
    Cycle

    Calling this method is equivalent to the following sequence:

      FetchMessages;
      DispatchMessages;
      If WaitForMessage(Timeout) = mwrMessage then
        begin
          FetchMessages;
          DispatchMessages;
        end;
  }
    procedure Cycle(Timeout: UInt32); virtual;
  {
    AutoCycle

    By calling this method, the endpoint enters the autocycle. It sets
    InAutoCycle property to true and then repeatedly calls method Cycle with
    the provided timeout as long as the InAutoCycle is true.

    The autocyle can be exited by calling BreakAutoCycle or by excluding
    mdfAutoCycle from dispatch flags within any dispatch handler.

    If autocycle is already running when calling this method, it will do
    nothing and immediately returns.
  }
    procedure AutoCycle(Timeout: UInt32); virtual;
  {
    BreakAutoCycle

    Sets InAutoCycle to false, effectively breaking out of the autocycle.

    Note that the autocycle might not end immediately, depending on multiple
    factors, mainly of depth of send recursion (see SendLevelMaximum and
    SendLevel properties).
  }
    procedure BreakAutoCycle; virtual;
    // properties
    property EndpointID: TMsgrEndpointID read fEndpointID;
  {
    AutomaticBufferedSend

    When AutomaticBufferedSend is set to true, all buffered messages are
    automatically posted whenever you send or post another message (before it).
    When false, you have to explicitly call PostBufferedMessages method to post
    buffered messages.

    If SendBlocked is true, this automatic posting is blocked too.

    By default enabled.
  }
    property AutomaticBufferedSend: Boolean read fAutoBuffSend write fAutoBuffSend;
  {
    SendLevelMaximum

    Maximum depth of send recursion, see property SendLevel for details.
  }
    property SendLevelMaximum: Integer read fSendLevelMax write fSendLevelMax;
  {
    SendLevel

    When you send a synchronous message (methods SendMessage), the endpoint
    will, while waiting for the message to be processed, fetch and dispatch
    any incoming synchronous (sent) messages.

    This means that, while waiting for SendMessage to return, the endpoint can
    and will call OnMessage* handler. It is possible to again call SendMessage
    from this handler where this call can again result in dispatch and so on.

    This creates somewhat complex indirect recursion - if unchecked, it can
    lead to stack overflow and other problems.

    To limit a posibility of problems, each SendMessage increments SendLevel
    counter. When this counter is equal to or above SendLevelMaximum before
    the increment, the call to SendMessage will fail (actual send is not even
    attempted). This limits possible depth of recursion this process can go
    into.

    This counter is exposed so you can check whether SendMessage failing was
    due to this protection or not (SendLevel < SendLevelMaximum means it was
    not), and then act accordingly.
  }
    property SendLevel: Integer read fSendLevel;
  {
    InAutoCycle

    This property indicates whether the endpoint is running within the
    autocycle or not.
  }
    property InAutoCycle: Boolean read fAutoCycle;
  {
    SendBlocked

    When true, all sending and posting is blocked. It affects all SendMessage,
    PostMessage and also PostBufferedMessages methods. BufferMessage is not
    affected.

    The effect of blocking is that when you try to post/send a message, the
    respective method will just exit and return false.

    As for PostBufferedMessages - it will not technically fail, so all currently
    buffered messages stay buffered and undelivered handlers are NOT called.
  }
    property SendBlocked: Boolean read fSendBlocked;
  {
    MessageCount

    Number of messages currently received - it does not include incoming but
    not yet fetched messages.
  }
    property MessageCount: Integer read GetMessageCount;
  {
    Messages

    All received messages, both posted and sent.
  }
    property Messages[Index: Integer]: TMsgrMessage read GetMessage;
    // event/callback properties
    property OnMessageCallback: TMsgrMessageInCallback read fOnMessageCallback write fOnMessageCallback;
    property OnMessageEvent: TMsgrMessageInEvent read fOnMessageEvent write fOnMessageEvent;
    property OnMessage: TMsgrMessageInEvent read fOnMessageEvent write fOnMessageEvent;
    property OnUndeliveredCallback: TMsgrMessageOutCallback read fOnUndeliveredCallback write fOnUndeliveredCallback;
    property OnUndeliveredEvent: TMsgrMessageOutEvent read fOnUndeliveredEvent write fOnUndeliveredEvent;
    property OnUndelivered: TMsgrMessageOutEvent read fOnUndeliveredEvent write fOnUndeliveredEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TMessanger
--------------------------------------------------------------------------------
===============================================================================}
type
  TMsgrSendResult = (msrFail,msrSent,msrReleased,msrProcessed);

{===============================================================================
    TMessanger - class declaration
===============================================================================}
type
  TMessanger = class(TCustomObject)
  protected
    fEndpoints:     array of TMessangerEndpoint;
    fSynchronizer:  TMultiReadExclusiveWriteSynchronizerRTL;
    // getters, setters
    Function GetEndpointCapacity: Integer;
    Function GetEndpointCount: Integer;
    Function GetEndpoint(Index: Integer): TMessangerEndpoint;
    // methods called from endpoint
    procedure RemoveEndpoint(EndpointID: TMsgrEndpointID); virtual;
    Function SendMessage(Msg: TMsgrMessage): TMsgrSendResult; virtual;
    Function PostMessage(Msg: TMsgrMessage): Boolean; virtual;
    procedure PostBufferedMessages(Messages,Undelivered: TMsgrMessageVector); virtual;
    // init, final
    procedure Initialize(EndpointCapacity: TMsgrEndpointID); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(EndpointCapacity: TMsgrEndpointID = 128);
    destructor Destroy; override;
  {
    IDAvailable

    Returns true when endpoint with the given ID does not exist (ie. the ID is
    not taken). False otherwise.
  }
    Function IDAvailable(EndpointID: TMsgrEndpointID): Boolean; virtual;
  {
    CreateEndpoint

    Creates new endpoint. First overload uses first unused ID.

    Second overload will create new endpoint with exactly the ID given. If the
    reaquested ID is not available (is out of allocated capacity or is already
    taken), then an exception is raised.
  }
    Function CreateEndpoint: TMessangerEndpoint; overload; virtual;
    Function CreateEndpoint(EndpointID: TMsgrEndpointID): TMessangerEndpoint; overload; virtual;
    // properties
    property Endpoints[Index: Integer]: TMessangerEndpoint read GetEndpoint;
    property EndpointCapacity: Integer read GetEndpointCapacity;
    property EndpointCount: Integer read GetEndpointCount;
  end;

{===============================================================================
    Auxiliary functions - declaration
===============================================================================}

Function GetTimestamp: TMsgrTimestamp;

Function BuildMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): TMsgrMessageOut;

implementation

uses
  {$IFDEF Windows}Windows{$ELSE}Linux, SysCall{$ENDIF};

{===============================================================================
    Auxiliary functions - implementation
===============================================================================}

Function GetTimestamp: TMsgrTimestamp;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
begin
If clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = 0 then
  Result := (Int64(Time.tv_sec) * 1000000000) + Time.tv_nsec
else
  raise EMsgrTimestampError.CreateFmt('GetTimestamp: Cannot obtain time stamp (%d).',[errno]);
{$ELSE}
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise EMsgrTimestampError.CreateFmt('GetTimestamp: Cannot obtain time stamp (%d).',[GetLastError]);
{$ENDIF}
Result := Result and $7FFFFFFFFFFFFFFF; // mask out sign bit
end;

//------------------------------------------------------------------------------

Function GetElapsedMillis(StartTime: TMsgrTimestamp): UInt32;
var
  CurrentTime:  TMsgrTimestamp;
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
      raise EMsgrTimestampError.CreateFmt('GetElapsedMillis: Failed to obtain timer frequency (%d).',[GetLastError]);
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

Function BuildMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): TMsgrMessageOut;
begin
Result.Recipient := Recipient;
Result.Priority := Priority;
Result.Parameter1 := P1;
Result.Parameter2 := P2;
Result.Parameter3 := P3;
Result.Parameter4 := P4;
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
                               TMsgrMessageVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMsgrMessageVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMsgrMessageVector - protected methods
-------------------------------------------------------------------------------}

Function TMsgrMessageVector.GetItem(Index: Integer): TMsgrMessage;
begin
Result := TMsgrMessage(GetItemPtr(Index)^);
end;

//------------------------------------------------------------------------------

procedure TMsgrMessageVector.SetItem(Index: Integer; Value: TMsgrMessage);
begin
SetItemPtr(Index,@Value);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{
  Because messages are traversed from high index to low, the sorting is done in
  reversed order.
}
Result := 0;
If Assigned(TMsgrMessage(Item1^).SendParam) xor Assigned(TMsgrMessage(Item2^).SendParam) then
  begin
    If Assigned(TMsgrMessage(Item1^).SendParam) then
      Result := +1    // first is assigned, second is not => change order
    else
      Result := -1;   // first in not assigned, second is => keep order
  end
else
  begin
  {
    Both are assigned or not assigned, decide upon other parameters - first
    take priority (must go up), then Timestamp (down).
  }
    If TMsgrMessage(Item1^).Priority > TMsgrMessage(Item2^).Priority then
      Result := +1
    else If TMsgrMessage(Item1^).Priority < TMsgrMessage(Item2^).Priority then
      Result := -1
    else
      begin
        If TMsgrMessage(Item1^).Timestamp < TMsgrMessage(Item2^).Timestamp then
          Result := +1
        else If TMsgrMessage(Item1^).Timestamp > TMsgrMessage(Item2^).Timestamp then
          Result := -1;
      end; 
  end;
end;

{-------------------------------------------------------------------------------
    TMsgrMessageVector - public methods
-------------------------------------------------------------------------------}

constructor TMsgrMessageVector.Create;
begin
inherited Create(SizeOf(TMsgrMessage));
ShrinkMode := smKeepCap;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMsgrMessageVector.Create(Memory: Pointer; Count: Integer);
begin
inherited Create(Memory,Count,SizeOf(TMsgrMessage));
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.First: TMsgrMessage;
begin
Result := TMsgrMessage(inherited First^);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Last: TMsgrMessage;
begin
Result := TMsgrMessage(inherited Last^);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.IndexOf(Item: TMsgrMessage): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Add(Item: TMsgrMessage): Integer;
begin
Result := inherited Add(@Item);
end;

//------------------------------------------------------------------------------

procedure TMsgrMessageVector.Insert(Index: Integer; Item: TMsgrMessage);
begin
inherited Insert(Index,@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Remove(Item: TMsgrMessage): Integer;
begin
Result := inherited Remove(@Item);
end;

//------------------------------------------------------------------------------

Function TMsgrMessageVector.Extract(Item: TMsgrMessage): TMsgrMessage;
var
  TempPtr:  Pointer;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := TMsgrMessage(TempPtr^)
else
  FillChar(Addr(Result)^,SizeOf(Result),0);
end;


{===============================================================================
--------------------------------------------------------------------------------
                           TMsgrBufferedMessagesVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMsgrBufferedMessageVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMsgrBufferedMessageVector - protected methods
-------------------------------------------------------------------------------}

Function TMsgrBufferedMessageVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{
  TMsgrBufferedMessageVector is sorted only by recipients, so there is no need
  for other comparisons.
}
Result := Integer(TMsgrMessage(Item2^).Recipient) - Integer(TMsgrMessage(Item1^).Recipient);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TMsgrWaiter
--------------------------------------------------------------------------------
===============================================================================}
const
  MSGR_FLAG_SENTMESSAGE   = $00000001;
  MSGR_FLAG_POSTEDMESSAGE = $00000002;

  MSGR_LOCALFLAG_RELEASED  = $00000001;
  MSGR_LOCALFLAG_PROCESSED = $00000002;

{===============================================================================
    TMsgrWaiter - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMsgrWaiter - protected methods
-------------------------------------------------------------------------------}

procedure TMsgrWaiter.Initialize;
begin
fInterlock := TCriticalSectionRTL.Create;
fEvent := TEvent.Create(False,False);
fFlags := 0;
end;

//------------------------------------------------------------------------------

procedure TMsgrWaiter.Finalize;
begin
fEvent.Free;
fInterlock.Free;
end;

{-------------------------------------------------------------------------------
    TMsgrWaiter - public methods
-------------------------------------------------------------------------------}

constructor TMsgrWaiter.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TMsgrWaiter.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TMsgrWaiter.SentMessageReceived;
begin
fInterlock.Lock;
try
  fFlags := fFlags or MSGR_FLAG_SENTMESSAGE;
  fEvent.Unlock; 
finally
  fInterlock.Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TMsgrWaiter.PostedMessageReceived;
begin
fInterlock.Lock;
try
  fFlags := fFlags or MSGR_FLAG_POSTEDMESSAGE;
  fEvent.Unlock;
finally
  fInterlock.Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TMsgrWaiter.ReleaseSendMessage(var LocalFlags: UInt32; Processed: Boolean);
begin
fInterlock.Lock;
try
  If Processed then
    LocalFlags := LocalFlags or MSGR_LOCALFLAG_RELEASED or MSGR_LOCALFLAG_PROCESSED
  else
    LocalFlags := LocalFlags or MSGR_LOCALFLAG_RELEASED;
  fEvent.Unlock;
finally
  fInterlock.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TMsgrWaiter.WaitForMessage(Timeout: UInt32): TMsgrWaiterResult;
var
  StartTime:        TMsgrTimestamp;
  TimeoutRemaining: UInt32;
  ElapsedMillis:    UInt32;
  ExitWait:         Boolean;

  Function CheckMessages: Boolean;
  begin
    fInterlock.Lock;
    try
      If (fFlags and (MSGR_FLAG_SENTMESSAGE or MSGR_FLAG_POSTEDMESSAGE)) <> 0 then
        begin
          fFlags := fFlags and not(MSGR_FLAG_SENTMESSAGE or MSGR_FLAG_POSTEDMESSAGE);
          Result := True;
        end
      else Result := False;
    finally
      fInterlock.Unlock;
    end;
  end;

begin
StartTime := GetTimestamp;
TimeoutRemaining := Timeout;
If not CheckMessages then
  repeat
    ExitWait := True;
    Result := wtrMessage;
    case fEvent.Wait(TimeoutRemaining) of
      wrSignaled: If not CheckMessages then
                    begin
                      // recalcualte timeout
                      If Timeout <> INFINITE then
                        begin
                          ElapsedMillis := GetElapsedMillis(StartTime);
                          If Timeout <= ElapsedMillis then
                            begin
                              Result := wtrTimeout;
                              Break{repeat};
                            end
                          else TimeoutRemaining := Timeout - ElapsedMillis;
                        end;
                      ExitWait := False;
                    end;
      wrTimeout:  Result := wtrTimeout;
    else
      Result := wtrError;
    end;
  until ExitWait
else Result := wtrMessage;
end;

//------------------------------------------------------------------------------

Function TMsgrWaiter.WaitForRelease(var LocalFlags: UInt32): TMsgrWaiterResults;

  Function CheckLocalFlags(var WaitResult: TMsgrWaiterResults): Boolean;
  begin
    Result := False;
    If (LocalFlags and MSGR_LOCALFLAG_RELEASED) <> 0 then
      begin
        Include(WaitResult,wtrSendReleased);
        Result := True;
      end;
    If (LocalFlags and MSGR_LOCALFLAG_PROCESSED) <> 0 then
      Include(WaitResult,wtrSendProcessed);
  end;

var
  Released: Boolean;
begin
Result := [];
fInterlock.Lock;
try
  Released := CheckLocalFlags(Result);
finally
  fInterlock.Unlock;
end;
If not Released then
  begin
    fEvent.Wait(INFINITE);
    fInterlock.Lock;
    try
      If (fFlags and MSGR_FLAG_SENTMESSAGE) <> 0 then
        begin
          Include(Result,wtrSentMessage);
          fFlags := fFlags and not MSGR_FLAG_SENTMESSAGE;
        end;
      CheckLocalFlags(Result);
    finally
      fInterlock.Unlock;
    end;
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TMessangerEndpoint
--------------------------------------------------------------------------------
===============================================================================}
const
  MSGR_SEND_LEVEL_MAX = 128;

type
  TMsgrSendRecord = record
    Flags:  UInt32;
    Waiter: TMsgrWaiter;
  end;
  PMsgrSendRecord = ^TMsgrSendRecord;

{===============================================================================
    TMessangerEndpoint - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMessangerEndpoint - protected methods
-------------------------------------------------------------------------------}

Function TMessangerEndpoint.GetMessageCount: Integer;
begin
Result := fReceivedSentMessages.Count + fReceivedPostedMessages.Count;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.GetMessage(Index: Integer): TMsgrMessage;
begin
If (Index >= 0) and (Index < (fReceivedSentMessages.Count + fReceivedPostedMessages.Count)) then
  begin
    If Index > fReceivedSentMessages.HighIndex then
      Result := fReceivedPostedMessages[Index - fReceivedSentMessages.Count]
    else
      Result := fReceivedSentMessages[Index];
  end
else raise EMsgrIndexOutOfBounds.CreateFmt('TMessangerEndpoint.GetMessage: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.ReceiveSentMessages(Messages: PMsgrMessage; Count: Integer);
begin
If Count > 0 then
  begin
    fIncomingSynchronizer.Lock;
    try
      fIncomingSentMessages.Append(Messages,Count);
      fWaiter.SentMessageReceived;
    finally
      fIncomingSynchronizer.Unlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.ReceivePostedMessages(Messages: PMsgrMessage; Count: Integer);
begin
If Count > 0 then
  begin
    fIncomingSynchronizer.Lock;
    try
      fIncomingPostedMessages.Append(Messages,Count);
      fWaiter.PostedMessageReceived;
    finally
      fIncomingSynchronizer.Unlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.SentMessagesFetch;
begin
fIncomingSynchronizer.Lock;
try
  fReceivedSentMessages.Append(fIncomingSentMessages);
  fIncomingSentMessages.Clear;
finally
  fIncomingSynchronizer.Unlock;
end;
fReceivedSentMessages.Sort;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.SentMessagesDispatch;
var
  TempMsg:  TMsgrMessage;
  Flags:    TMsgrDispatchFlags;
  i:        Integer;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    while fReceivedSentMessages.Count > 0 do
      begin
        // create local copy of the processed message
        TempMsg := fReceivedSentMessages[fReceivedSentMessages.HighIndex];
        // prepare flags
        Flags := [mdfSentMessage];
        If fAutoCycle then
          Include(Flags,mdfAutoCycle);
        If fSendBlocked then
          Include(Flags,mdfSendBlocked);
      {
        Delete the message now - another dispatch can occur inside of DoMessage
        call, which can change the fReceivedSentMessages list under our hands!
      }
        fReceivedSentMessages.Delete(fReceivedSentMessages.HighIndex);
        // call event
        DoMessage(MsgToMsgIn(TempMsg),Flags);
        // release sender
        PMsgrSendRecord(TempMsg.SendParam)^.Waiter.ReleaseSendMessage(PMsgrSendRecord(TempMsg.SendParam)^.Flags,True);
        // process output flags
        fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
        If mdfStopDispatching in Flags then
          Break{For i};
      end;
  end
else
  begin
    // no handler assigned, release waiting senders and remove all messages
    For i := fReceivedSentMessages.HighIndex downto fReceivedSentMessages.LowIndex do
      PMsgrSendRecord(fReceivedSentMessages[i].SendParam)^.Waiter.ReleaseSendMessage(
        PMsgrSendRecord(fReceivedSentMessages[i].SendParam)^.Flags,False);
    fReceivedSentMessages.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.PostedMessagesDispatch;
var
  TempMsg:  TMsgrMessage;
  Flags:    TMsgrDispatchFlags;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    while fReceivedPostedMessages.Count > 0 do
      begin
        TempMsg := fReceivedPostedMessages[fReceivedPostedMessages.HighIndex];
        Flags := [];
        If fAutoCycle then
          Include(Flags,mdfAutoCycle);
        If fSendBlocked then
          Include(Flags,mdfSendBlocked);          
        fReceivedPostedMessages.Delete(fReceivedPostedMessages.HighIndex);
        DoMessage(MsgToMsgIn(TempMsg),Flags);
        fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
        If mdfStopDispatching in Flags then
          Break{For i};
      end;
  end
else fReceivedPostedMessages.Clear;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.UndeliveredDispatch;
var
  i:      Integer;
  Flags:  TMsgrDispatchFlags;
begin
fSendBlocked := True;
try
  If Assigned(fOnUndeliveredEvent) or Assigned(fOnUndeliveredCallback) then
    begin
      fUndeliveredMessages.Sort;
      For i := fUndeliveredMessages.HighIndex downto fUndeliveredMessages.LowIndex do
        begin
          // prepare flags
          Flags := [mdfUndeliveredMessage,mdfSendBlocked];
          If fAutoCycle then
            Include(Flags,mdfAutoCycle);
          // call event
          DoUndelivered(MsgToMsgOut(fUndeliveredMessages[i]),Flags);
          // process output flags
          fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
          If mdfStopDispatching in Flags then
            Break{For i};
        end;
    end;
  fUndeliveredMessages.Clear;
finally
  fSendBlocked := False;
end;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.DirectDispatch(Msg: TMsgrMessage): Boolean;
var
  Flags:  TMsgrDispatchFlags;
begin
If Assigned(fOnMessageEvent) or Assigned(fOnMessageCallback) then
  begin
    // prepare flags
    Flags := [mdfSentMessage,mdfDirectDispatch];
    If fAutoCycle then
      Include(Flags,mdfAutoCycle);
    If fSendBlocked then
      Include(Flags,mdfSendBlocked);
    // call event
    DoMessage(MsgToMsgIn(Msg),Flags);
    // process output flags
    fAutoCycle := fAutoCycle and (mdfAutoCycle in Flags);
    Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.DoMessage(Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags);
begin
If Assigned(fOnMessageEvent) then
  fOnMessageEvent(Self,Msg,Flags)
else If Assigned(fOnMessageCallback) then
  fOnMessageCallback(Self,Msg,Flags);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.DoUndelivered(Msg: TMsgrMessageOut; var Flags: TMsgrDispatchFlags);
begin
If Assigned(fOnUndeliveredEvent) then
  fOnUndeliveredEvent(Self,Msg,Flags)
else If Assigned(fOnUndeliveredCallback) then
  fOnUndeliveredCallback(Self,Msg,Flags);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.DoDestroying;
begin
If Assigned(fOnDestroyingEvent) then
  fOnDestroyingEvent(Self)
else If Assigned(fOnDestroyingCallback) then
  fOnDestroyingCallback(Self);
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.Initialize(EndpointID: TMsgrEndpointID; Messanger: TObject);
begin
fVectorsReady := False;
fWorkingThread := 0;
fMessanger := Messanger;
fEndpointID := EndpointID;
fAutoBuffSend := True;
fSendLevelMax := MSGR_SEND_LEVEL_MAX;
// runtime variables
fSendLevel := 0;
fAutoCycle := False;
fSendBlocked := False;
// synchronizers
fIncomingSynchronizer := TCriticalSectionRTL.Create;
fWaiter := TMsgrWaiter.Create;
// message storage vectors
fIncomingSentMessages := TMsgrMessageVector.Create;
fIncomingPostedMessages := TMsgrMessageVector.Create;
fReceivedSentMessages := TMsgrMessageVector.Create;
fReceivedPostedMessages := TMsgrMessageVector.Create;
fBufferedMessages := TMsgrBufferedMessageVector.Create;
fUndeliveredMessages := TMsgrMessageVector.Create;
fVectorsReady := True;
// events
fOnMessageEvent := nil;
fOnMessageCallback := nil;
fOnUndeliveredEvent := nil;
fOnUndeliveredCallback := nil;
fOnDestroyingEvent := nil;
fOnDestroyingCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.Finalize;
begin
// prevent sending of new messages
fSendBlocked := True;
// remove self from messanger (prevents receiving of new messages)
If Assigned(fMessanger) then
  TMessanger(fMessanger).RemoveEndpoint(fEndpointID);
If fVectorsReady then
  begin
    // fetch messages and give user a chance to process them
    FetchMessages;
    DispatchMessages; // clears the messages if handler is not assigned
    // dispatch buffered messages as undelivered
    fUndeliveredMessages.Assign(fBufferedMessages);
    fBufferedMessages.Clear;
    UndeliveredDispatch;
  end;
// call destroying event
DoDestroying;
// free vectors
fUndeliveredMessages.Free;
fBufferedMessages.Free;
fReceivedPostedMessages.Free;
fReceivedSentMessages.Free;
fIncomingPostedMessages.Free;
fIncomingSentMessages.Free;
// free synchronizers
fWaiter.Free;
fIncomingSynchronizer.Free;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.MsgOutToMsg(Msg: TMsgrMessageOut; SendParamPtr: PMsgrSendParam): TMsgrMessage;
begin
Result.Sender := fEndpointID;
Result.Recipient := Msg.Recipient;
Result.Priority := Msg.Priority;
Result.Timestamp := GetTimestamp;
Result.Parameter1 := Msg.Parameter1;
Result.Parameter2 := Msg.Parameter2;
Result.Parameter3 := Msg.Parameter3;
Result.Parameter4 := Msg.Parameter4;
Result.SendParam := SendParamPtr;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.MsgToMsgIn(Msg: TMsgrMessage): TMsgrMessageIn;
begin
Result.Sender := Msg.Sender;
Result.Parameter1 := Msg.Parameter1;
Result.Parameter2 := Msg.Parameter2;
Result.Parameter3 := Msg.Parameter3;
Result.Parameter4 := Msg.Parameter4;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.MsgToMsgOut(Msg: TMsgrMessage): TMsgrMessageOut;
begin
Result.Recipient := Msg.Recipient;
Result.Priority := Msg.Priority;
Result.Parameter1 := Msg.Parameter1;
Result.Parameter2 := Msg.Parameter2;
Result.Parameter3 := Msg.Parameter3;
Result.Parameter4 := Msg.Parameter4;
end;

{-------------------------------------------------------------------------------
    TMessangerEndpoint - public methods
-------------------------------------------------------------------------------}

constructor TMessangerEndpoint.Create(EndpointID: TMsgrEndpointID; Messanger: TObject);
begin
inherited Create;
Initialize(EndpointID,Messanger);
end;

//------------------------------------------------------------------------------

destructor TMessangerEndpoint.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.ThreadInit;
begin
fWorkingThread := {$IFDEF Windows}GetCurrentThreadID{$ELSE}gettid{$ENDIF};
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.SendMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean;
begin
Result := SendMessage(BuildMessage(Recipient,P1,P2,P3,P4,Priority));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMessangerEndpoint.SendMessage(Msg: TMsgrMessageOut): Boolean;
var
  SendRecord:   TMsgrSendRecord;
  ExitWait:     Boolean;
  WaiterResult: TMsgrWaiterResults;
begin
Result := False;
If not fSendBlocked then
  begin
    If fAutoBuffSend then
      PostBufferedMessages;
    If (fSendLevel < fSendLevelMax) and (Msg.Recipient <> MSGR_ID_BROADCAST) then
      begin
        Inc(fSendLevel);
        try
          If Msg.Recipient <> fEndpointID then
            begin
              SendRecord.Flags := 0;
              SendRecord.Waiter := fWaiter;
              case TMessanger(fMessanger).SendMessage(MsgOutToMsg(Msg,@SendRecord)) of
                msrSent:
                  repeat
                    WaiterResult := fWaiter.WaitForRelease(SendRecord.Flags);
                    If wtrSentMessage in WaiterResult then
                      begin
                        // some messages were received, dispatch the sent ones
                        SentMessagesFetch;
                        SentMessagesDispatch;
                      end;
                    ExitWait := wtrSendReleased in WaiterResult;
                    Result := wtrSendProcessed in WaiterResult;
                  until ExitWait;
                msrReleased:
                  Result := False;
                msrProcessed:
                  Result := True;
              end;
              SendRecord.Waiter := nil;
            end
          else Result := DirectDispatch(MsgOutToMsg(Msg,nil));
        finally
          Dec(fSendLevel);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.PostMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL): Boolean;
begin
Result := PostMessage(BuildMessage(Recipient,P1,P2,P3,P4,Priority));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TMessangerEndpoint.PostMessage(Msg: TMsgrMessageOut): Boolean;
begin
If not fSendBlocked then
  begin
    If fAutoBuffSend then
      PostBufferedMessages;
    Result := TMessanger(fMessanger).PostMessage(MsgOutToMsg(Msg,nil));
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.BufferMessage(Recipient: TMsgrEndpointID; P1,P2,P3,P4: TMsgrParam; Priority: TMsgrPriority = MSGR_PRIORITY_NORMAL);
begin
BufferMessage(BuildMessage(Recipient,P1,P2,P3,P4,Priority));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TMessangerEndpoint.BufferMessage(Msg: TMsgrMessageOut);
begin
fBufferedMessages.Add(MsgOutToMsg(Msg,nil));
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.PostBufferedMessages;
begin
If not fSendBlocked then
  begin
    If fBufferedMessages.Count > 0 then
      begin
        fBufferedMessages.Sort;
        TMessanger(fMessanger).PostBufferedMessages(fBufferedMessages,fUndeliveredMessages);
        fBufferedMessages.Clear;
      end;
    UndeliveredDispatch;
  end;
end;

//------------------------------------------------------------------------------

Function TMessangerEndpoint.WaitForMessage(Timeout: UInt32): TMsgrWaitResult;
begin
case fWaiter.WaitForMessage(Timeout) of
  wtrMessage:  Result := mwrMessage;
  wtrTimeout:  Result := mwrTimeout;
else
  Result := mwrError;
end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.FetchMessages;
begin
fIncomingSynchronizer.Lock;
try
  fReceivedSentMessages.Append(fIncomingSentMessages);
  fReceivedPostedMessages.Append(fIncomingPostedMessages);
  fIncomingSentMessages.Clear;
  fIncomingPostedMessages.Clear;
finally
  fIncomingSynchronizer.Unlock;
end;
fReceivedSentMessages.Sort;
fReceivedPostedMessages.Sort;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.DispatchMessages;
begin
SentMessagesDispatch;
PostedMessagesDispatch;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.ClearMessages;
var
  i:  Integer;
begin
// release waiting senders
For i := fReceivedSentMessages.HighIndex downto fReceivedSentMessages.LowIndex do
  PMsgrSendRecord(fReceivedSentMessages[i].SendParam)^.Waiter.ReleaseSendMessage(
    PMsgrSendRecord(fReceivedSentMessages[i].SendParam)^.Flags,False);
fReceivedSentMessages.Clear;
fReceivedPostedMessages.Clear;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.Cycle(Timeout: UInt32);
begin
FetchMessages;
DispatchMessages;
If WaitForMessage(Timeout) = mwrMessage then
  begin
    FetchMessages;
    DispatchMessages;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.AutoCycle(Timeout: UInt32);
begin
If not fAutoCycle then
  begin
    fAutoCycle := True;
    while fAutoCycle do
      Cycle(Timeout);
  end;
end;

//------------------------------------------------------------------------------

procedure TMessangerEndpoint.BreakAutoCycle;
begin
fAutoCycle := False;
end;


{==============================================================================}
{------------------------------------------------------------------------------}
{                                  TMessanger                                  }
{------------------------------------------------------------------------------}
{==============================================================================}
{===============================================================================
    TMessanger - class implementation
===============================================================================}
{------------------------------------------------------------------------------
    TMessanger - protected methods
-------------------------------------------------------------------------------}

Function TMessanger.GetEndpointCapacity: Integer;
begin
Result := Length(fEndpoints);
end;

//------------------------------------------------------------------------------

Function TMessanger.GetEndpointCount: Integer;
var
  i:  Integer;
begin
fSynchronizer.ReadLock;
try
  Result := 0;
  For i := Low(fEndpoints) to High(fEndpoints) do
    If Assigned(fEndpoints[i]) then Inc(Result);
finally
  fSynchronizer.ReadUnlock;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.GetEndpoint(Index: Integer): TMessangerEndpoint;
begin
Result := nil;
fSynchronizer.ReadLock;
try
  If (Index >= Low(fEndpoints)) and (Index <= High(fEndpoints)) then
    Result := fEndpoints[Index]
  else
    raise EMsgrIndexOutOfBounds.CreateFmt('TMessanger.GetEndpoint: Index (%d) out of bounds.',[Index]);
finally
  fSynchronizer.ReadUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure TMessanger.RemoveEndpoint(EndpointID: TMsgrEndpointID);
begin
fSynchronizer.WriteLock;
try
  If EndpointID <= High(fEndpoints) then
    fEndpoints[EndpointID] := nil
  else
    raise EMsgrIndexOutOfBounds.CreateFmt('TMessanger.RemoveEndpoint: EndpointID (%d) out of bounds.',[EndpointID]);
finally
  fSynchronizer.WriteUnlock;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.SendMessage(Msg: TMsgrMessage): TMsgrSendResult;
begin
Result := msrFail;
fSynchronizer.ReadLock;
try
  If Msg.Recipient <= High(fEndpoints) then
    If Assigned(fEndpoints[Msg.Recipient]) then
      begin
        If (fEndpoints[Msg.Recipient].fWorkingThread = fEndpoints[Msg.Sender].fWorkingThread) and
          (fEndpoints[Msg.Recipient].fWorkingThread <> 0) and (fEndpoints[Msg.Sender].fWorkingThread <> 0) then
          begin
            If fEndpoints[Msg.Recipient].DirectDispatch(Msg) then
              Result := msrProcessed
            else
              Result := msrReleased;
          end
        else
          begin
            fEndpoints[Msg.Recipient].ReceiveSentMessages(@Msg,1);
            Result := msrSent;
          end;
      end;
finally
  fSynchronizer.ReadUnlock;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.PostMessage(Msg: TMsgrMessage): Boolean;
var
  i:  Integer;
begin
Result := False;
fSynchronizer.ReadLock;
try
  If Msg.Recipient = MSGR_ID_BROADCAST then
    begin
      For i := Low(fEndpoints) to High(fEndpoints) do
        If Assigned(fEndpoints[i]) then
          begin
            fEndpoints[i].ReceivePostedMessages(@Msg,1);
            Result := True;
          end;
    end
  else
    begin
      If Msg.Recipient <= High(fEndpoints) then
        If Assigned(fEndpoints[Msg.Recipient]) then
          begin
            fEndpoints[Msg.Recipient].ReceivePostedMessages(@Msg,1);
            Result := True;
          end;
    end;
finally
  fSynchronizer.ReadUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure TMessanger.PostBufferedMessages(Messages,Undelivered: TMsgrMessageVector);

  Function PostMessages(Start,Count: Integer): Boolean;
  var
    i:  Integer;
  begin
    Result := False;
    If Messages[Start].Recipient = MSGR_ID_BROADCAST then
      begin
        For i := Low(fEndpoints) to High(fEndpoints) do
          If Assigned(fEndpoints[i]) then
            begin
              fEndpoints[i].ReceivePostedMessages(Messages.Pointers[Start],Count);
              Result := True;
            end;
      end
    else
      begin
        If Messages[Start].Recipient <= High(fEndpoints) then
          If Assigned(fEndpoints[Messages[Start].Recipient]) then
            begin
              fEndpoints[Messages[Start].Recipient].ReceivePostedMessages(Messages.Pointers[Start],Count);
              Result := True;
            end;
      end;
  end;

var
  Start:  Integer;
  Count:  Integer;
  i:      Integer;  
begin
If Messages.Count > 0 then
  begin
    fSynchronizer.ReadLock;
    try
      // the messages are sorted by recipient id
      Start := Messages.LowIndex;
      while Start <= Messages.HighIndex do
        begin
          Count := 1;
          For i := Succ(Start) to Messages.HighIndex do
            If Messages[i].Recipient = Messages[Start].Recipient then
              Inc(Count)
            else
              Break{For i};
          If not PostMessages(Start,Count) then
            Undelivered.Append(Messages.Pointers[Start],Count);
          Start := Start + Count;
        end;
    finally
      fSynchronizer.ReadUnlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMessanger.Initialize(EndpointCapacity: TMsgrEndpointID);
begin
// High(TMsgrEndpointID) = $FFFF(65535) is reserved for broadcast
If EndpointCapacity < High(TMsgrEndpointID) then
  SetLength(fEndpoints,EndpointCapacity)
else
  raise EMsgrInvalidValue.CreateFmt('TMessanger.Initialize: Required capacity (%d) is too high.',[EndpointCapacity]);
fSynchronizer := TMultiReadExclusiveWriteSynchronizerRTL.Create;
end;

//------------------------------------------------------------------------------

procedure TMessanger.Finalize;
begin
If EndpointCount > 0 then
  raise EMsgrInvalidState.Create('TMessanger.Finalize: Not all endpoints were freed.');
fSynchronizer.Free;
end;

{-------------------------------------------------------------------------------
    TMessanger - public methods
-------------------------------------------------------------------------------}

constructor TMessanger.Create(EndpointCapacity: TMsgrEndpointID = 128);
begin
inherited Create;
Initialize(EndpointCapacity);
end;

//------------------------------------------------------------------------------

destructor TMessanger.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TMessanger.IDAvailable(EndpointID: TMsgrEndpointID): Boolean;
begin
fSynchronizer.ReadLock;
try
  If EndpointID <= High(fEndpoints) then
    Result := not Assigned(fEndpoints[EndpointID])
  else
    Result := False;
finally
  fSynchronizer.ReadUnlock;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.CreateEndpoint: TMessangerEndpoint;
var
  i,Idx:  Integer;
begin
Result := nil;
fSynchronizer.WriteLock;
try
  Idx := -1;
  For i := Low(fEndpoints) to High(fEndpoints) do
    If not Assigned(fEndpoints[i]) then
      begin
        Idx := i;
        Break{For i};
      end;
  If Idx >= 0 then
    begin
      Result := TMessangerEndpoint.Create(TMsgrEndpointID(Idx),Self);
      fEndpoints[Idx] := Result;
    end
  else raise EMsgrNoResources.Create('TMessanger.CreateEndpoint: No endpoint slot available.');
finally
  fSynchronizer.WriteUnlock;
end;
end;

//------------------------------------------------------------------------------

Function TMessanger.CreateEndpoint(EndpointID: TMsgrEndpointID): TMessangerEndpoint;
begin
Result := nil;
fSynchronizer.WriteLock;
try
  If EndpointID <= High(fEndpoints) then
    begin
      If not Assigned(fEndpoints[EndpointID]) then
        begin
          Result := TMessangerEndpoint.Create(EndpointID,Self);
          fEndpoints[EndpointID] := Result;
        end
      else raise EMsgrInvalidValue.CreateFmt('TMessanger.CreateEndpoint: Requested endpoint ID (%d) is already taken.',[EndpointID]);
    end
  else raise EMsgrNoResources.CreateFmt('TMessanger.CreateEndpoint: Requested endpoint ID (%d) is not allocated.',[EndpointID]);
finally
  fSynchronizer.WriteUnlock;
end;
end;

end.
