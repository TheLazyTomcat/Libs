{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinMsgComm

    Small library for interprocess communication based on Windows message queue
    system (but also works on Linux). Intended only for exchange of small data
    and/or for notifications.

    As there is no Windows-like messaging system in Linux (obviously),
    SimpleMessages library is used there instead. Refer to that library for
    potential limitations.

      NOTE - SimpleMessages can also be used on Windows OS if you undefine
             symbol UseWindowsMessages.

    With one exception (see further), it is necessary to repeatedly call method
    Update, as it manages processing of incoming messages/data and their
    dispatching to events, and is also responsible for internal workings of the
    communication. Without this, the communication object might become
    unresponsive and even block other threads that are communicating with it.

    Only when Windows messaging system is used for communication and when the
    object is running in a main thread of an GUI application, the message
    processing and dispatching is done automatically by the Application object.

  Version 2.0 (2022-10-24)

  Last change 2023-05-01

  ©2015-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.WinMsgComm

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreaming    - github.com/TheLazyTomcat/Lib.BinaryStreaming
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    BitVector          - github.com/TheLazyTomcat/Lib.BitVector
    CRC32              - github.com/TheLazyTomcat/Lib.CRC32
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
  * LinSyncObjs        - github.com/TheLazyTomcat/Lib.LinSyncObjs
  * ListSorters        - github.com/TheLazyTomcat/Lib.ListSorters
  * MemVector          - github.com/TheLazyTomcat/Lib.MemVector
  * MulticastEvent     - github.com/TheLazyTomcat/Lib.MulticastEvent
  * NamedSharedItems   - github.com/TheLazyTomcat/Lib.NamedSharedItems
  * SHA1               - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex
  * SimpleMessages     - github.com/TheLazyTomcat/Lib.SimpleMessages
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
  * UtilityWindow      - github.com/TheLazyTomcat/Lib.UtilityWindow
  * WinSyncObjs        - github.com/TheLazyTomcat/Lib.WinSyncObjs
  * WndAlloc           - github.com/TheLazyTomcat/Lib.WndAlloc

  SimpleCPUID library might not be needed, depending on defined symbols in
  other libraries.

  Libraries MulticastEvent, UtilityWindow ans WndAlloc are required only when
  compiling for Windows OS and symbol UseWindowsMessages is defined.

  Libraries UInt64Utils and WinSyncObjs are required only when compiling for
  Windows OS and symbol UseWindowsMessages is not defined.

  Libraries ListSorters, MemVector, NamedSharedItems, SHA1 and SimpleMessages
  are required only when compiling for Linux OS or when UseWindowsMessages
  symbol is not defined.

  Libraries LinSyncObjs and SimpleFutex are required only when compiling for
  Linux OS.

===============================================================================}
unit WinMsgComm;

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

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
  {$MODESWITCH ClassicProcVars+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  UseWindowsMessages

  When defined, windows messaging system (SendMessage, GetMessage, ...) is used
  to send and receive messages whenever it is available - that is, on Windows
  operating system.

  When not defined, the data are being sent using SimpleMessages library.
  SimpleMessages is also used when compiling for operating system other than
  Windows.

  Has no effect when compiling for non-Windows operating systems.

  By default enabled.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol WinMsgComm_UseWindowsMessages_Off.
}
{$DEFINE UseWindowsMessages}
{$IFDEF WinMsgComm_UseWindowsMessages_Off}
  {$UNDEF UseWindowsMessages}
{$ENDIF}

{
  ConserveMemory

  Normally, communication objects are using large lookup table to directly
  translate connection ID to its index in a list of known connections. This
  table reguires about 256KiB of memory, and such memory consumption for each
  object migh be undesirable - if so, define this symbol and the table will be
  omitted.

  But be warned, the memory footprint goes down, but the communication will
  become slower, especially when large number of endpoints is connected.

  By default undefined.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol WinMsgComm_ConserveMemory_On.
}
{$UNDEF ConserveMemory}
{$IFDEF WinMsgComm_ConserveMemory_On}
  {$DEFINE ConserveMemory}
{$ENDIF}

{
  SimpleMessagesHigherLimits

  When SimpleMessages library is used for communication, it is done using its
  default limits. These limits can be too low for some uses. You can define
  this symbol in such cases to increase the limits (to 1024 clients and 10240
  messages).

  Has no effect when Windows messages are used for communication.

  By default undefined.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol WinMsgComm_SimpleMessagesHigherLimits_On.
}
{$UNDEF SimpleMessagesHigherLimits}
{$IFDEF WinMsgComm_SimpleMessagesHigherLimits_On}
  {$DEFINE SimpleMessagesHigherLimits}
{$ENDIF}

{
  SimpleMessagesNoLimits

  Similar to SimpleMessagesHigherLimits, but it sets client limit to its
  technical maximum (65534) and messages limit to 655340.
  Note that, if both SimpleMessagesHigherLimits and SimpleMessagesNoLimits are
  defined, then this symbol takes precendence.

    WARNING - when this symbol is defined, then this library allocates over
              35MiB of global shared memory for each used domain.

  By default undefined.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol WinMsgComm_SimpleMessagesNoLimits_On.
}
{$UNDEF SimpleMessagesNoLimits}
{$IFDEF WinMsgComm_SimpleMessagesNoLimits_On}
  {$DEFINE SimpleMessagesNoLimits}
{$ENDIF}

//------------------------------------------------------------------------------

// do not touch following...
{$IFNDEF Windows}
  {$UNDEF UseWindowsMessages}
{$ENDIF}

interface

uses
  {$IFDEF UseWindowsMessages}Windows, Messages,{$ENDIF} SysUtils,
  AuxTypes, AuxClasses, BitVector, CRC32, SharedMemoryStream,
  {$IFDEF UseWindowsMessages}UtilityWindow{$ELSE}SimpleMessages{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EWMCException = class(Exception);

  EWMCInvalidConnection = class(EWMCException);
  EWMCInvalidOperation  = class(EWMCException);
  EWMCInvalidValue      = class(EWMCException);
  EWMCIndexOutOfBounds  = class(EWMCException);
  EWMCOutOfResources    = class(EWMCException);  
  EWMCTooMuchData       = class(EWMCException);
  EWMCServerExists      = class(EWMCException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TWinMsgComm
--------------------------------------------------------------------------------
===============================================================================}
type
  TWMCConnectionID = UInt16;  // used in internal types, so it must be up here

{===============================================================================
    TWinMsgComm - internal types
===============================================================================}
type
  TWMCMessagePayload = UInt64;

  TWMCSharedDataID = UInt32;

  TWMCSystemID = {$IFDEF UseWindowsMessages}HWND{$ELSE}TSMClientID{$ENDIF};  

  TWMCGlobalData = packed record
    Flags:    UInt32;
    Counter:  UInt32; // only interlocked access
    IDPool: array[0..Pred(Succ(Integer(High(TWMCConnectionID))) div 8){8191}] of UInt8;
  end;
  PWMCGlobalData = ^TWMCGlobalData;

{$IFNDEF ConserveMemory}
  TWMCIDToIndexTable = array[TWMCConnectionID] of Integer;
{$ENDIF}

  TWMCTransaction = record
    DataPtr:  Pointer;
    DataSize: TMemSize;
    Position: TMemSize;
    CheckSum: TCRC32;
  end;

  TWMCConnectionData = record
    ConnectionID: TWMCConnectionID;
    Is32bit:      Boolean;
    SystemID:     TWMCSystemID;
    Transacting:  Boolean;
    Transaction:  TWMCTransaction;
  end;

{===============================================================================
    TWinMsgComm - public types
===============================================================================}
type
  TWMCMessageCode = UInt8;
  TWMCUserData    = Int8;

  TWMCConnection = record
    ConnectionID: TWMCConnectionID;
    Is32Bit:      Boolean;
    Transacting:  Boolean;
  end;

  TWMCValueType = (mvtBool,mvtUInt8,mvtInt8,mvtUInt16,mvtInt16,mvtUInt32,
                   mvtInt32,mvtUInt64,mvtInt64,mvtFloat32,mvtFloat64,mvtString,
                   mvtData);

  TWMCValue = record
    UserData:    TWMCUserData;
    StringValue: String;
    case ValueType: TWMCValueType of
      mvtBool:    (BoolValue:     ByteBool);
      mvtUInt8:   (UInt8Value:    UInt8);
      mvtInt8:    (Int8Value:     Int8);
      mvtUInt16:  (UInt16Value:   UInt16);
      mvtInt16:   (Int16Value:    Int16);
      mvtUInt32:  (UInt32Value:   UInt32);
      mvtInt32:   (Int32Value:    Int32);
      mvtUInt64:  (UInt64Value:   UInt64);
      mvtInt64:   (Int64Value:    Int64);
      mvtFloat32: (Float32Value:  Float32);
      mvtFloat64: (Float64Value:  Float64);
      mvtData:    (DataPtr:       Pointer;
                   DataSize:      TMemSize)
  end;

  TWMCValueEvent = procedure(Sender: TObject; SenderID: TWMCConnectionID; Value: TWMCValue; Sent: Boolean) of object;
  TWMCValueCallback = procedure(Sender: TObject; SenderID: TWMCConnectionID; Value: TWMCValue; Sent: Boolean);

const
  WMC_BROADCAST = TWMCConnectionID(High(TWMCConnectionID));

{===============================================================================
    TWinMsgComm - class declaration
===============================================================================}
type
  TWinMsgComm = class(TCustomListObject)
  protected
    fLargeDataThreshold:      TMemSize;
    fDomainName:              String;
    fGlobalDataShrdMem:       TSharedMemory;
    fIDPoolVector:            TBitVector;
    fConnectionID:            TWMCConnectionID;
    fConnections:             array of TWMCConnectionData;
    fConnectionCount:         Integer;
  {$IFNDEF ConserveMemory}
    fIDToIndexTable:          TWMCIDToIndexTable;
  {$ENDIF}
  {$IFDEF UseWindowsMessages}
    fWindowsMessageID:        UINT;
    fOwnsRecevingWindow:      Boolean;
    fReceivingWindow:         TUtilityWindow;
  {$ELSE}
    fMessagesClient:          TSimpleMessagesClient;
  {$ENDIF}
    fSystemID:                TWMCSystemID;
    fOnIncomingValueCallback: TWMCValueCallback;
    fOnIncomingValueEvent:    TWMCValueEvent;
    fOnConnectionCallback:    TNotifyCallback;
    fOnConnectionEvent:       TNotifyEvent;
    // getters, setters
    Function GetConnection(Index: Integer): TWMCConnection; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    Function ConnectionAdd(ConnectionID: TWMCConnectionID; Is32bit: Boolean; SystemID: TWMCSystemID): Integer; virtual;
    Function ConnectionRemove(ConnectionID: TWMCConnectionID): Integer; virtual;
    procedure ConnectionDelete(Index: Integer); virtual;
    // incoming transaction
    Function TransactionStart(Sender: TWMCConnectionID; DataSize: TMemSize): Boolean; virtual;
    Function TransactionBuff(Sender: TWMCConnectionID; Payload: TWMCMessagePayload; Size: TMemSize): Boolean; virtual;
    Function TransactionEnd(Sender: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; virtual;
    // incoming messages
    class Function DataToValue(var Value: TWMCValue; MessageCode: TWMCMessageCode; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
  {$IFDEF UseWindowsMessages}
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean; Sent: Boolean); virtual;
    Function ProcessCopyData(CopyDataStruct: TCopyDataStruct; Sent: Boolean): Boolean; virtual;
  {$ELSE}
    procedure HandleMessage(Sender: TObject; var Msg: TSMMessage; var Flags: TSMDispatchFlags); virtual;
  {$ENDIF}
    Function ProcessSharedData(Sender: TWMCConnectionID; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; virtual;
    Function ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; virtual;
    // outgoing messages
    Function SysSendSinglecast(SystemID: TWMCSystemID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Integer; virtual;
    Function SysSendBroadcast(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Integer; virtual;
    Function SysPostSinglecast(SystemID: TWMCSystemID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function SysPostBroadcast(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function SendMessageIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function PostMessageIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function SendMessageAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function PostMessageAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function SendMessageRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function PostMessageRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function Send8ByteQuantity(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    Function Post8ByteQuantity(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean; virtual;
    // note that there is, for various reasons, no data posting
  {$IFDEF UseWindowsMessages}
    Function SendCopyData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
  {$ENDIF}
    Function SendSharedData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
    Function SendTransactedData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
    Function TrySendTinyDataIdx(Index: Integer; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean; virtual;
    Function TryPostTinyDataIdx(Index: Integer; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean; virtual;
    Function TrySendTinyDataRcp(Recipient: TWMCConnectionID; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean; virtual;
    Function TryPostTinyDataRcp(Recipient: TWMCConnectionID; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean; virtual;
    Function SendDataIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
    Function SendDataAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
    Function SendDataRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean; virtual;
    // events firing
    procedure DoIncomingValue(SenderID: TWMCConnectionID; Value: TWMCValue; Sent: Boolean); virtual;
    procedure DoConnectionChange; virtual;
    // init/final
    procedure Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF}); virtual;
    procedure Finalize; virtual;
    // some utilities
    class Function SysParamUsableSize(RecipientIs32Bit: Boolean): TMemSize; virtual;
  public
    class Function MaxDataSize: TMemSize; virtual;
    class Function ValueTypeStr(ValueType: TWMCValueType): String; virtual;
    constructor Create(const DomainName: String = ''{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow = nil{$ENDIF});
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function ConnectionIndexOf(ConnectionID: TWMCConnectionID): Integer; virtual;
    Function ConnectionFind(ConnectionID: TWMCConnectionID; out Index: Integer): Boolean; virtual;
    Function ConnectionsCheck: Boolean; virtual;
    procedure Update(WaitForValue: Boolean = False); virtual;
    Function SendBool(Recipient: TWMCConnectionID; Value: Boolean; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostBool(Recipient: TWMCConnectionID; Value: Boolean; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendUInt8(Recipient: TWMCConnectionID; Value: UInt8; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostUInt8(Recipient: TWMCConnectionID; Value: UInt8; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendInt8(Recipient: TWMCConnectionID; Value: Int8; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostInt8(Recipient: TWMCConnectionID; Value: Int8; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendUInt16(Recipient: TWMCConnectionID; Value: UInt16; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostUInt16(Recipient: TWMCConnectionID; Value: UInt16; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendInt16(Recipient: TWMCConnectionID; Value: Int16; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostInt16(Recipient: TWMCConnectionID; Value: Int16; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendUInt32(Recipient: TWMCConnectionID; Value: UInt32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostUInt32(Recipient: TWMCConnectionID; Value: UInt32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendInt32(Recipient: TWMCConnectionID; Value: Int32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostInt32(Recipient: TWMCConnectionID; Value: Int32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendUInt64(Recipient: TWMCConnectionID; Value: UInt64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostUInt64(Recipient: TWMCConnectionID; Value: UInt64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendInt64(Recipient: TWMCConnectionID; Value: Int64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostInt64(Recipient: TWMCConnectionID; Value: Int64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendInteger(Recipient: TWMCConnectionID; Value: Integer; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostInteger(Recipient: TWMCConnectionID; Value: Integer; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendFloat32(Recipient: TWMCConnectionID; Value: Float32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostFloat32(Recipient: TWMCConnectionID; Value: Float32; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendFloat64(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostFloat64(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendFloat(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostFloat(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendString(Recipient: TWMCConnectionID; const Value: String; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostString(Recipient: TWMCConnectionID; const Value: String; UserData: TWMCUserData = 0): Boolean; virtual;
    Function SendData(Recipient: TWMCConnectionID; const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean; virtual;
    Function PostData(Recipient: TWMCConnectionID; const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean; virtual;
    property LargeDataThreshold: TMemSize read fLargeDataThreshold write fLargeDataThreshold;
    property DomainName: String read fDomainName;
    property ConnectionID: TWMCConnectionID read fConnectionID;
    property Connections[Index: Integer]: TWMCConnection read GetConnection; default;
    property OnIncomingValueCallback: TWMCValueCallback read fOnIncomingValueCallback write fOnIncomingValueCallback;
    property OnIncomingValueEvent: TWMCValueEvent read fOnIncomingValueEvent write fOnIncomingValueEvent;
    property OnIncomingValue: TWMCValueEvent read fOnIncomingValueEvent write fOnIncomingValueEvent;
    property OnConnectionChangeCallback: TNotifyCallback read fOnConnectionCallback write fOnConnectionCallback;
    property OnConnectionChangeEvent: TNotifyEvent read fOnConnectionEvent write fOnConnectionEvent;
    property OnConnectionChange: TNotifyEvent read fOnConnectionEvent write fOnConnectionEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TWinMsgCommPeer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommPeer - class declaration
===============================================================================}
type
  TWinMsgCommPeer = class(TWinMsgComm)
  protected
    Function ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; override;
    procedure Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF}); override;
    procedure Finalize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TWinMsgCommServer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommServer - class declaration
===============================================================================}
type
  TWinMsgCommServer = class(TWinMsgComm)
  protected
    fInitComplete:  Boolean;
    Function ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; override;
    procedure Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF}); override;
    procedure Finalize; override;
  public
    class Function ServerPresentOnDomain(const DomainName: String = ''): Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TWinMsgCommClient
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommClient - class declaration
===============================================================================}
type
  TWinMsgCommClient = class(TWinMsgComm)
  protected
    fServerID:  TWMCConnectionID;
    Function ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean; override;
    procedure Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF}); override;
    procedure Finalize; override;
  public
    Function ServerOnline: Boolean;
    Function SendBool(Value: Boolean; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostBool(Value: Boolean; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendUInt8(Value: UInt8; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostUInt8(Value: UInt8; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendInt8(Value: Int8; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostInt8(Value: Int8; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendUInt16(Value: UInt16; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostUInt16(Value: UInt16; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendInt16(Value: Int16; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostInt16(Value: Int16; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendUInt32(Value: UInt32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostUInt32(Value: UInt32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendInt32(Value: Int32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostInt32(Value: Int32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendUInt64(Value: UInt64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostUInt64(Value: UInt64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendInt64(Value: Int64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostInt64(Value: Int64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendInteger(Value: Integer; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostInteger(Value: Integer; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendFloat32(Value: Float32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostFloat32(Value: Float32; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendFloat64(Value: Float64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostFloat64(Value: Float64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendFloat(Value: Float64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostFloat(Value: Float64; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendString(const Value: String; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostString(const Value: String; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function SendData(const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean; reintroduce;
    Function PostData(const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean; reintroduce;
  end;

implementation

uses
  Math,
  StrRect, InterlockedOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
  {$DEFINE W5028:={$WARN 5028 OFF}} // Local $1 "$2" is not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TWinMsgComm
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgComm - implementation types and constants
===============================================================================}
type
  TWMCSharedDataHeader = packed record
    MessageCode:  TWMCMEssageCode;
    DataSize:     UInt64;
    Payload:      record end; // zero-size field
  end;
  PWMCSharedDataHeader = ^TWMCSharedDataHeader;

const
  WMC_MAXDATASIZE = 512 * 1024 * 1024;  // 512 MiB

  WMC_NAMEPREFIX_GLBDATA = 'wmc_glbdata_';
  WMC_NAMEPREFIX_MSGNAME = 'wmc_msgname_';
  WMC_NAMEPREFIX_SHRDATA = 'wmc_shrdata_';

  WMC_FLAG_INITIALIZED   = UInt32($00000001);
  WMC_FLAG_WINDOWSMSGS   = UInt32($00000002);
  WMC_FLAG_SERVERPRESENT = UInt32($00000004);

  WMC_USERDATA_FLAG_32BIT = Int8($01);

  WMC_MSGRES_ERR = 0;
  WMC_MSGRES_OK  = 1;

{-------------------------------------------------------------------------------
    TWinMsgComm - message codes
-------------------------------------------------------------------------------}
{
  sync    whether the message is sent (+) or posted (-)
  ret     returned value (message result)
  sender  valid sender

  (!)     user data contains some additional info
  ?       can be both sent or posted
  R       returns WMC_MSGRES_OK after successful processing (sent messages only)
  /       returned value is ignored
}
//------------------------------------|------------- payload -----------|- sync -|- ret -|- sender -|- notes -
const
  WMC_MSG_PING          = $00;      // sender system ID                 |   +    |   R   |  any     | used to check connection
  WMC_MSG_SERVERONLINE  = $01;      // server system ID             (!) |   +    |   /   |  server  | broadcasted when server connects
  WMC_MSG_SERVEROFFLINE = $02;      // server system ID                 |   -    |   /   |  server  | broadcasted when server is disconnecting
  WMC_MSG_SERVER        = $03;      // server system ID             (!) |   +    |   /   |  server  | sent in response to WMC_MSG_CLIENTONLINE
  WMC_MSG_CLIENTONLINE  = $04;      // client system ID             (!) |   +    |   /   |  client  | broadcasted when client connects
  WMC_MSG_CLIENTOFFLINE = $05;      // client system ID                 |   -    |   /   |  client  | sent to server (if any is connected) when client is disconnecting
  WMC_MSG_CLIENT        = $06;      // client system ID             (!) |   +    |   /   |  client  | sent in response to WMC_MSG_SERVERONLINE
  WMC_MSG_PEERONLINE    = $07;      // peer system ID               (!) |   +    |   /   |  peer    | broadcasted when peer connects
  WMC_MSG_PEEROFFLINE   = $08;      // peer system ID                   |   -    |   /   |  peer    | broadcasted when peer is disconnecting
  WMC_MSG_PEER          = $09;      // peer system ID               (!) |   +    |   /   |  peer    | sent in response to WMC_MSG_PEERONLINE

  WMC_MSG_VAL_BOOL    = $10;        // 8bit boolean value (ByteBool)    |   ?    |   /   |  any     |
  WMC_MSG_VAL_UINT8   = $11;        // 8bit unsigned integer value      |   ?    |   /   |  any     |
  WMC_MSG_VAL_INT8    = $12;        // 8bit signed integer value        |   ?    |   /   |  any     |
  WMC_MSG_VAL_UINT16  = $13;        // 16bit unsigned integer value     |   ?    |   /   |  any     |
  WMC_MSG_VAL_INT16   = $14;        // 16bit signed integer value       |   ?    |   /   |  any     |
  WMC_MSG_VAL_UINT32  = $15;        // 32bit unsigned integer value     |   ?    |   /   |  any     |
  WMC_MSG_VAL_INT32   = $16;        // 32bit signed integer value       |   ?    |   /   |  any     |
  WMC_MSG_VAL_UINT64  = $17;        // 64bit unsigned integer value     |   ?    |   /   |  any     |
  WMC_MSG_VAL_INT64   = $18;        // 64bit signed integer value       |   ?    |   /   |  any     |
  WMC_MSG_VAL_FLOAT32 = $19;        // 32bit floating point value       |   ?    |   /   |  any     |
  WMC_MSG_VAL_FLOAT64 = $1A;        // 64bit floating point value       |   ?    |   /   |  any     |

  WMC_MSG_STRING0 = $20;            // 0-length string                  |   ?    |   /   |  any     |
//WMC_MSG_STRING1 = $21;            // string of length 1               |   ?    |   /   |  any     |
//WMC_MSG_STRING2 = $22;            // string of length 2               |   ?    |   /   |  any     |
//WMC_MSG_STRING3 = $23;            // string of length 3               |   ?    |   /   |  any     |
//WMC_MSG_STRING4 = $24;            // string of length 4               |   ?    |   /   |  any     |
//WMC_MSG_STRING5 = $25;            // string of length 5               |   ?    |   /   |  any     |
//WMC_MSG_STRING6 = $26;            // string of length 6               |   ?    |   /   |  any     |
//WMC_MSG_STRING7 = $27;            // string of length 7               |   ?    |   /   |  any     |
  WMC_MSG_STRING8 = $28;            // string of length 8               |   ?    |   /   |  any     |

  WMC_MSG_DATA0 = $30;              // 0 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA1 = $31;              // 1 byte of data                   |   ?    |   /   |  any     |
//WMC_MSG_DATA2 = $32;              // 2 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA3 = $33;              // 3 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA4 = $34;              // 4 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA5 = $35;              // 5 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA6 = $36;              // 6 bytes of data                  |   ?    |   /   |  any     |
//WMC_MSG_DATA7 = $37;              // 7 bytes of data                  |   ?    |   /   |  any     |
  WMC_MSG_DATA8 = $38;              // 8 bytes of data                  |   ?    |   /   |  any     |

  WMC_MSG_DATA  = $39;              // shared data size and ID          |   +    |   /   |  any     |

  WMC_MSG_TRANS_START       = $40;  // size of sent data                |   +    |   R   |  any     | transaction start
  WMC_MSG_TRANS_END_UINT64  = $41;  // data checksum                    |   +    |   R   |  any     | also used in elsewhere to identify data type
  WMC_MSG_TRANS_END_INT64   = $42;  // data checksum                    |   +    |   R   |  any     | -//-
  WMC_MSG_TRANS_END_FLOAT64 = $43;  // data checksum                    |   +    |   R   |  any     | -//-
  WMC_MSG_TRANS_END_STRING  = $44;  // data checksum                    |   +    |   R   |  any     | -//-
  WMC_MSG_TRANS_END_DATA    = $45;  // data checksum                    |   +    |   R   |  any     | -//-

  WMC_MSG_TRANS_BUFF0 = $50;        // 0 bytes of data                  |   +    |   R   |  any     | normally unused
//WMC_MSG_TRANS_BUFF1 = $51;        // 1 byte of data                   |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF2 = $52;        // 2 bytes of data                  |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF3 = $53;        // 3 bytes of data                  |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF4 = $54;        // 4 bytes of data                  |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF5 = $55;        // 5 bytes of data                  |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF6 = $56;        // 6 bytes of data                  |   +    |   R   |  any     |
//WMC_MSG_TRANS_BUFF7 = $57;        // 7 bytes of data                  |   +    |   R   |  any     |
  WMC_MSG_TRANS_BUFF8 = $58;        // 8 bytes of data                  |   +    |   R   |  any     |

{===============================================================================
    TWinMsgComm - auxiliary functions
===============================================================================}
type
  TWMCMsgMetaData = packed record
    ConnectionID: TWMCConnectionID;
    MessageCode:  TWMCMessageCode;
    UserData:     TWMCUserData;
  end;

  TWMCMsgSharedData = packed record
    DataSize:     UInt32; // do not use TMemSize as it changes size depending on platform
    SharedDataID: TWMCSharedDataID
  end;

//------------------------------------------------------------------------------

Function GetConnectionID(MetaData: TWMCMessagePayload): TWMCConnectionID;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := TWMCMsgMetaData(UInt32(MetaData)).ConnectionID;
end;

//------------------------------------------------------------------------------

Function GetMessageCode(MetaData: TWMCMessagePayload): TWMCMessageCode;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := TWMCMsgMetaData(UInt32(MetaData)).MessageCode;
end;

//------------------------------------------------------------------------------

Function GetUserData(MetaData: TWMCMessagePayload): TWMCUserData;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := TWMCMsgMetaData(UInt32(MetaData)).UserData;
end;

//------------------------------------------------------------------------------

Function GetMessageMetaData(ConnectionID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData): TWMCMessagePayload;{$IFDEF CanInline} inline;{$ENDIF}
var
  MetaData: TWMCMsgMetaData;
begin
MetaData.ConnectionID := ConnectionID;
MetaData.MessageCode := MessageCode;
MetaData.UserData := UserData;
Result := TWMCMessagePayload(UInt32(MetaData));
end;

//------------------------------------------------------------------------------

Function GetSharedDataSize(MetaData: TWMCMessagePayload): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := TMemSize(TWMCMsgSharedData(MetaData).DataSize);
end;

//------------------------------------------------------------------------------

Function GetSharedDataID(MetaData: TWMCMessagePayload): TWMCSharedDataID;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := TWMCMsgSharedData(MetaData).SharedDataID;
end;

//------------------------------------------------------------------------------

Function GetSharedPayload(DataSize: TMemSize; SharedDataID: TWMCSharedDataID): TWMCMessagePayload;{$IFDEF CanInline} inline;{$ENDIF}
var
  TempPayload:  TWMCMsgSharedData;
begin
TempPayload.DataSize := UInt32(DataSize);
TempPayload.SharedDataID := SharedDataID;
Result := TWMCMessagePayload(TempPayload);
end;

//------------------------------------------------------------------------------

Function BoolToInt(Value: Boolean): Integer;{$IFDEF CanInline} inline;{$ENDIF}
begin
If Value then
  Result := 1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function IntToBool(Value: Integer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := Value <> 0;
end;

//------------------------------------------------------------------------------

Function TransactionEndCodeToValueCode(Code: Integer): Integer;
begin
case Code of
  WMC_MSG_TRANS_END_UINT64:   Result := WMC_MSG_VAL_UINT64;
  WMC_MSG_TRANS_END_INT64:    Result := WMC_MSG_VAL_INT64;
  WMC_MSG_TRANS_END_FLOAT64:  Result := WMC_MSG_VAL_FLOAT64;
else
  raise EWMCInvalidValue.CreateFmt('TransactionEndCodeToValueCode: Invalid transaction end code (%d).',[Code]);
end;
end;

{===============================================================================
    TWinMsgComm - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinMsgComm - protected methods
-------------------------------------------------------------------------------}

Function TWinMsgComm.GetConnection(Index: Integer): TWMCConnection;
begin
If CheckIndex(Index) then
  begin
    Result.ConnectionID := fConnections[Index].ConnectionID;
    Result.Is32Bit := fConnections[Index].Is32bit;
    Result.Transacting := fConnections[Index].Transacting;
  end
else raise EWMCIndexOutOfBounds.CreateFmt('TWinMsgComm.GetConnection: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.GetCapacity: Integer;
begin
Result := Length(fConnections);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.SetCapacity(Value: Integer);
begin
SetLength(fConnections,Value);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.GetCount: Integer;
begin
Result := fConnectionCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TWinMsgComm.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionAdd(ConnectionID: TWMCConnectionID; Is32bit: Boolean; SystemID: TWMCSystemID): Integer;
begin
If ConnectionID <> WMC_BROADCAST then
  begin
    If not ConnectionFind(ConnectionID,Result) then
      begin
        Grow;
        Result := fConnectionCount;
        fConnections[Result].ConnectionID := ConnectionID;
        fConnections[Result].Is32bit := Is32Bit;
        fConnections[Result].SystemID := SystemID;
        fConnections[Result].Transacting := False;
        fConnections[Result].Transaction.DataPtr := nil;        
        fConnections[Result].Transaction.DataSize := 0;
        fConnections[Result].Transaction.Position := 0;
        fConnections[Result].Transaction.CheckSum := ZeroCRC32;
        Inc(fConnectionCount);
      {$IFNDEF ConserveMemory}
        fIDToIndexTable[ConnectionID] := Result;
      {$ENDIF}
        DoConnectionChange;
      end
    else raise EWMCInvalidConnection.CreateFmt('TWinMsgComm.ConnectionAdd: Connection (%u) already exists.',[ConnectionID]);
  end
else raise EWMCInvalidConnection.CreateFmt('TWinMsgComm.ConnectionAdd: Invalid connection ID (%u)',[ConnectionID]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionRemove(ConnectionID: TWMCConnectionID): Integer;
begin
If ConnectionID <> WMC_BROADCAST then
  begin
    If ConnectionFind(ConnectionID,Result) then
      ConnectionDelete(Result);
  end
else raise EWMCInvalidConnection.CreateFmt('TWinMsgComm.ConnectionRemove: Invalid connection ID (%u)',[ConnectionID]);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.ConnectionDelete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
  {$IFNDEF ConserveMemory}
    fIDToIndexTable[Connections[Index].ConnectionID] := -1;
  {$ENDIF}
    If fConnections[Index].Transacting then
      with fConnections[Index].Transaction do
        FreeMem(DataPtr,DataSize);
    For i := Index to Pred(HighIndex) do
      begin
        fConnections[i] := fConnections[i + 1];
      {$IFNDEF ConserveMemory}
        fIDToIndexTable[Connections[i].ConnectionID] := i;
      {$ENDIF}
      end;
    Dec(fConnectionCount);
    Shrink;
    DoConnectionChange;
  end
else raise EWMCIndexOutOfBounds.CreateFmt('TWinMsgComm.ConnectionDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TransactionStart(Sender: TWMCConnectionID; DataSize: TMemSize): Boolean;
var
  Index:  Integer;
begin
Result := False;
If ConnectionFind(Sender,Index) then
  If not fConnections[Index].Transacting then
    begin
      fConnections[Index].Transacting := True;
      fConnections[Index].Transaction.DataPtr := AllocMem(DataSize);
      fConnections[Index].Transaction.DataSize := DataSize;
      fConnections[Index].Transaction.Position := 0;
      fConnections[Index].Transaction.CheckSum := InitialCRC32;
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TransactionBuff(Sender: TWMCConnectionID; Payload: TWMCMessagePayload; Size: TMemSize): Boolean;
var
  Index:  Integer;
begin
Result := False;
If ConnectionFind(Sender,Index) then
  with fConnections[Index] do
    If Transacting and (Transaction.Position + Size <= Transaction.DataSize) then
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        Move(Payload,Pointer(PtrUInt(Transaction.DataPtr) + PtrUInt(Transaction.Position))^,Size);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        Transaction.CheckSum := BufferCRC32(Transaction.CheckSum,Payload,Size);
        Inc(Transaction.Position,Size);
        Result := True;
      end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TransactionEnd(Sender: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;
var
  Index:      Integer;
  TempValue:  TWMCValue;
begin
Result := False;
If ConnectionFind(Sender,Index) then
  with fConnections[Index] do
    If Transacting and (Transaction.Position = Transaction.DataSize) and
      SameCRC32(Transaction.CheckSum,TCRC32(UInt32(Payload))) then
        begin
          TempValue.UserData := UserData;
          TempValue.StringValue := '';   
          If DataToValue(TempValue,MessageCode,Transaction.DataPtr,Transaction.DataSize) then
            begin
              DoIncomingValue(Sender,TempValue,Sent);
              Result := True;
            end;
          Transacting := False;
          FreeMem(Transaction.DataPtr,Transaction.DataSize);
        end;
end;

//------------------------------------------------------------------------------

class Function TWinMsgComm.DataToValue(var Value: TWMCValue; MessageCode: TWMCMessageCode; DataPtr: Pointer; DataSize: TMemSize): Boolean;
var
  TempStr:  UTF8String;
begin
Result := False;
case MessageCode of
  WMC_MSG_TRANS_END_UINT64:
    If DataSize = 8 then
      begin
        Value.ValueType := mvtUInt64;
        Move(DataPtr^,Value.UInt64Value,DataSize);
        Result := True;
      end;
  WMC_MSG_TRANS_END_INT64:
    If DataSize = 8 then
      begin
        Value.ValueType := mvtInt64;
        Move(DataPtr^,Value.Int64Value,DataSize);
        Result := True;
      end;
  WMC_MSG_TRANS_END_FLOAT64:
    If DataSize = 8 then
      begin
        Value.ValueType := mvtFloat64;
        Move(DataPtr^,Value.Float64Value,DataSize);
        Result := True;
      end;
  WMC_MSG_TRANS_END_STRING:
    begin
      SetLength(TempStr,DataSize);
      Move(DataPtr^,PUTF8Char(TempStr)^,DataSize);
      Value.ValueType := mvtString;
      Value.StringValue := UTF8ToStr(TempStr);
      Result := True;
    end;
else
 {WMC_MSG_TRANS_END_DATA}
  Value.ValueType := mvtData;
  Value.DataPtr := DataPtr;
  Value.DataSize := DataSize;
  Result := True;
end;
end;

//------------------------------------------------------------------------------

{$IFDEF UseWindowsMessages}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TWinMsgComm.HandleMessage(var Msg: TMessage; var Handled: Boolean; Sent: Boolean);
begin
If Msg.Msg = fWindowsMessageID then
  begin
    If ProcessMessage(GetConnectionID(TWMCMessagePayload(Msg.wParam)),
                      GetMessageCode(TWMCMessagePayload(Msg.wParam)),
                      GetUserData(TWMCMessagePayload(Msg.wParam)),
                      TWMCMessagePayload(Msg.lParam),Sent) then
      Msg.Result := WMC_MSGRES_OK
    else
      Msg.Result := WMC_MSGRES_ERR;
    Handled := True;
  end
else If Msg.Msg = WM_COPYDATA then
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If ProcessCopyData(PCopyDataStruct(PtrUInt(Msg.lParam))^,Sent) then
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
      Msg.Result := WMC_MSGRES_OK
    else
      Msg.Result := WMC_MSGRES_ERR;
    Handled := True;
  end
else Handled := False;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.ProcessCopyData(CopyDataStruct: TCopyDataStruct; Sent: Boolean): Boolean;
var
  Sender:     TWMCConnectionID;
  Index:      Integer;
  TempValue:  TWMCValue;
begin
Result := False;
Sender := GetConnectionID(TWMCMessagePayload(CopyDataStruct.dwData));
If ConnectionFind(Sender,Index) then
  begin
    TempValue.UserData := GetUserData(TWMCMessagePayload(CopyDataStruct.dwData));
    TempValue.StringValue := '';
    If DataToValue(TempValue,GetMessageCode(TWMCMessagePayload(CopyDataStruct.dwData)),
                   CopyDataStruct.lpData,CopyDataStruct.cbData) then
      begin
        DoIncomingValue(Sender,TempValue,Sent);
        Result := True;
      end;
  end;
end;

{$ELSE}//-----------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TWinMsgComm.HandleMessage(Sender: TObject; var Msg: TSMMessage; var Flags: TSMDispatchFlags);
begin
If ProcessMessage(GetConnectionID(TWMCMessagePayload(Msg.Param1)),
                  GetMessageCode(TWMCMessagePayload(Msg.Param1)),
                  GetUserData(TWMCMessagePayload(Msg.Param1)),
                  TWMCMessagePayload(Msg.Param2),dfSentMessage in Flags) then
  Msg.Result := WMC_MSGRES_OK
else
  Msg.Result := WMC_MSGRES_ERR;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.ProcessSharedData(Sender: TWMCConnectionID; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;
var
  Index:        Integer;
  SharedMemory: TSimpleSharedMemory;
  TempValue:    TWMCValue;
begin
Result := False;
If ConnectionFind(Sender,Index) then
  begin
    // open the shared memory
    SharedMemory := TSimpleSharedMemory.Create(GetSharedDataSize(Payload),
      WMC_NAMEPREFIX_SHRDATA + fDomainName + AnsiLowerCase(Format('[%.8x]',[GetSharedDataID(Payload)])));
    try
      TempValue.UserData := UserData;
      TempValue.StringValue := '';
      If DataToValue(TempValue,
        PWMCSharedDataHeader(SharedMemory.Memory)^.MessageCode,
        Addr(PWMCSharedDataHeader(SharedMemory.Memory)^.Payload),
        TMemSize(PWMCSharedDataHeader(SharedMemory.Memory)^.DataSize)) then
        begin
          DoIncomingValue(Sender,TempValue,Sent);
          Result := True;
        end;
    finally
      SharedMemory.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;

  Function ProcessValue(ValueType: TWMCValueType): Boolean;
  var
    TempValue:  TWMCValue;
  begin
    Result := True;
    TempValue.UserData := UserData;
    TempValue.StringValue := '';
    TempValue.ValueType := ValueType;
    case TempValue.ValueType of
      mvtBool:      TempValue.BoolValue := ByteBool(Payload);
      mvtUInt8:     TempValue.UInt8Value := UInt8(Payload);
      mvtInt8:      TempValue.Int8Value := Int8(Payload);
      mvtUInt16:    TempValue.UInt16Value := UInt16(Payload);
      mvtInt16:     TempValue.Int16Value := Int16(Payload);
      mvtUInt32:    TempValue.UInt32Value := UInt32(Payload);
      mvtInt32:     TempValue.Int32Value := Int32(Payload);
      mvtUInt64:    TempValue.UInt64Value := UInt64(Payload);
      mvtInt64:     TempValue.Int64Value := Int64(Payload);
      mvtFloat32:   TempValue.Float32Value := PFloat32(@Payload)^;
      mvtFloat64:   TempValue.Float64Value := PFloat64(@Payload)^;
    else
      Result := False;
    end;
    If Result then
      DoIncomingValue(Sender,TempValue,Sent);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ProcessString(Len: TStrSize): Boolean;
  var
    TempValue:  TWMCValue;
    TempStr:    UTF8String;
  begin
    SetLength(TempStr,Len);
    Move(Payload,PUTF8Char(TempStr)^,Len);
    TempValue.UserData := UserData;
    TempValue.StringValue := UTF8ToStr(TempStr);
    TempValue.ValueType := mvtString;
    DoIncomingValue(Sender,TempValue,Sent);
    Result := True
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ProcessData(Size: TMemSize): Boolean;
  var
    TempValue:  TWMCValue;
  begin
    TempValue.UserData := UserData;
    TempValue.StringValue := '';
    TempValue.ValueType := mvtData;
    TempValue.DataPtr := @Payload;
    TempValue.DataSize := Size;
    DoIncomingValue(Sender,TempValue,Sent);
    Result := True
  end;

begin
case MessageCode of
  WMC_MSG_PING:             Result := True;
  
  WMC_MSG_VAL_BOOL:         Result := ProcessValue(mvtBool);
  WMC_MSG_VAL_UINT8:        Result := ProcessValue(mvtUInt8);
  WMC_MSG_VAL_INT8:         Result := ProcessValue(mvtInt8);
  WMC_MSG_VAL_UINT16:       Result := ProcessValue(mvtUInt16);
  WMC_MSG_VAL_INT16:        Result := ProcessValue(mvtInt16);
  WMC_MSG_VAL_UINT32:       Result := ProcessValue(mvtUInt32);
  WMC_MSG_VAL_INT32:        Result := ProcessValue(mvtInt32);
  WMC_MSG_VAL_UINT64:       Result := ProcessValue(mvtUInt64);
  WMC_MSG_VAL_INT64:        Result := ProcessValue(mvtInt64);
  WMC_MSG_VAL_FLOAT32:      Result := ProcessValue(mvtFloat32);
  WMC_MSG_VAL_FLOAT64:      Result := ProcessValue(mvtFloat64);

  WMC_MSG_STRING0..
  WMC_MSG_STRING8:          Result := ProcessString(TStrSize(MessageCode - WMC_MSG_STRING0));

  WMC_MSG_DATA0..
  WMC_MSG_DATA8:            Result := ProcessData(TMemSize(MessageCode - WMC_MSG_DATA0));

  WMC_MSG_DATA:             Result := ProcessSharedData(Sender,UserData,Payload,Sent);

  WMC_MSG_TRANS_START:      Result := TransactionStart(Sender,TMemSize(Payload));
  WMC_MSG_TRANS_END_UINT64..
  WMC_MSG_TRANS_END_DATA:   Result := TransactionEnd(Sender,MessageCode,UserData,Payload,Sent);

  WMC_MSG_TRANS_BUFF0..
  WMC_MSG_TRANS_BUFF8:      Result := TransactionBuff(Sender,Payload,TMemSize(MessageCode - WMC_MSG_TRANS_BUFF0));
else
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SysSendSinglecast(SystemID: TWMCSystemID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Integer;
begin
{$IFDEF UseWindowsMessages}
Result := Integer(Windows.SendMessageW(SystemID,fWindowsMessageID,wParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),lParam(Payload)));
{$ELSE}
Result := Integer(fMessagesClient.SendMessage(SystemID,TSMMessageParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),TSMMessageParam(Payload)));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SysSendBroadcast(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Integer;
begin
{$IFDEF UseWindowsMessages}
Result := Integer(Windows.SendMessageW(HWND_BROADCAST,fWindowsMessageID,wParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),lParam(Payload)));
{$ELSE}
Result := Integer(fMessagesClient.SendMessage(CLIENTID_BROADCAST,TSMMessageParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),TSMMessageParam(Payload)));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SysPostSinglecast(SystemID: TWMCSystemID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
{$IFDEF UseWindowsMessages}
Result := Windows.PostMessageW(SystemID,fWindowsMessageID,wParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),lParam(Payload));
{$ELSE}
Result := fMessagesClient.PostMessage(SystemID,TSMMessageParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),TSMMessageParam(Payload));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SysPostBroadcast(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
{$IFDEF UseWindowsMessages}
Result := Windows.PostMessageW(HWND_BROADCAST,fWindowsMessageID,wParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),lParam(Payload));
{$ELSE}
Result := fMessagesClient.PostMessage(CLIENTID_BROADCAST,TSMMessageParam(GetMessageMetaData(fConnectionID,MessageCode,UserData)),TSMMessageParam(Payload));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendMessageIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
If CheckIndex(Index) then
  Result := SysSendSinglecast(fConnections[Index].SystemID,MessageCode,UserData,Payload) = WMC_MSGRES_OK
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostMessageIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
begin
If CheckIndex(Index) then
  Result := SysPostSinglecast(fConnections[Index].SystemID,MessageCode,UserData,Payload)
else
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendMessageAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
var
  i:  Integer;
begin
Result := fConnectionCount > 0;
For i := LowIndex to HighIndex do
  If SysSendSinglecast(fConnections[i].SystemID,MessageCode,UserData,Payload) <> WMC_MSGRES_OK then
    Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostMessageAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
var
  i:  Integer;
begin
Result := fConnectionCount > 0;
For i := LowIndex to HighIndex do
  If not SysPostSinglecast(fConnections[i].SystemID,MessageCode,UserData,Payload) then
    Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendMessageRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
If Recipient <> WMC_BROADCAST then
  Result := SendMessageIdx(ConnectionIndexOf(Recipient),MessageCode,UserData,Payload)
else
  Result := SendMessageAll(MessageCode,UserData,Payload);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostMessageRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
begin
If Recipient <> WMC_BROADCAST then
  Result := PostMessageIdx(ConnectionIndexOf(Recipient),MessageCode,UserData,Payload)
else
  Result := PostMessageAll(MessageCode,UserData,Payload);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.Send8ByteQuantity(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
{$IFDEF UseWindowsMessages}
{$IFDEF CPU64bit}
var
  Index:  Integer;
begin
{
  Parameters of sent windows messages are 64bits wide, so they can fit the
  entire value, but we also have to check whether the recipient can receive
  64bit windows messages (normal send) or not (send as data).
}
If ConnectionFind(Recipient,Index) then
  begin
    If fConnections[Index].Is32bit then
      Result := SendDataRcp(Recipient,MessageCode,UserData,@Payload,SizeOf(Payload))
    else
      Result := SendMessageRcp(Recipient,TransactionEndCodeToValueCode(MessageCode),UserData,Payload);
  end
else Result := False;
{$ELSE}
begin
// parameters of sent windows messages are only 32bits wide, send the value as data
Result := SendDataRcp(Recipient,MessageCode,UserData,@Payload,SizeOf(Payload));
{$ENDIF}
{$ELSE}
begin
// sending through simple messages, parameters are 64bits wide so the entire value can fit in
Result := SendMessageRcp(Recipient,TransactionEndCodeToValueCode(MessageCode),UserData,Payload);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.Post8ByteQuantity(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload): Boolean;
{$IFDEF UseWindowsMessages}
{$IFDEF CPU64bit}
var
  Index:  Integer;
begin
If ConnectionFind(Recipient,Index) then
  begin
    If fConnections[Index].Is32bit then
      Result := SendDataRcp(Recipient,MessageCode,UserData,@Payload,SizeOf(Payload))
    else
      Result := PostMessageRcp(Recipient,TransactionEndCodeToValueCode(MessageCode),UserData,Payload);
  end
else Result := False;
{$ELSE}
begin
Result := SendDataRcp(Recipient,MessageCode,UserData,@Payload,SizeOf(Payload));
{$ENDIF}
{$ELSE}
begin
Result := PostMessageRcp(Recipient,TransactionEndCodeToValueCode(MessageCode),UserData,Payload);
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF UseWindowsMessages}

Function TWinMsgComm.SendCopyData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
var
  CopyDataStruct: TCopyDataStruct;
begin
CopyDataStruct.dwData := PtrUInt(GetMessageMetaData(fConnectionID,MessageCode,UserData));
CopyDataStruct.cbData := DataSize;
CopyDataStruct.lpData := DataPtr;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Integer(Windows.SendMessageW(fConnections[Index].SystemID,
  WM_COPYDATA,wParam(fSystemID),lParam(PtrUInt(@CopyDataStruct)))) = WMC_MSGRES_OK;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.SendSharedData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
var
  SharedDataID: TWMCSharedDataID;
  SharedMemory: TSimpleSharedMemory;
begin
SharedDataID := TWMCSharedDataID(InterlockedExchangeAdd(PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Counter,1));
SharedMemory := TSimpleSharedMemory.Create(DataSize + SizeOf(TWMCSharedDataHeader),
  WMC_NAMEPREFIX_SHRDATA + fDomainName + AnsiLowerCase(Format('[%.8x]',[SharedDataID])));
try
  PWMCSharedDataHeader(SharedMemory.Memory)^.MessageCode := MessageCode;
  PWMCSharedDataHeader(SharedMemory.Memory)^.DataSize := UInt64(DataSize);
  Move(DataPtr^,PWMCSharedDataHeader(SharedMemory.Memory)^.Payload,DataSize);
  // send message and wait for its processing
  Result := SysSendSinglecast(fConnections[Index].SystemID,WMC_MSG_DATA,UserData,
    GetSharedPayload(SharedMemory.Size,SharedDataID)) = WMC_MSGRES_OK;
finally
  SharedMemory.Free;
end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendTransactedData(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
var
  SendBuffSize: TMemSize;
  CurrDataPtr:  PByte;
  TempPayload:  TWMCMessagePayload;
  BytesToCopy:  Integer;
  CheckSum:     TCRC32;
begin
Result := False;
SendBuffSize := SysParamUsableSize(fConnections[Index].Is32bit);
// start transaction
If SysSendSinglecast(fConnections[Index].SystemID,WMC_MSG_TRANS_START,0,TWMCMessagePayload(DataSize)) = WMC_MSGRES_OK then
  begin
    // send data in segments
    CurrDataPtr := DataPtr;
    TempPayload := 0;
    CheckSum := InitialCRC32;
    while DataSize > 0 do
      begin
        BytesToCopy := Min(Int64(SendBuffSize),Int64(DataSize));
        Move(CurrDataPtr^,TempPayload,BytesToCopy);
        If SysSendSinglecast(fConnections[Index].SystemID,WMC_MSG_TRANS_BUFF0 + BytesToCopy,0,TempPayload) <> WMC_MSGRES_OK then
          Exit; // result will be false
        CheckSum := BufferCRC32(CheckSum,TempPayload,BytesToCopy);
        Dec(DataSize,BytesToCopy);
        Inc(CurrDataPtr,BytesToCopy);
      end;
    // end transaction
    Result := SysSendSinglecast(fConnections[Index].SystemID,MessageCode,UserData,TWMCMessagePayload(UInt32(CheckSum))) = WMC_MSGRES_OK;
  end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TrySendTinyDataIdx(Index: Integer; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean;
var
  TempPayload:  TWMCMessagePayload;
begin
Result := False;
If CheckIndex(Index) then
  begin
    // only if the entire data can fit into usable payload...
    If DataSize <= SysParamUsableSize(fConnections[Index].Is32bit) then
      begin
        TempPayload := 0;
        Move(DataPtr^,TempPayload,DataSize);
        If IsString then
          SendResult := SysSendSinglecast(fConnections[Index].SystemID,
            WMC_MSG_STRING0 + DataSize,UserData,TempPayload) = WMC_MSGRES_OK
        else
          SendResult := SysSendSinglecast(fConnections[Index].SystemID,
            WMC_MSG_DATA0 + DataSize,UserData,TempPayload) = WMC_MSGRES_OK;
        Result := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TryPostTinyDataIdx(Index: Integer; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean;
var
  TempPayload:  TWMCMessagePayload;
begin
Result := False;
If CheckIndex(Index) then
  begin
    If DataSize <= SysParamUsableSize(fConnections[Index].Is32bit) then
      begin
        TempPayload := 0;
        Move(DataPtr^,TempPayload,DataSize);
        If IsString then
          SendResult := SysPostSinglecast(fConnections[Index].SystemID,
            WMC_MSG_STRING0 + DataSize,UserData,TempPayload)
        else
          SendResult := SysPostSinglecast(fConnections[Index].SystemID,
            WMC_MSG_DATA0 + DataSize,UserData,TempPayload);
        Result := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TrySendTinyDataRcp(Recipient: TWMCConnectionID; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean;
begin
If Recipient <> WMC_BROADCAST then
  Result := TrySendTinyDataIdx(ConnectionIndexOf(Recipient),IsString,UserData,DataPtr,DataSize,SendResult)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TryPostTinyDataRcp(Recipient: TWMCConnectionID; IsString: Boolean; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize; out SendResult: Boolean): Boolean;
begin
If Recipient <> WMC_BROADCAST then
  Result := TryPostTinyDataIdx(ConnectionIndexOf(Recipient),IsString,UserData,DataPtr,DataSize,SendResult)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendDataIdx(Index: Integer; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
begin
If CheckIndex(Index) then
  begin
    If DataSize >= fLargeDataThreshold then
    {$IFDEF UseWindowsMessages}
      Result := SendCopyData(Index,MessageCode,UserData,DataPtr,DataSize)
    {$ELSE}
      Result := SendSharedData(Index,MessageCode,UserData,DataPtr,DataSize)
    {$ENDIF}
    else
      Result := SendTransactedData(Index,MessageCode,UserData,DataPtr,DataSize);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendDataAll(MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
var
  i:  Integer;
begin
Result := fConnectionCount > 0;
For i := LowIndex to HighIndex do
  If not SendDataIdx(i,MessageCode,UserData,DataPtr,DataSize) then
    Result := False;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendDataRcp(Recipient: TWMCConnectionID; MessageCode: TWMCMEssageCode; UserData: TWMCUserData; DataPtr: Pointer; DataSize: TMemSize): Boolean;
begin
If Recipient <> WMC_BROADCAST then
  Result := SendDataIdx(ConnectionIndexOf(Recipient),MessageCode,UserData,DataPtr,DataSize)
else
  Result := SendDataAll(MessageCode,UserData,DataPtr,DataSize);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.DoIncomingValue(SenderID: TWMCConnectionID; Value: TWMCValue; Sent: Boolean);
begin
If Assigned(fOnIncomingValueEvent) then
  fOnIncomingValueEvent(Self,SenderID,Value,Sent)
else If Assigned(fOnIncomingValueCallback) then
  fOnIncomingValueCallback(Self,SenderID,Value,Sent);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.DoConnectionChange;
begin
If Assigned(fOnConnectionEvent) then
  fOnConnectionEvent(Self)
else If Assigned(fOnConnectionCallback) then
  fOnConnectionCallback(Self);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF});
var
  i:  Integer;
begin
fLargeDataThreshold := 128{bytes};
fDomainName := AnsiLowerCase(DomainName);
If Length(fDomainName) > 32 then
  SetLength(fDomainName,32);
fGlobalDataShrdMem := TSharedMemory.Create(SizeOf(TWMCGlobalData),WMC_NAMEPREFIX_GLBDATA + fDomainName);
fGlobalDataShrdMem.Lock;
try
  // set or check flags
  If PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags and WMC_FLAG_INITIALIZED = 0 then
    PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags := WMC_FLAG_INITIALIZED {$IFDEF UseWindowsMessages}or WMC_FLAG_WINDOWSMSGS{$ENDIF}
  else If PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags and WMC_FLAG_WINDOWSMSGS {$IFDEF UseWindowsMessages}={$ELSE}<>{$ENDIF} 0 then
     raise EWMCInvalidConnection.Create('TWinMsgComm.Initialize: Invalid connection type.');
  // get first free ID
  fIDPoolVector := TBitVector.Create(Addr(PWMCGlobalData(fGlobalDataShrdMem.Memory)^.IDPool),$10000);
  fIDPoolVector[Integer(WMC_BROADCAST)] := True; // make sure nobody can get broadcast ID
  i := fIDPoolVector.FirstClean;
  If (i >= 0) and (i < $FFFF) then
    begin
      fIDPoolVector[i] := True;
      fConnectionID := TWMCConnectionID(i);
    end
  else raise EWMCOutOfResources.Create('TWinMsgComm.Initialize: No free connection ID.');
finally
  fGlobalDataShrdMem.Unlock;
end;
// initilaize connections
SetLength(fConnections,0);
fConnectionCount := 0;
{$IFNDEF ConserveMemory}
// init connection ID to index table
For i := Low(fIDToIndexTable) to High(fIDToIndexTable) do
  fIDToIndexTable[i] := -1;
{$ENDIF}
{$IFDEF UseWindowsMessages}
fWindowsMessageID := RegisterWindowMessageW(PWideChar(StrToWinW(WMC_NAMEPREFIX_MSGNAME + fDomainName)));
If Assigned(ReceivingWindow) then
  begin
    fOwnsRecevingWindow := False;
    fReceivingWindow := ReceivingWindow;
  end
else
  begin
    fOwnsRecevingWindow := True;
    fReceivingWindow := TUtilityWindow.Create
  end;
fReceivingWindow.OnMessage.Add(HandleMessage);
fSystemID := fReceivingWindow.WindowHandle;
{$ELSE}
{$IFDEF SimpleMessagesNoLimits}
fMessagesClient := TSimpleMessagesClient.Create(Pred(High(TWMCConnectionID)),High(TWMCConnectionID) * 10,WMC_NAMEPREFIX_MSGNAME + fDomainName);
{$ELSE}
{$IFDEF SimpleMessagesHigherLimits}
fMessagesClient := TSimpleMessagesClient.Create(1024,10 * 1024,WMC_NAMEPREFIX_MSGNAME + fDomainName);
{$ELSE}
fMessagesClient := TSimpleMessagesClient.Create(WMC_NAMEPREFIX_MSGNAME + fDomainName);
{$ENDIF}
{$ENDIF}
fMessagesClient.OnMessageEvent := HandleMessage;
fSystemID := fMessagesClient.ClientID;
{$ENDIF}
// init events
fOnIncomingValueCallback := nil;
fOnIncomingValueEvent := nil;
fOnConnectionCallback := nil;
fOnConnectionEvent := nil;
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.Finalize;
var
  i:  Integer;
begin
// disable events
fOnIncomingValueCallback := nil;
fOnIncomingValueEvent := nil;
fOnConnectionCallback := nil;
fOnConnectionEvent := nil;
{$IFDEF UseWindowsMessages}
If Assigned(fReceivingWindow) then
  fReceivingWindow.OnMessage.Remove(HandleMessage);
If fOwnsRecevingWindow then
  fReceivingWindow.Free;
{$ELSE}
fMessagesClient.Free;
{$ENDIF}
// clear all connections, if there is transaction in progress, end it and free memory
For i := LowIndex to HighIndex do
  If fConnections[i].Transacting then
    with fConnections[i].Transaction do
      FreeMem(DataPtr,DataSize);
SetLength(fConnections,0);
fConnectionCount := 0;
// remove self
If Assigned(fGlobalDataShrdMem) then
  begin
    fGlobalDataShrdMem.Lock;
    try
      If Assigned(fIDPoolVector) then
        fIDPoolVector[Integer(fConnectionID)] := False;
    finally
      fGlobalDataShrdMem.Unlock;
    end;
  end;
fIDPoolVector.Free;
fGlobalDataShrdMem.Free;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
class Function TWinMsgComm.SysParamUsableSize(RecipientIs32Bit: Boolean): TMemSize;
begin
Result := TMemSize(
{$IFDEF UseWindowsMessages}
  {$IFDEF CPU64bit}
    IfThen(RecipientIs32Bit,4,8){64bit windows messages, check recipient bits}
  {$ELSE}
    4{32bit windows messages}
  {$ENDIF}
{$ELSE}
    8{simple messages}
{$ENDIF});
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TWinMsgComm - public methods
-------------------------------------------------------------------------------}

class Function TWinMsgComm.MaxDataSize: TMemSize;
begin
Result := WMC_MAXDATASIZE;
end;

//------------------------------------------------------------------------------

class Function TWinMsgComm.ValueTypeStr(ValueType: TWMCValueType): String;
const
  WMC_VALTYPESTR_ARR: array[TWMCValueType] of String = (
    'Boolean','UInt8','Int8','UInt16','Int16','UInt32','Int32','UInt64','Int64',
    'Float32','Float64','String','Data');
begin
If (ValueType >= Low(WMC_VALTYPESTR_ARR)) and (ValueType <= High(WMC_VALTYPESTR_ARR)) then
  Result := WMC_VALTYPESTR_ARR[ValueType]
else
  Result := '';
end;

//------------------------------------------------------------------------------

constructor TWinMsgComm.Create(const DomainName: String = ''{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow = nil{$ENDIF});
begin
inherited Create;
Initialize(DomainName{$IFDEF UseWindowsMessages},ReceivingWindow{$ENDIF});
end;

//------------------------------------------------------------------------------

destructor TWinMsgComm.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.LowIndex: Integer;
begin
Result := Low(fConnections);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.HighIndex: Integer;
begin
Result := Pred(fConnectionCount);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionIndexOf(ConnectionID: TWMCConnectionID): Integer;
{$IFDEF ConserveMemory}
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fConnections[i].ConnectionID = ConnectionID then
    begin
      Result := i;
      Break{For i};
    end;
end;
{$ELSE}
begin
Result := fIDToIndexTable[ConnectionID];
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionFind(ConnectionID: TWMCConnectionID; out Index: Integer): Boolean;
begin
Index := ConnectionIndexOf(ConnectionID);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionsCheck: Boolean;
var
  i:  Integer;
begin
Result := True;
For i := HighIndex downto LowIndex do
  If SysSendSinglecast(fConnections[i].SystemID,WMC_MSG_PING,0,TWMCMessagePayload(fSystemID)) <> WMC_MSGRES_OK then
    begin
      Result := False;
      ConnectionDelete(i);
    end;
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.Update(WaitForValue: Boolean = False);
begin
{$IFDEF UseWindowsMessages}
fReceivingWindow.ProcessMessages(WaitForValue);
{$ELSE}
If WaitForValue then
  fMessagesClient.GetMessages(INFINITE)
else
  fMessagesClient.PeekMessages;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendBool(Recipient: TWMCConnectionID; Value: Boolean; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_BOOL,UserData,TWMCMEssagePayload(BoolToInt(Value)));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostBool(Recipient: TWMCConnectionID; Value: Boolean; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_BOOL,UserData,TWMCMEssagePayload(BoolToInt(Value)));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendUInt8(Recipient: TWMCConnectionID; Value: UInt8; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_UINT8,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostUInt8(Recipient: TWMCConnectionID; Value: UInt8; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_UINT8,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendInt8(Recipient: TWMCConnectionID; Value: Int8; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_INT8,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostInt8(Recipient: TWMCConnectionID; Value: Int8; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_INT8,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendUInt16(Recipient: TWMCConnectionID; Value: UInt16; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_UINT16,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostUInt16(Recipient: TWMCConnectionID; Value: UInt16; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_UINT16,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendInt16(Recipient: TWMCConnectionID; Value: Int16; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_INT16,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostInt16(Recipient: TWMCConnectionID; Value: Int16; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_INT16,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendUInt32(Recipient: TWMCConnectionID; Value: UInt32; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_UINT32,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostUInt32(Recipient: TWMCConnectionID; Value: UInt32; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_UINT32,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendInt32(Recipient: TWMCConnectionID; Value: Int32; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_INT32,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostInt32(Recipient: TWMCConnectionID; Value: Int32; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_INT32,UserData,TWMCMEssagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendUInt64(Recipient: TWMCConnectionID; Value: UInt64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Send8ByteQuantity(Recipient,WMC_MSG_TRANS_END_UINT64,UserData,TWMCMessagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostUInt64(Recipient: TWMCConnectionID; Value: UInt64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Post8ByteQuantity(Recipient,WMC_MSG_TRANS_END_UINT64,UserData,TWMCMessagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendInt64(Recipient: TWMCConnectionID; Value: Int64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Send8ByteQuantity(Recipient,WMC_MSG_TRANS_END_INT64,UserData,TWMCMessagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostInt64(Recipient: TWMCConnectionID; Value: Int64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Post8ByteQuantity(Recipient,WMC_MSG_TRANS_END_INT64,UserData,TWMCMessagePayload(Value));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendInteger(Recipient: TWMCConnectionID; Value: Integer; UserData: TWMCUserData = 0): Boolean;
begin
case SizeOf(Integer) of
  2:  Result := SendInt16(Recipient,Int16(Value),UserData);
  4:  Result := SendInt32(Recipient,Int32(Value),UserData);
  8:  Result := SendInt64(Recipient,Int64(Value),UserData);
else
  raise EWMCInvalidOperation.CreateFmt('TWinMsgComm.SendInteger: Invalid size of Integer type (%d).',[SizeOf(Integer)]);
end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostInteger(Recipient: TWMCConnectionID; Value: Integer; UserData: TWMCUserData = 0): Boolean;
begin
case SizeOf(Integer) of
  2:  Result := PostInt16(Recipient,Int16(Value),UserData);
  4:  Result := PostInt32(Recipient,Int32(Value),UserData);
  8:  Result := PostInt64(Recipient,Int64(Value),UserData);
else
  raise EWMCInvalidOperation.CreateFmt('TWinMsgComm.PostInteger: Invalid size of Integer type (%d).',[SizeOf(Integer)]);
end;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendFloat32(Recipient: TWMCConnectionID; Value: Float32; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendMessageRcp(Recipient,WMC_MSG_VAL_FLOAT32,UserData,TWMCMEssagePayload(PUInt32(@Value)^));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostFloat32(Recipient: TWMCConnectionID; Value: Float32; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostMessageRcp(Recipient,WMC_MSG_VAL_FLOAT32,UserData,TWMCMEssagePayload(PUInt32(@Value)^));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendFloat64(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Send8ByteQuantity(Recipient,WMC_MSG_TRANS_END_FLOAT64,UserData,TWMCMEssagePayload(PUInt64(@Value)^));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostFloat64(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Post8ByteQuantity(Recipient,WMC_MSG_TRANS_END_FLOAT64,UserData,TWMCMEssagePayload(PUInt64(@Value)^));
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendFloat(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := SendFloat64(Recipient,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostFloat(Recipient: TWMCConnectionID; Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := PostFloat64(Recipient,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendString(Recipient: TWMCConnectionID; const Value: String; UserData: TWMCUserData = 0): Boolean;
var
  TempStr:  UTF8String;
begin
TempStr := StrToUTF8(Value);
If TMemSize(Length(TempStr) * SizeOf(UTF8Char)) <= MaxDataSize then
  begin
    If not TrySendTinyDataRcp(Recipient,True,UserData,PUTF8Char(TempStr),Length(TempStr) * SizeOf(UTF8Char),Result) then
      Result := SendDataRcp(Recipient,WMC_MSG_TRANS_END_STRING,UserData,PUTF8Char(TempStr),Length(TempStr) * SizeOf(UTF8Char));
  end
else raise EWMCTooMuchData.CreateFmt('TWinMsgComm.SendString: Too much data to send (%u).',[Length(TempStr) * SizeOf(UTF8Char)]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostString(Recipient: TWMCConnectionID; const Value: String; UserData: TWMCUserData = 0): Boolean;
var
  TempStr:  UTF8String;
begin
TempStr := StrToUTF8(Value);
If TMemSize(Length(TempStr) * SizeOf(UTF8Char)) <= MaxDataSize then
  begin
    If not TryPostTinyDataRcp(Recipient,True,UserData,PUTF8Char(TempStr),Length(TempStr) * SizeOf(UTF8Char),Result) then
      Result := SendDataRcp(Recipient,WMC_MSG_TRANS_END_STRING,UserData,PUTF8Char(TempStr),Length(TempStr) * SizeOf(UTF8Char));
  end
else raise EWMCTooMuchData.CreateFmt('TWinMsgComm.PostString: Too much data to post (%u).',[Length(TempStr) * SizeOf(UTF8Char)]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.SendData(Recipient: TWMCConnectionID; const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean;
begin
If Size <= MaxDataSize then
  begin
    If not TrySendTinyDataRcp(Recipient,False,UserData,@Data,Size,Result) then
      Result := SendDataRcp(Recipient,WMC_MSG_TRANS_END_DATA,UserData,@Data,Size);
  end
else raise EWMCTooMuchData.CreateFmt('TWinMsgComm.SendData: Too much data to send (%u).',[Size]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.PostData(Recipient: TWMCConnectionID; const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean;
begin
If Size <= MaxDataSize then
  begin
    If not TryPostTinyDataRcp(Recipient,False,UserData,@Data,Size,Result) then
      Result := SendDataRcp(Recipient,WMC_MSG_TRANS_END_DATA,UserData,@Data,Size);
  end
else raise EWMCTooMuchData.CreateFmt('TWinMsgComm.PostData: Too much data to post (%u).',[Size]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TWinMsgCommPeer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommPeer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinMsgCommPeer - protected methods
-------------------------------------------------------------------------------}

Function TWinMsgCommPeer.ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;
begin
case MessageCode of
  WMC_MSG_SERVERONLINE,
  WMC_MSG_SERVEROFFLINE,
  WMC_MSG_SERVER,
  WMC_MSG_CLIENTONLINE,
  WMC_MSG_CLIENTOFFLINE,
  WMC_MSG_CLIENT:         Result := False;
  WMC_MSG_PEERONLINE:     If Sender <> fConnectionID then
                            begin
                              // add the new peer to connections and inform it that we exist
                              ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                              SysSendSinglecast(TWMCSystemID(Payload),WMC_MSG_PEER,
                                                {$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},
                                                TWMCMessagePayload(fSystemID));
                              Result := True;
                            end
                          else Result := False;
  WMC_MSG_PEEROFFLINE:    If Sender <> fConnectionID then
                            begin
                              // remove the peer from connections
                              ConnectionRemove(Sender);
                              Result := True;
                            end
                          else Result := False;
  WMC_MSG_PEER:           begin
                            // just add this peer to connections
                            ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                            Result := True;
                          end;
else
  Result := inherited ProcessMessage(Sender,MessageCode,UserData,Payload,Sent);
end;
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommPeer.Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF});
begin
inherited Initialize(DomainName{$IFDEF UseWindowsMessages},ReceivingWindow{$ENDIF});
SysSendBroadcast(WMC_MSG_PEERONLINE,{$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},TWMCMessagePayload(fSystemID));
Update; // to process all responses to previsou send
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommPeer.Finalize;
begin
SysPostBroadcast(WMC_MSG_PEEROFFLINE,0,TWMCMessagePayload(fSystemID));
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TWinMsgCommServer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommServer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinMsgCommServer - protected methods
-------------------------------------------------------------------------------}

Function TWinMsgCommServer.ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;
begin
case MessageCode of
  WMC_MSG_SERVERONLINE,
  WMC_MSG_SERVEROFFLINE,
  WMC_MSG_SERVER:         Result := False;
  WMC_MSG_CLIENTONLINE:   begin
                            ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                            SysSendSinglecast(TWMCSystemID(Payload),WMC_MSG_SERVER,
                                              {$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},
                                              TWMCMessagePayload(fSystemID));
                            Result := True;
                          end;
  WMC_MSG_CLIENTOFFLINE:  begin
                            ConnectionRemove(Sender);
                            Result := True;
                          end;
  WMC_MSG_CLIENT:         begin
                            ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                            Result := True;
                          end;
  WMC_MSG_PEERONLINE,
  WMC_MSG_PEEROFFLINE,
  WMC_MSG_PEER:           Result := False;
else
  Result := inherited ProcessMessage(Sender,MessageCode,UserData,Payload,Sent);
end;
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommServer.Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF});
begin
inherited Initialize(DomainName{$IFDEF UseWindowsMessages},ReceivingWindow{$ENDIF});
fGlobalDataShrdMem.Lock;
try
  If PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags and WMC_FLAG_SERVERPRESENT = 0 then
    begin
      PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags :=
        PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags or WMC_FLAG_SERVERPRESENT;
      fInitComplete := True;
    end
  else raise EWMCServerExists.Create('TWinMsgCommServer.Initialize: Server is already present on this domain.');
finally
  fGlobalDataShrdMem.Unlock;
end;
SysSendBroadcast(WMC_MSG_SERVERONLINE,{$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},TWMCMessagePayload(fSystemID));
Update;
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommServer.Finalize;
begin
If fInitComplete then
  begin
    SysPostBroadcast(WMC_MSG_SERVEROFFLINE,0,TWMCMessagePayload(fSystemID));  
    fGlobalDataShrdMem.Lock;
    try
      PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags :=
        PWMCGlobalData(fGlobalDataShrdMem.Memory)^.Flags and not WMC_FLAG_SERVERPRESENT;
    finally
      fGlobalDataShrdMem.Unlock;
    end;
  end;
inherited;
end;

{-------------------------------------------------------------------------------
    TWinMsgCommServer - public methods
-------------------------------------------------------------------------------}

class Function TWinMsgCommServer.ServerPresentOnDomain(const DomainName: String = ''): Boolean;
var
  SharedMemory: TSharedMemory;
begin
SharedMemory := TSharedMemory.Create(SizeOf(TWMCGlobalData),WMC_NAMEPREFIX_GLBDATA + Copy(AnsiLowerCase(DomainName),1,32));
try
  SharedMemory.Lock;
  try
    Result := PWMCGlobalData(SharedMemory.Memory)^.Flags and WMC_FLAG_SERVERPRESENT <> 0;
  finally
    SharedMemory.Unlock;
  end;
finally
  SharedMemory.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TWinMsgCommClient
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgCommClient - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinMsgCommClient - protected methods
-------------------------------------------------------------------------------}

Function TWinMsgCommClient.ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserData: TWMCUserData; Payload: TWMCMessagePayload; Sent: Boolean): Boolean;
begin
case MessageCode of
  WMC_MSG_SERVERONLINE:   If not ServerOnline then
                            begin
                              fServerID := Sender;
                              ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                              SysSendSinglecast(TWMCSystemID(Payload),WMC_MSG_CLIENT,
                                                {$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},
                                                TWMCMessagePayload(fSystemID));
                              Result := True;
                            end
                          else Result := False;
  WMC_MSG_SERVEROFFLINE:  If ServerOnline then
                            begin
                              ConnectionRemove(Sender);
                              Result := True;
                            end
                          else Result := False;
  WMC_MSG_SERVER:         If not ServerOnline then
                            begin
                              fServerID := Sender;
                              ConnectionAdd(Sender,WMC_USERDATA_FLAG_32BIT and UserData <> 0,TWMCSystemID(Payload));
                              Result := True;
                            end
                          else Result := False;
  WMC_MSG_CLIENTONLINE,
  WMC_MSG_CLIENTOFFLINE,
  WMC_MSG_CLIENT,
  WMC_MSG_PEERONLINE,
  WMC_MSG_PEEROFFLINE,
  WMC_MSG_PEER:           Result := False;
else
  Result := inherited ProcessMessage(Sender,MessageCode,UserData,Payload,Sent);
end;
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommClient.Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF});
begin
inherited Initialize(DomainName{$IFDEF UseWindowsMessages},ReceivingWindow{$ENDIF});
fServerID := WMC_BROADCAST;
SysSendBroadcast(WMC_MSG_CLIENTONLINE,{$IFDEF CPU64bit}0{$ELSE}WMC_USERDATA_FLAG_32BIT{$ENDIF},TWMCMessagePayload(fSystemID));
Update;
end;

//------------------------------------------------------------------------------

procedure TWinMsgCommClient.Finalize;
begin
SysPostBroadcast(WMC_MSG_CLIENTOFFLINE,0,TWMCMessagePayload(fSystemID));
inherited;
end;

{-------------------------------------------------------------------------------
    TWinMsgCommClient - public methods
-------------------------------------------------------------------------------}

Function TWinMsgCommClient.ServerOnline: Boolean;
begin
Result := fConnectionCount > 0; 
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendBool(Value: Boolean; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendBool(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostBool(Value: Boolean; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostBool(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendUInt8(Value: UInt8; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendUInt8(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostUInt8(Value: UInt8; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostUInt8(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendInt8(Value: Int8; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendInt8(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostInt8(Value: Int8; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostInt8(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendUInt16(Value: UInt16; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendUInt16(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostUInt16(Value: UInt16; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostUInt16(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendInt16(Value: Int16; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendInt16(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostInt16(Value: Int16; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostInt16(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendUInt32(Value: UInt32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendUInt32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostUInt32(Value: UInt32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostUInt32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendInt32(Value: Int32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendInt32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostInt32(Value: Int32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostInt32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendUInt64(Value: UInt64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendUInt64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostUInt64(Value: UInt64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostUInt64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendInt64(Value: Int64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendInt64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostInt64(Value: Int64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostInt64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendInteger(Value: Integer; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendInteger(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostInteger(Value: Integer; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostInteger(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendFloat32(Value: Float32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendFloat32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostFloat32(Value: Float32; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostFloat32(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendFloat64(Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendFloat64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostFloat64(Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostFloat64(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendFloat(Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendFloat(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostFloat(Value: Float64; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostFloat(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendString(const Value: String; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendString(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostString(const Value: String; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostString(fServerID,Value,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.SendData(const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited SendData(fServerID,Data,Size,UserData);
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PostData(const Data; Size: TMemSize; UserData: TWMCUserData = 0): Boolean;
begin
Result := Inherited PostData(fServerID,Data,Size,UserData);
end;

end.

