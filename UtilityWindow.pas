{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Utility Window

    Simple window intended for use in sending and processing of custom messages,
    or when there is a need for invisible window that is capable of reacting to
    messages sent or posted to it.

    Can be used in a non-main thread as long as you call method ProcessMessages
    or ContinuousProcessMessages at least once.

      WARNING - the window must be created and managed (eg. call to method
                ProcessMessages) in the same thread where you want to process
                the messages, otherwise it will not work!

  Version 1.5.1 (2024-05-03)

  Last change 2024-05-03

  ©2015-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.UtilityWindow

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent
    WndAlloc       - github.com/TheLazyTomcat/Lib.WndAlloc

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol UtilityWindow_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxMath     - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit UtilityWindow;
{
  UtilityWindow_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  UtilityWindow_UseAuxExceptions to achieve this.
}
{$IF Defined(UtilityWindow_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
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
  Windows, Messages, SysUtils,
  AuxClasses, MulticastEvent{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

type
  EUWException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EUWSystemEror = class(EUWException);

{===============================================================================
--------------------------------------------------------------------------------
                             TMulticastMessageEvent
--------------------------------------------------------------------------------
===============================================================================}
{
  Msg

    Contains currently processed message.

  Handled

    When it is false on entry, it indicates the message was not yet handled,
    when true it was handled by at least one, and possibly more, handlers.

    The handler should set it to true when it does something with the message,
    but it is not mandatory. Never set it to false (it has no effect).

  Sent

    Indicates that the processed message was sent, rather than posted.

    Note that if the message was sent from the same thread as the one that is
    processing this message, this parameter will read as False.

    Calling BreakProcessing when it is True has no immediate effect, as that
    works only when processing posted message.
}
  TMessageCallback = procedure(var Msg: TMessage; var Handled: Boolean; Sent: Boolean);
  TMessageEvent    = procedure(var Msg: TMessage; var Handled: Boolean; Sent: Boolean) of object;

{===============================================================================
    TMulticastMessageEvent - class declaration
===============================================================================}
type
  TMulticastMessageEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TMessageCallback): Integer; reintroduce; overload;
    Function IndexOf(const Handler: TMessageEvent): Integer; reintroduce; overload;
    Function Add(Handler: TMessageCallback; AllowDuplicity: Boolean = False): Integer; reintroduce; overload;
    Function Add(Handler: TMessageEvent; AllowDuplicity: Boolean = False): Integer; reintroduce; overload;
    Function Remove(const Handler: TMessageCallback): Integer; reintroduce; overload;
    Function Remove(const Handler: TMessageEvent): Integer; reintroduce; overload;
    procedure Call(var Msg: TMessage; var Handled: Boolean; Sent: Boolean); reintroduce;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TUtilityWindow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUtilityWindow - class declarationn
===============================================================================}
type
  TUtilityWindow = class(TCustomObject)
  protected
    fWindowHandle:        HWND;
    fContinueProcessing:  Boolean;
    fOnMessage:           TMulticastMessageEvent;
    procedure WndProc(var Msg: TMessage); virtual;
    Function ProcessMessageInternal(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  {
    BreakProcessing

      This method, when called, will cause methods ProcessMessages and
      ContinuousProcessMessages to exit before all messages are processed.

      Note that if it is called from message handler and the processed message
      was sent, rather than posted, this method has no immediate effect.
      It takes effect only after processing at least one posted message.
  }
    procedure BreakProcessing; virtual;
  {
    ProcessMessage

      Processes (dispatches) all sent message and then retrieves and dispatches
      exactly one, not more, posted message from the queue.

      When WaitForMessage is set to true, the function will not return until
      at least one message is posted to the queue. During that time, all sent
      messages are dispatched as they arrive.
      When WaitForMessage is set to false, the function tries to retrive one
      message from the queue. If there is at least one, it dispatches it and
      then returns. If there is no message posted in the queue, the function
      return immediately. In both cases, it first dispatches all pending sent
      messages.

      Output parameter ReceivedQuitMessage is set to true when WM_QUIT message
      is retrived from the queue, otherwise it is always false.
      Note that WM_QUIT message is never dispatched.
  }
    procedure ProcessMessage(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean); overload; virtual;
    procedure ProcessMessage(WaitForMessage: Boolean = False); overload; virtual;
  {
    ProcessMessages

      Works exactly the same as ProcessMessage, but it will try to retrieve and
      dispatch all pending posted messages, not just one.

      Note it is possible that not all messages will be retrived. This is
      because the retrieving can be aborted by calling BreakProcessing or when
      WM_QUIT message is encountered.

      Returns true when all incoming messages were processed (which might
      actually be none), false when the processing was interrupted before
      all messages could be processed.
  }
    Function ProcessMessages(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean; overload; virtual;
    Function ProcessMessages(WaitForMessage: Boolean = False): Boolean; overload; virtual;
  {
    ContinuousProcessMessages

      Repeatedly waits for incoming messages and dispatches them as they arrive.

      This method does not return until WM_QUIT message is delivered (in which
      case the result is set to True) or the processing is interrupted by
      calling BreakProcessing (result set to False).
  }
    Function ContinuousProcessMessages: Boolean; virtual;
    property WindowHandle: HWND read fWindowHandle;
    property OnMessage: TMulticastMessageEvent read fOnMessage;
  end;

implementation

uses
  WndAlloc;

{===============================================================================
--------------------------------------------------------------------------------
                             TMulticastMessageEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMulticastMessageEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMulticastMessageEvent - public methods
-------------------------------------------------------------------------------}

Function TMulticastMessageEvent.IndexOf(const Handler: TMessageCallback): Integer;
begin
Result := inherited IndexOf(TCallback(Handler));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastMessageEvent.IndexOf(const Handler: TMessageEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastMessageEvent.Add(Handler: TMessageCallback; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TCallback(Handler),AllowDuplicity);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastMessageEvent.Add(Handler: TMessageEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastMessageEvent.Remove(const Handler: TMessageCallback): Integer;
begin
Result := inherited Remove(TCallback(Handler));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastMessageEvent.Remove(const Handler: TMessageEvent): Integer;
begin
Result := inherited Remove(TEvent(Handler));
end;

//------------------------------------------------------------------------------

procedure TMulticastMessageEvent.Call(var Msg: TMessage; var Handled: Boolean; Sent: Boolean);
var
  i:            Integer;
  EntryHandled: Boolean;
begin
Handled := False;
For i := LowIndex to HighIndex do
  begin
    EntryHandled := Handled;
    If Entries[i].IsMethod then
      TMessageEvent(Entries[i].HandlerMethod)(Msg,EntryHandled,Sent)
    else
      TMessageCallback(Entries[i].HandlerProcedure)(Msg,EntryHandled,Sent);
    If EntryHandled then
      Handled := True;
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TUtilityWindow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUtilityWindow - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUtilityWindow - protected methods
-------------------------------------------------------------------------------}

procedure TUtilityWindow.WndProc(var Msg: TMessage);
var
  Handled:  Boolean;
begin
Handled := False;
fOnMessage.Call(Msg,Handled,InSendMessage);
If not Handled then
  Msg.Result := DefWindowProc(fWindowHandle,Msg.Msg,Msg.wParam,Msg.lParam);
end;

//------------------------------------------------------------------------------

Function TUtilityWindow.ProcessMessageInternal(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean;
type
  TGetMsgResult = (gmrMessage,gmrQuit,gmrFailure);

  Function GetMessageWrapper(var Msg: TMsg): TGetMsgResult;
  begin
  {
    GetMessage can return 0, -1 or other non-zero value.

      - when a message other than WM_QUIT was received, it returns non-zero value other than -1
      - when WM_QUIT was received, it returns 0
      - on error it will return -1
  }
    case Integer(GetMessage(Msg,fWindowHandle,0,0)) of
      -1: Result := gmrFailure;
       0: Result := gmrQuit;
    else
      Result := gmrMessage;
    end;
  end;

var
  Msg:  TMsg;
begin
{
  GetMessage does not return until some message is placed in the message queue,
  that is, it was posted to the window, not sent. But, while the GetMessage is
  blocking, it can and will dispatch sent messages.

  PeekMessage, when called, will dispatch all sent messages, then retrieves
  posted message, if any exists, and then returns. If there is no sent or
  queued message, it returns immediately.

  This means the thread cannot respond to sent messages unless it calls
  PeekMessage or is currently waiting on GetMessage.
}
Result := True;
ReceivedQuitMessage := False;
FillChar(Addr(Msg)^,SizeOf(TMsg),0);
If WaitForMessage then
  begin
    case GetMessageWrapper(Msg) of
      gmrMessage: begin
                    TranslateMessage(Msg);
                    DispatchMessage(Msg);
                  end;
      gmrQuit:    ReceivedQuitMessage := True;
      gmrFailure: raise EUWSystemEror.CreateFmt('TUtilityWindow.ProcessMessageInternal:' +
                    ' Failed to retrieve a message (%d).',[GetLastError]);
    end;
  end
else
  begin
    If PeekMessage(Msg,fWindowHandle,0,0,PM_REMOVE) then
      begin
        If Msg.message <> WM_QUIT then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end
        else ReceivedQuitMessage := True;
      end
    else Result := False;
  end;
end;

{-------------------------------------------------------------------------------
    TUtilityWindow - public methods
-------------------------------------------------------------------------------}

constructor TUtilityWindow.Create;
begin
inherited;
fOnMessage := TMulticastMessageEvent.Create(Self);
fWindowHandle := WndAlloc.AllocateHWND(WndProc);
end;

//------------------------------------------------------------------------------

destructor TUtilityWindow.Destroy;
begin
WndAlloc.DeallocateHWND(fWindowHandle);
fOnMessage.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindow.BreakProcessing;
begin
fContinueProcessing := False;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindow.ProcessMessage(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean);
begin
ProcessMessageInternal(WaitForMessage,ReceivedQuitMessage);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TUtilityWindow.ProcessMessage(WaitForMessage: Boolean = False);
var
  ReceivedQuitMessage:  Boolean;
begin
ProcessMessage(WaitForMessage,ReceivedQuitMessage);
end;

//------------------------------------------------------------------------------

Function TUtilityWindow.ProcessMessages(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean;
begin
Result := True;
fContinueProcessing := True;
If ProcessMessageInternal(WaitForMessage,ReceivedQuitMessage) then
  begin
    If not ReceivedQuitMessage and fContinueProcessing then
      begin
        while ProcessMessageInternal(False,ReceivedQuitMessage) do  // peek remaining messages
          If ReceivedQuitMessage or not fContinueProcessing then
            begin
              Result := False;
              Break{while};
            end;
      end
    else Result := False;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUtilityWindow.ProcessMessages(WaitForMessage: Boolean = False): Boolean;
var
  ReceivedQuitMessage:  Boolean;
begin
Result := ProcessMessages(WaitForMessage,ReceivedQuitMessage);
end;

//------------------------------------------------------------------------------

Function TUtilityWindow.ContinuousProcessMessages: Boolean;
var
  ReceivedQuitMessage:  Boolean;
begin
fContinueProcessing := True;
while ProcessMessageInternal(True,ReceivedQuitMessage) do
  If ReceivedQuitMessage or not fContinueProcessing then Break{while};
Result := ReceivedQuitMessage;
end;

end.
