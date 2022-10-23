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

  Version 1.4.1 (2021-11-26)

  Last change 2022-09-14

  ©2015-2022 František Milt

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
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
    MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent
    WndAlloc       - github.com/TheLazyTomcat/Lib.WndAlloc
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

    SimpleCPUID is required only when PurePascal symbol is not defined.

===============================================================================}
unit UtilityWindow;

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
  AuxClasses, MulticastEvent;

type
  EUWException = class(Exception);

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

    It means the handling of the message was not called from method
    ProcessMessages via a call to DispatchMessage, but directly by the system.

    This also means calling BreakProcessing has no immediate effect, it just
    resets the Continue flag.

    When it is true, you must take care what are you doing, because it is
    possible to cause an application deadlock (eg. don't call SendMessage).    
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
    fWindowHandle:      HWND;
    fMessageProcessed:  Boolean;  // internal, do not publish
    fContinue:          Boolean;
    fOnMessage:         TMulticastMessageEvent;
    procedure WndProc(var Msg: TMessage); virtual;
    Function ProcessMessagesInternal(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  {
    BreakProcessing

      This method, when called, will cause method ContinuousProcessMessages
      to exit before all messages has been processed.

      Note that if it is called from message handler and the processed message
      was sent, rather than posted, this method has no immediate effect.
      It takes effect only after processing at least one posted message.
  }
    procedure BreakProcessing; virtual;
  {
    ProcessMessages

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

      Return value is set to true when at least one sent or posted message is
      dispatched, otherwise it is false.
  }
    Function ProcessMessages(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean; overload; virtual;
    Function ProcessMessages(WaitForMessage: Boolean = False): Boolean; overload; virtual;
  {
    ContinuousProcessMessages

      Repeatedly calls ProcessMessages as long as that method returns true
      (ie. it is actually processing something).
      After each call it also checks whether WM_QUIT message was received or
      whether the processing was not terminated by a call to method
      BreakProcessing. If either of these is true, it returns.

      When WaitForMessage is set to true, this method will not return until
      WM_QUIT is received or the processing is cancelled using BreakProcessing.

      When set to false, the function will return after processing all pending
      sent messages and all posted messages or after receiving WM_QUIT
      or breaking processing using BreakProcessing.
  }
    procedure ContinuousProcessMessages(WaitForMessage: Boolean = False); virtual;
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
fMessageProcessed := True;
Handled := False;
fOnMessage.Call(Msg,Handled,InSendMessage);
If not Handled then
  Msg.Result := DefWindowProc(fWindowHandle,Msg.Msg,Msg.wParam,Msg.lParam);
end;

//------------------------------------------------------------------------------

Function TUtilityWindow.ProcessMessagesInternal(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean;
var
  Msg:    TMsg;
  GetRes: Integer;

  Function GetMessageWrapper(out IntResult: Integer): Boolean;
  begin
  {
    GetMessage can return 0, -1 or other non-zero value.

      - when a message other than WM_QUIT was received, it returns non-zero value other than -1
      - when WM_QUIT was received, it returns 0
      - on error it will return -1
  }
    IntResult := Integer(GetMessage(Msg,fWindowHandle,0,0));
    Result := IntResult <> 0;
  end;

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
FillChar(Addr(Msg)^,SizeOf(TMsg),0);
ReceivedQuitMessage := False;
fMessageProcessed := False;
If fContinue then
  begin
    If WaitForMessage then
      begin
        If GetMessageWrapper(GetRes) then
          begin
            If GetRes <> -1 then
              begin
                TranslateMessage(Msg);
                DispatchMessage(Msg);
              end
            else raise EUWException.CreateFmt('TUtilityWindow.ProcessMessages: Failed to retrieve a message (0x%.8x).',[GetLastError]);
          end
        else ReceivedQuitMessage := True;
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
          end;
      end;
  end;
Result := fMessageProcessed;
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
fContinue := False;
end;

//------------------------------------------------------------------------------

Function TUtilityWindow.ProcessMessages(WaitForMessage: Boolean; out ReceivedQuitMessage: Boolean): Boolean;
begin
fContinue := True;
Result := ProcessMessagesInternal(WaitForMessage,ReceivedQuitMessage);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUtilityWindow.ProcessMessages(WaitForMessage: Boolean = False): Boolean;
var
  QuitReceived: Boolean;
begin
fContinue := True;
Result := ProcessMessagesInternal(WaitForMessage,QuitReceived);
end;

//------------------------------------------------------------------------------

procedure TUtilityWindow.ContinuousProcessMessages(WaitForMessage: Boolean = False);
var
  QuitReceived: Boolean;
begin
fContinue := True;
while ProcessMessagesInternal(WaitForMessage,QuitReceived) do
  If not fContinue or QuitReceived then Break{while...};
end;

end.
