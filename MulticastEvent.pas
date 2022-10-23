{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Multicast event management classes

  Version 1.1 (2019-09-30)

  Last change 2022-09-13

  ©2015-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MulticastEvent

  Dependencies:
    AuxTypes   - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses - github.com/TheLazyTomcat/Lib.AuxClasses

===============================================================================}
unit MulticastEvent;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxClasses;

type
  EMCEException = class(Exception);

  EMCEIndexOutOfBounds = class(EMCEException);

{===============================================================================
--------------------------------------------------------------------------------
                                TMulticastEvent                                
--------------------------------------------------------------------------------
===============================================================================}
type
  TCallback = procedure;
  TEvent    = procedure of object;

  TMulticastEntry = record
    case IsMethod: Boolean of
      False:  (HandlerProcedure: TProcedure);
      True:   (HandlerMethod:    TMethod);
  end;

{===============================================================================
    TMulticastEvent - class declaration
===============================================================================}
type
  TMulticastEvent = class(TCustomListObject)
  protected
    fOwner:   TObject;
    fEntries: array of TMulticastEntry;
    fCount:   Integer;
    Function GetEntry(Index: Integer): TMulticastEntry; virtual;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
  public
    constructor Create(Owner: TObject = nil);
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function IndexOf(const Handler: TCallback): Integer; overload; virtual;
    Function IndexOf(const Handler: TEvent): Integer; overload; virtual;
    Function Add(const Handler: TCallback; AllowDuplicity: Boolean = False): Integer; overload; virtual;
    Function Add(const Handler: TEvent; AllowDuplicity: Boolean = False): Integer; overload; virtual;
    Function Remove(const Handler: TCallback; RemoveAll: Boolean = True): Integer; overload; virtual;
    Function Remove(const Handler: TEvent; RemoveAll: Boolean = True): Integer; overload; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure Call; overload; virtual;
    property Entries[Index: Integer]: TMulticastEntry read GetEntry;
    property Owner: TObject read fOwner;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TMulticastNotifyEvent                                                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMulticastNotifyEvent - class declaration
===============================================================================}
type
  TMulticastNotifyEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TNotifyCallback): Integer; reintroduce; overload;
    Function IndexOf(const Handler: TNotifyEvent): Integer; reintroduce; overload;
    Function Add(const Handler: TNotifyCallback; AllowDuplicity: Boolean = False): Integer; reintroduce; overload;
    Function Add(const Handler: TNotifyEvent; AllowDuplicity: Boolean = False): Integer; reintroduce; overload;
    Function Remove(const Handler: TNotifyCallback; RemoveAll: Boolean = True): Integer; reintroduce; overload;
    Function Remove(const Handler: TNotifyEvent; RemoveAll: Boolean = True): Integer; reintroduce; overload;
    procedure Call(Sender: TObject); reintroduce;
  end;

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                TMulticastEvent                                
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMulticastEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMulticastEvent - protected methods
-------------------------------------------------------------------------------}

Function TMulticastEvent.GetEntry(Index: Integer): TMulticastEntry;
begin
If CheckIndex(Index) then
  Result := fEntries[Index]
else
  raise EMCEIndexOutOfBounds.CreateFmt('TMulticastEvent.GetEntry: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.GetCapacity: Integer;
begin
Result := Length(fEntries);
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.SetCapacity(Value: Integer);
begin
If Value <> Length(fEntries) then
  begin
    If Value < Length(fEntries) then
      fCount := Value;
    SetLength(fEntries,Value);
  end;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMulticastEvent.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TMulticastEvent - public methods
-------------------------------------------------------------------------------}

constructor TMulticastEvent.Create(Owner: TObject = nil);
begin
inherited Create;
fOwner := Owner;
SetLength(fEntries,0);
fCount := 0;
// adjust growing, no need for fast growth
GrowMode := gmLinear;
GrowFactor := 16;
end;

//------------------------------------------------------------------------------

destructor TMulticastEvent.Destroy;
begin
Clear;
inherited;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.LowIndex: Integer;
begin
Result := Low(fEntries);
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.IndexOf(const Handler: TCallback): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If not fEntries[i].IsMethod then
    If @fEntries[i].HandlerProcedure = @Handler then
      begin
        Result := i;
        Break{For i};
      end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastEvent.IndexOf(const Handler: TEvent): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fEntries[i].IsMethod then
    If (fEntries[i].HandlerMethod.Code = TMethod(Handler).Code) and
       (fEntries[i].HandlerMethod.Data = TMethod(Handler).Data) then
      begin
        Result := i;
        Break{For i};
      end;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.Add(const Handler: TCallback; AllowDuplicity: Boolean = False): Integer;
begin
If Assigned(Handler) then
  begin
    Result := IndexOf(Handler);
    If (Result < 0) or AllowDuplicity then
      begin
        Grow;
        Result := fCount;
        fEntries[Result].IsMethod := False;
        fEntries[Result].HandlerProcedure := TProcedure(Handler);
        Inc(fCount);
      end;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastEvent.Add(const Handler: TEvent; AllowDuplicity: Boolean = False): Integer;
begin
If Assigned(TMethod(Handler).Code) and Assigned(TMethod(Handler).Data) then
  begin
    Result := IndexOf(Handler);
    If (Result < 0) or AllowDuplicity then
      begin
        Grow;
        Result := fCount;
        fEntries[Result].IsMethod := True;
        fEntries[Result].HandlerMethod := TMethod(Handler);
        Inc(fCount);
      end;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TMulticastEvent.Remove(const Handler: TCallback; RemoveAll: Boolean = True): Integer;
begin
repeat
  Result := IndexOf(Handler);
  If Result >= 0 then
    Delete(Result);
until not RemoveAll or (Result < 0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastEvent.Remove(const Handler: TEvent; RemoveAll: Boolean = True): Integer;
begin
repeat
  Result := IndexOf(Handler);
  If Result >= 0 then
    Delete(Result);
until not RemoveAll or (Result < 0);
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    For i := Index to Pred(HighIndex) do
      fEntries[i] := fEntries[i + 1];
    Dec(fCount);
    Shrink;
  end
else raise EMCEIndexOutOfBounds.CreateFmt('TMulticastEvent.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Clear;
begin
fCount := 0;
Shrink;
end;

//------------------------------------------------------------------------------

procedure TMulticastEvent.Call;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If fEntries[i].IsMethod then
    TEvent(fEntries[i].HandlerMethod)
  else
    TCallback(fEntries[i].HandlerProcedure);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             TMulticastNotifyEvent                                                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMulticastNotifyEvent - class implementation
===============================================================================}  
{-------------------------------------------------------------------------------
    TMulticastNotifyEvent - public methods
-------------------------------------------------------------------------------}

Function TMulticastNotifyEvent.IndexOf(const Handler: TNotifyCallback): Integer;
begin
Result := inherited IndexOf(TCallback(Handler));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastNotifyEvent.IndexOf(const Handler: TNotifyEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TMulticastNotifyEvent.Add(const Handler: TNotifyCallback; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TCallback(Handler),AllowDuplicity);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastNotifyEvent.Add(const Handler: TNotifyEvent; AllowDuplicity: Boolean = False): Integer;
begin
Result := inherited Add(TEvent(Handler),AllowDuplicity);
end;

//------------------------------------------------------------------------------

Function TMulticastNotifyEvent.Remove(const Handler: TNotifyCallback; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TCallback(Handler),RemoveAll);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMulticastNotifyEvent.Remove(const Handler: TNotifyEvent; RemoveAll: Boolean = True): Integer;
begin
Result := inherited Remove(TEvent(Handler),RemoveAll);
end;

//------------------------------------------------------------------------------

procedure TMulticastNotifyEvent.Call(Sender: TObject);
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If fEntries[i].IsMethod then
    TNotifyEvent(fEntries[i].HandlerMethod)(Sender)
  else
    TNotifyCallback(fEntries[i].HandlerProcedure)(Sender);
end;

end.
