{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SimpleLog

    Very simple class designed to ease logging. It can log into internal
    string list, several external objects (TStrings descendants), file,
    user-provided stream, or write into console if one is present.

    There is also a function which allows capture of console - a log object
    binds (attaches itself to) current console and captures all input and
    output text that happens to be put into console via standard functions
    (Write(Ln), Read(Ln)). This text is then logged as usual.

  Version 1.4 (2020-11-24)

  Last change 2022-09-13

  ©2012-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Simplelog

  Dependencies:
    AuxTypes   - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect    - github.com/TheLazyTomcat/Lib.StrRect

===============================================================================}
unit SimpleLog;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESLException = class(Exception);

  ESLIndexOutOfBounds = class(ESLException);
  ESLInvalidValue     = class(ESLException);

{===============================================================================
    Auxiliary routines - declaration
===============================================================================}

procedure InitFormatSettings(out FormatSettings: TFormatSettings);

{===============================================================================
--------------------------------------------------------------------------------
                                Console binding
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Console binding - declaration
===============================================================================}

Function ConsoleIsBinded: Boolean;
Function ConsoleBind(const LogFileName: String): Boolean;
procedure ConsoleUnbind;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSimpleLog
--------------------------------------------------------------------------------
===============================================================================}
type
  TSLLogOutput = (loInternal,loStream,loFile,loConsole,loExternals);

  TSLLogOutputs = set of TSLLogOutput;

  TSLSettings = record
    Outputs:            TSLLogOutputs;
    FormatSettings:     TFormatSettings;
    TimeFormat:         String;
    TimeSeparator:      String;
    ForceTime:          Boolean;
    ForceTimeAutoreset: Boolean;    
    ForcedTime:         TDateTime;
    IndentLines:        Boolean;
  end;

  TSLStrings = record
    BreakerCharThin:  Char;
    BreakerCharThick: Char;
    BreakerLength:    Integer;
    TimeStamp:        String;
    StartStamp:       String;
    EndStamp:         String;
    AppendStamp:      String;
    HeaderText:       String;
  end;

  // TSLExternalLogItem is for internal use only.
  TSLExternalLogItem = record
    LogObject:  TStrings;
    Active:     Boolean;
    Owned:      Boolean;
  end;

{===============================================================================
    TSimpleLog - class declaration
===============================================================================}
type
  TSimpleLog = class(TCustomListObject)
  protected
    // settings and info fields
    fSettings:            TSLSettings;
    fStrings:             TSLStrings;
    fTimeOfCreation:      TDateTime;
    fLogCounter:          UInt32;
    // log output fields
    fInternalLog:         TStringList;
    fStreamLog:           TStream;
    fFileLog:             String;
    fFileLogStream:       TFileStream;  // only internal, do not publish
    fConsolePresent:      Boolean;     
    fExternalLogs:        array of TSLExternalLogItem;
    fExternalLogCount:    Integer;
    // console binding fields
    fConsoleBinded:       Boolean;
    fOriginalErrOutput:   TTextRec;
    fOriginalOutput:      TTextRec;
    fOriginalInput:       TTextRec;
    // event/callback properties
    fOnLogEvent:          TStringEvent;
    fOnLogCallback:       TStringCallback;
    // getters, setters
    Function GetExternalLog(Index: Integer): TStrings; virtual;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // init/final
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // internal logging methods
    Function GetTime: TDateTime; virtual;
    Function GetTimeString(Time: TDateTime): String; virtual;
    Function GetIndentedString(const Str: String; IndentCount: Integer): String; virtual;
    Function GetStampStr(const StampText: String; ThickBreak: Boolean): String;
    procedure WriteLogToOutputs(const LogText: String; LineBreakInStreams: Boolean); virtual;
    procedure ProcessConsoleLog(const LogText: String); virtual;
    procedure ProcessLocalLog(const LogText: String; IndentCount: Integer = 0); virtual;
    // events
    procedure DoOnLog(const LogText: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // output setup methods
  {
    OutputIsActive returns true when selected output is active, false otherwise.

    But note the fact that some output is active does not necessarily mean
    writing to that output will be performed, there are checks done in each
    write which might prevent it.
  }
    Function OutputIsActive(Output: TSLLogOutput): Boolean; virtual;
  {
    OutputActivate activates selected output method and returns its previous
    state.
  }
    Function OutputActivate(Output: TSLLogOutput): Boolean; virtual;
    Function OutputDeactivate(Output: TSLLogOutput): Boolean; virtual;
    procedure SetupOutputToStream(Stream: TStream; Append: Boolean; Activate: Boolean = True); virtual;
    procedure SetupOutputToFile(const FileName: String; Append: Boolean; Activate: Boolean = True); virtual;
    // external logs list methods
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function ExternalLogLowIndex: Integer; virtual;
    Function ExternalLogHighIndex: Integer; virtual;
    Function ExternalLogIndexOf(LogObject: TStrings): Integer; virtual;
    Function ExternalLogAdd(LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False): Integer; virtual;
    procedure ExternalLogInsert(Index: Integer; LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False); virtual;
    Function ExternalLogExtract(LogObject: TStrings): TStrings; virtual;
    Function ExternalLogRemove(LogObject: TStrings): Integer; virtual;
    procedure ExternalLogDelete(Index: Integer); virtual;
    procedure ExternalLogClear; virtual;
    Function ExternalLogIsActive(Index: Integer): Boolean; virtual;
  {
    ExternalLogSetActive returns previous state.
  }
    Function ExternalLogSetActive(Index: Integer; Active: Boolean): Boolean; virtual;
    Function ExternalLogIsOwned(Index: Integer): Boolean; virtual;
  {
    ExternalLogSetOwned returns previous state.
  }
    Function ExternalLogSetOwned(Index: Integer; Owned: Boolean): Boolean; virtual;
    // public logging methods
    Function ForceTimeSet(Time: TDateTime; Autoreset: Boolean = False): Boolean; virtual;
    procedure AddLogNoTime(const LogText: String); virtual;
    procedure AddLogTime(const LogText: String; Time: TDateTime); virtual;
    procedure AddLog(const LogText: String); virtual;
    procedure AddEmpty; virtual;
    procedure AddBreaker; virtual;
    procedure AddBreakerThin; virtual;
    procedure AddBreakerThick; virtual;
    procedure AddTimeStamp; virtual;
    procedure AddStartStamp; virtual;
    procedure AddEndStamp; virtual;
    procedure AddAppendStamp; virtual;
    procedure AddHeader; virtual;
    // console binding
  {
    Note that if the console is binded, then output to console is disabled.
  }
    Function BindConsole: Boolean; virtual;
    procedure UnbindConsole; virtual;
    // settings properties
    property Settings: TSLSettings read fSettings;
    // to (de)activate individual log outputs, use methods ActivateOutput and DeactivateOutput
    property Outputs: TSLLogOutputs read fSettings.Outputs write fSettings.Outputs;
    property FormatSettings: TFormatSettings read fSettings.FormatSettings write fSettings.FormatSettings;
    property TimeFormat: String read fSettings.TimeFormat write fSettings.TimeFormat;
    property TimeSeparator: String read fSettings.TimeSeparator write fSettings.TimeSeparator;
    property ForceTime: Boolean read fSettings.ForceTime write fSettings.ForceTime;
    property ForceTimeAutoreset: Boolean read fSettings.ForceTimeAutoreset write fSettings.ForceTimeAutoreset;    
    property ForcedTime: TDateTime read fSettings.ForcedTime write fSettings.ForcedTime;
    property IndentLines: Boolean read fSettings.IndentLines write fSettings.IndentLines;
    // strings properties
    property Strings: TSLStrings read fStrings;
    property BreakerThin: Char read fStrings.BreakerCharThin write fStrings.BreakerCharThin;
    property BreakerThick: Char read fStrings.BreakerCharThick write fStrings.BreakerCharThick;
    property BreakerLength: Integer read fStrings.BreakerLength write fStrings.BreakerLength;
    property TimeStamp: String read fStrings.TimeStamp write fStrings.TimeStamp;
    property StartStamp: String read fStrings.StartStamp write fStrings.StartStamp;
    property EndStamp: String read fStrings.EndStamp write fStrings.EndStamp;
    property AppendStamp: String read fStrings.AppendStamp write fStrings.AppendStamp;
    property HeaderText: String read fStrings.HeaderText write fStrings.HeaderText;
    // informative properties
    property TimeOfCreation: TDateTime read fTimeOfCreation;
    property LogCounter: UInt32 read fLogCounter;
    // log output properties
    property InternalLog: TStringList read fInternalLog write fInternalLog;
    property StreamLog: TStream read fStreamLog;
    property FileLog: String read fFileLog;
    property ConsolePresent: Boolean read fConsolePresent;
    property ExternalLogs[Index: Integer]: TStrings read GetExternalLog; default;
    property ExternalLogCount: Integer read GetCount;
    property Capacity: Integer read GetCapacity;  // redeclaration to make the property read-only
    property Count: Integer read GetCount;        // -//-
    // console binding
    property ConsoleBinded: Boolean read fConsoleBinded;
    // events/callbacks properties
    property OnLogEvent: TStringEvent read fOnLogEvent write fOnLogEvent;
    property OnLogCallback: TStringCallback read fOnLogCallback write fOnLogCallback;
    property OnLog: TStringEvent read fOnLogEvent write fOnLogEvent;
  end;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}BaseUnix,{$ENDIF}
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Auxiliary routines - implementation
===============================================================================}

procedure InitFormatSettings(out FormatSettings: TFormatSettings);
begin
{$WARN SYMBOL_PLATFORM OFF}
{$IF not Defined(FPC) and (CompilerVersion >= 18)}
// Delphi 2006+
FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
{$ELSE}
// older delphi and FPC
{$IFDEF Windows}
// windows
{$IFDEF FPC}
FillChar(Addr(FormatSettings)^,Sizeof(TFormatSettings),0);
{$ENDIF}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,FormatSettings);
{$ELSE}
// non-windows
FormatSettings := DefaultFormatSettings;
{$ENDIF}
{$IFEND}
{$WARN SYMBOL_PLATFORM ON}
end;

{===============================================================================
--------------------------------------------------------------------------------
                                Console binding
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Console binding - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Console binding - internal routines
-------------------------------------------------------------------------------}
type
  TSLCB_IOFunc = Function(var F: TTextRec): Integer;

const
  SLCB_ERROR_SUCCESS                 = 0;
  SLCB_ERROR_UNSUPPORTED_MODE        = 10;
  SLCB_ERROR_WRITE_FAILED            = 11;
  SLCB_ERROR_READ_FAILED             = 12;
  SLCB_ERROR_FLUSH_FUNC_NOT_ASSIGNED = 13;

  SLCB_USERDATAINDEX_OBJECT = Low(TTextRec(nil^).UserData);

  SLCB_STATUS_LOCKED   = 1;
  SLCB_STATUS_UNLOCKED = 0;

var
  SLCB_StatusWord: Integer = SLCB_STATUS_UNLOCKED;

threadvar
  SLCB_BindedLogObject: TSimpleLog;

//==============================================================================

Function SLCB_WriteConsole(Handle: THandle; Ptr: Pointer; CharsToWrite: TStrSize; out CharsWritten: TStrSize): Boolean;
{$IFDEF Windows}
var
  WrittenChars: DWORD;
begin
{$IFDEF FPC}
WrittenChars := 0;
{$ENDIF}
Result := Windows.WriteConsole(Handle,Ptr,DWORD(CharsToWrite),WrittenChars,nil);
CharsWritten := TStrSize(WrittenChars);
end;
{$ELSE}
begin
CharsWritten := TStrSize(fpWrite(cInt(Handle),Ptr^,TSize(CharsToWrite)));
Result := CharsWritten >= 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SLCB_ReadConsole(Handle: THandle; Ptr: Pointer; CharsToRead: TStrSize; out CharsRead: TStrSize): Boolean;
{$IFDEF Windows}
var
  ReadChars:  DWORD;
begin
{$IFDEF FPC}
ReadChars := 0;
{$ENDIF}
Result := Windows.ReadConsole(Handle,Ptr,DWORD(CharsToRead),ReadChars,nil);
CharsRead := TStrSize(ReadChars);
end;
{$ELSE}
begin
CharsRead := TStrSize(fpRead(cInt(Handle),Ptr^,TSize(CharsToRead)));
Result := CharsRead >= 0;
end;
{$ENDIF}

//==============================================================================

Function SLCB_Output(var F: TTextRec): Integer;
{
  Take whatever is in the text buffer and pass it to both system call (which
  will do the output into console) and simple log object stored in user data.

  Note that the text buffer always consists of single-byte characters, even
  when compiled with unicode.
}
var
  CharsWritten: TStrSize;
  ConsoleText:  AnsiString;
{$IF Defined(Unicode) and Defined(Windows)} // afaik the console cannot be wide-char in linux (?? :/)
  WideText:     WideString;
{$IFEND}
begin
SetLength(ConsoleText,F.BufPos);
Move(F.Buffer,PAnsiChar(ConsoleText)^,F.BufPos);
{$IF Defined(Unicode) and Defined(Windows)}
{
  Text in text buffer is single byte, but we must pass pointer to unicode text.
  So copy data from text buffer into ansi string, convert it to wide string
  and then pass reference to this wide string.
}
WideText := StrToWide(AnsiToStr(ConsoleText));
If SLCB_WriteConsole(F.Handle,PWideChar(WideText),Length(WideText),CharsWritten) then
  begin
    If CharsWritten = TStrSize(Length(WideText)) then
      begin
        TSimpleLog(Addr(F.UserData[SLCB_USERDATAINDEX_OBJECT])^).
          ProcessConsoleLog(AnsiToStr(ConsoleText));
{$ELSE}
If SLCB_WriteConsole(F.Handle,F.BufPtr,F.BufPos,CharsWritten) then
  begin
    If CharsWritten = TStrSize(F.BufPos) then
      begin
        TSimpleLog(Addr(F.UserData[SLCB_USERDATAINDEX_OBJECT])^).
          ProcessConsoleLog(CslToStr(ConsoleText));
{$IFEND}
        Result := SLCB_ERROR_SUCCESS;
      end
    else Result := SLCB_ERROR_WRITE_FAILED;
  end
else Result := SLCB_ERROR_WRITE_FAILED;
F.BufPos := 0;
end;

//------------------------------------------------------------------------------

Function SLCB_Input(var F: TTextRec): Integer;
var
  CharsRead:    TStrSize;
  ConsoleText:  AnsiString;
{$IF Defined(Unicode) and Defined(Windows)}
  WideText:     WideString;
begin
{
  ReadConsole loads wide string, but since the text buffer accepts only
  single-byte strings, it must be converted.

  Note that only BufSize/2 characters is read - this is to be sure that the
  converted string will fit into the text buffer.
}
SetLength(WideText,F.BufSize div 2);
If SLCB_ReadConsole(F.Handle,PWideChar(WideText),Length(WideText),CharsRead) then
  begin
    SetLength(WideText,CharsRead);
    ConsoleText := StrToAnsi(WideToStr(WideText));
    If Length(ConsoleText) <= Integer(F.BufSize) then
      begin
        Move(PAnsiChar(ConsoleText)^,F.Buffer,Length(ConsoleText));
        TSimpleLog(Addr(F.UserData[SLCB_USERDATAINDEX_OBJECT])^).ProcessConsoleLog(WideToStr(WideText));
        F.BufEnd := Length(ConsoleText);
        Result := SLCB_ERROR_SUCCESS;
      end
    else Result := SLCB_ERROR_READ_FAILED;
  end
{$ELSE}
begin
If SLCB_ReadConsole(F.Handle,F.BufPtr,F.BufSize,CharsRead) then
  begin
    SetLength(ConsoleText,CharsRead);
    Move(F.Buffer,PAnsiChar(ConsoleText)^,CharsRead);
    TSimpleLog(Addr(F.UserData[SLCB_USERDATAINDEX_OBJECT])^).ProcessConsoleLog(CslToStr(ConsoleText));
    F.BufEnd := CharsRead;
    Result := SLCB_ERROR_SUCCESS;
  end
{$IFEND}
else Result := SLCB_ERROR_READ_FAILED;
F.BufPos := 0;
end;

//------------------------------------------------------------------------------

Function SLCB_Flush(var F: TTextRec): Integer;
begin
case F.Mode of
  fmOutput: begin
              If Assigned(F.InOutFunc) then
                TSLCB_IOFunc(F.InOutFunc)(F);
              Result := SLCB_ERROR_SUCCESS;
            end;
  fmInput:  begin
              F.BufPos := 0;
              F.BufEnd := 0;
              Result := SLCB_ERROR_SUCCESS;
            end;
else
  Result := SLCB_ERROR_UNSUPPORTED_MODE;
end;
end;

//------------------------------------------------------------------------------

Function SLCB_Open(var F: TTextRec): Integer;
begin
case F.Mode of
  fmOutput: begin
            {$IFDEF Windows}
              F.Handle := GetStdHandle(STD_OUTPUT_HANDLE);
            {$ELSE}
              F.Handle := StdOutputHandle;
            {$ENDIF}
              F.InOutFunc := @SLCB_Output;
              Result := SLCB_ERROR_SUCCESS;
            end;
  fmInput:  begin
            {$IFDEF Windows}
              F.Handle := GetStdHandle(STD_INPUT_HANDLE);
            {$ELSE}
              F.Handle := StdInputHandle;
            {$ENDIF}
              F.InOutFunc := @SLCB_Input;
              Result := SLCB_ERROR_SUCCESS;
            end;
else
  Result := SLCB_ERROR_UNSUPPORTED_MODE;
end;
end;

//------------------------------------------------------------------------------

Function SLCB_Close(var F: TTextRec): Integer;
begin
If Assigned(F.FlushFunc) then
  Result := TSLCB_IOFunc(F.FlushFunc)(F)
else
  Result := SLCB_ERROR_FLUSH_FUNC_NOT_ASSIGNED;
F.Mode := fmClosed;
end;

//------------------------------------------------------------------------------

procedure SLCB_BindLogObject(var T: Text; LogObject: TSimpleLog);
begin
with TTextRec(T) do
  begin
    Mode := fmClosed;
  {$IFDEF FPC}
    LineEnd := sLineBreak;
  {$ELSE}
    {$IFDEF Windows}Flags := tfCRLF{$ENDIF};
  {$ENDIF}    
    BufSize := SizeOf(Buffer);
    BufPos := 0;
    BufEnd := 0;
    BufPtr := @Buffer;
    OpenFunc := @SLCB_Open;
    FlushFunc := @SLCB_Flush;
    CloseFunc := @SLCB_Close;
    TSimpleLog(Addr(UserData[SLCB_USERDATAINDEX_OBJECT])^) := LogObject;
  {$IF not Defined(FPC) and Defined(Windows) and Defined(Unicode)}
  {
    I have no idea when this field was added. But I am assuming if Delphi has
    unicode support, this field is already there.
  }
    CodePage := CP_ACP;
  {$IFEND}
    Name := '';
  end;
end;

{-------------------------------------------------------------------------------
    Console binding - public routines
-------------------------------------------------------------------------------}

Function ConsoleIsBinded: Boolean;
begin
If Assigned(SLCB_BindedLogObject) then
  Result := SLCB_BindedLogObject.ConsoleBinded
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function ConsoleBind(const LogFileName: String): Boolean;
var
  ObjTemp:  TSimpleLog;
begin
Result := False;
ObjTemp := TSimpleLog.Create;
try
  ObjTemp.Outputs := [];
  If ObjTemp.BindConsole then
    begin
      ObjTemp.SetupOutputToFile(LogFileName,False,True);
      If ObjTemp.OutputIsActive(loFile) then
        begin
          SLCB_BindedLogObject := ObjTemp;
          Result := True;
        end
      else FreeAndNil(ObjTemp);      
    end
  else FreeAndNil(ObjTemp);
except
  FreeAndNil(ObjTemp);
  raise;
end;
end;

//------------------------------------------------------------------------------

procedure ConsoleUnbind;
begin
If Assigned(SLCB_BindedLogObject) then
  FreeAndNil(SLCB_BindedLogObject); // this will automatically unbind
end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSimpleLog                                    
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleLog - implementation constants
===============================================================================}
const
  SL_DEFSTR_TIMEFORMAT    = 'yyyy-mm-dd hh:nn:ss.zzz';
  SL_DEFSTR_TIMESEPARATOR = ' //: ';

  SL_DEFSTR_BREAKERCHAR_THIN  = '-';
  SL_DEFSTR_BREAKERCHAR_THICK = '=';

  SL_DEFSTR_BREAKER_LENGTH = 80;

  SL_DEFSTR_TIMESTAMP   = '%s';
  SL_DEFSTR_STARTSTAMP  = '%s - Starting log';
  SL_DEFSTR_ENDSTAMP    = '%s - Ending log';
  SL_DEFSTR_APPENDSTAMP = '%s - Appending log';

  // those plus must be there...
  SL_DEFSTR_HEADERTEXT: WideString = 'SimpleLog 2.0, ' + #$00A9 + '2015-2021 Franti' + #$0161 + 'ek Milt';

{===============================================================================
    TSimpleLog - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleLog - protected methods
-------------------------------------------------------------------------------}

Function TSimpleLog.GetExternalLog(Index: Integer): TStrings;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].LogObject
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.GetExternalLog: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetCapacity: Integer;
begin
Result := Length(fExternalLogs);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  begin
    If Value <> Length(fExternalLogs) then
      begin
        // Removing existing assigned items? If so, free owned objects.
        If Value < Count then
          begin
            For i := Value to HighIndex do
              If fExternalLogs[i].Owned then
                FreeAndNil(fExternalLogs[i].LogObject);
            fExternalLogCount := Value;
          end;
        SetLength(fExternalLogs,Value);
      end;
  end
else raise ESLInvalidValue.CreateFmt('TSimpleLog.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetCount: Integer;
begin
Result := fExternalLogCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleLog.SetCount(Value: Integer);
begin
// nothing to do, count is read only
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleLog.Initialize;
begin
// init settings
fSettings.Outputs := [loInternal];
InitFormatSettings(fSettings.FormatSettings);
fSettings.TimeFormat := SL_DEFSTR_TIMEFORMAT;
fSettings.TimeSeparator := SL_DEFSTR_TIMESEPARATOR;
fSettings.ForceTime := False;
fSettings.ForceTimeAutoreset := False;
fSettings.ForcedTime := Now;
fSettings.IndentLines := False;
// init strings
fStrings.BreakerCharThin := SL_DEFSTR_BREAKERCHAR_THIN;
fStrings.BreakerCharThick := SL_DEFSTR_BREAKERCHAR_THICK;
fStrings.BreakerLength := SL_DEFSTR_BREAKER_LENGTH;
fStrings.TimeStamp := SL_DEFSTR_TIMESTAMP;
fStrings.StartStamp := SL_DEFSTR_STARTSTAMP;
fStrings.EndStamp := SL_DEFSTR_ENDSTAMP;
fStrings.AppendStamp := SL_DEFSTR_APPENDSTAMP;
fStrings.HeaderText := WideToStr(SL_DEFSTR_HEADERTEXT);
// init other stuff
fTimeOfCreation := Now;
fLogCounter := 0;
fInternalLog := TStringList.Create;
fStreamLog := nil;
fFileLog := '';
fFileLogStream := nil;
fConsolePresent := System.IsConsole;
SetLength(fExternalLogs,0);
fExternalLogCount := 0;
fConsoleBinded := False;
fOnLogEvent := nil;
fOnLogCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.Finalize;
begin
fOnLogEvent := nil;
fOnLogCallback := nil;
If fConsoleBinded then
  UnbindConsole;
ExternalLogClear;
// destroy internally created objects
If Assigned(fFileLogStream) then
  FreeAndNil(fFileLogStream);
If Assigned(fStreamLog) then
  FreeAndNil(fStreamLog);
FreeAndNil(fInternalLog);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTime: TDateTime;
begin
If fSettings.ForceTime then
  begin
    Result := fSettings.ForcedTime;
    If fSettings.ForceTimeAutoreset then
      fSettings.ForceTime := False;
  end
else Result := Now;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTimeString(Time: TDateTime): String;
begin
DateTimeToString(Result,fSettings.TimeFormat,Time,fSettings.FormatSettings);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetIndentedString(const Str: String; IndentCount: Integer): String;

  procedure PutIndentation(AtPos: TStrOffset);
  var
    ii: TStrOffset;
  begin
    For ii := AtPos to Pred(AtPos + IndentCount) do
      Result[ii] := ' ';
  end;

var
  StrPos,ResPos:  TStrOffset;
  ResLen:         TStrSize;
begin
{
  folloving are all recognized linebreak sequences:

    #0
    #10#13
    #13#10
    #10 not followed by #13
    #13 not followed by #10
}
If Length(Str) > 0 then
  begin
    // count how long the resulting string will be for preallocation
    ResLen := 0;
    StrPos := 1;
    while StrPos <= Length(Str) do
      begin
        If Ord(Str[StrPos]) in [10,13] then
          begin
            If StrPos < Length(Str) then
              If (Ord(Str[StrPos + 1]) in [10,13]) and (Str[StrPos + 1] <> Str[StrPos]) then
                begin
                  Inc(ResLen);
                  Inc(StrPos);
                end;
            Inc(ResLen,IndentCount + 1);
          end
        else If Ord(Str[StrPos]) = 0 then
          Inc(ResLen,IndentCount + 1)
        else
          Inc(ResLen);
        Inc(StrPos);
      end;
    SetLength(Result,ResLen); // preallocation
    // construct the result
    StrPos := 1;
    ResPos := 1;
    while (StrPos <= Length(Str)) and (ResPos <= Length(Result)) do
      begin
        If Ord(Str[StrPos]) in [10,13] then
          begin
            Result[ResPos] := Str[StrPos];
            If StrPos < Length(Str) then
              If (Ord(Str[StrPos + 1]) in [10,13]) and (Str[StrPos + 1] <> Str[StrPos]) then
                begin
                  Inc(StrPos);
                  Inc(ResPos);
                  Result[ResPos] := Str[StrPos];
                end;
            PutIndentation(ResPos + 1);
            Inc(ResPos,IndentCount);
          end
        else If Ord(Str[StrPos]) = 0 then
          begin
            Result[ResPos] := Str[StrPos];
            PutIndentation(ResPos + 1);
            Inc(ResPos,IndentCount);
          end
        else
          Result[ResPos] := Str[StrPos];
        Inc(StrPos);
        Inc(ResPos);        
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetStampStr(const StampText: String; ThickBreak: Boolean): String;
begin
If ThickBreak then
  Result := StringOfChar(fStrings.BreakerCharThick,fStrings.BreakerLength) + sLineBreak +
    StampText + sLineBreak + StringOfChar(fStrings.BreakerCharThick,fStrings.BreakerLength)
else
  Result := StringOfChar(fStrings.BreakerCharThin,fStrings.BreakerLength) + sLineBreak +
    StampText + sLineBreak + StringOfChar(fStrings.BreakerCharThin,fStrings.BreakerLength);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSimpleLog.WriteLogToOutputs(const LogText: String; LineBreakInStreams: Boolean);
var
  i:          Integer;
  StreamStr:  UTF8String;
begin
// write to outputs
If loInternal in fSettings.Outputs then
  fInternalLog.Add(LogText);
If LineBreakInStreams then
  StreamStr := StrToUTF8(LogText + sLineBreak)
else
  StreamStr := StrToUTF8(LogText);
If (loStream in fSettings.Outputs) and Assigned(fStreamLog) then
  fStreamLog.WriteBuffer(PUTF8Char(StreamStr)^,Length(StreamStr) * SizeOf(UTF8Char));
If (loFile in fSettings.Outputs) and Assigned(fFileLogStream) then
  fFileLogStream.WriteBuffer(PUTF8Char(StreamStr)^,Length(StreamStr) * SizeOf(UTF8Char));
If (loConsole in fSettings.Outputs) and fConsolePresent and not fConsoleBinded then
  WriteLn(StrToCsl(LogText));
If loExternals in fSettings.Outputs then
  For i := LowIndex to HighIndex do
    If fExternalLogs[i].Active then
      fExternalLogs[i].LogObject.Add(LogText);
Inc(fLogCounter); // this can overflow
DoOnLog(LogText);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleLog.ProcessConsoleLog(const LogText: String);
begin
WriteLogToOutputs(LogText,False);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ProcessLocalLog(const LogText: String; IndentCount: Integer = 0);
begin
If fSettings.IndentLines and (IndentCount > 0) then
  WriteLogToOutputs(GetIndentedString(LogText,IndentCount),True)
else
  WriteLogToOutputs(LogText,True);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.DoOnLog(const LogText: String);
begin
If Assigned(fOnLogEvent) then
  fOnLogEvent(Self,LogText)
else If Assigned(fOnLogCallback) then
  fOnLogCallback(Self,LogText);
end;

{-------------------------------------------------------------------------------
    TSimpleLog - public methods
-------------------------------------------------------------------------------}

constructor TSimpleLog.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSimpleLog.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.OutputIsActive(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.Outputs;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.OutputActivate(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.Outputs;
Include(fSettings.Outputs,Output);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.OutputDeactivate(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.Outputs;
Exclude(fSettings.Outputs,Output);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetupOutputToStream(Stream: TStream; Append: Boolean; Activate: Boolean = True);
begin
{
  If the stream is already assigned, ignore it and just assign the new one.
  Management of these streams is completely external to simple log.

  If Append is true, set position to the end of the stream, otherwise leave
  whatever position is currently set.
}
fStreamLog := Stream;
If Append then
  fStreamLog.Seek(0,soEnd);
If Activate then
  OutputActivate(loStream);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetupOutputToFile(const FileName: String; Append: Boolean; Activate: Boolean = True);
begin
// If there is already a file opened, close it.
If Assigned(fFileLogStream) then
  FreeAndNil(fFileLogStream);
fFileLog := FileName;
If FileExists(StrToRTL(fFileLog)) and Append then
  fFileLogStream := TFileStream.Create(StrToRTL(fFileLog),fmOpenReadWrite or fmShareDenyWrite)
else
  fFileLogStream := TFileStream.Create(StrToRTL(fFileLog),fmCreate or fmShareDenyWrite);
If Append then
  fFileLogStream.Seek(0,soEnd);
If Activate then
  OutputActivate(loFile);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.LowIndex: Integer;
begin
Result := Low(fExternalLogs);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.HighIndex: Integer;
begin
Result := Pred(fExternalLogCount);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogLowIndex: Integer;
begin
Result := LowIndex;
end;
  
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogHighIndex: Integer;
begin
Result := HighIndex;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIndexOf(LogObject: TStrings): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fExternalLogs[i].LogObject = LogObject then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogAdd(LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False): Integer;
begin
Grow;
Result := fExternalLogCount;
fExternalLogs[Result].LogObject := LogObject;
fExternalLogs[Result].Active := Active;
fExternalLogs[Result].Owned := Owned;
Inc(fExternalLogCount);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogInsert(Index: Integer; LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    Grow;
    For i := HighIndex downto Index do
      fExternalLogs[i + 1] := fExternalLogs[i];
    fExternalLogs[Index].LogObject := LogObject;
    fExternalLogs[Index].Active := Active;
    fExternalLogs[Index].Owned := Owned;
    Inc(fExternalLogCount);
  end
else If Index = fExternalLogCount then
  ExternalLogAdd(LogObject,Active,Owned)
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogInsert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogExtract(LogObject: TStrings): TStrings;
var
  Index,i:  Integer;
begin
Index := ExternalLogIndexOf(LogObject);
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].LogObject;
    For i := Index to Pred(HighIndex) do
      fExternalLogs[i] := fExternalLogs[i + 1];
    Dec(fExternalLogCount);
    Shrink;
  end
else Result := nil;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogRemove(LogObject: TStrings): Integer;
begin
Result := ExternalLogIndexOf(LogObject);
If CheckIndex(Result) then
  ExternalLogDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogDelete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    If fExternalLogs[Index].Owned then
      FreeAndNil(fExternalLogs[Index].LogObject);
    For i := Index to Pred(HighIndex) do
      fExternalLogs[i] := fExternalLogs[i + 1];
    Dec(fExternalLogCount);
    Shrink;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogClear;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If fExternalLogs[i].Owned then
    FreeAndNil(fExternalLogs[i].LogObject);
SetLength(fExternalLogs,0);
fExternalLogCount := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIsActive(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].Active
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogActive: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogSetActive(Index: Integer; Active: Boolean): Boolean;
begin
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].Active;
    fExternalLogs[Index].Active := Active;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogSetActive: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIsOwned(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].Owned
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogOwned: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogSetOwned(Index: Integer; Owned: Boolean): Boolean;
begin
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].Owned;
    fExternalLogs[Index].Owned := Owned;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogSetOwned: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ForceTimeSet(Time: TDateTime; Autoreset: Boolean = False): Boolean;
begin
Result := fSettings.ForceTime;
fSettings.ForceTime := True;
fSettings.ForcedTime := Time;
fSettings.ForceTimeAutoreset := Autoreset;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogNoTime(const LogText: String);
begin
ProcessLocalLog(LogText);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogTime(const LogText: String; Time: TDateTime);
var
  TimeStr:  String;
begin
TimeStr := GetTimeString(Time) + fSettings.TimeSeparator;
ProcessLocalLog(TimeStr + LogText,Length(TimeStr));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLog(const LogText: String);
begin
AddLogTime(LogText,GetTime);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEmpty;
begin
AddLogNoTime('');
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreaker;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThin,fStrings.BreakerLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreakerThin;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThin,fStrings.BreakerLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreakerThick;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThick,fStrings.BreakerLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddTimeStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.TimeStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddStartStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.StartStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEndStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.EndStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddAppendStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.AppendStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddHeader;
begin
If Length(fStrings.HeaderText) < fStrings.BreakerLength then
  AddLogNoTime(GetStampStr(StringOfChar(' ',(fStrings.BreakerLength - Length(fStrings.HeaderText)) div 2) + fStrings.HeaderText,True))
else
  AddLogNoTime(GetStampStr(fStrings.HeaderText,True));
end;

//------------------------------------------------------------------------------

Function TSimpleLog.BindConsole: Boolean;
begin
If InterlockedExchange(SLCB_StatusWord,SLCB_STATUS_LOCKED) = SLCB_STATUS_UNLOCKED then
  begin
    If fConsolePresent and not fConsoleBinded then
      begin
        // error output
        fOriginalErrOutput := TTextRec(ErrOutput);
        SLCB_BindLogObject(ErrOutput,Self);
        Rewrite(ErrOutput);
        // "normal" output
        fOriginalOutput := TTextRec(Output);
        SLCB_BindLogObject(Output,Self);
        Rewrite(Output);
        // input
        fOriginalInput := TTextRec(Input);
        SLCB_BindLogObject(Input,Self);
        Reset(Input);
        fConsoleBinded := True;
      end;
    Result := fConsoleBinded;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.UnbindConsole;
begin
If fConsoleBinded then
  begin
    Close(Input);
    TTextRec(Input) := fOriginalInput;
    Close(Output);
    TTextRec(Output) := fOriginalOutput;
    Close(ErrOutput);
    TTextRec(ErrOutput) := fOriginalErrOutput;    
    fConsoleBinded := False;
    InterlockedExchange(SLCB_StatusWord,SLCB_STATUS_UNLOCKED);
  end;
end;

end.
