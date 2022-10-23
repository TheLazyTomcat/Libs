{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple Command Line Parser

    In current implementation, three basic objects are parsed from the command
    line - short command, long command and general object.

    Special characters

      command introduction character - dash (-)
      termination character          - semicolon (;)
      arguments delimiter character  - comma (,)
      quotation character            - double quotes (")

      WARNING - in linux, all double quotes are removed from the command line
                by the system, to ensure they are preserved for parsing,
                prepend each of them with a backslash (\)

    Short command

      - starts with a single command intro char
      - exactly one character long
      - only lower and upper case letters (a..z, A..Z)
      - case sensitive (a is NOT the same as A)
      - cannot be enclosed in quote chars
      - can be compounded (several short commands merged into one block)
      - can have arguments, first is separated by a white space, subsequent are
        delimited by a delimiter character (in counpound commands, only the
        last command can have an argument)

      Short command examples:

        -v                             simple short command
        -vbT                           compound command (commands v, b and T)
        -f file1.txt, "file 2.txt"     simple with two arguments
        -Tzf "file.dat"                compound, last command (f) with one argument

    Long command

      - starts with two command intro chars
      - length is not explicitly limited
      - only lower and upper case letters (a..z, A..Z), numbers (0..9),
        underscore (_) and dash (-)
      - case insensitive (FOO is the same as Foo)
      - cannot start with a dash
      - cannot contain white-space characters
      - cannot be enclosed in quote chars
      - cannot be compounded
      - can have arguments, first argument is separated by a white space,
        subsequent are delimited by a delimiter character

      Long command examples:

        --show_warnings                       simple long command
        --input_file "file1.txt"              simple with one argument
        --files "file1.dat", "files2.dat"     simple with two arguments

    General object

      - any text that is not a command, delimiter, dash or termination char
      - cannot contain whitespaces, delimiter, quotation or command intro
        character...
      - ...unless it is enclosed in quote chars
      - to add one quote char, escape it with another one
      - if general object has to appear after a command (normally, it would be
        parsed as a command argument), add command termination character after
        the command and before the text
      - if first parsed object from a command line is a general object, it is
        assumed to be the image path

      General object examples:

        this_is_simple_general_text
        "quoted text with ""whitespaces"" and quote chars"
        "special characters: - -- , """   

  Version 1.2.1 (2020-07-27)

  Last change 2022-09-24

  ©2017-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleCmdLineParser

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
  * StrRect            - github.com/TheLazyTomcat/Lib.StrRect

    Library StrRect is required only when compiling for Windows OS.

===============================================================================}
unit SimpleCmdLineParser;

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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESCLPException = class(Exception);

  ESCLPIndexOutOfBounds = class(ESCLPException);
  ESCLPInvalidValue     = class(ESCLPException);
  ESCLPInvalidState     = class(ESCLPException);

{===============================================================================
--------------------------------------------------------------------------------
                                  TSCLPParser                                  
--------------------------------------------------------------------------------
===============================================================================}
type
  TSCLPParamType = (ptShortCommand,ptLongCommand,ptGeneral);

  TSCLPParameter = record
    ParamType:  TSCLPParamType;
    Str:        String;
    Arguments:  array of String;
  end;

  TSCLPParserState = (psInitial,psCommand,psArgument,psGeneral);

{===============================================================================
    TSCLPParser - class declaration
===============================================================================}
type
  TSCLPParser = class(TCustomListObject)
  protected
    // lexing settings
    fCommandIntroChar:  Char;
    fQuoteChar:         Char;
    fDelimiterChar:     Char;
    fTerminatorChar:    Char;
    // data
    fCommandLine:       String;
    fImagePath:         String;
    fParameters:        array of TSCLPParameter;
    fCount:             Integer;
    // parsing variables
    fLexer:             TObject;
    fState:             TSCLPParserState;
    fTokenIndex:        Integer;
    fCurrentParam:      TSCLPParameter;
    Function GetParameter(Index: Integer): TSCLPParameter; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // list manipulation
    procedure AddParam(Data: TSCLPParameter); overload; virtual;
    procedure AddParam(ParamType: TSCLPParamType; const Str: String); overload; virtual;
    // parsing
    procedure Process_Initial; virtual;
    procedure Process_Command; virtual;
    procedure Process_Argument; virtual;
    procedure Process_General; virtual;
    // utility
    procedure SetCurrentParam(ParamType: TSCLPParamType); virtual;
    procedure AddParamArgument(var Param: TSCLPParameter; const Arg: String); virtual;
  public
    class Function GetSysCmdLine: String; virtual;
    constructor CreateEmpty;
    constructor Create(const CommandLine: String); overload;
    constructor Create{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF}; overload;  // parses command line of current module
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function First: TSCLPParameter; virtual;
    Function Last: TSCLPParameter; virtual;
    Function IndexOf(const Str: String; CaseSensitive: Boolean): Integer; virtual;
  {
    CommandCount

    Returns number of commands (both long and short) in parameter list, as
    opposed to property Count, which indicates number of all parameters.
  }
    Function CommandCount: Integer; virtual;
  {
    CommandPresentShort

    Returns true when given short command is present at least once, false
    otherwise.
  }
    Function CommandPresentShort(ShortForm: Char): Boolean; virtual;
  {
    CommandPresentLong

    Returns true when given long command is present at least once, false
    otherwise.
  }
    Function CommandPresentLong(const LongForm: String): Boolean; virtual;
  {
    CommandPresent

    Returns true when either short or long form of selected command is present
    at least once, false otherwise.
  }
    Function CommandPresent(ShortForm: Char; const LongForm: String): Boolean; virtual;
  {
    CommandDataShort

    Returns true when selected short form command is present, false otherwise.

    When successfull, CommandData is set to selected short form string and
    type is set to short command. It will also contain arguments from all
    occurences of selected command, in the order they appear in the command
    line.
    When not successfull, content of CommandData is undefined.
  }
    Function CommandDataShort(ShortForm: Char; out CommandData: TSCLPParameter): Boolean; virtual;
  {
    CommandDataLong

    Returns true when selected long form command is present, false otherwise.

    When successfull, CommandData is set to selected long form string and
    type is set to long command. It will also contain arguments from all
    occurences of selected command, in the order they appear in the command
    line.
    When not successfull, content of CommandData is undefined.
  }
    Function CommandDataLong(const LongForm: String; out CommandData: TSCLPParameter): Boolean; virtual;
  {
    CommandData

    Returns true when either long form or short form of selected command is
    present, false otherwise.

    When successfull, CommandData will also contain arguments from all
    occurences of selected command, in the order they appear in the command
    line. Type and string is set to short form when only short form is present,
    to long form in other situations.
    When not successfull, content of CommandData is undefined.
  }
    Function CommandData(ShortForm: Char; const LongForm: String; out CommandData: TSCLPParameter): Boolean; virtual;
    procedure Clear; virtual;
    procedure Parse(const CommandLine: String); overload; virtual;
    procedure Parse; overload; virtual; // parses command line of current module
    procedure ReParse; virtual;         // parses whatever is currently in the CommandLine
    property CommandIntroChar: Char read fCommandIntroChar write fCommandIntroChar;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    property DelimiterChar: Char read fDelimiterChar write fDelimiterChar;
    property TerminatorChar: Char read fTerminatorChar write fTerminatorChar;
    property CommandLine: String read fCommandLine;
    property ImagePath: String read fImagePath;
    property Parameters[Index: Integer]: TSCLPParameter read GetParameter; default;
  end;

implementation

{$IFDEF Windows}
uses
  Windows,
  StrRect;
{$ENDIF}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}
  
{===============================================================================
    Auxiliary functions
===============================================================================}

{$If not Declared(CharInSet)}

Function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
begin
Result := C in CharSet;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
begin
If Ord(C) <= 255 then
  Result := AnsiChar(C) in CharSet
else
  Result := False;
end;

{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                                   TSCLPLexer
--------------------------------------------------------------------------------
===============================================================================}
type
  TSCLPLexerTokenType = (lttShortCommand,lttLongCommand,lttDelimiter,
                         lttTerminator,lttGeneral);

  TSCLPLexerToken = record
    TokenType:  TSCLPLexerTokenType;
    Str:        String;   // text of the token
    Position:   Integer;  // position of the token in lexed string
  end;

  TSCLPLexerCharType = (lctWhiteSpace,lctCommandIntro,lctQuote,lctDelimiter,
                        lctTerminator,lctOther);

  TSCLPLexerState = (lsStart,lsTraverse,lsText,lsQuoted,lsShortCommand,
                     lsLongCommandStart,lsLongCommand);

const
  SCLP_CHAR_CMDINTRO   = '-';
  SCLP_CHAR_QUOTE      = '"';
  SCLP_CHAR_DELIMITER  = ',';
  SCLP_CHAR_TERMINATOR = ';';

  SCLP_CHARS_SHORTCOMMAND = ['a'..'z','A'..'Z'];
  SCLP_CHARS_LONGCOMMAND  = ['a'..'z','A'..'Z','0'..'9','_','-'];
  SCLP_CHARS_WHITESPACE   = [#0..#32];

{===============================================================================
    TSCLPLexer - class declaration
===============================================================================}
type
  TSCLPLexer = class(TCustomListObject)
  protected
    // settings
    fCommandIntroChar:  Char;
    fQuoteChar:         Char;
    fDelimiterChar:     Char;
    fTerminatorChar:    Char;
    // data
    fCommandLine:       String;
    fTokens:            array of TSCLPLexerToken;
    fCount:             Integer;
    // lexing variables
    fState:             TSCLPLexerState;
    fPosition:          Integer;
    fTokenStart:        Integer;
    fTokenLength:       Integer;
    Function GetToken(Index: Integer): TSCLPLexerToken; virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // list manipulation
    procedure AddToken(TokenType: TSCLPLexerTokenType; const Str: String; Position: Integer); virtual;
    // lexing
    Function IsAhead(aChar: Char): Boolean; virtual;
    Function CurrCharType: TSCLPLexerCharType; virtual;
    procedure Process_Common(TokenType: TSCLPLexerTokenType); virtual;
    procedure Process_Start; virtual;
    procedure Process_Traverse; virtual;
    procedure Process_Text; virtual;
    procedure Process_Quoted; virtual;
    procedure Process_ShortCommand; virtual;
    procedure Process_LongCommandStart; virtual;
    procedure Process_LongCommand; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    procedure Analyze(const CommandLine: String); virtual;
    procedure Clear; virtual;
    property Tokens[Index: Integer]: TSCLPLexerToken read GetToken; default;
    property CommandIntroChar: Char read fCommandIntroChar write fCommandIntroChar;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    property DelimiterChar: Char read fDelimiterChar write fDelimiterChar;
    property TerminatorChar: Char read fTerminatorChar write fTerminatorChar;
    property CommandLine: String read fCommandLine;
  end;

  
{===============================================================================
    TSCLPLexer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSCLPLexer - protected methods
-------------------------------------------------------------------------------}

Function TSCLPLexer.GetToken(Index: Integer): TSCLPLexerToken;
begin
If CheckIndex(Index) then
  Result := fTokens[Index]
else
  raise ESCLPIndexOutOfBounds.CreateFmt('TSCLPLexer.GetToken: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.GetCapacity: Integer;
begin
Result := Length(fTokens);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.SetCapacity(Value: Integer);
begin
If Value >= 0 then
  begin
    If Value <> Length(fTokens) then
      begin
        SetLength(fTokens,Value);
        If Value < fCount then
          fCount := Value
      end;
  end
else raise ESCLPInvalidValue.CreateFmt('TSCLPLexer.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSCLPLexer.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSCLPLexer.AddToken(TokenType: TSCLPLexerTokenType; const Str: String; Position: Integer);
var
  i,OutPos: Integer;
  TempStr:  String;
  LastChar: Char;
begin
If (TokenType = lttShortCommand) and (Length(Str) > 1) then
  begin
    // split compound short commands
    For i := 1 to Length(Str) do
      AddToken(lttShortCommand,Str[i],Position + Pred(i));
  end
else
  begin
    // replace any double quotes with single quote
    If TokenType = lttGeneral then
      begin
        SetLength(TempStr,Length(Str));
        OutPos := 0;
        LastChar := #0;
        For i := 1 to Length(Str) do
          If not((Str[i] = fQuoteChar) and (LastChar = fQuoteChar)) then
            begin
              Inc(OutPos);
              TempStr[OutPos] := Str[i]; 
              LastChar := Str[i];
            end
          else LastChar := #0;
        SetLength(TempStr,OutPos);
      end
    else TempStr := Str;
    // store token data
    If Length(TempStr) > 0 then
      begin
        Grow;
        fTokens[fCount].TokenType := TokenType;
        fTokens[fCount].Str := TempStr;
        fTokens[fCount].Position := Position;
        Inc(fCount);
      end;
  end;
fTokenLength := 0;
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.IsAhead(aChar: Char): Boolean;
begin
If fPosition < Length(fCommandLine) then
  Result := fCommandLine[fPosition + 1] = aChar
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.CurrCharType: TSCLPLexerCharType;
begin
If CharInSet(fCommandLine[fPosition],SCLP_CHARS_WHITESPACE) then
  Result := lctWhiteSpace
else If fCommandLine[fPosition] = fCommandIntroChar then
  Result := lctCommandIntro
else If fCommandLine[fPosition] = fQuoteChar then
  Result := lctQuote
else If fCommandLine[fPosition] = fDelimiterChar then
  Result := lctDelimiter
else If fCommandLine[fPosition] = fTerminatorChar then
  Result := lctTerminator
else
  Result := lctOther;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Common(TokenType: TSCLPLexerTokenType);
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      If fTokenLength > 0 then
                        AddToken(TokenType,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
                      fState := lsTraverse;
                    end;
  lctCommandIntro:  begin
                      If fTokenLength > 0 then
                        AddToken(TokenType,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
                      If IsAhead(fCommandIntroChar) then
                        begin
                          fState := lsLongCommandStart;
                          Inc(fPosition);
                        end
                      else fState := lsShortCommand;
                      fTokenStart := fPosition + 1;
                      fTokenLength := 0;                      
                    end;
  lctQuote:         begin
                      If fTokenLength > 0 then
                        AddToken(TokenType,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
                      fState := lsQuoted;
                      fTokenStart := fPosition + 1;
                      fTokenLength := 0;
                    end;
  lctDelimiter:     begin
                      If fTokenLength > 0 then
                        AddToken(TokenType,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
                      AddToken(lttDelimiter,fCommandLine[fPosition],fPosition);
                      fState := lsTraverse;
                    end;
  lctTerminator:    begin
                      If fTokenLength > 0 then
                        AddToken(TokenType,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
                      AddToken(lttTerminator,fCommandLine[fPosition],fPosition);
                      fState := lsTraverse;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Start;
begin
fState := lsTraverse;
fPosition := 0;
fTokenStart := 0;
fTokenLength := 0;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Traverse;
begin
case CurrCharType of
  lctWhiteSpace:;   // just continue
  lctCommandIntro:  begin
                      If IsAhead(fCommandIntroChar) then
                        begin
                          fState := lsLongCommandStart;
                          Inc(fPosition);
                        end
                      else fState := lsShortCommand;
                      fTokenStart := fPosition + 1;
                      fTokenLength := 0;
                    end;
  lctQuote:         begin
                      fState := lsQuoted;
                      fTokenStart := fPosition + 1;
                      fTokenLength := 0;
                    end;
  lctDelimiter:     AddToken(lttDelimiter,fCommandLine[fPosition],fPosition);
  lctTerminator:    AddToken(lttTerminator,fCommandLine[fPosition],fPosition);
  lctOther:         begin
                      fState := lsText;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Text;
begin
If CurrCharType = lctOther then
  Inc(fTokenLength)
else
  Process_Common(lttGeneral);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Quoted;
begin
If CurrCharType = lctQuote then
  begin
    If IsAhead(fQuoteChar) then
      begin
        Inc(fPosition);
        Inc(fTokenLength,2);
      end
    else
      begin
        If fTokenLength > 0 then
          AddToken(lttGeneral,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
        fState := lsTraverse;
      end;
  end
else Inc(fTokenLength);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_ShortCommand;
begin
If CurrCharType = lctOther then
  begin
    If not CharInSet(fCommandLine[fPosition],SCLP_CHARS_SHORTCOMMAND) then
      begin
        If fTokenLength > 0 then
          AddToken(lttShortCommand,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
        Dec(fPosition);
        fState := lsTraverse;
      end
    else Inc(fTokenLength);
  end
else Process_Common(lttShortCommand);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_LongCommandStart;
begin
If CurrCharType = lctCommandIntro then
  fState := lsTraverse
else
  fState := lsLongCommand;
Dec(fPosition);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_LongCommand;
begin
If (CurrCharType = lctOther) or ((CurrCharType = lctCommandIntro) and not IsAhead(SCLP_CHAR_CMDINTRO)) then
  begin
    If not CharInSet(fCommandLine[fPosition],SCLP_CHARS_LONGCOMMAND) then
      begin
        If fTokenLength > 0 then
          AddToken(lttLongCommand,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
        Dec(fPosition);
        fState := lsTraverse;
      end
    else Inc(fTokenLength);
  end
else Process_Common(lttLongCommand);
end;

{-------------------------------------------------------------------------------
    TSCLPLexer - public methods
-------------------------------------------------------------------------------}

constructor TSCLPLexer.Create;
begin
inherited;
fCommandIntroChar := SCLP_CHAR_CMDINTRO;
fQuoteChar := SCLP_CHAR_QUOTE;
fDelimiterChar := SCLP_CHAR_DELIMITER;
fTerminatorChar := SCLP_CHAR_TERMINATOR;
fCommandLine := '';
SetLength(fTokens,0);
fCount := 0;
end;

//------------------------------------------------------------------------------

destructor TSCLPLexer.Destroy;
begin
Clear;
inherited;
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.LowIndex: Integer;
begin
Result := Low(fTokens);
end;

//------------------------------------------------------------------------------

Function TSCLPLexer.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Analyze(const CommandLine: String);
begin
Clear;
fCommandLine := CommandLine;
fState := lsStart;
fPosition := 0;
while fPosition <= Length(fCommandLine) do
  begin
    case fState of
      lsStart:            Process_Start;
      lsTraverse:         Process_Traverse;
      lsText:             Process_Text;
      lsQuoted:           Process_Quoted;
      lsShortCommand:     Process_ShortCommand;
      lsLongCommandStart: Process_LongCommandStart;      
      lsLongCommand:      Process_LongCommand;
    else
      raise ESCLPInvalidState.CreateFmt('TSCLPLexer.Analyze: Invalid lexer state (%d).',[Ord(fState)]);
    end;
    Inc(fPosition);
  end;
If fTokenLength > 0 then
  case fState of
    lsText,
    lsQuoted:       AddToken(lttGeneral,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
    lsShortCommand: AddToken(lttShortCommand,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
    lsLongCommand:  AddToken(lttLongCommand,Copy(fCommandLine,fTokenStart,fTokenLength),fTokenStart);
  end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Clear;
begin
fCommandLine := '';
SetLength(fTokens,0);
fCount := 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TSCLPParser                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSCLPParser - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSCLPParser - protected methods
-------------------------------------------------------------------------------}

Function TSCLPParser.GetParameter(Index: Integer): TSCLPParameter;
begin
If CheckIndex(Index) then
  Result := fParameters[Index]
else
  raise ESCLPIndexOutOfBounds.CreateFmt('TSCLPParser.GetParameter: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.GetCapacity: Integer;
begin
Result := Length(fParameters);
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.SetCapacity(Value: Integer);
begin
If Value >= 0 then
  begin
    If Value <> Length(fParameters) then
      begin
        SetLength(fParameters,Value);
        If Value < fCount then
          fCount := Value;
      end;
  end
else raise ESCLPInvalidValue.CreateFmt('TSCLPParser.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSCLPParser.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSCLPParser.AddParam(Data: TSCLPParameter);
begin
// do not check for duplicitites
Grow;
fParameters[fCount] := Data;
Inc(fCount);
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.AddParam(ParamType: TSCLPParamType; const Str: String);
var
  Temp: TSCLPParameter;
begin
Temp.ParamType := ParamType;
Temp.Str := Str;
SetLength(Temp.Arguments,0);
AddParam(Temp);
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Process_Initial;
begin
case TSCLPLexer(fLexer)[fTokenIndex].TokenType of
  lttShortCommand:  begin
                      SetCurrentParam(ptShortCommand);
                      fState := psCommand;
                    end;
  lttLongCommand:   begin
                      SetCurrentParam(ptLongCommand);
                      fState := psCommand;
                    end;
  lttDelimiter,
  lttTerminator:    fState := psGeneral;
  lttGeneral:       begin
                      fImagePath := TSCLPLexer(fLexer)[fTokenIndex].Str;
                      AddParam(ptGeneral,TSCLPLexer(fLexer)[fTokenIndex].Str);
                      fState := psGeneral;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Process_Command;
begin
case TSCLPLexer(fLexer)[fTokenIndex].TokenType of
  lttShortCommand:  begin
                      AddParam(fCurrentParam);
                      SetCurrentParam(ptShortCommand);
                      fState := psCommand;
                    end;
  lttLongCommand:   begin
                      AddParam(fCurrentParam);
                      SetCurrentParam(ptLongCommand);
                      fState := psCommand;
                    end;
  lttDelimiter:     fState := psGeneral;
  lttTerminator:    begin
                      AddParam(fCurrentParam);
                      fState := psGeneral;
                    end;
  lttGeneral:       begin
                      AddParamArgument(fCurrentParam,TSCLPLexer(fLexer)[fTokenIndex].Str);
                      fState := psArgument;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Process_Argument;
begin
case TSCLPLexer(fLexer)[fTokenIndex].TokenType of
  lttShortCommand:  begin
                      AddParam(fCurrentParam);
                      SetCurrentParam(ptShortCommand);
                      fState := psCommand;
                    end;
  lttLongCommand:   begin
                      AddParam(fCurrentParam);
                      SetCurrentParam(ptLongCommand);
                      fState := psCommand;
                    end;
  lttDelimiter:     fState := psCommand;
  lttTerminator:    begin
                      AddParam(fCurrentParam);
                      fState := psGeneral;
                    end;
  lttGeneral:       begin
                      AddParam(fCurrentParam);
                      AddParam(ptGeneral,TSCLPLexer(fLexer)[fTokenIndex].Str);
                      fState := psGeneral;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Process_General;
begin
case TSCLPLexer(fLexer)[fTokenIndex].TokenType of
  lttShortCommand:  begin
                      SetCurrentParam(ptShortCommand);
                      fState := psCommand;
                    end;
  lttLongCommand:   begin
                      SetCurrentParam(ptLongCommand);
                      fState := psCommand;
                    end;
  lttDelimiter,
  lttTerminator:    fState := psGeneral;
  lttGeneral:       begin
                      AddParam(ptGeneral,TSCLPLexer(fLexer)[fTokenIndex].Str);
                      fState := psGeneral;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.SetCurrentParam(ParamType: TSCLPParamType);
begin
fCurrentParam.ParamType := ParamType;
fCurrentParam.Str := TSCLPLexer(fLexer)[fTokenIndex].Str;
SetLength(fCurrentParam.Arguments,0);
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.AddParamArgument(var Param: TSCLPParameter; const Arg: String);
begin
SetLength(Param.Arguments,Length(Param.Arguments) + 1);
Param.Arguments[High(Param.Arguments)] := Arg;
end;

{-------------------------------------------------------------------------------
    TSCLPParser - public methods
-------------------------------------------------------------------------------}

class Function TSCLPParser.GetSysCmdLine: String;
{$IFDEF Windows}
var
  CmdLine:  PChar;
begin
// the assignment could be done directly, but to be sure
CmdLine := GetCommandLine;
If Assigned(CmdLine) then
  Result := WinToStr(CmdLine)
else
  Result := '';
{$ELSE}
var
  Arguments:  PPointer;
  i:          Integer;
begin
// reconstruct command line
If (argc > 0) and Assigned(argv) then
  begin
    Arguments := PPointer(argv);
    Result := '';
    For i := 1 to argc do
      If Assigned(Arguments^) then
        begin
          If Length(Result) > 0 then
            Result := Result + ' ' + PPChar(Arguments)^
          else
            Result := PPChar(Arguments)^;
          Inc(Arguments);
        end
      else Break{For i};
  end
else Result := '';
{$ENDIF}
end;

//------------------------------------------------------------------------------

constructor TSCLPParser.CreateEmpty;
begin
inherited;
fCommandIntroChar := SCLP_CHAR_CMDINTRO;
fQuoteChar := SCLP_CHAR_QUOTE;
fDelimiterChar := SCLP_CHAR_DELIMITER;
fTerminatorChar := SCLP_CHAR_TERMINATOR;
fCommandLine := '';
fImagePath := '';
SetLength(fParameters,0);
fCount := 0;
fLexer := TSCLPLexer.Create;
fState := psInitial;
fTokenIndex := -1;
end;

//------------------------------------------------------------------------------

constructor TSCLPParser.Create(const CommandLine: String);
begin
CreateEmpty;
Parse(CommandLine);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSCLPParser.Create{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF};
begin
Create(GetSysCmdLine);
end;

//------------------------------------------------------------------------------

destructor TSCLPParser.Destroy;
begin
Clear;
fLexer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.LowIndex: Integer;
begin
Result := Low(fParameters);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.First: TSCLPParameter;
begin
Result := GetParameter(LowIndex);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.Last: TSCLPParameter;
begin
Result := GetParameter(HighIndex);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.IndexOf(const Str: String; CaseSensitive: Boolean): Integer;
var
  i:  Integer;
begin
Result := -1;
If CaseSensitive then
  begin
    For i := LowIndex to HighIndex do
      If AnsiSameStr(Str,fParameters[i].Str) then
        begin
          Result := i;
          Break{For i};
        end;
  end
else
  begin
    For i := LowIndex to HighIndex do
      If AnsiSameText(Str,fParameters[i].Str) then
        begin
          Result := i;
          Break{For i};
        end;
  end;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandCount: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := LowIndex to HighIndex do
  If fParameters[i].ParamType in [ptShortCommand,ptLongCommand] then
    Inc(Result);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandPresentShort(ShortForm: Char): Boolean;
var
  Index:  Integer;
begin
Index := IndexOf(ShortForm,True);
If CheckIndex(Index) then
  Result := fParameters[Index].ParamType = ptShortCommand
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandPresentLong(const LongForm: String): Boolean;
var
  Index:  Integer;
begin
Index := IndexOf(LongForm,False);
If CheckIndex(Index) then
  Result := fParameters[Index].ParamType = ptLongCommand
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandPresent(ShortForm: Char; const LongForm: String): Boolean;
begin
Result := CommandPresentShort(ShortForm) or CommandPresentLong(LongForm);
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandDataShort(ShortForm: Char; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
CommandData.ParamType := ptShortCommand;
CommandData.Str := ShortForm;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptShortCommand) and
     AnsiSameStr(ShortForm,fParameters[i].Str) then
    begin
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandDataLong(const LongForm: String; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
CommandData.ParamType := ptLongCommand;
CommandData.Str := LongForm;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptLongCommand) and
     AnsiSameText(LongForm,fParameters[i].Str) then
    begin
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

Function TSCLPParser.CommandData(ShortForm: Char; const LongForm: String; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptShortCommand) and AnsiSameStr(ShortForm,fParameters[i].Str) then
    begin
      If not Result then
        begin
          CommandData.ParamType := ptShortCommand;
          CommandData.Str := ShortForm;
        end;
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end
  else If (fParameters[i].ParamType = ptLongCommand) and AnsiSameText(LongForm,fParameters[i].Str) then
    begin
      CommandData.ParamType := ptLongCommand;
      CommandData.Str := LongForm;
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Clear;
begin
fImagePath := '';
SetLength(fParameters,0);
fCount := 0;
TSCLPLexer(fLexer).Clear;
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.Parse(const CommandLine: String);
begin
fCommandLine := CommandLine;
ReParse;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSCLPParser.Parse;
begin
Parse(GetSysCmdLine);
end;

//------------------------------------------------------------------------------

procedure TSCLPParser.ReParse;
begin
Clear;
TSCLPLexer(fLexer).CommandIntroChar := fCommandIntroChar;
TSCLPLexer(fLexer).QuoteChar := fQuoteChar;
TSCLPLexer(fLexer).DelimiterChar := fDelimiterChar;
TSCLPLexer(fLexer).TerminatorChar := fTerminatorChar;
TSCLPLexer(fLexer).Analyze(fCommandLine);
fState := psInitial;
fTokenIndex := TSCLPLexer(fLexer).LowIndex;
while fTokenIndex <= TSCLPLexer(fLexer).HighIndex do
  begin
    case fState of
      psInitial:  Process_Initial;
      psCommand:  Process_Command;
      psArgument: Process_Argument;
      psGeneral:  Process_General;
    else
      raise ESCLPInvalidState.CreateFmt('TSCLPParser.ReParse: Invalid parser state (%d).',[Ord(fState)]);
    end;
    Inc(fTokenIndex);
  end;
If fState in [psCommand,psArgument] then
  AddParam(fCurrentParam);
end;

end.
