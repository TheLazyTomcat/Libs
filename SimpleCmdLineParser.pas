{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple Command Line Parser

    Provides a class (TSimpleCmdLineParser) that can parse and split a command
    line string into individial commands, each possibly with arguments, and
    other textual parameters.

    You can either give the command line string explicitly as a textual
    parameter, or you can use parameter-less methods (Create, Parse) that will
    obtain and parse command line string of the current process/program.

    When getting the command line string from the system, it is necessary to
    remember how this works in different operating systems...

      In Windows, it is possible to obtain the string directly and unaltered,
      so not much problem there.

      But in Linux, there is, AFAIK, no way how to obtain the original string.
      The command line is processed by the used shell and only already split
      and processed (eg. escaping is resolved, quoting is removed, ...)
      parameters are given to the program.
      This library tries to reconstruct the full command line in Linux by
      joining the parameters back together. Each parameter is also scanned and
      altered so it can be later correctly parsed again (eg. text containing
      white spaces is enclosed in quotes, backslashes are doubled, ...), but
      some information might be lost by this point, so full reconstruction is
      not always possible.
      To mitigate the biggest problems, it is usually enought to observe all
      rules of currently configured shell (quoting, escaping, ...) when writing
      the line and making sure the shell produces something this library could
      work with. Only one thing cannot be corrected this way - if the string
      contains a parameter that is indistinguishable from a commnad, but is not
      a command (eg. is quoted), then it must be prepared so that it is not
      seen as a command when obtained from arguments vector - enclose it in
      quotes that guarantee the content is not resolved (usually single quotes
      - consult documentation of your shell) and prepend it with a single
      backslash.


    Now for the parsing...

      Special characters:

        command introduction character - dash (-)
        escape character               - backslash (\)
        quotation characters           - double quotes (") and single quotes (')

    Objects (commands, texts) are delimited (separated) by a white space,
    unless that white space is enclosed in quotes (both double and single
    quotes are allowed, but they cannot be mixed).
    Backslash is used as escape character - whatever follows it is preserved
    in the final parameter text as is. This way, you can put quotes or escape
    character in the parameters.
    That being said - if you are giving the string explicitly, it is necessary
    to double all backslash (\) characters if they are to be preserved (eg. in
    file paths) - use class method CommandLinePreprocess for that purpose.

    In current implementation, three basic objects are recognized in the
    command line - short command, long command and general object.

      Short command

        - starts with a single command intro char
        - exactly one character long
        - only lower and upper case letters (a..z, A..Z)
        - case sensitive (a is NOT the same as A)
        - cannot be enclosed in quote chars
        - can be compounded (several short commands merged into one block)
        - can have arguments delimited by a white space

        Short command examples:

          -v                             simple short command
          -vbT                           compound command (commands v, b and T)
          -f file1.txt "file 2.txt"      simple with two arguments
          -Tzf "file.dat"                compound, last command (f) with one
                                         argument

      Long command

        - starts with two command intro chars
        - length is not explicitly limited
        - only lower and upper case letters (a..z, A..Z), numbers (0..9),
          underscore (_) and dash (-)
        - case insensitive (FOO is the same as Foo)
        - cannot start with a dash
        - cannot contain escape character
        - cannot be enclosed in quote chars
        - cannot be compounded
        - can have arguments delimited by a white space

        Long command examples:

          --show_warnings                       simple long command
          --input_file "file1.txt"              long command with one argument
          --files "file1.dat" "files2.dat"      long command with two arguments

      General object

        - any text that is not a command
        - if it contains white spaces, it must be enclosed in quotes (as
          objects are delimited by white spaces) - both single (') and double
          (") quotes are allowed (but they cannot be mixed)
        - backslash is an escape character, meaning anything following it is
          preserved as is (including quotes or another escape char), while the
          escaping character itself is removed
        - any general object appearing after a command is also added as an
          argument of that command
        - if first parsed object from a command line is a general object, it is
          assumed to be the image path

        General object examples:

          this_is_simple_general_text
          "quoted text with \"whitespaces\" and quote chars"
          "special characters: - -- \\ \" \'"


    Now let's have some example command lines to see how they will be parsed...

    First a Windows-style example:

      "c:\test.exe" -sab 1 15 9 --test "output file.txt"

          c:\test.exe       general object (image path)
          s                 short command
          a                 short command
          b                 short command with three arguments:
                                1
                                15
                                9
          1                 general object
          15                general object
          9                 general object
          test              long command with one argument:
                                output file.txt
          output file.txt   general object

    And now something more Linux-like:

      .\test --test -u 999 0 \-t 'string'"'"'string'

          .\test            general object (image path)
          test              long command
          u                 short command with four arguments:
                                999
                                0
                                -t
                                string'string
          999               general object
          0                 general object
          -t                general object
          string'string     general object

  Version 2.0.2 (2024-05-03)

  Last change 2024-05-03

  ©2017-2024 František Milt

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
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
  * StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimpleCmdLineParser_UseAuxExceptions for details).

  Library StrRect is required only when compiling for Windows OS.

  Libraries AuxExceptions and StrRect might also be required as an indirect
  dependencies.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SimpleCmdLineParser;
{
  SimpleCmdLineParser_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimpleCmdLineParser_UseAuxExceptions to achieve this.
}
{$IF Defined(SimpleCmdLineParser_UseAuxExceptions)}
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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESCLPException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESCLPIndexOutOfBounds = class(ESCLPException);
  ESCLPInvalidValue     = class(ESCLPException);
  ESCLPInvalidState     = class(ESCLPException);

{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleCmdLineParser
--------------------------------------------------------------------------------
===============================================================================}
type
  // note that ptCommandBoth is only used when returning command data
  TSCLPParamType = (ptGeneral,ptCommandShort,ptCommandLong,ptCommandBoth);

  TSCLPParameter = record
    ParamType:  TSCLPParamType;
    Str:        String;
    Arguments:  array of String;
  end;

{===============================================================================
    TSimpleCmdLineParser - class declaration
===============================================================================}
type
  TSimpleCmdLineParser = class(TCustomListObject)
  protected
    // data
    fCommandLine:   String;
    fImagePath:     String;
    fParameters:    array of TSCLPParameter;
    fCount:         Integer;
    fCommandCount:  Integer;
    Function GetParameter(Index: Integer): TSCLPParameter; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // parameter list manipulation
    Function AddParam(ParamType: TSCLPParamType; const Str: String): Integer; virtual;
    class procedure AddParamArgument(var Param: TSCLPParameter; const Arg: String); overload; virtual;
    procedure AddParamArgument(Index: Integer; const Arg: String); overload; virtual;
    // init/final
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    class Function CommandLinePreprocess(const CommandLine: String): String; virtual;
    class Function ArgumentPreprocess(const Argument: String): String; virtual;
    class Function GetCommandLine: String; virtual;
    constructor CreateEmpty;
    constructor Create(const CommandLine: String); overload;
    // following overload parses command line of the current program
    constructor Create{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF}; overload;
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function First: TSCLPParameter; virtual;
    Function Last: TSCLPParameter; virtual;
    Function IndexOf(const Str: String; CaseSensitive: Boolean): Integer; virtual;
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
    type is set to ptCommandShort. It will also contain arguments from all
    occurences of selected command, in the order they appear in the command
    line.
    When not successfull, content of CommandData is undefined.
  }
    Function CommandDataShort(ShortForm: Char; out CommandData: TSCLPParameter): Boolean; virtual;
  {
    CommandDataLong

    Returns true when selected long form command is present, false otherwise.

    When successfull, CommandData is set to selected long form string and
    type is set to ptCommandLong. It will also contain arguments from all
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
    to long form when only long form is present. When both forms are present,
    then the string is set to a long form and type is set to ptCommandBoth.
    When not successfull, content of CommandData is undefined.
  }
    Function CommandData(ShortForm: Char; const LongForm: String; out CommandData: TSCLPParameter): Boolean; virtual;
    procedure Clear; virtual;
    procedure Parse(const CommandLine: String); overload; virtual;
    // following overload parses command line of the current program
    procedure Parse; overload; virtual;
    property CommandLine: String read fCommandLine;
    property ImagePath: String read fImagePath;
    property Parameters[Index: Integer]: TSCLPParameter read GetParameter; default;
  {
    CommandCount

    Returns number of commands (both long and short) in parameter list, as
    opposed to property Count, which indicates number of all parameters.

    DO NOT use this number to iterate trough property Parameters.
  }
    property CommandCount: Integer read fCommandCount;
  end;

{===============================================================================
    TSimpleCmdLineParser - class aliases
===============================================================================}
type
  TSimpleCommandLineParser = TSimpleCmdLineParser;

  TSCLPParser = TSimpleCmdLineParser; // for backward compatibility
  TSCLParser  = TSimpleCmdLineParser;

implementation


uses
  {$IFDEF Windows}Windows,{$ENDIF} Math,
  AuxTypes{$IFDEF Windows}, StrRect{$ENDIF};

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
  TSCLPLexerTokenType = (lttGeneral,lttCommandShort,lttCommandLong);

  TSCLPLexerToken = record
    TokenType:    TSCLPLexerTokenType;
    OriginalStr:  String;   // unprocessed string, as it appears in the command line
    Position:     Integer;  // position of the token in command line string
    Str:          String;   // processed text of the token
  end;

  TSCLPLexerCharType = (lctWhiteSpace,lctCommandIntro,lctQuoteSingle,
                        lctQuoteDouble,lctEscape,lctOther);

  TSCLPLexerState = (lsStart,lsWhiteSpace,lsCommandIntro,lsCommandIntroDouble,
                     lsCommandShort,lsCommandLong,lsQuotedSingle,lsQuotedDouble,
                     lsEscape,lsEscapeQuotedSingle,lsEscapeQuotedDouble,lsText);

const
  SCLP_CHAR_CMDINTRO    = '-';
  SCLP_CHAR_QUOTESINGLE = '''';
  SCLP_CHAR_QUOTEDOUBLE = '"';
  SCLP_CHAR_ESCAPE      = '\';

  SCLP_CHARS_WHITESPACE   = [#0..#32];
  SCLP_CHARS_COMMANDSHORT = ['a'..'z','A'..'Z'];
  SCLP_CHARS_COMMANDLONG  = ['a'..'z','A'..'Z','0'..'9','_','-'];

{===============================================================================
    TSCLPLexer - class declaration
===============================================================================}
type
  TSCLPLexer = class(TCustomListObject)
  protected
    // data
    fCommandLine: String;
    fTokens:      array of TSCLPLexerToken;
    fCount:       Integer;
    // lexing variables
    fState:       TSCLPLexerState;
    fPosition:    TStrOff;
    fTokenStart:  TStrOff;
    fTokenLength: TStrSize;
    // getters, setters
    Function GetToken(Index: Integer): TSCLPLexerToken; virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // lexing
    Function CurrCharType: TSCLPLexerCharType; virtual;
    procedure ChangeStateAndAdvance(NewState: TSCLPLexerState); virtual;
    procedure AddToken(TokenType: TSCLPLexerTokenType); virtual;
    procedure Process_Start; virtual;
    procedure Process_WhiteSpace; virtual;
    procedure Process_CommandIntro; virtual;
    procedure Process_CommandIntroDouble; virtual;
    procedure Process_CommandShort; virtual;
    procedure Process_CommandLong; virtual;
    procedure Process_QuotedSingle; virtual;
    procedure Process_QuotedDouble; virtual;
    procedure Process_Escape; virtual;
    procedure Process_EscapeQuotedSingle; virtual;
    procedure Process_EscapeQuotedDouble; virtual;
    procedure Process_Text; virtual;
    // init/final
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    procedure Analyze(const CommandLine: String); virtual;
    procedure Clear; virtual;
    property Tokens[Index: Integer]: TSCLPLexerToken read GetToken; default;
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

Function TSCLPLexer.CurrCharType: TSCLPLexerCharType;
begin
If CharInSet(fCommandLine[fPosition],SCLP_CHARS_WHITESPACE) then
  Result := lctWhiteSpace
else If fCommandLine[fPosition] = SCLP_CHAR_CMDINTRO then
  Result := lctCommandIntro
else If fCommandLine[fPosition] = SCLP_CHAR_QUOTESINGLE then
  Result := lctQuoteSingle
else If fCommandLine[fPosition] = SCLP_CHAR_QUOTEDOUBLE then
  Result := lctQuoteDouble
else If fCommandLine[fPosition] = SCLP_CHAR_ESCAPE then
  Result := lctEscape
else
  Result := lctOther;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.ChangeStateAndAdvance(NewState: TSCLPLexerState);
begin
fState := NewState;
Inc(fTokenLength);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.AddToken(TokenType: TSCLPLexerTokenType);

  Function PostprocessTokenString(const Str: String): String;
  type
    TQuoteState = (qsNone,qsSingle,qsDouble,qsEscape,qsEscapeSingle,qsEscapeDouble);
  var
    QuoteState: TQuoteState;
    StrPos:     TStrOff;
    ResPos:     TStrOff;

    procedure CopyChar(NewQuoteState: TQuoteState);
    begin
      QuoteState := NewQuoteState;
      Result[ResPos] := Str[StrPos];
      Inc(StrPos);
      Inc(ResPos);
    end;

    procedure ChageState(NewQuoteState: TQuoteState);
    begin
      QuoteState := NewQuoteState;
      Inc(StrPos);
    end;

  begin
    // note the resulting string will never be longer than the original
    Result := '';
    SetLength(Result,Length(Str));
    QuoteState := qsNone;
    StrPos := 1;
    ResPos := 1;
    while StrPos <= Length(Str) do
      begin
        case QuoteState of
          qsNone:         case Str[StrPos] of
                            SCLP_CHAR_QUOTESINGLE:  ChageState(qsSingle);
                            SCLP_CHAR_QUOTEDOUBLE:  ChageState(qsDouble);
                            SCLP_CHAR_ESCAPE:       ChageState(qsEscape);
                          else
                            CopyChar(qsNone);
                          end;
          qsSingle:       case Str[StrPos] of
                            SCLP_CHAR_QUOTESINGLE:  ChageState(qsNone);
                            SCLP_CHAR_ESCAPE:       ChageState(qsEscapeSingle);
                          else
                            CopyChar(qsSingle);
                          end;
          qsDouble:       case Str[StrPos] of
                            SCLP_CHAR_QUOTEDOUBLE:  ChageState(qsNone);
                            SCLP_CHAR_ESCAPE:       ChageState(qsEscapeDouble);
                          else
                            CopyChar(qsDouble);
                          end;
          qsEscape:       CopyChar(qsNone);
          qsEscapeSingle: CopyChar(qsSingle);
          qsEscapeDouble: CopyChar(qsDouble);
        end;
      end;
    If QuoteState in [qsEscape,qsEscapeSingle,qsEscapeDouble] then
      begin
        Result[ResPos] := SCLP_CHAR_ESCAPE;
        SetLength(Result,ResPos);
      end
    else SetLength(Result,Pred(ResPos));
  end;

var
  i:  Integer;
begin
If (TokenType = lttCommandShort) and (fTokenLength > 2) then
  begin
    // split compound short commands (eg. -abc -> -a -b -c)
    Grow(Pred(fTokenLength));
    For i := 0 to (fTokenLength - 2) do
      begin
        fTokens[fCount + i].TokenType := lttCommandShort;
        If i <= 0 then
          begin
            fTokens[fCount + i].OriginalStr := Copy(fCommandLine,fTokenStart,2);
            fTokens[fCount + i].Str := fCommandLine[fTokenStart + i + 1];
          end
        else
          begin
            fTokens[fCount + i].OriginalStr := fCommandLine[fTokenStart + i + 1];
            fTokens[fCount + i].Str := fTokens[fCount + i].OriginalStr
          end;
        fTokens[fCount + i].Position := fTokenStart + i + 1;
      end;
    Inc(fCount,Pred(fTokenLength));
  end
else
  begin
    Grow;
    fTokens[fCount].TokenType := TokenType;
    fTokens[fCount].OriginalStr := Copy(fCommandLine,fTokenStart,fTokenLength);
    fTokens[fCount].Position := fTokenStart;
    case TokenType of
      lttGeneral:       fTokens[fCount].Str := PostprocessTokenString(fTokens[fCount].OriginalStr);
      lttCommandShort:  fTokens[fCount].Str := fCommandLine[fTokenStart + 1];
      lttCommandLong:   fTokens[fCount].Str := Copy(fCommandLine,fTokenStart + 2,fTokenLength - 2);
    end;
    Inc(fCount);
  end;
fTokenLength := 0;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Start;
begin
fState := lsWhiteSpace;
fPosition := 0;
fTokenStart := 0;
fTokenLength := 0;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_WhiteSpace;
begin
case CurrCharType of
  lctWhiteSpace:;   // just continue
  lctCommandIntro:  begin
                      fState := lsCommandIntro;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
  lctQuoteSingle:   begin
                      fState := lsQuotedSingle;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
  lctQuoteDouble:   begin
                      fState := lsQuotedDouble;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
  lctEscape:        begin
                      fState := lsEscape;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
  lctOther:         begin
                      fState := lsText;
                      fTokenStart := fPosition;
                      fTokenLength := 1;
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_CommandIntro;
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      AddToken(lttGeneral);
                      fState := lsWhiteSpace;
                    end;
  lctCommandIntro:  ChangeStateAndAdvance(lsCommandIntroDouble);
  lctQuoteSingle:   ChangeStateAndAdvance(lsQuotedSingle);
  lctQuoteDouble:   ChangeStateAndAdvance(lsQuotedDouble);
  lctEscape:        ChangeStateAndAdvance(lsEscape);
  lctOther:         begin
                      If CharInSet(fCommandLine[fPosition],SCLP_CHARS_COMMANDSHORT) then
                        fState := lsCommandShort
                      else
                        fState := lsText;
                      Inc(fTokenLength);
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_CommandIntroDouble;
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      AddToken(lttGeneral);
                      fState := lsWhiteSpace;
                    end;
  lctCommandIntro:  ChangeStateAndAdvance(lsText);
  lctQuoteSingle:   ChangeStateAndAdvance(lsQuotedSingle);
  lctQuoteDouble:   ChangeStateAndAdvance(lsQuotedDouble);
  lctEscape:        ChangeStateAndAdvance(lsEscape);
  lctOther:         begin
                      If CharInSet(fCommandLine[fPosition],SCLP_CHARS_COMMANDLONG) then
                        fState := lsCommandLong
                      else
                        fState := lsText;
                      Inc(fTokenLength);
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_CommandShort;
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      AddToken(lttCommandShort);
                      fState := lsWhiteSpace;
                    end;
  lctCommandIntro:  ChangeStateAndAdvance(lsText);
  lctQuoteSingle:   ChangeStateAndAdvance(lsQuotedSingle);
  lctQuoteDouble:   ChangeStateAndAdvance(lsQuotedDouble);
  lctEscape:        ChangeStateAndAdvance(lsEscape);

  lctOther:         begin
                      If not CharInSet(fCommandLine[fPosition],SCLP_CHARS_COMMANDSHORT) then
                        fState := lsText;
                      Inc(fTokenLength);
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_CommandLong;
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      AddToken(lttCommandLong);
                      fState := lsWhiteSpace;
                    end;
  lctQuoteSingle:   ChangeStateAndAdvance(lsQuotedSingle);
  lctQuoteDouble:   ChangeStateAndAdvance(lsQuotedDouble);
  lctEscape:        ChangeStateAndAdvance(lsEscape);
  lctCommandIntro,
  lctOther:         begin
                      If not CharInSet(fCommandLine[fPosition],SCLP_CHARS_COMMANDLONG) then
                        fState := lsText;
                      Inc(fTokenLength);
                    end;
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_QuotedSingle;
begin
case CurrCharType of
  lctQuoteSingle: ChangeStateAndAdvance(lsText);
  lctEscape:      ChangeStateAndAdvance(lsEscapeQuotedSingle);
else
 {lctWhiteSpace,lctCommandIntro,lctQuoteDouble,lctOther}
  Inc(fTokenLength);
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_QuotedDouble;
begin
case CurrCharType of
  lctQuoteDouble: ChangeStateAndAdvance(lsText);
  lctEscape:      ChangeStateAndAdvance(lsEscapeQuotedDouble);
else
 {lctWhiteSpace,lctCommandIntro,lctQuoteSingle,lctOther}
  Inc(fTokenLength);
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Escape;
begin
ChangeStateAndAdvance(lsText);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_EscapeQuotedSingle;
begin
ChangeStateAndAdvance(lsQuotedSingle);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_EscapeQuotedDouble;
begin
ChangeStateAndAdvance(lsQuotedDouble);
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Process_Text;
begin
case CurrCharType of
  lctWhiteSpace:    begin
                      AddToken(lttGeneral);
                      fState := lsWhiteSpace;
                    end;
  lctQuoteSingle:   ChangeStateAndAdvance(lsQuotedSingle);
  lctQuoteDouble:   ChangeStateAndAdvance(lsQuotedDouble);
  lctEscape:        ChangeStateAndAdvance(lsEscape)
else
 {lctCommandIntro,lctOther}
  Inc(fTokenLength);
end;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Initialize;
begin
fCommandLine := '';
SetLength(fTokens,0);
fCount := 0;
end;

//------------------------------------------------------------------------------

procedure TSCLPLexer.Finalize;
begin
Clear;
end;

{-------------------------------------------------------------------------------
    TSCLPLexer - public methods
-------------------------------------------------------------------------------}

constructor TSCLPLexer.Create;
begin
inherited;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSCLPLexer.Destroy;
begin
Finalize;
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
      lsStart:              Process_Start;
      lsWhiteSpace:         Process_WhiteSpace;
      lsCommandIntro:       Process_CommandIntro;
      lsCommandIntroDouble: Process_CommandIntroDouble;
      lsCommandShort:       Process_CommandShort;
      lsCommandLong:        Process_CommandLong;
      lsQuotedSingle:       Process_QuotedSingle;
      lsQuotedDouble:       Process_QuotedDouble;
      lsEscape:             Process_Escape;
      lsEscapeQuotedSingle: Process_EscapeQuotedSingle;
      lsEscapeQuotedDouble: Process_EscapeQuotedDouble;
      lsText:               Process_Text;
    else
      raise ESCLPInvalidState.CreateFmt('TSCLPLexer.Analyze: Invalid lexer state (%d).',[Ord(fState)]);
    end;
    Inc(fPosition);
  end;
case fState of
  lsCommandShort:       AddToken(lttCommandShort);
  lsCommandLong:        AddToken(lttCommandLong);
  lsCommandIntro,
  lsCommandIntroDouble,
  lsQuotedSingle,
  lsQuotedDouble,
  lsEscape,
  lsEscapeQuotedSingle,
  lsEscapeQuotedDouble,
  lsText:               AddToken(lttGeneral);
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
                              TSimpleCmdLineParser
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleCmdLineParser - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleCmdLineParser - protected methods
-------------------------------------------------------------------------------}

Function TSimpleCmdLineParser.GetParameter(Index: Integer): TSCLPParameter;
begin
If CheckIndex(Index) then
  Result := fParameters[Index]
else
  raise ESCLPIndexOutOfBounds.CreateFmt('TSimpleCmdLineParser.GetParameter: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.GetCapacity: Integer;
begin
Result := Length(fParameters);
end;

//------------------------------------------------------------------------------

procedure TSimpleCmdLineParser.SetCapacity(Value: Integer);
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
else raise ESCLPInvalidValue.CreateFmt('TSimpleCmdLineParser.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleCmdLineParser.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.AddParam(ParamType: TSCLPParamType; const Str: String): Integer;
begin
Grow;
Result := fCount;
fParameters[Result].ParamType := ParamType;
fParameters[Result].Str := Str;
SetLength(fParameters[Result].Arguments,0);
Inc(fCount);
If ParamType <> ptGeneral then
  Inc(fCommandCount);
end;

//------------------------------------------------------------------------------

class procedure TSimpleCmdLineParser.AddParamArgument(var Param: TSCLPParameter; const Arg: String);
begin
SetLength(Param.Arguments,Length(Param.Arguments) + 1);
Param.Arguments[High(Param.Arguments)] := Arg;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSimpleCmdLineParser.AddParamArgument(Index: Integer; const Arg: String);
begin
If CheckIndex(Index) then
  AddParamArgument(fParameters[Index],Arg)
else
  raise ESCLPIndexOutOfBounds.CreateFmt('TSimpleCmdLineParser.AddParamArgument: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSimpleCmdLineParser.Initialize;
begin
fCommandLine := '';
fImagePath := '';
SetLength(fParameters,0);
fCommandCount := 0;
fCount := 0;
end;

//------------------------------------------------------------------------------

procedure TSimpleCmdLineParser.Finalize;
begin
Clear;
end;

{-------------------------------------------------------------------------------
    TSimpleCmdLineParser - public methods
-------------------------------------------------------------------------------}

class Function TSimpleCmdLineParser.CommandLinePreprocess(const CommandLine: String): String;
var
  i:      TStrOff;
  Cntr:   TStrSize;
  ResPos: TStrOff;
begin
// double all escape characters (backslash)
Cntr := 0;
For i := 1 to Length(CommandLine) do
  If CommandLine[i] = SCLP_CHAR_ESCAPE then
    Inc(Cntr);
Result := '';
SetLength(Result,Length(CommandLine) + Cntr);
ResPos := 1;
For i := 1 to Length(CommandLine) do
  begin
    If CommandLine[i] = SCLP_CHAR_ESCAPE then
      begin
        Result[ResPos] := SCLP_CHAR_ESCAPE;
        Inc(ResPos);
      end;
    Result[ResPos] := CommandLine[i];
    Inc(ResPos);
  end;
end;

//------------------------------------------------------------------------------

class Function TSimpleCmdLineParser.ArgumentPreprocess(const Argument: String): String;
var
  i,ResPos:   TStrOff;
  CanBeCmd:   Boolean;
  AddQuote:   Boolean;
  EscapeCnt:  Integer;
begin
{
  - double all escape characters (backslash)
  - enclose non-command strings to single quotes when necessary (when it
    contains white space)
  - prepend all quotes (both single and double) with escape character
  - if string starts with a command intro char and is not a command, prepend
    first command intro char with escape character
}
If Length(Argument) > 0 then
  begin
    // first check whether the argument can be a command
    If (Length(Argument) > 1) and (Argument[1] = SCLP_CHAR_CMDINTRO) then
      begin
        CanBeCmd := True;
        // can it be a long command?
        If (Length(Argument) > 2) and (Argument[2] = SCLP_CHAR_CMDINTRO) then
          begin
            If Argument[3] <> SCLP_CHAR_CMDINTRO then
              begin
                For i := 3 to Length(Argument) do
                  If not CharInSet(Argument[i],SCLP_CHARS_COMMANDLONG) then
                    begin
                      CanBeCmd := False;
                      Break{For i};
                    end;
              end
            else CanBeCmd := False;
          end
        else
          begin
            For i := 2 to Length(Argument) do
              If not CharInSet(Argument[i],SCLP_CHARS_COMMANDSHORT) then
                begin
                  CanBeCmd := False;
                  Break{For i};
                end;
          end;
      end
    else CanBeCmd := False;
    // count escapements and look whether to add quotes
    AddQuote := False;
    EscapeCnt := 0;
    // if it can be a command, then there is nothing to be escaped or quoted
    If not CanBeCmd then
      For i := 1 to Length(Argument) do
        begin
          If CharInSet(Argument[i],SCLP_CHARS_WHITESPACE) then
            AddQuote := True
          else If CharInSet(Argument[i],[SCLP_CHAR_ESCAPE,SCLP_CHAR_QUOTESINGLE,SCLP_CHAR_QUOTEDOUBLE]) then
            Inc(EscapeCnt);
        end;
    If (Argument[1] = SCLP_CHAR_CMDINTRO) and not CanBeCmd then
      Inc(EscapeCnt);
    // prepare result
    Result := '';
    SetLength(Result,Length(Argument) + EscapeCnt + IfThen(AddQuote,2,0));
    // construct result
    If AddQuote then
      begin
        Result[1] := SCLP_CHAR_QUOTESINGLE;
        Result[Length(Result)] := SCLP_CHAR_QUOTESINGLE;
        ResPos := 2;
      end
    else ResPos := 1;
    If (Argument[1] = SCLP_CHAR_CMDINTRO) and not CanBeCmd then
      begin
        Result[ResPos] := SCLP_CHAR_ESCAPE;
        Inc(ResPos);
      end;
    For i := 1 to Length(Argument) do
      begin
        If CharInSet(Argument[i],[SCLP_CHAR_ESCAPE,SCLP_CHAR_QUOTESINGLE,SCLP_CHAR_QUOTEDOUBLE]) then
          begin
            Result[ResPos] := SCLP_CHAR_ESCAPE;
            Inc(ResPos);
          end;
        Result[ResPos] := Argument[i];
        Inc(ResPos);
      end;
  end
else Result := StringOfChar(SCLP_CHAR_QUOTESINGLE,2);
end;

//------------------------------------------------------------------------------

class Function TSimpleCmdLineParser.GetCommandLine: String;
{$IFDEF Windows}
var
  CmdLine:  PChar;
begin
CmdLine := Windows.GetCommandLine;
If Assigned(CmdLine) then
  Result := CommandLinePreprocess(WinToStr(CmdLine))
else
  Result := '';
end;
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
            Result := Result + ' ' + ArgumentPreprocess(PPChar(Arguments)^)
          else
            Result := ArgumentPreprocess(PPChar(Arguments)^);
          Inc(Arguments);
        end
      else Break{For i};
  end
else Result := '';
end;
{$ENDIF}

//------------------------------------------------------------------------------

constructor TSimpleCmdLineParser.CreateEmpty;
begin
inherited;
Initialize;
end;

//------------------------------------------------------------------------------

constructor TSimpleCmdLineParser.Create(const CommandLine: String);
begin
CreateEmpty;
Parse(CommandLine);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleCmdLineParser.Create{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF};
begin
Create(GetCommandLine);
end;

//------------------------------------------------------------------------------

destructor TSimpleCmdLineParser.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.LowIndex: Integer;
begin
Result := Low(fParameters);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.First: TSCLPParameter;
begin
Result := GetParameter(LowIndex);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.Last: TSCLPParameter;
begin
Result := GetParameter(HighIndex);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.IndexOf(const Str: String; CaseSensitive: Boolean): Integer;
var
  i:  Integer;
begin
Result := -1;
{
  There are two seperate cycles for performance reasons - it removes check for
  CaseSensitive that would be otherwise in every single iteration.
}
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

Function TSimpleCmdLineParser.CommandPresentShort(ShortForm: Char): Boolean;
var
  Index:  Integer;
begin
Index := IndexOf(ShortForm,True);
If CheckIndex(Index) then
  Result := fParameters[Index].ParamType = ptCommandShort
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.CommandPresentLong(const LongForm: String): Boolean;
var
  Index:  Integer;
begin
Index := IndexOf(LongForm,False);
If CheckIndex(Index) then
  Result := fParameters[Index].ParamType = ptCommandLong
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.CommandPresent(ShortForm: Char; const LongForm: String): Boolean;
begin
Result := CommandPresentShort(ShortForm) or CommandPresentLong(LongForm);
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.CommandDataShort(ShortForm: Char; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
CommandData.ParamType := ptCommandShort;
CommandData.Str := ShortForm;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptCommandShort) and
     AnsiSameStr(ShortForm,fParameters[i].Str) then
    begin
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.CommandDataLong(const LongForm: String; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
CommandData.ParamType := ptCommandLong;
CommandData.Str := LongForm;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptCommandLong) and
     AnsiSameText(LongForm,fParameters[i].Str) then
    begin
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

Function TSimpleCmdLineParser.CommandData(ShortForm: Char; const LongForm: String; out CommandData: TSCLPParameter): Boolean;
var
  i,j:  Integer;
begin
Result := False;
CommandData.ParamType := ptGeneral;
SetLength(CommandData.Arguments,0);
For i := LowIndex to HighIndex do
  If (fParameters[i].ParamType = ptCommandShort) and AnsiSameStr(ShortForm,fParameters[i].Str) then
    begin
      case CommandData.ParamType of
        ptGeneral:      begin
                          CommandData.ParamType := ptCommandShort;
                          CommandData.Str := ShortForm;
                        end;
        ptCommandLong:  CommandData.ParamType := ptCommandBoth;
      else
       {ptCommandShort,ptCommandBoth - do nothing}
      end;
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end
  else If (fParameters[i].ParamType = ptCommandLong) and AnsiSameText(LongForm,fParameters[i].Str) then
    begin
      case CommandData.ParamType of
        ptGeneral:      begin
                          CommandData.ParamType := ptCommandLong;
                          CommandData.Str := LongForm;
                        end;
        ptCommandShort: begin
                          CommandData.ParamType := ptCommandBoth;
                          CommandData.Str := LongForm;
                        end;
      else
       {ptCommandLong,ptCommandBoth - do nothing}
      end;
      For j := Low(fParameters[i].Arguments) to High(fParameters[i].Arguments) do
        AddParamArgument(CommandData,fParameters[i].Arguments[j]);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCmdLineParser.Clear;
begin
fCommandLine := '';
fImagePath := '';
SetLength(fParameters,0);
fCount := 0;
fCommandCount := 0;
end;

//------------------------------------------------------------------------------

procedure TSimpleCmdLineParser.Parse(const CommandLine: String);
var
  Lexer:      TSCLPLexer;
  LastCmdIdx: Integer;
  i:          Integer;
begin
Lexer := TSCLPLexer.Create;
try
  Clear;
  fCommandLine := CommandLine;
  Lexer.Analyze(fCommandLine);
  LastCmdIdx := -1;
  For i := Lexer.LowIndex to Lexer.HighIndex do
    begin
      case Lexer.Tokens[i].TokenType of
        lttGeneral:       begin
                            If CheckIndex(LastCmdIdx) then
                              AddParamArgument(LastCmdIdx,Lexer.Tokens[i].Str);
                            AddParam(ptGeneral,Lexer.Tokens[i].Str);
                          end;
        lttCommandShort:  LastCmdIdx := AddParam(ptCommandShort,Lexer.Tokens[i].Str);
        lttCommandLong:   LastCmdIdx := AddParam(ptCommandLong,Lexer.Tokens[i].Str);
      end;
    end;
  If fCount > 0 then
    If First.ParamType = ptGeneral then
      fImagePath := First.Str;
finally
  Lexer.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSimpleCmdLineParser.Parse;
begin
Parse(GetCommandLine);
end;

end.
