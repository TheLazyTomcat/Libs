{$IF not(Defined(WINDOWS) or Defined(MSWINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
{$ENDIF}
{$H+}

{
  LARGE_FILES_SUPPORT

  When defined, type off_t is an alias for off64_t and default file handling
  functions (those with no suffix) are calling _64 suffixed library functions
  (e.g. mpg123_open calls lib:mpg123_open_64).
  When not defined, off_t is declared as long and non-suffixed functions are
  calling default library functions (e.g. mpg123_open calls lib:mpg123_open).

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol mpg123_LARGE_FILES_SUPPORT_Off.
}
{$DEFINE LARGE_FILES_SUPPORT}
{$IFDEF mpg123_LARGE_FILES_SUPPORT_Off}
  {$UNDEF LARGE_FILES_SUPPORT}
{$ENDIF}

{
  LEAK_WARNINGS

  Some library functions are allocating and returning buffers where caller is
  responsible for their freeing. But this cannot be done in Delphi, so calling
  such function will introduce a memory leak.
  When this symbol is defined, such functions are marked with warning messages.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol mpg123_LEAK_WARNINGS_On.
}
{.$DEFINE LEAK_WARNINGS}
{$IFDEF mpg123_LEAK_WARNINGS_On}
  {$DEFINE LEAK_WARNINGS}
{$ENDIF} 
