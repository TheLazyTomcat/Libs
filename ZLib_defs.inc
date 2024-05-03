{
  ZLib_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  ZLib_UseAuxExceptions to achieve this.
}
{$IF Defined(ZLib_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU.'}
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
  {$MODESWITCH ClassicProcVars+}
  {$ASMMODE Intel}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$IF CompilerVersion >= 17}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{
  GZIP_Support

  When defined, the binding provides functions for GZIP file manipulation.
  Requires presence of msvcrt.dll in the system (should be integral part of
  Windows NT and newer) for statically linked version.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol ZLib_GZIP_Support_Off.
}
{$DEFINE GZIP_Support}
{$IFDEF ZLib_GZIP_Support_Off}
  {$UNDEF GZIP_Support}
{$ENDIF}

{
  CheckCompatibility

  Asserts compatibility of this binding and used zlib (be it DLL or statically
  linked code).
  For statically linked zlib, it is done at unit initialization. For dynamically
  linked zlib, it is done at binding initialization.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol ZLib_CheckCompatibility_On.
}
{$UNDEF CheckCompatibility}
{$IFDEF ZLib_CheckCompatibility_On}
  {$DEFINE CheckCompatibility}
{$ENDIF}
