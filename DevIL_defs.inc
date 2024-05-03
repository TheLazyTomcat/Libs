{
  DevIL_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  DevIL_UseAuxExceptions to achieve this.
}
{$IF Defined(DevIL_UseAuxExceptions)}
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
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  DevIL_Unicode

  When defined, the binding is using unicode (wide) strings in the interface
  calls. Note that the use of this requires that the linked libraries (*.DLL)
  are also of proper (unicode) build.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol DevIL_Unicode_On.
}
{$UNDEF DevIL_Unicode}
{$IFDEF DevIL_Unicode_On}
  {$DEFINE DevIL_Unicode}
{$ENDIF}

{
  DevILUT_UseOpenGL

  When defined, the binding for ILUT (DevILUT.pas) provides functions for
  cooperation with OpenGL.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol DevILUT_UseOpenGL_Off.
}
{$DEFINE DevILUT_UseOpenGL}
{$IFDEF DevILUT_UseOpenGL_Off}
  {$UNDEF DevILUT_UseOpenGL}
{$ENDIF}

{
  DevILUT_UseWin32

  When defined, the binding for ILUT (DevILUT.pas) provides functions for
  cooperation with Windows GDI subsystem.
  This symbol has effect only on Windows operating system.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol DevILUT_UseWin32_Off.
}
{$DEFINE DevILUT_UseWin32}
{$IFDEF DevILUT_UseWin32_Off}
  {$UNDEF DevILUT_UseWin32}
{$ENDIF}

//-- do not touch followng lines -----------------------------------------------

{$IFNDEF Windows}
  {$UNDEF DevILUT_UseWin32}
{$ENDIF}
