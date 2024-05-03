{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  AuxExceptions

    Set of exception classes designed to simplify exception creation in
    specific situations (eg. index out of bounds, invalid variable value,
    system error, ...).

    This library is written in a way that allows complete rebasing of all
    implemented classes - that is, changing their common ancestor.
    To do that, you only need to copy the entire code from this unit
    (AuxException.pas) to a new unit of your choice, copy AuxException.inc
    (which implements everything) so it can be included, and change declaration
    of EBaseException to a class you desire to be the new common ancestor.
    You can also provide aliases to existing specialized exception classes (see
    current implementation on how to do that).

    Depending on defined symbols and compilation target, the implemented
    exception classes can also provide some more advanced information, eg.
    processor registers snapshot, simple stack trace, list of loaded modules
    and more.

      Registers snapshot:

        The stored register values are NOT the same as when the exception was
        raised, they are obtained inside of EAEExtendedException constructor
        and are therefore heavily polluted.
        There is, as far as I know, no general way of obtaining the values
        before the exception class is instantiated (well, there is, but it
        works only in 32bit Windows, so let's ignore it for now). If you are
        aware of any way on how to do that, please let me know.

      Stack trace:

        Only very simple stack tracing is implemented. First, the entire active
        area of stack is dumped (copied to a buffer) and then this buffer is
        scanned for stack frames - a list of stack frames is created (if any
        is recognized). For each frame, its start address (both real and
        in-dump), size and return address (corresponds to an address inside of
        function that was using this stack frame) are stored.

      Modules list:

        In Linux, this list is obtained by parsing "/proc/[pid]/maps" file, so
        it might not be completely accurate and might not show all loaded
        modules (eg. vdso).

      Threads list:

        In Linux, this list might not be accurate as the threads can be created
        and/or destroyed during the enumeration.

  Version 1.2.2 (2024-04-20)

  Last change 2024-04-20

  ©2019-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.AuxExceptions

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * StrRect     - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
  * WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

  Library SimpleCPUID is required only when symbol PurePascal is not defined.

  Libraries StrRect, UInt64Utils and WinFileInfo are required only when symbol
  AllowExtendedException is defined. Also, StrRect is required only when
  compiling for Windows OS and UInt64Utils when compiling for Linux OS.

  Library StrRect might also be required as an indirect dependency.

===============================================================================}
unit AuxExceptions;
{
  AllowExtendedException

  If defined and when other symbols and compilation target allows it, then
  EGeneralException (and its descendants) are based on EAEExtendedException
  instead of EAECustomException. Extended exception implements advanced stuff
  like registers snapshot, stack trace and so on, and therefore enables these
  functions in all descendant exception classes.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_AllowExtendedException_OFF.
}
{$DEFINE AllowExtendedException}
{$IFDEF AuxExceptions_AllowExtendedException_OFF}
  {$UNDEF AllowExtendedException}
{$ENDIF}

{
  ExtendedExceptionRegisters

  When defined, the extended exception will try to create a register snapshot
  of current processor at its creation.

  Has no effect when symbol AllowExtendedException is not defined or when
  PurePascal symbol is defined.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionRegisters_OFF.
}
{$DEFINE ExtendedExceptionRegisters}
{$IFDEF AuxExceptions_ExtendedExceptionRegisters_OFF}
  {$UNDEF ExtendedExceptionRegisters}
{$ENDIF}

{
  ExtendedExceptionStack

  When defined, the extended exception will create stack dump and a simple
  stack trace at its creation.

  Has no effect when symbol AllowExtendedException is not defined or when
  PurePascal symbol is defined.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionStack_OFF.
}
{$DEFINE ExtendedExceptionStack}
{$IFDEF AuxExceptions_ExtendedExceptionStack_OFF}
  {$UNDEF ExtendedExceptionStack}
{$ENDIF}

{
  ExtendedExceptionProcess

  When defined, the extended exception will obtain a set of information about
  the executable file of current process.

  Has no effect when symbol AllowExtendedException is not defined.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionProcess_ON.
}
{$UNDEF ExtendedExceptionProcess}
{$IFDEF AuxExceptions_ExtendedExceptionProcess_ON}
  {$DEFINE ExtendedExceptionProcess}
{$ENDIF}

{
  ExtendedExceptionModule

  When defined, the extended exception will obtain information about the
  current module (be it executable or dynamic library) in which the exception
  was created/raised.

  Has no effect when symbol AllowExtendedException is not defined.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionModule_ON.
}
{$UNDEF ExtendedExceptionModule}
{$IFDEF AuxExceptions_ExtendedExceptionModule_ON}
  {$DEFINE ExtendedExceptionModule}
{$ENDIF}

{
  ExtendedExceptionModules

  When defined, the extended exception will compile a list of all modules
  loaded in the process memory.

  Has no effect when symbol AllowExtendedException is not defined.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionModules_ON.
}
{$UNDEF ExtendedExceptionModules}
{$IFDEF AuxExceptions_ExtendedExceptionModules_ON}
  {$DEFINE ExtendedExceptionModules}
{$ENDIF}

{
  ExtendedExceptionThreads

  When defined, the extended exception will obtain a list of all threads within
  the current process.

  Has no effect when symbol AllowExtendedException is not defined.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol AuxExceptions_ExtendedExceptionThreads_ON.
}
{$UNDEF ExtendedExceptionThreads}
{$IFDEF AuxExceptions_ExtendedExceptionThreads_ON}
  {$DEFINE ExtendedExceptionThreads}
{$ENDIF}

//------------------------------------------------------------------------------

{$UNDEF AE_Include_Defs}
{$UNDEF AE_Include_Interface_Uses}
{$UNDEF AE_Include_Interface}
{$UNDEF AE_Include_Implementation_Uses}
{$UNDEF AE_Include_Implementation}
{$UNDEF AE_Include_UnitInitFinal}

{$DEFINE AE_Include_Defs}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Defs}

interface

{$DEFINE AE_Include_Interface_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface_Uses}

type
  // you can change following declaration to rebase all implemented classes
  EBaseException = class(Exception);

  // do NOT change following
  EAEBaseException = class(EBaseException);

{$DEFINE AE_Include_Interface}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface}

type
  // aliases of all implemented exceptions
  ECustomException      = EAECustomException;
{$IFDEF AE_ExtendedException}
  EExtendedException    = EAEExtendedException;
{$ENDIF}
  EGeneralException     = EAEGeneralException;
  ESystemError          = EAESystemError;
  EIndexException       = EAEIndexException;
  EIndexOutOfBounds     = EAEIndexOutOfBounds;
  EIndexTooLow          = EAEIndexTooLow;
  EIndexTooHigh         = EAEIndexTooHigh;
  EIndexInvalid         = EAEIndexInvalid;
  EValueException       = EAEValueException;
  EValueInvalid         = EAEValueInvalid;
  EValueInvalidNameOnly = EAEValueInvalidNameOnly;

implementation

{$DEFINE AE_Include_Implementation_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation_Uses}

{$DEFINE AE_Include_Implementation}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation}

{$DEFINE AE_Include_UnitInitFinal}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_UnitInitFinal}

end.
