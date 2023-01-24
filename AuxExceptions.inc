{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  AuxExceptions - implementation file

    This file (AuxException.inc) contains almost entire implementation of
    AuxExceptions library. It is stored separately to allow for classes
    rebasing.

  Version 1.1.2 (2023-01-20)

  Last change 2023-01-20

  ©2019-2023 František Milt

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
  * AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * StrRect     - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
  * WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

  Libraries AuxTypes, SimpleCPUID and WinFileInfo are required only when
  PurePascal is not defined (note it might be automatically defined, depending
  on taget CPU, see source code for details) and symbol AllowExtendedException
  is defined.
  In addition, StrRect is only required when compiling for Windows and
  UInt64Utils only for Linux OS.

===============================================================================}
{$IFDEF AE_Include_Defs}
{
  AuxExceptions_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF AuxExceptions_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

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

  Has no effect when symbol AllowExtendedException is not defined.

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

  Has no effect when symbol AllowExtendedException is not defined.

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

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)} // there are FPC-specific things used 
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------

{$IF not Defined(PurePascal) and Defined(AllowExtendedException)}
  {$DEFINE ExtendedException}
{$ELSE}
  {$UNDEF ExtendedException}
{$IFEND}

{$IFOPT W+}
  {$DEFINE AE_StackFramesEnabled}
{$ELSE}
  {$UNDEF AE_StackFramesEnabled}
{$ENDIF}

{$ENDIF AE_Include_Defs}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Interface_Uses}
uses
  SysUtils {$IFDEF Windows}, Windows{$ELSE}, BaseUnix{$ENDIF}
  {$IFDEF ExtendedException}, AuxTypes, WinFileInfo{$ENDIF};

{$ENDIF AE_Include_Interface_Uses}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Interface}
{===============================================================================
    Common types
===============================================================================}
type
  TAEProcessID   = {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};
  TAEThreadID    = {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};
{$IFNDEF Windows}
  TAELibThreadID = pthread_t;
{$ENDIF}
  TAESysErrCode  = {$IFDEF Windows}DWORD{$ELSE}cInt{$ENDIF};

{===============================================================================
--------------------------------------------------------------------------------
                               EAECustomException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAECustomException - class declaration
===============================================================================}
type
  EAECustomException = class(EAEBaseException)
  protected
    fTime:        TDateTime;
    fProcessID:   TAEProcessID;
    fThreadID:    TAEThreadID;
  {$IFNDEF Windows}
    fLibThreadID: TAELibThreadID;
  {$ENDIF}
    fExcAddr:     Pointer;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    constructor Create(const Msg: String);
    property Time: TDateTime read fTime;
    property ProcessID: TAEProcessID read fProcessID;
    property ThreadID: TAEThreadID read fThreadID;
  {$IFNDEF Windows}
    property LibThreadID: TAELibThreadID read fLibThreadID;
  {$ENDIF}
    property ExceptAddress: Pointer read fExcAddr;
  end;

{$IFDEF ExtendedException}
{===============================================================================
--------------------------------------------------------------------------------
                               EAEExtendedException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEExtendedException - public storage types
===============================================================================}
{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - registers
-------------------------------------------------------------------------------}
type
  TAENativeRegister = PtrUInt;

const
  AE_GPR_HIGH = {$IFDEF x64}15{$ELSE}7{$ENDIF}; // highest general purpose register

//------------------------------------------------------------------------------
type
  TAEGeneralPurposeRegister = packed record
    case Integer of
      0:  (LoByte:  UInt8;
           HiByte:  UInt8);
      1:  (Word:    UInt16);
      2:  (Long:    UInt32);
    {$IFDEF x64}
      3:  (Quad:    UInt64);
    {$ENDIF}
      4:  (Native:  TAENativeRegister)
  end;

  TAEGeneralPurposeRegisters = packed record
    case Integer of
      0:  (A,B,C,D,SI,DI,BP,SP:           TAEGeneralPurposeRegister);
      1:  (R0,R1,R2,R3,R4,R5,R6,R7:       TAEGeneralPurposeRegister;
        {$IFDEF x64}
           R8,R9,R10,R11,R12,R13,R14,R15: TAEGeneralPurposeRegister;
        {$ENDIF});
      2:  (Regs:  array[0..AE_GPR_HIGH] of TAEGeneralPurposeRegister);
  end;

  TAESegmentRegisters = packed record
    case Integer of
      0:  (CS,DS,SS,ES,FS,GS: UInt16);
      1:  (Regs:              packed array[0..5] of UInt16);
  end;

  TAEBasicRegisters = record
    GeneralPurpose:     TAEGeneralPurposeRegisters;
    Segment:            TAESegmentRegisters;
    Flags:              TAENativeRegister;
    InstructionPointer: TAENativeRegister;
  end;

//------------------------------------------------------------------------------
type
  TAEFloatRegister = packed record
    case Integer of
      0:  (Mantissa:      UInt64;
           SignExponent:  UInt16);
      1:  (Float80:       Float80;
           Float64:       Float64;
           Float32:       Float32);
  end;

  TAEFloatRegisters = record
    Stack:                          array[0..7] of TAEFloatRegister;
    ControlWord,StatusWord,TagWord: UInt16;
    LastInstructionPointerSelector: UInt16;
    LastInstructionPointerOffset:   UInt32;
    LastDataPointerSelector:        UInt16;
    LastDataPointerOffset:          UInt32;
    OpCode:                         UInt16; // only lower 11 bits are walid
  end;

//------------------------------------------------------------------------------
type
  TAEIntegerVectorRegister = packed record
    case Integer of
      0:  (VecByte: packed array[0..7] of UInt8);
      1:  (VecWord: packed array[0..3] of UInt16);
      2:  (VecLong: packed array[0..1] of UInt32);
      3:  (VecQuad: UInt64);
  end;

  TAEIntegerVectorRegisters = packed record
    MM: packed array[0..7] of TAEIntegerVectorRegister;
  end;

//------------------------------------------------------------------------------
type
  TAEFloatVectorRegister = packed record
    case Integer of
      0:  (VecByte: packed array[0..63] of UInt8);
      1:  (VecWord: packed array[0..31] of UInt16);
      2:  (VecLong: packed array[0..15] of UInt32);
      3:  (VecQuad: packed array[0..7] of UInt64);
      4:  (XMMs: packed array[0..3] of Float32);
      5:  (XMMd: packed array[0..1] of Float64);
      6:  (YMMs: packed array[0..7] of Float32);
      7:  (YMMd: packed array[0..3] of Float64);
      8:  (ZMMs: packed array[0..15] of Float32);
      9:  (ZMMd: packed array[0..7] of Float64)
  end;

  TAEFloatVectorRegisters = record
    Length: Integer;    // number of valid 32bit floats in each register
    Count:  Integer;    // number of valid registers (Regs array items)
    Regs:   packed array[0..31] of TAEFloatVectorRegister;
    MXCSR:  UInt32;
    OPMask: packed array[0..7] of UInt64;  // K0-K7 registers
  end;

//------------------------------------------------------------------------------
type
  TAESystemRegisters = record
    MachineStatusWord:  UInt16;
  end;

//------------------------------------------------------------------------------  
type
  TAEPresentRegister = (
    prBasic,    // general purpose, flags, segment and instruction pointer registers
    prFPU,      // x87 FPU registers (ST0-ST7, control word, status word, ...)
    prMMX,      // MMX registers (MM0-MM7)
    prSSE,      // SSE-SSE4.x registers (XMM0-XMM7/15)
    prAVX,      // AVX and AVX2 registers (YMM0-YMM7/15)
    prAVX512,   // AVX-512 registers (ZMM0-ZMM7/31, K0-K7)
    prSystem    // MSW and similar
  );

  TAEPresentRegisters = set of TAEPresentRegister;

  TAERegisters = record
    PresentRegisters: TAEPresentRegisters;
    Basic:            TAEBasicRegisters;
    Float:            TAEFloatRegisters;          // x87 FPU
    IntegerVector:    TAEIntegerVectorRegisters;  // MMX
    FloatVector:      TAEFloatVectorRegisters;    // SSE, AVX
    System:           TAESystemRegisters;         // MSW
  end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - stack
-------------------------------------------------------------------------------}
{
  All functions are expected to start with following code or its x64 equivalent
  (if not, then all bets are off):

    PUSH    EBP
    MOV     EBP,  ESP

  For the purpose of this library, the frame is started by calling a subroutine
  (function). This means the frame start (its highest address) points just
  above the linking information (stored return address of the caller).

  Let's use some "ASCII art" to ilustrate (x86 variant):

               high address ---------------------     ---
                            |                   |       |
                            |   params for B    |       |-- stack frame of function A
                            |                   |       |
                  call to B ---------------------     ---
                            | EIP (points to A) |       |
                            ---------------------       |
                            |      EBP(A)       |       |
         EBP(B) points here ---------------------       |
                            |                   |       |
                            |  variables of B   |       |-- stack frame of function B
                            |                   |       |
                            ---------------------       |
                            |                   |       |
                            |  variables of C   |       |
                            |                   |       |
                  call to C ---------------------     ---
                            | EIP (points to B) |       |
                            ---------------------       |
                            |      EBP(B)       |       |
            EBP points here ---------------------       |-- stack frame of function C (current function)
                            |                   |       |
                            |  variables of C   |       |
                            |                   |       |
                low address ---------------------     ---
}
type
  TAEStackFrame = record
    FrameStart:     Pointer;  // address just above the linking info
    FrameBase:      Pointer;  // points to stored EBP/RBP
    FrameAddress:   Pointer;  // where the stack frame starts in memory (its lowest addresss)
    FrameSize:      TMemSize; // size of this frame in bytes
    OwnerFunction:  Pointer;  // points inside the function owning/using this frame
    InDump:
      record
        FrameStart:   Pointer;
        FrameBase:    Pointer;
        FrameAddress: Pointer;
      end;
    LinkingInfo:
      record
        FrameBase:      Pointer;  // stack frame base pointer of the caller
        ReturnAddress:  Pointer;  // return address of the calling function
      end;
  end;

  TAEStackTrace = record
    StackBottom:    Pointer;  // stack base, points above the first pushed value
    StackTop:       Pointer;  // stack pointer, ESP/RSP address, points to the last pushed value
    StackDump:      Pointer;  // points to a buffer containing the copy of active stack area
    StackDumpSize:  TMemSize; // size of stack dump buffer
    StackFrames:    array of TAEStackFrame;
  end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - process
-------------------------------------------------------------------------------}
type
  TAEProcessInfo = record
    ProcessID:    TAEProcessID;
    ExecFileName: String;
    FileInfo:     TWinFileInfo;
  end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - current module
-------------------------------------------------------------------------------}
type
  TAECurrModuleInfo = record
    FileName: String;
    FileInfo: TWinFileInfo;
  end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - modules
-------------------------------------------------------------------------------}
{
  Windows

    BaseAddress is the address at which the module is loaded.
    ExecMemory is set to the same value as BaseAddress.
    ExecSize is set to the module size obtained when enumerating modules (which
    does NOT correspond to actual size of the module file on disk, but rather
    in memory).
    ExecContiguous is alway true.

  Linux

    BaseAddress is address of the first memory block into which the module is
    mapped.
    ExecMemory is address of the first memory block with execute permission
    into which the module is mapped.
    ExecSize is the size of all memory blocks with execute permission in which
    the module is mapped.
    ExecContiguous is set to true when all memory blocks with execute permission
    are contiguous, false otherwise.

      NOTE - in Linux, the modules are obtained by parsing a text file
             "proc/[pid]/maps", where pid is ID of the current process.
}
type
  TAEModuleInfo = record
    ModuleFileName: String; // full path
    BaseAddress:    Pointer;
    ExecMemory:     Pointer;
    ExecSize:       TMemSize;
    ExecContiguous: Boolean;
    FileInfo:       TWinFileInfo;
  end;

  TAEModulesInfo = record
    Modules:  array of TAEModuleInfo;
  end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public storage types - threads
-------------------------------------------------------------------------------}
type
  TAEThreadInfo = record
    ThreadID: TAEThreadID;
  end;

  TAEThreadsInfo = record
    Threads:  array of TAEThreadInfo;
  end;

{===============================================================================
    EAEExtendedException - class declaration
===============================================================================}
type
  EAEExtendedException = class(EAECustomException)
  protected
    fRegisters:   TAERegisters;
    fStackTrace:  TAEStackTrace;
    fProcessInfo: TAEProcessInfo;
    fModuleInfo:  TAECurrModuleInfo;
    fModulesInfo: TAEModulesInfo;
    fThreadsInfo: TAEThreadsInfo;
    procedure InitRegisters; virtual;
    procedure InitStackTrace; virtual;
    procedure InitProcessInfo; virtual;
    procedure InitModuleInfo; virtual;
    procedure InitModulesInfo; virtual;
    procedure InitThreadsInfo; virtual;
    procedure Finalize; virtual;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    destructor Destroy; override;
    property Registers: TAERegisters read fRegisters;
    property StackTrace: TAEStackTrace read fStackTrace;
    property ProcessInfo: TAEProcessInfo read fProcessInfo;
    property ModuleInfo: TAECurrModuleInfo read fModuleInfo;
    property ModulesInfo: TAEModulesInfo read fModulesInfo;
    property ThreadsInfo: TAEThreadsInfo read fThreadsInfo;
  end;

{$ENDIF ExtendedException}

{===============================================================================
--------------------------------------------------------------------------------
                               EAEGeneralException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEGeneralException - class declaration
===============================================================================}
type
{$IFDEF ExtendedException}
  EAEGeneralException = class(EAEExtendedException)
{$ELSE ExtendedException}
  EAEGeneralException = class(EAECustomException)
{$ENDIF ExtendedException}
  protected
    fFaultingObject:    String;
    fFaultingFunction:  String;
    fFullMessage:       String;
  public
    constructor CreateFmt(const Msg: String; Args: array of const; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const Msg: String; FaultObject: TObject; const FaultFunction: String); overload;
    property FaultingObject: String read fFaultingObject;
    property FaultingFunction: String read fFaultingFunction;
    property FullMessage: String read fFullMessage;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAESystemError
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAESystemError - class declaration
===============================================================================}
type
  EAESystemError = class(EAEGeneralException)
  protected
    fErrorCode: TAESysErrCode;
  public
    constructor Create(ErrorCode: TAESysErrCode; FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String); overload;
    property ErrorCode: TAESysErrCode read fErrorCode;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                EAEIndexException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexException - class declaration
===============================================================================}
type
  EAEIndexException = class(EAEGeneralException)
  protected
    fIndex: Integer;
    class Function GetDefaultMessage: String; virtual;
  public
    constructor Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    property Index: Integer read fIndex;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               EAEIndexOutOfBounds
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexOutOfBounds - class declaration
===============================================================================}
type
  EAEIndexOutOfBounds = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooLow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooLow - class declaration
===============================================================================}
type
  EAEIndexTooLow = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooHigh
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooHigh - class declaration
===============================================================================}
type
  EAEIndexTooHigh = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexInvalid - class declaration
===============================================================================}
type
  EAEIndexInvalid = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                EAEValueException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueException - class declaration
===============================================================================}
type
  EAEValueException = class(EAEGeneralException)
  protected
    fValueName: String;
    fValue:     Variant;
    class Function VariantToStr(Value: Variant): String; virtual;
    class Function VariantArrayToStr(Value: Variant): String; virtual;
    class Function GetDefaultMessage(ValueString: Boolean): String; virtual;
  public
    constructor Create(const Msg,ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const Msg,ValueName: String; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const ValueName: String; FaultObject: TObject; const FaultFunction: String); overload;
    property ValueName: String read FValueName;
    property Value: Variant read fValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEValueInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalid - class declaration
===============================================================================}
type
  EAEValueInvalid = class(EAEValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             EAEValueInvalidNameOnly                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalidNameOnly - class declaration
===============================================================================}
type
  EAEValueInvalidNameOnly = class(EAEValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{$ENDIF AE_Include_Interface}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF FPC}{$PUSH}{$WARN 2005 OFF}{$ENDIF}
{$IFDEF AE_Include_Implementation_Uses}
uses
  Variants {$IFDEF Windows}{$ELSE}, syscall, pthreads{$ENDIF}
{$IFDEF ExtendedException}
  {$IFDEF Windows}
    ,{$IFDEF FPC}jwaTlHelp32, jwaPsApi{$ELSE}TlHelp32, PsApi{$ENDIF}, StrRect
  {$ELSE}
    , Classes, StrUtils, dl, UInt64Utils
  {$ENDIF}, SimpleCPUID
{$ENDIF};

//------------------------------------------------------------------------------

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}}   // Conversion between ordinals and pointers is not portable
  {$DEFINE W7102:={$WARN 7102 OFF}}   // Use of +offset(%ebp) for parameters invalid here
  {$PUSH}{$WARN 2005 OFF}             // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
    {$DEFINE W7122:={$WARN 7122 OFF}} // Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits + $4 byte offset]"
  {$ELSE}
    {$DEFINE W7122:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{$ENDIF AE_Include_Implementation_Uses}
{$IFDEF FPC}{$POP}{$ENDIF}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Implementation}
{===============================================================================
--------------------------------------------------------------------------------
                               External functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    External functions - implementation
===============================================================================}
{$IFNDEF Windows}

Function getpid: pid_t; cdecl; external;

//------------------------------------------------------------------------------

Function gettid: pid_t;
begin
Result := do_syscall(syscall_nr_gettid);
end;

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               EAECustomException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAECustomException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAECustomException - public methods
-------------------------------------------------------------------------------}

constructor EAECustomException.CreateFmt(const Msg: String; Args: array of const);
begin
inherited CreateFmt(Msg,Args);
fTime := Now;
{$IFDEF Windows}
fProcessID := Windows.GetCurrentProcessID;
fThreadID := Windows.GetCurrentThreadID;
{$ELSE}
fProcessID := getpid;
fThreadID := gettid;
fLibThreadID := pthread_self;
{$ENDIF}
fExcAddr := ExceptAddr;
end;

//------------------------------------------------------------------------------

constructor EAECustomException.Create(const Msg: String);
begin
CreateFmt(Msg,[]);
end;

{$IFDEF ExtendedException}
{===============================================================================
--------------------------------------------------------------------------------
                               Registers obtaining
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Registers obtaining  - machine features word
===============================================================================}
const
  AE_MACHINEFEATURES_FPU    = $00000001;
  AE_MACHINEFEATURES_MMX    = $00000002;
  AE_MACHINEFEATURES_SSE    = $00000004;
  AE_MACHINEFEATURES_AVX    = $00000008;
  AE_MACHINEFEATURES_AVX512 = $00000010;

var
  MachineFeaturesWord:  UInt32 = 0;

{===============================================================================
    Registers obtaining - thread-local storage
===============================================================================}
type
  TAERegistersStorage = packed record
    IP:           TAENativeRegister;
    // RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15
    GP:           packed array[0..AE_GPR_HIGH] of TAENativeRegister;
    FLAGS:        TAENativeRegister;
    // segment (CS, DS, SS, ES, FS, GS)
    SG:           packed array[0..5] of UInt16;
  {
    x87 - following fields, starting with CW, correspond to protected-mode x87
    FPU state memory image.
  }
    CW,fpad0:     UInt16;   // control word
    SW,fpad1:     UInt16;   // status word
    TW,fpad2:     UInt16;   // tag word
    FIP:          UInt32;   // FPU Instruction Pointer Offset
    FCS:          UInt16;   // FPU Instruction Pointer Selector
    OpCode:       UInt16;   // only lower 10 bits used
    FDP:          UInt32;   // FPU Data Pointer Offset
    FDS,fpad3:    UInt16;   // FPU Data Pointer Selector
    Stack:        packed array[0..7] of packed array[0..9] of UInt8;
  {
    MMX - the vectors can be obtained directly from x87 stack, but to be sure
    they are loaded separately.
  }
    MM:           packed array[0..7] of UInt64;
  {
    SSE, AVX(2), AVX-512 - the storage is made large enough to accomodate all
    possible data, but depending on machine features, only lower part of the
    space might be actually used.
  }
    ZMM:          packed array[0..{$IFDEF x64}2047{$ELSE}511{$ENDIF}] of UInt8;
    K:            packed array[0..7] of UInt64;
    MXCSR:        UInt32;
    // system registers
    MSW:          UInt16; // machine status word
  end;
  PAERegistersStorage = ^TAERegistersStorage;

threadvar
  RegistersSnapshot:  PAERegistersStorage;

{===============================================================================
    Registers obtaining - internal functions
===============================================================================}

procedure GetX87Data(Storage: Pointer); register; assembler;
asm
    // get x87 state
    FSAVE   [Storage] // this will initialize FPU...
    FRSTOR  [Storage] // ...so we restore it again
    FWAIT
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W7122{$ENDIF}
procedure GetMMXData(Storage: Pointer); register; assembler;
asm
    MOVQ    qword ptr [Storage], MM0
    MOVQ    qword ptr [Storage + 8], MM1
    MOVQ    qword ptr [Storage + 16], MM2
    MOVQ    qword ptr [Storage + 24], MM3
    MOVQ    qword ptr [Storage + 32], MM4
    MOVQ    qword ptr [Storage + 40], MM5
    MOVQ    qword ptr [Storage + 48], MM6
    MOVQ    qword ptr [Storage + 56], MM7
    EMMS
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure GetAVX512Data(Storage: Pointer); assembler; register;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    // Win64
    DB  $62, $f1, $7c, $48, $11, $01      // VMOVUPS zmmword ptr [RCX + 0000], ZMM0
    DB  $62, $f1, $7c, $48, $11, $49, $01 // VMOVUPS zmmword ptr [RCX + 0064], ZMM1
    DB  $62, $f1, $7c, $48, $11, $51, $02 // VMOVUPS zmmword ptr [RCX + 0128], ZMM2
    DB  $62, $f1, $7c, $48, $11, $59, $03 // VMOVUPS zmmword ptr [RCX + 0192], ZMM3
    DB  $62, $f1, $7c, $48, $11, $61, $04 // VMOVUPS zmmword ptr [RCX + 0256], ZMM4
    DB  $62, $f1, $7c, $48, $11, $69, $05 // VMOVUPS zmmword ptr [RCX + 0320], ZMM5
    DB  $62, $f1, $7c, $48, $11, $71, $06 // VMOVUPS zmmword ptr [RCX + 0384], ZMM6
    DB  $62, $f1, $7c, $48, $11, $79, $07 // VMOVUPS zmmword ptr [RCX + 0448], ZMM7

    DB  $62, $71, $7c, $48, $11, $41, $08 // VMOVUPS zmmword ptr [RCX + 0512], ZMM8
    DB  $62, $71, $7c, $48, $11, $49, $09 // VMOVUPS zmmword ptr [RCX + 0576], ZMM9
    DB  $62, $71, $7c, $48, $11, $51, $0a // VMOVUPS zmmword ptr [RCX + 0640], ZMM10
    DB  $62, $71, $7c, $48, $11, $59, $0b // VMOVUPS zmmword ptr [RCX + 0704], ZMM11
    DB  $62, $71, $7c, $48, $11, $61, $0c // VMOVUPS zmmword ptr [RCX + 0768], ZMM12
    DB  $62, $71, $7c, $48, $11, $69, $0d // VMOVUPS zmmword ptr [RCX + 0832], ZMM13
    DB  $62, $71, $7c, $48, $11, $71, $0e // VMOVUPS zmmword ptr [RCX + 0896], ZMM14
    DB  $62, $71, $7c, $48, $11, $79, $0f // VMOVUPS zmmword ptr [RCX + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $41, $10 // VMOVUPS zmmword ptr [RCX + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $49, $11 // VMOVUPS zmmword ptr [RCX + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $51, $12 // VMOVUPS zmmword ptr [RCX + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $59, $13 // VMOVUPS zmmword ptr [RCX + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $61, $14 // VMOVUPS zmmword ptr [RCX + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $69, $15 // VMOVUPS zmmword ptr [RCX + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $71, $16 // VMOVUPS zmmword ptr [RCX + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $79, $17 // VMOVUPS zmmword ptr [RCX + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $41, $18 // VMOVUPS zmmword ptr [RCX + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $49, $19 // VMOVUPS zmmword ptr [RCX + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $51, $1A // VMOVUPS zmmword ptr [RCX + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $59, $1B // VMOVUPS zmmword ptr [RCX + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $61, $1C // VMOVUPS zmmword ptr [RCX + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $69, $1D // VMOVUPS zmmword ptr [RCX + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $71, $1E // VMOVUPS zmmword ptr [RCX + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $79, $1F // VMOVUPS zmmword ptr [RCX + 1984], ZMM31
  {$ELSE}
    // Lin64
    DB  $62, $F1, $7C, $48, $11, $07      // VMOVUPS zmmword ptr [RDI + 0000], ZMM0
    DB  $62, $F1, $7C, $48, $11, $4F, $01 // VMOVUPS zmmword ptr [RDI + 0064], ZMM1
    DB  $62, $F1, $7C, $48, $11, $57, $02 // VMOVUPS zmmword ptr [RDI + 0128], ZMM2
    DB  $62, $F1, $7C, $48, $11, $5F, $03 // VMOVUPS zmmword ptr [RDI + 0192], ZMM3
    DB  $62, $F1, $7C, $48, $11, $67, $04 // VMOVUPS zmmword ptr [RDI + 0256], ZMM4
    DB  $62, $F1, $7C, $48, $11, $6F, $05 // VMOVUPS zmmword ptr [RDI + 0320], ZMM5
    DB  $62, $F1, $7C, $48, $11, $77, $06 // VMOVUPS zmmword ptr [RDI + 0384], ZMM6
    DB  $62, $F1, $7C, $48, $11, $7F, $07 // VMOVUPS zmmword ptr [RDI + 0448], ZMM7

    DB  $62, $71, $7C, $48, $11, $47, $08 // VMOVUPS zmmword ptr [RDI + 0512], ZMM8
    DB  $62, $71, $7C, $48, $11, $4F, $09 // VMOVUPS zmmword ptr [RDI + 0576], ZMM9
    DB  $62, $71, $7C, $48, $11, $57, $0A // VMOVUPS zmmword ptr [RDI + 0640], ZMM10
    DB  $62, $71, $7C, $48, $11, $5F, $0B // VMOVUPS zmmword ptr [RDI + 0704], ZMM11
    DB  $62, $71, $7C, $48, $11, $67, $0C // VMOVUPS zmmword ptr [RDI + 0768], ZMM12
    DB  $62, $71, $7C, $48, $11, $6F, $0D // VMOVUPS zmmword ptr [RDI + 0832], ZMM13
    DB  $62, $71, $7C, $48, $11, $77, $0E // VMOVUPS zmmword ptr [RDI + 0896], ZMM14
    DB  $62, $71, $7C, $48, $11, $7F, $0F // VMOVUPS zmmword ptr [RDI + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $47, $10 // VMOVUPS zmmword ptr [RDI + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $4F, $11 // VMOVUPS zmmword ptr [RDI + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $57, $12 // VMOVUPS zmmword ptr [RDI + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $5F, $13 // VMOVUPS zmmword ptr [RDI + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $67, $14 // VMOVUPS zmmword ptr [RDI + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $6F, $15 // VMOVUPS zmmword ptr [RDI + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $77, $16 // VMOVUPS zmmword ptr [RDI + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $7F, $17 // VMOVUPS zmmword ptr [RDI + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $47, $18 // VMOVUPS zmmword ptr [RDI + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $4F, $19 // VMOVUPS zmmword ptr [RDI + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $57, $1A // VMOVUPS zmmword ptr [RDI + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $5F, $1B // VMOVUPS zmmword ptr [RDI + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $67, $1C // VMOVUPS zmmword ptr [RDI + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $6F, $1D // VMOVUPS zmmword ptr [RDI + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $77, $1E // VMOVUPS zmmword ptr [RDI + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $7F, $1F // VMOVUPS zmmword ptr [RDI + 1984], ZMM31
  {$ENDIF}
{$ELSE}
    // Win32, Lin32
    DB  $62, $f1, $7c, $48, $11, $00      // VMOVUPS zmmword ptr [EAX + 0000], ZMM0
    DB  $62, $f1, $7c, $48, $11, $48, $01 // VMOVUPS zmmword ptr [EAX + 0064], ZMM1
    DB  $62, $f1, $7c, $48, $11, $50, $02 // VMOVUPS zmmword ptr [EAX + 0128], ZMM2
    DB  $62, $f1, $7c, $48, $11, $58, $03 // VMOVUPS zmmword ptr [EAX + 0192], ZMM3
    DB  $62, $f1, $7c, $48, $11, $60, $04 // VMOVUPS zmmword ptr [EAX + 0256], ZMM4
    DB  $62, $f1, $7c, $48, $11, $68, $05 // VMOVUPS zmmword ptr [EAX + 0320], ZMM5
    DB  $62, $f1, $7c, $48, $11, $70, $06 // VMOVUPS zmmword ptr [EAX + 0384], ZMM6
    DB  $62, $f1, $7c, $48, $11, $78, $07 // VMOVUPS zmmword ptr [EAX + 0448], ZMM7
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GetAVX512MaskRegs(Storage: Pointer); assembler; register;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    // Win64
    DB  $C4, $E1, $F8, $91, $01       // KMOVQ [RCX + 00], K0
    DB  $C4, $E1, $F8, $91, $49, $08  // KMOVQ [RCX + 08], K1
    DB  $C4, $E1, $F8, $91, $51, $10  // KMOVQ [RCX + 16], K2
    DB  $C4, $E1, $F8, $91, $59, $18  // KMOVQ [RCX + 24], K3
    DB  $C4, $E1, $F8, $91, $61, $20  // KMOVQ [RCX + 32], K4
    DB  $C4, $E1, $F8, $91, $69, $28  // KMOVQ [RCX + 40], K5
    DB  $C4, $E1, $F8, $91, $71, $30  // KMOVQ [RCX + 48], K6
    DB  $C4, $E1, $F8, $91, $79, $38  // KMOVQ [RCX + 56], K7
  {$ELSE}
    // Lin64
    DB  $C4, $E1, $F8, $91, $07       // KMOVQ [RDI + O0], K0
    DB  $C4, $E1, $F8, $91, $4F, $08  // KMOVQ [RDI + O8], K1
    DB  $C4, $E1, $F8, $91, $57, $10  // KMOVQ [RDI + 16], K2
    DB  $C4, $E1, $F8, $91, $5F, $18  // KMOVQ [RDI + 24], K3
    DB  $C4, $E1, $F8, $91, $67, $20  // KMOVQ [RDI + 32], K4
    DB  $C4, $E1, $F8, $91, $6F, $28  // KMOVQ [RDI + 40], K5
    DB  $C4, $E1, $F8, $91, $77, $30  // KMOVQ [RDI + 48], K6
    DB  $C4, $E1, $F8, $91, $7F, $38  // KMOVQ [RDI + 56], K7
  {$ENDIF}
{$ELSE}
    // Win32, Lin32
    DB  $C4, $E1, $F8, $91, $00       // KMOVQ [EAX + 00], K0
    DB  $C4, $E1, $F8, $91, $48, $08  // KMOVQ [EAX + 08], K1
    DB  $C4, $E1, $F8, $91, $50, $10  // KMOVQ [EAX + 16], K2
    DB  $C4, $E1, $F8, $91, $58, $18  // KMOVQ [EAX + 24], K3
    DB  $C4, $E1, $F8, $91, $60, $20  // KMOVQ [EAX + 32], K4
    DB  $C4, $E1, $F8, $91, $68, $28  // KMOVQ [EAX + 40], K5
    DB  $C4, $E1, $F8, $91, $70, $30  // KMOVQ [EAX + 48], K6
    DB  $C4, $E1, $F8, $91, $78, $38  // KMOVQ [EAX + 56], K7
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GetAVXData(Storage: Pointer); assembler; register;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    // Win64
    DB  $C5, $FC, $11, $01                      // VMOVUPS ymmword ptr [RCX + 000], YMM0
    DB  $C5, $FC, $11, $49, $40                 // VMOVUPS ymmword ptr [RCX + 064], YMM1
    DB  $C5, $FC, $11, $91, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 128], YMM2
    DB  $C5, $FC, $11, $99, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 192], YMM3
    DB  $C5, $FC, $11, $A1, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 256], YMM4
    DB  $C5, $FC, $11, $A9, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 320], YMM5
    DB  $C5, $FC, $11, $B1, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 384], YMM6
    DB  $C5, $FC, $11, $B9, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 448], YMM7

    DB  $C5, $7C, $11, $81, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 512], YMM8
    DB  $C5, $7C, $11, $89, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 576], YMM9
    DB  $C5, $7C, $11, $91, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 640], YMM10
    DB  $C5, $7C, $11, $99, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 704], YMM11
    DB  $C5, $7C, $11, $A1, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 768], YMM12
    DB  $C5, $7C, $11, $A9, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 832], YMM13
    DB  $C5, $7C, $11, $B1, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 896], YMM14
    DB  $C5, $7C, $11, $B9, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 960], YMM15
  {$ELSE}
    // Lin64
    DB  $C5, $FC, $11, $07                      // VMOVUPS ymmword ptr [RDI + 000], YMM0
    DB  $C5, $FC, $11, $4F, $40                 // VMOVUPS ymmword ptr [RDI + 064], YMM1
    DB  $C5, $FC, $11, $97, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 128], YMM2
    DB  $C5, $FC, $11, $9F, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 192], YMM3
    DB  $C5, $FC, $11, $A7, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 256], YMM4
    DB  $C5, $FC, $11, $AF, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 320], YMM5
    DB  $C5, $FC, $11, $B7, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 384], YMM6
    DB  $C5, $FC, $11, $BF, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 448], YMM7

    DB  $C5, $7C, $11, $87, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 512], YMM8
    DB  $C5, $7C, $11, $8F, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 576], YMM9
    DB  $C5, $7C, $11, $97, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 640], YMM10
    DB  $C5, $7C, $11, $9F, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 704], YMM11
    DB  $C5, $7C, $11, $A7, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 768], YMM12
    DB  $C5, $7C, $11, $AF, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 832], YMM13
    DB  $C5, $7C, $11, $B7, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 896], YMM14
    DB  $C5, $7C, $11, $BF, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 960], YMM15
  {$ENDIF}
{$ELSE}
    // Win32, Lin32
    DB  $c5, $fc, $11, $00                      // VMOVUPS ymmword ptr [EAX + 000], YMM0
    DB  $c5, $fc, $11, $48, $40                 // VMOVUPS ymmword ptr [EAX + 064], YMM1
    DB  $c5, $fc, $11, $90, $80, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 128], YMM2
    DB  $c5, $fc, $11, $98, $c0, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 192], YMM3
    DB  $c5, $fc, $11, $a0, $00, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 256], YMM4
    DB  $c5, $fc, $11, $a8, $40, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 320], YMM5
    DB  $c5, $fc, $11, $b0, $80, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 384], YMM6
    DB  $c5, $fc, $11, $b8, $c0, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 448], YMM7
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W7122{$ENDIF}
procedure GetSSEData(Storage: Pointer); assembler; register;
asm
    MOVUPS  dqword ptr [Storage + 000], XMM0
    MOVUPS  dqword ptr [Storage + 064], XMM1
    MOVUPS  dqword ptr [Storage + 128], XMM2
    MOVUPS  dqword ptr [Storage + 192], XMM3
    MOVUPS  dqword ptr [Storage + 256], XMM4
    MOVUPS  dqword ptr [Storage + 320], XMM5
    MOVUPS  dqword ptr [Storage + 384], XMM6
    MOVUPS  dqword ptr [Storage + 448], XMM7
  {$IFDEF x64}
    MOVUPS  dqword ptr [Storage + 512], XMM8
    MOVUPS  dqword ptr [Storage + 576], XMM9
    MOVUPS  dqword ptr [Storage + 640], XMM10
    MOVUPS  dqword ptr [Storage + 704], XMM11
    MOVUPS  dqword ptr [Storage + 768], XMM12
    MOVUPS  dqword ptr [Storage + 832], XMM13
    MOVUPS  dqword ptr [Storage + 896], XMM14
    MOVUPS  dqword ptr [Storage + 960], XMM15
  {$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure CreateRegistersSnapshotCopyToTLS(Storage: PAERegistersStorage); register;
begin
If not Assigned(RegistersSnapshot) then
  RegistersSnapshot := AllocMem(SizeOf(TAERegistersStorage));
RegistersSnapshot^ := Storage^;
end;

{===============================================================================
    Registers obtaining - implementation
===============================================================================}

{$STACKFRAMES ON} // make sure stack frame is created in RegistersSnapshot

{$IFDEF FPCDWM}{$PUSH}W7102{$ENDIF}
Function CreateRegistersSnapshot: Pointer; register; assembler;
var
  Data: TAERegistersStorage;  // allocated on stack
asm
{$IFDEF x64}
    // get and preserve RAX
    MOV     qword ptr [Data.GP], RAX
    // RIP (return address of this function)
    MOV     RAX, qword ptr [RBP + 8]
    MOV     qword ptr [Data.IP], RAX
    // other GP
    MOV     qword ptr [Data.GP + 8], RBX
    MOV     qword ptr [Data.GP + 16], RCX
    MOV     qword ptr [Data.GP + 24], RDX
    MOV     qword ptr [Data.GP + 32], RSI
    MOV     qword ptr [Data.GP + 40], RDI
    MOV     qword ptr [Data.GP + 64], R8
    MOV     qword ptr [Data.GP + 72], R9
    MOV     qword ptr [Data.GP + 80], R10
    MOV     qword ptr [Data.GP + 88], R11
    MOV     qword ptr [Data.GP + 96], R12
    MOV     qword ptr [Data.GP + 104], R13
    MOV     qword ptr [Data.GP + 112], R14
    MOV     qword ptr [Data.GP + 120], R15
    // get stack base stored on stack in the function prolog
    MOV     RAX, qword ptr [RBP]
    MOV     qword ptr [Data.GP + 48], RAX   // RBP
    // stack pointer as it was before the call and will be after it
    LEA     RAX, [RBP + 16]
    MOV     qword ptr [Data.GP + 56], RAX   // RSP
    // flags
    PUSHFQ
    POP     RAX
    MOV     qword ptr [Data.FLAGS], RAX
{$ELSE}
    // get and preserve EAX
    MOV     dword ptr [Data.GP], EAX
    // EIP (return address of this function)
    MOV     EAX, dword ptr [EBP + 4]
    MOV     dword ptr [Data.IP], EAX
    // other GP
    MOV     dword ptr [Data.GP + 4], EBX
    MOV     dword ptr [Data.GP + 8], ECX
    MOV     dword ptr [Data.GP + 12], EDX
    MOV     dword ptr [Data.GP + 16], ESI
    MOV     dword ptr [Data.GP + 20], EDI
    // get stack base stored on stack in the function prolog
    MOV     EAX, dword ptr [EBP]
    MOV     dword ptr [Data.GP + 24], EAX   // EBP
    // stack pointer as it was before the call and will be after it
    LEA     EAX, [EBP + 8]
    MOV     dword ptr [Data.GP + 28], EAX   // ESP
    // flags
    PUSHFD
    POP     EAX
    MOV     dword ptr [Data.FLAGS], EAX
{$ENDIF}

    // segments
    MOV     word ptr [Data.SG], CS
    MOV     word ptr [Data.SG + 2], DS
    MOV     word ptr [Data.SG + 4], SS
    MOV     word ptr [Data.SG + 5], ES
    MOV     word ptr [Data.SG + 8], FS
    MOV     word ptr [Data.SG + 10], GS

    // FPU
{$IFDEF x64}
    TEST    dword ptr [RIP + MachineFeaturesWord], AE_MACHINEFEATURES_FPU
    JZ      @SkipFPU
  {$IFDEF Windows}
    LEA     RCX, [Data.CW]
  {$ELSE}
    LEA     RDI, [Data.CW]
  {$ENDIF}
{$ELSE}
    TEST    dword ptr [MachineFeaturesWord], AE_MACHINEFEATURES_FPU
    JZ      @SkipFPU
    LEA     EAX, [Data.CW]
{$ENDIF}
    CALL    GetX87Data
  @SkipFPU:

    // MMX
{$IFDEF x64}
    TEST    dword ptr [RIP + MachineFeaturesWord], AE_MACHINEFEATURES_MMX
    JZ      @SkipMMX
  {$IFDEF Windows}
    LEA     RCX, [Data.MM]
  {$ELSE}
    LEA     RDI, [Data.MM]
  {$ENDIF}
{$ELSE}
    TEST    dword ptr [MachineFeaturesWord], AE_MACHINEFEATURES_MMX
    JZ      @SkipMMX
    LEA     EAX, [Data.MM]
{$ENDIF}
    CALL    GetMMXData
  @SkipMMX:

    // MXCSR
    STMXCSR   dword ptr [Data.MXCSR]

    // AVX512
{$IFDEF x64}
    TEST    dword ptr [RIP + MachineFeaturesWord], AE_MACHINEFEATURES_AVX512
    JZ      @SkipAVX512
  {$IFDEF Windows}
    LEA     RCX, [Data.ZMM]
  {$ELSE}
    LEA     RDI, [Data.ZMM]
  {$ENDIF}
{$ELSE}
    TEST    dword ptr [MachineFeaturesWord], AE_MACHINEFEATURES_AVX512
    JZ      @SkipAVX512
    LEA     EAX, [Data.ZMM]
{$ENDIF}
    CALL    GetAVX512Data
{$IFDEF x64}
  {$IFDEF Windows}
    LEA     RCX, [Data.K]
  {$ELSE}
    LEA     RDI, [Data.K]
  {$ENDIF}
{$ELSE}
    LEA     EAX, [Data.K]
{$ENDIF}
    CALL    GetAVX512MaskRegs
    JMP     @SkipSSE  // AVX and SSE states are already stored within AVX-512
  @SkipAVX512:

    // AVX(2)
{$IFDEF x64}
    TEST    dword ptr [RIP + MachineFeaturesWord], AE_MACHINEFEATURES_AVX
    JZ      @SkipAVX
  {$IFDEF Windows}
    LEA     RCX, [Data.ZMM]
  {$ELSE}
    LEA     RDI, [Data.ZMM]
  {$ENDIF}
{$ELSE}
    TEST    dword ptr [MachineFeaturesWord], AE_MACHINEFEATURES_AVX
    JZ      @SkipAVX
    LEA     EAX, [Data.ZMM]
{$ENDIF}
    CALL    GetAVXData
    JMP     @SkipSSE  // SSE state is already stored within AVX
  @SkipAVX:

    // SSE
{$IFDEF x64}
    TEST    dword ptr [RIP + MachineFeaturesWord], AE_MACHINEFEATURES_SSE
    JZ      @SkipSSE
  {$IFDEF Windows}
    LEA     RCX, [Data.ZMM]
  {$ELSE}
    LEA     RDI, [Data.ZMM]
  {$ENDIF}
{$ELSE}
    TEST    dword ptr [MachineFeaturesWord], AE_MACHINEFEATURES_SSE
    JZ      @SkipSSE
    LEA     EAX, [Data.ZMM]
{$ENDIF}
    CALL    GetSSEData
  @SkipSSE:

    // system registers
    SMSW    word ptr [Data.MSW]

    // copy data to thread-local storage
{$IFDEF x64}
  {$IFDEF Windows}
    LEA     RCX, [Data]
  {$ELSE}
    LEA     RDI, [Data]
  {$ENDIF}
{$ELSE}
    LEA     EAX, [Data]
{$ENDIF}
    CALL    CreateRegistersSnapshotCopyToTLS

{$IFDEF x64}
    // result
    MOV     RAX, qword ptr [Data.IP]
{$ELSE}
    // result
    MOV     EAX, dword ptr [Data.IP]
{$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{$IFNDEF AE_StackFramesEnabled}
  {$STACKFRAMES OFF}
{$ENDIF}

{$IFNDEF Windows}
{===============================================================================
--------------------------------------------------------------------------------
                                  TAEMapsParser
--------------------------------------------------------------------------------
===============================================================================}
{
  Format of lines in maps file:

    start_addr-end_addr permstr offset dev_major:dev_minor inode   pathname
}
type
  TAEMapsPermission = (permRead,permWrite,permExecute,permShared,permPrivate);
  TAEMapsPermissions = set of TAEMapsPermission;

type
  TAEMapsLine = record
    StartAddr:    Pointer;
    EndAddr:      Pointer;
    PermStr:      String;
    Permissions:  TAEMapsPermissions;
    Offset:       TMemSize;
    DeviceMajor:  Integer;
    DeviceMinor:  Integer;
    iNode:        UInt64;
    PathName:     String;
  end;

  TAEMapsModule = array of TAEMapsLine;

  TAEMapsModules = array of TAEMapsModule;

type
  TAEMapsParseLineStage = (mplsStartAddr,mplsEndAddr,mplsPerms,mplsOffset,
                           mplsDevMajor,mplsDevMinor,mplsINode,mplsPathName,
                           mplsDone);

{===============================================================================
    TAEMapsParser - class declaration
===============================================================================}
type
  TAEMapsParser = class(TObject)
  protected
    fMapsModules:     TAEMapsModules;
    // line parsing variables
    fParsLineStr:     String;
    fParsStage:       TAEMapsParseLineStage;
    fParsCurrDelim:   Char;
    fParsPosition:    TStrOff;
    fParsTokenStart:  TStrOff;
    fParsTokenLength: TStrSize;
    fParsMapsLine:    TAEMapsLine;
    Function ParseLineProcessToken(const TokenStr: String): Boolean; virtual;
    Function ParseLine(const LineStr: String): Boolean; virtual;
  public
    procedure AddLine(const LineStr: String); virtual;
    property MapsModules: TAEMapsModules read fMapsModules;
  end;

{===============================================================================
    TAEMapsParser - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TAEMapsParser - protected methods
-------------------------------------------------------------------------------}

Function TAEMapsParser.ParseLineProcessToken(const TokenStr: String): Boolean;
var
  Temp64: UInt64;
  Temp:   Integer;
begin
fParsCurrDelim := ' ';
case fParsStage of
  mplsStartAddr:  begin
                    Result := TryStrToUInt64('$' + Trim(TokenStr),Temp64);
                    If Result then
                      begin
                      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                        fParsMapsLine.StartAddr := Pointer(PtrUInt(Temp64));
                      {$IFDEF FPCDWM}{$POP}{$ENDIF}
                        fParsStage := mplsEndAddr;
                      end;
                  end;
  mplsEndAddr:    begin
                    Result := TryStrToUInt64('$' + Trim(TokenStr),Temp64);
                    If Result then
                      begin
                      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                        fParsMapsLine.EndAddr := Pointer(PtrUInt(Temp64));
                      {$IFDEF FPCDWM}{$POP}{$ENDIF}
                        fParsStage := mplsPerms;
                        end;
                  end;
  mplsPerms:      begin
                    fParsMapsLine.PermStr := Trim(TokenStr);
                    fParsMapsLine.Permissions := [];
                    If AnsiContainsText(fParsMapsLine.PermStr,'r') then
                      Include(fParsMapsLine.Permissions,permRead);
                    If AnsiContainsText(fParsMapsLine.PermStr,'w') then
                      Include(fParsMapsLine.Permissions,permWrite);
                    If AnsiContainsText(fParsMapsLine.PermStr,'x') then
                      Include(fParsMapsLine.Permissions,permExecute);
                    If AnsiContainsText(fParsMapsLine.PermStr,'s') then
                      Include(fParsMapsLine.Permissions,permShared);
                    If AnsiContainsText(fParsMapsLine.PermStr,'p') then
                      Include(fParsMapsLine.Permissions,permPrivate);
                    fParsStage := mplsOffset;
                    Result := True;
                  end;
  mplsOffset:      begin
                    Result := TryStrToUInt64('$' + Trim(TokenStr),Temp64);
                    If Result then
                      begin
                        fParsMapsLine.Offset := TMemSize(Temp64);
                        fParsStage := mplsDevMajor;
                        fParsCurrDelim := ':';
                      end;
                  end;
  mplsDevMajor:   begin
                    Result := TryStrToInt('$' + Trim(TokenStr),Temp);
                    If Result then
                      begin
                        fParsMapsLine.DeviceMajor := Temp;
                        fParsStage := mplsDevMinor;
                      end;
                  end;
  mplsDevMinor:   begin
                    Result := TryStrToInt('$' + Trim(TokenStr),Temp);
                    If Result then
                      begin
                        fParsMapsLine.DeviceMinor := Temp;
                        fParsStage := mplsINode;
                      end;
                  end;
  mplsINode:      begin
                    Result := TryStrToUInt64('$' + Trim(TokenStr),Temp64);
                    If Result then
                      begin
                        fParsMapsLine.iNode := Temp64;
                        fParsStage := mplsPathName;
                      end;
                  end;
  mplsPathName:   begin
                    fParsMapsLine.PathName := Trim(TokenStr);
                    fParsStage := mplsDone;
                    If Length(fParsMapsLine.PathName) > 0 then
                      Result := not(AnsiStartsText('[',fParsMapsLine.PathName) and
                                    AnsiEndsText(']',fParsMapsLine.PathName)) and
                                not AnsiEndsText('(deleted)',fParsMapsLine.PathName)
                    else
                      Result := False;
                  end;
else
 {mlpsDone}
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TAEMapsParser.ParseLine(const LineStr: String): Boolean;
begin
Result := False;
fParsLineStr := LineStr;
fParsStage := mplsStartAddr;
fParsCurrDelim := '-';
fParsPosition := 1;
fParsTokenStart := 1;
fParsTokenLength := 0;
while (fParsPosition <= Length(fParsLineStr)) and (fParsStage < mplsPathName) do
  begin
    If fParsLineStr[fParsPosition] = fParsCurrDelim then
      begin
        // Processtoken also changes fParsStage and fParsCurrDelim
        If not ParseLineProcessToken(Copy(fParsLineStr,fParsTokenStart,fParsTokenLength)) then
          Exit;
        fParsTokenStart := Succ(fParsPosition);
        fParsTokenLength := 0;
      end
    else Inc(fParsTokenLength);
    Inc(fParsPosition);
  end;
If ParseLineProcessToken(Copy(fParsLineStr,fParsTokenStart,Length(fParsLineStr))) then
  Result := fParsStage = mplsDone;
end;

{-------------------------------------------------------------------------------
    TAEMapsParser - public methods
-------------------------------------------------------------------------------}

procedure TAEMapsParser.AddLine(const LineStr: String);
var
  i:        Integer;
  ModIdx:   Integer;
  AddrIdx:  Integer;
begin
If ParseLine(LineStr) then
  begin
    // find if this module is already listed
    ModIdx := -1;
    For i := Low(fMapsModules) to High(fMapsModules) do
      If Length(fMapsModules[i]) > 0 then
        If AnsiSameText(fMapsModules[i,0].PathName,fParsMapsLine.PathName) then
          begin
            ModIdx := i;
            Break{For i};
          end;
    If ModIdx < 0 then
      begin
        // this module is not yet listed
      {
        I know growing by one is far from being optimal, ok! But this code is
        not time-critical and I don't feel like keeping some counters around.
      }
        SetLength(fMapsModules,Length(fMapsModules) + 1);
        ModIdx := High(fMapsModules);
      end;
    // add this module and make sure the sublist is ordered by start addresses
    AddrIdx := Length(fMapsModules[ModIdx]);
    For i := Low(fMapsModules[ModIdx]) to High(fMapsModules[ModIdx]) do
      If fMapsModules[ModIdx,i].StartAddr > fParsMapsLine.StartAddr then
        begin
          AddrIdx := i;
          Break{For i};
        end;
    SetLength(fMapsModules[ModIdx],Length(fMapsModules[ModIdx]) + 1);
    For i := High(fMapsModules[ModIdx]) downto Succ(AddrIdx) do
      fMapsModules[ModIdx,i] := fMapsModules[ModIdx,Pred(i)];
    fMapsModules[ModIdx,AddrIdx] := fParsMapsLine;
  end;
end;

{$ENDIF Windows}

{===============================================================================
--------------------------------------------------------------------------------
                               EAEExtendedException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEExtendedException - internals
===============================================================================}
{-------------------------------------------------------------------------------
    EAEExtendedException - internals - registers
-------------------------------------------------------------------------------}
const
  X87CW_EMASK_InvalidOP = UInt16($0001);

//------------------------------------------------------------------------------  

Function GetX87ControlWord: UInt16; register; assembler;
var
  Temp: UInt16;
asm
    FSTCW   word ptr [Temp]
    MOV     AX, word ptr [Temp]
end;

//------------------------------------------------------------------------------

procedure SetX87ControlWord(NewValue: UInt16); register; assembler;
var
  Temp: UInt16;
asm
    MOV     word ptr [Temp], NewValue
    FLDCW   word ptr [Temp]
end;

//------------------------------------------------------------------------------

procedure Float80ToFloat64(F80,F64: Pointer); register; assembler;
asm
    FLD     tbyte ptr [F80]
    FSTP    qword ptr [F64]
    FCLEX   // also issues FWAIT
end;

//------------------------------------------------------------------------------

procedure Float80ToFloat32(F80,F32: Pointer); register; assembler;
asm
    FLD     tbyte ptr [F80]
    FSTP    dword ptr [F32]
    FCLEX
end;

{-------------------------------------------------------------------------------
    EAEExtendedException - internals - stack
-------------------------------------------------------------------------------}
{$IFDEF Windows}      
type
  TThreadInfoBlock = record
    ExceptionList:  Pointer;
    StackBase:      Pointer;
    StackLimit:     Pointer;
    SubSystemTIB:   Pointer;
    case Integer of
      0: (FiberData:      Pointer);
      1: (Version:        UInt32;
          ArbitraryData:  Pointer;
          Self:           Pointer);
  end;
  PThreadInfoBlock = ^TThreadInfoBlock;

//------------------------------------------------------------------------------

Function GetTIB: PThreadInfoBlock; assembler; register;
asm
{$IFDEF x64)}
    MOV   RAX, qword ptr GS:[$30]
{$ELSE}
    MOV   EAX, dword ptr FS:[$18]
{$ENDIF}
end;

{$ENDIF}
//------------------------------------------------------------------------------

{$STACKFRAMES OFF}

Function GetInstructionPointer: Pointer; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    MOV   RAX, [RSP]
{$ELSE}
    MOV   EAX, [ESP]
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetStackPointer: Pointer; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    MOV   RAX, RSP
{$ELSE}
    MOV   EAX, ESP
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetFramePointer: Pointer; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    MOV   RAX, RBP
{$ELSE}
    MOV   EAX, EBP
{$ENDIF}
end;

{$IFDEF AE_StackFramesEnabled}
  {$STACKFRAMES ON}
{$ENDIF}

{===============================================================================
    EAEExtendedException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEExtendedException - protected methods
-------------------------------------------------------------------------------}

procedure EAEExtendedException.InitRegisters;
var
  TLSRegsPtr: PAERegistersStorage;
  i:          Integer;
  OldX87CW:   UInt16;

  procedure DoFloatVectors(Length,Count: Integer);
  var
    ii: Integer;
  begin
    fRegisters.FloatVector.Length := Length;
    fRegisters.FloatVector.Count := Count;
    For ii := 0 to Pred(Count) do
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      Move(Pointer(PtrUInt(Addr(TLSRegsPtr^.ZMM)) + PtrUInt(ii * Length * 4))^,
           fRegisters.FloatVector.Regs[ii],Length * 4);
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
    fRegisters.FloatVector.MXCSR := TLSRegsPtr^.MXCSR;
  end;

begin
If Assigned(RegistersSnapshot) then
  begin
    // prevent reapeated calls to GetTLS
    TLSRegsPtr := RegistersSnapshot;
    // copy registers from TLS to this object's field
    For i := 0 to AE_GPR_HIGH do
      fRegisters.Basic.GeneralPurpose.Regs[i].Native := TLSRegsPtr^.GP[i];
    For i := 0 to 5 do
      fRegisters.Basic.Segment.Regs[i] := TLSRegsPtr^.SG[i];
    fRegisters.Basic.Flags := TLSRegsPtr^.FLAGS;
    fRegisters.Basic.InstructionPointer := TLSRegsPtr^.IP;
    fRegisters.PresentRegisters := [prBasic];
    // x87 FPU
    If MachineFeaturesWord and AE_MACHINEFEATURES_FPU <> 0 then
      begin
        OldX87CW := GetX87ControlWord;
        try
          SetX87ControlWord(OldX87CW or X87CW_EMASK_InvalidOP);
          For i := 0 to 7 do
            begin
              Move(TLSRegsPtr^.Stack[i],fRegisters.Float.Stack[i],10); 
              Float80ToFloat64(Addr(fRegisters.Float.Stack[i].Float80),
                               Addr(fRegisters.Float.Stack[i].Float64));
              Float80ToFloat32(Addr(fRegisters.Float.Stack[i].Float80),
                               Addr(fRegisters.Float.Stack[i].Float32));
            end;
        finally
          SetX87ControlWord(OldX87CW);
        end;
        fRegisters.Float.ControlWord := TLSRegsPtr^.CW;
        fRegisters.Float.StatusWord := TLSRegsPtr^.SW;
        fRegisters.Float.TagWord := TLSRegsPtr^.TW;
        fRegisters.Float.LastInstructionPointerSelector := TLSRegsPtr^.FCS;
        fRegisters.Float.LastInstructionPointerOffset := TLSRegsPtr^.FIP;
        fRegisters.Float.LastDataPointerSelector := TLSRegsPtr^.FDS;
        fRegisters.Float.LastDataPointerOffset := TLSRegsPtr^.FDP;
        fRegisters.Float.OpCode := TLSRegsPtr^.OpCode;
        Include(fRegisters.PresentRegisters,prFPU);
      end;
    // MMX
    If MachineFeaturesWord and AE_MACHINEFEATURES_MMX <> 0 then
      begin
        For i := 0 to 7 do
          fRegisters.IntegerVector.MM[i].VecQuad := TLSRegsPtr^.MM[i];
        Include(fRegisters.PresentRegisters,prMMX);
      end;
    // AVX-512
    If MachineFeaturesWord and AE_MACHINEFEATURES_AVX512 <> 0 then
      begin
        DoFloatVectors(16{64bytes, 512bits},{$IFDEF x64}32{$ELSE}8{$ENDIF});
        For i := 0 to 7 do
          fRegisters.FloatVector.OPMask[i] := TLSRegsPtr^.K[i];
        fRegisters.PresentRegisters := fRegisters.PresentRegisters + [prSSE,prAVX,prAVX512];
      end
    // AVX
    else If MachineFeaturesWord and AE_MACHINEFEATURES_AVX <> 0 then
      begin
        DoFloatVectors(8{32bytes, 256bits},{$IFDEF x64}16{$ELSE}8{$ENDIF});
        fRegisters.PresentRegisters := fRegisters.PresentRegisters + [prSSE,prAVX];
      end
    // SSE  
    else If MachineFeaturesWord and AE_MACHINEFEATURES_SSE <> 0 then
      begin
        DoFloatVectors(4{16bytes, 128bits},{$IFDEF x64}16{$ELSE}8{$ENDIF});
        fRegisters.PresentRegisters := fRegisters.PresentRegisters + [prSSE];
      end;
    // system regs
    fRegisters.System.MachineStatusWord := TLSRegsPtr^.MSW;
    // cleanup
    FreeMem(RegistersSnapshot,SizeOf(TAERegistersStorage));
    RegistersSnapshot := nil;
  end
else fRegisters.PresentRegisters := [];
end;

//------------------------------------------------------------------------------

procedure EAEExtendedException.InitStackTrace;
type
  TAEStackLinkingInfo = packed record
    FrameBase:      Pointer;
    ReturnAddress:  Pointer;
  end;
  PAEStackLinkingInfo = ^TAEStackLinkingInfo;

  Function LoadFrame(PrevFrame: TAEStackFrame; out NextFrame: TAEStackFrame): Boolean;
  begin
    Result := False;
    NextFrame.FrameBase := PAEStackLinkingInfo(PrevFrame.InDump.FrameBase)^.FrameBase;
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    NextFrame.FrameStart := Pointer(PtrUInt(NextFrame.FrameBase) + PtrUInt(SizeOf(TAEStackLinkingInfo)));
    NextFrame.FrameAddress := PrevFrame.FrameStart;
  {
    Start of the next frame must be both below or at the stack bottom and
    above or at previous frame start.
  }
    If (PtrUInt(NextFrame.FrameStart) <= PtrUInt(fStackTrace.StackBottom)) and
       (PtrUInt(NextFrame.FrameBase) >= PtrUInt(PrevFrame.FrameStart)) then
      begin
        NextFrame.FrameSize := TMemSize(PtrUInt(NextFrame.FrameStart) - PtrUInt(NextFrame.FrameAddress));
        NextFrame.OwnerFunction := PAEStackLinkingInfo(PrevFrame.InDump.FrameBase)^.ReturnAddress;
        NextFrame.InDump.FrameStart := Pointer(PtrUInt(PrevFrame.InDump.FrameStart) + PtrUInt(NextFrame.FrameSize));
        NextFrame.InDump.FrameBase := Pointer(PtrUInt(PtrUInt(NextFrame.InDump.FrameStart) - PtrUInt(SizeOf(TAEStackLinkingInfo))));
        NextFrame.InDump.FrameAddress := Pointer(PtrUInt(PrevFrame.InDump.FrameAddress) + PtrUInt(PrevFrame.FrameSize));
        NextFrame.LinkingInfo.FrameBase := PAEStackLinkingInfo(NextFrame.InDump.FrameBase)^.FrameBase;
        NextFrame.LinkingInfo.ReturnAddress := PAEStackLinkingInfo(NextFrame.InDump.FrameBase)^.ReturnAddress;
        Result := True;
      end;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end;

var
  FrameCount:   Integer;
  TempFrame:    TAEStackFrame;

  procedure AddFrame(NewFrame: TAEStackFrame);
  begin
    If Length(fStackTrace.StackFrames) <= FrameCount then
      SetLength(fStackTrace.StackFrames,Length(fStackTrace.StackFrames) + 16);
    fStackTrace.StackFrames[FrameCount] := NewFrame;
    Inc(FrameCount);
  end;

begin
TempFrame.OwnerFunction := GetInstructionPointer;
TempFrame.FrameBase := GetFramePointer;
TempFrame.FrameAddress := GetStackPointer;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
TempFrame.FrameStart := Pointer(PtrUInt(TempFrame.FrameBase) + PtrUInt(SizeOf(TAEStackLinkingInfo)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
// get global stack info
{$IFDEF Windows}
fStackTrace.StackBottom := GetTIB^.StackBase;
{$ELSE}
{
  NOTE - In Intel's documentation, the stack bottom is its base (ie. its
         largest address, because it is growing down), and stack top is the the
         current RSP/ESP (points to the last pushed value). In FPC system unit,
         this nomenclature is reversed. We stick to Intel scheme.
}
fStackTrace.StackBottom := System.StackTop;
{$ENDIF}
fStackTrace.StackTop := TempFrame.FrameAddress;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
// stack sanity check
If PtrUInt(fStackTrace.StackBottom) >= PtrUInt(fStackTrace.StackTop) then
  begin
    fStackTrace.StackDumpSize := TMemSize(PtrUInt(fStackTrace.StackBottom) - PtrUInt(fStackTrace.StackTop));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
    // dump the stack
    GetMem(fStackTrace.StackDump,fStackTrace.StackDumpSize);
    Move(fStackTrace.StackTop^,fStackTrace.StackDump^,fStackTrace.StackDumpSize);
    // scan the dumped stack
    SetLength(fStackTrace.StackFrames,0);
    FrameCount := 0;
    // sanity checks of current frame
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If (PtrUInt(TempFrame.FrameStart) <= PtrUInt(fStackTrace.StackBottom)) and
       (PtrUInt(TempFrame.FrameBase) >= PtrUInt(fStackTrace.StackTop)) then
      begin
        // process the current frame
        TempFrame.FrameSize := TMemSize(PtrUInt(TempFrame.FrameStart) - PtrUInt(TempFrame.FrameAddress));
        TempFrame.InDump.FrameStart := Pointer(PtrUInt(fStackTrace.StackDump) + PtrUInt(TempFrame.FrameSize));
        TempFrame.InDump.FrameBase := Pointer(PtrUInt(PtrUInt(TempFrame.InDump.FrameStart) - PtrUInt(SizeOf(TAEStackLinkingInfo))));
        TempFrame.InDump.FrameAddress := fStackTrace.StackDump;
        TempFrame.LinkingInfo.FrameBase := PAEStackLinkingInfo(TempFrame.InDump.FrameBase)^.FrameBase;
        TempFrame.LinkingInfo.ReturnAddress := PAEStackLinkingInfo(TempFrame.InDump.FrameBase)^.ReturnAddress;
        AddFrame(TempFrame);
        while LoadFrame(TempFrame,TempFrame) do
          AddFrame(TempFrame);
        SetLength(fStackTrace.StackFrames,FrameCount);
      end;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end
else FillChar(fStackTrace,SizeOf(TAEStackTrace),0);
end;

//------------------------------------------------------------------------------

procedure EAEExtendedException.InitProcessInfo;
begin
fProcessInfo.ProcessID := fProcessID;
fProcessInfo.ExecFileName := ParamStr(0);
fProcessInfo.FileInfo := TWinFileInfo.Create(fProcessInfo.ExecFileName,WFI_LS_All);
end;

//------------------------------------------------------------------------------

procedure EAEExtendedException.InitModuleInfo;
{$IFDEF Windows}
{$IF not Declared(UNICODE_STRING_MAX_CHARS)}
const
  UNICODE_STRING_MAX_CHARS = 32767;
{$IFEND}
var
  ModuleFileName: WideString;
begin
SetLength(ModuleFileName,UNICODE_STRING_MAX_CHARS);
SetLength(ModuleFileName,GetModuleFileNameExW(GetCurrentProcess,hInstance,PWideChar(ModuleFileName),Length(ModuleFileName)));
fModuleInfo.FileName := WideToStr(ModuleFileName);
fModuleInfo.FileInfo := TWinFileInfo.Create(fModuleInfo.FileName,WFI_LS_All);
end;
{$ELSE}
var
  ModuleFileName: String;
  Info:           dl_info;
begin
ModuleFileName := '';
FillChar(Addr(Info)^,SizeOf(dl_info),0);
If dladdr(@GetX87ControlWord{a function in this unit},@Info) <> 0 then
  If Assigned(Info.dli_fname) then
    ModuleFileName := ExpandFileName(String(Info.dli_fname));
fModuleInfo.FileName := ModuleFileName;
fModuleInfo.FileInfo := TWinFileInfo.Create(fModuleInfo.FileName,WFI_LS_All);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EAEExtendedException.InitModulesInfo;
{$IFDEF Windows}
var
  HelpSnapshot: THandle;
  ModuleEntry:  TModuleEntry32W;
  ModuleCount:  Integer;

  procedure AddModuleInfoFromEntry;
  begin
    // grow the list
    If Length(fModulesInfo.Modules) <= ModuleCount then
      SetLength(fModulesInfo.Modules,Length(fModulesInfo.Modules) + 16);
    // copy and get data
    fModulesInfo.Modules[ModuleCount].ModuleFileName := WideToStr(ModuleEntry.szExePath);
    fModulesInfo.Modules[ModuleCount].BaseAddress := Pointer(ModuleEntry.modBaseAddr);
    fModulesInfo.Modules[ModuleCount].ExecMemory := Pointer(ModuleEntry.modBaseAddr);
    fModulesInfo.Modules[ModuleCount].ExecSize := TMemSize(ModuleEntry.modBaseSize);
    fModulesInfo.Modules[ModuleCount].ExecContiguous := True;
    fModulesInfo.Modules[ModuleCount].FileInfo := TWinFileInfo.Create(
      fModulesInfo.Modules[ModuleCount].ModuleFileName,WFI_LS_All);
    Inc(ModuleCount);
  end;

begin
HelpSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,0{current process});
If HelpSnapshot <> INVALID_HANDLE_VALUE then
  begin
    ModuleCount := 0;
    SetLength(fModulesInfo.Modules,0);
    ModuleEntry.dwSize := SizeOf(TModuleEntry32W);
    // enum the modules
    If Module32FirstW(HelpSnapshot,ModuleEntry) then
      repeat
        AddModuleInfoFromEntry;
      until not Module32NextW(HelpSnapshot,ModuleEntry);
    SetLength(fModulesInfo.Modules,ModuleCount);
    CloseHandle(HelpSnapshot);
  end
else FillChar(fModulesInfo,SizeOf(TAEModulesInfo),0);
end;
{$ELSE}
var
  MapsFile:     TStringList;
  MapsParser:   TAEMapsParser;
  i,j:          Integer;
  LastExecEnd:  Pointer;
begin
MapsFile := TStringList.Create;
try
  MapsFile.LoadFromFile(Format('/proc/%u/maps',[fProcessID]));
  MapsParser := TAEMapsParser.Create;
  try
    For i := 0 to Pred(MapsFile.Count) do
      MapsParser.AddLine(MapsFile[i]);
    // get individual modules info
    SetLength(fModulesInfo.Modules,Length(MapsParser.MapsModules));
    For i := Low(fModulesInfo.Modules) to High(fModulesInfo.Modules) do
      begin
        fModulesInfo.Modules[i].ModuleFileName := MapsParser.MapsModules[i,0].PathName;
        fModulesInfo.Modules[i].BaseAddress := MapsParser.MapsModules[i,0].StartAddr;
        // get info about executable memory
        fModulesInfo.Modules[i].ExecMemory := nil;
        fModulesInfo.Modules[i].ExecSize := 0;
        fModulesInfo.Modules[i].ExecContiguous := True;
        LastExecEnd := nil;
        For j := Low(MapsParser.MapsModules[i]) to High(MapsParser.MapsModules[i]) do
          If permExecute in MapsParser.MapsModules[i,j].Permissions then
            begin
              If not Assigned(fModulesInfo.Modules[i].ExecMemory) then
                fModulesInfo.Modules[i].ExecMemory := MapsParser.MapsModules[i,j].StartAddr;
              fModulesInfo.Modules[i].ExecSize := fModulesInfo.Modules[i].ExecSize +
            {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                TMemSize(PtrUInt(MapsParser.MapsModules[i,j].EndAddr) - PtrUInt(MapsParser.MapsModules[i,j].StartAddr));
            {$IFDEF FPCDWM}{$POP}{$ENDIF}
              If Assigned(LastExecEnd) then
                If LastExecEnd <> MapsParser.MapsModules[i,j].StartAddr then
                  fModulesInfo.Modules[i].ExecContiguous := False;
              LastExecEnd := MapsParser.MapsModules[i,j].EndAddr;
            end;
        // get file info
        fModulesInfo.Modules[i].FileInfo := TWinFileInfo.Create(
          fModulesInfo.Modules[i].ModuleFileName,WFI_LS_ALL);
      end;
    {
      Remove all files that do not have any executable memory - we are assuming
      they are not true binary executable modules, but ordinary memory mapped
      files.
    }
    For i := High(fModulesInfo.Modules) downto Low(fModulesInfo.Modules) do
      If fModulesInfo.Modules[i].ExecSize <= 0 then
        begin
          fModulesInfo.Modules[i].FileInfo.Free;
          For j := i to Pred(High(fModulesInfo.Modules)) do
             fModulesInfo.Modules[i] := fModulesInfo.Modules[i + 1];
          SetLength(fModulesInfo.Modules,Length(fModulesInfo.Modules) - 1);
        end;
  finally
    MapsParser.Free;
  end;
finally
  MapsFile.Free;
end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EAEExtendedException.InitThreadsInfo;
{$IFDEF Windows}
var
  HelpSnapshot: THandle;
  ThreadEntry:  TThreadEntry32;
  ThreadCount:  Integer;
begin
HelpSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
If HelpSnapshot <> INVALID_HANDLE_VALUE then
  try
    ThreadCount := 0;
    SetLength(fThreadsInfo.Threads,0);
    ThreadEntry.dwSize := SizeOf(TThreadEntry32);
    // enum the modules
    If Thread32First(HelpSnapshot,ThreadEntry) then
      repeat
        // check that the thread IDs are available
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        If ThreadEntry.dwSize < (PtrUInt(Addr(TThreadEntry32(nil^).th32ThreadID)) + SizeOf(DWORD)) then
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
          Exit;
        // all threads in system are enumerated, get only those belonging to this process
        If ThreadEntry.th32OwnerProcessID = fProcessID then
          begin
            // grow the list
            If Length(fThreadsInfo.Threads) <= ThreadCount then
              SetLength(fThreadsInfo.Threads,Length(fThreadsInfo.Threads) + 16);
            fThreadsInfo.Threads[ThreadCount].ThreadID := ThreadEntry.th32ThreadID;
            Inc(ThreadCount);
          end;
      until not Thread32Next(HelpSnapshot,ThreadEntry);
    SetLength(fThreadsInfo.Threads,ThreadCount);
  finally
    CloseHandle(HelpSnapshot);
  end
else FillChar(fThreadsInfo,SizeOf(TAEThreadsInfo),0);
end;
{$ELSE}
const
  DT_DIR = 4;
var
  DirStreamPtr: PDir;
  DirEntryPtr:  PDirEnt;
  TempStrLen:   TStrSize;
  TempStr:      String;
  TempTID:      TAEThreadID;
  ThreadCount:  Integer;
begin
DirStreamPtr := fpOpenDir(Format('/proc/%d/task/',[fProcessID]));
If Assigned(DirStreamPtr) then
  try
    ThreadCount := 0;
    SetLength(fThreadsInfo.Threads,0);
    repeat
      DirEntryPtr := fpReadDir(DirStreamPtr^);
      If Assigned(DirEntryPtr) then
        If DirEntryPtr^.d_type = DT_DIR then
          begin
            TempStrLen := StrLen(DirEntryPtr^.d_name);
            SetLength(TempStr,TempStrLen);
            Move(DirEntryPtr^.d_name,PChar(TempStr)^,TempStrLen * SizeOf(Char));
            If TryStrToInt(TempStr,TempTID) then
              begin
                If Length(fThreadsInfo.Threads) <= ThreadCount then
                  SetLength(fThreadsInfo.Threads,Length(fThreadsInfo.Threads) + 16);
                fThreadsInfo.Threads[ThreadCount].ThreadID := TempTID;
                Inc(ThreadCount);
              end;
          end;
    until not Assigned(DirEntryPtr);
    SetLength(fThreadsInfo.Threads,ThreadCount);
  finally
    fpCloseDir(DirStreamPtr^);
  end
else FillChar(fThreadsInfo,SizeOf(TAEThreadsInfo),0);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EAEExtendedException.Finalize;
var
  i:  Integer;
begin
If Assigned(fStackTrace.StackDump) and (fStackTrace.StackDumpSize <> 0) then
  FreeMem(fStackTrace.StackDump,fStackTrace.StackDumpSize);
If Assigned(fProcessInfo.FileInfo) then
  fProcessInfo.FileInfo.Free;
If Assigned(fModuleInfo.FileInfo) then
  fModuleInfo.FileInfo.Free;
For i := Low(fModulesInfo.Modules) to High(fModulesInfo.Modules) do
  fModulesInfo.Modules[i].FileInfo.Free;
end;

{-------------------------------------------------------------------------------
    EAEExtendedException - public methods
-------------------------------------------------------------------------------}

constructor EAEExtendedException.CreateFmt(const Msg: String; Args: array of const);
begin
If not Assigned(RegistersSnapshot) then
  CreateRegistersSnapshot;
inherited CreateFmt(Msg,Args);
// registers snapshot
{$IFDEF ExtendedExceptionRegisters}
InitRegisters;
{$ELSE}
fRegisters.PresentRegisters := [];
{$ENDIF}
// stack trace
{$IFDEF ExtendedExceptionStack}
InitStackTrace;
{$ELSE}
FillChar(fStackTrace,SizeOf(TAEStackTrace),0);
{$ENDIF}
// process file info
{$IFDEF ExtendedExceptionProcess}
InitProcessInfo;
{$ELSE}
FillChar(fProcessInfo,SizeOf(TAEProcessInfo),0);
{$ENDIF}
// current module info
{$IFDEF ExtendedExceptionProcess}
InitModuleInfo;
{$ELSE}
FillChar(fModuleInfo,SizeOf(TAECurrModuleInfo),0);
{$ENDIF}  
// modules info
{$IFDEF ExtendedExceptionModules}
InitModulesInfo;
{$ELSE}
FillChar(fModulesInfo,SizeOf(TAEModulesInfo),0);
{$ENDIF}
// threads info
{$IFDEF ExtendedExceptionThreads}
InitThreadsInfo;
{$ELSE}
FillChar(fThreadsInfo,SizeOf(TAEThreadsInfo),0);
{$ENDIF}
end;

//------------------------------------------------------------------------------

destructor EAEExtendedException.Destroy;
begin
Finalize;
inherited;
end;

{$ENDIF ExtendedException}

{===============================================================================
--------------------------------------------------------------------------------
                               EAEGeneralException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEGeneralException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEGeneralException - public methods
-------------------------------------------------------------------------------}

constructor EAEGeneralException.CreateFmt(const Msg: String; Args: array of const; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,Args);
If Assigned(FaultObject) then
  fFaultingObject := Format('%s(%p)',[FaultObject.ClassName,Pointer(FaultObject)])
else
  fFaultingObject := '';
fFaultingFunction := FaultFunction;
If Assigned(FaultObject) then
  begin
    If Length(fFaultingFunction) > 0 then
      fFullMessage := Format(Format('%s.%s: %s',[fFaultingObject,fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Format('%s: %s',[fFaultingObject,Msg]),Args);
  end
else
  begin
    If Length(fFaultingFunction) > 0 then
      fFullMessage := Format(Format('%s: %s',[fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Msg,Args);
  end;
end;

//------------------------------------------------------------------------------

constructor EAEGeneralException.Create(const Msg: String; FaultObject: TObject; const FaultFunction: String);
begin
CreateFmt(Msg,[],FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 EAESystemError
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAESystemError - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    EAESystemError - public methods
-------------------------------------------------------------------------------}

constructor EAESystemError.Create(ErrorCode: TAESysErrCode; FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String);
begin
If FullSysMsg then
  inherited CreateFmt({$IFDEF Windows}'System error %u: %s'{$ELSE}'System error %d: %s'{$ENDIF},
                      [ErrorCode,SysErrorMessage(ErrorCode)],FaultObject,FaultFunction)
else
  inherited CreateFmt({$IFDEF Windows}'System error occured (%u).'{$ELSE}'System error occured (%d).'{$ENDIF},
                      [ErrorCode],FaultObject,FaultFunction);
fErrorCode := ErrorCode;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAESystemError.Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String);
begin
Create({$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF},FullSysMsg,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                EAEIndexException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexException - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexException.GetDefaultMessage: String;
begin
Result := 'Index (%d) error.';
end;

{-------------------------------------------------------------------------------
    EAEIndexException - public methods
-------------------------------------------------------------------------------}

constructor EAEIndexException.Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[Index],FaultObject,FaultFunction);
fIndex := Index;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEIndexException.Create(Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage,Index,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               EAEIndexOutOfBounds
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexOutOfBounds - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexOutOfBounds - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexOutOfBounds.GetDefaultMessage: String;
begin
Result := 'Index (%d) out of bounds.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooLow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooLow - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexTooLow - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexTooLow.GetDefaultMessage: String;
begin
Result := 'Index (%d) too low.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooHigh
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooHigh - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexTooHigh - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexTooHigh.GetDefaultMessage: String;
begin
Result := 'Index (%d) too high.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexInvalid - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexInvalid - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexInvalid.GetDefaultMessage: String;
begin
Result := 'Index (%d) is invalid.';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                EAEValueException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEValueException - protected methods
-------------------------------------------------------------------------------}

class Function EAEValueException.VariantToStr(Value: Variant): String;
begin
If (VarType(Value) and varArray) <> 0 then
  Result := VariantArrayToStr(Value)
else
  Result := VarToStrDef(Value,'ERROR');
end;

//------------------------------------------------------------------------------

class Function EAEValueException.VariantArrayToStr(Value: Variant): String;
var
  Dimensions: Integer;
  Indices:    array of Integer;

  procedure ConvertVarArrayDimension(var Str: String; Dim: Integer);
  var
    Index:  Integer;
  begin
    Str := Str + '[';
    For Index := VarArrayLowBound(Value,Dim) to VarArrayHighBound(Value,Dim) do
      begin
        Indices[Pred(Dim)] := Index;
        If Dim >= Dimensions then
          begin
            If Index <> VarArrayHighBound(Value,Dim) then
              Str := Str + VariantToStr(VarArrayGet(Value,Indices)) + ','
            else
              Str := Str + VariantToStr(VarArrayGet(Value,Indices));
          end
        else ConvertVarArrayDimension(Str,Dim + 1);
      end;
    Str := Str + ']';
  end;

begin
Result := '';
Dimensions := VarArrayDimCount(Value);
If Dimensions > 0 then
  begin
    SetLength(Indices,Dimensions);
    ConvertVarArrayDimension(Result,1);
  end;
end;

//------------------------------------------------------------------------------

class Function EAEValueException.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Value %s error (%s).'
else
  Result := 'Value %s error.';
end;

{-------------------------------------------------------------------------------
    EAEValueException - public methods
-------------------------------------------------------------------------------}

constructor EAEValueException.Create(const Msg,ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[ValueName,VariantToStr(Value)],FaultObject,FaultFunction);
fValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const Msg,ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[ValueName],FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(True),ValueName,Value,FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(False),ValueName,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 EAEValueInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalid - class implementation
===============================================================================}

class Function EAEValueInvalid.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s value (%s).'
else
  Result := 'Invalid %s value.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                             EAEValueInvalidNameOnly
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalidNameOnly - class implementation
===============================================================================}

class Function EAEValueInvalidNameOnly.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s (%s).'
else
  Result := 'Invalid %s.';
end;

{$ENDIF AE_Include_Implementation}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_UnitInitFinal}
{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization                                             
--------------------------------------------------------------------------------
===============================================================================}

{$IFDEF ExtendedException}

procedure UnitInitialize;
begin
with TSimpleCPUID.Create do
try
  MachineFeaturesWord := 0;
  // prepare machine features word
  If Info.SupportedExtensions.X87 and not Info.SupportedExtensions.EmulatedX87 then
    MachineFeaturesWord := MachineFeaturesWord or AE_MACHINEFEATURES_FPU;
  If Info.SupportedExtensions.MMX then
    MachineFeaturesWord := MachineFeaturesWord or AE_MACHINEFEATURES_MMX;
  If Info.SupportedExtensions.SSE then
    MachineFeaturesWord := MachineFeaturesWord or AE_MACHINEFEATURES_SSE;
  If Info.SupportedExtensions.AVX then
    MachineFeaturesWord := MachineFeaturesWord or AE_MACHINEFEATURES_AVX;
  If Info.SupportedExtensions.AVX512F then
    MachineFeaturesWord := MachineFeaturesWord or AE_MACHINEFEATURES_AVX512;
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
If Assigned(RegistersSnapshot) then
  FreeMem(RegistersSnapshot,SizeOf(TAERegistersStorage));
end;

//------------------------------------------------------------------------------

initialization
  UnitInitialize;

finalization
  UnitFinalize;

{$ENDIF}

{$ENDIF AE_Include_UnitInitFinal}
