{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SimpleCPUID

    Small library designed to provide some basic parsed information (mainly CPU
    features) obtained by the CPUID instruction on x86(-64) processors.
    Should be compatible with any Windows and Linux system running on x86(-64)
    architecture.

  Version 1.3 (2023-04-23)

  Last change 2024-03-05

  ©2016-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleCPUID

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

  Sources:
    - en.wikipedia.org/wiki/CPUID
    - sandpile.org/x86/cpuid.htm
    - Intel® 64 and IA-32 Architectures Software Developer’s Manual (April 2022)
    - AMD64 Architecture Programmer’s Manual; Publication #40332 Revision 4.02
      (November 2020)
    - AMD CPUID Specification; Publication #25481 Revision 2.34 (September 2010)

===============================================================================}
unit SimpleCPUID;
{
  SimpleCPUID_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.

  This unit cannot be compiled without asm, but meh...
}
{$IFDEF SimpleCPUID_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
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
  {$MODESWITCH DuplicateLocals+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$ASMMODE Intel}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled without ASM.'}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESCIDException = class(Exception);

  ESCIDSystemError      = class(ESCIDException);
  ESCIDIndexOutOfBounds = class(ESCIDException);
  ESCIDInvalidProcessor = class(ESCIDException);

{===============================================================================
    Main CPUID routines
===============================================================================}

Function CPUIDSupported: LongBool; register; assembler;

procedure CPUID(Leaf, SubLeaf: UInt32; Result: Pointer); register; overload; assembler;
procedure CPUID(Leaf: UInt32; Result: Pointer); overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleCPUID
--------------------------------------------------------------------------------
===============================================================================}
type
  TCPUIDResult = packed record
    EAX,EBX,ECX,EDX:  UInt32;
  end;
  PCPUIDResult = ^TCPUIDResult;

  TCPUIDLeaf = record
    ID:       UInt32;
    Data:     TCPUIDResult;
    SubLeafs: array of TCPUIDResult;
  end;
  PCPUIDLeaf = ^TCPUIDLeaf;

  TCPUIDLeafs = array of TCPUIDLeaf;

const
  NullLeaf: TCPUIDResult = (EAX: 0; EBX: 0; ECX: 0; EDX: 0);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

type
  TCPUIDManufacturerID = (mnOthers,mnAMD,mnCentaur,mnCyrix,mnIntel,mnTransmeta,
                          mnNationalSemiconductor,mnNexGen,mnRise,mnSiS,mnUMC,
                          mnVIA,mnVortex);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  TCPUIDInfo_AdditionalInfo = record
    BrandID:                Byte;
    CacheLineFlushSize:     Word; // in bytes (raw data is in qwords)
    LogicalProcessorCount:  Byte; // HTT (see features) must be on, otherwise reserved
    LocalAPICID:            Byte;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  TCPUIDInfo_Features = record
  {leaf 1, ECX register}
    SSE3,           // [00] SSE3 Extensions
    PCLMULQDQ,      // [01] Carryless Multiplication
    DTES64,         // [02] 64-bit Debug Store Area
    MONITOR,        // [03] MONITOR and MWAIT Instructions
    DS_CPL,         // [04] CPL Qualified Debug Store
    VMX,            // [05] Virtual Machine Extensions
    SMX,            // [06] Safer Mode Extensions
    EIST,           // [07] Enhanced Intel SpeedStep® Technology
    TM2,            // [08] Thermal Monitor 2
    SSSE3,          // [09] SSSE3 Extensions
    CNXT_ID,        // [10] L1 Context ID
    SDBG,           // [11]	Silicon Debug interface
    FMA,            // [12] Fused Multiply Add
    CMPXCHG16B,     // [13] CMPXCHG16B Instruction
    xTPR,           // [14] Update Control (Can disable sending task priority messages)
    PDCM,           // [15] Perform & Debug Capability MSR
    PCID,           // [17] Process-context Identifiers
    DCA,            // [18] Direct Cache Access
    SSE4_1,         // [19] SSE4.1 Instructions
    SSE4_2,         // [20] SSE4.2 Instructions
    x2APIC,         // [21] x2APIC Support
    MOVBE,          // [22] MOVBE Instruction
    POPCNT,         // [23] POPCNT Instruction
    TSC_Deadline,   // [24] APIC supports one-shot operation using a TSC deadline value
    AES,            // [25] AES Instruction Set
    XSAVE,          // [26] XSAVE, XRESTOR, XSETBV, XGETBV Instructions
    OSXSAVE,        // [27] XSAVE enabled by OS
    AVX,            // [28] Advanced Vector Extensions
    F16C,           // [29] F16C (half-precision) FP Support
    RDRAND,         // [30] RDRAND (on-chip random number generator) Support
    HYPERVISOR,     // [31] Running on a hypervisor (always 0 on a real CPU)
  {leaf 1, EDX register}
    FPU,            // [00] x87 FPU on Chip
    VME,            // [01] Virtual-8086 Mode Enhancement
    DE,             // [02] Debugging Extensions
    PSE,            // [03] Page Size Extensions
    TSC,            // [04] Time Stamp Counter
    MSR,            // [05] RDMSR and WRMSR Support
    PAE,            // [06] Physical Address Extensions
    MCE,            // [07] Machine Check Exception
    CX8,            // [08] CMPXCHG8B Instruction
    APIC,           // [09] APIC on Chip
    SEP,            // [11] SYSENTER and SYSEXIT Instructions
    MTRR,           // [12] Memory Type Range Registers
    PGE,            // [13] PTE Global Bit
    MCA,            // [14] Machine Check Architecture
    CMOV,           // [15] Conditional Move/Compare Instruction
    PAT,            // [16] Page Attribute Table
    PSE_36,         // [17] Page Size Extension
    PSN,            // [18] Processor Serial Number
    CLFSH,          // [19] CLFLUSH Instruction
    DS,             // [21] Debug Store
    ACPI,           // [22] Thermal Monitor and Clock Control
    MMX,            // [23] MMX Technology
    FXSR,           // [24] FXSAVE/FXRSTOR Instructions
    SSE,            // [25] SSE Extensions
    SSE2,           // [26] SSE2 Extensions
    SS,             // [27] Self Snoop
    HTT,            // [28] Multi-threading
    TM,             // [29] Thermal Monitor
    IA64,           // [30] IA64 processor emulating x86
    PBE,            // [31] Pending Break Enable
  {leaf 7:0, EBX register}
    FSGSBASE,       // [00] RDFSBASE/RDGSBASE/WRFSBASE/WRGSBASE Support
    TSC_ADJUST,     // [01] IA32_TSC_ADJUST MSR Support
    SGX,            // [02] Intel Software Guard Extensions (Intel SGX Extensions)
    BMI1,           // [03] Bit Manipulation Instruction Set 1
    HLE,            // [04] Transactional Synchronization Extensions
    AVX2,           // [05] Advanced Vector Extensions 2
    FPDP,           // [06] x87 FPU Data Pointer updated only on x87 exceptions
    SMEP,           // [07] Supervisor-Mode Execution Prevention
    BMI2,           // [08] Bit Manipulation Instruction Set 2
    ERMS,           // [09] Enhanced REP MOVSB/STOSB
    INVPCID,        // [10] INVPCID Instruction
    RTM,            // [11] Transactional Synchronization Extensions
    PQM,            // [12] Platform Quality of Service Monitoring
    FPCSDS,         // [13] FPU CS and FPU DS deprecated
    MPX,            // [14] Intel MPX (Memory Protection Extensions)
    PQE,            // [15] Platform Quality of Service Enforcement
    AVX512F,        // [16] AVX-512 Foundation
    AVX512DQ,       // [17] AVX-512 Doubleword and Quadword Instructions
    RDSEED,         // [18] RDSEED instruction
    ADX,            // [19] Intel ADX (Multi-Precision Add-Carry Instruction Extensions)
    SMAP,           // [20] Supervisor Mode Access Prevention
    AVX512IFMA,     // [21] AVX-512 Integer Fused Multiply-Add Instructions
    PCOMMIT,        // [22] PCOMMIT Instruction
    CLFLUSHOPT,     // [23] CLFLUSHOPT Instruction
    CLWB,           // [24] CLWB Instruction
    PT,             // [25] Intel Processor Trace
    AVX512PF,       // [26] AVX-512 Prefetch Instructions
    AVX512ER,       // [27] AVX-512 Exponential and Reciprocal Instructions
    AVX512CD,       // [28] AVX-512 Conflict Detection Instructions
    SHA,            // [29] Intel SHA extensions
    AVX512BW,       // [30] AVX-512 Byte and Word Instructions
    AVX512VL,       // [31] AVX-512 Vector Length Extensions
  {leaf 7:0, ECX register}
    PREFETCHWT1,    // [00] PREFETCHWT1 Instruction
    AVX512VBMI,     // [01] AVX-512 Vector Bit Manipulation Instructions
    UMIP,           // [02] User-mode Instruction Prevention
    PKU,            // [03] Memory Protection Keys for User-mode pages
    OSPKE,          // [04] PKU enabled by OS
    CET,            // [07] ??? (http://sandpile.org/x86/cpuid.htm)
    VA57:  Boolean; // [16] 5-level paging, CR4.VA57
    MAWAU: Byte;    // [17..21] The value of MAWAU (User MPX (Memory Protection Extensions) address-width adjust)
                    //          used by the BNDLDX and BNDSTX instructions in 64-bit mode.
    RDPID,          // [22] Read Processor ID
    SGX_LC,         // [30] SGX Launch Configuration
  {leaf 7:0, EDX register}
    AVX512QVNNIW,   // [02] AVX-512 Neural Network Instructions
    AVX512QFMA:     // [03] AVX-512 Multiply Accumulation Single precision
      Boolean;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  TCPUIDInfo_ExtendedFeatures = record
  {leaf $80000001, ECX register}
    AHF64,          // [00] LAHF/SAHF available in 64-bit mode
    CMP,            // [01] Hyperthreading not valid (HTT=1 indicates HTT(0) or CMP(1))
    SVM,            // [02] Secure Virtual Machine
    EAS,            // [03] Extended APIC space
    CR8D,           // [04] CR8 in 32-bit mode
    LZCNT,          // [05] Advanced bit manipulation (lzcnt and popcnt)
    ABM,            // [05] Equal to LZCNT
    SSE4A,          // [06] SSE4a Support
    MSSE,           // [07] Misaligned SSE mode
    _3DNowP,        // [08] PREFETCH and PREFETCHW Instructions
    OSVW,           // [09] OS Visible Workaround
    IBS,            // [10] Instruction Based Sampling
    XOP,            // [11] XOP instruction set
    SKINIT,         // [12] SKINIT/STGI instructions
    WDT,            // [13] Watchdog timer
    LWP,            // [15] Light Weight Profiling
    FMA4,           // [16] 4 operands fused multiply-add
    TCE,            // [17] Translation Cache Extension
    NODEID,         // [19] NodeID MSR
    TBM,            // [21] Trailing Bit Manipulation
    TOPX,           // [22] Topology Extensions
    PCX_CORE,       // [23] Core performance counter extensions
    PCX_NB,         // [24] NB performance counter extensions
    DBX,            // [26] Data breakpoint extensions
    PERFTSC,        // [27] Performance TSC
    PCX_L2I,        // [28] L2I performance counter extensions
    MON,            // [29] MONITORX/MWAITX Instructions
  {leaf $80000001, EDX register}
    FPU,            // [00] Onboard x87 FPU
    VME,            // [01] Virtual mode extensions (VIF)
    DE,             // [02] Debugging extensions (CR4 bit 3)
    PSE,            // [03] Page Size Extension
    TSC,            // [04] Time Stamp Counter
    MSR,            // [05] Model-specific registers
    PAE,            // [06] Physical Address Extension
    MCE,            // [07] Machine Check Exception
    CX8,            // [08] CMPXCHG8 (compare-and-swap) Instruction
   {
   	If the APIC has been disabled, then the APIC feature flag will read as 0.
   }
    APIC,           // [09] Onboard Advanced Programmable Interrupt Controller
   {
    The AMD K6 processor, model 6, uses bit 10 to indicate SEP. Beginning with
    model 7, bit 11 is used instead.
    Intel processors only report SEP when CPUID is executed in PM64.
   }
    SEP,            // [11] SYSCALL and SYSRET Instructions
    MTRR,           // [12] Memory Type Range Registers
    PGE,            // [13] Page Global Enable bit in CR4
    MCA,            // [14] Machine check architecture
    CMOV,           // [15] Conditional move and FCMOV instructions
    PAT,            // [16] Page Attribute Table
  (*FCMOV,*)        // [16] ??? (http://sandpile.org/x86/cpuid.htm)
    PSE36,          // [17] 36-bit page size extension
  {
    AMD K7 processors prior to CPUID=0662h may report 0 even if they are MP-capable.
  }
    MP,             // [19] Multiprocessor Capable
    NX,             // [20] Execute Disable Bit available
    MMXExt,         // [22] Extended MMX (AMD specific, MMX-SSE and SSE-MEM)
    MMX,            // [23]	MMX Instructions
    FXSR,           // [24] FXSAVE, FXRSTOR instructions, CR4 bit 9
  (*MMXExt,*)       // [24] Extended MMX (Cyrix specific)
    FFXSR,          // [25] FXSAVE/FXRSTOR optimizations
    PG1G,           // [26] 1-GByte pages are available
    TSCP,           // [27] RDTSCP and IA32_TSC_AUX are available
    LM,             // [29] AMD64/EM64T, Long Mode
    _3DNowExt,      // [30] Extended 3DNow!
    _3DNow,         // [31] 3DNow!
  {leaf $80000007, EDX register}
    ITSC:           // [08] Invariant TSC
      Boolean;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  TCPUIDInfo_SupportedExtensions = record
    X87,          // x87 FPU                                            features.FPU
    EmulatedX87,  // x87 is emulated                                    CR0[EM:2]=1
    MMX,          // MMX Technology                                     features.MMX and CR0[EM:2]=0
    SSE,          // Streaming SIMD Extensions                          features.FXSR and features.SSE and (system support for SSE)
    SSE2,         // Streaming SIMD Extensions 2                        features.SSE2 and SSE
    SSE3,         // Streaming SIMD Extensions 3                        features.SSE3 and SSE2
    SSSE3,        // Supplemental Streaming SIMD Extensions 3           features.SSSE3 and SSE3
    SSE4_1,       // Streaming SIMD Extensions 4.1                      features.SSE4_1 and SSSE3
    SSE4_2,       // Streaming SIMD Extensions 4.2                      features.SSE4_2 and SSE4_1
    CRC32,        // CRC32 Instruction                                  features.SSE4_2
    POPCNT,       // POPCNT Instruction                                 features.POPCNT and features.SSE4_2
    AES,          // AES New Instructions                               features.AES and SSE2
    PCLMULQDQ,    // PCLMULQDQ Instruction                              features.PCLMULQDQ and SSE2
    AVX,          // Advanced Vector Extensions                         features.OSXSAVE -> XCR0[1..2]=11b and features.AVX
    F16C,         // 16bit Float Conversion Instructions                features.F16C and AVX
    FMA,          // Fused-Multiply-Add Instructions                    features.FMA and AVX
    AVX2,         // Advanced Vector Extensions 2                       features.AVX2 and AVX
    AVX512F,      // AVX-512 Foundation Instructions                    features.OSXSAVE -> XCR0[1..2]=11b and XCR0[5..7]=111b and features.AVX512F
    AVX512ER,     // AVX-512 Exponential and Reciprocal Instructions    features.AVX512ER and AVX512F
    AVX512PF,     // AVX-512 Prefetch Instructions                      features.AVX512PF and AVX512F
  {
    WARNING - If instructions from CD, DQ or BW group are to operate on 256bit
              or 128bit vectors (not only on 512bit vector), it is necessary to
              also check AVX512VL flag (vector length extension).
  }
    AVX512CD,     // AVX-512 Conflict Detection Instructions            features.AVX512CD and AVX512F
    AVX512DQ,     // AVX-512 Doubleword and Quadword Instructions       features.AVX512DQ and AVX512F
    AVX512BW:     // AVX-512 Byte and Word Instructions                 features.AVX512BW and AVX512F
      Boolean;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  TCPUIDInfo = record
    // leaf 0x00000000
    ManufacturerID:             TCPUIDManufacturerID;
    ManufacturerIDString:       String;
    // leaf 0x00000001
    ProcessorType:              Byte;
    ProcessorFamily:            Byte;
    ProcessorModel:             Byte;
    ProcessorStepping:          Byte;
    AdditionalInfo:             TCPUIDInfo_AdditionalInfo;
    // leaf 0x00000001 and 0x00000007
    ProcessorFeatures:          TCPUIDInfo_Features;
    // leaf 0x80000001
    ExtendedProcessorFeatures:  TCPUIDInfo_ExtendedFeatures;
    // leaf 0x80000002 - 0x80000004
    BrandString:                String;
    // some processor extensions whose full support cannot (or should not)
    // be determined directly from processor features
    SupportedExtensions:        TCPUIDInfo_SupportedExtensions;
  end;

{===============================================================================
    TSimpleCPUID - class declaration
===============================================================================}
type
  TSimpleCPUID = class(TObject)
  protected
    fIncludeEmptyLeafs: Boolean;
    fSupported:         Boolean;
    fLoaded:            Boolean;
    fLeafs:             TCPUIDLeafs;
    fInfo:              TCPUIDInfo;
    fHighestStdLeaf:    TCPUIDResult;
    Function GetLeafCount: Integer; virtual;
    Function GetLeaf(Index: Integer): TCPUIDLeaf; virtual;
    class Function EqualLeafs(A,B: TCPUIDResult): Boolean; virtual;
    Function EqualsToHighestStdLeaf(Leaf: TCPUIDResult): Boolean; virtual;
    procedure DeleteLeaf(Index: Integer); virtual;
    procedure InitLeafs(Mask: UInt32); virtual;
    procedure InitStdLeafs; virtual;                // standard leafs
    procedure ProcessLeaf_0000_0000; virtual;
    procedure ProcessLeaf_0000_0001; virtual;
    procedure ProcessLeaf_0000_0002; virtual;
    procedure ProcessLeaf_0000_0004; virtual;
    procedure ProcessLeaf_0000_0007; virtual;
    procedure ProcessLeaf_0000_000B; virtual;
    procedure ProcessLeaf_0000_000D; virtual;
    procedure ProcessLeaf_0000_000F; virtual;
    procedure ProcessLeaf_0000_0010; virtual;
    procedure ProcessLeaf_0000_0012; virtual;
    procedure ProcessLeaf_0000_0014; virtual;
    procedure ProcessLeaf_0000_0017; virtual;
    procedure InitPhiLeafs; virtual;                // Intel Xeon Phi leafs
    procedure InitHypLeafs; virtual;                // hypervisor leafs
    procedure InitExtLeafs; virtual;                // extended leafs
    procedure ProcessLeaf_8000_0001; virtual;
    procedure ProcessLeaf_8000_0002_to_8000_0004; virtual;
    procedure ProcessLeaf_8000_0007; virtual;
    procedure ProcessLeaf_8000_001D; virtual;
    procedure InitTNMLeafs; virtual;                // Transmeta leafs
    procedure InitCNTLeafs; virtual;                // Centaur leafs
    procedure InitSupportedExtensions; virtual;
    procedure ClearInfo; virtual;
    procedure Initialize(IncludeEmptyLeafs: Boolean); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(LoadInfo: Boolean = True; IncludeEmptyLeafs: Boolean = True);
    destructor Destroy; override;
    procedure LoadInfo; virtual;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    Function IndexOf(LeafID: UInt32): Integer; virtual;
    Function Find(LeafID: UInt32; out Index: Integer): Boolean; virtual;
    property IncludeEmptyLeafs: Boolean read fIncludeEmptyLeafs write fIncludeEmptyLeafs;
    property Supported: Boolean read fSupported;
    property Loaded: Boolean read fLoaded;
    property Info: TCPUIDInfo read fInfo;
    property Leafs[Index: Integer]: TCPUIDLeaf read GetLeaf; default;
    property Count: Integer read GetLeafCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSimpleCPUIDEx
--------------------------------------------------------------------------------
===============================================================================}
type
  TCPUSet = {$IFNDEF Windows}array[0..Pred(128 div SizeOf(PtrUInt))] of{$ENDIF} PtrUInt;
  PCPUSet = ^TCPUSet;

{===============================================================================
    TSimpleCPUIDEx - class declaration                                               
===============================================================================}
type
  TSimpleCPUIDEx = class(TSimpleCPUID)
  protected
    fProcessorID: Integer;
    class procedure SetThreadAffinity(var ProcessorMask: TCPUSet); virtual;
  public
    class Function ProcessorAvailable(ProcessorID: Integer): Boolean; virtual;
    constructor Create(ProcessorID: Integer = 0; LoadInfo: Boolean = True; IncludeEmptyLeafs: Boolean = True);
    procedure LoadInfo; override;
    property ProcessorID: Integer read fProcessorID write fProcessorID;
  end;

implementation

uses
{$IFDEF Windows}
  Windows
{$ELSE}
  baseunix
{$ENDIF}
{$IF not Defined(FPC) and (CompilerVersion >= 20)}  // Delphi 2009+
  , AnsiStrings
{$IFEND};

{$IFNDEF Windows}
  {$LINKLIB C}
{$ENDIF}

{===============================================================================
    Auxiliary routines and declarations
===============================================================================}

{$IFDEF Windows}

Function GetProcessAffinityMask(hProcess: THandle; lpProcessAffinityMask,lpSystemAffinityMask: PPtrUInt): BOOL; stdcall; external kernel32;

//------------------------------------------------------------------------------

{$ELSE}
Function getpid: pid_t; cdecl; external;

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function sched_getaffinity(pid: pid_t; cpusetsize: size_t; mask: PCPUSet): cint; cdecl; external;
Function sched_setaffinity(pid: pid_t; cpusetsize: size_t; mask: PCPUSet): cint; cdecl; external;

//------------------------------------------------------------------------------

threadvar
  ThrErrorCode: cInt;

Function CheckErr(ReturnedValue: cInt): Boolean;
begin
Result := ReturnedValue = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := errno_ptr^;
end;

//------------------------------------------------------------------------------

Function GetLastError: cInt;
begin
Result := ThrErrorCode;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IF not(Defined(Windows) and Defined(x86))}
Function GetBit(Value: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$IFEND}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function GetBit(const Value: TCPUSet; Bit: Integer): Boolean; overload;{$IF Defined(Windows) and Defined(CanInline)} inline; {$IFEND}
begin
{$IFDEF Windows}
Result := ((Value shr Bit) and 1) <> 0;
{$ELSE}
{$IFDEF x64}
Result := Value[Bit shr 6] and PtrUInt(PtrUInt(1) shl (Bit and 63)) <> 0;
{$ELSE}
Result := Value[Bit shr 5] and PtrUInt(PtrUInt(1) shl (Bit and 31)) <> 0;
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SetBit(var Value: TCPUSet; Bit: Integer);{$IF Defined(Windows) and Defined(CanInline)} inline; {$IFEND}
begin
{$IFDEF Windows}
Value := Value or PtrUInt(PtrUInt(1) shl Bit);
{$ELSE}
{$IFDEF x64}
Value[Bit shr 6] := Value[Bit shr 6] or PtrUInt(PtrUInt(1) shl (Bit and 63));
{$ELSE}
Value[Bit shr 5] := Value[Bit shr 5] or PtrUInt(PtrUInt(1) shl (Bit and 31));
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetBits(Value: UInt32; FromBit, ToBit: Integer): UInt32;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := (Value and ($FFFFFFFF shr (31 - ToBit))) shr FromBit;
end;

//------------------------------------------------------------------------------

Function GetMSW: UInt16 assembler; register;
asm
{
  Replacement for GetCR0 (now removed), which cannot be used in user mode.
  It returns only lower 16 bits of CR0 (a Machine Status Word), but that should
  suffice.
}
  SMSW    AX
end;

//------------------------------------------------------------------------------

Function GetXCR0L: UInt32; assembler; register;
asm
  XOR     ECX,  ECX

  // note - support for XGETBV (OSXSAVE) IS checked before calling this routine
  DB  $0F, $01, $D0 // XGETBV (XCR0.Low -> EAX (result), XCR0.Hi -> EDX)
end;

//------------------------------------------------------------------------------

procedure TestSSE; register; assembler;
asm
  XORPS   XMM0, XMM0
end;

//------------------------------------------------------------------------------

Function SystemSupportsSSE: Boolean;
begin
try
  TestSSE;
  Result := True;
except
  Result := False;
end;
end;

{===============================================================================
    Main CPUID routines (ASM)
===============================================================================}

Function CPUIDSupported: LongBool;
const
  EFLAGS_BitMask_ID = UInt32($00200000);
asm
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Result is returned in EAX register (all modes).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
{$IFDEF x64}
  // save initial RFLAGS register value
  PUSHFQ

  // save RFLAGS register value again for further use
  PUSHFQ
  // invert ID bit in RFLAGS value stored on stack (bit #21)
  XOR   qword ptr [RSP],  EFLAGS_BitMask_ID
  // load RFLAGS register from stack (with inverted ID bit)
  POPFQ

  // save RFLAGS register to stack (if ID bit can be changed, it is saved
  // as inverted, otherwise it has its initial value)
  PUSHFQ
  // load saved RFLAGS value to EAX
  POP   RAX
  // get whichever bit has changed in comparison with initial RFLAGS value
  XOR   RAX, qword ptr [RSP]
  // check if ID bit has changed (if not => CPUID instruction not supported)
  AND   RAX, EFLAGS_BitMask_ID

  // restore initial RFLAGS value
  POPFQ
{$ELSE}
  // save initial EFLAGS register value
  PUSHFD

  // save EFLAGS register value again for further use
  PUSHFD
  // invert ID bit in EFLAGS value stored on stack (bit #21)
  XOR   dword ptr [ESP],  EFLAGS_BitMask_ID
  // load EFLAGS register from stack (with inverted ID bit)
  POPFD

  // save EFLAGS register to stack (if ID bit can be changed, it is saved
  // as inverted, otherwise it has its initial value)
  PUSHFD
  // load saved EFLAGS value to EAX
  POP   EAX
  // get whichever bit has changed in comparison with initial EFLAGS value
  XOR   EAX, dword ptr [ESP]
  // check if ID bit has changed (if not => CPUID instruction not supported)
  AND   EAX, EFLAGS_BitMask_ID

  // restore initial EFLAGS value
  POPFD
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure CPUID(Leaf, SubLeaf: UInt32; Result: Pointer);
asm
{$IFDEF x64}
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Register content on enter:

  Win64  Lin64

   ECX    EDI   Leaf of the CPUID info (parameter for CPUID instruction)
   EDX    ESI   SubLeaf of the CPUID info (valid only for some leafs)
    R8    RDX   Address of memory space (at least 16 bytes long) to which
                resulting data (registers EAX, EBX, ECX and EDX, in that order)
                will be copied

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

  // save non-volatile registers
  PUSH  RBX

{$IFDEF Windows}
  // Code for Windows 64bit

  // move leaf and subleaf id to a proper register
  MOV   EAX,  ECX
  MOV   ECX,  EDX

{$ELSE}
  // Code for Linux 64bit

  // copy address of memory storage, so it is available for further use
  MOV   R8,   RDX

  // move leaf and subleaf id to a proper register
  MOV   EAX,  EDI
  MOV   ECX,  ESI

{$ENDIF}

  // get the info
  CPUID

  // copy resulting registers to a provided memory
  MOV   [R8],       EAX
  MOV   [R8 + 4],   EBX
  MOV   [R8 + 8],   ECX
  MOV   [R8 + 12],  EDX

  // restore non-volatile registers
  POP   RBX
{$ELSE x64}
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Win32, Lin32

  Register content on enter:

    EAX - Leaf of the CPUID info (parameter for CPUID instruction)
    EDX - SubLeaf of the CPUID info (valid only for some leafs)
    ECX - Address of memory space (at least 16 bytes long) to which resulting
          data (registers EAX, EBX, ECX and EDX, in that order) will be copied

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

  // save non-volatile registers
  PUSH  EDI
  PUSH  EBX

  // copy address of memory storage, so it is available for further use
  MOV   EDI,  ECX

  // move subleaf number to ECX register where it is expected
  MOV   ECX,  EDX

  // get the info (EAX register already contains the leaf number)
  CPUID

  // copy resulting registers to a provided memory
  MOV   [EDI],      EAX
  MOV   [EDI + 4],  EBX
  MOV   [EDI + 8],  ECX
  MOV   [EDI + 12], EDX

  // restore non-volatile registers
  POP   EBX
  POP   EDI
{$ENDIF x64}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure CPUID(Leaf: UInt32; Result: Pointer);
begin
CPUID(Leaf,0,Result);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleCPUID
--------------------------------------------------------------------------------
===============================================================================}
type
  TManufacturersItem = record
    IDStr:  String;
    ID:     TCPUIDManufacturerID;
  end;

const
  Manufacturers: array[0..13] of TManufacturersItem = (
    (IDStr: 'AMDisbetter!'; ID: mnAMD),
    (IDStr: 'AuthenticAMD'; ID: mnAMD),
    (IDStr: 'CentaurHauls'; ID: mnCentaur),
    (IDStr: 'CyrixInstead'; ID: mnCyrix),
    (IDStr: 'GenuineIntel'; ID: mnIntel),
    (IDStr: 'TransmetaCPU'; ID: mnTransmeta),
    (IDStr: 'GenuineTMx86'; ID: mnTransmeta),
    (IDStr: 'Geode by NSC'; ID: mnNationalSemiconductor),
    (IDStr: 'NexGenDriven'; ID: mnNexGen),
    (IDStr: 'RiseRiseRise'; ID: mnRise),
    (IDStr: 'SiS SiS SiS '; ID: mnSiS),
    (IDStr: 'UMC UMC UMC '; ID: mnUMC),
    (IDStr: 'VIA VIA VIA '; ID: mnVIA),
    (IDStr: 'Vortex86 SoC'; ID: mnVortex));

{===============================================================================
    TSimpleCPUID - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleCPUID - protected methods
-------------------------------------------------------------------------------}

Function TSimpleCPUID.GetLeafCount: Integer;
begin
Result := Length(fLeafs);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.GetLeaf(Index: Integer): TCPUIDLeaf;
begin
If CheckIndex(Index) then
  Result := fLeafs[Index]
else
  raise ESCIDIndexOutOfBounds.CreateFmt('TSimpleCPUID.GetLeaf: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

class Function TSimpleCPUID.EqualLeafs(A,B: TCPUIDResult): Boolean;
begin
Result := (A.EAX = B.EAX) and (A.EBX = B.EBX) and (A.ECX = B.ECX) and (A.EDX = B.EDX);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.EqualsToHighestStdLeaf(Leaf: TCPUIDResult): Boolean;
begin
Result := EqualLeafs(Leaf,fHighestStdLeaf);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.DeleteLeaf(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    For i := Index to Pred(HighIndex) do
      fLeafs[i] := fLeafs[i + 1];
    SetLength(fLeafs,Length(fLeafs) - 1);
  end
else raise ESCIDIndexOutOfBounds.CreateFmt('TSimpleCPUID.DeleteLeaf: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitLeafs(Mask: UInt32);
var
  Temp: TCPUIDResult;
  Cnt:  Integer;
  i:    Integer;
begin
// get leaf count
CPUID(Mask,Addr(Temp));
If ((Temp.EAX and $FFFF0000) = Mask) and not EqualsToHighestStdLeaf(Temp) then
  begin
    Cnt := Length(fLeafs);
    SetLength(fLeafs,Length(fLeafs) + Integer(Temp.EAX and not Mask) + 1);
    // load all leafs
    For i := Cnt to HighIndex do
      begin
        fLeafs[i].ID := UInt32(i - Cnt) or Mask;
        CPUID(fLeafs[i].ID,Addr(fLeafs[i].Data));
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitStdLeafs;
begin
InitLeafs($00000000);
If Length(fLeafs) > 0 then
  fHighestStdLeaf := fLeafs[HighIndex].Data
else
  FillChar(fHighestStdLeaf,SizeOf(fHighestStdLeaf),0);
// process individual leafs
ProcessLeaf_0000_0000;
ProcessLeaf_0000_0001;
ProcessLeaf_0000_0002;
ProcessLeaf_0000_0004;
ProcessLeaf_0000_0007;
ProcessLeaf_0000_000B;
ProcessLeaf_0000_000D;
ProcessLeaf_0000_000F;
ProcessLeaf_0000_0010;
ProcessLeaf_0000_0012;
ProcessLeaf_0000_0014;
ProcessLeaf_0000_0017;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0000;
var
  Index:      Integer;
  Str:        AnsiString;
  StrOverlay: PByteArray;
  i:          Integer;
begin
If Find($00000000,Index) then
  begin
    Str := '';
    SetLength(Str,12);
    StrOverlay := Pointer(PAnsiChar(Str));
    Move(fLeafs[Index].Data.EBX,StrOverlay^[0],4);
    Move(fLeafs[Index].Data.EDX,StrOverlay^[4],4);
    Move(fLeafs[Index].Data.ECX,StrOverlay^[8],4);
    fInfo.ManufacturerIDString := String(Str);
    fInfo.ManufacturerID := mnOthers;
    For i := Low(Manufacturers) to High(Manufacturers) do
      If AnsiSameStr(Manufacturers[i].IDStr,fInfo.ManufacturerIDString) then
        fInfo.ManufacturerID := Manufacturers[i].ID;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0001;
var
  Index:  Integer;
begin
If Find($00000001,Index) then
  begin
    // processor type
    fInfo.ProcessorType := GetBits(fLeafs[Index].Data.EAX,12,13);
    // processor family
    If GetBits(fLeafs[Index].Data.EAX,8,11) <> $F then
      fInfo.ProcessorFamily := GetBits(fLeafs[Index].Data.EAX,8,11)
    else
      fInfo.ProcessorFamily := GetBits(fLeafs[Index].Data.EAX,8,11) +
                                    GetBits(fLeafs[Index].Data.EAX,20,27);
    // processor model
    if GetBits(fLeafs[Index].Data.EAX,8,11) in [$6,$F] then
      fInfo.ProcessorModel := (GetBits(fLeafs[Index].Data.EAX,16,19) shl 4) +
                                    GetBits(fLeafs[Index].Data.EAX,4,7)
    else
      fInfo.ProcessorModel := GetBits(fLeafs[Index].Data.EAX,4,7);
    // processor stepping
    fInfo.ProcessorStepping := GetBits(fLeafs[Index].Data.EAX,0,3);
    // additional info
    fInfo.AdditionalInfo.BrandID               := GetBits(fLeafs[Index].Data.EBX,0,7);
    fInfo.AdditionalInfo.CacheLineFlushSize    := GetBits(fLeafs[Index].Data.EBX,8,15) * 8;
    fInfo.AdditionalInfo.LogicalProcessorCount := GetBits(fLeafs[Index].Data.EBX,16,23);
    fInfo.AdditionalInfo.LocalAPICID           := GetBits(fLeafs[Index].Data.EBX,24,31);
    // processor features
    with fInfo.ProcessorFeatures do
      begin
      {ECX register}
        SSE3         := GetBit(fLeafs[Index].Data.ECX,0);
        PCLMULQDQ    := GetBit(fLeafs[Index].Data.ECX,1);
        DTES64       := GetBit(fLeafs[Index].Data.ECX,2);
        MONITOR      := GetBit(fLeafs[Index].Data.ECX,3);
        DS_CPL       := GetBit(fLeafs[Index].Data.ECX,4);
        VMX          := GetBit(fLeafs[Index].Data.ECX,5);
        SMX          := GetBit(fLeafs[Index].Data.ECX,6);
        EIST         := GetBit(fLeafs[Index].Data.ECX,7);
        TM2          := GetBit(fLeafs[Index].Data.ECX,8);
        SSSE3        := GetBit(fLeafs[Index].Data.ECX,9);
        CNXT_ID      := GetBit(fLeafs[Index].Data.ECX,10);
        SDBG         := GetBit(fLeafs[Index].Data.ECX,11);
        FMA          := GetBit(fLeafs[Index].Data.ECX,12);
        CMPXCHG16B   := GetBit(fLeafs[Index].Data.ECX,13);
        xTPR         := GetBit(fLeafs[Index].Data.ECX,14);
        PDCM         := GetBit(fLeafs[Index].Data.ECX,15);
        PCID         := GetBit(fLeafs[Index].Data.ECX,17);
        DCA          := GetBit(fLeafs[Index].Data.ECX,18);
        SSE4_1       := GetBit(fLeafs[Index].Data.ECX,19);
        SSE4_2       := GetBit(fLeafs[Index].Data.ECX,20);
        x2APIC       := GetBit(fLeafs[Index].Data.ECX,21);
        MOVBE        := GetBit(fLeafs[Index].Data.ECX,22);
        POPCNT       := GetBit(fLeafs[Index].Data.ECX,23);
        TSC_Deadline := GetBit(fLeafs[Index].Data.ECX,24);
        AES          := GetBit(fLeafs[Index].Data.ECX,25);
        XSAVE        := GetBit(fLeafs[Index].Data.ECX,26);
        OSXSAVE      := GetBit(fLeafs[Index].Data.ECX,27);
        AVX          := GetBit(fLeafs[Index].Data.ECX,28);
        F16C         := GetBit(fLeafs[Index].Data.ECX,29);
        RDRAND       := GetBit(fLeafs[Index].Data.ECX,30);
        HYPERVISOR   := GetBit(fLeafs[Index].Data.ECX,31);
      {EDX register}
        FPU          := GetBit(fLeafs[Index].Data.EDX,0);
        VME          := GetBit(fLeafs[Index].Data.EDX,1);
        DE           := GetBit(fLeafs[Index].Data.EDX,2);
        PSE          := GetBit(fLeafs[Index].Data.EDX,3);
        TSC          := GetBit(fLeafs[Index].Data.EDX,4);
        MSR          := GetBit(fLeafs[Index].Data.EDX,5);
        PAE          := GetBit(fLeafs[Index].Data.EDX,6);
        MCE          := GetBit(fLeafs[Index].Data.EDX,7);
        CX8          := GetBit(fLeafs[Index].Data.EDX,8);
        APIC         := GetBit(fLeafs[Index].Data.EDX,9);
        SEP          := GetBit(fLeafs[Index].Data.EDX,11);
        MTRR         := GetBit(fLeafs[Index].Data.EDX,12);
        PGE          := GetBit(fLeafs[Index].Data.EDX,13);
        MCA          := GetBit(fLeafs[Index].Data.EDX,14);
        CMOV         := GetBit(fLeafs[Index].Data.EDX,15);
        PAT          := GetBit(fLeafs[Index].Data.EDX,16);
        PSE_36       := GetBit(fLeafs[Index].Data.EDX,17);
        PSN          := GetBit(fLeafs[Index].Data.EDX,18);
        CLFSH        := GetBit(fLeafs[Index].Data.EDX,19);
        DS           := GetBit(fLeafs[Index].Data.EDX,21);
        ACPI         := GetBit(fLeafs[Index].Data.EDX,22);
        MMX          := GetBit(fLeafs[Index].Data.EDX,23);
        FXSR         := GetBit(fLeafs[Index].Data.EDX,24);
        SSE          := GetBit(fLeafs[Index].Data.EDX,25);
        SSE2         := GetBit(fLeafs[Index].Data.EDX,26);
        SS           := GetBit(fLeafs[Index].Data.EDX,27);
        HTT          := GetBit(fLeafs[Index].Data.EDX,28);
        TM           := GetBit(fLeafs[Index].Data.EDX,29);
        IA64         := GetBit(fLeafs[Index].Data.EDX,30);
        PBE          := GetBit(fLeafs[Index].Data.EDX,31);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0002;
var
  Index:  Integer;
  i:      Integer;
begin
{
  this whole function must be run on the same processor (core), otherwise
  results will be wrong

  Intel documentation explicitly states that EAX[0..7] for this leaf is always
  1, and this value should be ignored.
  But according to sandpile, this leaf might be run several times with differing
  results and this number gives the repeat count.
}
If Find($00000002,Index) then
  If Byte(fLeafs[Index].Data.EAX) > 0 then
    begin
      SetLength(fLeafs[Index].SubLeafs,Byte(fLeafs[Index].Data.EAX));
      fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
      For i := 1 to High(fLeafs[Index].SubLeafs) do
        CPUID(2,UInt32(i),Addr(fLeafs[Index].SubLeafs[i]));
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0004;
var
  Index:  Integer;
  Temp:   TCPUIDResult;
begin
If Find($00000004,Index) then
  begin
    Temp := fLeafs[Index].Data;
    while ((Temp.EAX and $1F) <> 0) and (Length(fLeafs[Index].SubLeafs) <= 128) do
      begin
        SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
        fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
        CPUID(4,UInt32(Length(fLeafs[Index].SubLeafs)),@Temp);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0007;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000007,Index) then
  begin
    // get all subleafs
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID(7,UInt32(i),Addr(fLeafs[Index].SubLeafs[i]));
    // processor features
    with fInfo.ProcessorFeatures do
      begin
      {EBX register}
        FSGSBASE     := GetBit(fLeafs[Index].Data.EBX,0);
        TSC_ADJUST   := GetBit(fLeafs[Index].Data.EBX,1);
        SGX          := GetBit(fLeafs[Index].Data.EBX,2);
        BMI1         := GetBit(fLeafs[Index].Data.EBX,3);
        HLE          := GetBit(fLeafs[Index].Data.EBX,4);
        AVX2         := GetBit(fLeafs[Index].Data.EBX,5);
        FPDP         := GetBit(fLeafs[Index].Data.EBX,6);
        SMEP         := GetBit(fLeafs[Index].Data.EBX,7);
        BMI2         := GetBit(fLeafs[Index].Data.EBX,8);
        ERMS         := GetBit(fLeafs[Index].Data.EBX,9);
        INVPCID      := GetBit(fLeafs[Index].Data.EBX,10);
        RTM          := GetBit(fLeafs[Index].Data.EBX,11);
        PQM          := GetBit(fLeafs[Index].Data.EBX,12);
        FPCSDS       := GetBit(fLeafs[Index].Data.EBX,13);
        MPX          := GetBit(fLeafs[Index].Data.EBX,14);
        PQE          := GetBit(fLeafs[Index].Data.EBX,15);
        AVX512F      := GetBit(fLeafs[Index].Data.EBX,16);
        AVX512DQ     := GetBit(fLeafs[Index].Data.EBX,17);
        RDSEED       := GetBit(fLeafs[Index].Data.EBX,18);
        ADX          := GetBit(fLeafs[Index].Data.EBX,19);
        SMAP         := GetBit(fLeafs[Index].Data.EBX,20);
        AVX512IFMA   := GetBit(fLeafs[Index].Data.EBX,21);
        PCOMMIT      := GetBit(fLeafs[Index].Data.EBX,22);
        CLFLUSHOPT   := GetBit(fLeafs[Index].Data.EBX,23);
        CLWB         := GetBit(fLeafs[Index].Data.EBX,24);
        PT           := GetBit(fLeafs[Index].Data.EBX,25);
        AVX512PF     := GetBit(fLeafs[Index].Data.EBX,26);
        AVX512ER     := GetBit(fLeafs[Index].Data.EBX,27);
        AVX512CD     := GetBit(fLeafs[Index].Data.EBX,28);
        SHA          := GetBit(fLeafs[Index].Data.EBX,29);
        AVX512BW     := GetBit(fLeafs[Index].Data.EBX,30);
        AVX512VL     := GetBit(fLeafs[Index].Data.EBX,31);
      {ECX register}
        PREFETCHWT1  := GetBit(fLeafs[Index].Data.ECX,0);
        AVX512VBMI   := GetBit(fLeafs[Index].Data.ECX,1);
        UMIP         := GetBit(fLeafs[Index].Data.ECX,2);
        PKU          := GetBit(fLeafs[Index].Data.ECX,3);
        OSPKE        := GetBit(fLeafs[Index].Data.ECX,4);
        CET          := GetBit(fLeafs[Index].Data.ECX,7);
        VA57         := GetBit(fLeafs[Index].Data.ECX,16);
        MAWAU        := Byte(GetBits(fLeafs[Index].Data.ECX,17,21));
        RDPID        := GetBit(fLeafs[Index].Data.ECX,22);
        SGX_LC       := GetBit(fLeafs[Index].Data.ECX,30);
      {EDX register}
        AVX512QVNNIW := GetBit(fLeafs[Index].Data.EDX,2);
        AVX512QFMA   := GetBit(fLeafs[Index].Data.EDX,3);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_000B;
var
  Index:  Integer;
  Temp:   TCPUIDResult;
begin
If Find($0000000B,Index) then
  begin
    Temp := fLeafs[Index].Data;
    while GetBits(Temp.ECX,8,15) <> 0 do
      begin
        SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
        fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
        CPUID($B,UInt32(Length(fLeafs[Index].SubLeafs)),@Temp);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_000D;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($0000000D,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,2);
    fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
    CPUID($D,1,Addr(fLeafs[Index].SubLeafs[1]));
    For i := 2 to 31 do
      If GetBit(fLeafs[Index].SubLeafs[0].EAX,i) and GetBit(fLeafs[Index].SubLeafs[1].ECX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($D,UInt32(i),Addr(fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]));
        end;
    For i := 0 to 31 do
      If GetBit(fLeafs[Index].SubLeafs[0].EDX,i) and GetBit(fLeafs[Index].SubLeafs[1].EDX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($D,UInt32(32 + i),Addr(fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]));
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_000F;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($0000000F,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,1);
    fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
    For i := 1 to 31 do
      If GetBit(fLeafs[Index].Data.EDX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($F,UInt32(i),Addr(fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]));
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0010;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000010,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,1);
    fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
    For i := 1 to 31 do
      If GetBit(fLeafs[Index].Data.EBX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($10,UInt32(i),Addr(fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]));
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0012;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000012,Index) then
  begin
    If fInfo.ProcessorFeatures.SGX then
      begin
        SetLength(fLeafs[Index].SubLeafs,1);
        fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
        For i := 0 to 31 do
          If GetBit(fLeafs[Index].Data.EAX,i) then
            begin
              SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
              CPUID($12,UInt32(i + 1),Addr(fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]));
            end;
      end
    else DeleteLeaf(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0014;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000014,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID($14,UInt32(i),Addr(fLeafs[Index].SubLeafs[i]));
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0017;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000017,Index) then
  begin
    If fLeafs[Index].Data.EAX >= 3 then
      begin
        SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
        For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
          CPUID($17,UInt32(i),Addr(fLeafs[Index].SubLeafs[i]));
      end
    else DeleteLeaf(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitPhiLeafs;
begin
InitLeafs($20000000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitHypLeafs;

  procedure AddHypLeaf(ID: UInt32);
  var
    Temp: TCPUIDResult;
  begin
    CPUID(ID,Addr(Temp));
    If not EqualsToHighestStdLeaf(Temp) then
      begin
        SetLength(fLeafs,Length(fLeafs) + 1);
        fLeafs[HighIndex].ID := ID;
        fLeafs[HighIndex].Data := Temp;
      end;
  end;

begin
If fInfo.ProcessorFeatures.HYPERVISOR then
  begin
    AddHypLeaf($40000000);
    AddHypLeaf($40000001);
    AddHypLeaf($40000002);
    AddHypLeaf($40000003);
    AddHypLeaf($40000004);
    AddHypLeaf($40000005);
    AddHypLeaf($40000006);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitExtLeafs;
begin
InitLeafs($80000000);
// process individual leafs
ProcessLeaf_8000_0001;
ProcessLeaf_8000_0002_to_8000_0004;
ProcessLeaf_8000_0007;
ProcessLeaf_8000_001D;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_0001;
var
  Index:  Integer;
begin
If Find($80000001,Index) then
  begin
    // extended processor features
    with fInfo.ExtendedProcessorFeatures do
      begin
      {ECX register}
        AHF64     := GetBit(fLeafs[Index].Data.ECX,0);
        CMP       := GetBit(fLeafs[Index].Data.ECX,1);
        SVM       := GetBit(fLeafs[Index].Data.ECX,2);
        EAS       := GetBit(fLeafs[Index].Data.ECX,3);
        CR8D      := GetBit(fLeafs[Index].Data.ECX,4);
        LZCNT     := GetBit(fLeafs[Index].Data.ECX,5);
        ABM       := GetBit(fLeafs[Index].Data.ECX,5);
        SSE4A     := GetBit(fLeafs[Index].Data.ECX,6);
        MSSE      := GetBit(fLeafs[Index].Data.ECX,7);
        _3DNowP   := GetBit(fLeafs[Index].Data.ECX,8);
        OSVW      := GetBit(fLeafs[Index].Data.ECX,9);
        IBS       := GetBit(fLeafs[Index].Data.ECX,10);
        XOP       := GetBit(fLeafs[Index].Data.ECX,11);
        SKINIT    := GetBit(fLeafs[Index].Data.ECX,12);
        WDT       := GetBit(fLeafs[Index].Data.ECX,13);
        LWP       := GetBit(fLeafs[Index].Data.ECX,15);
        FMA4      := GetBit(fLeafs[Index].Data.ECX,16);
        TCE       := GetBit(fLeafs[Index].Data.ECX,17);
        NODEID    := GetBit(fLeafs[Index].Data.ECX,19);
        TBM       := GetBit(fLeafs[Index].Data.ECX,21);
        TOPX      := GetBit(fLeafs[Index].Data.ECX,22);
        PCX_CORE  := GetBit(fLeafs[Index].Data.ECX,23);
        PCX_NB    := GetBit(fLeafs[Index].Data.ECX,24);
        DBX       := GetBit(fLeafs[Index].Data.ECX,26);
        PERFTSC   := GetBit(fLeafs[Index].Data.ECX,27);
        PCX_L2I   := GetBit(fLeafs[Index].Data.ECX,28);
        MON       := GetBit(fLeafs[Index].Data.ECX,29);
      {EDX register}
        FPU       := GetBit(fLeafs[Index].Data.EDX,0);
        VME       := GetBit(fLeafs[Index].Data.EDX,1);
        DE        := GetBit(fLeafs[Index].Data.EDX,2);
        PSE       := GetBit(fLeafs[Index].Data.EDX,3);
        TSC       := GetBit(fLeafs[Index].Data.EDX,4);
        MSR       := GetBit(fLeafs[Index].Data.EDX,5);
        PAE       := GetBit(fLeafs[Index].Data.EDX,6);
        MCE       := GetBit(fLeafs[Index].Data.EDX,7);
        CX8       := GetBit(fLeafs[Index].Data.EDX,8);
        APIC      := GetBit(fLeafs[Index].Data.EDX,9);
        SEP       := GetBit(fLeafs[Index].Data.EDX,11);
        MTRR      := GetBit(fLeafs[Index].Data.EDX,12);
        PGE       := GetBit(fLeafs[Index].Data.EDX,13);
        MCA       := GetBit(fLeafs[Index].Data.EDX,14);
        CMOV      := GetBit(fLeafs[Index].Data.EDX,15);
        PAT       := GetBit(fLeafs[Index].Data.EDX,16);
        PSE36     := GetBit(fLeafs[Index].Data.EDX,17);
        MP        := GetBit(fLeafs[Index].Data.EDX,19);
        NX        := GetBit(fLeafs[Index].Data.EDX,20);
        MMXExt    := GetBit(fLeafs[Index].Data.EDX,22);
        MMX       := GetBit(fLeafs[Index].Data.EDX,23);
        FXSR      := GetBit(fLeafs[Index].Data.EDX,24);
        FFXSR     := GetBit(fLeafs[Index].Data.EDX,25);
        PG1G      := GetBit(fLeafs[Index].Data.EDX,26);
        TSCP      := GetBit(fLeafs[Index].Data.EDX,27);
        LM        := GetBit(fLeafs[Index].Data.EDX,29);
        _3DNowExt := GetBit(fLeafs[Index].Data.EDX,30);
        _3DNow    := GetBit(fLeafs[Index].Data.EDX,31);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_0002_to_8000_0004;
var
  Str:        AnsiString;
  StrOverlay: PByteArray;
  i:          Integer;
  Index:      Integer;
begin
Str := '';
// get brand string
SetLength(Str,48);
StrOverlay := Pointer(PAnsiChar(Str));
For i := 0 to 2 do
  If Find($80000002 + UInt32(i),Index) then
    begin
      Move(fLeafs[Index].Data.EAX,StrOverlay^[i * 16],4);
      Move(fLeafs[Index].Data.EBX,StrOverlay^[(i * 16) + 4],4);
      Move(fLeafs[Index].Data.ECX,StrOverlay^[(i * 16) + 8],4);
      Move(fLeafs[Index].Data.EDX,StrOverlay^[(i * 16) + 12],4);
    end
  else
    begin
      fInfo.BrandString := '';
      Exit;
    end;
{$IF not Defined(FPC) and (CompilerVersion >= 20)}
SetLength(Str,AnsiStrings.StrLen(PAnsiChar(Str)));
{$ELSE}
SetLength(Str,StrLen(PAnsiChar(Str)));
{$IFEND}
fInfo.BrandString := Trim(String(Str));
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_0007;
var
  Index:  Integer;
begin
If Find($80000007,Index) then
  fInfo.ExtendedProcessorFeatures.ITSC := GetBit(fLeafs[Index].Data.EDX,8);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_001D;
var
  Index:  Integer;
  Temp:   TCPUIDResult;
begin
If Find($8000001D,Index) and fInfo.ExtendedProcessorFeatures.TOPX then
  begin
    Temp := fLeafs[Index].Data;
    while GetBits(Temp.EAX,0,4) <> 0 do
      begin
        SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
        fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
        CPUID($8000001D,UInt32(Length(fLeafs[Index].SubLeafs)),@Temp);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitTNMLeafs;
begin
InitLeafs($80860000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitCNTLeafs;
begin
InitLeafs($C0000000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitSupportedExtensions;
begin
with fInfo.SupportedExtensions do
  begin
    // FPU - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    X87         := fInfo.ProcessorFeatures.FPU;
    EmulatedX87 := GetBit(GetMSW,2);
    MMX         := fInfo.ProcessorFeatures.MMX and not EmulatedX87;

    // SSE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  {
    Full check for SSE support (mainly from OS) is complicated and pretty much
    impossible to do from normal code (ie. in privilege level above 0) without
    cooperation with the OS (which I want to avoid).
    FYI these are SOME things that needs to be checked:

      - CR4.OSFXSR[bit 9] must be 1 (any CR can be accessed only from CPL 0)
      - CR4.OSXMMEXCPT[bit 10] must be 1
      - CR0.TS[bit 3] must be 0
      - CR0.EM[bit 2] must be 0
      - CR0.MP[bit 1] must be 1
      - CPUID.1:EDX.FXSR[bit 24] must be 1
      - CPUID.1:EDX.SSE[bit 25] must be 1

    Only the last two points can be checked from CPL > 0. We check those and
    then do a brute force approach - SSE instruction is executed and, if it
    goes without error, it is assumed the system has proper suport for SSE,
    otherwise it is assumed the system does not support SSE. This, of course,
    assumes that there is exception handler assigned for #UD (invalid opcode)
    by the system and that the program can process it. This should be true on
    most sane systems...
  }
    If fInfo.ProcessorFeatures.FXSR then
      SSE := fInfo.ProcessorFeatures.SSE and SystemSupportsSSE
    else
      SSE := False;
    SSE2      := fInfo.ProcessorFeatures.SSE2 and SSE;
    SSE3      := fInfo.ProcessorFeatures.SSE3 and SSE2;
    SSSE3     := fInfo.ProcessorFeatures.SSSE3 and SSE3;
    SSE4_1    := fInfo.ProcessorFeatures.SSE4_1 and SSSE3;
    SSE4_2    := fInfo.ProcessorFeatures.SSE4_2 and SSE4_1;
    CRC32     := fInfo.ProcessorFeatures.SSE4_2;
    POPCNT    := fInfo.ProcessorFeatures.POPCNT and fInfo.ProcessorFeatures.SSE4_2;
    AES       := fInfo.ProcessorFeatures.AES and SSE2;
    PCLMULQDQ := fInfo.ProcessorFeatures.PCLMULQDQ and SSE2;

    // AVX - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    If fInfo.ProcessorFeatures.OSXSAVE then
      AVX := (GetXCR0L and $6 = $6) and fInfo.ProcessorFeatures.AVX
    else
      AVX := False;
    F16C := fInfo.ProcessorFeatures.F16C and AVX;
    FMA  := fInfo.ProcessorFeatures.FMA and AVX;

    // AVX2  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    AVX2 := fInfo.ProcessorFeatures.AVX2 and AVX;
    
    // AVX-512 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    If fInfo.ProcessorFeatures.OSXSAVE then
      AVX512F := (GetXCR0L and $E6 = $E6) and fInfo.ProcessorFeatures.AVX512F
    else
      AVX512F := False;
    AVX512ER := fInfo.ProcessorFeatures.AVX512ER and AVX512F;
    AVX512PF := fInfo.ProcessorFeatures.AVX512PF and AVX512F;
    AVX512CD := fInfo.ProcessorFeatures.AVX512CD and AVX512F;
    AVX512DQ := fInfo.ProcessorFeatures.AVX512DQ and AVX512F;
    AVX512BW := fInfo.ProcessorFeatures.AVX512BW and AVX512F;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ClearInfo;
begin
fLoaded := False;
SetLength(fLeafs,0);
fInfo.ManufacturerIDString := '';
fInfo.BrandString := '';
FillChar(fInfo,SizeOf(TCPUIDInfo),0);
FillChar(fHighestStdLeaf,SizeOf(TCPUIDResult),0);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.Initialize(IncludeEmptyLeafs: Boolean);
begin
fIncludeEmptyLeafs := IncludeEmptyLeafs;
fSupported := SimpleCPUID.CPUIDSupported;
ClearInfo;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.Finalize;
begin
ClearInfo;
end;

{-------------------------------------------------------------------------------
    TSimpleCPUID - public methods
-------------------------------------------------------------------------------}

constructor TSimpleCPUID.Create(LoadInfo: Boolean = True; IncludeEmptyLeafs: Boolean = True);
begin
inherited Create;
Initialize(IncludeEmptyLeafs);
If LoadInfo then
  Self.LoadInfo;
end;

//------------------------------------------------------------------------------

destructor TSimpleCPUID.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadInfo;
var
  i:  Integer;
begin
ClearInfo;
If fSupported then
  begin
    InitStdLeafs;
    InitPhiLeafs;
    InitHypLeafs;
    InitExtLeafs;
    InitTNMLeafs;
    InitCNTLeafs;
    If not fIncludeEmptyLeafs then
      For i := HighIndex downto LowIndex do
        If EqualLeafs(fLeafs[i].Data,NullLeaf) then DeleteLeaf(i);
    InitSupportedExtensions;
    fLoaded := True;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.LowIndex: Integer;
begin
Result := Low(fLeafs);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.HighIndex: Integer;
begin
Result := High(fLeafs);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.IndexOf(LeafID: UInt32): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fLeafs[i].ID = LeafID then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.Find(LeafID: UInt32; out Index: Integer): Boolean;
begin
Index := IndexOf(LeafID);
Result := CheckIndex(Index);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSimpleCPUIDEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleCPUIDEx - class implementation
===============================================================================} 
{-------------------------------------------------------------------------------
    TSimpleCPUIDEx - protected methods
-------------------------------------------------------------------------------}

class procedure TSimpleCPUIDEx.SetThreadAffinity(var ProcessorMask: TCPUSet);
{$IFDEF Windows}
begin
ProcessorMask := SetThreadAffinityMask(GetCurrentThread,ProcessorMask);
If ProcessorMask = 0 then
  raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity:' +
    ' Failed to set thread affinity mask (%d).',[Integer(GetLastError)]);
end;
{$ELSE}
var
  OldProcessorMask: TCPUSet;
begin
// pid zero for the calling thread
If CheckErr(sched_getaffinity(0,SizeOf(TCPUSet),@OldProcessorMask)) then
  begin
    If CheckErr(sched_setaffinity(0,SizeOf(TCPUSet),@ProcessorMask)) then
      ProcessorMask := OldProcessorMask
    else
      raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity:' +
        ' Failed to set thread affinity mask (%d).',[Integer(GetLastError)]);
  end
else raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity:' +
  ' Failed to get thread affinity mask (%d).',[Integer(GetLastError)]);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    TSimpleCPUIDEx - public methods
-------------------------------------------------------------------------------}

class Function TSimpleCPUIDEx.ProcessorAvailable(ProcessorID: Integer): Boolean;
var
  ProcessAffinityMask:  TCPUSet;
{$IFDEF Windows}
  SystemAffinityMask:   TCPUSet;
{$ENDIF}
begin
If (ProcessorID >= 0) and (ProcessorID < (SizeOf(TCPUSet) * 8)) then
  begin
  {$IFDEF Windows}
    If GetProcessAffinityMask(GetCurrentProcess,@ProcessAffinityMask,@SystemAffinityMask) then
  {$ELSE}
    // sched_getaffinity called with process id (getpid) returns mask of main thread (process mask)
    If CheckErr(sched_getaffinity(getpid,SizeOf(TCPUSet),@ProcessAffinityMask)) then
  {$ENDIF}
      Result := GetBit(ProcessAffinityMask,ProcessorID)
    else
      raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.ProcessorAvailable:' +
        ' Failed to get process affinity mask (%d).',[Integer(GetLastError)]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

constructor TSimpleCPUIDEx.Create(ProcessorID: Integer = 0; LoadInfo: Boolean = True; IncludeEmptyLeafs: Boolean = True);
begin
inherited Create(False,IncludeEmptyLeafs);
fProcessorID := ProcessorID;
If LoadInfo then
  Self.LoadInfo;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUIDEx.LoadInfo;
var
  ProcessorMask:  TCPUSet;
begin
If ProcessorAvailable(fProcessorID) then
  begin
    FillChar(Addr(ProcessorMask)^,SizeOf(ProcessorMask),0);
    SetBit(ProcessorMask,fProcessorID);
    SetThreadAffinity(ProcessorMask);
    try
      inherited LoadInfo;
    finally
      // restore the affinity
      SetThreadAffinity(ProcessorMask);
    end;
  end
else raise ESCIDInvalidProcessor.CreateFmt('TSimpleCPUIDEx.Initialize: Processor ID %d not available.',[fProcessorID]);
end;

end.
