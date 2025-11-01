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

      NOTE - some provided information are valid only on processors of specific
             type, model or manufacturer. Consult source code for mode details.

  Version 1.4 (2025-11-01)

  Last change 2025-11-01

  ©2016-2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleCPUID

  Sources:
    - en.wikipedia.org/wiki/CPUID
    - sandpile.org/x86/cpuid.htm
    - Intel® 64 and IA-32 Architectures Software Developer’s Manual (document
      #325462-088US, June 2025)
    - Intel® Advanced Performance Extensions (Intel® APX) Architecture
      Specification (document #355828-007US, revision 7, July 2025)
    - Intel® Architecture Instruction Set Extensions and Future Features
      Programming Reference (document #319433-059, September 2025)
    - AMD64 Architecture Programmer’s Manual Volume 3... (publication #24594,
      revision 3.37, July 2025)

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimpleCPUID_UseAuxExceptions for details).

  Indirect dependencies:
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

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

{
  SimpleCPUID_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimpleCPUID_UseAuxExceptions to achieve this.
}
{$IF Defined(SimpleCPUID_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled without ASM.'}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESCIDException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESCIDSystemError      = class(ESCIDException);
  ESCIDIndexOutOfBounds = class(ESCIDException);
  ESCIDInvalidProcessor = class(ESCIDException);

{===============================================================================
    Main CPUID routines
===============================================================================}
type
  TCPUIDLeafData = packed record
    EAX,EBX,ECX,EDX:  UInt32;
  end;
  PCPUIDLeafData = ^TCPUIDLeafData;

const
  EmptyLeafData: TCPUIDLeafData = (EAX: 0; EBX: 0; ECX: 0; EDX: 0);

//------------------------------------------------------------------------------

Function CPUIDSupported: Boolean; register; assembler;

procedure CPUID(Leaf,SubLeaf: UInt32; Result: Pointer); register; overload; assembler;
procedure CPUID(Leaf: UInt32; Result: Pointer); overload;{$IFDEF CanInline} inline; {$ENDIF}

procedure CPUID(Leaf,SubLeaf: UInt32; out Info: TCPUIDLeafData); overload;{$IFDEF CanInline} inline; {$ENDIF}
procedure CPUID(Leaf: UInt32; out Info: TCPUIDLeafData); overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleCPUID
--------------------------------------------------------------------------------
===============================================================================}
type
  TCPUIDLeaf = record
    // leaf number/index (value in EAX before calling CPUID instruction)
    ID:       UInt32;
    // main leaf data - obtained when CPUID is called with SubLeaf index 0
    Data:     TCPUIDLeafData;
  {
    If there is any subleaf (array is not empty), then the first subleaf
    (index 0) is a mirror of field Data.
  }
    SubLeafs: array of TCPUIDLeafData;
  end;
  PCPUIDLeaf = ^TCPUIDLeaf;

//------------------------------------------------------------------------------
type
  TCPUIDManufacturerID = (mnOthers,mnAMD,mnCentaur,mnCyrix,mnIntel,mnTransmeta,
                          mnNationalSemiconductor,mnNexGen,mnRise,mnSiS,mnUMC,
                          mnVIA,mnVortex);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TCPUIDInfo_AdditionalInfo = record
    BrandID:                Byte;
    CacheLineFlushSize:     Word; // in bytes (raw data is in qwords)
    LogicalProcessorCount:  Byte; // HTT (see features) must be on, otherwise 0
    LocalAPICID:            Byte;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{
  TCPUIDInfo_Features
  TCPUIDInfo_ExtendedFeatures

  Fields marked with asterisk in curly braces are preliminar. They identify
  features that belong to future extensions which are not yet finalized,
  meaning they can be changed or even removed. They are provided only for
  testing, do not rely on them.

  Number in square brackets is zero-based index of that particular bit in the
  noted register.

  Letters in brackets just behind the bit index in description mark whether
  that particular bit flag is documented by Intel (i) or AMD (a) to be used
  for described feature (at least in documents available while writing this
  library - these might be outdated by now).

      NOTE - some feature bits are supported only by processors from certain
             manufacturers. They should not be relied upon on CPUs that do not
             support them.
}
  TCPUIDInfo_Features = packed record
  {leaf 1, ECX register -------------------------------------------------------}
    SSE3,             // [00] (ia) SSE3 Extensions
    PCLMULQDQ,        // [01] (ia) Carryless Multiplication
    DTES64,           // [02] (i)  64-bit Debug Store Area
    MONITOR,          // [03] (ia) MONITOR and MWAIT Instructions
    DS_CPL,           // [04] (i)  CPL Qualified Debug Store
    VMX,              // [05] (i)  Virtual Machine Extensions
    SMX,              // [06] (i)  Safer Mode Extensions
    EIST,             // [07] (i)  Enhanced Intel SpeedStep Technology
    TM2,              // [08] (i)  Thermal Monitor 2
    SSSE3,            // [09] (ia) SSSE3 Extensions
    CNXT_ID,          // [10] (i)  L1 Context ID
    SDBG,             // [11] (i)  Silicon Debug interface
    FMA,              // [12] (ia) Fused Multiply Add
    CMPXCHG16B,       // [13] (ia) CMPXCHG16B Instruction
    xTPR,             // [14] (i)  Update Control (Can disable sending task priority messages)
    PDCM,             // [15] (i)  Perform & Debug Capability MSR
    PCID,             // [17] (i)  Process-context Identifiers
    DCA,              // [18] (i)  Direct Cache Access
    SSE4_1,           // [19] (ia) SSE4.1 Instructions
    SSE4_2,           // [20] (ia) SSE4.2 Instructions
    x2APIC,           // [21] (ia) x2APIC Support
    MOVBE,            // [22] (ia) MOVBE Instruction
    POPCNT,           // [23] (ia) POPCNT Instruction
    TSC_DEADLINE,     // [24] (i)  APIC supports one-shot operation using a TSC deadline value
    AES,              // [25] (ia) AES Instruction Set
    XSAVE,            // [26] (ia) XSAVE, XRESTOR, XSETBV, XGETBV Instructions
    OSXSAVE,          // [27] (ia) XSAVE enabled by OS
    AVX,              // [28] (ia) Advanced Vector Extensions
    F16C,             // [29] (ia) F16C (half-precision) FP Support
    RDRAND: Boolean;  // [30] (ia) RDRAND (on-chip random number generator) Support
    case Integer of
 0:(HYPERVISOR:       // [31] (a)  Running on a hypervisor (always 0 on a real CPU)
      Boolean);
 1:(RAZ,              //           Same as HYPERVISOR
  {leaf 1, EDX register -------------------------------------------------------}
    FPU,              // [00] (ia) x87 FPU on Chip
    VME,              // [01] (ia) Virtual-8086 Mode Enhancement
    DE,               // [02] (ia) Debugging Extensions
    PSE,              // [03] (ia) Page Size Extensions
    TSC,              // [04] (ia) Time Stamp Counter
    MSR,              // [05] (ia) RDMSR and WRMSR Support
    PAE,              // [06] (ia) Physical Address Extensions
    MCE,              // [07] (ia) Machine Check Exception
    CX8,              // [08] (ia) CMPXCHG8B Instruction
    APIC,             // [09] (ia) APIC on Chip
    SEP,              // [11] (ia) SYSENTER and SYSEXIT Instructions
    MTRR,             // [12] (ia) Memory Type Range Registers
    PGE,              // [13] (ia) PTE Global Bit
    MCA,              // [14] (ia) Machine Check Architecture
    CMOV,             // [15] (ia) Conditional Move/Compare Instruction
    PAT,              // [16] (ia) Page Attribute Table
    PSE_36,           // [17] (ia) Page Size Extension
    PSN,              // [18] (i)  Processor Serial Number
    CLFSH,            // [19] (ia) CLFLUSH Instruction
    DS,               // [21] (i)  Debug Store
    ACPI,             // [22] (i)  Thermal Monitor and Clock Control
    MMX,              // [23] (ia) MMX Technology
    FXSR,             // [24] (ia) FXSAVE/FXRSTOR Instructions
    SSE,              // [25] (ia) SSE Extensions
    SSE2,             // [26] (ia) SSE2 Extensions
    SS,               // [27] (i)  Self Snoop
    HTT,              // [28] (ia) Multi-threading
    TM,               // [29] (i)  Thermal Monitor
    IA64,             // [30]      IA64 processor emulating x86
    PBE,              // [31] (i)  Pending Break Enable
  {leaf 7:0, EBX register -----------------------------------------------------}
    FSGSBASE,             // [00] (ia) RDFSBASE/RDGSBASE/WRFSBASE/WRGSBASE Support
    TSC_ADJUST,           // [01] (ia) IA32_TSC_ADJUST MSR Support
    SGX,                  // [02] (i)  Intel Software Guard Extensions (Intel SGX Extensions)
    BMI1,                 // [03] (ia) Bit Manipulation Instruction Set 1
    HLE,                  // [04] (i)  Transactional Synchronization Extensions
    AVX2,                 // [05] (ia) Advanced Vector Extensions 2
    FPDP,                 // [06] (i)  x87 FPU Data Pointer updated only on x87 exceptions
    SMEP,                 // [07] (ia) Supervisor-Mode Execution Prevention
    BMI2,                 // [08] (ia) Bit Manipulation Instruction Set 2
    ERMS,                 // [09] (ia) Enhanced REP MOVSB/STOSB
    INVPCID,              // [10] (ia) INVPCID Instruction
    RTM: Boolean;         // [11] (i)  Transactional Synchronization Extensions
    case Integer of
 0:(PQM: Boolean);        // [12] (ia) Platform Quality of Service Monitoring
 1:(RDT_M,                //           Resource Director Technology (RDT) Monitoring (PQM)
    FPCSDS,               // [13] (i)  FPU CS and FPU DS deprecated
    MPX: Boolean;         // [14] (i)  Intel MPX (Memory Protection Extensions)
    case Integer of
 0:(PQE: Boolean);        // [15] (ia) Platform Quality of Service Enforcement
 1:(RDT_A,                //           Resource Director Technology Allocation (PQE)
    AVX512F,              // [16] (ia) AVX-512 Foundation
    AVX512DQ,             // [17] (ia) AVX-512 Doubleword and Quadword Instructions
    RDSEED,               // [18] (ia) RDSEED instruction
    ADX,                  // [19] (ia) Intel ADX (Multi-Precision Add-Carry Instruction Extensions)
    SMAP,                 // [20] (ia) Supervisor Mode Access Prevention
    AVX512_IFMA,          // [21] (ia) AVX-512 Integer Fused Multiply-Add Instructions
    PCOMMIT,              // [22]      PCOMMIT Instruction
    CLFLUSHOPT,           // [23] (ia) CLFLUSHOPT Instruction
    CLWB,                 // [24] (ia) CLWB Instruction
    PT,                   // [25] (i)  Intel Processor Trace
    AVX512PF,             // [26] (i)  AVX-512 Prefetch Instructions
    AVX512ER,             // [27] (i)  AVX-512 Exponential and Reciprocal Instructions
    AVX512CD,             // [28] (ia) AVX-512 Conflict Detection Instructions
    SHA,                  // [29] (ia) Intel SHA extensions
    AVX512BW,             // [30] (ia) AVX-512 Byte and Word Instructions
    AVX512VL,             // [31] (ia) AVX-512 Vector Length Extensions
  {leaf 7:0, ECX register -----------------------------------------------------}
    PREFETCHWT1,          // [00] (i)  PREFETCHWT1 Instruction
    AVX512_VBMI,          // [01] (ia) AVX-512 Vector Bit Manipulation Instructions
    UMIP,                 // [02] (ia) User-mode Instruction Prevention
    PKU,                  // [03] (ia) Memory Protection Keys for User-mode pages
    OSPKE,                // [04] (ia) PKU enabled by OS
    WAITPKG,              // [05] (i)  TPAUSE, UMONITOR and UMWAIT instructions
    AVX512_VBMI2:         // [06] (ia) AVX-512 Vector Bit Manipulation Instrutions 2
      Boolean;
    case Integer of
 0:(CET: Boolean);        // [07] (ia) Support for CET shadow stack features
 1:(CET_SS,               //
    GFNI,                 // [08] (ia) Galois Field calculations
    VAES,                 // [09] (ia) (E)VEX-encoded AES instructions (256bit, 512bit)
    VPCLMULQDQ,           // [10] (ia) VPCLMULQDQ instrution
    AVX512_VNNI,          // [11] (ia) AVX-512 Vector Neural Network Instructions
    AVX512_BITALG,        // [12] (ia) AVX-512 Bit Algorithms (VPOPCNT, VPSHUFBITQMB)
    TME_EN,               // [13] (i)  Support for TME model-specific registers (MSR)
    AVX512_VPOPCNTDQ:     // [14] (ia) AVX-512 VPOPCNTD and VPOPCNTQ instructions
      Boolean;
    case Integer of
 0:(VA57: Boolean);       // [16] (ia) 57-bit linear addresses and five-level paging (CR4.LA57),
 1:(LA57: Boolean;        //           VA57 was probably naming mistake
    MAWAU: Byte;          // [17..21]  The value of MAWAU (User MPX (Memory Protection Extensions) address-width adjust)
                          //      (i)  used by the BNDLDX and BNDSTX instructions in 64-bit mode.
    RDPID,                // [22] (ia) Read Processor ID
    KL,                   // [23] (i)  Key Locker
    BUS_LOCK_DETECT,      // [24] (ia) Support for OS bus-lock detection
    CLDEMOTE,             // [25] (i)  Cache Line Demote
    MOVDIRI,              // [27] (ia) MOVDIRI (direct store) instruction
    MOVDIR64B,            // [28] (ia) MOVDIR64B (direct store) instruction
    ENQCMD,               // [29] (i)  Enqueue stores support
    SGX_LC,               // [30] (i)  SGX Launch Configuration
    PKS,                  // [31] (i)  Protection keys for supervisor-mode pages
  {leaf 7:0, EDX register -----------------------------------------------------}
    SGX_KEYS:             // [01] (i)  Attestation Services for SGX
      Boolean;
    case Integer of
 0:(AVX512_QVNNIW,        // [02] (i)  AVX-512 4-iteration single-precision dot products
    AVX512_QFMA: Boolean);// [03] (i)  AVX-512 4-iteration single-precision fused multiply-add
 1:(AVX512_4VNNIW,
    AVX512_4FMAPS,
    REPMOV_FS,            // [04] (i)  Fast Short REP MOV
    UINTR,                // [05] (i)  User Interrups support
    AVX512_VP2INTERSECT,  // [08] (i)  VP2INTERSECTD, VP2INTERSECTQ instructions
    SRBDS_CTRL,           // [09] (i)  Support for IA32_MCU_OPT_CTRL model-specific register (MSR)
    MD_CLEAR,             // [10] (i)  MD_CLEAR operations (fill buffer overwrite, VERW instruction)
    RTM_ALWAYS_ABORT,     // [11] (i)  Execution of XBEGIN immediately aborts and transitions to the specified fallback address
    RTM_FORCE_ABORT,      // [13] (i)  IA32_TSX_FORCE_ABORT MSR support
    SERIALIZE,            // [14] (i)  SERIALIZE instruction
    HYBRID,               // [15] (i)  Processor is identified as a hybrid part
    TSXLDTRK,             // [16] (i)  TSX suspend/resume of load address tracking
    PCONFIG,              // [18] (i)  PCONFIG instruction (platform configuration)
    ARCHITECTURAL_LBRS,   // [19] (i)  Support for architectural LBRs (last branch record)
    CET_IBT,              // [20] (i)  CET indirect branch tracking
    AMX_BF16,             // [22] (i)  AMX tile computational operations on bfloat16
    AVX512_FP16,          // [23] (i)  AVX-512 Half-precision floats (16bit) support
    AMX_TILE,             // [24] (i)  Advanced Matrix Extensions (AMX) support
    AMX_INT8,             // [25] (i)  AMX tile computational operations on 8-bit integers
    IBRS_IBPB,            // [26] (i)  Support for indirect branch restricted speculation and indirect branch predictor barrier
    STIBP,                // [27] (i)  Support for single thread indirect branch predictors
    L1D_FLUSH,            // [28] (i)  IA32_FLUSH_CMD MSR support
    ARCH_CAPABILITIES,    // [29] (i)  IA32_ARCH_CAPABILITIES MSR support
    CORE_CAPABILITIES,    // [30] (i)  IA32_CORE_CAPABILITIES MSR support
    SSBD,                 // [31] (i)  Speculative Store Bypass Disable
  {leaf 7:1, EAX register -----------------------------------------------------}
    SHA512,               // [00] (i)  SHA512 instructions
    SM3,                  // [01] (i)  SM3 instructions
    SM4,                  // [02] (i)  SM4 instructions
{*} RAO_INT,              // [03] (i)  <--pre--> RAO-INT instructions support
    AVX_VNNI,             // [04] (ia) AVX (VEX-encoded) versions of the Vector Neural Network Instructions
    AVX512_BF16,          // [05] (ia) Vector Neural Network Instructions supporting bfloat16
    LASS,                 // [06] (i)  Linear Address Space Separation
    CMPCCXADD,            // [07] (i)  CMPccXADD instruction
    ARCH_PERFMON_EXT,     // [08] (i)  Architectural Performance Monitoring
    REPMOV_FZ,            // [10] (i)  Fast zero-length REP MOVSB
    REPSTOS_FS,           // [11] (i)  Fast short REP STOSB
    REPCMPS_FS,           // [12] (i)  Fast short REP CMPSB and REP SCASB
{*} FRED,                 // [17] (i)  <--pre--> Flexible Return and Event Delivery
{*} LKGS,                 // [18] (i)  <--pre--> Load into IA32_KERNEL_GS_BASE support
    WRMSRNS,              // [19] (i)  WRMSRNS instruction
{*} NMI_SRC,              // [20] (i)  <--pre--> NMI-source reporting support
    AMX_FP16,             // [21] (i)  AMX Tile computational operations on FP16 numbers
    HRESET,               // [22] (i)  History reset support
    AVX_IFMA,             // [23] (i)  AVX versions of Integer Fused Multiply-Add
    LAM,                  // [26] (i)  Linear Address Masking
    MSRLIST,              // [27] (i)  RDMSRLIST and WRMSRLIST instructions
    INVD_DIS_POST_BIOS,   // [30] (i)  (INVD_DISABLE_POST_BIOS_DONE) INVD execution prevention after BIOS done
{*} MOVRS,                // [31] (i)  <--pre--> MOVRS support
  {leaf 7:1, EBX register -----------------------------------------------------}
    PPIN,                 // [00] (i)  IA32_PPIN and IA32_PPIN_CTL MSR support
{*} PBNDKB,               // [01] (i)  <--pre--> PBNDKB instruction support
    CPUIDMAXVAL_LIM_RMV,  // [03] (i)  IA32_MISC_ENABLE[22] cannot be set to 1
  {leaf 7:1, ECX register -----------------------------------------------------}
{*} ASYM_RDTM,            // [00] (i)  <--pre--> At least one logical processor supports Asymmetrical Intel RDT Monitoring capability
{*} ASYM_RDTA,            // [01] (i)  <--pre--> at least one logical processor supports Asymmetrical Intel RDT Allocation capability
{*} MSR_IMM,              // [05] (i)  <--pre--> Immediate forms of the RDMSR and WRMSRNS instructions are supported
  {leaf 7:1, EDX register -----------------------------------------------------}
    AVX_VNNI_INT8,        // [04] (i)  AVX Vector Neural Network Instructions for 8bit integers
    AVX_NE_CONVERT,       // [05] (i)  AVX Instruction for bf16 and fp16 conversions
{*} AMX_COMPLEX,          // [08] (i)  <--pre--> AMX-COMPLEX instructions support
    AVX_VNNI_INT16,       // [10] (i)  AVX Vector Neural Network Instructions for 16bit integers
{*} UTMR,                 // [13] (i)  <--pre--> User-timer events support
    PREFETCHI,            // [14] (i)  PREFETCHIT0/1 instructions
{*} USER_MSR,             // [15] (i)  <--pre--> URDMSR and UWRMSR instructions support
    UIRET_UIF,            // [17] (i)  UIRET sets UIF to the value of bit 1 of the RFLAGS image loaded from the stack
    CET_SSS,              // [18] (i)  Operating system can enable supervisor shadow stacks
    AVX10,                // [19] (i)  Advanced Vector Extensions 10
{*} APX_F,                // [21] (i)  <--pre--> Foundational support for Intel Advanced Performance Extensions
{*} MWAIT,                // [23] (i)  <--pre--> MWAIT support
    SLSM,                 // [24] (i)  IA32_INTEGRITY_STATUS MSR support
  {leaf 7:2, EDX register -----------------------------------------------------}
    PSFD,                 // [00] (i)  Indicates bit 7 of the IA32_SPEC_CTRL MSR is supported (disables Fast
                          //           Store Forwarding Predictor without disabling Speculative Store Bypass)
    IPRED_CTRL,           // [01] (i)  Bits 3 and 4 of the IA32_SPEC_CTRL MSR are supported (#3 enables IPRED_DIS
                          //           control for CPL3, #4 enables IPRED_DIS control for CPL0/1/2)
    RRSBA_CTRL,           // [02] (i)  Bits 5 and 6 of the IA32_SPEC_CTRL MSR are supported (#5 disables RRSBA
                          //           behavior for CPL3, #6 disables RRSBA behavior for CPL0/1/2.)
    DDPD_U,               // [03] (i)  Bit 8 of the IA32_SPEC_CTRL MSR is supported (disables Data Dependent Prefetcher)
    BHI_CTRL,             // [04] (i)  Bit 10 of the IA32_SPEC_CTRL MSR is supported (enables BHI_DIS_S behavior)
    MCDT_NO,              // [05] (i)  Processor do not exhibit MXCSR Configuration Dependent Timing (MCDT)
    UC_LOCK_DIS,          // [06] (i)  Support for UC-lock disable feature
    MONITOR_MITG_NO,      // [07] (i)  Indicates that the MONITOR/UMONITOR instructions are not affected by performance
                          //           or power issues due to exceeding the capacity of an internal monitor tracking table
  {leaf $1E:1, EAX register - AMX TMUL Information leaf -----------------------}
(*
{*} AMX_INT8,             // [00] (i)  <--pre--> AMX tile computational operations on 8-bit integers (mirror of 7:0:EDX[25])
{*} AMX_BF16,             // [01] (i)  <--pre--> AMX tile computational operations on bfloat16 (mirror of 7:0:EDX[22])
{*} AMX_COMPLEX,          // [02] (i)  <--pre--> AMX-COMPLEX instructions support (mirror of 7:1:EDX[08])
{*} AMX_FP16,             // [03] (i)  <--pre--> AMX Tile computational operations on FP16 numbers (mirror of 7:1:ESX[21])
*)
{*} AMX_FP8,              // [04] (i)  <--pre--> Intel AMX computations for the FP8 data type
{*} AMX_TF32,             // [06] (i)  <--pre--> AMX-TF32 (FP19) instructions support
{*} AMX_AVX512,           // [07] (i)  <--pre--> AMX-AVX512 instructions support
{*} AMX_MOVRS:            // [08] (i)  <--pre--> AMX-MOVRS instructions support
      Boolean))))));
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TCPUIDInfo_ExtendedFeatures = packed record
  {leaf $80000001, ECX register -----------------------------------------------}
    AHF64,            // [00] (ia) LAHF/SAHF available in 64-bit mode
    CMP,              // [01] (a)  Core multi-processing legacy mode
    SVM,              // [02] (a)  Secure Virtual Machine
    EAS,              // [03] (a)  Extended APIC space
    CR8D: Boolean;    // [04] (a)  LOCK MOV CR0 means MOV CR8
    case Integer of
 0:(LZCNT: Boolean);  // [05] (ia) Advanced bit manipulation / LZCNT instruction
 1:(ABM,
    SSE4A,            // [06] (a)  SSE4a Support (instructions EXTRQ, INSERTQ, MOVNTSS, and MOVNTSD)
    MSSE: Boolean;    // [07] (a)  Misaligned SSE mode
    case Integer of
 0:(PREFETCHW:        // [08] (i)  PREFETCHW Instructions
      Boolean);
 1:(_3DNOWP:          // [08] (a)  PREFETCH and PREFETCHW Instructions
      Boolean);
 2:(_3DNOW_PREFETCH,
    OSVW,             // [09] (a)  OS Visible Workaround
    IBS,              // [10] (a)  Instruction Based Sampling
    XOP,              // [11] (a)  Extended operation (XOP) instruction set
    SKINIT,           // [12] (a)  SKINIT/STGI instructions
    WDT,              // [13] (a)  Watchdog timer
    LWP,              // [15] (a)  Light Weight Profiling
    FMA4,             // [16] (a)  4-operand fused multiply-add
    TCE,              // [17] (a)  Translation Cache Extension
    NODEID,           // [19]      Node ID MSR (C001_100C)
    TBM,              // [21] (a)  Trailing Bit Manipulation
    TOPX,             // [22] (a)  Topology Extensions
    PCX_CORE,         // [23] (a)  Core performance counter extensions
    PCX_NB,           // [24] (a)  NB performance counter extensions
    DBX,              // [26] (a)  Data access breakpoint extensions
    PERFTSC,          // [27] (a)  Performance time-stamp counter (TSC)
    PCX_L2I: Boolean; // [28] (a)  L2I/L3 performance counter MSR extensions
    case Integer of
 0:(MON: Boolean);    // [29] (a)  MONITORX/MWAITX Instructions
 1:(MONX,
    ADDRMASKEXT,      // [30] (a)  Breakpoint Addressing masking extended to bit 31
  {leaf $80000001, EDX register -----------------------------------------------}
    FPU,              // [00] (a)  Onboard x87 FPU (mirror of CPUID:1:EDX[FPU])
    VME,              // [01] (a)  Virtual mode enhancements (mirror of CPUID:1:EDX[VME])
    DE,               // [02] (a)  Debugging extensions (mirror of CPUID:1:EDX[DE])
    PSE,              // [03] (a)  Page Size Extension (mirror of CPUID:1:EDX[PSE])
    TSC,              // [04] (a)  Time Stamp Counter (mirror of CPUID:1:EDX[TSC])
    MSR,              // [05] (a)  Model-specific registers (mirror of CPUID:1:EDX[MSR])
    PAE,              // [06] (a)  Physical Address Extension (mirror of CPUID:1:EDX[PAE])
    MCE,              // [07] (a)  Machine Check Exception (mirror of CPUID:1:EDX[MCE])
    CX8,              // [08] (a)  CMPXCHG8 instruction (mirror of CPUID:1:EDX[CX8])
  {
    If the APIC has been disabled, then the APIC feature flag will read as 0.
  }
    APIC,             // [09] (a)  Onboard Advanced Programmable Interrupt Controller (mirror of CPUID:1:EDX[APIC])
  {
    The AMD K6 processor, model 6, uses bit 10 to indicate SEP. Beginning with
    model 7, bit 11 is used instead.
    Intel processors only report SEP when CPUID is executed in PM64.
  }
    SEP,              // [11] (ia) SYSCALL and SYSRET Instructions (mirror of CPUID:1:EDX[SEP])
    MTRR,             // [12] (a)  Memory-Type Range Registers (mirror of CPUID:1:EDX[MTRR])
    PGE,              // [13] (a)  Page Global Extension (mirror of CPUID:1:EDX[PGE])
    MCA,              // [14] (a)  Machine check architecture (mirror of CPUID:1:EDX[MCA])
    CMOV: Boolean;    // [15] (a)  Conditional move instructions (mirror of CPUID:1:EDX[CMOV])
    case Integer of
 0:(PAT: Boolean);    // [16] (a)  Page Attribute Table (mirror of CPUID:1:EDX[PAT])
 1:(FCMOV,            // [16]      ??? (http://sandpile.org/x86/cpuid.htm)
    PSE36,            // [17] (a)  36-bit page size extension (mirror of CPUID:1:EDX[PSE36])
  {
    AMD K7 processors prior to CPUID=0662h may report 0 even if they are MP-capable.
  }
    MP,               // [19]      Multiprocessor Capable
    NX,               // [20] (ia) Execute Disable Bit available
    MMXEXT,           // [22] (a)  AMD extensions to MMX instructions
    MMX: Boolean;     // [23] (a)  MMX Instructions (mirror of CPUID:1:EDX[MMX])
    case Integer of
 0:(FXSR: Boolean);   // [24] (a)  FXSAVE, FXRSTOR instructions (mirror of CPUID:1:EDX[FXSR])
 1:(MMXEXT_CYRIX,     // [24]      Cyrix Extended MMX
    FFXSR,            // [25] (a)  FXSAVE/FXRSTOR optimizations
    PG1G,             // [26] (ia) 1-GByte pages are available
    TSCP,             // [27] (ia) RDTSCP and IA32_TSC_AUX are available
    LM,               // [29] (ia) Long Mode - AMD64/EM64T/Intel 64 Architecture
    _3DNOWEXT,        // [30] (a)  AMD extensions to 3DNow! instructions.
    _3DNOW: Boolean;  // [31] (a)  3DNow! instructions
  {leaf $80000007, EDX register - Advanced Power Management Features ----------}
    ITSC,             // [08] (ia) Invariant TSC
  {leaf $80000008, EBX register -----------------------------------------------}
    CLZERO: Boolean;  // [00] (a)  CLZERO instruction
    case Integer of
 0:(INSTRSTCNTMSR,    // [01] (a)  Instruction Retired Counter MSR
    RSTRFPERRPTRS:    // [02] (a)  FP Error Pointers Restored by XRSTOR
      Boolean);
 1:(IRPERF,           // [01]      Read-only IRPERF (MSR C000_00E9h)
    ASRFPEP,          // [02] (a)  Always save/restore FP error pointers (same thing as RSTRFPERRPTRS, only different field name)
    INVLPGB,          // [03] (a)  INVLPGB and TLBSYNC instructions
    RDPRU,            // [04] (a)  RDPRU instruction
    BE,               // [06] (a)  Bandwidth Enforcement Extension
    MCOMMIT,          // [08] (a)  MCOMMIT instruction
    WBNOINVD,         // [09] (ia) WBNOINVD instruction
    IBPB,             // [12] (a)  Indirect Branch Prediction Barrier
    INT_WBINVD,       // [13] (a)  WBINVD/WBNOINVD are interruptible
    IBRS,             // [14] (a)  Indirect Branch Restricted Speculation
    STIBP,            // [15] (a)  Single Thread Indirect Branch Prediction mode
    IBRS_ALL,         // [16] (a)  IBRS always on mode
    STIBP_ALL,        // [17] (a)  STIBP always on mode
    IBRS_PREF,        // [18] (a)  IBRS is preferred to software solution
    IBRS_SAMEMODE,    // [19] (a)  IBRS provides same mode speculation limits
    EFER_LMSLE_UNSUP, // [20] (a)  EFER.LMSLE is unsupported
    INVLPGB_NESTPGS,  // [21] (a)  INVLPGB support for invalidating guest nested translations
    SSBD,             // [24] (a)  Speculative Store Bypass Disable
    SSBD_VSC,         // [25] (a)  (SsbdVirtSpecCtrl) Use VIRT_SPEC_CTL MSR (C001_011Fh) for SSBD
    SSBD_NR,          // [26] (a)  (SsbdNotRequired) SSBD not needed on this processor
    CPPC,             // [27] (a)  Collaborative Processor Performance Control
    PSFD,             // [28] (a)  Predictive Store Forward Disable
    BTC_NO,           // [29] (a)  The processor is not affected by branch type confusion
    IBPB_RET,         // [30] (a)  IBPB clears return address predictor
  {leaf $80000021, EAX register -----------------------------------------------}
    NONESTDATABP,     // [00] (a)  Processor ignores nested data breakpoints
    FSGSBW_NSER,      // [01] (a)  WRMSR to FS.Base, GS.Base and KernelGSBase MSRs is not serializing
    LFENCE_SER,       // [02] (a)  LFENCE is always dispatch serializing
    SMMPGCFGLOCK,     // [03] (a)  SMM paging configuration lock supported
    NULLSELCLRBASE,   // [06] (a)  Null segment selector loads also clear the destination segment register base and limit
    UPADDRIGNORE,     // [07] (a)  Upper Address Ignore is supported
    AUTOIBRS,         // [08] (a)  Automatic IBRS
    NOSMMCTL,         // [09] (a)  SMM_CTL MSR (C001_0116h) is not supported
    REPSTOS_FS,       // [10] (a)  Fast short REP STOSB supported
    REPCMPS_FS,       // [11] (a)  Fast short REPE CMPSB supported
    PMC2_PRECRET,     // [12] (a)  MSR PerfEvtSel2[PreciseRetire] is supported
    PREFETCHCTLMSR,   // [13] (a)  Prefetch control MSR supported. See Core::X86::Msr::PrefetchControl in BKDG or PPR for details
    L2TLBSIZEX32,     // [14] (a)  L2TLB sizes are encoded as multiples of 32
    AMD_ERMSB,        // [15] (a)  AMD implementation of Enhanced REP MOVSB/STOSB is supported
    OP0F017RECL,      // [16] (a)  0F 01/7 opcode space is reserved for AMD use
    CPUIDUSESDIS,     // [17] (a)  CPUID disable for non-privileged software
    EPSF,             // [18] (a)  Enhanced Predictive Store Forwarding supported
    REPSCASB_FS,      // [19] (a)  Fast short REP SCASB supported
    PREFETCHI,        // [20] (a)  IC prefetch supported
    FP512_DOWNGD,     // [21] (a)  FP512 is downgraded to FP256
    ERAPS,            // [24] (a)  Enhanced Return Address Predictor Security supported
    SBPB,             // [27] (a)  Selective Branch Predictor Barrier supported
    IBPB_BRTYPE,      // [28] (a)  PRED_CMD[IBPB] clears all branch type predictions from the branch predictor
    SRSO_NO,          // [29] (a)  The processor is not affected by Speculative Return Stack Overflow vulnerability
    SRSO_USRKRNL_NO,  // [30] (a)  The processor is not affected by Speculative Return Stack Overflow vulnerability across user/kernel boundaries
    SRSO_MSR_FIX:     // [31] (a)  Software may use MSR_BP_CFG[BpSpecReduce] to mitigate Speculative Return Stack Overflow vulnerability
      Boolean))))));
  end;

//------------------------------------------------------------------------------
type
  TCPUIDInfo_SupportedExtensions = packed record
    X87,            // x87 FPU                                            features.FPU
    EmulatedX87,    // x87 is emulated                                    CR0[EM:2]=1
    MMX,            // MMX Technology                                     features.MMX and CR0[EM:2]=0
    SSE,            // Streaming SIMD Extensions                          features.FXSR and features.SSE and (system support for SSE)
    SSE2,           // Streaming SIMD Extensions 2                        features.SSE2 and SSE
    SSE3,           // Streaming SIMD Extensions 3                        features.SSE3 and SSE2
    SSSE3,          // Supplemental Streaming SIMD Extensions 3           features.SSSE3 and SSE3
    SSE4_1,         // Streaming SIMD Extensions 4.1                      features.SSE4_1 and SSSE3
    SSE4_2,         // Streaming SIMD Extensions 4.2                      features.SSE4_2 and SSE4_1
    CRC32,          // CRC32 Instruction                                  features.SSE4_2
    POPCNT,         // POPCNT Instruction                                 features.POPCNT and features.SSE4_2
    AES,            // AES New Instructions                               features.AES and SSE2
    PCLMULQDQ,      // PCLMULQDQ Instruction                              features.PCLMULQDQ and SSE2
    GFNI,           // Galois Field Instructions                          features.GFNI and SSE2
    AVX,            // Advanced Vector Extensions                         features.OSXSAVE -> XCR0[1..2]=11b and features.AVX
    F16C,           // 16bit Float Conversion Instructions                features.F16C and AVX
    FMA,            // Fused-Multiply-Add Instructions                    features.FMA and AVX
    // VEX-encoded instructions supporting three-operand format
    VAES128,        // VEX-encoded 128bit AES Instructions                features.AES and AVX
    VPCLMULQDQ128,  // VEX-encoded 128bit PCLMULQDQ Instruction           features.PCLMULQDQ and AVX
    VAES,           // VEX-encoded 256bit AES Instructions                features.VAES and AVX
    VPCLMULQDQ,     // VEX-encoded 256bit PCLMULQDQ Instruction           features.VPCLMULQDQ and AVX
    // VGFNI supports both 128bit and 256 vectors
    VGFNI,          // VEX-encoded Galois Field Instructions              features.GFNI and AVX
    AVX2: Boolean;  // Advanced Vector Extensions 2                       features.AVX2 and AVX
    case Integer of
0: (AVX512F,        // see AVX512.Supported
    AVX512ER,       // see AVX512.AVX512ER
    AVX512PF,       // see AVX512.AVX512PF
    AVX512CD,       // see AVX512.AVX512CD
    AVX512DQ,       // see AVX512.AVX512DQ
    AVX512BW:       // see AVX512.AVX512BW
      Boolean);
1: (AVX512: record
      // structured AVX512 extension report
      Supported,            // AVX-512 Foundation Instructions                            features.OSXSAVE -> XCR0[1..2]=11b and
                            //                                                              XCR0[5..7]=111b and features.AVX512F
    {
      Extensions AVX512ER, AVX512PF, AVX512_4VNNIW and AVX512_4FMAPS are
      available only on Intel Xeon Phi processors.

        WARNING - If any AVX512 instruction is to operate on 256bit or 128bit
                  vectors (not only on 512bit vector), it is necessary to also
                  check AVX512VL flag (vector length extension). But note that
                  some instructions cannot operate on 128bit vectors even when
                  vector length extension is supported.
    }
      AVX512ER,             // AVX-512 Exponential and Reciprocal Instructions            features.AVX512ER and AVX512F
      AVX512PF,             // AVX-512 Prefetch Instructions                              features.AVX512PF and AVX512F
      AVX512CD,             // AVX-512 Conflict Detection Instructions                    features.AVX512CD and AVX512F
      AVX512DQ,             // AVX-512 Doubleword and Quadword Instructions               features.AVX512DQ and AVX512F
      AVX512BW,             // AVX-512 Byte and Word Instructions                         features.AVX512BW and AVX512F
      AVX512VL,             // AVX-512 Vector Length Extensions                           features.AVX512VL and AVX512F
      AVX512_VBMI,          // AVX-512 Vector Bit Manipulation Instructions               features.AVX512_VBMI and AVX512F
      AVX512_VBMI2,         // AVX-512 Vector Bit Manipulation Instructions 2             features.AVX512_VBMI2 and AVX512F
      AVX512_IFMA,          // AVX-512 Integer Fused Multiply-Add Instructions            features.AVX512_IFMA and AVX512F
      AVX512_VNNI,          // AVX-512 Vector Neural Network Instructions                 features.AVX512_VNNI and AVX512F
      AVX512_BF16,          // AVX-512 VNNI supporting bfloat16                           features.AVX512_BF16 and AVX512F
      AVX512_VPOPCNTDQ,     // AVX-512 VPOPCNTD and VPOPCNTQ instructions                 features.AVX512_VPOPCNTDQ and AVX512F
      AVX512_BITALG,        // AVX-512 Bit Algorithms                                     features.AVX512_BITALG and AVX512F
      AVX512_FP16,          // AVX-512 Half-precision floats (16bit) support              features.AVX512_FP16 and AVX512F
      AVX512_4VNNIW,        // AVX-512 4-iteration single-precision dot products          features.AVX512_4VNNIW and AVX512F
      AVX512_4FMAPS,        // AVX-512 4-iteration single-precision fused multiply-add    features.AVX512_4FMAPS and AVX512F
      AVX512_VP2INTERSECT,  // AVX-512 Intersect instructions                             features.AVX512_VP2INTERSECT and AVX512F
      VAES,                 // EVEX-encoded AES Instructions                              features.VAES and AVX512F
      VPCLMULQDQ,           // EVEX-encoded PCLMULQDQ Instruction                         features.VPCLMULQDQ and AVX512F
      GFNI:                 // EVEX-encoded Galois Field Instructions                     features.GFNI and AVX512F
        Boolean;
    end;
  {
    Following three substructures (AVX10, AMX and APX) are all preliminar,
    because afaik none of the corresponding extensions currently have final
    specification (2025-11).

    This means not only that new fields can be added, but also that any field
    can be changed or even removed, be aware of that.
  }

    AVX10: record           // Advanced Vector Extensions 10
      Supported:  Boolean;  // AVX10 supported and usable                 features.AVX10
      Version:    Byte;     // >= 1, 0 if AVX10 is not supported          features.AVX10 -> CPUID:0x24.0:EBX[0..7]
    {
      If AVX10 is not supported in any version, then all three following flags
      (VecXXX) are guaranteed to be false.

      Note that these fields are superfluous, because current specification of
      AVX10 states that support for all vector lengths is mandatory for all
      implementations. Also, the corresponding bits are, in current documents,
      marked as reserved and always read as 1.
      These fields are kept for backward compatibility and their values can
      be relied upon.  
    }
      Vec128,               // 128bit vectors supported                   features.AVX10 -> CPUID:0x24.0:EBX[16]
      Vec256,               // 256bit vectors supported                   features.AVX10 -> CPUID:0x24.0:EBX[17]
      Vec512:     Boolean;  // 512bit vectors supported                   features.AVX10 -> CPUID:0x24.0:EBX[18]
    end;
    AMX: record             // Advanced Matrix Extensions
      Supported,            // Tile architecture supported                features.AMX_TILE
      AMX_INT8,             // AMX operations on 8-bit integers           features.AMX_INT8
      AMX_BF16,             // AMX operations on bfloat16                 features.AMX_BF16
      AMX_COMPLEX,          // AMX-COMPLEX instructions                   features.AMX_COMPLEX
      AMX_FP16,             // AMX operations on FP16 numbers             features.AMX_FP16
      AMX_FP8,              // AMX computations for the FP8 data type     features.AMX_FP8
      AMX_TF32,             // AMX-TF32 (FP19) instructions               features.AMX_TF32
      AMX_AVX512,           // AMX_AVX512 instructions                    features.AMX_AVX512
      AMX_MOVRS:  Boolean;  // AMX-MOVRS instructions                     features.AMX_MOVRS
    end;
    APX: record
      Supported:  Boolean;  // Advanced Performance Extensions            features.APX_F
      // expecting some future extensions or flags
    end);
  end;

//------------------------------------------------------------------------------
type
  TCPUIDInfo = record
    // leaf 0x00000000
    ManufacturerIDString:       String;
    ManufacturerID:             TCPUIDManufacturerID; // discerned from ManufacturerIDString
    // leaf 0x00000001
    ProcessorType:              Byte;
    ProcessorFamily:            Byte;
    ProcessorModel:             Byte;
    ProcessorStepping:          Byte;
    AdditionalInfo:             TCPUIDInfo_AdditionalInfo;
    // leafs 0x00000001+ (basic leafs)
    ProcessorFeatures:          TCPUIDInfo_Features;
    // leafs 0x80000001+ (extended leafs)
    ExtendedProcessorFeatures:  TCPUIDInfo_ExtendedFeatures;
    // leaf 0x80000002 - 0x80000004
    BrandString:                String;
    // some processor extensions whose full support cannot (or should not)
    // be determined directly from processor features...
    SupportedExtensions:        TCPUIDInfo_SupportedExtensions;
  end;

{===============================================================================
    TSimpleCPUID - class declaration
===============================================================================}
type
  TSimpleCPUID = class(TObject)
  protected
    fSupported: Boolean;
    fLoaded:    Boolean;
    fLeafs:     array of TCPUIDLeaf;
    fInfo:      TCPUIDInfo;
    fHiStdLeaf: Integer;  // index of highest standard leaf
    Function GetLeaf(Index: Integer): TCPUIDLeaf; virtual;
    Function GetLeafCount: Integer; virtual;    
    // leading and processing of CPUID leafs
    procedure LoadLeafGroup(GroupMask: UInt32); virtual;
    procedure LoadStdLeafs; virtual;        // standard leafs
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
    procedure ProcessLeaf_0000_0018; virtual;
    procedure ProcessLeaf_0000_001A; virtual;
    procedure ProcessLeaf_0000_001B; virtual;
    procedure ProcessLeaf_0000_001D; virtual;
    procedure ProcessLeaf_0000_001E; virtual;
    procedure ProcessLeaf_0000_001F; virtual;
    procedure ProcessLeaf_0000_0020; virtual;
    procedure ProcessLeaf_0000_0023; virtual;
    procedure ProcessLeaf_0000_0024; virtual;
    procedure ProcessLeaf_0000_0027; virtual;
    procedure ProcessLeaf_0000_0028; virtual;
    procedure LoadPhiLeafs; virtual;        // Intel Xeon Phi leafs
    procedure LoadHypLeafs; virtual;        // hypervisor leafs
    procedure LoadExtLeafs; virtual;        // extended leafs
    procedure ProcessLeaf_8000_0001; virtual;
    procedure ProcessLeaf_8000_0002_to_8000_0004; virtual;
    procedure ProcessLeaf_8000_0007; virtual;
    procedure ProcessLeaf_8000_0008; virtual;
    procedure ProcessLeaf_8000_001D; virtual;
    procedure ProcessLeaf_8000_0021; virtual;
    procedure LoadTNMLeafs; virtual;        // Transmeta leafs
    procedure LoadCNTLeafs; virtual;        // Centaur leafs
    procedure LoadAllLeafs; virtual;
    // information parsing
    procedure InitSupportedExtensions; virtual;
    procedure ClearInfo; virtual;
    // object init/final
    procedure Initialize(LoadInfo: Boolean); virtual;
    procedure Finalize; virtual;
    // leaf utilities
    class Function SameLeafData(A,B: TCPUIDLeafData): Boolean; virtual;
    Function EqualsToHighestStdLeafData(LeafData: TCPUIDLeafData): Boolean; virtual;
  public
    constructor Create(LoadInfo: Boolean = True);
    destructor Destroy; override;
    procedure LoadInfo; virtual;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    Function IndexOf(LeafID: UInt32): Integer; virtual;
    Function Find(LeafID: UInt32; out Index: Integer): Boolean; virtual;
    property Supported: Boolean read fSupported;
    property Loaded: Boolean read fLoaded;
    property Leafs[Index: Integer]: TCPUIDLeaf read GetLeaf; default;
    property Count: Integer read GetLeafCount;
    property Info: TCPUIDInfo read fInfo;    
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
    constructor Create(ProcessorID: Integer = 0; LoadInfo: Boolean = True);
    procedure LoadInfo; override;
    property ProcessorID: Integer read fProcessorID write fProcessorID;
  end;

implementation

uses
{$IFDEF Windows}Windows{$ELSE}BaseUnix{$ENDIF}
{$IF not Defined(FPC) and (CompilerVersion >= 20)}  // Delphi 2009+
  , AnsiStrings
{$IFEND};

{$IFNDEF Windows}
  {$LINKLIB C}
{$ENDIF}

{===============================================================================
    External and system functions
===============================================================================}
{$IFDEF Windows}

Function GetProcessAffinityMask(hProcess: THandle; lpProcessAffinityMask,lpSystemAffinityMask: PPtrUInt): BOOL; stdcall; external kernel32;

{$ELSE}
//------------------------------------------------------------------------------

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

{===============================================================================
    Auxiliary routines
===============================================================================}

{$IF not(Defined(Windows) and Defined(x86))}
Function GetBit(Value: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := ((Value shr Bit) and 1) <> 0;
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
  // following should preserve content of XMM0
  ORPS    XMM0, XMM0
end;

//------------------------------------------------------------------------------

Function CanExecuteSSE: Boolean;
begin
try
  TestSSE;
  Result := True;
except
  // eat all exceptions
  Result := False;
end;
end;

{===============================================================================
    Main CPUID routines (ASM)
===============================================================================}

Function CPUIDSupported: Boolean;
const
  FLAGS_IDFlagBit = 21;
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  Result is returned in EAX register (all modes).
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}

  // see 32bit code for description, there is no difference
  PUSHFQ
  MOV     RAX, qword ptr [RSP]
  BTC     qword ptr [RSP], FLAGS_IDFlagBit
  POPFQ

  PUSHFQ
  XOR     RAX, qword ptr [RSP]
  BT      RAX, FLAGS_IDFlagBit
  SETC    AL
  ADD     RSP, 8

{$ELSE}

  // save initial value of FLAGS register
  PUSHFD
  // make copy of initial value for final comparison
  MOV     EAX, dword ptr [ESP]
  // invert bit 21 (ID flag bit)
  BTC     dword ptr [ESP], FLAGS_IDFlagBit
  // load value with inverted ID flag from stack back into FLAGS
  POPFD

  // save FLAGS again
  PUSHFD
{
  XOR current FLAGS value with initial value - this will clear all bits that
  are the same and set bits that are different.
}
  XOR     EAX, dword ptr [ESP]
{
  Test whether the ID flag bit is set - if so, that means it can be changed in
  FLAGS register and therefore CPUID is supported.

  Note that this test could be omitted if we assume that no other flag bit
  changed, but let's be paranoid and not assume anything.
}
  BT      EAX, FLAGS_IDFlagBit
  // set result accordingly
  SETC    AL
  // clear stack
  ADD     ESP, 4
  
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure CPUID(Leaf, SubLeaf: UInt32; Result: Pointer);
asm
{$IFDEF x64}
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Register content on enter:

  Win64  Lin64

   ECX    EDI   Leaf of the CPUID info (parameter for CPUID instruction)
   EDX    ESI   SubLeaf of the CPUID info (valid only for some leafs)
    R8    RDX   Address of memory space (at least 16 bytes long) to which
                resulting data (registers EAX, EBX, ECX and EDX, in that order)
                will be copied

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }

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
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Win32, Lin32

  Register content on enter:

    EAX - Leaf of the CPUID info (parameter for CPUID instruction)
    EDX - SubLeaf of the CPUID info (valid only for some leafs)
    ECX - Address of memory space (at least 16 bytes long) to which resulting
          data (registers EAX, EBX, ECX and EDX, in that order) will be copied

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }

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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CPUID(Leaf: UInt32; Result: Pointer);
begin
CPUID(Leaf,0,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CPUID(Leaf,SubLeaf: UInt32; out Info: TCPUIDLeafData);
begin
CPUID(Leaf,SubLeaf,@Info);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CPUID(Leaf: UInt32; out Info: TCPUIDLeafData);
begin
CPUID(Leaf,0,@Info);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleCPUID
--------------------------------------------------------------------------------
===============================================================================}
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

procedure TSimpleCPUID.LoadLeafGroup(GroupMask: UInt32);
const
  LEAF_GROUPMASK = UInt32($FFFF0000);
var
  LeafData: TCPUIDLeafData;
  OldCount: Integer;
  i:        Integer;
begin
// get leaf count
CPUID(GroupMask,LeafData);
If (LeafData.EAX and LEAF_GROUPMASK) = GroupMask then
  begin
    OldCount := Length(fLeafs);
    SetLength(fLeafs,Length(fLeafs) + Integer(LeafData.EAX and UInt32(not Int32(LEAF_GROUPMASK))) + 1);
    // load all leafs
    For i := OldCount to HighIndex do
      begin
        fLeafs[i].ID := (UInt32(i - OldCount) and UInt32(not Int32(LEAF_GROUPMASK))) or (GroupMask and LEAF_GROUPMASK);
        CPUID(fLeafs[i].ID,fLeafs[i].Data);
        fLeafs[i].SubLeafs := nil;  // these are resolved for specific leafs
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadStdLeafs;
begin
LoadLeafGroup($00000000);
If Length(fLeafs) > 0 then
  fHiStdLeaf := High(fLeafs)
else
  fHiStdLeaf := -1;
// process specific leafs
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
ProcessLeaf_0000_0018;
ProcessLeaf_0000_001A;
ProcessLeaf_0000_001B;
ProcessLeaf_0000_001D;
ProcessLeaf_0000_001E;
ProcessLeaf_0000_001F;
ProcessLeaf_0000_0020;
ProcessLeaf_0000_0023;
ProcessLeaf_0000_0024;
ProcessLeaf_0000_0027;
ProcessLeaf_0000_0028;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0000;
const
  Manufacturers: array[0..13] of record
    IDStr:  String;
    ID:     TCPUIDManufacturerID;
  end = (
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
type
  TIDStringOverlay = packed array[0..2] of UInt32;
  PIDStringOverlay = ^TIDStringOverlay;
var
  Index:      Integer;
  Str:        AnsiString;
  StrOverlay: PIDStringOverlay;
  i:          Integer;
begin
If Find($00000000,Index) then
  begin
    Str := '';
    SetLength(Str,12);
    StrOverlay := PIDStringOverlay(PAnsiChar(Str));
    StrOverlay^[0] := fLeafs[Index].Data.EBX;
    StrOverlay^[1] := fLeafs[Index].Data.EDX;
    StrOverlay^[2] := fLeafs[Index].Data.ECX;
    fInfo.ManufacturerIDString := String(Str);
    fInfo.ManufacturerID := mnOthers;
    For i := Low(Manufacturers) to High(Manufacturers) do
      If AnsiSameStr(Manufacturers[i].IDStr,fInfo.ManufacturerIDString) then
        begin
          fInfo.ManufacturerID := Manufacturers[i].ID;
          Break{For i};
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0001;
var
  Index:  Integer;
begin
If Find($00000001,Index) then
  begin
    // EAX - processor info   --  --  --  --  --  --  --  --  --  --  --  --  --
    
    // processor type (intel only)
    fInfo.ProcessorType := GetBits(fLeafs[Index].Data.EAX,12,13);
    // processor family
    If GetBits(fLeafs[Index].Data.EAX,8,11) >= $F then
      // extended family
      fInfo.ProcessorFamily := GetBits(fLeafs[Index].Data.EAX,8,11) + GetBits(fLeafs[Index].Data.EAX,20,27)
    else
      fInfo.ProcessorFamily := GetBits(fLeafs[Index].Data.EAX,8,11);
    // processor model
    If (GetBits(fLeafs[Index].Data.EAX,8,11) = $F) or
      ((GetBits(fLeafs[Index].Data.EAX,8,11) = $6) and (fInfo.ManufacturerID = mnIntel)) then
      // extended model
      fInfo.ProcessorModel := (GetBits(fLeafs[Index].Data.EAX,16,19) shl 4) or GetBits(fLeafs[Index].Data.EAX,4,7)
    else
      fInfo.ProcessorModel := GetBits(fLeafs[Index].Data.EAX,4,7);
    // processor stepping
    fInfo.ProcessorStepping := GetBits(fLeafs[Index].Data.EAX,0,3);

    // EBX - additional info  --  --  --  --  --  --  --  --  --  --  --  --  --

    fInfo.AdditionalInfo.BrandID               := GetBits(fLeafs[Index].Data.EBX,0,7);
    fInfo.AdditionalInfo.CacheLineFlushSize    := GetBits(fLeafs[Index].Data.EBX,8,15) * 8;
    If GetBit(fLeafs[Index].Data.EDX,28{HTT}) then
      fInfo.AdditionalInfo.LogicalProcessorCount := GetBits(fLeafs[Index].Data.EBX,16,23)
    else
      fInfo.AdditionalInfo.LogicalProcessorCount := 0; // this fields is invalid
    fInfo.AdditionalInfo.LocalAPICID           := GetBits(fLeafs[Index].Data.EBX,24,31);

    // ECX,EDX - processor features   --  --  --  --  --  --  --  --  --  --  --

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
  This entire function must be run on the same processor (system cannot be
  allowed to assign current thread elsewhere), otherwise results are undefined.

  Intel documentation explicitly states that EAX[0..7] for this leaf is always
  1, and this value should be ignored.
  But according to sandpile, this leaf might be run several times with differing
  results and this number gives the repeat count.
}
If Find($00000002,Index) then
  If Byte(fLeafs[Index].Data.EAX) <> 0 then
    begin
      SetLength(fLeafs[Index].SubLeafs,Byte(fLeafs[Index].Data.EAX));
      fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
      For i := 1 to High(fLeafs[Index].SubLeafs) do
        CPUID(2,UInt32(i),fLeafs[Index].SubLeafs[i]);
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0004;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($00000004,Index) then
  begin
    Temp := fLeafs[Index].Data;
    while (Temp.EAX and $1F{cache type field}) <> 0 do
      begin
        SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
        fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
        CPUID(4,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
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
      CPUID(7,UInt32(i),fLeafs[Index].SubLeafs[i]);
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
        AVX512_IFMA  := GetBit(fLeafs[Index].Data.EBX,21);
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
        PREFETCHWT1      := GetBit(fLeafs[Index].Data.ECX,0);
        AVX512_VBMI      := GetBit(fLeafs[Index].Data.ECX,1);
        UMIP             := GetBit(fLeafs[Index].Data.ECX,2);
        PKU              := GetBit(fLeafs[Index].Data.ECX,3);
        OSPKE            := GetBit(fLeafs[Index].Data.ECX,4);
        WAITPKG          := GetBit(fLeafs[Index].Data.ECX,5);
        AVX512_VBMI2     := GetBit(fLeafs[Index].Data.ECX,6);
        CET              := GetBit(fLeafs[Index].Data.ECX,7);
        GFNI             := GetBit(fLeafs[Index].Data.ECX,8);
        VAES             := GetBit(fLeafs[Index].Data.ECX,9);
        VPCLMULQDQ       := GetBit(fLeafs[Index].Data.ECX,10);
        AVX512_VNNI      := GetBit(fLeafs[Index].Data.ECX,11);
        AVX512_BITALG    := GetBit(fLeafs[Index].Data.ECX,12);
        TME_EN           := GetBit(fLeafs[Index].Data.ECX,13);
        AVX512_VPOPCNTDQ := GetBit(fLeafs[Index].Data.ECX,14);
        VA57             := GetBit(fLeafs[Index].Data.ECX,16);
        MAWAU            := Byte(GetBits(fLeafs[Index].Data.ECX,17,21));
        RDPID            := GetBit(fLeafs[Index].Data.ECX,22);
        KL               := GetBit(fLeafs[Index].Data.ECX,23);
        BUS_LOCK_DETECT  := GetBit(fLeafs[Index].Data.ECX,24);
        CLDEMOTE         := GetBit(fLeafs[Index].Data.ECX,25);
        MOVDIRI          := GetBit(fLeafs[Index].Data.ECX,27);
        MOVDIR64B        := GetBit(fLeafs[Index].Data.ECX,28);
        ENQCMD           := GetBit(fLeafs[Index].Data.ECX,29);
        SGX_LC           := GetBit(fLeafs[Index].Data.ECX,30);
        PKS              := GetBit(fLeafs[Index].Data.ECX,31);
      {EDX register}
        SGX_KEYS            := GetBit(fLeafs[Index].Data.EDX,1);
        AVX512_QVNNIW       := GetBit(fLeafs[Index].Data.EDX,2);
        AVX512_QFMA         := GetBit(fLeafs[Index].Data.EDX,3);
        REPMOV_FS           := GetBit(fLeafs[Index].Data.EDX,4);
        UINTR               := GetBit(fLeafs[Index].Data.EDX,5);
        AVX512_VP2INTERSECT := GetBit(fLeafs[Index].Data.EDX,8);
        SRBDS_CTRL          := GetBit(fLeafs[Index].Data.EDX,9);
        MD_CLEAR            := GetBit(fLeafs[Index].Data.EDX,10);
        RTM_ALWAYS_ABORT    := GetBit(fLeafs[Index].Data.EDX,11);
        RTM_FORCE_ABORT     := GetBit(fLeafs[Index].Data.EDX,13);
        SERIALIZE           := GetBit(fLeafs[Index].Data.EDX,14);
        HYBRID              := GetBit(fLeafs[Index].Data.EDX,15);
        TSXLDTRK            := GetBit(fLeafs[Index].Data.EDX,16);
        PCONFIG             := GetBit(fLeafs[Index].Data.EDX,18);
        ARCHITECTURAL_LBRS  := GetBit(fLeafs[Index].Data.EDX,19);
        CET_IBT             := GetBit(fLeafs[Index].Data.EDX,20);
        AMX_BF16            := GetBit(fLeafs[Index].Data.EDX,22);
        AVX512_FP16         := GetBit(fLeafs[Index].Data.EDX,23);
        AMX_TILE            := GetBit(fLeafs[Index].Data.EDX,24);
        AMX_INT8            := GetBit(fLeafs[Index].Data.EDX,25);
        IBRS_IBPB           := GetBit(fLeafs[Index].Data.EDX,26);
        STIBP               := GetBit(fLeafs[Index].Data.EDX,27);
        L1D_FLUSH           := GetBit(fLeafs[Index].Data.EDX,28);
        ARCH_CAPABILITIES   := GetBit(fLeafs[Index].Data.EDX,29);
        CORE_CAPABILITIES   := GetBit(fLeafs[Index].Data.EDX,30);
        SSBD                := GetBit(fLeafs[Index].Data.EDX,31);
      end;
    If Length(fLeafs[Index].SubLeafs) > 1 then
      with fInfo.ProcessorFeatures do
        begin
        {EAX register}
          SHA512             := GetBit(fLeafs[Index].SubLeafs[1].EAX,0);
          SM3                := GetBit(fLeafs[Index].SubLeafs[1].EAX,1);
          SM4                := GetBit(fLeafs[Index].SubLeafs[1].EAX,2);
          RAO_INT            := GetBit(fLeafs[Index].SubLeafs[1].EAX,3);  // preliminar
          AVX_VNNI           := GetBit(fLeafs[Index].SubLeafs[1].EAX,4);
          AVX512_BF16        := GetBit(fLeafs[Index].SubLeafs[1].EAX,5);
          LASS               := GetBit(fLeafs[Index].SubLeafs[1].EAX,6);
          CMPCCXADD          := GetBit(fLeafs[Index].SubLeafs[1].EAX,7);
          ARCH_PERFMON_EXT   := GetBit(fLeafs[Index].SubLeafs[1].EAX,8);
          REPMOV_FZ          := GetBit(fLeafs[Index].SubLeafs[1].EAX,10);
          REPSTOS_FS         := GetBit(fLeafs[Index].SubLeafs[1].EAX,11);
          REPCMPS_FS         := GetBit(fLeafs[Index].SubLeafs[1].EAX,12);
          FRED               := GetBit(fLeafs[Index].SubLeafs[1].EAX,17); // preliminar
          LKGS               := GetBit(fLeafs[Index].SubLeafs[1].EAX,18); // preliminar
          WRMSRNS            := GetBit(fLeafs[Index].SubLeafs[1].EAX,19);
          NMI_SRC            := GetBit(fLeafs[Index].SubLeafs[1].EAX,20); // preliminar
          AMX_FP16           := GetBit(fLeafs[Index].SubLeafs[1].EAX,21);
          HRESET             := GetBit(fLeafs[Index].SubLeafs[1].EAX,22);
          AVX_IFMA           := GetBit(fLeafs[Index].SubLeafs[1].EAX,23);
          LAM                := GetBit(fLeafs[Index].SubLeafs[1].EAX,26);
          MSRLIST            := GetBit(fLeafs[Index].SubLeafs[1].EAX,27);
          INVD_DIS_POST_BIOS := GetBit(fLeafs[Index].SubLeafs[1].EAX,30);
          MOVRS              := GetBit(fLeafs[Index].SubLeafs[1].EAX,31); // preliminar
        {EBX register}
          PPIN                := GetBit(fLeafs[Index].SubLeafs[1].EBX,0);
          PBNDKB              := GetBit(fLeafs[Index].SubLeafs[1].EBX,1); // preliminar
          CPUIDMAXVAL_LIM_RMV := GetBit(fLeafs[Index].SubLeafs[1].EBX,3);
        {ECX register}
          ASYM_RDTM      := GetBit(fLeafs[Index].SubLeafs[1].EBX,0); // preliminar
          ASYM_RDTA      := GetBit(fLeafs[Index].SubLeafs[1].EBX,1); // preliminar
          MSR_IMM        := GetBit(fLeafs[Index].SubLeafs[1].EBX,5); // preliminar
        {EDX register}
          AVX_VNNI_INT8  := GetBit(fLeafs[Index].SubLeafs[1].EDX,4);
          AVX_NE_CONVERT := GetBit(fLeafs[Index].SubLeafs[1].EDX,5);
          AMX_COMPLEX    := GetBit(fLeafs[Index].SubLeafs[1].EDX,8);  // preliminar
          AVX_VNNI_INT16 := GetBit(fLeafs[Index].SubLeafs[1].EDX,10);
          UTMR           := GetBit(fLeafs[Index].SubLeafs[1].EDX,13); // preliminar
          PREFETCHI      := GetBit(fLeafs[Index].SubLeafs[1].EDX,14);
          USER_MSR       := GetBit(fLeafs[Index].SubLeafs[1].EDX,15); // preliminar
          UIRET_UIF      := GetBit(fLeafs[Index].SubLeafs[1].EDX,17);
          CET_SSS        := GetBit(fLeafs[Index].SubLeafs[1].EDX,18);
          AVX10          := GetBit(fLeafs[Index].SubLeafs[1].EDX,19);
          APX_F          := GetBit(fLeafs[Index].SubLeafs[1].EDX,21); // preliminar
          MWAIT          := GetBit(fLeafs[Index].SubLeafs[1].EDX,23); // preliminar
          SLSM           := GetBit(fLeafs[Index].SubLeafs[1].EDX,24);
        end;
    If Length(fLeafs[Index].SubLeafs) > 2 then
      with fInfo.ProcessorFeatures do
        begin
        {EDX register}
          PSFD            := GetBit(fLeafs[Index].SubLeafs[2].EDX,0);
          IPRED_CTRL      := GetBit(fLeafs[Index].SubLeafs[2].EDX,1);
          RRSBA_CTRL      := GetBit(fLeafs[Index].SubLeafs[2].EDX,2);
          DDPD_U          := GetBit(fLeafs[Index].SubLeafs[2].EDX,3);
          BHI_CTRL        := GetBit(fLeafs[Index].SubLeafs[2].EDX,4);
          MCDT_NO         := GetBit(fLeafs[Index].SubLeafs[2].EDX,5);
          UC_LOCK_DIS     := GetBit(fLeafs[Index].SubLeafs[2].EDX,6);
          MONITOR_MITG_NO := GetBit(fLeafs[Index].SubLeafs[2].EDX,7);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_000B;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($0000000B,Index) then
  begin
    Temp := fLeafs[Index].Data;
    If GetBits(Temp.EBX,0,15) <> 0 then
      while GetBits(Temp.ECX,8,15) <> 0 do
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
          CPUID($B,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
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
    CPUID($D,1,fLeafs[Index].SubLeafs[1]);
    For i := 2 to 31 do
      If GetBit(fLeafs[Index].SubLeafs[0].EAX,i) or GetBit(fLeafs[Index].SubLeafs[1].ECX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($D,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
        end;
    For i := 0 to 31 do
      If GetBit(fLeafs[Index].SubLeafs[0].EDX,i) or GetBit(fLeafs[Index].SubLeafs[1].EDX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($D,UInt32(32 + i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
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
          CPUID($F,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
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
          CPUID($10,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0012;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($00000012,Index) then
  begin
    If fInfo.ProcessorFeatures.SGX then
      begin
        SetLength(fLeafs[Index].SubLeafs,2);
        fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
        CPUID($12,UInt32(1),fLeafs[Index].SubLeafs[1]);
        CPUID($12,UInt32(2),Temp);
        while GetBits(Temp.EAX,0,3) <> 0 do
          begin
            SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
            fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
            CPUID($12,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
          end;
      end;
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
      CPUID($14,UInt32(i),fLeafs[Index].SubLeafs[i]);
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
          CPUID($17,UInt32(i),fLeafs[Index].SubLeafs[i]);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0018;
var
  Index:  Integer;
  i:      Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($00000018,Index) then
  begin
    For i := 0 to fLeafs[Index].Data.EAX do
      begin
        CPUID($18,UInt32(i),Temp);
        If GetBits(Temp.EDX,0,4) <> 0 then
          begin
            SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
            fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
          end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_001A;
var
  Index:  Integer;
begin
If Find($0000001A,Index) then
  begin
    If fLeafs[Index].Data.EAX <> 0 then
      begin
        SetLength(fLeafs[Index].SubLeafs,1);
        fLeafs[Index].SubLeafs[0] := fLeafs[Index].Data;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_001B;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($0000001B,Index) then
  begin
    If fInfo.ProcessorFeatures.PCONFIG then
      begin
        Temp := fLeafs[Index].Data;
        while Temp.EAX <> 0 do
          begin
            SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
            fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
            CPUID($1B,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
          end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_001D;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($0000001D,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID($1D,UInt32(i),fLeafs[Index].SubLeafs[i]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_001E;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($0000001E,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID($1E,UInt32(i),fLeafs[Index].SubLeafs[i]);
    If Length(fLeafs[Index].SubLeafs) > 1 then
      with fInfo.ProcessorFeatures do
        begin
        {EAX register}
        {
          Following four feature flags were already loaded from their mirrors
          in leaf 7.

          AMX_INT8    := GetBit(fLeafs[Index].SubLeafs[1].EAX,0);
          AMX_BF16    := GetBit(fLeafs[Index].SubLeafs[1].EAX,1);
          AMX_COMPLEX := GetBit(fLeafs[Index].SubLeafs[1].EAX,2);
          AMX_FP16    := GetBit(fLeafs[Index].SubLeafs[1].EAX,3);
        }
          AMX_FP8     := GetBit(fLeafs[Index].SubLeafs[1].EAX,4);
          AMX_TF32    := GetBit(fLeafs[Index].SubLeafs[1].EAX,6);
          AMX_AVX512  := GetBit(fLeafs[Index].SubLeafs[1].EAX,7);
          AMX_MOVRS   := GetBit(fLeafs[Index].SubLeafs[1].EAX,8);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_001F;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($0000001F,Index) then
  begin
    Temp := fLeafs[Index].Data;
    If GetBits(Temp.EBX,0,15) <> 0 then
      while GetBits(Temp.ECX,8,15) <> 0 do
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
          CPUID($1F,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0020;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000020,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID($20,UInt32(i),fLeafs[Index].SubLeafs[i]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0023;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000023,Index) then
  begin
    For i := 0 to 31 do
      If GetBit(fLeafs[Index].Data.EAX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($23,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0024;
var
  Index:  Integer;
  i:      Integer;
begin
If Find($00000024,Index) then
  begin
    SetLength(fLeafs[Index].SubLeafs,fLeafs[Index].Data.EAX + 1);
    For i := Low(fLeafs[Index].SubLeafs) to High(fLeafs[Index].SubLeafs) do
      CPUID($24,UInt32(i),fLeafs[Index].SubLeafs[i]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0027;
var
  Index:  Integer;
  i:      Integer;
begin
// entire function must be run on the same processor (core)
If Find($00000027,Index) then
  begin
    For i := 1 to 31 do
      If GetBit(fLeafs[Index].Data.EDX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($27,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_0000_0028;
var
  Index:  Integer;
  i:      Integer;
begin
// entire function must be run on the same processor (core)
If Find($00000028,Index) then
  begin
    For i := 1 to 31 do
      If GetBit(fLeafs[Index].Data.EBX,i) then
        begin
          SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
          CPUID($28,UInt32(i),fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)]);
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadPhiLeafs;
begin
LoadLeafGroup($20000000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadHypLeafs;

  procedure AddHypLeaf(ID: UInt32);
  var
    LeafData: TCPUIDLeafData;
  begin
    CPUID(ID,LeafData);
    If not EqualsToHighestStdLeafData(LeafData) then
      begin
        SetLength(fLeafs,Length(fLeafs) + 1);
        fLeafs[HighIndex].ID := ID;
        fLeafs[HighIndex].Data := LeafData;
        fLeafs[HighIndex].SubLeafs := nil;
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

procedure TSimpleCPUID.LoadExtLeafs;
begin
LoadLeafGroup($80000000);
// process specific leafs
ProcessLeaf_8000_0001;
ProcessLeaf_8000_0002_to_8000_0004;
ProcessLeaf_8000_0007;
ProcessLeaf_8000_0008;
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
        AHF64       := GetBit(fLeafs[Index].Data.ECX,0);
        CMP         := GetBit(fLeafs[Index].Data.ECX,1);
        SVM         := GetBit(fLeafs[Index].Data.ECX,2);
        EAS         := GetBit(fLeafs[Index].Data.ECX,3);
        CR8D        := GetBit(fLeafs[Index].Data.ECX,4);
        LZCNT       := GetBit(fLeafs[Index].Data.ECX,5);
        SSE4A       := GetBit(fLeafs[Index].Data.ECX,6);
        MSSE        := GetBit(fLeafs[Index].Data.ECX,7);
        _3DNowP     := GetBit(fLeafs[Index].Data.ECX,8);
        OSVW        := GetBit(fLeafs[Index].Data.ECX,9);
        IBS         := GetBit(fLeafs[Index].Data.ECX,10);
        XOP         := GetBit(fLeafs[Index].Data.ECX,11);
        SKINIT      := GetBit(fLeafs[Index].Data.ECX,12);
        WDT         := GetBit(fLeafs[Index].Data.ECX,13);
        LWP         := GetBit(fLeafs[Index].Data.ECX,15);
        FMA4        := GetBit(fLeafs[Index].Data.ECX,16);
        TCE         := GetBit(fLeafs[Index].Data.ECX,17);
        NODEID      := GetBit(fLeafs[Index].Data.ECX,19);
        TBM         := GetBit(fLeafs[Index].Data.ECX,21);
        TOPX        := GetBit(fLeafs[Index].Data.ECX,22);
        PCX_CORE    := GetBit(fLeafs[Index].Data.ECX,23);
        PCX_NB      := GetBit(fLeafs[Index].Data.ECX,24);
        DBX         := GetBit(fLeafs[Index].Data.ECX,26);
        PERFTSC     := GetBit(fLeafs[Index].Data.ECX,27);
        PCX_L2I     := GetBit(fLeafs[Index].Data.ECX,28);
        MON         := GetBit(fLeafs[Index].Data.ECX,29);
        ADDRMASKEXT := GetBit(fLeafs[Index].Data.ECX,30);
      {EDX register}
        FPU         := GetBit(fLeafs[Index].Data.EDX,0);
        VME         := GetBit(fLeafs[Index].Data.EDX,1);
        DE          := GetBit(fLeafs[Index].Data.EDX,2);
        PSE         := GetBit(fLeafs[Index].Data.EDX,3);
        TSC         := GetBit(fLeafs[Index].Data.EDX,4);
        MSR         := GetBit(fLeafs[Index].Data.EDX,5);
        PAE         := GetBit(fLeafs[Index].Data.EDX,6);
        MCE         := GetBit(fLeafs[Index].Data.EDX,7);
        CX8         := GetBit(fLeafs[Index].Data.EDX,8);
        APIC        := GetBit(fLeafs[Index].Data.EDX,9);
        SEP         := GetBit(fLeafs[Index].Data.EDX,11);
        MTRR        := GetBit(fLeafs[Index].Data.EDX,12);
        PGE         := GetBit(fLeafs[Index].Data.EDX,13);
        MCA         := GetBit(fLeafs[Index].Data.EDX,14);
        CMOV        := GetBit(fLeafs[Index].Data.EDX,15);
        PAT         := GetBit(fLeafs[Index].Data.EDX,16);
        PSE36       := GetBit(fLeafs[Index].Data.EDX,17);
        MP          := GetBit(fLeafs[Index].Data.EDX,19);
        NX          := GetBit(fLeafs[Index].Data.EDX,20);
        MMXEXT      := GetBit(fLeafs[Index].Data.EDX,22);
        MMX         := GetBit(fLeafs[Index].Data.EDX,23);
        FXSR        := GetBit(fLeafs[Index].Data.EDX,24);
        FFXSR       := GetBit(fLeafs[Index].Data.EDX,25);
        PG1G        := GetBit(fLeafs[Index].Data.EDX,26);
        TSCP        := GetBit(fLeafs[Index].Data.EDX,27);
        LM          := GetBit(fLeafs[Index].Data.EDX,29);
        _3DNOWEXT   := GetBit(fLeafs[Index].Data.EDX,30);
        _3DNOW      := GetBit(fLeafs[Index].Data.EDX,31);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_0002_to_8000_0004;
type
  TBrandStringOverlay = packed array[0..2] of TCPUIDLeafData;
  PBrandStringOverlay = ^TBrandStringOverlay;
var
  Str:        AnsiString;
  StrOverlay: PBrandStringOverlay;
  i:          Integer;
  Index:      Integer;
begin
Str := '';
// get brand string
SetLength(Str,48);
StrOverlay := PBrandStringOverlay(PAnsiChar(Str));
For i := 0 to 2 do
  If not Find($80000002 + UInt32(i),Index) then
    begin
      fInfo.BrandString := '';
      Exit;
    end
  else StrOverlay^[i] := fLeafs[Index].Data;
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

procedure TSimpleCPUID.ProcessLeaf_8000_0008;
var
  Index:  Integer;
begin
If Find($80000008,Index) then
  with fInfo.ExtendedProcessorFeatures do
    begin
    {EBX register}
      CLZERO           := GetBit(fLeafs[Index].Data.EBX,0);
      INSTRSTCNTMSR    := GetBit(fLeafs[Index].Data.EBX,1);
      RSTRFPERRPTRS    := GetBit(fLeafs[Index].Data.EBX,2);
      INVLPGB          := GetBit(fLeafs[Index].Data.EBX,3);
      RDPRU            := GetBit(fLeafs[Index].Data.EBX,4);
      BE               := GetBit(fLeafs[Index].Data.EBX,6);
      MCOMMIT          := GetBit(fLeafs[Index].Data.EBX,8);
      WBNOINVD         := GetBit(fLeafs[Index].Data.EBX,9);
      IBPB             := GetBit(fLeafs[Index].Data.EBX,12);
      INT_WBINVD       := GetBit(fLeafs[Index].Data.EBX,13);
      IBRS             := GetBit(fLeafs[Index].Data.EBX,14);
      STIBP            := GetBit(fLeafs[Index].Data.EBX,15);
      IBRS_ALL         := GetBit(fLeafs[Index].Data.EBX,16);
      STIBP_ALL        := GetBit(fLeafs[Index].Data.EBX,17);
      IBRS_PREF        := GetBit(fLeafs[Index].Data.EBX,18);
      IBRS_SAMEMODE    := GetBit(fLeafs[Index].Data.EBX,19);
      EFER_LMSLE_UNSUP := GetBit(fLeafs[Index].Data.EBX,20);
      INVLPGB_NESTPGS  := GetBit(fLeafs[Index].Data.EBX,21);
      SSBD             := GetBit(fLeafs[Index].Data.EBX,24);
      SSBD_VSC         := GetBit(fLeafs[Index].Data.EBX,25);
      SSBD_NR          := GetBit(fLeafs[Index].Data.EBX,26);
      CPPC             := GetBit(fLeafs[Index].Data.EBX,27);
      PSFD             := GetBit(fLeafs[Index].Data.EBX,28);
      BTC_NO           := GetBit(fLeafs[Index].Data.EBX,29);
      IBPB_RET         := GetBit(fLeafs[Index].Data.EBX,30);
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_001D;
var
  Index:  Integer;
  Temp:   TCPUIDLeafData;
begin
If Find($8000001D,Index) and fInfo.ExtendedProcessorFeatures.TOPX then
  begin
    Temp := fLeafs[Index].Data;
    while GetBits(Temp.EAX,0,4) <> 0 do
      begin
        SetLength(fLeafs[Index].SubLeafs,Length(fLeafs[Index].SubLeafs) + 1);
        fLeafs[Index].SubLeafs[High(fLeafs[Index].SubLeafs)] := Temp;
        CPUID($8000001D,UInt32(Length(fLeafs[Index].SubLeafs)),Temp);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ProcessLeaf_8000_0021;
var
  Index:  Integer;
begin
If Find($80000021,Index) then
  with fInfo.ExtendedProcessorFeatures do
    begin
    {EAX register}
      NONESTDATABP    := GetBit(fLeafs[Index].Data.EAX,0);
      FSGSBW_NSER     := GetBit(fLeafs[Index].Data.EAX,1);
      LFENCE_SER      := GetBit(fLeafs[Index].Data.EAX,2);
      SMMPGCFGLOCK    := GetBit(fLeafs[Index].Data.EAX,3);
      NULLSELCLRBASE  := GetBit(fLeafs[Index].Data.EAX,6);
      UPADDRIGNORE    := GetBit(fLeafs[Index].Data.EAX,7);
      AUTOIBRS        := GetBit(fLeafs[Index].Data.EAX,8);
      NOSMMCTL        := GetBit(fLeafs[Index].Data.EAX,9);
      REPSTOS_FS      := GetBit(fLeafs[Index].Data.EAX,10);
      REPCMPS_FS      := GetBit(fLeafs[Index].Data.EAX,11);
      PMC2_PRECRET    := GetBit(fLeafs[Index].Data.EAX,12);
      PREFETCHCTLMSR  := GetBit(fLeafs[Index].Data.EAX,13);
      L2TLBSIZEX32    := GetBit(fLeafs[Index].Data.EAX,14);
      AMD_ERMSB       := GetBit(fLeafs[Index].Data.EAX,15);
      OP0F017RECL     := GetBit(fLeafs[Index].Data.EAX,16);
      CPUIDUSESDIS    := GetBit(fLeafs[Index].Data.EAX,17);
      EPSF            := GetBit(fLeafs[Index].Data.EAX,18);
      REPSCASB_FS     := GetBit(fLeafs[Index].Data.EAX,19);
      PREFETCHI       := GetBit(fLeafs[Index].Data.EAX,20);
      FP512_DOWNGD    := GetBit(fLeafs[Index].Data.EAX,21);
      ERAPS           := GetBit(fLeafs[Index].Data.EAX,24);
      SBPB            := GetBit(fLeafs[Index].Data.EAX,27);
      IBPB_BRTYPE     := GetBit(fLeafs[Index].Data.EAX,28);
      SRSO_NO         := GetBit(fLeafs[Index].Data.EAX,29);
      SRSO_USRKRNL_NO := GetBit(fLeafs[Index].Data.EAX,30);
      SRSO_MSR_FIX    := GetBit(fLeafs[Index].Data.EAX,31);
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadTNMLeafs;
begin
LoadLeafGroup($80860000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadCNTLeafs;
begin
LoadLeafGroup($C0000000);
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadAllLeafs;
begin
// load and parse all known leaf groups 
LoadStdLeafs;
LoadPhiLeafs;
LoadHypLeafs;
LoadExtLeafs;
LoadTNMLeafs;
LoadCNTLeafs;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.InitSupportedExtensions;
var
  Index:  Integer;
begin
with fInfo.SupportedExtensions do
  begin
    // FPU  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    X87         := fInfo.ProcessorFeatures.FPU;
    EmulatedX87 := GetBit(GetMSW,2);
    MMX         := fInfo.ProcessorFeatures.MMX and not EmulatedX87;

    // SSE  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
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
      SSE := fInfo.ProcessorFeatures.SSE and CanExecuteSSE
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
    GFNI      := fInfo.ProcessorFeatures.GFNI and SSE2; 

    // AVX  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    If fInfo.ProcessorFeatures.OSXSAVE then
      AVX := (GetXCR0L and $6 = $6) and fInfo.ProcessorFeatures.AVX
    else
      AVX := False;
    F16C          := fInfo.ProcessorFeatures.F16C and AVX;
    FMA           := fInfo.ProcessorFeatures.FMA and AVX;
    VAES128       := fInfo.ProcessorFeatures.AES and AVX;
    VPCLMULQDQ128 := fInfo.ProcessorFeatures.PCLMULQDQ and AVX;
    VAES          := fInfo.ProcessorFeatures.VAES and AVX;
    VPCLMULQDQ    := fInfo.ProcessorFeatures.VPCLMULQDQ and AVX;
    VGFNI         := fInfo.ProcessorFeatures.GFNI and AVX;

    // AVX2 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    AVX2 := fInfo.ProcessorFeatures.AVX2 and AVX;

    // AVX-512  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    If fInfo.ProcessorFeatures.OSXSAVE then
      AVX512.Supported := (GetXCR0L and $E6 = $E6) and fInfo.ProcessorFeatures.AVX512F
    else
      AVX512.Supported := False;
    If AVX512.Supported then
      begin
        AVX512.AVX512ER            := fInfo.ProcessorFeatures.AVX512ER;
        AVX512.AVX512PF            := fInfo.ProcessorFeatures.AVX512PF;
        AVX512.AVX512CD            := fInfo.ProcessorFeatures.AVX512CD;
        AVX512.AVX512DQ            := fInfo.ProcessorFeatures.AVX512DQ;
        AVX512.AVX512BW            := fInfo.ProcessorFeatures.AVX512BW;
        AVX512.AVX512VL            := fInfo.ProcessorFeatures.AVX512VL;
        AVX512.AVX512_VBMI         := fInfo.ProcessorFeatures.AVX512_VBMI;
        AVX512.AVX512_VBMI2        := fInfo.ProcessorFeatures.AVX512_VBMI2;
        AVX512.AVX512_IFMA         := fInfo.ProcessorFeatures.AVX512_IFMA;
        AVX512.AVX512_VNNI         := fInfo.ProcessorFeatures.AVX512_VNNI;
        AVX512.AVX512_BF16         := fInfo.ProcessorFeatures.AVX512_BF16;
        AVX512.AVX512_VPOPCNTDQ    := fInfo.ProcessorFeatures.AVX512_VPOPCNTDQ;
        AVX512.AVX512_BITALG       := fInfo.ProcessorFeatures.AVX512_BITALG;
        AVX512.AVX512_FP16         := fInfo.ProcessorFeatures.AVX512_FP16;
        AVX512.AVX512_4VNNIW       := fInfo.ProcessorFeatures.AVX512_4VNNIW;
        AVX512.AVX512_4FMAPS       := fInfo.ProcessorFeatures.AVX512_4FMAPS;
        AVX512.AVX512_VP2INTERSECT := fInfo.ProcessorFeatures.AVX512_VP2INTERSECT;
        AVX512.VAES                := fInfo.ProcessorFeatures.VAES;
        AVX512.VPCLMULQDQ          := fInfo.ProcessorFeatures.VPCLMULQDQ;
        AVX512.GFNI                := fInfo.ProcessorFeatures.GFNI;
      end;

    // AVX10    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    According to current documentation, it should suffice to test for AVX10
    feature flag and, unlike for AVX/AVX2, it is not necessary to also test
    OSXSAVE and selected bits in XCR0 register.
  }
    AVX10.Supported := fInfo.ProcessorFeatures.AVX10 and Find($24,Index);
    If AVX10.Supported then
      begin
        AVX10.Version := Byte(GetBits(fLeafs[Index].Data.EBX,0,7));
        If AVX10.Version >= 1 then
          begin
            AVX10.Vec128 := GetBit(fLeafs[Index].Data.EBX,16);
            AVX10.Vec256 := GetBit(fLeafs[Index].Data.EBX,17);
            AVX10.Vec512 := GetBit(fLeafs[Index].Data.EBX,18);
          end
        else AVX10.Supported := False;
      end;

    // AMX  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    AMX.Supported := fInfo.ProcessorFeatures.AMX_TILE;
    If AMX.Supported then
      begin
        AMX.AMX_INT8    := fInfo.ProcessorFeatures.AMX_INT8;
        AMX.AMX_BF16    := fInfo.ProcessorFeatures.AMX_BF16;
        AMX.AMX_COMPLEX := fInfo.ProcessorFeatures.AMX_COMPLEX;
        AMX.AMX_FP16    := fInfo.ProcessorFeatures.AMX_FP16;
        AMX.AMX_FP8     := fInfo.ProcessorFeatures.AMX_FP8;
        AMX.AMX_TF32    := fInfo.ProcessorFeatures.AMX_TF32;
        AMX.AMX_AVX512  := fInfo.ProcessorFeatures.AMX_AVX512;
        AMX.AMX_MOVRS   := fInfo.ProcessorFeatures.AMX_MOVRS;
      end;

    // APX  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    APX.Supported := fInfo.ProcessorFeatures.APX_F;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.ClearInfo;
begin
fLoaded := False;
SetLength(fLeafs,0);  // subleaf arrays are cleared automatically
fInfo.ManufacturerIDString := '';
fInfo.BrandString := '';
FillChar(fInfo,SizeOf(TCPUIDInfo),0);
fHiStdLeaf := -1;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.Initialize(LoadInfo: Boolean);
begin
fSupported := SimpleCPUID.CPUIDSupported;
If LoadInfo then
  Self.LoadInfo // calls ClearInfo
else
  ClearInfo;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.Finalize;
begin
ClearInfo;
end;

//------------------------------------------------------------------------------

class Function TSimpleCPUID.SameLeafData(A,B: TCPUIDLeafData): Boolean;
begin
Result := (A.EAX = B.EAX) and (A.EBX = B.EBX) and (A.ECX = B.ECX) and (A.EDX = B.EDX);
end;

//------------------------------------------------------------------------------

Function TSimpleCPUID.EqualsToHighestStdLeafData(LeafData: TCPUIDLeafData): Boolean;
begin
If CheckIndex(fHiStdLeaf) then
  Result := SameLeafData(LeafData,fLeafs[fHiStdLeaf].Data)
else
  Result := False;
end;

{-------------------------------------------------------------------------------
    TSimpleCPUID - public methods
-------------------------------------------------------------------------------}

constructor TSimpleCPUID.Create(LoadInfo: Boolean = True);
begin
inherited Create;
Initialize(LoadInfo);
end;

//------------------------------------------------------------------------------

destructor TSimpleCPUID.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleCPUID.LoadInfo;
begin
ClearInfo;
If fSupported then
  begin
    LoadAllLeafs;
    // resolve supported extensions
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
  raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity: Failed to set thread affinity mask (%d).',[Integer(GetLastError)]);
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
      raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity: Failed to set thread affinity mask (%d).',[Integer(GetLastError)]);
  end
else raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.SetThreadAffinity: Failed to get thread affinity mask (%d).',[Integer(GetLastError)]);
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
      raise ESCIDSystemError.CreateFmt('TSimpleCPUIDEx.ProcessorAvailable: Failed to get process affinity mask (%d).',[Integer(GetLastError)]);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

constructor TSimpleCPUIDEx.Create(ProcessorID: Integer = 0; LoadInfo: Boolean = True);
begin
inherited Create(False);
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
