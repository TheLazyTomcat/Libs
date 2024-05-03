{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Windows version

    This library provides WinAPI functions, constants and types used to obtain
    and/or verify version of Windows operating system the program is currently
    running on.

    It also implements and provides version helper functions and macros along
    with some utility functions.

  Version 1.2.1 (2024-05-03)

  Last change 2024-05-03

  ©2021-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.WindowsVersion

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol WindowsVersion_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit WindowsVersion;
{
  WindowsVersion_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  WindowsVersion_UseAuxExceptions to achieve this.
}
{$IF Defined(WindowsVersion_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Windows,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EWVException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EWVSystemError = class(EWVException);

{===============================================================================
--------------------------------------------------------------------------------
                               Windows NT version
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Windows NT version - constants
===============================================================================}
{
  Windows NT version constants.

  High byte contains major version, low byte contains minor version.

  Can be used eg. in calls to VerifyVersionInfo to check which OS is the
  program running on.

  To get major version, use function MajorVersion.
  
  To get minor version, use function MinorVersion.
}
const
  WIN32_WINNT_NT4          = $0400;
  WIN32_WINNT_WIN2K        = $0500;
  WIN32_WINNT_WINXP        = $0501;
  WIN32_WINNT_WS03         = $0502; // also Windows XP 64bit
  WIN32_WINNT_WIN6         = $0600;
  WIN32_WINNT_VISTA        = $0600;
  WIN32_WINNT_WS08         = $0600;
  WIN32_WINNT_LONGHORN     = $0600;
  WIN32_WINNT_WIN7         = $0601;
  WIN32_WINNT_WIN8         = $0602;
  WIN32_WINNT_WINBLUE      = $0603; // Windows 8.1
  WIN32_WINNT_WINTHRESHOLD = $0A00;
  WIN32_WINNT_WIN10        = $0A00;

{===============================================================================
    Windows NT version - functions declaration
===============================================================================}

// functions extracting major and minor version from full version
Function MajorVersion(Version: UInt16): UInt8;
Function MinorVersion(Version: UInt16): UInt8;

{===============================================================================
--------------------------------------------------------------------------------
                              Windows version info
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Windows version info - constants
===============================================================================}
{
  type mask bits

  For use in dwTypeMask parameter in function VerifyVersionInfo (multiple type
  mask bits can be OR-ed into this parameter) or in parameter TypeMask in
  function VerSetConditionMask (only one value).
}
const
  VER_MINORVERSION     = $00000001;
  VER_MAJORVERSION     = $00000002;
  VER_BUILDNUMBER      = $00000004;
  VER_PLATFORMID       = $00000008;
  VER_SERVICEPACKMINOR = $00000010;
  VER_SERVICEPACKMAJOR = $00000020;
  VER_SUITENAME        = $00000040;
  VER_PRODUCT_TYPE     = $00000080;

//------------------------------------------------------------------------------
{
  platform id

  Possible values of dwPlatformId field in TOSVersionInfoEx structure.
}
const
  VER_PLATFORM_WIN32s        = 0;
  VER_PLATFORM_WIN32_WINDOWS = 1;
  VER_PLATFORM_WIN32_NT      = 2;

//------------------------------------------------------------------------------
{
  suite mask

  Possible values of wSuiteMask field in TOSVersionInfoEx structure (can be
  a combination of multiple values).
}
const
  VER_SERVER_NT                      = $80000000;
  VER_WORKSTATION_NT                 = $40000000;
  VER_SUITE_SMALLBUSINESS            = $00000001;
  VER_SUITE_ENTERPRISE               = $00000002;
  VER_SUITE_BACKOFFICE               = $00000004;
  VER_SUITE_COMMUNICATIONS           = $00000008;
  VER_SUITE_TERMINAL                 = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  VER_SUITE_EMBEDDEDNT               = $00000040;
  VER_SUITE_DATACENTER               = $00000080;
  VER_SUITE_SINGLEUSERTS             = $00000100;
  VER_SUITE_PERSONAL                 = $00000200;
  VER_SUITE_BLADE                    = $00000400;
  VER_SUITE_EMBEDDED_RESTRICTED      = $00000800;
  VER_SUITE_SECURITY_APPLIANCE       = $00001000;
  VER_SUITE_STORAGE_SERVER           = $00002000;
  VER_SUITE_COMPUTE_SERVER           = $00004000;
  VER_SUITE_WH_SERVER                = $00008000;
  VER_SUITE_MULTIUSERTS              = $00020000;

//------------------------------------------------------------------------------
{
  product type

  Possible values of field wProductType in TOSVersionInfoEx structure.
}
const
  VER_NT_WORKSTATION       = $00000001;
  VER_NT_DOMAIN_CONTROLLER = $00000002;
  VER_NT_SERVER            = $00000003;

//------------------------------------------------------------------------------
{
  condition codes

  For use in parameter Condition in function VerSetConditionMask.

    WARINING - to build dwlConditionMask parameter for use in function
               VerifyVersionInfo, use VerSetConditionMask macro functions, do
               not combine them using logical operators.
}
const
  VER_EQUAL         = 1;
  VER_GREATER       = 2;
  VER_GREATER_EQUAL = 3;
  VER_LESS          = 4;
  VER_LESS_EQUAL    = 5;
  VER_AND           = 6;
  VER_OR            = 7;

  VER_CONDITION_MASK              = 7;
  VER_NUM_BITS_PER_CONDITION_MASK = 3;

{===============================================================================
    Windows version info - structures
===============================================================================}

type
  TOSVersionInfoA = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of AnsiChar;
  end;
  POSVersionInfoA = ^TOSVersionInfoA;

  TOSVersionInfoW = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of WideChar;
  end;
  POSVersionInfoW = ^TOSVersionInfoW;

{$IFDEF Unicode}
  TOSVersionInfo = TOSVersionInfoW;
{$ELSE}
  TOSVersionInfo = TOSVersionInfoA;
{$ENDIF}
  POSVersionInfo = ^TOSVersionInfo;

//------------------------------------------------------------------------------

type
  TOSVersionInfoExA = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of AnsiChar;
    wServicePackMajor:    Word;
    wServicePackMinor:    Word;
    wSuiteMask:           Word;
    wProductType:         Byte;
    wReserved:            Byte;
  end;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  TOSVersionInfoExW = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of WideChar;
    wServicePackMajor:    Word;
    wServicePackMinor:    Word;
    wSuiteMask:           Word;
    wProductType:         Byte;
    wReserved:            Byte;
  end;
  POSVersionInfoExW = ^TOSVersionInfoExW;

{$IFDEF Unicode}
  TOSVersionInfoEx = TOSVersionInfoExW;
{$ELSE}
  TOSVersionInfoEx = TOSVersionInfoExA;
{$ENDIF}
  POSVersionInfoEx = ^TOSVersionInfoEx;

{===============================================================================
    Windows version info - external functions
===============================================================================}

Function GetVersionExW(lpVersionInfo: POSVersionInfoExW): BOOL; stdcall; external kernel32;
Function GetVersionExA(lpVersionInfo: POSVersionInfoExA): BOOL; stdcall; external kernel32;
Function GetVersionEx(lpVersionInfo: POSVersionInfoEx): BOOL; overload; stdcall;
  external kernel32 name{$IFDEF Unicode}'GetVersionExW'{$ELSE}'GetVersionExA'{$ENDIF};

Function VerifyVersionInfoW(lpVersionInfo: POSVersionInfoExW; dwTypeMask: DWORD; dwlConditionMask: UInt64): BOOL; stdcall; external kernel32;
Function VerifyVersionInfoA(lpVersionInfo: POSVersionInfoExA; dwTypeMask: DWORD; dwlConditionMask: UInt64): BOOL; stdcall; external kernel32;
Function VerifyVersionInfo(lpVersionInfo: POSVersionInfoEx; dwTypeMask: DWORD; dwlConditionMask: UInt64): BOOL; overload; stdcall;
  external kernel32 name{$IFDEF Unicode}'VerifyVersionInfoW'{$ELSE}'VerifyVersionInfoA'{$ENDIF};

Function VerSetConditionMask(ConditionMask: UInt64; TypeMask: DWORD; Condition: Byte): UInt64; stdcall; external kernel32;

{===============================================================================
    Windows version info - wrapper functions declaration
===============================================================================}

Function GetVersionEx(out VersionInfo: TOSVersionInfo): Boolean; overload;
Function GetVersionEx(out VersionInfo: TOSVersionInfoEx): Boolean; overload;

Function VerifyVersionInfo(VersionInfo: TOSVersionInfoEx; TypeMask: DWORD; ConditionMask: UInt64): Boolean; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                  Product info
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Product info - product types
===============================================================================}
const
  PRODUCT_UNDEFINED                            = $00000000;

  PRODUCT_ULTIMATE                             = $00000001;
  PRODUCT_HOME_BASIC                           = $00000002;
  PRODUCT_HOME_PREMIUM                         = $00000003;
  PRODUCT_ENTERPRISE                           = $00000004;
  PRODUCT_HOME_BASIC_N                         = $00000005;
  PRODUCT_BUSINESS                             = $00000006;
  PRODUCT_STANDARD_SERVER                      = $00000007;
  PRODUCT_DATACENTER_SERVER                    = $00000008;
  PRODUCT_SMALLBUSINESS_SERVER                 = $00000009;
  PRODUCT_ENTERPRISE_SERVER                    = $0000000A;
  PRODUCT_STARTER                              = $0000000B;
  PRODUCT_DATACENTER_SERVER_CORE               = $0000000C;
  PRODUCT_STANDARD_SERVER_CORE                 = $0000000D;
  PRODUCT_ENTERPRISE_SERVER_CORE               = $0000000E;
  PRODUCT_ENTERPRISE_SERVER_IA64               = $0000000F;
  PRODUCT_BUSINESS_N                           = $00000010;
  PRODUCT_WEB_SERVER                           = $00000011;
  PRODUCT_CLUSTER_SERVER                       = $00000012;
  PRODUCT_HOME_SERVER                          = $00000013;
  PRODUCT_STORAGE_EXPRESS_SERVER               = $00000014;
  PRODUCT_STORAGE_STANDARD_SERVER              = $00000015;
  PRODUCT_STORAGE_WORKGROUP_SERVER             = $00000016;
  PRODUCT_STORAGE_ENTERPRISE_SERVER            = $00000017;
  PRODUCT_SERVER_FOR_SMALLBUSINESS             = $00000018;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM         = $00000019;
  PRODUCT_HOME_PREMIUM_N                       = $0000001A;
  PRODUCT_ENTERPRISE_N                         = $0000001B;
  PRODUCT_ULTIMATE_N                           = $0000001C;
  PRODUCT_WEB_SERVER_CORE                      = $0000001D;
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT     = $0000001E;
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY       = $0000001F;
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING      = $00000020;
  PRODUCT_SERVER_FOUNDATION                    = $00000021;
  PRODUCT_HOME_PREMIUM_SERVER                  = $00000022;
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V           = $00000023;
  PRODUCT_STANDARD_SERVER_V                    = $00000024;
  PRODUCT_DATACENTER_SERVER_V                  = $00000025;
  PRODUCT_ENTERPRISE_SERVER_V                  = $00000026;
  PRODUCT_DATACENTER_SERVER_CORE_V             = $00000027;
  PRODUCT_STANDARD_SERVER_CORE_V               = $00000028;
  PRODUCT_ENTERPRISE_SERVER_CORE_V             = $00000029;
  PRODUCT_HYPERV                               = $0000002A;
  PRODUCT_STORAGE_EXPRESS_SERVER_CORE          = $0000002B;
  PRODUCT_STORAGE_STANDARD_SERVER_CORE         = $0000002C;
  PRODUCT_STORAGE_WORKGROUP_SERVER_CORE        = $0000002D;
  PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE       = $0000002E;
  PRODUCT_STARTER_N                            = $0000002F;
  PRODUCT_PROFESSIONAL                         = $00000030;
  PRODUCT_PROFESSIONAL_N                       = $00000031;
  PRODUCT_SB_SOLUTION_SERVER                   = $00000032;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS              = $00000033;
  PRODUCT_STANDARD_SERVER_SOLUTIONS            = $00000034;
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE       = $00000035;
  PRODUCT_SB_SOLUTION_SERVER_EM                = $00000036;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM           = $00000037;
  PRODUCT_SOLUTION_EMBEDDEDSERVER              = $00000038;
  PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE         = $00000039;
  PRODUCT_PROFESSIONAL_EMBEDDED                = $0000003A;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT        = $0000003B;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL        = $0000003C;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC     = $0000003D;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC     = $0000003E;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE    = $0000003F;
  PRODUCT_CLUSTER_SERVER_V                     = $00000040;
  PRODUCT_EMBEDDED                             = $00000041;
  PRODUCT_STARTER_E                            = $00000042;
  PRODUCT_HOME_BASIC_E                         = $00000043;
  PRODUCT_HOME_PREMIUM_E                       = $00000044;
  PRODUCT_PROFESSIONAL_E                       = $00000045;
  PRODUCT_ENTERPRISE_E                         = $00000046;
  PRODUCT_ULTIMATE_E                           = $00000047;
  PRODUCT_ENTERPRISE_EVALUATION                = $00000048;
  PRODUCT_MULTIPOINT_STANDARD_SERVER           = $0000004C;
  PRODUCT_MULTIPOINT_PREMIUM_SERVER            = $0000004D;
  PRODUCT_STANDARD_EVALUATION_SERVER           = $0000004F;
  PRODUCT_DATACENTER_EVALUATION_SERVER         = $00000050;
  PRODUCT_ENTERPRISE_N_EVALUATION              = $00000054;
  PRODUCT_EMBEDDED_AUTOMOTIVE                  = $00000055;
  PRODUCT_EMBEDDED_INDUSTRY_A                  = $00000056;
  PRODUCT_THINPC                               = $00000057;
  PRODUCT_EMBEDDED_A                           = $00000058;
  PRODUCT_EMBEDDED_INDUSTRY                    = $00000059;
  PRODUCT_EMBEDDED_E                           = $0000005A;
  PRODUCT_EMBEDDED_INDUSTRY_E                  = $0000005B;
  PRODUCT_EMBEDDED_INDUSTRY_A_E                = $0000005C;
  PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER  = $0000005F;
  PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER   = $00000060;
  PRODUCT_CORE_ARM                             = $00000061;
  PRODUCT_CORE_N                               = $00000062;
  PRODUCT_CORE_COUNTRYSPECIFIC                 = $00000063;
  PRODUCT_CORE_SINGLELANGUAGE                  = $00000064;
  PRODUCT_CORE                                 = $00000065;
  PRODUCT_PROFESSIONAL_WMC                     = $00000067;
{
  PRODUCT_MOBILE_CORE was removed in newer SDK - version 10.0.22000.0, and
  probably older ones too, definitely does not contain it.
}
  PRODUCT_MOBILE_CORE                          = $00000068 deprecated;
  PRODUCT_EMBEDDED_INDUSTRY_EVAL               = $00000069;
  PRODUCT_EMBEDDED_INDUSTRY_E_EVAL             = $0000006A;
  PRODUCT_EMBEDDED_EVAL                        = $0000006B;
  PRODUCT_EMBEDDED_E_EVAL                      = $0000006C;
  PRODUCT_NANO_SERVER                          = $0000006D;
  PRODUCT_CLOUD_STORAGE_SERVER                 = $0000006E;
  PRODUCT_CORE_CONNECTED                       = $0000006F;
  PRODUCT_PROFESSIONAL_STUDENT                 = $00000070;
  PRODUCT_CORE_CONNECTED_N                     = $00000071;
  PRODUCT_PROFESSIONAL_STUDENT_N               = $00000072;
  PRODUCT_CORE_CONNECTED_SINGLELANGUAGE        = $00000073;
  PRODUCT_CORE_CONNECTED_COUNTRYSPECIFIC       = $00000074;
  PRODUCT_CONNECTED_CAR                        = $00000075;
  PRODUCT_INDUSTRY_HANDHELD                    = $00000076;
  PRODUCT_PPI_PRO                              = $00000077;
  PRODUCT_ARM64_SERVER                         = $00000078;
  PRODUCT_EDUCATION                            = $00000079;
  PRODUCT_EDUCATION_N                          = $0000007A;
  PRODUCT_IOTUAP                               = $0000007B;
  PRODUCT_CLOUD_HOST_INFRASTRUCTURE_SERVER     = $0000007C;
  PRODUCT_ENTERPRISE_S                         = $0000007D;
  PRODUCT_ENTERPRISE_S_N                       = $0000007E;
  PRODUCT_PROFESSIONAL_S                       = $0000007F;
  PRODUCT_PROFESSIONAL_S_N                     = $00000080;
  PRODUCT_ENTERPRISE_S_EVALUATION              = $00000081;
  PRODUCT_ENTERPRISE_S_N_EVALUATION            = $00000082;
  PRODUCT_HOLOGRAPHIC                          = $00000087; 
  PRODUCT_HOLOGRAPHIC_BUSINESS                 = $00000088;
  PRODUCT_PRO_SINGLE_LANGUAGE                  = $0000008A;
  PRODUCT_PRO_CHINA                            = $0000008B;
  PRODUCT_ENTERPRISE_SUBSCRIPTION              = $0000008C;
  PRODUCT_ENTERPRISE_SUBSCRIPTION_N            = $0000008D;
  PRODUCT_DATACENTER_NANO_SERVER               = $0000008F;
  PRODUCT_STANDARD_NANO_SERVER                 = $00000090;
  PRODUCT_DATACENTER_A_SERVER_CORE             = $00000091;
  PRODUCT_STANDARD_A_SERVER_CORE               = $00000092;
  PRODUCT_DATACENTER_WS_SERVER_CORE            = $00000093;
  PRODUCT_STANDARD_WS_SERVER_CORE              = $00000094;
  PRODUCT_UTILITY_VM                           = $00000095;
  PRODUCT_DATACENTER_EVALUATION_SERVER_CORE    = $0000009F;
  PRODUCT_STANDARD_EVALUATION_SERVER_CORE      = $000000A0;
  PRODUCT_PRO_WORKSTATION                      = $000000A1;
  PRODUCT_PRO_WORKSTATION_N                    = $000000A2;
  PRODUCT_PRO_FOR_EDUCATION                    = $000000A4;
  PRODUCT_PRO_FOR_EDUCATION_N                  = $000000A5;
  PRODUCT_AZURE_SERVER_CORE                    = $000000A8;
  PRODUCT_AZURE_NANO_SERVER                    = $000000A9;
  PRODUCT_ENTERPRISEG                          = $000000AB;
  PRODUCT_ENTERPRISEGN                         = $000000AC;
  PRODUCT_SERVERRDSH                           = $000000AF;
  PRODUCT_CLOUD                                = $000000B2;
  PRODUCT_CLOUDN                               = $000000B3;
  PRODUCT_HUBOS                                = $000000B4;
  PRODUCT_ONECOREUPDATEOS                      = $000000B6;
  PRODUCT_CLOUDE                               = $000000B7;
  PRODUCT_IOTOS                                = $000000B9;
  PRODUCT_CLOUDEN                              = $000000BA;
  PRODUCT_IOTEDGEOS                            = $000000BB;
  PRODUCT_IOTENTERPRISE                        = $000000BC;
  PRODUCT_LITE                                 = $000000BD;
  PRODUCT_IOTENTERPRISES                       = $000000BF;
  PRODUCT_XBOX_SYSTEMOS                        = $000000C0;
  PRODUCT_XBOX_NATIVEOS                        = $000000C1;
  PRODUCT_XBOX_GAMEOS                          = $000000C2;
  PRODUCT_XBOX_ERAOS                           = $000000C3;
  PRODUCT_XBOX_DURANGOHOSTOS                   = $000000C4;
  PRODUCT_XBOX_SCARLETTHOSTOS                  = $000000C5;
  PRODUCT_AZURE_SERVER_CLOUDHOST               = $000000C7;
  PRODUCT_AZURE_SERVER_CLOUDMOS                = $000000C8;
  PRODUCT_CLOUDEDITIONN                        = $000000CA;
  PRODUCT_CLOUDEDITION                         = $000000CB;
  PRODUCT_AZURESTACKHCI_SERVER_CORE            = $00000196;
  PRODUCT_DATACENTER_SERVER_AZURE_EDITION      = $00000197;
  PRODUCT_DATACENTER_SERVER_CORE_AZURE_EDITION = $00000198;

  PRODUCT_UNLICENSED                           = $ABCDABCD;

{===============================================================================
    Product info - functions declaration
===============================================================================}
{
  GetProductInfo if wrapper for function of the same name residing kernel32.
  This function was introduced in Windows Vista, but I wanted the code to work
  on WinXP too.
  So in systems where this function is not present, it will alvays return true
  and ReturnedProductType will be set to PRODUCT_UNDEFINED.
}
Function GetProductInfo(OSMajorVersion,OSMinorVersion,SpMajorVersion,SpMinorVersion: DWORD; out ReturnedProductType: DWORD): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                 Version helper
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Version helper - functions declaration
===============================================================================}
{
  Implementation of version helper functions and macros from Windows SDK file
  VersionHelpers.h.
}
Function IsWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean; overload;
Function IsWindowsVersion(MajorVersion,MinorVersion: Word): Boolean; overload;
Function IsWindowsVersion(MajorVersion: Word): Boolean; overload;

Function IsWindowsVersionOrLess(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean; overload;
Function IsWindowsVersionOrLess(MajorVersion,MinorVersion: Word): Boolean; overload;
Function IsWindowsVersionOrLess(MajorVersion: Word): Boolean; overload;

Function IsWindowsVersionOrGreater(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean; overload;
Function IsWindowsVersionOrGreater(MajorVersion,MinorVersion: Word): Boolean; overload;
Function IsWindowsVersionOrGreater(MajorVersion: Word): Boolean; overload;

Function IsLesserWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean; overload;
Function IsLesserWindowsVersion(MajorVersion,MinorVersion: Word): Boolean; overload;
Function IsLesserWindowsVersion(MajorVersion: Word): Boolean; overload;

Function IsGreaterWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean; overload;
Function IsGreaterWindowsVersion(MajorVersion,MinorVersion: Word): Boolean; overload;
Function IsGreaterWindowsVersion(MajorVersion: Word): Boolean; overload;

{
  WARNING - Windows XP 64bit is completely different system than WinXP 32bit.
            IsWindowsXPSP3OrGreater will return true on 64bit XP even when only
            SP2 (or lower) is installed, that is because this system has
            version 5.2 (it is based on Windows Server 2003), which is above
            5.1 for 32bit XP.

  NOTE - From windows 8.1 up, an application must explicitly manifest support
         for a given system to get real Windows version, otherwise the system
         will identify itself as Windows 8.

  NOTE - Windows 11 are reporting themselves as Windows 10.
}
Function IsWindowsXPOrGreater: Boolean;
Function IsWindowsXPSP1OrGreater: Boolean;
Function IsWindowsXPSP2OrGreater: Boolean;
Function IsWindowsXPSP3OrGreater: Boolean;
Function IsWindowsVistaOrGreater: Boolean;
Function IsWindowsVistaSP1OrGreater: Boolean;
Function IsWindowsVistaSP2OrGreater: Boolean;
Function IsWindows7OrGreater: Boolean;
Function IsWindows7SP1OrGreater: Boolean;
Function IsWindows8OrGreater: Boolean;
Function IsWindows8Point1OrGreater: Boolean;
Function IsWindowsThresholdOrGreater: Boolean;
Function IsWindows10OrGreater: Boolean;
{
  WARNING - Windows 11 cannot, at this time (december 2021), be differentiated
            from Windows 10 without using some bullshit strings parsing.
            Thanks Microsoft!
            IsWindows11OrGreater is checking build number (>=22000), but this
            is not documented and probably not future-proof.
}
Function IsWindows11OrGreater: Boolean;
Function IsWindowsServer: Boolean;

Function IsActiveSessionCountLimited: Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                 Other utilities
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Other utilities - functions declaration
===============================================================================}

Function IsWoW64Process(ProcessHandle: THandle): Boolean;
Function IsRunningUnderWoW64(ProcessID: DWORD): Boolean; overload;
Function IsRunningUnderWoW64: Boolean; overload;  // for current process

//------------------------------------------------------------------------------
{
  To be used to check whether a particular function is present in a given DLL.
  Intended to check functions in system libraries, but can be used to check
  presence of a function in any DLL.
}
Function FunctionIsPresent(const LibraryName,FunctionName: String): Boolean;

//------------------------------------------------------------------------------
{
  Following functions are checking system properties using metrics.
}
Function IsServerR2: Boolean;             // Windows Server 2003 R2
Function IsMediaCenterEdition: Boolean;   // Windows XP Media Center Edition
Function IsStarterEdition: Boolean;       // Windows XP Starter Edition
Function IsTabletPCEdition: Boolean;      // Windows XP Tablet PC Edition

implementation

uses
  StrRect;

{===============================================================================
--------------------------------------------------------------------------------
                               Windows NT version
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Windows NT version - functions implementation
===============================================================================}

Function MajorVersion(Version: UInt16): UInt8;
begin
Result := (Version shr 8) and $FF;
end;

//------------------------------------------------------------------------------

Function MinorVersion(Version: UInt16): UInt8;
begin
Result := Version and $FF;
end;

{===============================================================================
--------------------------------------------------------------------------------
                              Windows version info
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Windows version info - wrapper functions implementation
===============================================================================}

Function GetVersionEx(out VersionInfo: TOSVersionInfo): Boolean;
begin
FillChar(Addr(VersionInfo)^,SizeOf(VersionInfo),0);
VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
Result := GetVersionEx(@VersionInfo);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetVersionEx(out VersionInfo: TOSVersionInfoEx): Boolean;
begin
FillChar(Addr(VersionInfo)^,SizeOf(VersionInfo),0);
VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
Result := GetVersionEx(@VersionInfo);
end;

//------------------------------------------------------------------------------

Function VerifyVersionInfo(VersionInfo: TOSVersionInfoEx; TypeMask: DWORD; ConditionMask: UInt64): Boolean;
var
  LastError:  DWORD;
begin
VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
If not VerifyVersionInfo(@VersionInfo,TypeMask,ConditionMask) then
  begin
    LastError := GetLastError;
    If LastError = ERROR_OLD_WIN_VERSION then
      Result := False
    else
      raise EWVSystemError.CreateFmt('VerifyVersionInfo: Failed to verify version info (%.8x).',[LastError]);
  end
else Result := True;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  Product info
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Product info - functions implementation
===============================================================================}

Function GetProductInfo(OSMajorVersion,OSMinorVersion,SpMajorVersion,SpMinorVersion: DWORD; out ReturnedProductType: DWORD): Boolean;
type
  TGetProductInfoFce = Function(dwOSMajorVersion: DWORD; dwOSMinorVersion: DWORD; dwSpMajorVersion:
    DWORD; dwSpMinorVersion: DWORD; pdwReturnedProductType: PDWORD): BOOL; stdcall;
var
  LibHandle:          THandle;
  GetProductInfoFce:  TGetProductInfoFce;
begin
LibHandle := LoadLibrary(kernel32);
If LibHandle <> 0 then
  try
    GetProductInfoFce := TGetProductInfoFce(GetProcAddress(LibHandle,'GetProductInfo'));
    If not Assigned(GetProductInfoFce) then
      begin
        ReturnedProductType := PRODUCT_UNDEFINED;
        Result := True;
      end
    else Result := GetProductInfoFce(OSMajorVersion,OSMinorVersion,SpMajorVersion,SpMinorVersion,@ReturnedProductType);
  finally
    FreeLibrary(LibHandle);
  end
else raise EWVSystemError.CreateFmt('GetProductInfo: Cannot load kernel32 library (%.8x).',[GetLastError]);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Version helper
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Version helper - functions implementation
===============================================================================}

Function IsWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
OSVersion.wServicePackMajor := ServicePackMajor;
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_EQUAL),
    VER_MINORVERSION,VER_EQUAL),
  VER_SERVICEPACKMAJOR,VER_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersion(MajorVersion,MinorVersion: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
ConditionMask := VerSetConditionMask(VerSetConditionMask(0,VER_MAJORVERSION,VER_EQUAL),VER_MINORVERSION,VER_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersion(MajorVersion: Word): Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION,VerSetConditionMask(0,VER_MAJORVERSION,VER_EQUAL));
end;

//------------------------------------------------------------------------------

Function IsWindowsVersionOrLess(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
OSVersion.wServicePackMajor := ServicePackMajor;
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS_EQUAL),
    VER_MINORVERSION,VER_LESS_EQUAL),
  VER_SERVICEPACKMAJOR,VER_LESS_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersionOrLess(MajorVersion,MinorVersion: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
ConditionMask := VerSetConditionMask(VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS_EQUAL),VER_MINORVERSION,VER_LESS_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersionOrLess(MajorVersion: Word): Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION,VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS_EQUAL));
end;

//------------------------------------------------------------------------------

Function IsWindowsVersionOrGreater(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
OSVersion.wServicePackMajor := ServicePackMajor;
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER_EQUAL),
    VER_MINORVERSION,VER_GREATER_EQUAL),
  VER_SERVICEPACKMAJOR,VER_GREATER_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersionOrGreater(MajorVersion,MinorVersion: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
ConditionMask := VerSetConditionMask(VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER_EQUAL),VER_MINORVERSION,VER_GREATER_EQUAL);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsWindowsVersionOrGreater(MajorVersion: Word): Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION,VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER_EQUAL));
end;

//------------------------------------------------------------------------------

Function IsLesserWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
OSVersion.wServicePackMajor := ServicePackMajor;
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS),
    VER_MINORVERSION,VER_LESS),
  VER_SERVICEPACKMAJOR,VER_LESS);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsLesserWindowsVersion(MajorVersion,MinorVersion: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
ConditionMask := VerSetConditionMask(VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS),VER_MINORVERSION,VER_LESS);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsLesserWindowsVersion(MajorVersion: Word): Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION,VerSetConditionMask(0,VER_MAJORVERSION,VER_LESS));
end;

//------------------------------------------------------------------------------

Function IsGreaterWindowsVersion(MajorVersion,MinorVersion,ServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
OSVersion.wServicePackMajor := ServicePackMajor;
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER),
    VER_MINORVERSION,VER_GREATER),
  VER_SERVICEPACKMAJOR,VER_GREATER);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsGreaterWindowsVersion(MajorVersion,MinorVersion: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  UInt64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
OSVersion.dwMinorVersion := MinorVersion;
ConditionMask := VerSetConditionMask(VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER),VER_MINORVERSION,VER_GREATER);
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION or VER_MINORVERSION,ConditionMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsGreaterWindowsVersion(MajorVersion: Word): Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwMajorVersion := MajorVersion;
Result := VerifyVersionInfo(OSVersion,VER_MAJORVERSION,VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER));
end;

//==============================================================================

Function IsWindowsXPOrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINXP),MinorVersion(WIN32_WINNT_WINXP),0);
end;

//------------------------------------------------------------------------------

Function IsWindowsXPSP1OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINXP),MinorVersion(WIN32_WINNT_WINXP),1);
end;

//------------------------------------------------------------------------------

Function IsWindowsXPSP2OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINXP),MinorVersion(WIN32_WINNT_WINXP),2);
end;

//------------------------------------------------------------------------------

Function IsWindowsXPSP3OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINXP),MinorVersion(WIN32_WINNT_WINXP),3);
end;

//------------------------------------------------------------------------------

Function IsWindowsVistaOrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_VISTA),MinorVersion(WIN32_WINNT_VISTA),0);
end;

//------------------------------------------------------------------------------

Function IsWindowsVistaSP1OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_VISTA),MinorVersion(WIN32_WINNT_VISTA),1);
end;

//------------------------------------------------------------------------------

Function IsWindowsVistaSP2OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_VISTA),MinorVersion(WIN32_WINNT_VISTA),2);
end;

//------------------------------------------------------------------------------

Function IsWindows7OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WIN7),MinorVersion(WIN32_WINNT_WIN7),0);
end;

//------------------------------------------------------------------------------

Function IsWindows7SP1OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WIN7),MinorVersion(WIN32_WINNT_WIN7),1);
end;

//------------------------------------------------------------------------------

Function IsWindows8OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WIN8),MinorVersion(WIN32_WINNT_WIN8),0);
end;

//------------------------------------------------------------------------------

Function IsWindows8Point1OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINBLUE),MinorVersion(WIN32_WINNT_WINBLUE),0);
end;

//------------------------------------------------------------------------------

Function IsWindowsThresholdOrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINTHRESHOLD),MinorVersion(WIN32_WINNT_WINTHRESHOLD),0);
end;

//------------------------------------------------------------------------------

Function IsWindows10OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(MajorVersion(WIN32_WINNT_WINTHRESHOLD),MinorVersion(WIN32_WINNT_WINTHRESHOLD),0);
end;

//------------------------------------------------------------------------------

Function IsWindows11OrGreater: Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
{
  The major or minor version has to be greater than that of Win10, or they
  equal to Win10 and at the same time the build number is equal or grater than
  22000.
}
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwBuildNumber := 22000;
Result := IsGreaterWindowsVersion(MajorVersion(WIN32_WINNT_WIN10),MinorVersion(WIN32_WINNT_WIN10)) or
          (IsWindowsVersion(MajorVersion(WIN32_WINNT_WIN10),MinorVersion(WIN32_WINNT_WIN10)) and
           VerifyVersionInfo(OSVersion,VER_BUILDNUMBER,VerSetConditionMask(0,VER_BUILDNUMBER,VER_GREATER_EQUAL)));
end;

//------------------------------------------------------------------------------

Function IsWindowsServer: Boolean;
var
  OSVersion:  TOSVersionInfoEx;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.wProductType := VER_NT_WORKSTATION;
Result := not VerifyVersionInfo(OSVersion,VER_PRODUCT_TYPE,VerSetConditionMask(0,VER_PRODUCT_TYPE,VER_EQUAL));
end;

//==============================================================================

Function IsActiveSessionCountLimited: Boolean;
var
  ConditionMask:      UInt64;
  VersionInfo:        TOSVersionInfoEx;
  SuiteTerminal:      Boolean;
  SuiteSingleUserTS:  Boolean;
begin
ConditionMask := VerSetConditionMask(0,VER_SUITENAME,VER_AND);
VersionInfo.wSuiteMask := VER_SUITE_TERMINAL;
SuiteTerminal := VerifyVersionInfo(VersionInfo,VER_SUITENAME,ConditionMask);
VersionInfo.wSuiteMask := VER_SUITE_SINGLEUSERTS;
SuiteSingleUserTS := VerifyVersionInfo(VersionInfo,VER_SUITENAME,ConditionMask);
Result := not(SuiteTerminal and not SuiteSingleUserTS);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Other utilities
--------------------------------------------------------------------------------
===============================================================================}
const
  SM_TABLETPC    = 86;
  SM_MEDIACENTER = 87;
  SM_STARTER     = 88;
  SM_SERVERR2    = 89;
  
{===============================================================================
    Other utilities - functions implementation
===============================================================================}

Function IsWoW64Process(ProcessHandle: THandle): Boolean;
type
  TIsWow64ProcessFce = Function(hProcess: THandle; Wow64Process: PBOOL): BOOL; stdcall;
var
  ModuleHandle:       THandle;
  IsWow64ProcessFce:  TIsWow64ProcessFce;
  ResultValue:        BOOL;
begin
Result := False;
ModuleHandle := GetModuleHandle('kernel32.dll');
If ModuleHandle <> 0 then
  begin
    IsWow64ProcessFce := TIsWow64ProcessFce(GetProcAddress(ModuleHandle,'IsWow64Process'));
    If Assigned(IsWow64ProcessFce) then
      If IsWow64ProcessFce(ProcessHandle,@ResultValue) then
        Result := ResultValue
      else
        raise EWVSystemError.CreateFmt('IsWoW64Process: Failed to check WoW64 (%.8x).',[GetLastError]);
  end
else raise EWVSystemError.CreateFmt('IsWoW64Process: Unable to get handle to module kernel32.dll (%.8x).',[GetLastError]);

end;

//------------------------------------------------------------------------------

Function IsRunningUnderWoW64(ProcessID: DWORD): Boolean;
var
  ProcHandle: THandle;
begin
ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION,False,ProcessID);
If ProcHandle <> 0 then
  Result := IsWoW64Process(ProcHandle)
else
  raise EWVSystemError.CreateFmt('IsRunningUnderWoW64: Cannot open process (ID: %d) (%.8x).',[ProcessID,GetLastError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsRunningUnderWoW64: Boolean;
begin
Result := IsWoW64Process(GetCurrentProcess);
end;

//==============================================================================

Function FunctionIsPresent(const LibraryName,FunctionName: String): Boolean;
var
  LibHandle:  THandle;
begin
LibHandle := LoadLibrary(PSysChar(StrToSys(LibraryName)));
If LibHandle <> 0 then
  try
    Result := Assigned(GetProcAddress(LibHandle,PSysChar(StrToSys(FunctionName))));
  finally
    FreeLibrary(LibHandle);
  end
else raise EWVSystemError.CreateFmt('FunctionIsPresent: Cannot load library (%s) (%.8x).',[LibraryName,GetLastError]);
end;

//==============================================================================

Function IsServerR2: Boolean;
begin
Result := GetSystemMetrics(SM_SERVERR2) <> 0;
end;

//------------------------------------------------------------------------------

Function IsMediaCenterEdition: Boolean;
begin
Result := GetSystemMetrics(SM_MEDIACENTER) <> 0;
end;

//------------------------------------------------------------------------------

Function IsStarterEdition: Boolean;
begin
Result := GetSystemMetrics(SM_STARTER) <> 0;
end;

//------------------------------------------------------------------------------

Function IsTabletPCEdition: Boolean;
begin
Result := GetSystemMetrics(SM_TABLETPC) <> 0;
end;

end.
