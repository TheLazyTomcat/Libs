{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  RegistryEx

    Provides class TRegistryEx, which offers similar, but significantly
    extended, functionality as standard TRegistry class implemented in RTL.
    
    Since the interface is slightly different, it is not intended as a direct
    drop-in replacement.

  Version 1.1.1 (2024-02-03)

  Last change 2024-02-03

  ©2019-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.RegistryEx

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol RegistryEx_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit RegistryEx;
{
  RegistryEx_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  RegistryEx_UseAuxExceptions to achieve this.
}
{$IF Defined(RegistryEx_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals}
  {$MODESWITCH PointerToProcVar}
  {$MODESWITCH ClassicProcVars}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  Windows, SysUtils, Classes,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ERXException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ERXTimeConversionError = class(ERXException);
  ERXInvalidValue        = class(ERXException);

  ERXRegistryError = class(ERXException);

  ERXRegInvalidKey = class(ERXRegistryError);
  ERXRegWriteError = class(ERXRegistryError);
  ERXRegReadError  = class(ERXRegistryError);

{===============================================================================
    System constants
===============================================================================}
{-------------------------------------------------------------------------------
    System constants - security information
-------------------------------------------------------------------------------}
const
  //possible values for SecurityDescriptorCopyInfo property
  OWNER_SECURITY_INFORMATION               = $00000001;
  GROUP_SECURITY_INFORMATION               = $00000002;
  DACL_SECURITY_INFORMATION                = $00000004;
  SACL_SECURITY_INFORMATION                = $00000008;
  LABEL_SECURITY_INFORMATION               = $00000010;
  ATTRIBUTE_SECURITY_INFORMATION           = $00000020;
  SCOPE_SECURITY_INFORMATION               = $00000040;
  PROCESS_TRUST_LABEL_SECURITY_INFORMATION = $00000080;
  ACCESS_FILTER_SECURITY_INFORMATION       = $00000100;
  BACKUP_SECURITY_INFORMATION              = $00010000;

  PROTECTED_DACL_SECURITY_INFORMATION      = $80000000;
  PROTECTED_SACL_SECURITY_INFORMATION      = $40000000;
  UNPROTECTED_DACL_SECURITY_INFORMATION    = $20000000;
  UNPROTECTED_SACL_SECURITY_INFORMATION    = $10000000;

{-------------------------------------------------------------------------------
    System constants - registry access rights
-------------------------------------------------------------------------------}
const
  KEY_QUERY_VALUE        = $0001;
  KEY_SET_VALUE          = $0002;
  KEY_CREATE_SUB_KEY     = $0004;
  KEY_ENUMERATE_SUB_KEYS = $0008;
  KEY_NOTIFY             = $0010;
  KEY_CREATE_LINK        = $0020;
  KEY_WOW64_32KEY        = $0200;
  KEY_WOW64_64KEY        = $0100;
  KEY_WOW64_RES          = $0300;

  KEY_READ = (STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY) and not SYNCHRONIZE;

  KEY_WRITE = (STANDARD_RIGHTS_WRITE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY) and not SYNCHRONIZE;

  KEY_EXECUTE = KEY_READ and not SYNCHRONIZE;

  KEY_ALL_ACCESS = (STANDARD_RIGHTS_ALL or KEY_QUERY_VALUE or KEY_SET_VALUE or
                    KEY_CREATE_SUB_KEY or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or
                    KEY_CREATE_LINK) and not SYNCHRONIZE;

{-------------------------------------------------------------------------------
    System constants - open/create options
-------------------------------------------------------------------------------}
const
  REG_OPTION_RESERVED        = DWORD($00000000);  // Parameter is reserved
  REG_OPTION_NON_VOLATILE    = DWORD($00000000);  // Key is preserved when system is rebooted
  REG_OPTION_VOLATILE        = DWORD($00000001);  // Key is not preserved when system is rebooted
  REG_OPTION_CREATE_LINK     = DWORD($00000002);  // Created key is a symbolic link
  REG_OPTION_BACKUP_RESTORE  = DWORD($00000004);  // Open for backup or restore, special access rules, privilege required
  REG_OPTION_OPEN_LINK       = DWORD($00000008);  // Open symbolic link
  REG_OPTION_DONT_VIRTUALIZE = DWORD($00000010);  // Disable Open/Read/Write virtualization for this open and the resulting handle.

  REG_LEGAL_OPTION = REG_OPTION_RESERVED or REG_OPTION_NON_VOLATILE or
                     REG_OPTION_VOLATILE or REG_OPTION_CREATE_LINK or
                     REG_OPTION_BACKUP_RESTORE or REG_OPTION_OPEN_LINK or
                     REG_OPTION_DONT_VIRTUALIZE;

  REG_OPEN_LEGAL_OPTION = REG_OPTION_RESERVED or REG_OPTION_BACKUP_RESTORE or
                          REG_OPTION_OPEN_LINK or REG_OPTION_DONT_VIRTUALIZE;

{-------------------------------------------------------------------------------
    System constants - key creation/open disposition
-------------------------------------------------------------------------------}
const
  REG_CREATED_NEW_KEY     = DWORD($00000001); // New Registry Key created
  REG_OPENED_EXISTING_KEY = DWORD($00000002); // Existing Key opened   

{-------------------------------------------------------------------------------
    System constants - hive format to be used by Reg(Nt)SaveKeyEx
-------------------------------------------------------------------------------}
const
  REG_STANDARD_FORMAT = 1;
  REG_LATEST_FORMAT   = 2;
  REG_NO_COMPRESSION  = 4;

{-------------------------------------------------------------------------------
    System constants - key restore & hive load flags
-------------------------------------------------------------------------------}
const
  REG_WHOLE_HIVE_VOLATILE       = DWORD($00000001);           // Restore whole hive volatile
  REG_REFRESH_HIVE              = DWORD($00000002);           // Unwind changes to last flush
  REG_NO_LAZY_FLUSH             = DWORD($00000004);           // Never lazy flush this hive
  REG_FORCE_RESTORE             = DWORD($00000008);           // Force the restore process even when we have open handles on subkeys
  REG_APP_HIVE                  = DWORD($00000010);           // Loads the hive visible to the calling process
  REG_PROCESS_PRIVATE           = DWORD($00000020);           // Hive cannot be mounted by any other process while in use
  REG_START_JOURNAL             = DWORD($00000040);           // Starts Hive Journal
  REG_HIVE_EXACT_FILE_GROWTH    = DWORD($00000080);           // Grow hive file in exact 4k increments
  REG_HIVE_NO_RM                = DWORD($00000100);           // No RM is started for this hive (no transactions)
  REG_HIVE_SINGLE_LOG           = DWORD($00000200);           // Legacy single logging is used for this hive
  REG_BOOT_HIVE                 = DWORD($00000400);           // This hive might be used by the OS loader
  REG_LOAD_HIVE_OPEN_HANDLE     = DWORD($00000800);           // Load the hive and return a handle to its root kcb
  REG_FLUSH_HIVE_FILE_GROWTH    = DWORD($00001000);           // Flush changes to primary hive file size as part of all flushes
  REG_OPEN_READ_ONLY            = DWORD($00002000);           // Open a hive's files in read-only mode
  REG_IMMUTABLE                 = DWORD($00004000);           // Load the hive, but don't allow any modification of it
  REG_NO_IMPERSONATION_FALLBACK = DWORD($00008000);           // Do not fall back to impersonating the caller if hive file access fails
  REG_APP_HIVE_OPEN_READ_ONLY   = DWORD(REG_OPEN_READ_ONLY);  // Open an app hive's files in read-only mode (if the hive was not previously loaded)

{-------------------------------------------------------------------------------
    System constants - unload flags
-------------------------------------------------------------------------------}
const
  REG_FORCE_UNLOAD       = 1;
  REG_UNLOAD_LEGAL_FLAGS = REG_FORCE_UNLOAD;

{-------------------------------------------------------------------------------
    System constants - notify filter values
-------------------------------------------------------------------------------}
const
  REG_NOTIFY_CHANGE_NAME       = DWORD($00000001);  // Create or delete (child)
  REG_NOTIFY_CHANGE_ATTRIBUTES = DWORD($00000002);
  REG_NOTIFY_CHANGE_LAST_SET   = DWORD($00000004);  // time stamp
  REG_NOTIFY_CHANGE_SECURITY   = DWORD($00000008);
  REG_NOTIFY_THREAD_AGNOSTIC   = DWORD($10000000);  // Not associated with a calling thread, can only be used for async user event based notification

  REG_LEGAL_CHANGE_FILTER = REG_NOTIFY_CHANGE_NAME or REG_NOTIFY_CHANGE_ATTRIBUTES or
                            REG_NOTIFY_CHANGE_LAST_SET or REG_NOTIFY_CHANGE_SECURITY or
                            REG_NOTIFY_THREAD_AGNOSTIC;

{-------------------------------------------------------------------------------
    System constants - predefined value types
-------------------------------------------------------------------------------}
const
  REG_NONE                       = 0;   // No value type
  REG_SZ                         = 1;   // Unicode nul terminated string
  REG_EXPAND_SZ                  = 2;   // Unicode nul terminated string (with environment variable references)
  REG_BINARY                     = 3;   // Free form binary
  REG_DWORD                      = 4;   // 32-bit number
  REG_DWORD_LITTLE_ENDIAN        = 4;   // 32-bit number (same as REG_DWORD)
  REG_DWORD_BIG_ENDIAN           = 5;   // 32-bit number
  REG_LINK                       = 6;   // Symbolic Link (unicode)
  REG_MULTI_SZ                   = 7;   // Multiple Unicode strings
  REG_RESOURCE_LIST              = 8;   // Resource list in the resource map
  REG_FULL_RESOURCE_DESCRIPTOR   = 9;   // Resource list in the hardware description
  REG_RESOURCE_REQUIREMENTS_LIST = 10;
  REG_QWORD                      = 11;  // 64-bit number
  REG_QWORD_LITTLE_ENDIAN        = 11;  // 64-bit number (same as REG_QWORD)

{-------------------------------------------------------------------------------
    System constants - (RRF) registry routine flags (for RegGetValue)
-------------------------------------------------------------------------------}
const
  RRF_RT_REG_NONE      = $00000001; // restrict type to REG_NONE      (other data types will not return ERROR_SUCCESS)
  RRF_RT_REG_SZ        = $00000002; // restrict type to REG_SZ        (other data types will not return ERROR_SUCCESS) (automatically converts REG_EXPAND_SZ to REG_SZ unless RRF_NOEXPAND is specified)
  RRF_RT_REG_EXPAND_SZ = $00000004; // restrict type to REG_EXPAND_SZ (other data types will not return ERROR_SUCCESS) (must specify RRF_NOEXPAND or RegGetValue will fail with ERROR_INVALID_PARAMETER)
  RRF_RT_REG_BINARY    = $00000008; // restrict type to REG_BINARY    (other data types will not return ERROR_SUCCESS)
  RRF_RT_REG_DWORD     = $00000010; // restrict type to REG_DWORD     (other data types will not return ERROR_SUCCESS)
  RRF_RT_REG_MULTI_SZ  = $00000020; // restrict type to REG_MULTI_SZ  (other data types will not return ERROR_SUCCESS)
  RRF_RT_REG_QWORD     = $00000040; // restrict type to REG_QWORD     (other data types will not return ERROR_SUCCESS)

  RRF_RT_DWORD         = RRF_RT_REG_BINARY or RRF_RT_REG_DWORD; // restrict type to *32-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
  RRF_RT_QWORD         = RRF_RT_REG_BINARY or RRF_RT_REG_QWORD; // restrict type to *64-bit* RRF_RT_REG_BINARY or RRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
  RRF_RT_ANY           = $0000FFFF;                             // no type restriction

  RRF_SUBKEY_WOW6464KEY = $00010000;  // when opening the subkey (if provided) force open from the 64bit location (only one SUBKEY_WOW64* flag can be set or RegGetValue will fail with ERROR_INVALID_PARAMETER)
  RRF_SUBKEY_WOW6432KEY = $00020000;  // when opening the subkey (if provided) force open from the 32bit location (only one SUBKEY_WOW64* flag can be set or RegGetValue will fail with ERROR_INVALID_PARAMETER)
  RRF_WOW64_MASK        = $00030000;

  RRF_NOEXPAND      = $10000000;  // do not automatically expand environment strings if value is of type REG_EXPAND_SZ
  RRF_ZEROONFAILURE = $20000000;  // if pvData is not NULL, set content to all zeros on failure

{-------------------------------------------------------------------------------
    System constants - flags for RegLoadAppKey
-------------------------------------------------------------------------------}
const
  REG_PROCESS_APPKEY               = $00000001;
  REG_USE_CURRENT_SECURITY_CONTEXT = $00000002;

{-------------------------------------------------------------------------------
    System constants - reserved key handles
-------------------------------------------------------------------------------}
const
  // the handles must be sign-extended for 64bit OS
  HKEY_CLASSES_ROOT                = HKEY(PtrInt(Int32($80000000)));
  HKEY_CURRENT_USER                = HKEY(PtrInt(Int32($80000001)));
  HKEY_LOCAL_MACHINE               = HKEY(PtrInt(Int32($80000002)));
  HKEY_USERS                       = HKEY(PtrInt(Int32($80000003)));
  HKEY_PERFORMANCE_DATA            = HKEY(PtrInt(Int32($80000004)));
  HKEY_PERFORMANCE_TEXT            = HKEY(PtrInt(Int32($80000050)));
  HKEY_PERFORMANCE_NLSTEXT         = HKEY(PtrInt(Int32($80000060)));
  HKEY_CURRENT_CONFIG              = HKEY(PtrInt(Int32($80000005)));
  HKEY_DYN_DATA                    = HKEY(PtrInt(Int32($80000006)));
  HKEY_CURRENT_USER_LOCAL_SETTINGS = HKEY(PtrInt(Int32($80000007)));

  // short aliases
  HKCR = HKEY_CLASSES_ROOT;
  HKCU = HKEY_CURRENT_USER;
  HKLM = HKEY_LOCAL_MACHINE;
  HKU  = HKEY_USERS;
  HKPD = HKEY_PERFORMANCE_DATA;
  HKPT = HKEY_PERFORMANCE_TEXT;
  HKPN = HKEY_PERFORMANCE_NLSTEXT;
  HKCC = HKEY_CURRENT_CONFIG;
  HKDD = HKEY_DYN_DATA;
  HKLS = HKEY_CURRENT_USER_LOCAL_SETTINGS;

{===============================================================================
--------------------------------------------------------------------------------
                                   TRegistryEx
--------------------------------------------------------------------------------
===============================================================================}
const
  REG_PATH_DELIMITER = '\';

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXKeyAccessRight = (karQueryValue,karSetValue,karCreateSubKey,
                       karEnumerateSubKeys,karNotify,karCreateLink,
                       karWoW64_32Key,karWoW64_64Key,{standard access rights...}
                       karDelete,karReadControl,karWriteDAC,karWriteOwner,
                       karSynchronize);

  TRXKeyAccessRights = set of TRXKeyAccessRight;

const
  karWoW64_Res = [karWoW64_32Key,karWoW64_64Key];

  karStandardRead    = [karReadControl];
  karStandardWrite   = [karReadControl];
  karStandardExecute = [karReadControl];
  karStandardAll     = [karDelete,karReadControl,karWriteDAC,karWriteOwner,karSynchronize];

  karRead    = karStandardRead + [karQueryValue,karEnumerateSubKeys,karNotify] - [karSynchronize];
  karWrite   = karStandardWrite + [karSetValue,karCreateSubKey] - [karSynchronize];
  karExecute = karRead - [karSynchronize];

  karAllAccess = karStandardAll + [karQueryValue,karSetValue, karCreateSubKey,
                 karEnumerateSubKeys,karNotify,karCreateLink] - [karSynchronize];

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXKeyCreateOption = (kcoNonVolatile,kcoVolatile,kcoCreateLink,
                        kcoBackupRestore,kcoOpenLink,kcoDontVirtualize);

  TRXKeyCreateOptions = set of TRXKeyCreateOption;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXValueType = (vtNone,vtString,vtExpandString,vtBinary,vtDWord,vtDWordLE,
                  vtDWordBE,vtLink,vtMultiString,vtResourceList,
                  vtFullResourceDescriptor,vtResourceRequirementsList,vtQWord,
                  vtQWordLE,vtUnknown);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXPredefinedKey = (pkClassesRoot,pkCurrentUser,pkLocalMachine,pkUsers,
                      pkPerformanceData,pkPerformanceText,pkPerformanceNLSText,
                      pkCurrentConfig,pkDynData,pkCurrentUserLocalSettings);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
{
  Lengths are in unicode characters, without terminating zero, except for
  MaxValueLen, which is in bytes.
}
  TRXKeyInfo = record
    SubKeys:          DWORD;
    MaxSubKeyLen:     DWORD;
    MaxClassLen:      DWORD;
    Values:           DWORD;
    MaxValueNameLen:  DWORD;
    MaxValueLen:      DWORD;
    SecDescrBytes:    DWORD;
    LastWriteTime:    TDateTime;
  end;

  TRXValueInfo = record
    ValueType:  TRXValueType;
    DataSize:   TMemSize;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXPrivilegeStatus = (psError,psRemoved,psDisabled,psEnabled,psEnabledDefault);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXFileFormat = (ffStandardFormat,ffLatestFormat,ffNoCompression);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXRestoreFlag = (rfNone,rfWholeHiveVolatile,rfRefreshHive,rfNoLazyFlush,
                    rfForceRestore,rfAppHive,rfProcessPrivate,rfStartJournal,
                    rfHiveExactFileGrowth,rfHiveNoRM,rfHiveSingleLog,
                    rfBootHive,rfLoadHiveOpenHandle,rfFlushHiveFileGrowth,
                    rfOpenReadOnly,rfImmutable,rfNoImpersonationFallback,
                    rfAppHiveOpenReadOnly);

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
type
  TRXNotifyFilterOption = (noNameChange,        // sub key addition or deletion
                           noAttributeChange,   // key attributes change
                           noLastSetChange,     // value change/addition/deletetion
                           noSecurityChange,    // change to security descriptor
                           noThreadAgnostic);

  TRXNotifyFilter = set of TRXNotifyFilterOption;

const
  noWaitAll = [noNameChange,noAttributeChange,noLastSetChange,noSecurityChange];

type
  TRXWaitResult = (wrError,wrTimeout,wrChanged);

Function WaitResultToStr(WaitResult: TRXWaitResult): String;

{===============================================================================
    TRegistryEx - class declaration
===============================================================================}
{
  Following functions, when they fail, will store an error code indicating the
  cause of failure in property LastSystemError:

    RegistryQuota*
    DisablePredefinedCache
    ConnectRegistry
    OpenCurrentUser
    OpenUserClassesRoot
    OverridePredefinedKey
    RestorePredefinedKey
    WaitForKeyChange
    OpenKey
    OpenKeyReadOnly
    KeyExists
    CreateKey
    DeleteKey
    GetKeyInfo
    HasSubKeys
    GetSubKeys
    GetValueInfo
    HasValues
    GetValues
    GetValueType
    GetValueDataSize
    ValueExists
    ValueOfTypeExists
    DeleteValue
    DeleteSubKeys
    DeleteValues
    DeleteContent
    CopyKey
    MoveKey
    RenameKey
    CopyValue*
    MoveValue*
    RenameValue 
    Query*Privilege
    Enable*Privilege
    Disable*Privilege
    SaveKey
    RestoreKey
    LoadKey
    UnloadKey
    ReplaceKey
    ExportKey
    TryRead*
    Read*Def

  Depending of how a key to work on/with is selected, there are several
  behavioral classes of some public methods (they are marked with appropriate
  class letter). These classes are:

    A - parameters RootKey of type TRXPredefinedKey and KeyName string

      These functions operate on a key given by KeyName that is a subkey of
      predefined key given in parameter RootKey.

    B - string parameter KeyName

      If KeyName is relative (ie. does NOT start with path delimiter) and
      current key is open (property CurrentKey[Handle/Name]), then these
      functions operate on a subkey given by parameter KeyName that is within
      current key. Otherwise they operate on a subkey given by KeyName that
      is within predefined key stored in object's property RootKey.

    C - no common parameter

      If current key is open, then these functions operate on the current key
      and its values, otherwise they operate on predefined key stored in
      property RootKey.

    D - no common parameter

      Such functions are affecting only the current key, if any is open.
      If no current key is open, then they have no effect.

  If more than one class is noted, then the method is operating on more than
  one key, and the order of noted classes matches order of parameters.
  If no class is given, it usually means that the method does not operate on
  any specific key.
}
type
  TRegistryEx = class(TCustomObject)
  protected
    fAccessRightsSys:         DWORD;
    fAccessRights:            TRXKeyAccessRights;
    fRootKeyHandle:           HKEY;
    fRootKey:                 TRXPredefinedKey;
    fRemoteRootKey:           Boolean;
    fCloseRootKey:            Boolean;
    fCurrentKeyHandle:        HKEY;
    fCurrentKeyName:          String;
    fCurrentKeyAccessRights:  TRXKeyAccessRights;
    fFlushOnClose:            Boolean;
    fLastSysError:            Integer;
    fSecDesrCopyInfo:         DWORD;
    //--- getters, setters ---
    procedure SetAccessRightsSys(Value: DWORD); virtual;
    procedure SetAccessRights(Value: TRXKeyAccessRights); virtual;
    procedure SetRootKeyHandle(Value: HKEY); virtual;
    procedure SetRootKey(Value: TRXPredefinedKey); virtual;
    Function GetRootKeyString: String; virtual;
    Function GetRootKeyShortString: String; virtual;
    Function GetCurrentKeyReflection: Boolean; virtual;
    procedure SetCurrentKeyReflection(Value: Boolean); virtual;
    Function GetCurrentKeyPath: String; virtual;
    //--- auxiliaty methods ---
    Function SetErrorCode(ErrorCode: Integer): Integer; virtual;
    Function CheckErrorCode(ErrorCode: Integer; AllowMoreDataErr: Boolean = False): Boolean; virtual;
    Function SysCallError(CallResult: BOOL): Boolean; virtual;
    Function WaitForKeyChange(Key: HKEY; Filter: DWORD; WatchSubTree: Boolean; Timeout: DWORD): TRXWaitResult; overload; virtual;
    Function AuxCreateKey(BaseKey: HKEY; const KeyName: String; AccessRights: DWORD; out NewKey: HKEY;
      CreateOptions: DWORD = REG_OPTION_NON_VOLATILE; Disposition: LPDWORD = nil): Boolean; overload; virtual;
    Function AuxOpenKey(BaseKey: HKEY; const KeyName: String; AccessRights: DWORD; out NewKey: HKEY): Boolean; overload; virtual;
    Function AuxCloseKey(Key: HKEY): Boolean; virtual;
    procedure ChangeRootKey(KeyHandle: HKEY; Key: TRXPredefinedKey); virtual;
    procedure ChangeCurrentKey(KeyHandle: HKEY; const KeyName: String); virtual;
    Function GetWorkingKey(Relative: Boolean; out WorkingKeyName: String): HKEY; overload; virtual;
    Function GetWorkingKey(Relative: Boolean): HKEY; overload; virtual;
    Function CurrentKeyIsSubKeyOf(RootKey: TRXPredefinedKey; const KeyName: String; Strict: Boolean): Boolean; overload; virtual;
    Function CurrentKeyIsSubKeyOf(const KeyName: String; Strict: Boolean): Boolean; overload; virtual;    
    Function GetKeyInfo(Key: HKEY; out KeyInfo: TRXKeyInfo): Boolean; overload; virtual;
    Function GetSubKeys(Key: HKEY; SubKeys: TStrings): Boolean; overload; virtual;
    Function GetSubTree(Key: HKEY; SubKeys: TStrings; const ParentPath: String): Boolean; overload; virtual;
    Function GetValueInfo(Key: HKEY; const ValueName: String; out ValueInfo: TRXValueInfo): Boolean; overload; virtual;
    Function GetValues(Key: HKEY; Values: TStrings): Boolean; overload; virtual;
    Function DeleteSubKeys(Key: HKEY): Boolean; overload; virtual;
    Function DeleteValues(Key: HKEY): Boolean; overload; virtual;
    Function QueryProcessPrivilege(const PrivilegeName: String): TRXPrivilegeStatus; virtual;
    Function SetProcessPrivilege(const PrivilegeName: String; Enable: Boolean): Boolean; virtual;
    Function ExecuteRegCommand(const Command: String): Boolean;
    //--- copy/move auxiliaty methods ---
    Function MoveKey(SrcBaseKey: HKEY; const SrcSubKey: String; DstBaseKey: HKEY; const DstSubKey: String; CopySecurity,DeleteSource: Boolean): Boolean; overload; virtual;
    Function MoveValue(SrcKey: HKEY; const SrcValueName: String; DstKey: HKEY; const DstValueName: String; DeleteSource: Boolean): Boolean; overload; virtual;
    //--- data access methods and macros ---
    procedure SetValueData(const TypeName: String; Key: HKEY; const ValueName: String; const Data; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure SetValueData(const TypeName: String; Key: HKEY; const ValueName: String; Data: Integer); overload; virtual;
    Function TrySetValueData(Key: HKEY; const ValueName: String; const Data; Size: TMemSize; ValueType: TRXValueType): Boolean; virtual;
    procedure WriteMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure WriteMacro(const TypeName,KeyName,ValueName: String; const Value; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure WriteMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Integer); overload; virtual;
    procedure WriteMacro(const TypeName,KeyName,ValueName: String; Value: Integer); overload; virtual;
    Function GetValueDataOut(Key: HKEY; const ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function GetValueDataOut(Key: HKEY; const ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean; overload; virtual;
    Function GetValueDataExtBuff(Key: HKEY; const ValueName: String; out Data; var Size: TMemSize; ValueType: TRXValueType): Boolean; virtual;
    Function GetValueData(Key: HKEY; const ValueName: String; out Data; Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function GetValueData(Key: HKEY; const ValueName: String; out Data: Integer): Boolean; overload; virtual;
    Function TryReadMacroOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacroOut(const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacroOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacroOut(const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacroExtBuff(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacroExtBuff(const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacro(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacro(const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType): Boolean; overload; virtual;
    Function TryReadMacro(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Success: Boolean): Integer; overload; virtual;
    Function TryReadMacro(const KeyName,ValueName: String; out Success: Boolean): Integer; overload; virtual;
    procedure ReadMacroOut(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroOut(const TypeName,KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroOut(const TypeName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroOut(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroOut(const TypeName,KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroOut(const TypeName,ValueName: String; out Str: WideString; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroExtBuff(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroExtBuff(const TypeName,KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacroExtBuff(const TypeName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacro(const TypeName,KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    procedure ReadMacro(const TypeName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType); overload; virtual;
    Function ReadMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Integer; overload; virtual;
    Function ReadMacro(const TypeName,KeyName,ValueName: String): Integer; overload; virtual;
    Function ReadMacro(const TypeName,ValueName: String): Integer; overload; virtual;
    //--- init/final methods ---
    procedure Initialize(RootKey: TRXPredefinedKey; AccessRights: TRXKeyAccessRights); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(RootKey: TRXPredefinedKey; AccessRights: TRXKeyAccessRights = karAllAccess); overload;
    constructor Create(AccessRights: TRXKeyAccessRights = karAllAccess); overload;  // root key is set to pkCurrentUser
    destructor Destroy; override;
    //--- global registry functions ---
  {
    If functions RegistryQuota* fail to obtain the number, they will return 0
    and LastSystemError will contain error code of the cause of failure.
  }    
    Function RegistryQuotaAllowed: UInt32; virtual;
    Function RegistryQuotaUsed: UInt32; virtual;
  {
    When parameter AllKeys is set to false, function RegDisablePredefinedCache
    is called. When set to true, RegDisablePredefinedCacheEx is called - but
    note that this function is implemented only from Windows Vista up. If you
    call this method with AllKeys set to true in older systems, it will fail
    (returns false and LastSystemError is set to ERROR_CALL_NOT_IMPLEMENTED)
    and has no effect.
  }
    Function DisablePredefinedCache(AllKeys: Boolean): Boolean; virtual;
  {
    ConnectRegistry only connects to a remote registry, proper access and
    possible needed logon is not managed.
  }
    Function ConnectRegistry(const MachineName: String; RootKey: TRXPredefinedKey): Boolean; virtual;
  {
    Sets root key to pkCurrentUserbut instead of using predefined key handle,
    the handle is obtained using function RegOpenCurrentUser - meaning it is
    bound to current user key of a user the calling thread is impersonating.
  }
    Function OpenCurrentUser(AccessRights: TRXKeyAccessRights = karAllAccess): Boolean; virtual;
  {
    Similarly to OpenCurrentUser, sets root key to pkClassesRootbut, but the
    handle is obtained using function RegOpenUserClassesRoot - it is a handle
    of classes root key belonging to a user identified by impersonation access
    token that is passed in AccessToken parameter.
    For more details, see documentation of RegOpenUserClassesRoot function.
  }
    Function OpenUserClassesRoot(AccessToken: THandle; AccessRights: TRXKeyAccessRights = karAllAccess): Boolean; virtual;
  {
    OverridePredefinedKey maps specified predefined registry key to another
    open key.

    RestorePredefinedKey restores the default mapping for specified predefined
    registry key.
  }
 {A}Function OverridePredefinedKey(PredefinedKey: TRXPredefinedKey; RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function OverridePredefinedKey(PredefinedKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {D}Function OverridePredefinedKey(PredefinedKey: TRXPredefinedKey): Boolean; overload; virtual;
    Function RestorePredefinedKey(PredefinedKey: TRXPredefinedKey): Boolean; virtual;
  {
    For any kind of failure, the WaitForKeyChange will return wrError.

      WARNING - if called on current key, you must close and re-open the key
                before next call to WaitForKeyChange, otherwise the function
                will behave incorrectly. It is therefore recommended to only
                use first (class A) or second (class B) overload with non-empty
                key name.
  }
 {A}Function WaitForKeyChange(RootKey: TRXPredefinedKey; const KeyName: String; WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult; overload; virtual;
 {B}Function WaitForKeyChange(const KeyName: String; WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult; overload; virtual;
 {C}Function WaitForKeyChange(WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult; overload; virtual;
    //--- keys management ---
  {
    Opens or creates requested key as a current key. If current key was already
    opened, it is first closed. If the function fails, nothing is changed
    (current key, if already opened, will stay opened).

    Note that the key can only be created when the KeyName is not empty.
  }
 {A}Function OpenKey(RootKey: TRXPredefinedKey; const KeyName: String; CanCreate: Boolean; out Created: Boolean; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean; overload; virtual;
 {B}Function OpenKey(const KeyName: String; CanCreate: Boolean; out Created: Boolean; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean; overload; virtual;
 {A}Function OpenKey(RootKey: TRXPredefinedKey; const KeyName: String; CanCreate: Boolean = False): Boolean; overload; virtual;
 {B}Function OpenKey(const KeyName: String; CanCreate: Boolean = False): Boolean; overload; virtual;
  {
    OpenKeyReadOnly, when successful, will change AccessRight property to
    karRead, but it will also preserve karWoW64_32Key and karWoW64_64Key if
    they were previously set.
  }
 {A}Function OpenKeyReadOnly(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function OpenKeyReadOnly(const KeyName: String): Boolean; overload; virtual;
  {
    KeyExists tries to open given key for reading. When it succeeds, it is
    assumed the key exists, otherwise it is assumed it does not exist.
  }
 {A}Function KeyExists(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function KeyExists(const KeyName: String): Boolean; overload; virtual;
  {
    KeyName must not be empty, otherwise the creation fails.
  }
 {A}Function CreateKey(RootKey: TRXPredefinedKey; const KeyName: String; AccessRights: TRXKeyAccessRights = karAllAccess; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean; overload; virtual;
 {B}Function CreateKey(const KeyName: String; AccessRights: TRXKeyAccessRights = karAllAccess; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean; overload; virtual;
  {
    Be carefull what you are deleting, I have managed to delete entire content
    of HKEY_CURRENT_USER during testing (thank god for system restore).
  }
 {A}Function DeleteKey(RootKey: TRXPredefinedKey; const KeyName: String; CanDeleteCurrentKey: Boolean; out CurrentKeyClosed: Boolean): Boolean; overload; virtual;
 {B}Function DeleteKey(const KeyName: String; CanDeleteCurrentKey: Boolean; out CurrentKeyClosed: Boolean): Boolean; overload; virtual;
 {A}Function DeleteKey(RootKey: TRXPredefinedKey; const KeyName: String; CanDeleteCurrentKey: Boolean = True): Boolean; overload; virtual;
 {B}Function DeleteKey(const KeyName: String; CanDeleteCurrentKey: Boolean = True): Boolean; overload; virtual;
 {D}procedure FlushKey; virtual; 
 {D}procedure CloseKey; virtual;
    //--- key information ---
  {
    If GetKeyInfo fails (returns false), the content of KeyInfo is undefined.
  }
 {A}Function GetKeyInfo(RootKey: TRXPredefinedKey; const KeyName: String; out KeyInfo: TRXKeyInfo): Boolean; overload; virtual;
 {B}Function GetKeyInfo(const KeyName: String; out KeyInfo: TRXKeyInfo): Boolean; overload; virtual;
 {C}Function GetKeyInfo(out KeyInfo: TRXKeyInfo): Boolean; overload; virtual;
  {
    Following three overloads of HasSubKeys return true when the requested key
    has any subkey.
    When they return false, it means either that there is no subkey or that the
    function has failed - check property LastSystemError. When it contains
    ERROR_SUCCESS, then the function succeeded and there is no subkey, when it
    contains other error code, it indicates that the function has failed (the
    code can be used to discern the cause of failure).
  }
 {A}Function HasSubKeys(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function HasSubKeys(const KeyName: String): Boolean; overload; virtual;
 {C}Function HasSubKeys: Boolean; overload; virtual;
  {
    Following overloads of HasSubKeys are working the same as previous versions,
    except that the result strictly indicates whether the function succeeded or
    not, whether the key has subkeys is indicated by output parameter
    SubKeysRes.
    Note that when the function fails, the value of SubKeysRes is undefined.
  }
 {A}Function HasSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; out SubKeysRes: Boolean): Boolean; overload; virtual;
 {B}Function HasSubKeys(const KeyName: String; out SubKeysRes: Boolean): Boolean; overload; virtual;
 {C}Function HasSubKeys(out SubKeysRes: Boolean): Boolean; overload; virtual;
  {
    SubKeys object is always cleared, irrespective of whether the function
    succeeds or not.
  }
 {A}Function GetSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; SubKeys: TStrings): Boolean; overload; virtual;
 {B}Function GetSubKeys(const KeyName: String; SubKeys: TStrings): Boolean; overload; virtual;
 {C}Function GetSubKeys(SubKeys: TStrings): Boolean; overload; virtual;
  {
    SubKeys object is always cleared, irrespective of whether the function
    succeeds or not.
  }
 {A}Function GetSubTree(RootKey: TRXPredefinedKey; const KeyName: String; SubKeys: TStrings): Boolean; overload; virtual;
 {B}Function GetSubTree(const KeyName: String; SubKeys: TStrings): Boolean; overload; virtual;
 {C}Function GetSubTree(SubKeys: TStrings): Boolean; overload; virtual;
    //--- values information/access ---
  {
    Functions GetValueInfo, HasValues and GetValues are working the same as
    corresponding functions for keys (GetKeyInfo, HasSubKeys, ...).
  }
 {A}Function GetValueInfo(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out ValueInfo: TRXValueInfo): Boolean; overload; virtual;
 {B}Function GetValueInfo(const KeyName,ValueName: String; out ValueInfo: TRXValueInfo): Boolean; overload; virtual;
 {C}Function GetValueInfo(const ValueName: String; out ValueInfo: TRXValueInfo): Boolean; overload; virtual;
 {A}Function HasValues(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function HasValues(const KeyName: String): Boolean; overload; virtual;
 {C}Function HasValues: Boolean; overload; virtual;
 {A}Function HasValues(RootKey: TRXPredefinedKey; const KeyName: String; out ValuesRes: Boolean): Boolean; overload; virtual;
 {B}Function HasValues(const KeyName: String; out ValuesRes: Boolean): Boolean; overload; virtual;
 {C}Function HasValues(out ValuesRes: Boolean): Boolean; overload; virtual;
 {A}Function GetValues(RootKey: TRXPredefinedKey; const KeyName: String; Values: TStrings): Boolean; overload; virtual;
 {B}Function GetValues(const KeyName: String; Values: TStrings): Boolean; overload; virtual;
 {C}Function GetValues(Values: TStrings): Boolean; overload; virtual;
  {
    GetValueType will return vtUnknown in case the value does not exist or
    cannot be queried in general.
  }
 {A}Function GetValueType(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TRXValueType; overload; virtual;
 {B}Function GetValueType(const KeyName,ValueName: String): TRXValueType; overload; virtual;
 {C}Function GetValueType(const ValueName: String): TRXValueType; overload; virtual;
  {
    Following three overloads of GetValueDataSize will return 0 in case the
    value does not exist or cannot be queried.
    Note that 0 is a valid data size, so do not assume the function have failed
    when it returned zero. This method will explicitly set property
    LastSystemError to ERROR_SUCCESS when it succeeds, so you can check whether
    the returned zero is a valid result or not (in which case the
    LastSystemError will contain other error code than ERROR_SUCCESS).
  }
 {A}Function GetValueDataSize(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TMemSize; overload; virtual;
 {B}Function GetValueDataSize(const KeyName,ValueName: String): TMemSize; overload; virtual;
 {C}Function GetValueDataSize(const ValueName: String): TMemSize; overload; virtual;
  {
    Following overloads will indicate success in ther result and the actual
    size is returned in output parameter DataSize.
    If the function fails, then content of DataSize is undefined.
  }
 {A}Function GetValueDataSize(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out DataSize: TMemSize): Boolean; overload; virtual;
 {B}Function GetValueDataSize(const KeyName,ValueName: String; out DataSize: TMemSize): Boolean; overload; virtual;
 {C}Function GetValueDataSize(const ValueName: String; out DataSize: TMemSize): Boolean; overload; virtual;
  {
    ValueExists returns result of GetValueInfo for the given key and value.
    So if the info can be obtained, it is assumed the value exists, otherwise
    it is assumed it does not exist.
  }
 {A}Function ValueExists(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean; overload; virtual;
 {B}Function ValueExists(const KeyName,ValueName: String): Boolean; overload; virtual;
 {C}Function ValueExists(const ValueName: String): Boolean; overload; virtual;
  {
    ValueOfTypeExists checks whether the given value exist within given key,
    and also whether is of given type.
    If the value exists but is of differing type, then false is returned and
    LastSystemError is set to ERROR_DATATYPE_MISMATCH.
  }
 {A}Function ValueOfTypeExists(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; ValueType: TRXValueType): Boolean; overload; virtual;
 {B}Function ValueOfTypeExists(const KeyName,ValueName: String; ValueType: TRXValueType): Boolean; overload; virtual;
 {C}Function ValueOfTypeExists(const ValueName: String; ValueType: TRXValueType): Boolean; overload; virtual;
 {A}Function DeleteValue(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean; overload; virtual;
 {B}Function DeleteValue(const KeyName,ValueName: String): Boolean; overload; virtual;
 {C}Function DeleteValue(const ValueName: String): Boolean; overload; virtual;
    //--- content deletion ---
  {
    DeleteSubKeys returns true when ALL subkeys are deleted. If deletion of
    even one subkey fails, it returns false.
    Note that the function will stop deleting the subkeys on a first failed
    attempt.
  }
 {A}Function DeleteSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; out CurrentKeyClosed: Boolean): Boolean; overload; virtual;
 {B}Function DeleteSubKeys(const KeyName: String; out CurrentKeyClosed: Boolean): Boolean; overload; virtual;
 {C}Function DeleteSubKeys: Boolean; overload; virtual;
 {A}Function DeleteSubKeys(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function DeleteSubKeys(const KeyName: String): Boolean; overload; virtual;
  {
    DeleteValues returns true when ALL values are deleted. If deletion of even
    one value fails, it returns false.
    Note that the function will stop deleting the values on a first failed
    attempt.
  }
 {A}Function DeleteValues(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function DeleteValues(const KeyName: String): Boolean; overload; virtual;
 {C}Function DeleteValues: Boolean; overload; virtual;
  {
    DeleteContent first tries to delete all values in the given key (calls
    DeleteValues). When it succeeds, it will try to delete all subkeys (using
    DeleteSubKeys) and return result of this operation.
    If it fails to delete all values, the subkeys deletion is not even
    attempted.
  }
 {A}Function DeleteContent(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; overload; virtual;
 {B}Function DeleteContent(const KeyName: String): Boolean; overload; virtual;
 {C}Function DeleteContent: Boolean; overload; virtual;
    //--- advanced keys and values manipulation ---
  {
    In CopyKey and MoveKey, the destination key must not exist, otherwise the
    function will fail.
  }
  {
    CopyKey, MoveKey and RenameKey (which calls MoveKey) are all multi-stage
    processes. It is possible, since there is no roll-back (for the sake of
    data protection), when the function fails at any point, that the destination
    will be partially or even fully created, or the source only partially
    removed.
  }
{AA}Function CopyKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{AB}Function CopyKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{BA}Function CopyKey(const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{BB}Function CopyKey(const SrcKeyName,DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
  {
    If current key is a subkey of moved source key, it is closed and reopened
    from the new location.
  }
{AA}Function MoveKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{AB}Function MoveKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{BA}Function MoveKey(const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
{BB}Function MoveKey(const SrcKeyName,DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
  {
    RenameKey changes name of a key given by OldKeyName to NewKeyName. Since
    there is no way of directly changing registry key name, it is instead
    copied and the original is then deleted (calls MoveKey).
  }
 {A}Function RenameKey(RootKey: TRXPredefinedKey; const OldKeyName,NewKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
 {B}Function RenameKey(const OldKeyName,NewKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean; overload; virtual;
  {
    For all CopyValue and MoveValue functions, the destination key must already
    exist, it will not be created.
    Also, the destination value must not exist, otherwise the function fails
    and LastSystemError is set to ERROR_ALREADY_EXISTS.

    All CopyValue, MoveValue and RenameValue methods are multi-stage. So it is
    possible that, in the case of failure, the destination value will be
    nevertheless created.
  }
  {
    CopyValue copies value from one arbitrary key into another arbitrary key.
  }
{AA}Function CopyValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{AB}Function CopyValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{BA}Function CopyValue(const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{BB}Function CopyValue(const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
  {
    CopyValueTo copies value from subkey of current key or root key into an
    arbitrary key.
  }
{CA}Function CopyValueTo(const SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{CB}Function CopyValueTo(const SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
  {
    CopyValueFrom copies value from an arbitrary key into a subkey of current
    key or root key.
  }
{AC}Function CopyValueFrom(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean; overload; virtual;  
{BC}Function CopyValueFrom(const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean; overload; virtual;
  {
    CopyValueIn copies value within one key, be it arbitrary, current or root
    key.
  }
 {A}Function CopyValueIn(RootKey: TRXPredefinedKey; const KeyName,SrcValueName,DstValueName: String): Boolean; overload; virtual;
 {B}Function CopyValueIn(const KeyName,SrcValueName,DstValueName: String): Boolean; overload; virtual;
 {C}Function CopyValueIn(const SrcValueName,DstValueName: String): Boolean; overload; virtual;
  {
    The processing in MoveValue* functions is two-phase. First, a copy of the
    source is created. If this fails, the function exits and returns false.
    When successful, then the source is then deleted and MoveValue* returns
    result of this deletion.
  }
{AA}Function MoveValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{AB}Function MoveValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{BA}Function MoveValue(const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{BB}Function MoveValue(const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{CA}Function MoveValueTo(const SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{CB}Function MoveValueTo(const SrcValueName: String; const DstKeyName,DstValueName: String): Boolean; overload; virtual;
{AC}Function MoveValueFrom(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean; overload; virtual;
{BC}Function MoveValueFrom(const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean; overload; virtual;
 {A}Function MoveValueIn(RootKey: TRXPredefinedKey; const KeyName,SrcValueName,DstValueName: String): Boolean; overload; virtual;
 {B}Function MoveValueIn(const KeyName,SrcValueName,DstValueName: String): Boolean; overload; virtual;
 {C}Function MoveValueIn(const SrcValueName,DstValueName: String): Boolean; overload; virtual;
  {
    Afaik there is no way to directly rename a value, so it is instead copied
    to a new value with NewName and the original (OldName) is then deleted.

    Value with the new name must not exist, otherwise the function fails
    and LastSystemError is set to ERROR_ALREADY_EXISTS.
  }
 {A}Function RenameValue(RootKey: TRXPredefinedKey; const KeyName,OldValueName,NewValueName: String): Boolean; overload; virtual;
 {B}Function RenameValue(const KeyName,OldValueName,NewValueName: String): Boolean; overload; virtual;
 {C}Function RenameValue(const OldValueName,NewValueName: String): Boolean; overload; virtual;
    //--- keys saving and loading ---
  {
    See further...
  }
    Function QueryBackupPrivilege: TRXPrivilegeStatus; virtual;
    Function EnableBackupPrivilege: Boolean; virtual;
    Function DisableBackupPrivilege: Boolean; virtual;
    Function QueryRestorePrivilege: TRXPrivilegeStatus; virtual;
    Function EnableRestorePrivilege: Boolean; virtual;
    Function DisableRestorePrivilege: Boolean; virtual;
  {
    Following functions are only simple wrappers for backup and restore
    registry functions. More detailed implementation is beyond the scope of
    this library.

    All presented functions require for the calling process to have special
    privileges enabled, otherwise they will fail. Namely SeBackupPrivilege and
    SeRestorePrivilege must be present in access token and must be enabled
    (exception being SaveKey functions - they require only backup privilege).

    For this, Query*Privilege, Enable*Privilege and Disable*Privilege methods
    are provided - use them to get a state of mentioned privileges and
    potentially enable or disable them.

      WARNING - it is not possible to add privilege to acess token, and none of
                the required privilege is present for normal precesses.
                So if you need to use those functions, make sure the process is
                run with administrator privileges (then the required privileges
                are present, but disabled - you can enable them).
  }
 {A}Function SaveKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean; overload; virtual;
 {B}Function SaveKey(const KeyName,FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean; overload; virtual;
 {C}Function SaveKey(const FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean; overload; virtual;
 {A}Function RestoreKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean; overload; virtual;
 {B}Function RestoreKey(const KeyName,FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean; overload; virtual;
 {C}Function RestoreKey(const FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean; overload; virtual;
  {
    RootKey parameter in LoadKey and UnLoadKey can only be set to pkUsers or
    pkLocalMachine, otherwise the function will fail.
    KeyName must not be empty and cannot be a path, only a simple key name.
  }
    Function LoadKey(RootKey: TRXPredefinedKey; const KeyName, FileName: String): Boolean; virtual;
    Function UnLoadKey(RootKey: TRXPredefinedKey; const KeyName: String): Boolean; virtual;
  {
      WARNING - considering how RegReplaceKey (which is internally called)
                works, using this function can be very dangerous. There is a
                good chance to completely erase registry hive when used
                inappropriately. Please refer to documentation of Win32
                function RegReplaceKey for details.
  }
 {A}Function ReplaceKey(RootKey: TRXPredefinedKey; const KeyName,NewFileName,OldFileName: String): Boolean; overload; virtual;
 {B}Function ReplaceKey(const KeyName,NewFileName,OldFileName: String): Boolean; overload; virtual;
 {C}Function ReplaceKey(const NewFileName,OldFileName: String): Boolean; overload; virtual;
 {A}Function ExportKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String): Boolean; overload; virtual;
 {B}Function ExportKey(const KeyName,FileName: String): Boolean; overload; virtual;
 {C}Function ExportKey(const FileName: String): Boolean; overload; virtual;
    Function ImportKey(const FileName: String): Boolean; virtual;
    //--- values write ---
  {
    When writing to other key than current or predefined, the key must already
    exist, otherwise an ERXRegInvalidKey exception is raised.
  }
 {A}procedure WriteBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Boolean); overload; virtual;
 {B}procedure WriteBool(const KeyName,ValueName: String; Value: Boolean); overload; virtual;
 {C}procedure WriteBool(const ValueName: String; Value: Boolean); overload; virtual;
 {A}procedure WriteInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int8); overload; virtual;
 {B}procedure WriteInt8(const KeyName,ValueName: String; Value: Int8); overload; virtual;
 {C}procedure WriteInt8(const ValueName: String; Value: Int8); overload; virtual;
 {A}procedure WriteUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt8); overload; virtual;
 {B}procedure WriteUInt8(const KeyName,ValueName: String; Value: UInt8); overload; virtual;
 {C}procedure WriteUInt8(const ValueName: String; Value: UInt8); overload; virtual;
 {A}procedure WriteInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int16); overload; virtual;
 {B}procedure WriteInt16(const KeyName,ValueName: String; Value: Int16); overload; virtual;
 {C}procedure WriteInt16(const ValueName: String; Value: Int16); overload; virtual;
 {A}procedure WriteUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt16); overload; virtual;
 {B}procedure WriteUInt16(const KeyName,ValueName: String; Value: UInt16); overload; virtual;
 {C}procedure WriteUInt16(const ValueName: String; Value: UInt16); overload; virtual;
 {A}procedure WriteInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int32); overload; virtual;
 {B}procedure WriteInt32(const KeyName,ValueName: String; Value: Int32); overload; virtual;
 {C}procedure WriteInt32(const ValueName: String; Value: Int32); overload; virtual;
 {A}procedure WriteUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt32); overload; virtual;
 {B}procedure WriteUInt32(const KeyName,ValueName: String; Value: UInt32); overload; virtual;
 {C}procedure WriteUInt32(const ValueName: String; Value: UInt32); overload; virtual;
 {A}procedure WriteInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int64); overload; virtual;
 {B}procedure WriteInt64(const KeyName,ValueName: String; Value: Int64); overload; virtual;
 {C}procedure WriteInt64(const ValueName: String; Value: Int64); overload; virtual;
 {A}procedure WriteUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt64); overload; virtual;
 {B}procedure WriteUInt64(const KeyName,ValueName: String; Value: UInt64); overload; virtual;
 {C}procedure WriteUInt64(const ValueName: String; Value: UInt64); overload; virtual;
 {A}procedure WriteInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Integer); overload; virtual;
 {B}procedure WriteInteger(const KeyName,ValueName: String; Value: Integer); overload; virtual;
 {C}procedure WriteInteger(const ValueName: String; Value: Integer); overload; virtual;
 {A}procedure WriteFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Float32); overload; virtual;
 {B}procedure WriteFloat32(const KeyName,ValueName: String; Value: Float32); overload; virtual;
 {C}procedure WriteFloat32(const ValueName: String; Value: Float32); overload; virtual;
 {A}procedure WriteFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Float64); overload; virtual;
 {B}procedure WriteFloat64(const KeyName,ValueName: String; Value: Float64); overload; virtual;
 {C}procedure WriteFloat64(const ValueName: String; Value: Float64); overload; virtual;
 {A}procedure WriteFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Double); overload; virtual;
 {B}procedure WriteFloat(const KeyName,ValueName: String; Value: Double); overload; virtual;
 {C}procedure WriteFloat(const ValueName: String; Value: Double); overload; virtual;
 {A}procedure WriteCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Currency); overload; virtual;
 {B}procedure WriteCurrency(const KeyName,ValueName: String; Value: Currency); overload; virtual;
 {C}procedure WriteCurrency(const ValueName: String; Value: Currency); overload; virtual;
 {A}procedure WriteDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {B}procedure WriteDateTime(const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {C}procedure WriteDateTime(const ValueName: String; Value: TDateTime); overload; virtual;
 {A}procedure WriteDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {B}procedure WriteDate(const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {C}procedure WriteDate(const ValueName: String; Value: TDateTime); overload; virtual;
 {A}procedure WriteTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {B}procedure WriteTime(const KeyName,ValueName: String; Value: TDateTime); overload; virtual;
 {C}procedure WriteTime(const ValueName: String; Value: TDateTime); overload; virtual;
 {A}procedure WriteString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value: String); overload; virtual;
 {B}procedure WriteString(const KeyName,ValueName: String; const Value: String); overload; virtual;
 {C}procedure WriteString(const ValueName: String; const Value: String); overload; virtual;
 {A}procedure WriteExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value: String; UnExpand: Boolean = False); overload; virtual;
 {B}procedure WriteExpandString(const KeyName,ValueName: String; const Value: String; UnExpand: Boolean = False); overload; virtual;
 {C}procedure WriteExpandString(const ValueName: String; const Value: String; UnExpand: Boolean = False); overload; virtual;
 {A}procedure WriteMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings); overload; virtual;
 {B}procedure WriteMultiString(const KeyName,ValueName: String; Value: TStrings); overload; virtual;
 {C}procedure WriteMultiString(const ValueName: String; Value: TStrings); overload; virtual;
 {A}procedure WriteBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Buff; Size: TMemSize); overload; virtual;
 {B}procedure WriteBinaryBuffer(const KeyName,ValueName: String; const Buff; Size: TMemSize); overload; virtual;
 {C}procedure WriteBinaryBuffer(const ValueName: String; const Buff; Size: TMemSize); overload; virtual;
 {A}procedure WriteBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize); overload; virtual;
 {B}procedure WriteBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize); overload; virtual;
 {C}procedure WriteBinaryMemory(const ValueName: String; Memory: Pointer; Size: TMemSize); overload; virtual;
  {
    Position of the passed stream is undefined after the call, do not assume
    anything about its value.
  }
 {A}procedure WriteBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream; Position, Count: Int64); overload; virtual;
 {B}procedure WriteBinaryStream(const KeyName,ValueName: String; Stream: TStream; Position, Count: Int64); overload; virtual;
 {C}procedure WriteBinaryStream(const ValueName: String; Stream: TStream; Position, Count: Int64); overload; virtual;
 {A}procedure WriteBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream); overload; virtual;
 {B}procedure WriteBinaryStream(const KeyName,ValueName: String; Stream: TStream); overload; virtual;
 {C}procedure WriteBinaryStream(const ValueName: String; Stream: TStream); overload; virtual;
    //--- values try-read ---
 {A}Function TryReadBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Boolean): Boolean; overload; virtual;
 {B}Function TryReadBool(const KeyName,ValueName: String; out Value: Boolean): Boolean; overload; virtual;
 {C}Function TryReadBool(const ValueName: String; out Value: Boolean): Boolean; overload; virtual;
 {A}Function TryReadInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int8): Boolean; overload; virtual;
 {B}Function TryReadInt8(const KeyName,ValueName: String; out Value: Int8): Boolean; overload; virtual;
 {C}Function TryReadInt8(const ValueName: String; out Value: Int8): Boolean; overload; virtual;
 {A}Function TryReadUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt8): Boolean; overload; virtual;
 {B}Function TryReadUInt8(const KeyName,ValueName: String; out Value: UInt8): Boolean; overload; virtual;
 {C}Function TryReadUInt8(const ValueName: String; out Value: UInt8): Boolean; overload; virtual;
 {A}Function TryReadInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int16): Boolean; overload; virtual;
 {B}Function TryReadInt16(const KeyName,ValueName: String; out Value: Int16): Boolean; overload; virtual;
 {C}Function TryReadInt16(const ValueName: String; out Value: Int16): Boolean; overload; virtual;
 {A}Function TryReadUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt16): Boolean; overload; virtual;
 {B}Function TryReadUInt16(const KeyName,ValueName: String; out Value: UInt16): Boolean; overload; virtual;
 {C}Function TryReadUInt16(const ValueName: String; out Value: UInt16): Boolean; overload; virtual;
 {A}Function TryReadInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int32): Boolean; overload; virtual;
 {B}Function TryReadInt32(const KeyName,ValueName: String; out Value: Int32): Boolean; overload; virtual;
 {C}Function TryReadInt32(const ValueName: String; out Value: Int32): Boolean; overload; virtual;
 {A}Function TryReadUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt32): Boolean; overload; virtual;
 {B}Function TryReadUInt32(const KeyName,ValueName: String; out Value: UInt32): Boolean; overload; virtual;
 {C}Function TryReadUInt32(const ValueName: String; out Value: UInt32): Boolean; overload; virtual;
 {A}Function TryReadInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int64): Boolean; overload; virtual;
 {B}Function TryReadInt64(const KeyName,ValueName: String; out Value: Int64): Boolean; overload; virtual;
 {C}Function TryReadInt64(const ValueName: String; out Value: Int64): Boolean; overload; virtual;
 {A}Function TryReadUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt64): Boolean; overload; virtual;
 {B}Function TryReadUInt64(const KeyName,ValueName: String; out Value: UInt64): Boolean; overload; virtual;
 {C}Function TryReadUInt64(const ValueName: String; out Value: UInt64): Boolean; overload; virtual;
 {A}Function TryReadInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Integer): Boolean; overload; virtual;
 {B}Function TryReadInteger(const KeyName,ValueName: String; out Value: Integer): Boolean; overload; virtual;
 {C}Function TryReadInteger(const ValueName: String; out Value: Integer): Boolean; overload; virtual;
 {A}Function TryReadFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Float32): Boolean; overload; virtual;
 {B}Function TryReadFloat32(const KeyName,ValueName: String; out Value: Float32): Boolean; overload; virtual;
 {C}Function TryReadFloat32(const ValueName: String; out Value: Float32): Boolean; overload; virtual;
 {A}Function TryReadFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Float64): Boolean; overload; virtual;
 {B}Function TryReadFloat64(const KeyName,ValueName: String; out Value: Float64): Boolean; overload; virtual;
 {C}Function TryReadFloat64(const ValueName: String; out Value: Float64): Boolean; overload; virtual;
 {A}Function TryReadFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Double): Boolean; overload; virtual;
 {B}Function TryReadFloat(const KeyName,ValueName: String; out Value: Double): Boolean; overload; virtual;
 {C}Function TryReadFloat(const ValueName: String; out Value: Double): Boolean; overload; virtual;
 {A}Function TryReadCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Currency): Boolean; overload; virtual;
 {B}Function TryReadCurrency(const KeyName,ValueName: String; out Value: Currency): Boolean; overload; virtual;
 {C}Function TryReadCurrency(const ValueName: String; out Value: Currency): Boolean; overload; virtual;
 {A}Function TryReadDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {B}Function TryReadDateTime(const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {C}Function TryReadDateTime(const ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {A}Function TryReadDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {B}Function TryReadDate(const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {C}Function TryReadDate(const ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {A}Function TryReadTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {B}Function TryReadTime(const KeyName,ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {C}Function TryReadTime(const ValueName: String; out Value: TDateTime): Boolean; overload; virtual;
 {A}Function TryReadString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: String): Boolean; overload; virtual;
 {B}Function TryReadString(const KeyName,ValueName: String; out Value: String): Boolean; overload; virtual;
 {C}Function TryReadString(const ValueName: String; out Value: String): Boolean; overload; virtual;
 {A}Function TryReadExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: String; Expand: Boolean = False): Boolean; overload; virtual;
 {B}Function TryReadExpandString(const KeyName,ValueName: String; out Value: String; Expand: Boolean = False): Boolean; overload; virtual;
 {C}Function TryReadExpandString(const ValueName: String; out Value: String; Expand: Boolean = False): Boolean; overload; virtual;
 {A}Function TryReadMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings): Boolean; overload; virtual;
 {B}Function TryReadMultiString(const KeyName,ValueName: String; Value: TStrings): Boolean; overload; virtual;
 {C}Function TryReadMultiString(const ValueName: String; Value: TStrings): Boolean; overload; virtual;
  {
    Size must, on enter, contain size of the preallocated output buffer.
    In case of success, it will contain true amount of data stored into the
    buffer. In case of failure its value is undefined and content of Buff is
    also undefined (might be changed).

    To obtain size of buffer that is required to store the data, use method
    GetValueDataSize.
  }
 {A}Function TryReadBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Buff; var Size: TMemSize): Boolean; overload; virtual;
 {B}Function TryReadBinaryBuffer(const KeyName,ValueName: String; out Buff; var Size: TMemSize): Boolean; overload; virtual;
 {C}Function TryReadBinaryBuffer(const ValueName: String; out Buff; var Size: TMemSize): Boolean; overload; virtual;
  {
    TryReadBinaryMemory behaves the same as TryReadBinaryBuffer.
  }
 {A}Function TryReadBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean; overload; virtual;
 {B}Function TryReadBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean; overload; virtual;
 {C}Function TryReadBinaryMemory(const ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean; overload; virtual;
  {
    This funtion does not need preallocated buffer. Instead, it itself
    allocates memory space that is necessary to store the data and, when it
    succeeds, returns pointer to this memory along with size of the allocated
    space. If it fails, the content of output arguments Memory and Size is
    undefined (no memory is left allocated, so there is no leak).

    To free the allocated memory, use standard memory management functions
    (FreeMem, ReallocMem, ...).

    This function is intended for situations where amount of stored data can
    rapidly change (eg. values in HKEY_PERFORMANCE_DATA).
  }
 {A}Function TryReadBinaryMemoryOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean; overload; virtual;
 {B}Function TryReadBinaryMemoryOut(const KeyName,ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean; overload; virtual;
 {C}Function TryReadBinaryMemoryOut(const ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean; overload; virtual;
  {
    Position of the stream is undefined after the call.
  }
 {A}Function TryReadBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream): Boolean; overload; virtual;
 {B}Function TryReadBinaryStream(const KeyName,ValueName: String; Stream: TStream): Boolean; overload; virtual;
 {C}Function TryReadBinaryStream(const ValueName: String; Stream: TStream): Boolean; overload; virtual;
    //--- values read-def ---
 {A}Function ReadBoolDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Boolean): Boolean; overload; virtual;
 {B}Function ReadBoolDef(const KeyName,ValueName: String; Default: Boolean): Boolean; overload; virtual;
 {C}Function ReadBoolDef(const ValueName: String; Default: Boolean): Boolean; overload; virtual;
 {A}Function ReadInt8Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int8): Int8; overload; virtual;
 {B}Function ReadInt8Def(const KeyName,ValueName: String; Default: Int8): Int8; overload; virtual;
 {C}Function ReadInt8Def(const ValueName: String; Default: Int8): Int8; overload; virtual;
 {A}Function ReadUInt8Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt8): UInt8; overload; virtual;
 {B}Function ReadUInt8Def(const KeyName,ValueName: String; Default: UInt8): UInt8; overload; virtual;
 {C}Function ReadUInt8Def(const ValueName: String; Default: UInt8): UInt8; overload; virtual;
 {A}Function ReadInt16Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int16): Int16; overload; virtual;
 {B}Function ReadInt16Def(const KeyName,ValueName: String; Default: Int16): Int16; overload; virtual;
 {C}Function ReadInt16Def(const ValueName: String; Default: Int16): Int16; overload; virtual;
 {A}Function ReadUInt16Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt16): UInt16; overload; virtual;
 {B}Function ReadUInt16Def(const KeyName,ValueName: String; Default: UInt16): UInt16; overload; virtual;
 {C}Function ReadUInt16Def(const ValueName: String; Default: UInt16): UInt16; overload; virtual;
 {A}Function ReadInt32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int32): Int32; overload; virtual;
 {B}Function ReadInt32Def(const KeyName,ValueName: String; Default: Int32): Int32; overload; virtual;
 {C}Function ReadInt32Def(const ValueName: String; Default: Int32): Int32; overload; virtual;
 {A}Function ReadUInt32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt32): UInt32; overload; virtual;
 {B}Function ReadUInt32Def(const KeyName,ValueName: String; Default: UInt32): UInt32; overload; virtual;
 {C}Function ReadUInt32Def(const ValueName: String; Default: UInt32): UInt32; overload; virtual;
 {A}Function ReadInt64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int64): Int64; overload; virtual;
 {B}Function ReadInt64Def(const KeyName,ValueName: String; Default: Int64): Int64; overload; virtual;
 {C}Function ReadInt64Def(const ValueName: String; Default: Int64): Int64; overload; virtual;
 {A}Function ReadUInt64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt64): UInt64; overload; virtual;
 {B}Function ReadUInt64Def(const KeyName,ValueName: String; Default: UInt64): UInt64; overload; virtual;
 {C}Function ReadUInt64Def(const ValueName: String; Default: UInt64): UInt64; overload; virtual;
 {A}Function ReadIntegerDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Integer): Integer; overload; virtual;
 {B}Function ReadIntegerDef(const KeyName,ValueName: String; Default: Integer): Integer; overload; virtual;
 {C}Function ReadIntegerDef(const ValueName: String; Default: Integer): Integer; overload; virtual;
 {A}Function ReadFloat32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Float32): Float32; overload; virtual;
 {B}Function ReadFloat32Def(const KeyName,ValueName: String; Default: Float32): Float32; overload; virtual;
 {C}Function ReadFloat32Def(const ValueName: String; Default: Float32): Float32; overload; virtual;
 {A}Function ReadFloat64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Float64): Float64; overload; virtual;
 {B}Function ReadFloat64Def(const KeyName,ValueName: String; Default: Float64): Float64; overload; virtual;
 {C}Function ReadFloat64Def(const ValueName: String; Default: Float64): Float64; overload; virtual;
 {A}Function ReadFloatDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Double): Double; overload; virtual;
 {B}Function ReadFloatDef(const KeyName,ValueName: String; Default: Double): Double; overload; virtual;
 {C}Function ReadFloatDef(const ValueName: String; Default: Double): Double; overload; virtual;
 {A}Function ReadCurrencyDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Currency): Currency; overload; virtual;
 {B}Function ReadCurrencyDef(const KeyName,ValueName: String; Default: Currency): Currency; overload; virtual;
 {C}Function ReadCurrencyDef(const ValueName: String; Default: Currency): Currency; overload; virtual;
 {A}Function ReadDateTimeDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {B}Function ReadDateTimeDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {C}Function ReadDateTimeDef(const ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {A}Function ReadDateDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {B}Function ReadDateDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {C}Function ReadDateDef(const ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {A}Function ReadTimeDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {B}Function ReadTimeDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {C}Function ReadTimeDef(const ValueName: String; Default: TDateTime): TDateTime; overload; virtual;
 {A}Function ReadStringDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Default: String): String; overload; virtual;
 {B}Function ReadStringDef(const KeyName,ValueName: String; const Default: String): String; overload; virtual;
 {C}Function ReadStringDef(const ValueName: String; const Default: String): String; overload; virtual;
 {A}Function ReadExpandStringDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Default: String; Expand: Boolean = False): String; overload; virtual;
 {B}Function ReadExpandStringDef(const KeyName,ValueName: String; const Default: String; Expand: Boolean = False): String; overload; virtual;
 {C}Function ReadExpandStringDef(const ValueName: String; const Default: String; Expand: Boolean = False): String; overload; virtual;
    //--- values read ---
 {A}Function ReadBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean; overload; virtual;
 {B}Function ReadBool(const KeyName,ValueName: String): Boolean; overload; virtual;
 {C}Function ReadBool(const ValueName: String): Boolean; overload; virtual;
 {A}Function ReadInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int8; overload; virtual;
 {B}Function ReadInt8(const KeyName,ValueName: String): Int8; overload; virtual;
 {C}Function ReadInt8(const ValueName: String): Int8; overload; virtual;
 {A}Function ReadUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt8; overload; virtual;
 {B}Function ReadUInt8(const KeyName,ValueName: String): UInt8; overload; virtual;
 {C}Function ReadUInt8(const ValueName: String): UInt8; overload; virtual;
 {A}Function ReadInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int16; overload; virtual;
 {B}Function ReadInt16(const KeyName,ValueName: String): Int16; overload; virtual;
 {C}Function ReadInt16(const ValueName: String): Int16; overload; virtual;
 {A}Function ReadUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt16; overload; virtual;
 {B}Function ReadUInt16(const KeyName,ValueName: String): UInt16; overload; virtual;
 {C}Function ReadUInt16(const ValueName: String): UInt16; overload; virtual;
 {A}Function ReadInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int32; overload; virtual;
 {B}Function ReadInt32(const KeyName,ValueName: String): Int32; overload; virtual;
 {C}Function ReadInt32(const ValueName: String): Int32; overload; virtual;
 {A}Function ReadUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt32; overload; virtual;
 {B}Function ReadUInt32(const KeyName,ValueName: String): UInt32; overload; virtual;
 {C}Function ReadUInt32(const ValueName: String): UInt32; overload; virtual;
 {A}Function ReadInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int64; overload; virtual;
 {B}Function ReadInt64(const KeyName,ValueName: String): Int64; overload; virtual;
 {C}Function ReadInt64(const ValueName: String): Int64; overload; virtual;
 {A}Function ReadUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt64; overload; virtual;
 {B}Function ReadUInt64(const KeyName,ValueName: String): UInt64; overload; virtual;
 {C}Function ReadUInt64(const ValueName: String): UInt64; overload; virtual;
 {A}Function ReadInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Integer; overload; virtual;
 {B}Function ReadInteger(const KeyName,ValueName: String): Integer; overload; virtual;
 {C}Function ReadInteger(const ValueName: String): Integer; overload; virtual;
 {A}Function ReadFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Float32; overload; virtual;
 {B}Function ReadFloat32(const KeyName,ValueName: String): Float32; overload; virtual;
 {C}Function ReadFloat32(const ValueName: String): Float32; overload; virtual;
 {A}Function ReadFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Float64; overload; virtual;
 {B}Function ReadFloat64(const KeyName,ValueName: String): Float64; overload; virtual;
 {C}Function ReadFloat64(const ValueName: String): Float64; overload; virtual;
 {A}Function ReadFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Double; overload; virtual;
 {B}Function ReadFloat(const KeyName,ValueName: String): Double; overload; virtual;
 {C}Function ReadFloat(const ValueName: String): Double; overload; virtual;
 {A}Function ReadCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Currency; overload; virtual;
 {B}Function ReadCurrency(const KeyName,ValueName: String): Currency; overload; virtual;
 {C}Function ReadCurrency(const ValueName: String): Currency; overload; virtual;
 {A}Function ReadDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime; overload; virtual;
 {B}Function ReadDateTime(const KeyName,ValueName: String): TDateTime; overload; virtual;
 {C}Function ReadDateTime(const ValueName: String): TDateTime; overload; virtual;
 {A}Function ReadDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime; overload; virtual;
 {B}Function ReadDate(const KeyName,ValueName: String): TDateTime; overload; virtual;
 {C}Function ReadDate(const ValueName: String): TDateTime; overload; virtual;
 {A}Function ReadTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime; overload; virtual;
 {B}Function ReadTime(const KeyName,ValueName: String): TDateTime; overload; virtual;
 {C}Function ReadTime(const ValueName: String): TDateTime; overload; virtual;
 {A}Function ReadString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): String; overload; virtual;
 {B}Function ReadString(const KeyName,ValueName: String): String; overload; virtual;
 {C}Function ReadString(const ValueName: String): String; overload; virtual;
 {A}Function ReadExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Expand: Boolean = False): String; overload; virtual;
 {B}Function ReadExpandString(const KeyName,ValueName: String; Expand: Boolean = False): String; overload; virtual;
 {C}Function ReadExpandString(const ValueName: String; Expand: Boolean = False): String; overload; virtual;
 {A}procedure ReadMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings); overload; virtual;
 {B}procedure ReadMultiString(const KeyName,ValueName: String; Value: TStrings); overload; virtual;
 {C}procedure ReadMultiString(const ValueName: String; Value: TStrings); overload; virtual;
  {
    ReadBinaryBuffer and ReadBinaryMemory are returning number of bytes actally
    stored in the provided buffer/memory.
  }
 {A}Function ReadBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Buff; Size: TMemSize): TMemSize; overload; virtual;
 {B}Function ReadBinaryBuffer(const KeyName,ValueName: String; out Buff; Size: TMemSize): TMemSize; overload; virtual;
 {C}Function ReadBinaryBuffer(const ValueName: String; out Buff; Size: TMemSize): TMemSize; overload; virtual;
 {A}Function ReadBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize; overload; virtual;
 {B}Function ReadBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize; overload; virtual;
 {C}Function ReadBinaryMemory(const ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize; overload; virtual;
  {
    ReadBinaryMemoryOut returns internally alocated memory space that is
    containing the read data along with its size. Use standard memory
    management functions to free this space.
  }
 {A}Function ReadBinaryMemoryOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Memory: Pointer): TMemSize; overload; virtual;
 {B}Function ReadBinaryMemoryOut(const KeyName,ValueName: String; out Memory: Pointer): TMemSize; overload; virtual;
 {C}Function ReadBinaryMemoryOut(const ValueName: String; out Memory: Pointer): TMemSize; overload; virtual;
 {A}procedure ReadBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream); overload; virtual;
 {B}procedure ReadBinaryStream(const KeyName,ValueName: String; Stream: TStream); overload; virtual;
 {C}procedure ReadBinaryStream(const ValueName: String; Stream: TStream); overload; virtual;
    //--- properties ---
  {
    Following access rights will be used in next call to OpenKey.
  }
    property AccessRightsSys: DWORD read fAccessRightsSys write SetAccessRightsSys;
    property AccessRights: TRXKeyAccessRights read fAccessRights write SetAccessRights;
  {
    Changing the root key will close current key if any is open.
  }
    property RootKeyHandle: HKEY read fRootKeyHandle write SetRootKeyHandle;
    property RootKey: TRXPredefinedKey read fRootKey write SetRootKey;
    property RootKeyString: String read GetRootKeyString;
    property RootKeyShortString: String read GetRootKeyShortString;
    property RemoteRootKey: Boolean read fRemoteRootKey;
    property CurrentKeyHandle: HKEY read fCurrentKeyHandle;
    property CurrentKeyName: String read fCurrentKeyName;
  {
    CurrentKeyAccessRights stores access rights with which the current key was
    opened.
  }
    property CurrentKeyAccessRights: TRXKeyAccessRights read fCurrentKeyAccessRights;
  {
    If there is no current key open, then CurrentKeyReflection returns false
    and changing it has no effect.
    Also, this settings is working only on 64bit system, it has no effect and
    returns false on 32bit systems.
  }
    property CurrentKeyReflection: Boolean read GetCurrentKeyReflection write SetCurrentKeyReflection;
    property CurrentKeyPath: String read GetCurrentKeyPath;
  {
    When FlushOnClose is set to true (by default false), the current key is
    flushed before it is closed - meaning all changes made to it are immediately
    saved and are not buffered.
    Use this only when really needed, as it negatively affects performance.
  }
    property FlushOnClose: Boolean read fFlushOnClose write fFlushOnClose;
    property LastSystemError: Integer read fLastSysError;
  {
    SecurityDescriptorCopyInfo denotes what information will be loaded and
    saved when copying security descriptors (used in key copy, move and rename).
  }
    property SecurityDescriptorCopyInfo: DWORD read fSecDesrCopyInfo write fSecDesrCopyInfo;
  end;

implementation

uses
  StrUtils,
  StrRect, DynLibUtils, WindowsVersion;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TRegistryEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRegistryEx - external/system functions and constants
===============================================================================}
const
  UNICODE_STRING_MAX_CHARS     = 32767;
  _DELETE                      = $00010000; // cannot use "DELETE" - conflict with function of the same name
  READ_CONTROL                 = $00020000;
  ERROR_INCORRECT_SIZE         = 1462;
  ERROR_DATATYPE_MISMATCH      = 1629;
  ERROR_GENERIC_COMMAND_FAILED = 14109;
  BELOW_NORMAL_PRIORITY_CLASS  = $00004000;

type
  LONG              = LongInt;
  LPBYTE            = ^Byte;
  LSTATUS           = Int32;
  HANDLE            = THandle;
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;
  LUID              = Int64;

//------------------------------------------------------------------------------
// statically linked functions

TStartupInfoW = record
  cb:               DWORD;
  lpReserved:       LPWSTR;
  lpDesktop:        LPWSTR;
  lpTitle:          LPWSTR;
  dwX:              DWORD;
  dwY:              DWORD;
  dwXSize:          DWORD;
  dwYSize:          DWORD;
  dwXCountChars:    DWORD;
  dwYCountChars:    DWORD;
  dwFillAttribute:  DWORD;
  dwFlags:          DWORD;
  wShowWindow:      WORD;
  cbReserved2:      WORD;
  lpReserved2:      LPBYTE;
  hStdInput:        HANDLE;
  hStdOutput:       HANDLE;
  hStdError:        HANDLE;
end;
PStartupInfoW = ^TStartupInfoW;

Function CreateProcessW(
  lpApplicationName:    LPCWSTR;
  lpCommandLine:        LPWSTR;
  lpProcessAttributes:  PSecurityAttributes;
  lpThreadAttributes:   PSecurityAttributes;
  bInheritHandles:      BOOL;
  dwCreationFlags:      DWORD;
  lpEnvironment:        Pointer;
  lpCurrentDirectory:   LPCWSTR;
  lpStartupInfo:        PStartupInfoW;
  lpProcessInformation: PProcessInformation): BOOL; stdcall; external 'kernel32.dll';

Function GetSystemRegistryQuota(pdwQuotaAllowed: PDWORD; pdwQuotaUsed: PDWORD): BOOL; stdcall; external 'kernel32.dll';

Function RegOverridePredefKey(hKey: HKEY; hNewHKey: HKEY): LONG; stdcall; external 'advapi32.dll';

Function RegDisablePredefinedCache: LONG; stdcall; external 'advapi32.dll';

Function RegOpenCurrentUser(samDesired: REGSAM; phkResult: PHKEY): LONG; stdcall; external 'advapi32.dll';

Function RegOpenUserClassesRoot(hToken: HANDLE; dwOptions: DWORD; samDesired: REGSAM; phkResult: PHKEY): LONG; stdcall; external 'advapi32.dll';

Function RegEnumValueW(
  hKey:           HKEY;
  dwIndex:        DWORD;
  lpValueName:    LPWSTR;
  lpcchValueName: LPDWORD;
  lpReserved:     LPDWORD;
  lpType:         LPDWORD;
  lpData:         LPBYTE;
  lpcbData:       LPDWORD): LSTATUS; stdcall; external 'advapi32.dll';

Function AdjustTokenPrivileges(
  TokenHandle:          HANDLE;
  DisableAllPrivileges: BOOL;
  NewState:             PTOKEN_PRIVILEGES;
  BufferLength:         DWORD;
  PreviousState:        PTOKEN_PRIVILEGES;
  ReturnLength:         PDWORD): BOOL; stdcall; external 'advapi32.dll';

Function RegSaveKeyExW(hKey: HKEY; lpFile: LPCWSTR; lpSecurityAttributes: PSecurityAttributes; Flags: DWORD): LONG; stdcall; external 'advapi32.dll';

Function SHDeleteKeyW(hkey: HKEY; pszSubKey: LPCWSTR): LSTATUS; stdcall; external 'shlwapi.dll';

Function PathUnExpandEnvStringsW(pszPath: LPCWSTR; pszBuf: LPWSTR; cchBuf: UINT): BOOL; stdcall; external 'shlwapi.dll';

//------------------------------------------------------------------------------
// dynamically linked functions (might not be present on all systems - namely Windows XP)

var
  RegQueryReflectionKey:        Function(hBase: HKEY; bIsReflectionDisabled: PBOOL): LONG; stdcall = nil;
  RegEnableReflectionKey:       Function(hBase: HKEY): LONG; stdcall = nil;
  RegDisableReflectionKey:      Function(hBase: HKEY): LONG; stdcall = nil;
  RegDisablePredefinedCacheEx:  Function: LONG; stdcall = nil;

//------------------------------------------------------------------------------
const
  // default value for SecurityDescriptorCopyInfo property
  SECINFO_NORMAL = OWNER_SECURITY_INFORMATION or
                   GROUP_SECURITY_INFORMATION or
                   DACL_SECURITY_INFORMATION or
                 //SACL_SECURITY_INFORMATION or   // requires special security privilege for the process
                   LABEL_SECURITY_INFORMATION or
                   ATTRIBUTE_SECURITY_INFORMATION or
                   SCOPE_SECURITY_INFORMATION or
                   PROCESS_TRUST_LABEL_SECURITY_INFORMATION or
                   ACCESS_FILTER_SECURITY_INFORMATION or
                   BACKUP_SECURITY_INFORMATION or
                   PROTECTED_DACL_SECURITY_INFORMATION or
                   PROTECTED_SACL_SECURITY_INFORMATION or
                   UNPROTECTED_DACL_SECURITY_INFORMATION or
                   UNPROTECTED_SACL_SECURITY_INFORMATION;

{===============================================================================
    TRegistryEx - internal functions
===============================================================================}
{-------------------------------------------------------------------------------
    TRegistryEx - internal functions - general functions
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  LocalTime:  TFileTime;
  SystemTime: TSystemTime;
begin
If FileTimeToLocalFileTime(FileTime,LocalTime) then
  begin
    If FileTimeToSystemTime(LocalTime,SystemTime) then
      Result := SystemTimeToDateTime(SystemTime)
    else
      raise ERXTimeConversionError.CreateFmt('FileTimeToDateTime: Unable to convert to system time (%d).',[GetLastError]);
  end
else raise ERXTimeConversionError.CreateFmt('FileTimeToDateTime: Unable to convert to local file time (%d).',[GetLastError]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function BoolToNum(Value: Boolean): Integer;
begin
If Value then
  Result := -1
else
  Result := 0;
end;

{-------------------------------------------------------------------------------
    TRegistryEx - internal functions - strings manipulation
-------------------------------------------------------------------------------}

Function WStrLen(const Str: WideString): TStrOff;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to Length(Str) do
  If Str[i] = WideChar(#0) then
    begin
      Result := Pred(i);
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function ExpandString(const Str: WideString): String;
var
  Temp: WideString;
begin
Temp := '';
SetLength(Temp,ExpandEnvironmentStringsW(PWideChar(Str),nil,0));
ExpandEnvironmentStringsW(PWideChar(Str),PWideChar(Temp),Length(Temp));
Result := WideToStr(Copy(Temp,1,Length(Temp) - 1));
end;

//------------------------------------------------------------------------------

Function UnExpandString(const Str: String): WideString;
var
  Temp: WideString;
begin
Temp := StrToWide(Str);
Result := '';
SetLength(Result,UNICODE_STRING_MAX_CHARS);
If PathUnExpandEnvStringsW(PWideChar(Temp),PWideChar(Result),Length(Result)) then
  SetLength(Result,WStrLen(Result))
else
  Result := StrToWide(Str);
end;

//------------------------------------------------------------------------------

procedure ParseMultiString(const Str: WideString; Strs: TStrings);
var
  i:    TStrOff;
  S,L:  TStrOff;
begin
Strs.Clear;
If Length(Str) > 0 then
  begin
    // parse the multi-string
    S := 1;
    L := 0;
    For i := 1 to Length(Str) do
      If Str[i] = WideChar(#0) then
        begin
          Strs.Add(WideToStr(Copy(Str,S,L)));
          S := Succ(i);
          L := 0;
        end
      else Inc(L);
  end;
end;

//------------------------------------------------------------------------------

Function UnParseMultiString(Strs: TStrings): WideString;
var
  Item:     WideString;
  i:        Integer;
  Len,Pos:  Integer;
begin
If Strs.Count > 0 then
  begin
    // calculate final length/size of saved data
    Len := 1; // list ternimation
    For i := 0 to Pred(Strs.Count) do
      Len := Len + Length(StrToWide(Strs[i])) + 1;
    // preallocate result
    Result := '';
    SetLength(Result,Len);
    FillChar(PWideChar(Result)^,Length(Result) * SizeOf(WideChar),0);
    // fill result
    Pos := 1;
    For i := 0 to Pred(Strs.Count) do
      begin
        Item := StrToWide(Strs[i]);
        Move(PWideChar(Item)^,Addr(Result[Pos])^,Length(Item) * SizeOf(WideChar));
        Pos := Pos + Length(Item) + 1;
      end;
  end
else Result := WideChar(#0);
end;

{-------------------------------------------------------------------------------
    TRegistryEx - internal functions - key names/paths operations
-------------------------------------------------------------------------------}

Function TrimKeyName(const KeyName: String): String;
begin
{
  Remove all path delimiters from both sides of the given string.
}
If Length(KeyName) > 1 then
  begin
    Result := KeyName;
    while Result[1] = REG_PATH_DELIMITER do
      Delete(Result,1,1);
    while Result[Length(Result)] = REG_PATH_DELIMITER do
      Delete(Result,Length(Result),1);
  end
else If Length(KeyName) = 1 then
  begin
    If KeyName[1] = REG_PATH_DELIMITER then
      Result := ''
    else
      Result := KeyName;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function IsRelativeKeyName(const KeyName: String): Boolean;
begin
{
  If the key name starts with register path delimiter (backslash, \, #92), it
  is considered to be an absolute path within the root key, otherwise it is
  considered to be relative to current key.
}
If Length(KeyName) > 0 then
  Result := KeyName[1] <> REG_PATH_DELIMITER
else
  Result := True;
end;

//------------------------------------------------------------------------------

Function IsSubKeyOf(const SubKeyName,KeyName: String): Boolean;
begin
If Length(KeyName) > 0 then
  begin
  {
    Key name is not empty, therefore we must check whether the name of given
    subkey starts with a name of this key - if so, then we can assume the given
    subkey to be hierarchically a subkey of given (super)key.

    Also when both names are matching, true is returned.
  }
    If Length(SubKeyName) > 0 then
      begin
        If Length(KeyName) < Length(SubKeyName) then
          Result := AnsiStartsText(KeyName,SubKeyName) and
                    (SubKeyName[Length(KeyName) + 1] = REG_PATH_DELIMITER)
        else
          Result := AnsiSameText(SubKeyName,KeyName);
      end
    else Result := False;
  end
// a key with empty name is a superkey for any other key
else Result := True;
end;

//------------------------------------------------------------------------------

Function IsStrictSubKeyOf(const SubKeyName,KeyName: String): Boolean;
begin
If Length(KeyName) > 0 then
  begin
    If Length(SubKeyName) > 0 then
      begin
        If Length(KeyName) < Length(SubKeyName) then
          Result := AnsiStartsText(KeyName,SubKeyName) and
                    (SubKeyName[Length(KeyName) + 1] = REG_PATH_DELIMITER)
        else
          Result := False;
      end
    else Result := False;
  end
else Result := Length(SubKeyName) > 0;
end;

//------------------------------------------------------------------------------

Function ConcatKeyNames(const A,B: String): String;
begin
// both A and B are expected to be trimmed
If (Length(A) > 0) and (Length(B) > 0) then
  Result := A + REG_PATH_DELIMITER + B
else If Length(A) > 0 then
  Result := A
else If Length(B) > 0 then
  Result := B
else
  Result := '';
end;

//------------------------------------------------------------------------------

Function ReplaceSuperKey(const KeyName,FromSuperKey,ToSuperKey: String; out ReplacedPath: String): Boolean;
begin
ReplacedPath := '';
Result := True;
If Length(KeyName) > 0 then
  begin
    If Length(FromSuperKey) > 0 then
      begin
        If Length(KeyName) > Length(FromSuperKey) then
          begin
            If AnsiStartsText(FromSuperKey,KeyName) and (KeyName[Length(FromSuperKey) + 1] = REG_PATH_DELIMITER) then
              ReplacedPath := ConcatKeyNames(ToSuperKey,Copy(KeyName,Length(FromSuperKey) + 2,Length(KeyName)))
            else
              Result := False;
          end
        else If AnsiSameText(KeyName,FromSuperKey) then
          ReplacedPath := ToSuperKey
        else
          Result := False;
      end
    else ReplacedPath := ConcatKeyNames(ToSuperKey,KeyName);
  end
else If Length(FromSuperKey) <= 0 then
  ReplacedPath := ToSuperKey
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function IsMultiLevelKeyPath(const KeyName: String): Boolean;
var
  i:  TStrOff;
begin
Result := False;
For i := 1 to Length(KeyName) do
  If KeyName[i] = REG_PATH_DELIMITER then
    begin
      Result := True;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function RemoveLowestKeyPathLevel(const KeyName: String): String;
var
  i:  TStrOff;
begin
If Length(KeyName) > 0 then
  begin
    i := Length(KeyName);
    while i >= 1 do
      begin
        If KeyName[i] = REG_PATH_DELIMITER then
          Break{while}
        else
          Dec(i);
      end;
    If i > 0 then
      Result := Copy(KeyName,1,i - 1)
    else
      Result := '';
  end
else Result := '';
end;

{-------------------------------------------------------------------------------
    TRegistryEx - internal functions - key handles operations
-------------------------------------------------------------------------------}

Function IsPredefinedKey(Key: HKEY): Boolean;
begin
If (Key and $80000000) <> 0 then
  Result := Byte(Key and $FF) in [0,1,2,3,4,5,6,7,$50,$60]
else
  Result := False;
end;

{-------------------------------------------------------------------------------
    TRegistryEx - internal functions - translations and conversions
-------------------------------------------------------------------------------}

Function TranslateAccessRights(AccessRights: DWORD): TRXKeyAccessRights; overload;

  procedure SetResultAccessRight(Flag: DWORD; AccessRight: TRXKeyAccessRight);
  begin
    If AccessRights and Flag = Flag then
      Include(Result,AccessRight);
  end;

begin
Result := [];
SetResultAccessRight(KEY_QUERY_VALUE,karQueryValue);
SetResultAccessRight(KEY_SET_VALUE,karSetValue);
SetResultAccessRight(KEY_CREATE_SUB_KEY,karCreateSubKey);
SetResultAccessRight(KEY_ENUMERATE_SUB_KEYS,karEnumerateSubKeys);
SetResultAccessRight(KEY_NOTIFY,karNotify);
SetResultAccessRight(KEY_CREATE_LINK,karCreateLink);
SetResultAccessRight(KEY_WOW64_32KEY,karWoW64_32Key);
SetResultAccessRight(KEY_WOW64_64KEY,karWoW64_64Key);
SetResultAccessRight(_DELETE,karDelete);
SetResultAccessRight(READ_CONTROL,karReadControl);
SetResultAccessRight(WRITE_DAC,karWriteDAC);
SetResultAccessRight(WRITE_OWNER,karWriteOwner);
SetResultAccessRight(SYNCHRONIZE,karSynchronize);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TranslateAccessRights(AccessRights: TRXKeyAccessRights): DWORD; overload;

  procedure SetResultAccessRight(AccessRight: TRXKeyAccessRight; Flag: DWORD);
  begin
    If AccessRight in AccessRights then
      Result := Result or Flag;
  end;

begin
Result := 0;
SetResultAccessRight(karQueryValue,KEY_QUERY_VALUE);
SetResultAccessRight(karSetValue,KEY_SET_VALUE);
SetResultAccessRight(karCreateSubKey,KEY_CREATE_SUB_KEY);
SetResultAccessRight(karEnumerateSubKeys,KEY_ENUMERATE_SUB_KEYS);
SetResultAccessRight(karNotify,KEY_NOTIFY);
SetResultAccessRight(karCreateLink,KEY_CREATE_LINK);
SetResultAccessRight(karWoW64_32Key,KEY_WOW64_32KEY);
SetResultAccessRight(karWoW64_64Key,KEY_WOW64_64KEY);
SetResultAccessRight(karDelete,_DELETE);
SetResultAccessRight(karReadControl,READ_CONTROL);
SetResultAccessRight(karWriteDAC,WRITE_DAC);
SetResultAccessRight(karWriteOwner,WRITE_OWNER);
SetResultAccessRight(karSynchronize,SYNCHRONIZE);
end;

//------------------------------------------------------------------------------

Function TranslateCreateOptions(CreateOptions: TRXKeyCreateOptions): DWORD;

  procedure SetResultCreateOption(CreateOption: TRXKeyCreateOption; Flag: DWORD);
  begin
    If CreateOption in CreateOptions then
      Result := Result or Flag;
  end;

begin
Result := 0;
//SetResultCreateOption(kcoNonVolatile,REG_OPTION_NON_VOLATILE);  // REG_OPTION_NON_VOLATILE is 0, so not needed
SetResultCreateOption(kcoVolatile,REG_OPTION_VOLATILE);
SetResultCreateOption(kcoCreateLink,REG_OPTION_CREATE_LINK);
SetResultCreateOption(kcoBackupRestore,REG_OPTION_BACKUP_RESTORE);
SetResultCreateOption(kcoOpenLink,REG_OPTION_OPEN_LINK);
SetResultCreateOption(kcoDontVirtualize,REG_OPTION_DONT_VIRTUALIZE);
end;

//------------------------------------------------------------------------------

Function TranslateValueType(ValueType: DWORD): TRXValueType; overload;
begin
case ValueType of
  REG_NONE:                       Result := vtNone;
  REG_SZ:                         Result := vtString;
  REG_EXPAND_SZ:                  Result := vtExpandString;
  REG_BINARY:                     Result := vtBinary;
  REG_DWORD:                      Result := vtDWord;
//REG_DWORD_LITTLE_ENDIAN:        Result := vtDWordLE;  // the same as REG_DWORD, duplicit label
  REG_DWORD_BIG_ENDIAN:           Result := vtDWordBE;
  REG_LINK:                       Result := vtLink;
  REG_MULTI_SZ:                   Result := vtMultiString;
  REG_RESOURCE_LIST:              Result := vtResourceList;
  REG_FULL_RESOURCE_DESCRIPTOR:   Result := vtFullResourceDescriptor;
  REG_RESOURCE_REQUIREMENTS_LIST: Result := vtResourceRequirementsList;
  REG_QWORD:                      Result := vtQWord;
//REG_QWORD_LITTLE_ENDIAN:        Result := vtQWordLE;  // the same as REG_QWORD, duplicit label
else
  Result := vtUnknown;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TranslateValueType(ValueType: TRXValueType): DWORD; overload;
begin
case ValueType of
  vtNone:                     Result := REG_NONE;
  vtString:                   Result := REG_SZ;
  vtExpandString:             Result := REG_EXPAND_SZ;
  vtBinary:                   Result := REG_BINARY;
  vtDWord:                    Result := REG_DWORD;
  vtDWordLE:                  Result := REG_DWORD_LITTLE_ENDIAN;
  vtDWordBE:                  Result := REG_DWORD_BIG_ENDIAN;
  vtLink:                     Result := REG_LINK;
  vtMultiString:              Result := REG_MULTI_SZ;
  vtResourceList:             Result := REG_RESOURCE_LIST;
  vtFullResourceDescriptor:   Result := REG_FULL_RESOURCE_DESCRIPTOR;
  vtResourceRequirementsList: Result := REG_RESOURCE_REQUIREMENTS_LIST;
  vtQWord:                    Result := REG_QWORD;
  vtQWordLE:                  Result := REG_QWORD_LITTLE_ENDIAN;
else
 {rvtUnknown}
  Result := REG_NONE;
end;
end;

//------------------------------------------------------------------------------

Function TranslatePredefinedKey(PredefinedKey: HKEY): TRXPredefinedKey; overload;
begin
case PredefinedKey of
  HKEY_CLASSES_ROOT:                Result := pkClassesRoot;
  HKEY_CURRENT_USER:                Result := pkCurrentUser;
  HKEY_LOCAL_MACHINE:               Result := pkLocalMachine;
  HKEY_USERS:                       Result := pkUsers;
  HKEY_PERFORMANCE_DATA:            Result := pkPerformanceData;
  HKEY_PERFORMANCE_TEXT:            Result := pkPerformanceText;
  HKEY_PERFORMANCE_NLSTEXT:         Result := pkPerformanceNLSText;
  HKEY_CURRENT_CONFIG:              Result := pkCurrentConfig;
  HKEY_DYN_DATA:                    Result := pkDynData;
  HKEY_CURRENT_USER_LOCAL_SETTINGS: Result := pkCurrentUserLocalSettings;
else
  raise ERXInvalidValue.CreateFmt('TranslatePredefinedKey: Invalid key (%d).',[PredefinedKey]);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TranslatePredefinedKey(PredefinedKey: TRXPredefinedKey): HKEY; overload;
begin
case PredefinedKey of
  pkClassesRoot:              Result := HKEY_CLASSES_ROOT;
  pkCurrentUser:              Result := HKEY_CURRENT_USER;
  pkLocalMachine:             Result := HKEY_LOCAL_MACHINE;
  pkUsers:                    Result := HKEY_USERS;
  pkPerformanceData:          Result := HKEY_PERFORMANCE_DATA;
  pkPerformanceText:          Result := HKEY_PERFORMANCE_TEXT;
  pkPerformanceNLSText:       Result := HKEY_PERFORMANCE_NLSTEXT;
  pkCurrentConfig:            Result := HKEY_CURRENT_CONFIG;
  pkDynData:                  Result := HKEY_DYN_DATA;
  pkCurrentUserLocalSettings: Result := HKEY_CURRENT_USER_LOCAL_SETTINGS;
else
  raise ERXInvalidValue.CreateFmt('TranslatePredefinedKey: Invalid key (%d).',[Ord(PredefinedKey)]);
end;
end;

//------------------------------------------------------------------------------

Function PredefinedKeyToStr(PredefinedKey: HKEY): String; overload;
begin
case PredefinedKey of
  HKEY_CLASSES_ROOT:                Result := 'HKEY_CLASSES_ROOT';
  HKEY_CURRENT_USER:                Result := 'HKEY_CURRENT_USER';
  HKEY_LOCAL_MACHINE:               Result := 'HKEY_LOCAL_MACHINE';
  HKEY_USERS:                       Result := 'HKEY_USERS';
  HKEY_PERFORMANCE_DATA:            Result := 'HKEY_PERFORMANCE_DATA';
  HKEY_PERFORMANCE_TEXT:            Result := 'HKEY_PERFORMANCE_TEXT';
  HKEY_PERFORMANCE_NLSTEXT:         Result := 'HKEY_PERFORMANCE_NLSTEXT';
  HKEY_CURRENT_CONFIG:              Result := 'HKEY_CURRENT_CONFIG';
  HKEY_DYN_DATA:                    Result := 'HKEY_DYN_DATA';
  HKEY_CURRENT_USER_LOCAL_SETTINGS: Result := 'HKEY_CURRENT_USER_LOCAL_SETTINGS';
else
  Result := '';
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PredefinedKeyToStr(PredefinedKey: TRXPredefinedKey): String; overload;
begin
Result := PredefinedKeyToStr(TranslatePredefinedKey(PredefinedKey));
end;

//------------------------------------------------------------------------------

Function PredefinedKeyToShortStr(PredefinedKey: HKEY): String; overload;
begin
case PredefinedKey of
  HKEY_CLASSES_ROOT:                Result := 'HKCR';
  HKEY_CURRENT_USER:                Result := 'HKCU';
  HKEY_LOCAL_MACHINE:               Result := 'HKLM';
  HKEY_USERS:                       Result := 'HKU';
  HKEY_PERFORMANCE_DATA:            Result := 'HKPD';
  HKEY_PERFORMANCE_TEXT:            Result := 'HKPT';
  HKEY_PERFORMANCE_NLSTEXT:         Result := 'HKPN';
  HKEY_CURRENT_CONFIG:              Result := 'HKCC';
  HKEY_DYN_DATA:                    Result := 'HKDD';
  HKEY_CURRENT_USER_LOCAL_SETTINGS: Result := 'HKLS';
else
  Result := '';
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PredefinedKeyToShortStr(PredefinedKey: TRXPredefinedKey): String; overload;
begin
Result := PredefinedKeyToShortStr(TranslatePredefinedKey(PredefinedKey));
end;

//------------------------------------------------------------------------------

Function TranslateFileFormat(FileFormat: TRXFileFormat): DWORD;
begin
case FileFormat of
  ffStandardFormat: Result := REG_STANDARD_FORMAT;
  ffLatestFormat:   Result := REG_LATEST_FORMAT;
  ffNoCompression:  Result := REG_NO_COMPRESSION;
else
  raise ERXInvalidValue.CreateFmt('TranslateFileFormat: Invalid format (%d).',[Ord(FileFormat)]);
end;
end;

//------------------------------------------------------------------------------

Function TranslateRestoreFlag(RestoreFlag: TRXRestoreFlag): DWORD;
begin
case RestoreFlag of
  rfNone:                     Result := 0;
  rfWholeHiveVolatile:        Result := REG_WHOLE_HIVE_VOLATILE;
  rfRefreshHive:              Result := REG_REFRESH_HIVE;
  rfNoLazyFlush:              Result := REG_NO_LAZY_FLUSH;
  rfForceRestore:             Result := REG_FORCE_RESTORE;
  rfAppHive:                  Result := REG_APP_HIVE;
  rfProcessPrivate:           Result := REG_PROCESS_PRIVATE;
  rfStartJournal:             Result := REG_START_JOURNAL;
  rfHiveExactFileGrowth:      Result := REG_HIVE_EXACT_FILE_GROWTH;
  rfHiveNoRM:                 Result := REG_HIVE_NO_RM;
  rfHiveSingleLog:            Result := REG_HIVE_SINGLE_LOG;
  rfBootHive:                 Result := REG_BOOT_HIVE;
  rfLoadHiveOpenHandle:       Result := REG_LOAD_HIVE_OPEN_HANDLE;
  rfFlushHiveFileGrowth:      Result := REG_FLUSH_HIVE_FILE_GROWTH;
  rfOpenReadOnly:             Result := REG_OPEN_READ_ONLY;
  rfImmutable:                Result := REG_IMMUTABLE;
  rfNoImpersonationFallback:  Result := REG_NO_IMPERSONATION_FALLBACK;
  rfAppHiveOpenReadOnly:      Result := REG_APP_HIVE_OPEN_READ_ONLY;
else
  raise ERXInvalidValue.CreateFmt('TranslateRestoreFlag: Invalid flag (%d).',[Ord(RestoreFlag)]);
end;
end;

//------------------------------------------------------------------------------

Function TranslateNotifyFilter(NotifyFilter: TRXNotifyFilter): DWORD;

  procedure SetNotifyFilterOption(NotifyFilterOption: TRXNotifyFilterOption; Flag: DWORD);
  begin
    If NotifyFilterOption in NotifyFilter then
      Result := Result or Flag;
  end;

begin
Result := 0;
SetNotifyFilterOption(noNameChange,REG_NOTIFY_CHANGE_NAME);
SetNotifyFilterOption(noAttributeChange,REG_NOTIFY_CHANGE_ATTRIBUTES);
SetNotifyFilterOption(noLastSetChange,REG_NOTIFY_CHANGE_LAST_SET);
SetNotifyFilterOption(noSecurityChange,REG_NOTIFY_CHANGE_SECURITY);
SetNotifyFilterOption(noThreadAgnostic,REG_NOTIFY_THREAD_AGNOSTIC);
end;

{===============================================================================
    TRegistryEx - public functions
===============================================================================}

Function WaitResultToStr(WaitResult: TRXWaitResult): String;
begin
case WaitResult of
  wrError:    Result := 'Error';
  wrTimeout:  Result := 'Timeout';
  wrChanged:  Result := 'Changed';
else
  Result := '<invalid>';
end;
end;

{===============================================================================
    TRegistryEx - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TRegistryEx - protected methods
-------------------------------------------------------------------------------}

procedure TRegistryEx.SetAccessRightsSys(Value: DWORD);
begin
fAccessRightsSys := Value;
fAccessRights := TranslateAccessRights(Value);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.SetAccessRights(Value: TRXKeyAccessRights);
begin
fAccessRightsSys := TranslateAccessRights(Value);
fAccessRights := Value;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.SetRootKeyHandle(Value: HKEY);
begin
If Value <> fRootKeyHandle then
  begin
    CloseKey;
    ChangeRootKey(Value,TranslatePredefinedKey(Value));
  end;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.SetRootKey(Value: TRXPredefinedKey);
begin
If Value <> fRootKey then
  begin
    CloseKey;
    ChangeRootKey(TranslatePredefinedKey(Value),Value);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetRootKeyString: String;
begin
Result := PredefinedKeyToStr(fRootKey);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetRootKeyShortString: String;
begin
Result := PredefinedKeyToShortStr(fRootKey);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetCurrentKeyReflection: Boolean;
var
  Value:  BOOL;
begin
If fCurrentKeyHandle <> 0 then
  begin
    If Assigned(RegQueryReflectionKey) then
      begin
        If CheckErrorCode(RegQueryReflectionKey(fCurrentKeyHandle,@Value)) then
          Result := not Value // RegQueryReflectionKey indicates whether the reflection is DISABLED
        else
          Result := False;
      end
    else
      begin
        SetErrorCode(ERROR_CALL_NOT_IMPLEMENTED);
        Result := False;
      end;
  end
else
  begin
    SetErrorCode(ERROR_INVALID_HANDLE);
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.SetCurrentKeyReflection(Value: Boolean);
begin
If fCurrentKeyHandle <> 0 then
  begin
    If Value then
      begin
        If Assigned(RegEnableReflectionKey) then
          SetErrorCode(RegEnableReflectionKey(fCurrentKeyHandle))
        else
          SetErrorCode(ERROR_CALL_NOT_IMPLEMENTED);
      end
    else
      begin
        If Assigned(RegDisableReflectionKey) then
          SetErrorCode(RegDisableReflectionKey(fCurrentKeyHandle))
        else
          SetErrorCode(ERROR_CALL_NOT_IMPLEMENTED);
      end;
  end
else SetErrorCode(ERROR_INVALID_HANDLE);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetCurrentKeyPath: String;
begin
If fCurrentKeyHandle <> 0 then
  Result := ConcatKeyNames(RootKeyString,fCurrentKeyName)
else
  Result := '';
end;

//------------------------------------------------------------------------------

Function TRegistryEx.SetErrorCode(ErrorCode: Integer): Integer;
begin
fLastSysError := ErrorCode;
Result := ErrorCode;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CheckErrorCode(ErrorCode: Integer; AllowMoreDataErr: Boolean = False): Boolean;
begin
fLastSysError := ErrorCode;
If AllowMoreDataErr then
  Result := ErrorCode in [ERROR_SUCCESS,ERROR_MORE_DATA]
else
  Result := ErrorCode = ERROR_SUCCESS;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.SysCallError(CallResult: BOOL): Boolean;
begin
fLastSysError := GetLastError;
Result := CallResult;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.WaitForKeyChange(Key: HKEY; Filter: DWORD; WatchSubTree: Boolean; Timeout: DWORD): TRXWaitResult;
var
  WaitEvent:  THandle;
begin
Result := wrError;
WaitEvent := CreateEvent(nil,True,False,nil);
If WaitEvent <> 0 then
  try
    If CheckErrorCode(RegNotifyChangeKeyValue(Key,WatchSubTree,Filter,WaitEvent,True)) then
      case WaitForSingleObject(WaitEvent,Timeout) of
        WAIT_OBJECT_0:  Result := wrChanged;
        WAIT_TIMEOUT:   Result := wrTimeout;
        WAIT_FAILED:    begin
                          Result := wrError;
                          SetErrorCode(GetLastError);
                        end;
      else
        Result := wrError;
        SetErrorCode(ERROR_INVALID_DATA);
      end;
  finally
    CloseHandle(WaitEvent);
  end
else SetErrorCode(GetLastError);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.AuxCreateKey(BaseKey: HKEY; const KeyName: String; AccessRights: DWORD; out NewKey: HKEY; CreateOptions: DWORD = REG_OPTION_NON_VOLATILE; Disposition: LPDWORD = nil): Boolean;
begin
NewKey := 0;
Result := False;
If Length(KeyName) > 0 then
  Result := CheckErrorCode(RegCreateKeyExW(BaseKey,PWideChar(StrToWide(KeyName)),
                           0,nil,CreateOptions,AccessRights,nil,NewKey,Disposition))
else
  SetErrorCode(ERROR_INVALID_PARAMETER);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.AuxOpenKey(BaseKey: HKEY; const KeyName: String; AccessRights: DWORD; out NewKey: HKEY): Boolean;
begin
NewKey := 0;
If Length(KeyName) > 0 then
  Result := CheckErrorCode(RegOpenKeyExW(BaseKey,PWideChar(StrToWide(KeyName)),0,AccessRights,NewKey))
else If IsPredefinedKey(BaseKey) then
  begin
    NewKey := BaseKey;
    Result := True;
  end
else Result := CheckErrorCode(RegOpenKeyExW(BaseKey,nil,0,AccessRights,NewKey));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.AuxCloseKey(Key: HKEY): Boolean;
begin
{
  Do not set fLastSysError here, AuxCloseKey is never a reason for failure of
  method that is calling it.
}
If IsPredefinedKey(Key) then
  Result := True
else
  Result := RegCloseKey(Key) = ERROR_SUCCESS;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ChangeRootKey(KeyHandle: HKEY; Key: TRXPredefinedKey);
begin
If fCloseRootKey then
  RegCloseKey(fRootKeyHandle);
fRootKeyHandle := KeyHandle;
fRootKey := Key;
fRemoteRootKey := False;
fCloseRootKey := False;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ChangeCurrentKey(KeyHandle: HKEY; const KeyName: String);
begin
fCurrentKeyHandle := KeyHandle;
fCurrentKeyName := KeyName;
fCurrentKeyAccessRights := fAccessRights;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetWorkingKey(Relative: Boolean; out WorkingKeyName: String): HKEY;
begin
If Relative and (fCurrentKeyHandle <> 0) then
  begin
    WorkingKeyName := fCurrentKeyName;
    Result := fCurrentKeyHandle;
  end
else
  begin
    WorkingKeyName := '';
    Result := fRootKeyHandle;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetWorkingKey(Relative: Boolean): HKEY;
var
  WorkingKeyName: String;
begin
Result := GetWorkingKey(Relative,WorkingKeyName);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CurrentKeyIsSubKeyOf(RootKey: TRXPredefinedKey; const KeyName: String; Strict: Boolean): Boolean;
begin
If (fCurrentKeyHandle <> 0) and (TranslatePredefinedKey(RootKey) = fRootKeyHandle) then
  begin
    If Strict then
      Result := IsStrictSubKeyOf(fCurrentKeyName,KeyName)
    else
      Result := IsSubKeyOf(fCurrentKeyName,KeyName)
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CurrentKeyIsSubKeyOf(const KeyName: String; Strict: Boolean): Boolean;
begin
If fCurrentKeyHandle <> 0 then
  begin
    If Strict then
      Result := IsStrictSubKeyOf(fCurrentKeyName,KeyName)
    else
      Result := IsSubKeyOf(fCurrentKeyName,KeyName)
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetKeyInfo(Key: HKEY; out KeyInfo: TRXKeyInfo): Boolean;
var
  LastWriteTime:  TFileTime;
begin
FillChar(Addr(KeyInfo)^,SizeOf(TRXKeyInfo),0);
Result := CheckErrorCode(RegQueryInfoKeyW(Key,nil,nil,nil,
             @KeyInfo.SubKeys,@KeyInfo.MaxSubKeyLen,@KeyInfo.MaxClassLen,
             @KeyInfo.Values,@KeyInfo.MaxValueNameLen,@KeyInfo.MaxValueLen,
             @KeyInfo.SecDescrBytes,@LastWriteTime));
// convert time             
If Result then
  KeyInfo.LastWriteTime := FileTimeToDateTime(LastWriteTime);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetSubKeys(Key: HKEY; SubKeys: TStrings): Boolean;
var
  i:        Integer;
  TempStr:  WideString;
  Len:      DWORD;
begin
Result := False;
SubKeys.Clear;
i := 0;
TempStr := '';
SetLength(TempStr,255 + 1{terminating zero}); // limit for key name length
while True do
  begin
    Len := Length(TempStr);
    case SetErrorCode(RegEnumKeyExW(Key,DWORD(i),PWideChar(TempStr),Len,nil,nil,nil,nil)) of
      ERROR_SUCCESS:
        SubKeys.Add(WideToStr(Copy(TempStr,1,WStrLen(TempStr))));
      ERROR_MORE_DATA:
        begin
          If Succ(Integer(Len)) > Length(TempStr) then
            SetLength(TempStr,Len + 1)
          else
            SetLength(TempStr,Length(TempStr) * 2);
          Dec(i); // call RegEnumKeyExW again with the same index
        end;
      ERROR_NO_MORE_ITEMS:
        begin
          Result := True;
          Break{while};
        end;
    else
     {some other error}
      Break{while};
    end;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetSubTree(Key: HKEY; SubKeys: TStrings; const ParentPath: String): Boolean;
var
  i:        Integer;
  Buffer:   WideString;
  Len:      DWORD;
  TempStr:  String;
  SubKey:   HKEY;
begin
// do not clear the list, this function is called recursively
Result := False;
i := 0;
Buffer := '';
SetLength(Buffer,255 + 1);
while True do
  begin
    Len := Length(Buffer);
    case SetErrorCode(RegEnumKeyExW(Key,DWORD(i),PWideChar(Buffer),Len,nil,nil,nil,nil)) of
      ERROR_SUCCESS:
        begin
          // recurse
          TempStr := WideToStr(Copy(Buffer,1,Len));
          SubKeys.Add(ConcatKeyNames(ParentPath,TempStr));
          If AuxOpenKey(Key,TempStr,KEY_ENUMERATE_SUB_KEYS,SubKey) then
            try
              If not GetSubTree(SubKey,SubKeys,SubKeys[Pred(SubKeys.Count)]) then
                Break{while};
            finally
              AuxCloseKey(SubKey);
            end
          else Break{while};
        end;
      ERROR_MORE_DATA:
        begin
          SetLength(Buffer,Length(Buffer) * 2);
          Dec(i);
        end;
      ERROR_NO_MORE_ITEMS:
        begin
          Result := True;
          Break{while};
        end;
    else
      Break{while};
    end;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueInfo(Key: HKEY; const ValueName: String; out ValueInfo: TRXValueInfo): Boolean;
var
  ValueType:  DWORD;
  DataSize:   DWORD;
begin
FillChar(Addr(ValueInfo)^,SizeOf(TRXValueInfo),0);
Result := CheckErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,@ValueType,nil,@DataSize),True);
If Result then
  begin
    ValueInfo.ValueType := TranslateValueType(ValueType);
    ValueInfo.DataSize := TMemSize(DataSize);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValues(Key: HKEY; Values: TStrings): Boolean;
var
  i:        Integer;
  TempStr:  WideString;
  Len:      DWORD;
begin
Result := False;
Values.Clear;
i := 0;
TempStr := '';
SetLength(TempStr,16383 + 1); // limit for a value name length
while True do
  begin
    Len := Length(TempStr);
    case SetErrorCode(RegEnumValueW(Key,DWORD(i),PWideChar(TempStr),@Len,nil,nil,nil,nil)) of
      ERROR_SUCCESS,
      ERROR_MORE_DATA:
        Values.Add(WideToStr(Copy(TempStr,1,WStrLen(TempStr))));
      ERROR_NO_MORE_ITEMS:
        begin
          Result := True;
          Break{while};
        end;
    else
     {other errors}
      Break{while};
    end;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteSubKeys(Key: HKEY): Boolean;
var
  SubKeys:  TStringList;
  i:        Integer;
begin
Result := True;
SubKeys := TStringList.Create;
try
  GetSubKeys(Key,SubKeys);
  For i := 0 to Pred(SubKeys.Count) do
    If not CheckErrorCode(SHDeleteKeyW(Key,PWideChar(StrToWide(SubKeys[i])))) then
      begin
        Result := False;
        Break{For i};
      end;
finally
  SubKeys.Free;
end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteValues(Key: HKEY): Boolean;
var
  Values: TStringList;
  i:      Integer;
begin
Result := True;
Values := TStringList.Create;
try
  GetValues(Key,Values);
  For i := 0 to Pred(Values.Count) do
    If not CheckErrorCode(RegDeleteValueW(Key,PWideChar(StrToWide(Values[i])))) then
      begin
        Result := False;
        Break{For i};
      end;
finally
  Values.Free;
end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function TRegistryEx.QueryProcessPrivilege(const PrivilegeName: String): TRXPrivilegeStatus;
var
  Token:        THandle;
  RetLength:    DWORD;
  Dummy:        DWORD;
  Buffer:       Pointer;
  PrivilegeID:  LUID;
  PrivilegePtr: PLUIDAndAttributes;
  i:            Integer;
begin
Result := psError;
If SysCallError(OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,Token)) then
  try
    SysCallError(GetTokenInformation(Token,TokenPrivileges,nil,0,RetLength));
    If (fLastSysError = ERROR_INSUFFICIENT_BUFFER) and (RetLength > 0) then
      begin
        GetMem(Buffer,RetLength);
        try
          If SysCallError(GetTokenInformation(Token,TokenPrivileges,Buffer,RetLength,Dummy)) then
            If SysCallError(LookupPrivilegeValue(nil,PChar(PrivilegeName),PrivilegeID)) then
              begin
                Result := psRemoved;
                PrivilegePtr := Addr(PTokenPrivileges(Buffer)^.Privileges[0]);
                For i := 1 to PTokenPrivileges(Buffer)^.PrivilegeCount do
                  If PrivilegePtr^.Luid = PrivilegeID then
                    begin
                      If (PrivilegePtr^.Attributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT) <> 0 then
                        Result := psEnabledDefault
                      else If (PrivilegePtr^.Attributes and SE_PRIVILEGE_ENABLED) <> 0 then
                        Result := psEnabled
                      else
                        Result := psDisabled;
                      Break{For i};
                    end
                  else Inc(PrivilegePtr);
              end;
        finally
          FreeMem(Buffer,RetLength);
        end;
      end;
  finally
    CloseHandle(Token);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function TRegistryEx.SetProcessPrivilege(const PrivilegeName: String; Enable: Boolean): Boolean;
var
  Token:            THandle;
  TokenPrivileges:  TTokenPrivileges;
begin
Result := False;
If SysCallError(OpenProcessToken(GetCurrentProcess,TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES,Token)) then
  try
    If SysCallError(LookupPrivilegeValue(nil,PChar(PrivilegeName),TokenPrivileges.Privileges[0].Luid)) then
      begin
        TokenPrivileges.PrivilegeCount := 1;
        If Enable then
          TokenPrivileges.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
        else
          TokenPrivileges.Privileges[0].Attributes := 0;
        If SysCallError(AdjustTokenPrivileges(Token,False,@TokenPrivileges,0,nil,nil)) then
          Result := fLastSysError = ERROR_SUCCESS;
      end;
  finally
    CloseHandle(Token);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function TRegistryEx.ExecuteRegCommand(const Command: String): Boolean;
var
  StartupInfo:  TStartupInfoW;
  ProcessInfo:  TProcessInformation;
  TempCmdLine:  WideString;
  ExitCode:     DWORD;
begin
Result := False;
FillChar(Addr(StartupInfo)^,SizeOf(StartupInfo),0);
StartupInfo.cb := SizeOf(StartupInfo);
StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
StartupInfo.wShowWindow := SW_HIDE;
{
  Since CreateProcessW can change content of the command line string, it is
  necessary to ensure it is in a writable memory (variable).
}
TempCmdLine := StrToWide('REG ' + Command);
If SysCallError(CreateProcessW(nil,PWideChar(TempCmdLine),nil,nil,False,CREATE_NO_WINDOW or
  BELOW_NORMAL_PRIORITY_CLASS,nil,nil,Addr(StartupInfo),@ProcessInfo)) then
  begin
    case WaitForSingleObject(ProcessInfo.hProcess,INFINITE) of
      WAIT_OBJECT_0:  If SysCallError(GetExitCodeProcess(ProcessInfo.hProcess,ExitCode)) then
                        begin
                          Result := ExitCode = 0;
                          If not Result then
                            SetErrorCode(ERROR_GENERIC_COMMAND_FAILED);
                        end;
      WAIT_ABANDONED,
      WAIT_TIMEOUT:   SetErrorCode(ERROR_INVALID_DATA);
    else
      SetErrorCode(GetLastError);
    end;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TRegistryEx.MoveKey(SrcBaseKey: HKEY; const SrcSubKey: String; DstBaseKey: HKEY; const DstSubKey: String; CopySecurity,DeleteSource: Boolean): Boolean;

  Function SrcSecAccRights: DWORD;
  begin
    If CopySecurity then
      Result := READ_CONTROL
    else
      Result := 0;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function DstSecAccRights: DWORD;
    begin
    If CopySecurity then
      Result := WRITE_DAC or WRITE_OWNER
    else
      Result := 0;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function HasSubKeysInternal(Key: HKEY; out SubKeysRes: Boolean): Boolean;
  var
    SubKeyCount:  DWORD;
  begin
    Result := CheckErrorCode(RegQueryInfoKeyW(Key,nil,nil,nil,@SubKeyCount,nil,nil,nil,nil,nil,nil,nil));
    If Result then
      SubKeysRes := SubKeyCount > 0;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
var
  SrcSubKeys: TStringList;  // used in MoveKeyInternal

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  // called only once, it is here only to simplify implementation
  Function MoveKeyInternal(SrcKey,DstKey: HKEY): Boolean;
  const
    PREALLOC_BUFF_SIZE = 8 * 1024;  // 8KiB
  var
    PreallocatedBuffer: Pointer;

  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    Function MoveValues(FromKey,ToKey: HKEY): Boolean;
    var
      i:        Integer;
      TempStr:  WideString;
      Len:      DWORD;
      ValName:  WideString;
      ValType:  DWORD;
      ValSize:  DWORD;
      ValData:  Pointer;
      BuffSize: TMemSize;
    begin
      Result := False;
      i := 0;
      TempStr := '';
      SetLength(TempStr,16383 + 1);
      while True do
        begin
          Len := Length(TempStr);
          ValSize := PREALLOC_BUFF_SIZE;
          case SetErrorCode(RegEnumValueW(FromKey,DWORD(i),PWideChar(TempStr),@Len,
                            nil,@ValType,PreallocatedBuffer,@ValSize)) of
            ERROR_MORE_DATA:
              begin
                // data cannot fir into preallocated buffer
                ValName := Copy(TempStr,1,WStrLen(TempStr));
              {
                Aaand since ValSize might not actually contain required size
                (HKEY_PERFORMANCE_DATA)...
              }
                If ValSize <= PREALLOC_BUFF_SIZE then
                  begin
                    // ...this bullshit begins
                    BuffSize := PREALLOC_BUFF_SIZE * 2;
                    GetMem(ValData,Buffsize);
                    try
                      while True do
                        begin
                          ValSize := DWORD(BuffSize);                        
                          case SetErrorCode(RegQueryValueExW(FromKey,PWideChar(ValName),
                                            nil,nil,ValData,@ValSize)) of
                            ERROR_MORE_DATA:
                              begin
                                // no need to preserve data, do not use ReallocMem
                                FreeMem(ValData,BuffSize);
                                BuffSize := BuffSize * 2;
                                GetMem(ValData,BuffSize);
                              end;
                            ERROR_SUCCESS:
                              begin
                                If not CheckErrorCode(RegSetValueExW(ToKey,
                                  PWideChar(ValName),0,ValType,ValData,ValSize)) then
                                  Exit;
                                If DeleteSource then
                                  If not CheckErrorCode(RegDeleteValueW(FromKey,
                                    PWideChar(ValName))) then
                                    Exit;
                                Break{inner while};
                              end;
                          else
                            Exit; // Break will not suffice here (nested loop)
                          end;
                        end;
                    finally
                      FreeMem(ValData,BuffSize);
                    end;
                  end
                else
                  begin
                    // ValSize seems to be set properly
                    GetMem(ValData,ValSize);
                    try
                      If CheckErrorCode(RegQueryValueExW(FromKey,PWideChar(ValName),
                        nil,nil,ValData,@ValSize)) then
                        begin
                          If not CheckErrorCode(RegSetValueExW(ToKey,PWideChar(ValName),
                            0,ValType,ValData,ValSize)) then
                            Break{while};
                        end
                      else Break{while};
                    finally
                      FreeMem(ValData,ValSize);
                    end;                  
                  end;
              end;
            ERROR_SUCCESS:
              begin
                // data fit into preallocated buffer
                ValName := Copy(TempStr,1,WStrLen(TempStr));
                If not CheckErrorCode(RegSetValueExW(ToKey,PWideChar(ValName),0,
                                      ValType,PreallocatedBuffer,ValSize)) then
                  Break{while};
                If DeleteSource then
                  If not CheckErrorCode(RegDeleteValueW(FromKey,PWideChar(ValName))) then
                    Break{while};
              end;
            ERROR_NO_MORE_ITEMS:
              begin
                // no more values present in the source key
                Result := True;
                Break{while};
              end;              
          else
            // other errors
            Break{while};
          end;
          If not DeleteSource then
            Inc(i);
        end;
    end;

  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

    Function CopySecurityDescriptor(FromKey,ToKey: HKEY): Boolean;
    var
      Buffer: Pointer;
      Size:   DWORD;
    begin
      Result := False;
      Size := PREALLOC_BUFF_SIZE;
      case SetErrorCode(RegGetKeySecurity(FromKey,fSecDesrCopyInfo,PreallocatedBuffer,Size)) of
        ERROR_INSUFFICIENT_BUFFER:
          begin
            GetMem(Buffer,Size);
            try
              If CheckErrorCode(RegGetKeySecurity(FromKey,fSecDesrCopyInfo,Buffer,Size)) then
                Result := CheckErrorCode(RegSetKeySecurity(ToKey,fSecDesrCopyInfo,Buffer));
            finally
              FreeMem(Buffer,Size);
            end;
          end;
        ERROR_SUCCESS:
          Result := CheckErrorCode(RegSetKeySecurity(ToKey,fSecDesrCopyInfo,PreallocatedBuffer));
        // other values are treated as fail-worthy errors    
      end;
    end;
    
  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  var
    i:              Integer;
    SrcWorkKey:     HKEY;
    DstWorkKey:     HKEY;
    SrcHasSubKeys:  Boolean;
  begin
    Result := False;
    GetMem(PreallocatedBuffer,PREALLOC_BUFF_SIZE);
    try
      // copy values and security desciptor of base key
      If not MoveValues(SrcKey,DstKey) then
        Exit;
      If CopySecurity then
        If not CopySecurityDescriptor(SrcKey,DstKey) then
          Exit;
      // re-create the source key tree and copy values and sec. descriptors
      For i := 0 to Pred(SrcSubKeys.Count) do
        SrcSubKeys.Objects[i] := nil;
      try
        For i := 0 to Pred(SrcSubKeys.Count) do
          If AuxOpenKey(SrcKey,SrcSubKeys[i],SrcSecAccRights or KEY_QUERY_VALUE or KEY_SET_VALUE,SrcWorkKey) then
            begin
              SrcSubKeys.Objects[i] := TObject(SrcWorkKey);  // nasty hack :-X
              If AuxCreateKey(DstKey,SrcSubKeys[i],DstSecAccRights or KEY_SET_VALUE,DstWorkKey) then
                try
                  // no need to check creation
                  If not MoveValues(SrcWorkKey,DstWorkKey) then
                    Exit;
                  If CopySecurity then
                    If not CopySecurityDescriptor(SrcWorkKey,DstWorkKey) then
                      Exit;
                finally
                  AuxCloseKey(DstWorkKey);
                end
              else Exit;
              // do not close source key now
            end
          else Exit;
        // delete empty source keys
        If DeleteSource then
          For i := Pred(SrcSubKeys.Count) downto 0 do
            If HasSubKeysInternal(HKEY(SrcSubKeys.Objects[i]),SrcHasSubKeys) then
              begin
                If not SrcHasSubKeys then
                  If not CheckErrorCode(RegDeleteKeyW(SrcKey,PWideChar(StrToWide(SrcSubKeys[i])))) then
                    Exit;
              end
            else Exit;
      finally
        For i := 0 to Pred(SrcSubKeys.Count) do
          If Assigned(SrcSubKeys.Objects[i]) then
            AuxCloseKey(HKEY(SrcSubKeys.Objects[i]))
      end;
    finally
      FreeMem(PreallocatedBuffer,PREALLOC_BUFF_SIZE);
    end;
    // if we are here, then all is good
    Result := True;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
var
  SrcKey,DstKey:  HKEY;
  Disposition:    DWORD;
  SrcHasSubKeys:  Boolean;
begin
{
  Why all this complexity?
  Because recursive approach will cause infinite loop when copying source key
  into its own subkey (eg. key_1 into key_1\key_copy).
}
DstKey := 0;
Result := False;
// open source key
If AuxOpenKey(SrcBaseKey,SrcSubKey,SrcSecAccRights or KEY_ENUMERATE_SUB_KEYS or
  KEY_QUERY_VALUE or KEY_SET_VALUE,SrcKey) then
  try
    SrcSubKeys := TStringList.Create;
    try
      // list complete subkey tree of the source
      If GetSubTree(SrcKey,SrcSubKeys,'') then
        // create destination key
        If AuxCreateKey(DstBaseKey,DstSubKey,DstSecAccRights or KEY_CREATE_SUB_KEY or
          KEY_SET_VALUE,DstKey,REG_OPTION_NON_VOLATILE,@Disposition) then
          try
            // check whether the destination key was created and not only opened
            case Disposition of
              REG_CREATED_NEW_KEY:
                // destination key was created
                Result := MoveKeyInternal(SrcKey,DstKey);
              REG_OPENED_EXISTING_KEY:
                // destination key existed and was only opened
                SetErrorCode(ERROR_ALREADY_EXISTS);
            else
              // some erroneous result
              SetErrorCode(ERROR_INVALID_DATA);
            end;
          finally
            AuxCloseKey(DstKey);
          end;
    finally
      SrcSubKeys.Free;
    end;
    // see if the source still has subkeys (possible only when moving source into itself)
    If Result then
      Result := HasSubKeysInternal(SrcKey,SrcHasSubKeys);
  finally
    AuxCloseKey(SrcKey);
  end;
// delete source key  
If Result and DeleteSource and not SrcHasSubKeys then
  Result := CheckErrorCode(RegDeleteKeyW(SrcBaseKey,PWideChar(StrToWide(SrcSubKey))));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveValue(SrcKey: HKEY; const SrcValueName: String; DstKey: HKEY; const DstValueName: String; DeleteSource: Boolean): Boolean;
var
  ValueInfo:  TRXValueInfo;
  Buffer:     Pointer;
  Size:       TMemSize;
begin
Result := False;
If not GetValueInfo(DstKey,DstValueName,ValueInfo) then
  begin
    If GetValueInfo(SrcKey,SrcValueName,ValueInfo) then
      begin
        If ValueInfo.DataSize > 0 then
          begin
            If GetValueDataOut(SrcKey,SrcValueName,Buffer,Size,ValueInfo.ValueType) then
              try
                Result := TrySetValueData(DstKey,DstValueName,Buffer^,Size,ValueInfo.ValueType);
              finally
                FreeMem(Buffer,Size);
              end;
          end
        else Result := TrySetValueData(DstKey,DstValueName,nil^,0,ValueInfo.ValueType);
        If Result and DeleteSource then
          Result := CheckErrorCode(RegDeleteValueW(SrcKey,PWideChar(StrToWide(SrcValueName))));
      end;
  end
else SetErrorCode(ERROR_ALREADY_EXISTS);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.SetValueData(const TypeName: String; Key: HKEY; const ValueName: String; const Data; Size: TMemSize; ValueType: TRXValueType);
begin
If not CheckErrorCode(RegSetValueExW(Key,PWideChar(StrToWide(ValueName)),0,TranslateValueType(ValueType),@Data,DWORD(Size))) then
  raise ERXRegWriteError.CreateFmt('TRegistryEx.Write%s: Unable to write value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.SetValueData(const TypeName: String; Key: HKEY; const ValueName: String; Data: Integer);
var
  Temp: DWORD;
begin
Temp := DWORD(Data);
SetValueData(TypeName,Key,ValueName,Temp,SizeOf(DWORD),vtDWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TrySetValueData(Key: HKEY; const ValueName: String; const Data; Size: TMemSize; ValueType: TRXValueType): Boolean;
begin
Result := CheckErrorCode(RegSetValueExW(Key,PWideChar(StrToWide(ValueName)),0,TranslateValueType(ValueType),@Data,DWORD(Size)));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value; Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_SET_VALUE,TempKey) then
  try
    SetValueData(TypeName,TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Write%s: Unable to open key "%s" for writing (%d).',
  [TypeName,ConcatKeyNames(PredefinedKeytoStr(RootKey),TrimKeyName(KeyName)),fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteMacro(const TypeName,KeyName,ValueName: String; const Value; Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_SET_VALUE,TempKey) then
  try
    SetValueData(TypeName,TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Write%s: Unable to open key "%s" for writing (%d).',
  [TypeName,KeyName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Integer);
var
  Temp: DWORD;
begin
Temp := DWORD(Value);
WriteMacro(TypeName,RootKey,KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteMacro(const TypeName,KeyName,ValueName: String; Value: Integer);
var
  Temp: DWORD;
begin
Temp := DWORD(Value);
WriteMacro(TypeName,KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueDataOut(Key: HKEY; const ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  RegDataSize:  DWORD;
  RegValueType: DWORD;
begin
Result := False;
If CheckErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,@RegValueType,nil,@RegDataSize),True) then
  begin
    If TranslateValueType(RegValueType) = ValueType then
      begin
        If RegDataSize <= 0 then
          Size := 8 * 1024 {8KiB}
        else
          Size := TMemSize(RegDataSize);
        // it is necessary to keep the size, as RegQueryValueExW can fill the RegDataSize with bogus data
        GetMem(Mem,Size);
        while True do
          begin
            RegDataSize := DWORD(Size);
            case SetErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,nil,Mem,@RegDataSize)) of
              ERROR_SUCCESS:    begin
                                  If TMemSize(RegDataSize) <> Size then
                                    begin
                                      Size := TMemSize(RegDataSize);
                                      ReallocMem(Mem,Size);
                                    end;
                                  Result := True;
                                  Break{while};
                                end;
              ERROR_MORE_DATA:  begin
                                  // do not call realloc, there is no need to preserve any data
                                  FreeMem(Mem,Size);
                                  Size := Size * 2;
                                  GetMem(Mem,Size);
                                end;
            else
             {some error...}
              Break{while};
            end;
          end;
        If not Result then
          begin
            FreeMem(Mem,Size);
            Mem := nil;
            Size := 0;
          end;
      end
    else SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataOut(Key: HKEY; const ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean;
var
  RegDataSize:  DWORD;
  RegValueType: DWORD;
  Temp:         TStrSize;
begin
Result := False;
If CheckErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,@RegValueType,nil,@RegDataSize),True) then
  begin
    If TranslateValueType(RegValueType) = ValueType then
      begin
        Str := '';
        If RegDataSize <= 0 then
          SetLength(Str,8 * 1024)
        else
          SetLength(Str,RegDataSize div SizeOf(WideChar));
        while True do
          begin
            RegDataSize := DWORD(Length(Str) * SizeOf(WideChar));
            case SetErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),
                                               nil,nil,PByte(PWideChar(Str)),@RegDataSize)) of
              ERROR_SUCCESS:    begin
                                  If RegDataSize <> DWORD(Length(Str) * SizeOf(WideChar)) then
                                    SetLength(Str,RegDataSize div SizeOf(WideChar));
                                  Result := True;
                                  Break{while};
                                end;
              ERROR_MORE_DATA:  begin
                                  Temp := Length(Str);
                                  SetLength(Str,0); // prevent copying of data
                                  SetLength(Str,Temp * 2);
                                end;
            else
             {some error...}
              Break{while};
            end;
          end;
        If Result then
          begin
            // remove terminating zero
            If Length(Str) > 0 then
              If Str[Length(Str)] = WideChar(#0) then
                SetLength(Str,Length(Str) - 1);
          end
        else Str := '';
      end
    else SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueDataExtBuff(Key: HKEY; const ValueName: String; out Data; var Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  RegDataSize:  DWORD;
  RegValueType: DWORD;
begin
Result := False;
RegDataSize := DWORD(Size);
If CheckErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,@RegValueType,@Data,@RegDataSize)) then
  begin
    If TranslateValueType(ValueType) = RegValueType then
      begin
        Result := True;
        Size := TMemSize(RegDataSize);
      end
    else SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueData(Key: HKEY; const ValueName: String; out Data; Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  RegDataSize:  DWORD;
  RegValueType: DWORD;
begin
Result := False;
RegDataSize := DWORD(Size);
If CheckErrorCode(RegQueryValueExW(Key,PWideChar(StrToWide(ValueName)),nil,@RegValueType,@Data,@RegDataSize)) then
  begin
  {
    This function is intended only for invariant-size data, so to consider it
    successful, the amount of read data must equal to what was requested, and
    actual data type must match requested type.
  }
    If TranslateValueType(ValueType) = RegValueType then
      begin
        If TMemSize(RegDataSize) = Size then
          Result := True
        else
          SetErrorCode(ERROR_INCORRECT_SIZE);
      end
    else SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueData(Key: HKEY; const ValueName: String; out Data: Integer): Boolean;
var
  Temp: DWORD;
begin
Result := GetValueData(Key,ValueName,Temp,SizeOf(DWORD),vtDWord);
Data := Integer(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadMacroOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataOut(TempKey,ValueName,Mem,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacroOut(const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataOut(TempKey,ValueName,Mem,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacroOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataOut(TempKey,ValueName,Str,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacroOut(const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataOut(TempKey,ValueName,Str,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadMacroExtBuff(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataExtBuff(TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacroExtBuff(const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueDataExtBuff(TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadMacro(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueData(TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacro(const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueData(TempKey,ValueName,Value,Size,ValueType);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacro(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Success: Boolean): Integer;
var
  Temp: DWORD;
begin
Success := TryReadMacro(RootKey,KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
Result := Integer(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMacro(const KeyName,ValueName: String; out Success: Boolean): Integer;
var
  Temp: DWORD;
begin
Success := TryReadMacro(KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
Result := Integer(Temp);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ReadMacroOut(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataOut(TempKey,ValueName,Mem,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,ConcatKeyNames(PredefinedKeytoStr(RootKey),TrimKeyName(KeyName)),fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroOut(const TypeName,KeyName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataOut(TempKey,ValueName,Mem,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,KeyName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroOut(const TypeName,ValueName: String; out Mem: Pointer; out Size: TMemSize; ValueType: TRXValueType);
begin
If not GetValueDataOut(GetWorkingKey(True),ValueName,Mem,Size,ValueType) then
  raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroOut(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataOut(TempKey,ValueName,Str,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,ConcatKeyNames(PredefinedKeytoStr(RootKey),TrimKeyName(KeyName)),fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroOut(const TypeName,KeyName,ValueName: String; out Str: WideString; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataOut(TempKey,ValueName,Str,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,KeyName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroOut(const TypeName,ValueName: String; out Str: WideString; ValueType: TRXValueType);
begin
If not GetValueDataOut(GetWorkingKey(True),ValueName,Str,ValueType) then
  raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ReadMacroExtBuff(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataExtBuff(TempKey,ValueName,Value,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,ConcatKeyNames(PredefinedKeytoStr(RootKey),TrimKeyName(KeyName)),fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroExtBuff(const TypeName,KeyName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueDataExtBuff(TempKey,ValueName,Value,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,KeyName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacroExtBuff(const TypeName,ValueName: String; out Value; var Size: TMemSize; ValueType: TRXValueType);
begin
If not GetValueDataExtBuff(GetWorkingKey(True),ValueName,Value,Size,ValueType) then
  raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ReadMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueData(TempKey,ValueName,Value,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,ConcatKeyNames(PredefinedKeytoStr(RootKey),TrimKeyName(KeyName)),fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacro(const TypeName,KeyName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType);
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    If not GetValueData(TempKey,ValueName,Value,Size,ValueType) then
      raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
  finally
    AuxCloseKey(TempKey);
  end
else raise ERXRegInvalidKey.CreateFmt('TRegistryEx.Read%s: Unable to open key "%s" for reading (%d).',
  [TypeName,KeyName,fLastSysError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMacro(const TypeName,ValueName: String; out Value; Size: TMemSize; ValueType: TRXValueType);
begin
If not GetValueData(GetWorkingKey(True),ValueName,Value,Size,ValueType) then
  raise ERXRegReadError.CreateFmt('TRegistryEx.Read%s: Unable to read value "%s" (%d).',[TypeName,ValueName,fLastSysError]);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadMacro(const TypeName: String; RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Integer;
var
  Temp: DWORD;
begin
ReadMacro(TypeName,RootKey,KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
Result := Integer(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadMacro(const TypeName,KeyName,ValueName: String): Integer;
var
  Temp: DWORD;
begin
ReadMacro(TypeName,KeyName,ValueName,Temp,SizeOf(DWORD),vtDWord);
Result := Integer(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadMacro(const TypeName,ValueName: String): Integer;
var
  Temp: DWORD;
begin
ReadMacro(TypeName,ValueName,Temp,SizeOf(DWORD),vtDWord);
Result := Integer(Temp);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.Initialize(RootKey: TRXPredefinedKey; AccessRights: TRXKeyAccessRights);
begin
fAccessRightsSys := TranslateAccessRights(AccessRights);;
fAccessRights := AccessRights;
fRootKeyHandle := TranslatePredefinedKey(RootKey);
fRootKey := RootKey;
fRemoteRootKey := False;
fCloseRootKey := False;
fCurrentKeyHandle := 0;
fCurrentKeyName := '';
fCurrentKeyAccessRights := [];
fFlushOnClose := False;
fLastSysError := ERROR_SUCCESS;
fSecDesrCopyInfo := SECINFO_NORMAL;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.Finalize;
begin
CloseKey;
If fCloseRootKey then
  RegCloseKey(fRootKeyHandle);
end;

{-------------------------------------------------------------------------------
    TRegistryEx - public methods
-------------------------------------------------------------------------------}

constructor TRegistryEx.Create(RootKey: TRXPredefinedKey; AccessRights: TRXKeyAccessRights = karAllAccess);
begin
inherited Create;
Initialize(RootKey,AccessRights);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRegistryEx.Create(AccessRights: TRXKeyAccessRights = karAllAccess);
begin
Create(pkCurrentUser,AccessRights);
end;

//------------------------------------------------------------------------------

destructor TRegistryEx.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RegistryQuotaAllowed: UInt32;
var
  Allowed:  DWORD;
  Used:     DWORD;
begin
If SysCallError(GetSystemRegistryQuota(@Allowed,@Used)) then
  Result := UInt32(Allowed)
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RegistryQuotaUsed: UInt32;
var
  Allowed:  DWORD;
  Used:     DWORD;
begin
If SysCallError(GetSystemRegistryQuota(@Allowed,@Used)) then
  Result := UInt32(Used)
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DisablePredefinedCache(AllKeys: Boolean): Boolean;
begin
If AllKeys then
  begin
    If not Assigned(RegDisablePredefinedCacheEx) then
      begin
        SetErrorCode(ERROR_CALL_NOT_IMPLEMENTED);
        Result := False;
      end
    else Result := CheckErrorCode(RegDisablePredefinedCacheEx);
  end
else Result := CheckErrorCode(RegDisablePredefinedCache);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ConnectRegistry(const MachineName: String; RootKey: TRXPredefinedKey): Boolean;
var
  TempKey:  HKEY;
begin
TempKey := 0;
Result := CheckErrorCode(RegConnectRegistryW(PWideChar(StrToWide(MachineName)),TranslatePredefinedKey(RootKey),TempKey));
If Result then
  begin
    CloseKey;
    ChangeRootKey(TempKey,RootKey);
    fRemoteRootKey := True;
    fCloseRootKey := True;
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.OpenCurrentUser(AccessRights: TRXKeyAccessRights = karAllAccess): Boolean;
var
  TempKey:  HKEY;
begin
TempKey := 0;
Result := CheckErrorCode(RegOpenCurrentUser(TranslateAccessRights(AccessRights),@TempKey));
If Result then
  begin
    CloseKey;
    ChangeRootKey(TempKey,pkCurrentUser);
    fCloseRootKey := True;
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.OpenUserClassesRoot(AccessToken: THandle; AccessRights: TRXKeyAccessRights = karAllAccess): Boolean;
var
  TempKey:  HKEY;
begin
TempKey := 0;
Result := CheckErrorCode(RegOpenUserClassesRoot(AccessToken,0,TranslateAccessRights(AccessRights),@TempKey));
If Result then
  begin
    CloseKey;
    ChangeRootKey(TempKey,pkClassesRoot);
    fCloseRootKey := True;
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.OverridePredefinedKey(PredefinedKey: TRXPredefinedKey; RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_ALL_ACCESS,TempKey) then
  try
    Result := CheckErrorCode(RegOverridePredefKey(TranslatePredefinedKey(PredefinedKey),TempKey));
  finally
    AuxCloseKey(TempKey); // this does not affect last error
  end
else Result := False; // last error is set in AuxOpenKey
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OverridePredefinedKey(PredefinedKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_ALL_ACCESS,TempKey) then
  try
    Result := CheckErrorCode(RegOverridePredefKey(TranslatePredefinedKey(PredefinedKey),TempKey));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OverridePredefinedKey(PredefinedKey: TRXPredefinedKey): Boolean;
begin
If fCurrentKeyHandle = 0 then
  begin
    SetErrorCode(ERROR_INVALID_HANDLE);
    Result := False;
  end
else Result := CheckErrorCode(RegOverridePredefKey(TranslatePredefinedKey(PredefinedKey),fCurrentKeyHandle))
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RestorePredefinedKey(PredefinedKey: TRXPredefinedKey): Boolean;
begin
Result := CheckErrorCode(RegOverridePredefKey(TranslatePredefinedKey(PredefinedKey),0));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.WaitForKeyChange(RootKey: TRXPredefinedKey; const KeyName: String; WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_NOTIFY,TempKey) then
  try
    Result := WaitForKeyChange(TempKey,TranslateNotifyFilter(Filter),WatchSubTree,Timeout);
  finally
    AuxCloseKey(TempKey);
  end
else Result := wrError;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.WaitForKeyChange(const KeyName: String; WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_NOTIFY,TempKey) then
  try
    Result := WaitForKeyChange(TempKey,TranslateNotifyFilter(Filter),WatchSubTree,Timeout);
  finally
    AuxCloseKey(TempKey);
  end
else Result := wrError;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.WaitForKeyChange(WatchSubTree: Boolean; Filter: TRXNotifyFilter = noWaitAll; Timeout: DWORD = INFINITE): TRXWaitResult;
begin
Result := WaitForKeyChange(GetWorkingKey(True),TranslateNotifyFilter(Filter),WatchSubTree,Timeout);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.OpenKey(RootKey: TRXPredefinedKey; const KeyName: String; CanCreate: Boolean; out Created: Boolean; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean;
var
  TempKey:      HKEY;
  Disposition:  DWORD;
begin
If CanCreate and (Length(TrimKeyName(KeyName)) > 0) then
  begin
    Result := AuxCreateKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),
      fAccessRightsSys,TempKey,TranslateCreateOptions(CreateOptions),@Disposition);
    If Result then
      case Disposition of
        REG_CREATED_NEW_KEY:      Created := True;
        REG_OPENED_EXISTING_KEY:  Created := False;
      else
        AuxCloseKey(TempKey);
        raise ERXRegistryError.CreateFmt('TRegistryEx.OpenKey: Invalid disposition (%d).',[Disposition]);
      end;
  end
else
  begin
    Result := AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),
                         fAccessRightsSys,TempKey);
    Created := False;
  end;
If Result then
  begin
    CloseKey;
    ChangeRootKey(TranslatePredefinedKey(RootKey),RootKey);
    ChangeCurrentKey(TempKey,TrimKeyName(KeyName));
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OpenKey(const KeyName: String; CanCreate: Boolean; out Created: Boolean; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean;
var
  TempKey:        HKEY;
  WorkingKeyName: String;
  Disposition:    DWORD;
begin
If CanCreate and (Length(TrimKeyName(KeyName)) > 0) then
  begin
    Result := AuxCreateKey(GetWorkingKey(IsRelativeKeyName(KeyName),WorkingKeyName),TrimKeyName(KeyName),
      fAccessRightsSys,TempKey,TranslateCreateOptions(CreateOptions),@Disposition);
    If Result then
      case Disposition of
        REG_CREATED_NEW_KEY:      Created := True;
        REG_OPENED_EXISTING_KEY:  Created := False;
      else
        AuxCloseKey(TempKey);
        raise ERXRegistryError.CreateFmt('TRegistryEx.OpenKey: Invalid disposition (%d).',[Disposition]);
      end;
  end
else
  begin
    Result := AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName),WorkingKeyName),
                         TrimKeyName(KeyName),fAccessRightsSys,TempKey);
    Created := False;
  end;
If Result then
  begin
    CloseKey;
    ChangeCurrentKey(TempKey,ConcatKeyNames(WorkingKeyName,TrimKeyName(KeyName)));
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OpenKey(RootKey: TRXPredefinedKey; const KeyName: String; CanCreate: Boolean = False): Boolean;
var
  Created:  Boolean;
begin
Result := OpenKey(RootKey,KeyName,CanCreate,Created);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OpenKey(const KeyName: String; CanCreate: Boolean = False): Boolean;
var
  Created:  Boolean;
begin
Result := OpenKey(KeyName,CanCreate,Created);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.OpenKeyReadOnly(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  AccessRightsTemp: DWORD;
  TempKey:          HKEY;
begin
// preserve karWoW64_32Key and karWoW64_64Key from current access rights
AccessRightsTemp := TranslateAccessRights(karRead + (fAccessRights * karWoW64_Res));
Result := AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),AccessRightsTemp,TempKey);
If Result then
  begin
    CloseKey;
    SetAccessRightsSys(AccessRightsTemp);
    ChangeRootKey(TranslatePredefinedKey(RootKey),RootKey);
    ChangeCurrentKey(TempKey,TrimKeyName(KeyName));
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.OpenKeyReadOnly(const KeyName: String): Boolean;
var
  AccessRightsTemp: DWORD;
  TempKey:          HKEY;
  WorkingKeyName:   String;
begin
AccessRightsTemp := TranslateAccessRights(karRead + (fAccessRights * karWoW64_Res));
Result := AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName),WorkingKeyName),
                     TrimKeyName(KeyName),AccessRightsTemp,TempKey);
If Result then
  begin
    CloseKey;
    SetAccessRightsSys(AccessRightsTemp);
    ChangeCurrentKey(TempKey,ConcatKeyNames(WorkingKeyName,TrimKeyName(KeyName)));
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.KeyExists(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := TempKey <> 0;
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.KeyExists(const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := TempKey <> 0;
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CreateKey(RootKey: TRXPredefinedKey; const KeyName: String; AccessRights: TRXKeyAccessRights = karAllAccess; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean;
var
  TempKey:      HKEY;
  Disposition:  DWORD;
begin
Result := False;
If AuxCreateKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),
  TranslateAccessRights(AccessRights),TempKey,TranslateCreateOptions(CreateOptions),@Disposition) then
  begin
    AuxCloseKey(TempKey);
    case Disposition of
      REG_CREATED_NEW_KEY:      Result := True;
      REG_OPENED_EXISTING_KEY:  SetErrorCode(ERROR_ALREADY_EXISTS);
    else
      raise ERXRegistryError.CreateFmt('TRegistryEx.CreateKey: Invalid disposition (%d).',[Disposition]);
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CreateKey(const KeyName: String; AccessRights: TRXKeyAccessRights = karAllAccess; CreateOptions: TRXKeyCreateOptions = [kcoNonVolatile]): Boolean;
var
  TempKey:      HKEY;
  Disposition:  DWORD;
begin
Result := False;
If AuxCreateKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),
  TranslateAccessRights(AccessRights),TempKey,TranslateCreateOptions(CreateOptions),@Disposition) then
  begin
    AuxCloseKey(TempKey);
    case Disposition of
      REG_CREATED_NEW_KEY:      Result := True;
      REG_OPENED_EXISTING_KEY:  SetErrorCode(ERROR_ALREADY_EXISTS);
    else
      raise ERXRegistryError.CreateFmt('TRegistryEx.CreateKey: Invalid disposition (%d).',[Disposition]);
    end;
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteKey(RootKey: TRXPredefinedKey; const KeyName: String; CanDeleteCurrentKey: Boolean; out CurrentKeyClosed: Boolean): Boolean;
var
  CurrentIsSubKey:  Boolean;
begin
{
  Check whether current key is a subkey (or matches) the key we are about to
  delete - current key must be open, root key must match (must be compared on
  hadle to account for remote roots) and a name must mark a subkey.
}
CurrentIsSubKey := CurrentKeyIsSubKeyOf(RootKey,TrimKeyName(KeyName),False);
If not CurrentIsSubKey or CanDeleteCurrentKey then
  Result := CheckErrorCode(SHDeleteKeyW(TranslatePredefinedKey(RootKey),PWideChar(StrToWide(TrimKeyName(KeyName)))))
else
  Result := False;
If Result and CurrentIsSubKey and CanDeleteCurrentKey then
  begin
    CloseKey;
    CurrentKeyClosed := True;
  end
else CurrentKeyClosed := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteKey(const KeyName: String; CanDeleteCurrentKey: Boolean; out CurrentKeyClosed: Boolean): Boolean;
var
  WorkingKey:       HKEY;
  WorkingKeyName:   String;
  CurrentIsSubKey:  Boolean;
begin
WorkingKey := GetWorkingKey(IsRelativeKeyName(KeyName),WorkingKeyName);
CurrentIsSubKey := CurrentKeyIsSubKeyOf(ConcatKeyNames(WorkingKeyName,TrimKeyName(KeyName)),False);
If not CurrentIsSubKey or CanDeleteCurrentKey then
  Result := SHDeleteKeyW(WorkingKey,PWideChar(StrToWide(TrimKeyName(KeyName)))) = ERROR_SUCCESS
else
  Result := False;
If Result and CurrentIsSubKey and CanDeleteCurrentKey then
  begin
    CloseKey;
    CurrentKeyClosed := True;
  end
else CurrentKeyClosed := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteKey(RootKey: TRXPredefinedKey; const KeyName: String; CanDeleteCurrentKey: Boolean = True): Boolean;
var
  Dummy: Boolean;
begin
Dummy := False;
Result := DeleteKey(RootKey,KeyName,CanDeleteCurrentKey,Dummy);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteKey(const KeyName: String; CanDeleteCurrentKey: Boolean = True): Boolean;
var
  Dummy: Boolean;
begin
Dummy := False;
Result := DeleteKey(KeyName,CanDeleteCurrentKey,Dummy);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.FlushKey;
begin
If fCurrentKeyHandle <> 0 then
  RegFlushKey(fCurrentKeyHandle);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.CloseKey;
begin
If fCurrentKeyHandle <> 0 then
  begin
    If fFlushOnClose then
      RegFlushKey(fCurrentKeyHandle);    
    RegCloseKey(fCurrentKeyHandle);
  end;
fCurrentKeyHandle := 0;
fCurrentKeyName := '';
fCurrentKeyAccessRights := [];
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetKeyInfo(RootKey: TRXPredefinedKey; const KeyName: String; out KeyInfo: TRXKeyInfo): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetKeyInfo(TempKey,KeyInfo);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetKeyInfo(const KeyName: String; out KeyInfo: TRXKeyInfo): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetKeyInfo(TempKey,KeyInfo);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetKeyInfo(out KeyInfo: TRXKeyInfo): Boolean;
begin
Result := GetKeyInfo(GetWorkingKey(True),KeyInfo);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.HasSubKeys(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(RootKey,KeyName,KeyInfo) then
  begin
    Result := KeyInfo.SubKeys > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasSubKeys(const KeyName: String): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(KeyName,KeyInfo) then
  begin
    Result := KeyInfo.SubKeys > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasSubKeys: Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(KeyInfo) then
  begin
    Result := KeyInfo.SubKeys > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; out SubKeysRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(RootKey,KeyName,KeyInfo);
If Result then
  SubKeysRes := KeyInfo.SubKeys > 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasSubKeys(const KeyName: String; out SubKeysRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(KeyName,KeyInfo);
If Result then
  SubKeysRes := KeyInfo.SubKeys > 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasSubKeys(out SubKeysRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(KeyInfo);
If Result then
  SubKeysRes := KeyInfo.SubKeys > 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; SubKeys: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_ENUMERATE_SUB_KEYS,TempKey) then
  try
    Result := GetSubKeys(TempKey,SubKeys);
  finally
    AuxCloseKey(TempKey);
  end
else SubKeys.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetSubKeys(const KeyName: String; SubKeys: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_ENUMERATE_SUB_KEYS,TempKey) then
  try
    Result := GetSubKeys(TempKey,SubKeys);
  finally
    AuxCloseKey(TempKey);
  end
else SubKeys.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetSubKeys(SubKeys: TStrings): Boolean;
begin
Result := GetSubKeys(GetWorkingKey(True),SubKeys);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetSubTree(RootKey: TRXPredefinedKey; const KeyName: String; SubKeys: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
SubKeys.Clear;
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_ENUMERATE_SUB_KEYS,TempKey) then
  try
    Result := GetSubTree(TempKey,SubKeys,'');
  finally
    AuxCloseKey(TempKey);
  end
else SubKeys.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetSubTree(const KeyName: String; SubKeys: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
SubKeys.Clear;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_ENUMERATE_SUB_KEYS,TempKey) then
  try
    Result := GetSubTree(TempKey,SubKeys,'');
  finally
    AuxCloseKey(TempKey);
  end
else SubKeys.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetSubTree(SubKeys: TStrings): Boolean;
begin
SubKeys.Clear;
Result := GetSubTree(GetWorkingKey(True),SubKeys,'');
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueInfo(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out ValueInfo: TRXValueInfo): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueInfo(TempKey,ValueName,ValueInfo);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueInfo(const KeyName,ValueName: String; out ValueInfo: TRXValueInfo): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValueInfo(TempKey,ValueName,ValueInfo);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueInfo(const ValueName: String; out ValueInfo: TRXValueInfo): Boolean;
begin
Result := GetValueInfo(GetWorkingKey(True),ValueName,ValueInfo);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.HasValues(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(RootKey,KeyName,KeyInfo) then
  begin
    Result := KeyInfo.Values > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasValues(const KeyName: String): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(KeyName,KeyInfo) then
  begin
    Result := KeyInfo.Values > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasValues: Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
If GetKeyInfo(KeyInfo) then
  begin
    Result := KeyInfo.Values > 0;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasValues(RootKey: TRXPredefinedKey; const KeyName: String; out ValuesRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(RootKey,KeyName,KeyInfo);
If Result then
  ValuesRes := KeyInfo.Values > 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasValues(const KeyName: String; out ValuesRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(KeyName,KeyInfo);
If Result then
  ValuesRes := KeyInfo.Values > 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.HasValues(out ValuesRes: Boolean): Boolean;
var
  KeyInfo:  TRXKeyInfo;
begin
Result := GetKeyInfo(KeyInfo);
If Result then
  ValuesRes := KeyInfo.Values > 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValues(RootKey: TRXPredefinedKey; const KeyName: String; Values: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValues(TempKey,Values);
  finally
    AuxCloseKey(TempKey);
  end
else Values.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValues(const KeyName: String; Values: TStrings): Boolean;
var
  TempKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE,TempKey) then
  try
    Result := GetValues(TempKey,Values);
  finally
    AuxCloseKey(TempKey);
  end
else Values.Clear;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValues(Values: TStrings): Boolean;
begin
Result := GetValues(GetWorkingKey(True),Values);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueType(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TRXValueType;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(RootKey,KeyName,ValueName,ValueInfo) then
  Result := ValueInfo.ValueType
else
  Result := vtUnknown;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueType(const KeyName,ValueName: String): TRXValueType;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(KeyName,ValueName,ValueInfo) then
  Result := ValueInfo.ValueType
else
  Result := vtUnknown;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueType(const ValueName: String): TRXValueType;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(ValueName,ValueInfo) then
  Result := ValueInfo.ValueType
else
  Result := vtUnknown;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.GetValueDataSize(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TMemSize;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(RootKey,KeyName,ValueName,ValueInfo) then
  begin
    Result := ValueInfo.DataSize;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataSize(const KeyName,ValueName: String): TMemSize;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(KeyName,ValueName,ValueInfo) then
  begin
    Result := ValueInfo.DataSize;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataSize(const ValueName: String): TMemSize;
var
  ValueInfo:  TRXValueInfo;
begin
If GetValueInfo(ValueName,ValueInfo) then
  begin
    Result := ValueInfo.DataSize;
    SetErrorCode(ERROR_SUCCESS);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataSize(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out DataSize: TMemSize): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(RootKey,KeyName,ValueName,ValueInfo);
If Result then
  DataSize := ValueInfo.DataSize;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataSize(const KeyName,ValueName: String; out DataSize: TMemSize): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(KeyName,ValueName,ValueInfo);
If Result then
  DataSize := ValueInfo.DataSize;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.GetValueDataSize(const ValueName: String; out DataSize: TMemSize): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(ValueName,ValueInfo);
If Result then
  DataSize := ValueInfo.DataSize;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ValueExists(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(RootKey,KeyName,ValueName,ValueInfo);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ValueExists(const KeyName,ValueName: String): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(KeyName,ValueName,ValueInfo);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ValueExists(const ValueName: String): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := GetValueInfo(ValueName,ValueInfo);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ValueOfTypeExists(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; ValueType: TRXValueType): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := False;
If GetValueInfo(RootKey,KeyName,ValueName,ValueInfo) then
  begin
    If ValueInfo.ValueType = ValueType then
      Result := True
    else
      SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ValueOfTypeExists(const KeyName,ValueName: String; ValueType: TRXValueType): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := False;
If GetValueInfo(KeyName,ValueName,ValueInfo) then
  begin
    If ValueInfo.ValueType = ValueType then
      Result := True
    else
      SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ValueOfTypeExists(const ValueName: String; ValueType: TRXValueType): Boolean;
var
  ValueInfo:  TRXValueInfo;
begin
Result := False;
If GetValueInfo(ValueName,ValueInfo) then
  begin
    If ValueInfo.ValueType = ValueType then
      Result := True
    else
      SetErrorCode(ERROR_DATATYPE_MISMATCH);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteValue(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_SET_VALUE,TempKey) then
  try
    Result := CheckErrorCode(RegDeleteValueW(TempKey,PWideChar(StrToWide(ValueName))));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteValue(const KeyName,ValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_SET_VALUE,TempKey) then
  try
    Result := CheckErrorCode(RegDeleteValueW(TempKey,PWideChar(StrToWide(ValueName))));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteValue(const ValueName: String): Boolean;
begin
Result := CheckErrorCode(RegDeleteValueW(GetWorkingKey(True),PWideChar(StrToWide(ValueName))));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteSubKeys(RootKey: TRXPredefinedKey; const KeyName: String; out CurrentKeyClosed: Boolean): Boolean;
var
  TempKey:  HKEY;
begin
CurrentKeyClosed := False;
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),
  KEY_ENUMERATE_SUB_KEYS or KEY_SET_VALUE,TempKey) then
  try
    Result := DeleteSubKeys(TempKey);
    // close current key if it was deleted
    If CurrentKeyIsSubKeyOf(RootKey,TrimKeyName(KeyName),True) then
      begin
        CloseKey;
        CurrentKeyClosed := True;
      end;
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteSubKeys(const KeyName: String; out CurrentKeyClosed: Boolean): Boolean;
var
  WorkingKey:     HKEY;
  WorkingKeyName: String;
  TempKey:        HKEY;
begin
CurrentKeyClosed := False;
WorkingKey := GetWorkingKey(IsRelativeKeyName(KeyName),WorkingKeyName);
If AuxOpenKey(WorkingKey,TrimKeyName(KeyName),KEY_ENUMERATE_SUB_KEYS or KEY_SET_VALUE,TempKey) then
  try
    Result := DeleteSubKeys(TempKey);
    If CurrentKeyIsSubKeyOf(ConcatKeyNames(WorkingKeyName,TrimKeyName(KeyName)),True) then
      begin
        CloseKey;
        CurrentKeyClosed := True;        
      end;
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteSubKeys: Boolean;
begin
// this can never delete current key
Result := DeleteSubKeys(GetWorkingKey(True));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteSubKeys(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  Dummy:  Boolean;
begin
Result := DeleteSubKeys(RootKey,KeyName,Dummy);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteSubKeys(const KeyName: String): Boolean;
var
  Dummy:  Boolean;
begin
Result := DeleteSubKeys(KeyName,Dummy);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteValues(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),
  KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := DeleteValues(TempKey);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteValues(const KeyName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),
  KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := DeleteValues(TempKey);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteValues: Boolean;
begin
Result := DeleteValues(GetWorkingKey(True));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DeleteContent(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
begin
If DeleteValues(RootKey,KeyName) then
  Result := DeleteSubKeys(RootKey,KeyName)
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteContent(const KeyName: String): Boolean;
begin
If DeleteValues(KeyName) then
  Result := DeleteSubKeys(KeyName)
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.DeleteContent: Boolean;
begin
If DeleteValues then
  Result := DeleteSubKeys
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CopyKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
begin
Result := MoveKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),
  TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),CopySecurityDescriptors,False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
begin
Result := MoveKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),
  GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),CopySecurityDescriptors,False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyKey(const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
begin
Result := MoveKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),
  TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),CopySecurityDescriptors,False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyKey(const SrcKeyName,DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
begin
Result := MoveKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),
  GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),CopySecurityDescriptors,False);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  NewCurrentKeyName:  String;
begin
Result := MoveKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),
  TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),CopySecurityDescriptors,True);
// close current key if it was moved and reopen it from new location
If Result and CurrentKeyIsSubKeyOf(SrcRootKey,TrimKeyName(SrcKeyName),False) then
  begin
    If ReplaceSuperKey(fCurrentKeyName,TrimKeyName(SrcKeyName),TrimKeyName(DstKeyName),NewCurrentKeyName) then
      begin
        // ensure the key will be reopened with the same access rights
        SetAccessRights(fCurrentKeyAccessRights);
        Result := OpenKey(DstRootKey,NewCurrentKeyName);
      end
    else
      begin
        SetErrorCode(ERROR_BAD_PATHNAME);
        Result := False;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveKey(SrcRootKey: TRXPredefinedKey; const SrcKeyName: String; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  DstWorkingKeyName:  String;
  NewCurrentKeyName:  String;
begin
Result := MoveKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),
  GetWorkingKey(IsRelativeKeyName(DstKeyName),DstWorkingKeyName),TrimKeyName(DstKeyName),
  CopySecurityDescriptors,True);
If Result and CurrentKeyIsSubKeyOf(SrcRootKey,TrimKeyName(SrcKeyName),False) then
  begin
    If ReplaceSuperKey(fCurrentKeyName,TrimKeyName(SrcKeyName),
      ConcatKeyNames(DstWorkingKeyName,TrimKeyName(DstKeyName)),NewCurrentKeyName) then
      begin
        SetAccessRights(fCurrentKeyAccessRights);
        Result := OpenKey(fRootKey,NewCurrentKeyName);
      end
    else
      begin
        SetErrorCode(ERROR_BAD_PATHNAME);
        Result := False;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveKey(const SrcKeyName: String; DstRootKey: TRXPredefinedKey; const DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  SrcWorkingKeyName:  String;
  NewCurrentKeyName:  String;
begin
Result := MoveKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName),SrcWorkingKeyName),TrimKeyName(SrcKeyName),
  TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),CopySecurityDescriptors,True);
If Result and CurrentKeyIsSubKeyOf(TrimKeyName(SrcKeyName),False) then
  begin
    If ReplaceSuperKey(fCurrentKeyName,ConcatKeyNames(SrcWorkingKeyName,TrimKeyName(SrcKeyName)),
      TrimKeyName(DstKeyName),NewCurrentKeyName) then
      begin
        SetAccessRights(fCurrentKeyAccessRights);
        Result := OpenKey(DstRootKey,NewCurrentKeyName);
      end
    else
      begin
        SetErrorCode(ERROR_BAD_PATHNAME);
        Result := False;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveKey(const SrcKeyName,DstKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  SrcWorkingKeyName:  String;
  DstWorkingKeyName:  String;
  NewCurrentKeyName:  String;
begin
Result := MoveKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName),SrcWorkingKeyName),TrimKeyName(SrcKeyName),
  GetWorkingKey(IsRelativeKeyName(DstKeyName),DstWorkingKeyName),TrimKeyName(DstKeyName),
  CopySecurityDescriptors,True);
If Result and CurrentKeyIsSubKeyOf(TrimKeyName(SrcKeyName),False) then
  begin
    If ReplaceSuperKey(fCurrentKeyName,ConcatKeyNames(SrcWorkingKeyName,TrimKeyName(SrcKeyName)),
      ConcatKeyNames(DstWorkingKeyName,TrimKeyName(DstKeyName)),NewCurrentKeyName) then
      begin
        SetAccessRights(fCurrentKeyAccessRights);
        Result := OpenKey(fRootKey,NewCurrentKeyName);
      end
    else
      begin
        SetErrorCode(ERROR_BAD_PATHNAME);
        Result := False;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RenameKey(RootKey: TRXPredefinedKey; const OldKeyName,NewKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  OldKeyWorkName: String;
  NewKeyWorkName: String;
begin
Result := False;
OldKeyWorkName := TrimKeyName(OldKeyName);
NewKeyWorkName := TrimKeyName(NewKeyName);
If (Length(OldKeyWorkName) > 0) and (Length(NewKeyWorkName) > 0) then
  begin
    If IsMultiLevelKeyPath(OldKeyWorkName) then
      begin
        If IsMultiLevelKeyPath(NewKeyWorkName) then
          begin
          {
            Both old and new names are multilevel. Compare if the paths,
            excluding lowest level, match. If so, do the move, if not, signal
            error.

            Note that if they are the same, the move will fail with error being
            ERROR_ALREADY_EXISTS.
          }
            If AnsiSameText(RemoveLowestKeyPathLevel(OldKeyWorkName),
              RemoveLowestKeyPathLevel(NewKeyWorkName)) then
              Result := MoveKey(RootKey,OldKeyWorkName,RootKey,NewKeyWorkName,CopySecurityDescriptors)
            else
              SetErrorCode(ERROR_BAD_PATHNAME);
          end
        else
          begin
          {
            Old is multilevel, new is simple - replace lowest level in old name
            with the new name.
          }
            Result := MoveKey(RootKey,OldKeyWorkName,RootKey,ConcatKeyNames(
              RemoveLowestKeyPathLevel(OldKeyWorkName),NewKeyWorkName),CopySecurityDescriptors);
          end;
      end
    else
      begin
        If not IsMultiLevelKeyPath(NewKeyWorkName) then
          // both old and new are simple names
          Result := MoveKey(RootKey,OldKeyWorkName,RootKey,NewKeyWorkName,CopySecurityDescriptors)
        else
          // invalid combination
          SetErrorCode(ERROR_BAD_PATHNAME);
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.RenameKey(const OldKeyName,NewKeyName: String; CopySecurityDescriptors: Boolean = False): Boolean;
var
  OldKeyWorkName: String;
  NewKeyWorkName: String;
begin
// get full paths and then just call the first overload
If IsRelativeKeyName(OldKeyName) then
  OldKeyWorkName := ConcatKeyNames(fCurrentKeyName,TrimKeyName(OldKeyName))
else
  OldKeyWorkName := TrimKeyName(OldKeyName);
If IsRelativeKeyName(NewKeyName) then
  NewKeyWorkName := ConcatKeyNames(fCurrentKeyName,TrimKeyName(NewKeyName))
else
  NewKeyWorkName := TrimKeyName(NewKeyName);
Result := RenameKey(fRootKey,OldKeyWorkName,NewKeyWorkName,CopySecurityDescriptors);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CopyValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,False);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,False);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValue(const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,False);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValue(const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,False);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CopyValueTo(const SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  DstKey: HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
  try
    Result := MoveValue(GetWorkingKey(True),SrcValueName,DstKey,DstValueName,False);
  finally
    AuxCloseKey(DstKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValueTo(const SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  DstKey: HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
  try
    Result := MoveValue(GetWorkingKey(True),SrcValueName,DstKey,DstValueName,False);
  finally
    AuxCloseKey(DstKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CopyValueFrom(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean;
var
  SrcKey: HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    Result := MoveValue(SrcKey,SrcValueName,GetWorkingKey(True),DstValueName,False);
  finally
    AuxCloseKey(SrcKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValueFrom(const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean;
var
  SrcKey: HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_QUERY_VALUE,SrcKey) then
  try
    Result := MoveValue(SrcKey,SrcValueName,GetWorkingKey(True),DstValueName,False);
  finally
    AuxCloseKey(SrcKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.CopyValueIn(RootKey: TRXPredefinedKey; const KeyName,SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,False);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValueIn(const KeyName,SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,False);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.CopyValueIn(const SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
TempKey := GetWorkingKey(True);
Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,False);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,True);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValue(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,True);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValue(const SrcKeyName,SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,True);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValue(const SrcKeyName,SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  SrcKey,DstKey:  HKEY;
begin
Result := False;
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
      try
        Result := MoveValue(SrcKey,SrcValueName,DstKey,DstValueName,True);
      finally
        AuxCloseKey(DstKey);
      end;
  finally
    AuxCloseKey(SrcKey);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveValueTo(const SrcValueName: String; DstRootKey: TRXPredefinedKey; const DstKeyName,DstValueName: String): Boolean;
var
  DstKey: HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(DstRootKey),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
  try
    Result := MoveValue(GetWorkingKey(True),SrcValueName,DstKey,DstValueName,True);
  finally
    AuxCloseKey(DstKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValueTo(const SrcValueName: String; const DstKeyName,DstValueName: String): Boolean;
var
  DstKey: HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(DstKeyName)),TrimKeyName(DstKeyName),KEY_SET_VALUE,DstKey) then
  try
    Result := MoveValue(GetWorkingKey(True),SrcValueName,DstKey,DstValueName,True);
  finally
    AuxCloseKey(DstKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveValueFrom(SrcRootKey: TRXPredefinedKey; const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean;
var
  SrcKey: HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(SrcRootKey),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    Result := MoveValue(SrcKey,SrcValueName,GetWorkingKey(True),DstValueName,True);
  finally
    AuxCloseKey(SrcKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValueFrom(const SrcKeyName,SrcValueName: String; const DstValueName: String): Boolean;
var
  SrcKey: HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(SrcKeyName)),TrimKeyName(SrcKeyName),KEY_SET_VALUE or KEY_QUERY_VALUE,SrcKey) then
  try
    Result := MoveValue(SrcKey,SrcValueName,GetWorkingKey(True),DstValueName,True);
  finally
    AuxCloseKey(SrcKey);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.MoveValueIn(RootKey: TRXPredefinedKey; const KeyName,SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,True);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValueIn(const KeyName,SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),KEY_QUERY_VALUE or KEY_SET_VALUE,TempKey) then
  try
    Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,True);
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.MoveValueIn(const SrcValueName,DstValueName: String): Boolean;
var
  TempKey:  HKEY;
begin
TempKey := GetWorkingKey(True);
Result := MoveValue(TempKey,SrcValueName,TempKey,DstValueName,True);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RenameValue(RootKey: TRXPredefinedKey; const KeyName,OldValueName,NewValueName: String): Boolean;
begin
Result := MoveValueIn(RootKey,KeyName,OldValueName,NewValueName);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.RenameValue(const KeyName,OldValueName,NewValueName: String): Boolean;
begin
Result := MoveValueIn(KeyName,OldValueName,NewValueName);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.RenameValue(const OldValueName,NewValueName: String): Boolean;
begin
Result := MoveValueIn(OldValueName,NewValueName);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.QueryBackupPrivilege: TRXPrivilegeStatus;
begin
Result := QueryProcessPrivilege('SeBackupPrivilege');
end;

//------------------------------------------------------------------------------

Function TRegistryEx.EnableBackupPrivilege: Boolean;
begin
Result := SetProcessPrivilege('SeBackupPrivilege',True);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DisableBackupPrivilege: Boolean;
begin
Result := SetProcessPrivilege('SeBackupPrivilege',False);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.QueryRestorePrivilege: TRXPrivilegeStatus;
begin
Result := QueryProcessPrivilege('SeRestorePrivilege');
end;

//------------------------------------------------------------------------------

Function TRegistryEx.EnableRestorePrivilege: Boolean;
begin
Result := SetProcessPrivilege('SeRestorePrivilege',True);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.DisableRestorePrivilege: Boolean;
begin
Result := SetProcessPrivilege('SeRestorePrivilege',False);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.SaveKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := CheckErrorCode(RegSaveKeyExW(TempKey,PWideChar(StrToWide(FileName)),nil,TranslateFileFormat(FileFormat)));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.SaveKey(const KeyName,FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := CheckErrorCode(RegSaveKeyExW(TempKey,PWideChar(StrToWide(FileName)),nil,TranslateFileFormat(FileFormat)));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.SaveKey(const FileName: String; FileFormat: TRXFileFormat = ffStandardFormat): Boolean;
begin
Result := CheckErrorCode(RegSaveKeyExW(GetWorkingKey(True),PWideChar(StrToWide(FileName)),nil,TranslateFileFormat(FileFormat)));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.RestoreKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(TranslatePredefinedKey(RootKey),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := CheckErrorCode(RegRestoreKeyW(TempKey,PWideChar(StrToWide(FileName)),TranslateRestoreFlag(Flag)));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.RestoreKey(const KeyName,FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean;
var
  TempKey:  HKEY;
begin
If AuxOpenKey(GetWorkingKey(IsRelativeKeyName(KeyName)),TrimKeyName(KeyName),STANDARD_RIGHTS_READ,TempKey) then
  try
    Result := CheckErrorCode(RegRestoreKeyW(TempKey,PWideChar(StrToWide(FileName)),TranslateRestoreFlag(Flag)));
  finally
    AuxCloseKey(TempKey);
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.RestoreKey(const FileName: String; Flag: TRXRestoreFlag = rfNone): Boolean;
begin
Result := CheckErrorCode(RegRestoreKeyW(GetWorkingKey(True),PWideChar(StrToWide(FileName)),TranslateRestoreFlag(Flag)));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.LoadKey(RootKey: TRXPredefinedKey; const KeyName, FileName: String): Boolean;
begin
Result := CheckErrorCode(RegLoadKeyW(TranslatePredefinedKey(RootKey),
  PWideChar(StrToWide(KeyName)),PWideChar(StrToWide(FileName))));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.UnLoadKey(RootKey: TRXPredefinedKey; const KeyName: String): Boolean;
begin
Result := CheckErrorCode(RegUnloadKeyW(TranslatePredefinedKey(RootKey),PWideChar(StrToWide(KeyName))));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReplaceKey(RootKey: TRXPredefinedKey; const KeyName,NewFileName,OldFileName: String): Boolean;
begin
Result := CheckErrorCode(RegReplaceKeyW(
  TranslatePredefinedKey(RootKey),PWideChar(StrToWide(TrimKeyName(KeyName))),
  PWideChar(StrToWide(NewFileName)),PWideChar(StrToWide(OldFileName))));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReplaceKey(const KeyName,NewFileName,OldFileName: String): Boolean;
begin
Result := CheckErrorCode(RegReplaceKeyW(
  GetWorkingKey(IsRelativeKeyName(KeyName)),PWideChar(StrToWide(TrimKeyName(KeyName))),
  PWideChar(StrToWide(NewFileName)),PWideChar(StrToWide(OldFileName))));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReplaceKey(const NewFileName,OldFileName: String): Boolean;
begin
Result := CheckErrorCode(RegReplaceKeyW(GetWorkingKey(True),nil,
  PWideChar(StrToWide(NewFileName)),PWideChar(StrToWide(OldFileName))));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ExportKey(RootKey: TRXPredefinedKey; const KeyName,FileName: String): Boolean;
begin
Result := ExecuteRegCommand(Format('EXPORT "%s" "%s"',[ConcatKeyNames(PredefinedKeyToShortStr(RootKey),TrimKeyName(KeyName)),FileName]));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ExportKey(const KeyName,FileName: String): Boolean;
var
  WorkKey:  String;
begin
GetWorkingKey(IsRelativeKeyName(KeyName),WorkKey);
Result := ExecuteRegCommand(Format('EXPORT "%s" "%s"',[
  ConcatKeyNames(ConcatKeyNames(PredefinedKeyToShortStr(fRootKey),WorkKey),TrimKeyName(KeyName)),FileName]));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ExportKey(const FileName: String): Boolean;
var
  WorkKey:  String;
begin
GetWorkingKey(True,WorkKey);
Result := ExecuteRegCommand(Format('EXPORT "%s" "%s"',[ConcatKeyNames(PredefinedKeyToShortStr(fRootKey),WorkKey),FileName]));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ImportKey(const FileName: String): Boolean;
begin
Result := ExecuteRegCommand(Format('IMPORT "%s"',[FileName]));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Boolean);
begin
WriteMacro('Bool',RootKey,KeyName,ValueName,BoolToNum(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBool(const KeyName,ValueName: String; Value: Boolean);
begin
WriteMacro('Bool',KeyName,ValueName,BoolToNum(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBool(const ValueName: String; Value: Boolean);
begin
SetValueData('Bool',GetWorkingKey(True),ValueName,BoolToNum(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int8);
begin
WriteMacro('Int8',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt8(const KeyName,ValueName: String; Value: Int8);
begin
WriteMacro('Int8',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt8(const ValueName: String; Value: Int8);
begin
SetValueData('Int8',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt8);
begin
WriteMacro('UInt8',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt8(const KeyName,ValueName: String; Value: UInt8);
begin
WriteMacro('UInt8',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt8(const ValueName: String; Value: UInt8);
begin
SetValueData('UInt8',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int16);
begin
WriteMacro('Int16',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt16(const KeyName,ValueName: String; Value: Int16);
begin
WriteMacro('Int16',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt16(const ValueName: String; Value: Int16); 
begin
SetValueData('Int16',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt16);
begin
WriteMacro('UInt16',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt16(const KeyName,ValueName: String; Value: UInt16);
begin
WriteMacro('UInt16',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt16(const ValueName: String; Value: UInt16); 
begin
SetValueData('UInt16',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int32);
begin
WriteMacro('Int32',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt32(const KeyName,ValueName: String; Value: Int32);
begin
WriteMacro('Int32',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt32(const ValueName: String; Value: Int32);
begin
SetValueData('Int32',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt32);
begin
WriteMacro('UInt32',RootKey,KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt32(const KeyName,ValueName: String; Value: UInt32);
begin
WriteMacro('UInt32',KeyName,ValueName,Integer(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt32(const ValueName: String; Value: UInt32); 
begin
SetValueData('UInt32',GetWorkingKey(True),ValueName,Integer(Value));
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Int64);
begin
WriteMacro('Int64',RootKey,KeyName,ValueName,Value,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt64(const KeyName,ValueName: String; Value: Int64);
begin
WriteMacro('Int64',KeyName,ValueName,Value,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInt64(const ValueName: String; Value: Int64);
begin
SetValueData('Int64',GetWorkingKey(True),ValueName,Value,SizeOf(Int64),vtQWord);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: UInt64);
begin
WriteMacro('UInt64',RootKey,KeyName,ValueName,Value,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt64(const KeyName,ValueName: String; Value: UInt64);
begin
WriteMacro('UInt64',KeyName,ValueName,Value,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteUInt64(const ValueName: String; Value: UInt64);
begin
SetValueData('UInt64',GetWorkingKey(True),ValueName,Value,SizeOf(Int64),vtQWord);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Integer);
begin
WriteMacro('Integer',RootKey,KeyName,ValueName,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInteger(const KeyName,ValueName: String; Value: Integer);
begin
WriteMacro('Integer',KeyName,ValueName,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteInteger(const ValueName: String; Value: Integer);
begin
SetValueData('Integer',GetWorkingKey(True),ValueName,Value);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Float32);
begin
WriteMacro('Float32',RootKey,KeyName,ValueName,Value,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat32(const KeyName,ValueName: String; Value: Float32);
begin
WriteMacro('Float32',KeyName,ValueName,Value,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat32(const ValueName: String; Value: Float32);
begin
SetValueData('Float32',GetWorkingKey(True),ValueName,Value,SizeOf(Float32),vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Float64);
begin
WriteMacro('Float64',RootKey,KeyName,ValueName,Value,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat64(const KeyName,ValueName: String; Value: Float64);
begin
WriteMacro('Float64',KeyName,ValueName,Value,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat64(const ValueName: String; Value: Float64);
begin
SetValueData('Float64',GetWorkingKey(True),ValueName,Value,SizeOf(Float64),vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Double);
begin
WriteMacro('Float',RootKey,KeyName,ValueName,Value,SizeOf(Double),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat(const KeyName,ValueName: String; Value: Double);
begin
WriteMacro('Float',KeyName,ValueName,Value,SizeOf(Double),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteFloat(const ValueName: String; Value: Double);
begin
SetValueData('Float',GetWorkingKey(True),ValueName,Value,SizeOf(Double),vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: Currency);
begin
WriteMacro('Currency',RootKey,KeyName,ValueName,Value,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteCurrency(const KeyName,ValueName: String; Value: Currency);
begin
WriteMacro('Currency',KeyName,ValueName,Value,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteCurrency(const ValueName: String; Value: Currency);
begin
SetValueData('Currency',GetWorkingKey(True),ValueName,Value,SizeOf(Currency),vtBinary);
end;
 
//------------------------------------------------------------------------------

procedure TRegistryEx.WriteDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime);
begin
WriteMacro('DateTime',RootKey,KeyName,ValueName,Value,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteDateTime(const KeyName,ValueName: String; Value: TDateTime);
begin
WriteMacro('DateTime',KeyName,ValueName,Value,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteDateTime(const ValueName: String; Value: TDateTime);
begin
SetValueData('DateTime',GetWorkingKey(True),ValueName,Value,SizeOf(TDateTime),vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Int(Value);
WriteMacro('Date',RootKey,KeyName,ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteDate(const KeyName,ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Int(Value);
WriteMacro('Date',KeyName,ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteDate(const ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Int(Value);
SetValueData('Date',GetWorkingKey(True),ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;
 
//------------------------------------------------------------------------------

procedure TRegistryEx.WriteTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Frac(Value);
WriteMacro('Time',RootKey,KeyName,ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteTime(const KeyName,ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Frac(Value);
WriteMacro('Time',KeyName,ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteTime(const ValueName: String; Value: TDateTime);
var
  Temp: TDateTime;
begin
Temp := Frac(Value);
SetValueData('Time',GetWorkingKey(True),ValueName,Temp,SizeOf(TDateTime),vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value: String);
var
  Temp: WideString;
begin
Temp := StrToWide(Value);
WriteMacro('String',RootKey,KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1{terminating zero}) * SizeOf(WideChar),vtString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteString(const KeyName,ValueName: String; const Value: String);
var
  Temp: WideString;
begin
Temp := StrToWide(Value);
WriteMacro('String',KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteString(const ValueName: String; const Value: String);
var
  Temp: WideString;
begin
Temp := StrToWide(Value);
SetValueData('String',GetWorkingKey(True),ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtString);
end;
 
//------------------------------------------------------------------------------

procedure TRegistryEx.WriteExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Value: String; UnExpand: Boolean = False);
var
  Temp: WideString;
begin
If UnExpand then
  Temp := UnExpandString(Value)
else
  Temp := StrToWide(Value);
WriteMacro('ExpandString',RootKey,KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtExpandString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteExpandString(const KeyName,ValueName: String; const Value: String; UnExpand: Boolean = False);
var
  Temp: WideString;
begin
If UnExpand then
  Temp := UnExpandString(Value)
else
  Temp := StrToWide(Value);
WriteMacro('ExpandString',KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtExpandString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteExpandString(const ValueName: String; const Value: String; UnExpand: Boolean = False);
var
  Temp: WideString;
begin
If UnExpand then
  Temp := UnExpandString(Value)
else
  Temp := StrToWide(Value);
SetValueData('ExpandString',GetWorkingKey(True),ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtExpandString);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
Temp := UnParseMultiString(Value);
WriteMacro('MultiString',RootKey,KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtMultiString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteMultiString(const KeyName,ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
Temp := UnParseMultiString(Value);
WriteMacro('MultiString',KeyName,ValueName,PWideChar(Temp)^,(Length(Temp) + 1) * SizeOf(WideChar),vtMultiString);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteMultiString(const ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
Temp := UnParseMultiString(Value);
SetValueData('MultiString',GetWorkingKey(True),ValueName,PWideChar(Temp)^,Length(Temp) * SizeOf(WideChar),vtMultiString);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Buff; Size: TMemSize);
begin
WriteMacro('BinaryBuffer',RootKey,KeyName,ValueName,Buff,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryBuffer(const KeyName,ValueName: String; const Buff; Size: TMemSize);
begin
WriteMacro('BinaryBuffer',KeyName,ValueName,Buff,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryBuffer(const ValueName: String; const Buff; Size: TMemSize);
begin
SetValueData('BinaryBuffer',GetWorkingKey(True),ValueName,Buff,Size,vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize);
begin
WriteMacro('BinaryMemory',RootKey,KeyName,ValueName,Memory^,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize);
begin
WriteMacro('BinaryMemory',KeyName,ValueName,Memory^,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryMemory(const ValueName: String; Memory: Pointer; Size: TMemSize);
begin
SetValueData('BinaryMemory',GetWorkingKey(True),ValueName,Memory^,Size,vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream; Position, Count: Int64);
var
  Buffer: Pointer;
begin
GetMem(Buffer,TMemSize(Count));
try
  Stream.Seek(Position,soBeginning);
  Stream.ReadBuffer(Buffer^,Count);
  WriteMacro('BinaryStream',RootKey,KeyName,ValueName,Buffer^,TMemSize(Count),vtBinary);
finally
  FreeMem(Buffer,TMemSize(Count));
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryStream(const KeyName,ValueName: String; Stream: TStream; Position, Count: Int64);
var
  Buffer: Pointer;
begin
GetMem(Buffer,TMemSize(Count));
try
  Stream.Seek(Position,soBeginning);
  Stream.ReadBuffer(Buffer^,Count);
  WriteMacro('BinaryStream',KeyName,ValueName,Buffer^,TMemSize(Count),vtBinary);
finally
  FreeMem(Buffer,TMemSize(Count));
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryStream(const ValueName: String; Stream: TStream; Position, Count: Int64);
var
  Buffer: Pointer;
begin
GetMem(Buffer,TMemSize(Count));
try
  Stream.Seek(Position,soBeginning);
  Stream.ReadBuffer(Buffer^,Count);
  SetValueData('BinaryStream',GetWorkingKey(True),ValueName,Buffer^,TMemSize(Count),vtBinary);
finally
  FreeMem(Buffer,TMemSize(Count));
end;
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.WriteBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream);
begin
WriteBinaryStream(RootKey,KeyName,ValueName,Stream,0,Stream.Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryStream(const KeyName,ValueName: String; Stream: TStream);
begin
WriteBinaryStream(KeyName,ValueName,Stream,0,Stream.Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.WriteBinaryStream(const ValueName: String; Stream: TStream);
begin
WriteBinaryStream(ValueName,Stream,0,Stream.Size);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Boolean): Boolean;
begin
Value := TryReadMacro(RootKey,KeyName,ValueName,Result) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBool(const KeyName,ValueName: String; out Value: Boolean): Boolean;
begin
Value := TryReadMacro(KeyName,ValueName,Result) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBool(const ValueName: String; out Value: Boolean): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := Temp <> 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int8): Boolean;
begin
Value := Int8(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt8(const KeyName,ValueName: String; out Value: Int8): Boolean;
begin
Value := Int8(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt8(const ValueName: String; out Value: Int8): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := Int8(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt8): Boolean;
begin
Value := UInt8(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt8(const KeyName,ValueName: String; out Value: UInt8): Boolean;
begin
Value := UInt8(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt8(const ValueName: String; out Value: UInt8): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := UInt8(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int16): Boolean;
begin
Value := Int16(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt16(const KeyName,ValueName: String; out Value: Int16): Boolean;
begin
Value := Int16(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt16(const ValueName: String; out Value: Int16): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := Int16(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt16): Boolean;
begin
Value := UInt16(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt16(const KeyName,ValueName: String; out Value: UInt16): Boolean;
begin
Value := UInt16(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt16(const ValueName: String; out Value: UInt16): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := UInt16(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int32): Boolean;
begin
Value := Int32(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt32(const KeyName,ValueName: String; out Value: Int32): Boolean;
begin
Value := Int32(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt32(const ValueName: String; out Value: Int32): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := Int32(Temp);
end;
 
//------------------------------------------------------------------------------

Function TRegistryEx.TryReadUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt32): Boolean;
begin
Value := UInt32(TryReadMacro(RootKey,KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt32(const KeyName,ValueName: String; out Value: UInt32): Boolean;
begin
Value := UInt32(TryReadMacro(KeyName,ValueName,Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt32(const ValueName: String; out Value: UInt32): Boolean;
var
  Temp: Integer;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Temp);
Value := UInt32(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Int64): Boolean;
begin
Result := TryReadMacro(RootKey,KeyName,ValueName,Value,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt64(const KeyName,ValueName: String; out Value: Int64): Boolean;
begin
Result := TryReadMacro(KeyName,ValueName,Value,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInt64(const ValueName: String; out Value: Int64): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value,SizeOf(Int64),vtQWord);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: UInt64): Boolean;
begin
Result := TryReadMacro(RootKey,KeyName,ValueName,Value,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt64(const KeyName,ValueName: String; out Value: UInt64): Boolean;
begin
Result := TryReadMacro(KeyName,ValueName,Value,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadUInt64(const ValueName: String; out Value: UInt64): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value,SizeOf(UInt64),vtQWord);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Integer): Boolean;
begin
Value := TryReadMacro(RootKey,KeyName,ValueName,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInteger(const KeyName,ValueName: String; out Value: Integer): Boolean;
begin
Value := TryReadMacro(KeyName,ValueName,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadInteger(const ValueName: String; out Value: Integer): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Float32): Boolean;
begin
Result := TryReadMacro(RootKey,KeyName,ValueName,Value,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat32(const KeyName,ValueName: String; out Value: Float32): Boolean;
begin
Result := TryReadMacro(KeyName,ValueName,Value,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat32(const ValueName: String; out Value: Float32): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value,SizeOf(Float32),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Float64): Boolean;
begin
Result := TryReadMacro(RootKey,KeyName,ValueName,Value,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat64(const KeyName,ValueName: String; out Value: Float64): Boolean;
begin
Result := TryReadMacro(KeyName,ValueName,Value,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat64(const ValueName: String; out Value: Float64): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value,SizeOf(Float64),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Double): Boolean;
begin
Result := TryReadFloat64(RootKey,KeyName,ValueName,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat(const KeyName,ValueName: String; out Value: Double): Boolean;
begin
Result := TryReadFloat64(KeyName,ValueName,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadFloat(const ValueName: String; out Value: Double): Boolean;
begin
Result := TryReadFloat64(ValueName,Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: Currency): Boolean;
begin
Result := TryReadMacro(RootKey,KeyName,ValueName,Value,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadCurrency(const KeyName,ValueName: String; out Value: Currency): Boolean;
begin
Result := TryReadMacro(KeyName,ValueName,Value,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadCurrency(const ValueName: String; out Value: Currency): Boolean;
begin
Result := GetValueData(GetWorkingKey(True),ValueName,Value,SizeOf(Currency),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(RootKey,KeyName,ValueName,Double(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadDateTime(const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(KeyName,ValueName,Double(Value));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadDateTime(const ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(ValueName,Double(Value));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(RootKey,KeyName,ValueName,Double(Value));
Value := Int(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadDate(const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(KeyName,ValueName,Double(Value));
Value := Int(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadDate(const ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(ValueName,Double(Value));
Value := Int(Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(RootKey,KeyName,ValueName,Double(Value));
Value := Frac(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadTime(const KeyName,ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(KeyName,ValueName,Double(Value));
Value := Frac(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadTime(const ValueName: String; out Value: TDateTime): Boolean;
begin
Result := TryReadFloat64(ValueName,Double(Value));
Value := Frac(Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: String): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(RootKey,KeyName,ValueName,Temp,vtString);
If Result then
  Value := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadString(const KeyName,ValueName: String; out Value: String): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(KeyName,ValueName,Temp,vtString);
If Result then
  Value := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadString(const ValueName: String; out Value: String): Boolean;
var
  Temp: WideString;
begin
Result := GetValueDataOut(GetWorkingKey(True),ValueName,Temp,vtString);
If Result then
  Value := WideToStr(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Value: String; Expand: Boolean = False): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(RootKey,KeyName,ValueName,Temp,vtExpandString);
If Result then
  begin
    If Expand then
      Value := ExpandString(Temp)
    else
      Value := WideToStr(Temp);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadExpandString(const KeyName,ValueName: String; out Value: String; Expand: Boolean = False): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(KeyName,ValueName,Temp,vtExpandString);
If Result then
  begin
    If Expand then
      Value := ExpandString(Temp)
    else
      Value := WideToStr(Temp);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadExpandString(const ValueName: String; out Value: String; Expand: Boolean = False): Boolean;
var
  Temp: WideString;
begin
Result := GetValueDataOut(GetWorkingKey(True),ValueName,Temp,vtExpandString);
If Result then
  begin
    If Expand then
      Value := ExpandString(Temp)
    else
      Value := WideToStr(Temp);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(RootKey,KeyName,ValueName,Temp,vtMultiString);
If Result then
  ParseMultiString(Temp,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMultiString(const KeyName,ValueName: String; Value: TStrings): Boolean;
var
  Temp: WideString;
begin
Result := TryReadMacroOut(KeyName,ValueName,Temp,vtMultiString);
If Result then
  ParseMultiString(Temp,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadMultiString(const ValueName: String; Value: TStrings): Boolean;
var
  Temp: WideString;
begin
Result := GetValueDataOut(GetWorkingKey(True),ValueName,Temp,vtMultiString);
If Result then
  ParseMultiString(Temp,Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Buff; var Size: TMemSize): Boolean;
begin
Result := TryReadMacroExtBuff(RootKey,KeyName,ValueName,Buff,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryBuffer(const KeyName,ValueName: String; out Buff; var Size: TMemSize): Boolean;
begin
Result := TryReadMacroExtBuff(KeyName,ValueName,Buff,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryBuffer(const ValueName: String; out Buff; var Size: TMemSize): Boolean;
begin
Result := GetValueDataExtBuff(GetWorkingKey(True),ValueName,Buff,Size,vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean;
begin
Result := TryReadMacroExtBuff(RootKey,KeyName,ValueName,Memory^,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean;
begin
Result := TryReadMacroExtBuff(KeyName,ValueName,Memory^,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryMemory(const ValueName: String; Memory: Pointer; var Size: TMemSize): Boolean;
begin
Result := GetValueDataExtBuff(GetWorkingKey(True),ValueName,Memory^,Size,vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadBinaryMemoryOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean;
begin
Result := TryReadMacroOut(RootKey,KeyName,ValueName,Memory,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryMemoryOut(const KeyName,ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean;
begin
Result := TryReadMacroOut(KeyName,ValueName,Memory,Size,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryMemoryOut(const ValueName: String; out Memory: Pointer; out Size: TMemSize): Boolean;
begin
Result := GetValueDataOut(GetWorkingKey(True),ValueName,Memory,Size,vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.TryReadBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream): Boolean;
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
Result := TryReadMacroOut(RootKey,KeyName,ValueName,Buffer,Size,vtBinary);
If Result then
  try
    Stream.WriteBuffer(Buffer^,LongInt(Size));
  finally
    FreeMem(Buffer,Size);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryStream(const KeyName,ValueName: String; Stream: TStream): Boolean;
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
Result := TryReadMacroOut(KeyName,ValueName,Buffer,Size,vtBinary);
If Result then
  try
    Stream.WriteBuffer(Buffer^,LongInt(Size));
  finally
    FreeMem(Buffer,Size);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.TryReadBinaryStream(const ValueName: String; Stream: TStream): Boolean;
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
Result := GetValueDataOut(GetWorkingKey(True),ValueName,Buffer,Size,vtBinary);
If Result then
  try
    Stream.WriteBuffer(Buffer^,LongInt(Size));
  finally
    FreeMem(Buffer,Size);
  end;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadBoolDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Boolean): Boolean;
begin
If not TryReadBool(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBoolDef(const KeyName,ValueName: String; Default: Boolean): Boolean;
begin
If not TryReadBool(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBoolDef(const ValueName: String; Default: Boolean): Boolean;
begin
If not TryReadBool(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt8Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int8): Int8;
begin
If not TryReadInt8(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt8Def(const KeyName,ValueName: String; Default: Int8): Int8;
begin
If not TryReadInt8(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt8Def(const ValueName: String; Default: Int8): Int8;
begin
If not TryReadInt8(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt8Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt8): UInt8;
begin
If not TryReadUInt8(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt8Def(const KeyName,ValueName: String; Default: UInt8): UInt8;
begin
If not TryReadUInt8(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt8Def(const ValueName: String; Default: UInt8): UInt8;
begin
If not TryReadUInt8(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt16Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int16): Int16;
begin
If not TryReadInt16(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt16Def(const KeyName,ValueName: String; Default: Int16): Int16;
begin
If not TryReadInt16(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt16Def(const ValueName: String; Default: Int16): Int16;
begin
If not TryReadInt16(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt16Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt16): UInt16;
begin
If not TryReadUInt16(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt16Def(const KeyName,ValueName: String; Default: UInt16): UInt16;
begin
If not TryReadUInt16(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt16Def(const ValueName: String; Default: UInt16): UInt16;
begin
If not TryReadUInt16(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int32): Int32;
begin
If not TryReadInt32(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt32Def(const KeyName,ValueName: String; Default: Int32): Int32;
begin
If not TryReadInt32(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt32Def(const ValueName: String; Default: Int32): Int32;
begin
If not TryReadInt32(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt32): UInt32;
begin
If not TryReadUInt32(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt32Def(const KeyName,ValueName: String; Default: UInt32): UInt32;
begin
If not TryReadUInt32(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt32Def(const ValueName: String; Default: UInt32): UInt32;
begin
If not TryReadUInt32(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Int64): Int64;
begin
If not TryReadInt64(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt64Def(const KeyName,ValueName: String; Default: Int64): Int64;
begin
If not TryReadInt64(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt64Def(const ValueName: String; Default: Int64): Int64;
begin
If not TryReadInt64(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: UInt64): UInt64;
begin
If not TryReadUInt64(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt64Def(const KeyName,ValueName: String; Default: UInt64): UInt64;
begin
If not TryReadUInt64(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt64Def(const ValueName: String; Default: UInt64): UInt64;
begin
If not TryReadUInt64(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadIntegerDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Integer): Integer;
begin
If not TryReadInteger(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadIntegerDef(const KeyName,ValueName: String; Default: Integer): Integer;
begin
If not TryReadInteger(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadIntegerDef(const ValueName: String; Default: Integer): Integer;
begin
If not TryReadInteger(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloat32Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Float32): Float32;
begin
If not TryReadFloat32(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat32Def(const KeyName,ValueName: String; Default: Float32): Float32;
begin
If not TryReadFloat32(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat32Def(const ValueName: String; Default: Float32): Float32;
begin
If not TryReadFloat32(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloat64Def(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Float64): Float64;
begin
If not TryReadFloat64(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat64Def(const KeyName,ValueName: String; Default: Float64): Float64;
begin
If not TryReadFloat64(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat64Def(const ValueName: String; Default: Float64): Float64;
begin
If not TryReadFloat64(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloatDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Double): Double;
begin
If not TryReadFloat(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloatDef(const KeyName,ValueName: String; Default: Double): Double;
begin
If not TryReadFloat(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloatDef(const ValueName: String; Default: Double): Double;
begin
If not TryReadFloat(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadCurrencyDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: Currency): Currency;
begin
If not TryReadCurrency(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadCurrencyDef(const KeyName,ValueName: String; Default: Currency): Currency;
begin
If not TryReadCurrency(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadCurrencyDef(const ValueName: String; Default: Currency): Currency;
begin
If not TryReadCurrency(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadDateTimeDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDateTime(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateTimeDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDateTime(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateTimeDef(const ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDateTime(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadDateDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDate(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDate(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateDef(const ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadDate(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadTimeDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadTime(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadTimeDef(const KeyName,ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadTime(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadTimeDef(const ValueName: String; Default: TDateTime): TDateTime;
begin
If not TryReadTime(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadStringDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Default: String): String;
begin
If not TryReadString(RootKey,KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadStringDef(const KeyName,ValueName: String; const Default: String): String;
begin
If not TryReadString(KeyName,ValueName,Result) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadStringDef(const ValueName: String; const Default: String): String;
begin
If not TryReadString(ValueName,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadExpandStringDef(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; const Default: String; Expand: Boolean = False): String;
begin
If not TryReadExpandString(RootKey,KeyName,ValueName,Result,Expand) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadExpandStringDef(const KeyName,ValueName: String; const Default: String; Expand: Boolean = False): String;
begin
If not TryReadExpandString(KeyName,ValueName,Result,Expand) then
  Result := Default;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadExpandStringDef(const ValueName: String; const Default: String; Expand: Boolean = False): String;
begin
If not TryReadExpandString(ValueName,Result,Expand) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadBool(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Boolean;
begin
Result := ReadMacro('Bool',RootKey,KeyName,ValueName) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBool(const KeyName,ValueName: String): Boolean;
begin
Result := ReadMacro('Bool',KeyName,ValueName) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBool(const ValueName: String): Boolean;
begin
Result := ReadMacro('Bool',ValueName) <> 0;
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int8;
begin
Result := Int8(ReadMacro('Int8',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt8(const KeyName,ValueName: String): Int8;
begin
Result := Int8(ReadMacro('Int8',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt8(const ValueName: String): Int8;
begin
Result := Int8(ReadMacro('Int8',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt8(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt8;
begin
Result := UInt8(ReadMacro('UInt8',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt8(const KeyName,ValueName: String): UInt8;
begin
Result := UInt8(ReadMacro('UInt8',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt8(const ValueName: String): UInt8;
begin
Result := UInt8(ReadMacro('UInt8',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int16;
begin
Result := Int16(ReadMacro('Int16',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt16(const KeyName,ValueName: String): Int16;
begin
Result := Int16(ReadMacro('Int16',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt16(const ValueName: String): Int16;
begin
Result := Int16(ReadMacro('Int16',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt16(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt16;
begin
Result := UInt16(ReadMacro('UInt16',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt16(const KeyName,ValueName: String): UInt16;
begin
Result := UInt16(ReadMacro('UInt16',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt16(const ValueName: String): UInt16;
begin
Result := UInt16(ReadMacro('UInt16',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int32;
begin
Result := Int32(ReadMacro('Int32',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt32(const KeyName,ValueName: String): Int32;
begin
Result := Int32(ReadMacro('Int32',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt32(const ValueName: String): Int32;
begin
Result := Int32(ReadMacro('Int32',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt32;
begin
Result := UInt32(ReadMacro('UInt32',RootKey,KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt32(const KeyName,ValueName: String): UInt32;
begin
Result := UInt32(ReadMacro('UInt32',KeyName,ValueName));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt32(const ValueName: String): UInt32;
begin
Result := UInt32(ReadMacro('UInt32',ValueName));
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Int64;
begin
ReadMacro('Int64',RootKey,KeyName,ValueName,Result,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt64(const KeyName,ValueName: String): Int64;
begin
ReadMacro('Int64',KeyName,ValueName,Result,SizeOf(Int64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInt64(const ValueName: String): Int64;
begin
ReadMacro('Int64',ValueName,Result,SizeOf(Int64),vtQWord);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadUInt64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): UInt64;
begin
ReadMacro('UInt64',RootKey,KeyName,ValueName,Result,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt64(const KeyName,ValueName: String): UInt64;
begin
ReadMacro('UInt64',KeyName,ValueName,Result,SizeOf(UInt64),vtQWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadUInt64(const ValueName: String): UInt64;
begin
ReadMacro('UInt64',ValueName,Result,SizeOf(UInt64),vtQWord);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadInteger(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Integer;
begin
Result := ReadMacro('Integer',RootKey,KeyName,ValueName);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInteger(const KeyName,ValueName: String): Integer;
begin
Result := ReadMacro('Integer',KeyName,ValueName);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadInteger(const ValueName: String): Integer;
begin
Result := ReadMacro('Integer',ValueName);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloat32(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Float32;
begin
ReadMacro('Float32',RootKey,KeyName,ValueName,Result,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat32(const KeyName,ValueName: String): Float32;
begin
ReadMacro('Float32',KeyName,ValueName,Result,SizeOf(Float32),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat32(const ValueName: String): Float32;
begin
ReadMacro('Float32',ValueName,Result,SizeOf(Float32),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloat64(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Float64;
begin
ReadMacro('Float64',RootKey,KeyName,ValueName,Result,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat64(const KeyName,ValueName: String): Float64;
begin
ReadMacro('Float64',KeyName,ValueName,Result,SizeOf(Float64),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat64(const ValueName: String): Float64;
begin
ReadMacro('Float64',ValueName,Result,SizeOf(Float64),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadFloat(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Double;
begin
ReadMacro('Float',RootKey,KeyName,ValueName,Result,SizeOf(Double),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat(const KeyName,ValueName: String): Double;
begin
ReadMacro('Float',KeyName,ValueName,Result,SizeOf(Double),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadFloat(const ValueName: String): Double;
begin
ReadMacro('Float',ValueName,Result,SizeOf(Double),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadCurrency(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): Currency;
begin
ReadMacro('Currency',RootKey,KeyName,ValueName,Result,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadCurrency(const KeyName,ValueName: String): Currency;
begin
ReadMacro('Currency',KeyName,ValueName,Result,SizeOf(Currency),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadCurrency(const ValueName: String): Currency;
begin
ReadMacro('Currency',ValueName,Result,SizeOf(Currency),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadDateTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('DateTime',RootKey,KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateTime(const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('DateTime',KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDateTime(const ValueName: String): TDateTime;
begin
ReadMacro('DateTime',ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadDate(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('Date',RootKey,KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDate(const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('Date',KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadDate(const ValueName: String): TDateTime;
begin
ReadMacro('Date',ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadTime(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('Time',RootKey,KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadTime(const KeyName,ValueName: String): TDateTime;
begin
ReadMacro('Time',KeyName,ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadTime(const ValueName: String): TDateTime;
begin
ReadMacro('Time',ValueName,Result,SizeOf(TDateTime),vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String): String;
var
  Temp: WideString;
begin
ReadMacroOut('String',RootKey,KeyName,ValueName,Temp,vtString);
Result := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadString(const KeyName,ValueName: String): String;
var
  Temp: WideString;
begin
ReadMacroOut('String',KeyName,ValueName,Temp,vtString);
Result := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadString(const ValueName: String): String;
var
  Temp: WideString;
begin
ReadMacroOut('String',ValueName,Temp,vtString);
Result := WideToStr(Temp);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadExpandString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Expand: Boolean = False): String;
var
  Temp: WideString;
begin
ReadMacroOut('ExpandString',RootKey,KeyName,ValueName,Temp,vtExpandString);
If Expand then
  Result := ExpandString(Temp)
else
  Result := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadExpandString(const KeyName,ValueName: String; Expand: Boolean = False): String;
var
  Temp: WideString;
begin
ReadMacroOut('ExpandString',KeyName,ValueName,Temp,vtExpandString);
If Expand then
  Result := ExpandString(Temp)
else
  Result := WideToStr(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadExpandString(const ValueName: String; Expand: Boolean = False): String;
var
  Temp: WideString;
begin
ReadMacroOut('ExpandString',ValueName,Temp,vtExpandString);
If Expand then
  Result := ExpandString(Temp)
else
  Result := WideToStr(Temp);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ReadMultiString(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
ReadMacroOut('MultiString',RootKey,KeyName,ValueName,Temp,vtMultiString);
ParseMultiString(Temp,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMultiString(const KeyName,ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
ReadMacroOut('MultiString',KeyName,ValueName,Temp,vtMultiString);
ParseMultiString(Temp,Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadMultiString(const ValueName: String; Value: TStrings);
var
  Temp: WideString;
begin
ReadMacroOut('MultiString',ValueName,Temp,vtMultiString);
ParseMultiString(Temp,Value);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadBinaryBuffer(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Buff; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryBuffer',RootKey,KeyName,ValueName,Buff,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryBuffer(const KeyName,ValueName: String; out Buff; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryBuffer',KeyName,ValueName,Buff,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryBuffer(const ValueName: String; out Buff; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryBuffer',ValueName,Buff,Result,vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadBinaryMemory(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryMemory',RootKey,KeyName,ValueName,Memory^,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryMemory(const KeyName,ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryMemory',KeyName,ValueName,Memory^,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryMemory(const ValueName: String; Memory: Pointer; Size: TMemSize): TMemSize;
begin
Result := Size;
ReadMacroExtBuff('BinaryMemory',ValueName,Memory^,Result,vtBinary);
end;

//------------------------------------------------------------------------------

Function TRegistryEx.ReadBinaryMemoryOut(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; out Memory: Pointer): TMemSize;
begin
ReadMacroOut('BinaryMemoryOut',RootKey,KeyName,ValueName,Memory,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryMemoryOut(const KeyName,ValueName: String; out Memory: Pointer): TMemSize;
begin
ReadMacroOut('BinaryMemoryOut',KeyName,ValueName,Memory,Result,vtBinary);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TRegistryEx.ReadBinaryMemoryOut(const ValueName: String; out Memory: Pointer): TMemSize;
begin
ReadMacroOut('BinaryMemoryOut',ValueName,Memory,Result,vtBinary);
end;

//------------------------------------------------------------------------------

procedure TRegistryEx.ReadBinaryStream(RootKey: TRXPredefinedKey; const KeyName,ValueName: String; Stream: TStream);
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
ReadMacroOut('BinaryStream',RootKey,KeyName,ValueName,Buffer,Size,vtBinary);
try
  Stream.WriteBuffer(Buffer^,LongInt(Size));
finally
  FreeMem(Buffer,Size);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadBinaryStream(const KeyName,ValueName: String; Stream: TStream);
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
ReadMacroOut('BinaryStream',KeyName,ValueName,Buffer,Size,vtBinary);
try
  Stream.WriteBuffer(Buffer^,LongInt(Size));
finally
  FreeMem(Buffer,Size);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TRegistryEx.ReadBinaryStream(const ValueName: String; Stream: TStream);
var
  Buffer: Pointer;
  Size:   TMemSize;
begin
ReadMacroOut('BinaryStream',ValueName,Buffer,Size,vtBinary);
try
  Stream.WriteBuffer(Buffer^,LongInt(Size));
finally
  FreeMem(Buffer,Size);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization
--------------------------------------------------------------------------------
===============================================================================}
var
  AdvApi32Handle: TDLULibraryHandle;

//------------------------------------------------------------------------------

procedure UnitInitialize;
begin
AdvApi32Handle := OpenAndCheckLibrary('advapi32.dll');
{$IFNDEF CPU64bit}
{
  Functions Reg*ReflectionKey are always loaded on 64bit Windows. On 32bit
  system, they are loaded only on Vista and newer or when running under WoW64
  (including Windows XP 64bit).
}
If IsWindowsVistaOrGreater or IsRunningUnderWoW64 then
{$ENDIF}
  begin
    RegQueryReflectionKey := GetAndCheckSymbolAddr(AdvApi32Handle,'RegQueryReflectionKey');
    RegEnableReflectionKey := GetAndCheckSymbolAddr(AdvApi32Handle,'RegEnableReflectionKey');
    RegDisableReflectionKey := GetAndCheckSymbolAddr(AdvApi32Handle,'RegDisableReflectionKey');
  end;
If IsWindowsVistaOrGreater then
  RegDisablePredefinedCacheEx := GetAndCheckSymbolAddr(AdvApi32Handle,'RegDisablePredefinedCacheEx');
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
CloseLibrary(AdvApi32Handle);
end;

//------------------------------------------------------------------------------

initialization
  UnitInitialize;

finalization
  UnitFinalize;

end.

