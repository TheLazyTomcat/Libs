{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinFileInfo

    Main aim of this library is to provide a simple way of obtaining file
    information such as size, attributes, time of creation and, in case of
    binaries on Windows OS, a version information.

    A complete parsing of raw version information data is implemented, so
    it is possible to obtain information even from badly constructed version
    info resource.

    Although the library was intended only for Windows OS, it can now be
    compiled for other systems too. But in such case, it provides only a
    limited file information set.

  Version 1.1.2 (2024-03-04)

  Last change 2024-03-04

  ©2015-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.WinFileInfo

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
  * StrRect  - github.com/TheLazyTomcat/Lib.StrRect

    StrRect is required only when compiling for Windows OS.

===============================================================================}
unit WinFileInfo;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
{
  There is FPC-specific code used in non-Windows systems, therefore compilation
  for these systems has to be allowed only on FPC.
}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported OS-compiler combination.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes, {$IFDEF Windows}Windows,{$ELSE}UnixType,{$ENDIF}
  AuxTypes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EWFIException = class(Exception);

  EWFIFileError        = class(EWFIException);
  EWFIProcessingError  = class(EWFIException);
  EWFISystemError      = class(EWFIException);
  EWFIIndexOutOfBounds = class(EWFIException);  // used only in windows

{===============================================================================
--------------------------------------------------------------------------------
                                Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - declaration
===============================================================================}
{
  Returns size of the given file in bytes.
  
  If the file does not exist, it will raise an EWFIFileError exception.
}
Function GetFileSize(const FileName: String): UInt64;

//------------------------------------------------------------------------------
{
  FileSizeToStr expects passed number to be a file size and converts it to its
  string representation (as a decimal number if needed), including proper unit
  (KiB, MiB, ...).

    NOTE - version without FormatSettings parameter is not thread safe, use
           FileSizeToStrThr in non-main thread(s) if you cannot provide filled
           format settings (it uses default settings provided by OS or RLT).
}
Function FileSizeToStr(FileSize: UInt64; FormatSettings: TFormatSettings; SpaceUnit: Boolean = True): String; overload;
Function FileSizeToStr(FileSize: UInt64; SpaceUnit: Boolean = True): String; overload;

Function FileSizeToStrThr(FileSize: UInt64; SpaceUnit: Boolean = True): String;

//------------------------------------------------------------------------------
{
  Returns true when both paths (A and B) points to the same file, false
  otherwise.
  If either of the two paths points to a file that does not exist, the function
  will raise an EWFIFileError exception.
}
Function SameFile(const A,B: String): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                  TWinFileInfo
--------------------------------------------------------------------------------
===============================================================================}
type
  TWFIFileHandle = {$IFDEF Windows}THandle{$ELSE}cint{$ENDIF};

{===============================================================================
    TWinFileInfo - constants
===============================================================================}
{$IFDEF Windows}
const
  // File attributes flags
  INVALID_FILE_ATTRIBUTES = DWORD(-1); 

  FILE_ATTRIBUTE_ARCHIVE             = $20;
  FILE_ATTRIBUTE_COMPRESSED          = $800;
  FILE_ATTRIBUTE_DEVICE              = $40;
  FILE_ATTRIBUTE_DIRECTORY           = $10;
  FILE_ATTRIBUTE_ENCRYPTED           = $4000;
  FILE_ATTRIBUTE_HIDDEN              = $2;
  FILE_ATTRIBUTE_INTEGRITY_STREAM    = $8000;
  FILE_ATTRIBUTE_NORMAL              = $80;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  FILE_ATTRIBUTE_NO_SCRUB_DATA       = $20000;
  FILE_ATTRIBUTE_OFFLINE             = $1000;
  FILE_ATTRIBUTE_READONLY            = $1;
  FILE_ATTRIBUTE_REPARSE_POINT       = $400;
  FILE_ATTRIBUTE_SPARSE_FILE         = $200;
  FILE_ATTRIBUTE_SYSTEM              = $4;
  FILE_ATTRIBUTE_TEMPORARY           = $100;
  FILE_ATTRIBUTE_VIRTUAL             = $10000;

  // Flags for field TVSFixedFileInfo.dwFileFlags
  VS_FF_DEBUG        = $00000001;
  VS_FF_INFOINFERRED = $00000010;
  VS_FF_PATCHED      = $00000004;
  VS_FF_PRERELEASE   = $00000002;
  VS_FF_PRIVATEBUILD = $00000008;
  VS_FF_SPECIALBUILD = $00000020;

  // Flags for field TVSFixedFileInfo.dwFileOS
  VOS_DOS           = $00010000;
  VOS_NT            = $00040000;
  VOS__WINDOWS16    = $00000001;
  VOS__WINDOWS32    = $00000004;
  VOS_OS216         = $00020000;
  VOS_OS232         = $00030000;
  VOS__PM16         = $00000002;
  VOS__PM32         = $00000003;
  VOS_UNKNOWN       = $00000000;
  VOS_DOS_WINDOWS16 = $00010001;
  VOS_DOS_WINDOWS32 = $00010004;
  VOS_NT_WINDOWS32  = $00040004;
  VOS_OS216_PM16    = $00020002;
  VOS_OS232_PM32    = $00030003;

  // Flags for field TVSFixedFileInfo.dwFileType
  VFT_APP        = $00000001;
  VFT_DLL        = $00000002;
  VFT_DRV        = $00000003;
  VFT_FONT       = $00000004;
  VFT_STATIC_LIB = $00000007;
  VFT_UNKNOWN    = $00000000;
  VFT_VXD        = $00000005;

  // Flags for field TVSFixedFileInfo.dwFileSubtype when
  // TVSFixedFileInfo.dwFileType is set to VFT_DRV
  VFT2_DRV_COMM              = $0000000A;
  VFT2_DRV_DISPLAY           = $00000004;
  VFT2_DRV_INSTALLABLE       = $00000008;
  VFT2_DRV_KEYBOARD          = $00000002;
  VFT2_DRV_LANGUAGE          = $00000003;
  VFT2_DRV_MOUSE             = $00000005;
  VFT2_DRV_NETWORK           = $00000006;
  VFT2_DRV_PRINTER           = $00000001;
  VFT2_DRV_SOUND             = $00000009;
  VFT2_DRV_SYSTEM            = $00000007;
  VFT2_DRV_VERSIONED_PRINTER = $0000000C;
  VFT2_UNKNOWN               = $00000000;

  // Flags for field TVSFixedFileInfo.dwFileSubtype when
  // TVSFixedFileInfo.dwFileType is set to VFT_FONT
  VFT2_FONT_RASTER   = $00000001;
  VFT2_FONT_TRUETYPE = $00000003;
  VFT2_FONT_VECTOR   = $00000002;

{$ENDIF}

{===============================================================================
    TWinFileInfo - types
===============================================================================}
{$IFDEF Windows}
{
  Following structures are used to store information about requested file in
  a more user-friendly and better accessible way.
}  
type
  TWFIFileAttributesDecoded = record
    Archive:            Boolean;
    Compressed:         Boolean;
    Device:             Boolean;
    Directory:          Boolean;
    Encrypted:          Boolean;
    Hidden:             Boolean;
    IntegrityStream:    Boolean;
    Normal:             Boolean;
    NotContentIndexed:  Boolean;
    NoScrubData:        Boolean;
    Offline:            Boolean;
    ReadOnly:           Boolean;
    ReparsePoint:       Boolean;
    SparseFile:         Boolean;
    System:             Boolean;
    Temporary:          Boolean;
    Virtual:            Boolean;
  end;

//------------------------------------------------------------------------------
{
  Group of structures used to store decoded information from fixed file info
  part of version information resource.
}
type
  TWFIFixedFileInfo_VersionMembers = record
    Major:    UInt16;
    Minor:    UInt16;
    Release:  UInt16;
    Build:    UInt16;
  end;  

  TWFIFixedFileInfo_FileFlags = record
    Debug:        Boolean;
    InfoInferred: Boolean;
    Patched:      Boolean;
    Prerelease:   Boolean;
    PrivateBuild: Boolean;
    SpecialBuild: Boolean;
  end;

  TWFIFixedFileInfoDecoded = record
    FileVersionFull:        UInt64;
    FileVersionMembers:     TWFIFixedFileInfo_VersionMembers;
    FileVersionStr:         String;
    ProductVersionFull:     UInt64;
    ProductVersionMembers:  TWFIFixedFileInfo_VersionMembers;
    ProductVersionStr:      String;
    FileFlags:              TWFIFixedFileInfo_FileFlags;
    FileOSStr:              String;
    FileTypeStr:            String;
    FileSubTypeStr:         String;
    FileDateFull:           UInt64;
  end;

//------------------------------------------------------------------------------
{
  Following structures are used to hold partially parsed information from
  version information structure.
}
type
  TWFIVersionInfoStruct_String = record
    Address:    Pointer;
    Size:       TMemSize;
    Key:        WideString;
    ValueType:  Integer;
    ValueSize:  TMemSize;
    Value:      Pointer;
  end;

  TWFIVersionInfoStruct_StringTable = record
    Address:    Pointer;
    Size:       TMemSize;
    Key:        WideString;
    ValueType:  Integer;
    ValueSize:  TMemSize;
    Strings:    array of TWFIVersionInfoStruct_String;
  end;

  TWFIVersionInfoStruct_StringFileInfo = record
    Address:      Pointer;
    Size:         TMemSize;
    Key:          WideString;
    ValueType:    Integer;
    ValueSize:    TMemSize;
    StringTables: array of TWFIVersionInfoStruct_StringTable;
  end;

  TWFIVersionInfoStruct_Var = record
    Address:    Pointer;
    Size:       TMemSize;
    Key:        WideString;
    ValueType:  Integer;
    ValueSize:  TMemSize;
    Value:      Pointer;
  end;

  TWFIVersionInfoStruct_VarFileInfo = record
    Address:    Pointer;
    Size:       TMemSize;
    Key:        WideString;
    ValueType:  Integer;
    ValueSize:  TMemSize;
    Vars:       array of TWFIVersionInfoStruct_Var;
  end;

  TWFIVersionInfoStruct = record
    Address:            Pointer;
    Size:               TMemSize;
    Key:                WideString;
    ValueType:          Integer;
    ValueSize:          TMemSize;
    FixedFileInfo:      Pointer;
    FixedFileInfoSize:  TMemSize;
    StringFileInfos:    array of TWFIVersionInfoStruct_StringFileInfo;
    VarFileInfos:       array of TWFIVersionInfoStruct_VarFileInfo;
  end;

//------------------------------------------------------------------------------
{
  Following structures are used to store fully parsed information from version
  information structure.
}
type
  TWFITranslationItem = record
    LanguageName: String;
    LanguageStr:  String;
    case Integer of
      0: (Language:     UInt16;
          CodePage:     UInt16);
      1: (Translation:  UInt32);
  end;

  TWFIStringTableItem = record
    Key:    String;
    Value:  String;
  end;

  TWFIStringTable = record
    Translation:  TWFITranslationItem;
    Strings:      array of TWFIStringTableItem;
  end;

{$ELSE}//-----------------------------------------------------------------------
{
  Types used for decoded file mode - stores information about file type and
  permissions.
}
type
  TWFIFileType = (ftUnknown,ftFIFO,ftCharacterDevice,ftDirectory,ftBlockDevice,
                  ftRegularFile,ftSymbolicLink,ftSocket);

  TWFIFilePermission = (fpUserRead,fpUserWrite,fpUserExecute,
                        fpGroupRead,fpGroupWrite,fpGroupExecute,
                        fpOthersRead,fpOthersWrite,fpOthersExecute,
                        fpSetUserID,fpSetGroupID,fpSticky);

  TWFIFilePermissions = set of TWFIFilePermission;

{$ENDIF}

{===============================================================================
    TWinFileInfo - loading strategy
===============================================================================}
{
  Loading strategy determines what file information will be loaded and decoded
  or parsed and how.
  Only one operation cannot be affected by loading strategy and is always
  performed even when loading strategy indicates no operation - a check whether
  the file actually exists.
  If one strategy requires some other strategy to be active, it means this
  strategy will not produce any result if the required one is not active, it
  does NOT mean an error will occur.

  lsaKeepOpen
    the file is kept open until the TWinFileInfo object is destroyed, when not
    present the file is closed as soon as possible

  lsaLoadBasicInfo
    load size, times, attributes and other basic info

  lsaDecodeBasicInfo
    decode attributes and fills size string, requires lsaLoadBasicInfo

  lsaLoadVersionInfo (windows only)
    load version info, also loads translations and strings

  lsaParseVersionInfo (windows only)
    do low-level parsing of version info data and enumerates keys, requires
    lsaLoadVersionInfo

  lsaLoadFixedFileInfo (windows only)
    load fixed file info, requires lsaLoadVersionInfo

  lsaDecodeFixedFileInfo (windows only)
    decode fixed file info if present, has effect only if FFI is present
    (indicated by (f)VersionInfoFixedFileInfoPresent), requires
    lsaLoadFixedFileInfo

  lsaVerInfoPredefinedKeys (windows only)
    when no key is successfully enumerated (see lsaParseVersionInfo),
    a predefined set of keys is used, requires lsaParseVersionInfo

  lsaVerInfoExtractTranslations (windows only)
    extract translations from parsed version info - might get some translation
    that normal translation loading (see lsaLoadVersionInfo) missed, requires
    lsaParseVersionInfo
}
type
  TWFILoadingStrategyAction = (lsaKeepOpen,lsaLoadBasicInfo,lsaDecodeBasicInfo
  {$IFDEF Windows},lsaLoadVersionInfo,lsaParseVersionInfo,lsaLoadFixedFileInfo,
    lsaDecodeFixedFileInfo,lsaVerInfoPredefinedKeys,lsaVerInfoExtractTranslations{$ENDIF});

  TWFILoadingStrategy = set of TWFILoadingStrategyAction;

// some predefined loading strategies (no need to define type of the set)
const
  WFI_LS_LoadNone = [];

  WFI_LS_BasicInfo = [lsaLoadBasicInfo,lsaDecodeBasicInfo];

{$IFDEF Windows}

  WFI_LS_FullInfo = WFI_LS_BasicInfo + [lsaLoadVersionInfo,lsaParseVersionInfo,
                    lsaLoadFixedFileInfo,lsaDecodeFixedFileInfo];

  WFI_LS_VersionInfo = [lsaLoadVersionInfo,lsaParseVersionInfo,lsaVerInfoExtractTranslations];

  WFI_LS_VersionInfoAndFFI = WFI_LS_VersionInfo + [lsaLoadFixedFileInfo,lsaDecodeFixedFileInfo];

  WFI_LS_All = WFI_LS_FullInfo + [lsaVerInfoPredefinedKeys,lsaVerInfoExtractTranslations];

{$ELSE}

  WFI_LS_All = WFI_LS_BasicInfo;

{$ENDIF}

{===============================================================================
    TWinFileInfo - class declaration
===============================================================================}
type
  TWinFileInfo = class(TObject)
  protected
    // internals
    fLoadingStrategy:         TWFILoadingStrategy;
    fFormatSettings:          TFormatSettings;
    // basic initial file info
    fName:                    String;
    fLongName:                String;
  {$IFDEF Windows}
    fShortName:               String;
  {$ENDIF}
    fExists:                  Boolean;
    fFileHandle:              TWFIFileHandle;
    // basic loaded file info
    fSize:                    UInt64;
    fSizeStr:                 String;
  {$IFDEF Windows}
    fCreationTimeRaw:         TDateTime;  // time how it is actually stored for the file
    fLastAccessTimeRaw:       TDateTime;
    fLastWriteTimeRaw:        TDateTime;
    fCreationTime:            TDateTime;  // stored time converted to local time
    fLastAccessTime:          TDateTime;
    fLastWriteTime:           TDateTime;
    fNumberOfLinks:           UInt32;
  {
    combination of volume serial and file id can be used to determine file
    path equality (also for directories, but WFI does not support dirs)
  }
    fVolumeSerialNumber:      UInt32;
    fFileID:                  UInt64;
    // file attributes (part of basic info)
    fAttributesFlags:         DWORD;
    fAttributesStr:           String;
    fAttributesText:          String;
    fAttributesDecoded:       TWFIFileAttributesDecoded;
  {$ELSE}
    fLastAccessTimeRaw:       TDateTime;
    fLastModificationTimeRaw: TDateTime;
    fLastStatusChangeTimeRaw: TDateTime;
    fLastAccessTime:          TDateTime;
    fLastModificationTime:    TDateTime;
    fLastStatusChangeTime:    TDateTime;
    fNumberOfHardLinks:       PtrUInt;
    // device ID and inode is used to determine file path equality
    fDeviceID:                UInt64;
    fiNodeNumber:             UInt64;
    fBlockSize:               PtrUInt;
    fBlocks:                  UInt64;
    fOwnerUserID:             UInt32;
    fOwnerGroupID:            UInt32;
    fMode:                    UInt32;
    // decoded mode
    fFileType:                TWFIFileType;
    fFileTypeStr:             String;
    fPermissions:             TWFIFilePermissions;
    fPermissionsStr:          String;
  {$ENDIF}
  {$IFDEF Windows}
    // version info unparsed data
    fVerInfoSize:             TMemSize;
    fVerInfoData:             Pointer;
    // version info data
    fVersionInfoPresent:      Boolean;
    // version info - fixed file info
    fVersionInfoFFIPresent:   Boolean;
    fVersionInfoFFI:          TVSFixedFileInfo;
    fVersionInfoFFIDecoded:   TWFIFixedFileInfoDecoded;
    // version info partially parsed data
    fVersionInfoStruct:       TWFIVersionInfoStruct;
    // version info fully parsed data
    fVersionInfoParsed:       Boolean;
    fVersionInfoStringTables: array of TWFIStringTable;
    // getters for fVersionInfoStruct fields
    Function GetVersionInfoStringTableCount: Integer; virtual;
    Function GetVersionInfoStringTable(Index: Integer): TWFIStringTable; virtual;
    Function GetVersionInfoTranslationCount: Integer; virtual;
    Function GetVersionInfoTranslation(Index: Integer): TWFITranslationItem; virtual;
    Function GetVersionInfoStringCount(Table: Integer): Integer; virtual;
    Function GetVersionInfoString(Table,Index: Integer): TWFIStringTableItem; virtual;
    Function GetVersionInfoValue(const Language,Key: String): String; virtual;
    // version info loading methods
    procedure VersionInfo_LoadTranslations; virtual;
    procedure VersionInfo_LoadStrings; virtual;
    // version info parsing methods
    procedure VersionInfo_Parse; virtual;
    procedure VersionInfo_ExtractTranslations; virtual;
    procedure VersionInfo_EnumerateKeys; virtual;
  {$ENDIF}
    // loading and decoding methods
    procedure LoadBasicInfo; virtual;
    procedure DecodeBasicInfo; virtual;
  {$IFDEF Windows}
    procedure LoadVersionInfo; virtual;
    procedure LoadFixedFileInfo; virtual;
    procedure DecodeFixedFileInfo; virtual;
  {$ENDIF}
    // other protected methods
    procedure Clear; virtual;    
    procedure Initialize(const FileName: String); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(LoadingStrategy: TWFILoadingStrategy = WFI_LS_All); overload;
    constructor Create(const FileName: String; LoadingStrategy: TWFILoadingStrategy = WFI_LS_All); overload;
    destructor Destroy; override;
    procedure Refresh; virtual;
    procedure CreateReport(Strings: TStrings); overload; virtual;
    Function CreateReport: String; overload; virtual;
  {$IFDEF Windows}
    Function IndexOfVersionInfoStringTable(Translation: DWORD): Integer; virtual;
    Function IndexOfVersionInfoString(Table: Integer; const Key: String): Integer; virtual;
  {$ENDIF}
    // internals
    property LoadingStrategy: TWFILoadingStrategy read fLoadingStrategy write fLoadingStrategy;
    property FormatSettings: TFormatSettings read fFormatSettings write fFormatSettings;
    // basic initial file info
    property Name: String read fName;
    property LongName: String read fLongName;
  {$IFDEF Windows}
    property ShortName: String read fShortName;
  {$ENDIF}
    property Exists: Boolean read fExists;
    property FileHandle: TWFIFileHandle read fFileHandle;
    // basic loaded file info
    property Size: UInt64 read fSize;
    property SizeStr: String read fSizeStr;
  {$IFDEF Windows}
    property CreationTimeRaw: TDateTime read fCreationTime;
    property LastAccessTimeRaw: TDateTime read fLastAccessTime;
    property LastWriteTimeRaw: TDateTime read fLastWriteTime;
    property CreationTime: TDateTime read fCreationTime;
    property LastAccessTime: TDateTime read fLastAccessTime;
    property LastWriteTime: TDateTime read fLastWriteTime;
    property NumberOfLinks: UInt32 read fNumberOfLinks;
    property VolumeSerialNumber: UInt32 read fVolumeSerialNumber;
    property FileID: UInt64 read fFileID;
    // file attributes (part of basic info)
    property AttributesFlags: DWORD read fAttributesFlags;
    property AttributesStr: String read fAttributesStr;
    property AttributesText: String read fAttributesText;
    property AttributesDecoded: TWFIFileAttributesDecoded read fAttributesDecoded;
  {$ELSE}
    property LastAccessTime: TDateTime read fLastAccessTime;
    property LastModificationTime: TDateTime read fLastModificationTime;
    property LastStatusChangeTime: TDateTime read fLastStatusChangeTime;
    property NumberOfHardLinks: PtrUInt read fNumberOfHardLinks;
    property DeviceID: UInt64 read fDeviceID;
    property iNodeNumber: UInt64 read fiNodeNumber;
    property BlockSize: PtrUInt read fBlockSize;
    property Blocks: UInt64 read fBlocks;
    property OwnerUserID: UInt32 read fOwnerUserID;
    property OwnerGroupID: UInt32 read fOwnerGroupID;
    property Mode: UInt32 read fMode;
    // decoded mode
    property FileType: TWFIFileType read fFileType;
    property FileTypeStr: String read fFileTypeStr;
    property Permissions: TWFIFilePermissions read fPermissions;
    property PermissionsStr: String read fPermissionsStr;
  {$ENDIF}
  {$IFDEF Windows}
    // version info unparsed data
    property VerInfoSize: PtrUInt read fVerInfoSize;
    property VerInfoData: Pointer read fVerInfoData;
    // version info data
    property VersionInfoPresent: Boolean read fVersionInfoPresent;
    // version info - fixed file info 
    property VersionInfoFixedFileInfoPresent: Boolean read fVersionInfoFFIPresent;
    property VersionInfoFixedFileInfo: TVSFixedFileInfo read fVersionInfoFFI;
    property VersionInfoFixedFileInfoDecoded: TWFIFixedFileInfoDecoded read fVersionInfoFFIDecoded;
    // version info partially parsed data
    property VersionInfoStruct: TWFIVersionInfoStruct read fVersionInfoStruct;
    // version info fully parsed data
    property VersionInfoParsed: Boolean read fVersionInfoParsed;
    property VersionInfoStringTableCount: Integer read GetVersionInfoStringTableCount;
    property VersionInfoStringTables[Index: Integer]: TWFIStringTable read GetVersionInfoStringTable;
    property VersionInfoTranslationCount: Integer read GetVersionInfoTranslationCount;
    property VersionInfoTranslations[Index: Integer]: TWFITranslationItem read GetVersionInfoTranslation;
    property VersionInfoStringCount[Table: Integer]: Integer read GetVersionInfoStringCount;
    property VersionInfoStrings[Table,Index: Integer]: TWFIStringTableItem read GetVersionInfoString;
    property VersionInfoValues[const Language,Key: String]: String read GetVersionInfoValue; default;
  {$ENDIF}
  end;

implementation

uses
{$IFDEF Windows}
  {$IFDEF FPC} jwaPSApi{$ELSE} PSApi{$ENDIF}, StrRect
{$ELSE}
  DateUtils, BaseUnix, DL
{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}}   // Conversion between ordinals and pointers is not portable
  {$DEFINE W5057:={$WARN 5057 OFF}}   // Local variable "$1" does not seem to be initialized
  {$PUSH}{$WARN 2005 OFF}             // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
    {$DEFINE W5058:=}
    {$DEFINE W5092:={$WARN 5092 OFF}}   // Variable "$1" of a managed type does not seem to be initialized
  {$ELSE}
    {$DEFINE W5058:={$WARN 5058 OFF}}   // Variable "$1" does not seem to be initialized
    {$DEFINE W5092:=}
  {$IFEND}
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30200)}
    {$DEFINE W6058:={$WARN 6058 OFF}} // Call to subroutine "$1" marked as inline is not inlined
  {$ELSE}
    {$DEFINE W6058:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Utility functions - internal functions
-------------------------------------------------------------------------------}

procedure ProcessFileSize(FileSize: UInt64; out Number: Double; out Decimals: Integer; out UnitPrefix: String);
{
  This routine takes size of a file in bytes and produces floating-point number
  that can be used in textual representation of this size. Depending on a
  "magnitude" of the size, a unit prefix is selected and number of decimal
  digits to show is calculated. Also, the number is properly divided for the
  selected unit prefix.
}
const
  BinaryPrefix: array[0..8] of String = ('','Ki','Mi','Gi','Ti','Pi','Ei','Zi','Yi');
  PrefixShift = 10;
var
  Magnitude:  Integer;
begin
Magnitude := -1;
repeat
  Inc(Magnitude);
until ((FileSize shr (PrefixShift * Succ(Magnitude))) = 0) or (Magnitude >= 8);
case FileSize shr (PrefixShift * Magnitude) of
   1..9:  Decimals := 2;
  10..99: Decimals := 1;
else
  Decimals := 0;
end;
Number := (FileSize shr (PrefixShift * Magnitude));
If Magnitude > 0 then
  Number := Number + (((FileSize shr (PrefixShift * Pred(Magnitude))) and 1023) / 1024)
else
  Decimals := 0;
UnitPrefix := BinaryPrefix[Magnitude];
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}{$IFDEF FPCDWM}{$PUSH}W5058 W5092{$ENDIF}{$ENDIF}
procedure InitFormatSettings(out FormatSettings: TFormatSettings);
begin
{$WARN SYMBOL_PLATFORM OFF}
{$IF not Defined(FPC) and (CompilerVersion >= 18)} // Delphi 2006+
FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
{$ELSE}
{$IFDEF Windows}
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,FormatSettings);
{$ELSE}
// non-windows
FormatSettings := DefaultFormatSettings;
{$ENDIF}
{$IFEND}
{$WARN SYMBOL_PLATFORM ON}
end;
{$IFDEF Windows}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

{$IFDEF Windows}
//------------------------------------------------------------------------------

{$IF not Declared(CP_THREAD_ACP)}
const
  CP_THREAD_ACP = 3;
{$IFEND}

Function WideToString(const WStr: WideString; AnsiCodePage: UINT = CP_THREAD_ACP): String;
begin
{$IFDEF Unicode}
// unicode Delphi or FPC (String = UnicodeString)
Result := WStr;
{$ELSE}
// non-unicode...
If not UTF8AnsiDefaultStrings then
  begin
    // CP ansi strings - bare FPC or Delphi
    Result := '';
    SetLength(Result,WideCharToMultiByte(AnsiCodePage,0,PWideChar(WStr),Length(WStr),nil,0,nil,nil));
    WideCharToMultiByte(AnsiCodePage,0,PWideChar(WStr),Length(WStr),PAnsiChar(Result),Length(Result) * SizeOf(AnsiChar),nil,nil);
    // a wrong codepage might be stored, try translation with default cp
    If (AnsiCodePage <> CP_THREAD_ACP) and (Length(Result) <= 0) and (Length(WStr) > 0) then
      Result := WideToString(WStr);
  end
// UTF8 ansi strings
else Result := StringToUTF8(WStr);
{$ENDIF}
end;

{$ENDIF}

{-------------------------------------------------------------------------------
    Utility functions - public functions
-------------------------------------------------------------------------------}

Function GetFileSize(const FileName: String): UInt64;
begin
Result := 0;
with TWinFileInfo.Create(FileName,[lsaLoadBasicInfo]) do
try
  If Exists then
    Result := Size
  else
    raise EWFIFileError.CreateFmt('SameFile: File "%s" does not exist.',[FileName]);
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSizeToStr(FileSize: UInt64; FormatSettings: TFormatSettings; SpaceUnit: Boolean = True): String;
var
  Number:     Double;
  Decimals:   Integer;
  UnitPrefix: String;
begin
ProcessFileSize(FileSize,Number,Decimals,UnitPrefix);
If SpaceUnit then
  Result := Format('%.*f %sB',[Decimals,Number,UnitPrefix],FormatSettings)
else
  Result := Format('%.*f%sB',[Decimals,Number,UnitPrefix],FormatSettings);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FileSizeToStr(FileSize: UInt64; SpaceUnit: Boolean = True): String;
var
  Number:     Double;
  Decimals:   Integer;
  UnitPrefix: String;
begin
ProcessFileSize(FileSize,Number,Decimals,UnitPrefix);
If SpaceUnit then
  Result := Format('%.*f %sB',[Decimals,Number,UnitPrefix])
else
  Result := Format('%.*f%sB',[Decimals,Number,UnitPrefix]);
end;

//------------------------------------------------------------------------------

Function FileSizeToStrThr(FileSize: UInt64; SpaceUnit: Boolean = True): String;
var
  FormatSettings: TFormatSettings;
begin
InitFormatSettings(FormatSettings);
Result := FileSizeToStr(FileSize,FormatSettings,SpaceUnit);
end;

//------------------------------------------------------------------------------

Function SameFile(const A,B: String): Boolean;
var
  AInfo,BInfo:  TWinFileInfo;
begin
Result := False;
AInfo := TWinFileInfo.Create(A,[lsaLoadBasicInfo,lsaKeepOpen]);
try
  If AInfo.Exists then
    begin
      BInfo := TWinFileInfo.Create(B,[lsaLoadBasicInfo,lsaKeepOpen]);
      try
        If BInfo.Exists then
        {$IFDEF Windows}
          Result := (AInfo.VolumeSerialNumber = BInfo.VolumeSerialNumber) and (AInfo.FileID = BInfo.FileID)
        {$ELSE}
          Result := (AInfo.DeviceID = BInfo.DeviceID) and (AInfo.iNodeNumber = BInfo.iNodeNumber)
        {$ENDIF}
        else
          raise EWFIFileError.CreateFmt('SameFile: File "%s" does not exist.',[B]);
      finally
        BInfo.Free;
      end;
    end
  else raise EWFIFileError.CreateFmt('SameFile: File "%s" does not exist.',[A]);
finally
  AInfo.Free;
end;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TWinFileInfo
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinFileInfo - system constants
===============================================================================}
{$IFNDEF Windows}
const
  O_NOATIME = $40000;   // do not set atime
  O_PATH	  = $200000;  // resolve pathname but do not open file

  S_ISUID = $800; // set-user-ID bit
  S_ISGID = $400; // set-group-ID bit
  S_ISVTX = $200; // sticky bit
{$ENDIF}

{===============================================================================
    TWinFileInfo - conversion tables
===============================================================================}
{$IFDEF Windows}
// structures used in conversion tables as items
type
  TWFIAttributeString = record
    Flag: DWORD;
    Text: String;
    Str:  String;
  end;

  TWFIFlagText = record
    Flag: DWORD;
    Text: String;
  end;

//------------------------------------------------------------------------------
{
  tables used to convert some binary information (mainly flags) to a textual
  representation
}
const
  WFI_FILE_ATTR_STRS: array[0..16] of TWFIAttributeString = (
    (Flag: FILE_ATTRIBUTE_ARCHIVE;             Text: 'Archive';             Str: 'A'),
    (Flag: FILE_ATTRIBUTE_COMPRESSED;          Text: 'Compressed';          Str: 'C'),
    (Flag: FILE_ATTRIBUTE_DEVICE;              Text: 'Device';              Str: ''),
    (Flag: FILE_ATTRIBUTE_DIRECTORY;           Text: 'Directory';           Str: 'D'),
    (Flag: FILE_ATTRIBUTE_ENCRYPTED;           Text: 'Encrypted';           Str: 'E'),
    (Flag: FILE_ATTRIBUTE_HIDDEN;              Text: 'Hidden';              Str: 'H'),
    (Flag: FILE_ATTRIBUTE_INTEGRITY_STREAM;    Text: 'Integrity stream';    Str: ''),
    (Flag: FILE_ATTRIBUTE_NORMAL;              Text: 'Normal';              Str: 'N'),
    (Flag: FILE_ATTRIBUTE_NOT_CONTENT_INDEXED; Text: 'Not content indexed'; Str: 'I'),
    (Flag: FILE_ATTRIBUTE_NO_SCRUB_DATA;       Text: 'No scrub data';       Str: ''),
    (Flag: FILE_ATTRIBUTE_OFFLINE;             Text: 'Offline';             Str: 'O'),
    (Flag: FILE_ATTRIBUTE_READONLY;            Text: 'Read only';           Str: 'R'),
    (Flag: FILE_ATTRIBUTE_REPARSE_POINT;       Text: 'Reparse point';       Str: 'L'),
    (Flag: FILE_ATTRIBUTE_SPARSE_FILE;         Text: 'Sparse file';         Str: 'P'),
    (Flag: FILE_ATTRIBUTE_SYSTEM;              Text: 'System';              Str: 'S'),
    (Flag: FILE_ATTRIBUTE_TEMPORARY;           Text: 'Temporary';           Str: 'T'),
    (Flag: FILE_ATTRIBUTE_VIRTUAL;             Text: 'Virtual';             Str: ''));

//------------------------------------------------------------------------------

  WFI_FFI_FILE_OS_STRS: array[0..13] of TWFIFlagText = (
    (Flag: VOS_DOS;           Text: 'MS-DOS'),
    (Flag: VOS_NT;            Text: 'Windows NT'),
    (Flag: VOS__WINDOWS16;    Text: '16-bit Windows'),
    (Flag: VOS__WINDOWS32;    Text: '32-bit Windows'),
    (Flag: VOS_OS216;         Text: '16-bit OS/2'),
    (Flag: VOS_OS232;         Text: '32-bit OS/2'),
    (Flag: VOS__PM16;         Text: '16-bit Presentation Manager'),
    (Flag: VOS__PM32;         Text: '32-bit Presentation Manager'),
    (Flag: VOS_UNKNOWN;       Text: 'Unknown'),
    (Flag: VOS_DOS_WINDOWS16; Text: '16-bit Windows running on MS-DOS'),
    (Flag: VOS_DOS_WINDOWS32; Text: '32-bit Windows running on MS-DOS'),
    (Flag: VOS_NT_WINDOWS32;  Text: 'Windows NT'),
    (Flag: VOS_OS216_PM16;    Text: '16-bit Presentation Manager running on 16-bit OS/2'),
    (Flag: VOS_OS232_PM32;    Text: '32-bit Presentation Manager running on 32-bit OS/2'));

//------------------------------------------------------------------------------

  WFI_FFI_FILE_TYPE_STRS: array[0..6] of TWFIFlagText = (
    (Flag: VFT_APP;        Text: 'Application'),
    (Flag: VFT_DLL;        Text: 'DLL'),
    (Flag: VFT_DRV;        Text: 'Device driver'),
    (Flag: VFT_FONT;       Text: 'Font'),
    (Flag: VFT_STATIC_LIB; Text: 'Static-link library'),
    (Flag: VFT_UNKNOWN;    Text: 'Unknown'),
    (Flag: VFT_VXD;        Text: 'Virtual device'));

//------------------------------------------------------------------------------

  WFI_FFI_FILE_SUBTYPE_DRV_STRS: array[0..11] of TWFIFlagText = (
    (Flag: VFT2_DRV_COMM;              Text: 'Communications driver'),
    (Flag: VFT2_DRV_DISPLAY;           Text: 'Display driver'),
    (Flag: VFT2_DRV_INSTALLABLE;       Text: 'Installable driver'),
    (Flag: VFT2_DRV_KEYBOARD;          Text: 'Keyboard driver'),
    (Flag: VFT2_DRV_LANGUAGE;          Text: 'Language driver'),
    (Flag: VFT2_DRV_MOUSE;             Text: 'Mouse driver'),
    (Flag: VFT2_DRV_NETWORK;           Text: 'Network driver'),
    (Flag: VFT2_DRV_PRINTER;           Text: 'Printer driver'),
    (Flag: VFT2_DRV_SOUND;             Text: 'Sound driver'),
    (Flag: VFT2_DRV_SYSTEM;            Text: 'System driver'),
    (Flag: VFT2_DRV_VERSIONED_PRINTER; Text: 'Versioned printer driver'),
    (Flag: VFT2_UNKNOWN;               Text: 'Unknown'));

//------------------------------------------------------------------------------

  WFI_FFI_FILE_SUBTYPE_FONT_STRS: array[0..3] of TWFIFlagText = (
    (Flag: VFT2_FONT_RASTER;   Text: 'Raster font'),
    (Flag: VFT2_FONT_TRUETYPE; Text: 'TrueType font'),
    (Flag: VFT2_FONT_VECTOR;   Text: 'Vector font'),
    (Flag: VFT2_UNKNOWN;       Text: 'Unknown'));

//------------------------------------------------------------------------------

  WFI_VERINFO_PREDEF_KEYS: array[0..11] of String = (
    'Comments','CompanyName','FileDescription','FileVersion','InternalName',
    'LegalCopyright','LegalTrademarks','OriginalFilename','ProductName',
    'ProductVersion','PrivateBuild','SpecialBuild');

{$ELSE}//-----------------------------------------------------------------------
const
  WFI_FILE_TYPE_STRS: array[TWFIFileType] of String = (
    'unknown','named pipe','character device','directory','block device',
    'regular file','symbolic link','socket');

//------------------------------------------------------------------------------

  WFI_PERMISSION_FLAGS: array[TWFIFilePermission] of UInt32 = (
    S_IRUSR,S_IWUSR,S_IXUSR,  // user
    S_IRGRP,S_IWGRP,S_IXGRP,  // group
    S_IROTH,S_IWOTH,S_IXOTH,  // others
    S_ISUID,S_ISGID,S_ISVTX);

{$ENDIF}

{===============================================================================
    TWinFileInfo - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinFileInfo - protected methods
-------------------------------------------------------------------------------}

{$IFDEF Windows}

Function TWinFileInfo.GetVersionInfoStringTableCount: Integer;
begin
Result := Length(fVersionInfoStringTables);
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.GetVersionInfoStringTable(Index: Integer): TWFIStringTable;
begin
If (Index >= Low(fVersionInfoStringTables)) and (Index <= High(fVersionInfoStringTables)) then
  Result := fVersionInfoStringTables[Index]
else
  raise EWFIIndexOutOfBounds.CreateFmt('TWinFileInfo.GetVersionInfoStringTable: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.GetVersionInfoTranslationCount: Integer;
begin
Result := Length(fVersionInfoStringTables);
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.GetVersionInfoTranslation(Index: Integer): TWFITranslationItem;
begin
If (Index >= Low(fVersionInfoStringTables)) and (Index <= High(fVersionInfoStringTables)) then
  Result := fVersionInfoStringTables[Index].Translation
else
  raise EWFIIndexOutOfBounds.CreateFmt('TWinFileInfo.GetVersionInfoTranslation: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.GetVersionInfoStringCount(Table: Integer): Integer;
begin
Result := Length(GetVersionInfoStringTable(Table).Strings);
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.GetVersionInfoString(Table,Index: Integer): TWFIStringTableItem;
begin
with GetVersionInfoStringTable(Table) do
  begin
    If (Index >= Low(Strings)) and (Index <= High(Strings)) then
      Result := Strings[Index]
    else
      raise EWFIIndexOutOfBounds.CreateFmt('TWinFileInfo.GetVersionInfoString: Index (%d) out of bounds.',[Index]);
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function TWinFileInfo.GetVersionInfoValue(const Language,Key: String): String;
var
  StrPtr:   Pointer;
  StrSize:  UInt32;
begin
Result := '';
If fVersionInfoPresent and (Language <> '') and (Key <> '') then
  If VerQueryValue(fVerInfoData,PChar(Format('\StringFileInfo\%s\%s',[Language,Key])),StrPtr,StrSize) then
    Result := WinToStr(PChar(StrPtr));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TWinFileInfo.VersionInfo_LoadTranslations;
var
  TrsPtr:   Pointer;
  TrsSize:  UInt32;
  i:        Integer;
begin
If VerQueryValue(fVerInfoData,'\VarFileInfo\Translation',TrsPtr,TrsSize) then
  begin
    SetLength(fVersionInfoStringTables,TrsSize div SizeOf(UInt32));
    For i := Low(fVersionInfoStringTables) to High(fVersionInfoStringTables) do
      with fVersionInfoStringTables[i].Translation do
        begin
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          Translation := PUInt32(PtrUInt(TrsPtr) + (PtrUInt(i) * SizeOf(UInt32)))^;
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
          SetLength(LanguageName,256);  // should be sufficiently long enough, hopefully
          SetLength(LanguageName,VerLanguageName(Translation,PChar(LanguageName),Length(LanguageName)));
          LanguageName := WinToStr(LanguageName);
          LanguageStr := AnsiUpperCase(IntToHex(Language,4) + IntToHex(CodePage,4));
        end;
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TWinFileInfo.VersionInfo_LoadStrings;
var
  Table:    Integer;
  i,j:      Integer;
  StrPtr:   Pointer;
  StrSize:  UInt32;
begin
// loads value of string according to the key
For Table := Low(fVersionInfoStringTables) to High(fVersionInfoStringTables) do
  with fVersionInfoStringTables[Table] do
    For i := High(Strings) downto Low(Strings) do
      If not VerQueryValue(fVerInfoData,PChar(Format('\StringFileInfo\%s\%s',[Translation.LanguageStr,StrToWin(Strings[i].Key)])),StrPtr,StrSize) then
        begin
          // remove this one string as its value could not be obtained
          For j := i to Pred(High(Strings)) do
            Strings[j] := Strings[j + 1];
          SetLength(Strings,Length(Strings) - 1);
        end
      else Strings[i].Value := WinToStr(PChar(StrPtr))
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TWinFileInfo.VersionInfo_Parse;
type
  PVIS_Base = ^TVIS_Base;
  TVIS_Base = record
    Address:    Pointer;
    Size:       TMemSize;
    Key:        WideString;
    ValueType:  Integer;
    ValueSize:  TMemSize;
  end;
var
  CurrentAddress: Pointer;
  TempBlock:      TVIS_Base;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function Align32bit(Ptr: Pointer): Pointer;
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If ((PtrUInt(Ptr) and 3) <> 0) then
      Result := Pointer((PtrUInt(Ptr) and not PtrUInt(3)) + 4)
    else
      Result := Ptr;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure ParseBlock(var Ptr: Pointer; BlockBase: Pointer);
  begin
    PVIS_Base(BlockBase)^.Address := Ptr;
    PVIS_Base(BlockBase)^.Size := PUInt16(PVIS_Base(BlockBase)^.Address)^;
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    PVIS_Base(BlockBase)^.Key := PWideChar(PtrUInt(PVIS_Base(BlockBase)^.Address) + (3 * SizeOf(UInt16)));
    PVIS_Base(BlockBase)^.ValueType := PUInt16(PtrUInt(PVIS_Base(BlockBase)^.Address) + (2 * SizeOf(UInt16)))^;
    PVIS_Base(BlockBase)^.ValueSize := PUInt16(PtrUInt(PVIS_Base(BlockBase)^.Address) + SizeOf(UInt16))^;
    Ptr := Align32bit(Pointer(PtrUInt(PVIS_Base(BlockBase)^.Address) + (3 * SizeOf(UInt16)) +
             PtrUInt((Length(PVIS_Base(BlockBase)^.Key) + 1{terminating zero}) * SizeOf(WideChar))));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CheckPointer(var Ptr: Pointer; BlockBase: Pointer): Boolean;
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    Result := (PtrUInt(Ptr) >= PtrUInt(PVIS_Base(BlockBase)^.Address)) and
              (PtrUInt(Ptr) < (PtrUInt(PVIS_Base(BlockBase)^.Address) + PVIS_Base(BlockBase)^.Size));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

begin
If (fVerInfoSize >= 6) and (fVerInfoSize >= PUInt16(fVerInfoData)^) then
  try
    CurrentAddress := fVerInfoData;
    ParseBlock(CurrentAddress,@fVersionInfoStruct);
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    fVersionInfoStruct.FixedFileInfoSize := PUInt16(PtrUInt(fVersionInfoStruct.Address) + SizeOf(UInt16))^;
    fVersionInfoStruct.FixedFileInfo := CurrentAddress;
    CurrentAddress := Align32bit(Pointer(PtrUInt(CurrentAddress) + fVersionInfoStruct.FixedFileInfoSize));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    // traverse remaining memory as long as the the current pointer points to a valid memory area
    while CheckPointer(CurrentAddress,@fVersionInfoStruct) do
      begin
        ParseBlock(CurrentAddress,@TempBlock);
        If WideSameText(TempBlock.Key,WideString('StringFileInfo')) then
          begin
            // strings
            SetLength(fVersionInfoStruct.StringFileInfos,Length(fVersionInfoStruct.StringFileInfos) + 1);
            with fVersionInfoStruct.StringFileInfos[High(fVersionInfoStruct.StringFileInfos)] do
              begin
                Address := TempBlock.Address;
                Size := TempBlock.Size;
                Key := TempBlock.Key;
                ValueType := TempBlock.ValueType;
                ValueSize := TempBlock.ValueSize;
                // parse-out string tables
                while CheckPointer(CurrentAddress,@fVersionInfoStruct.StringFileInfos[High(fVersionInfoStruct.StringFileInfos)]) do
                  begin
                    SetLength(StringTables,Length(StringTables) + 1);
                    ParseBlock(CurrentAddress,@StringTables[High(StringTables)]);
                    // parse-out individual strings
                    while CheckPointer(CurrentAddress,@StringTables[High(StringTables)]) do
                      with StringTables[High(StringTables)] do
                        begin
                          SetLength(Strings,Length(Strings) + 1);
                          ParseBlock(CurrentAddress,@Strings[High(Strings)]);
                          Strings[High(Strings)].Value := CurrentAddress;
                        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                          CurrentAddress := Align32bit(Pointer(PtrUInt(Strings[High(Strings)].Address) + Strings[High(Strings)].Size));
                        {$IFDEF FPCDWM}{$POP}{$ENDIF}
                        end;
                  end;
              end
          end
        else If WideSameText(TempBlock.Key,WideString('VarFileInfo')) then
          begin
            // variables
            SetLength(fVersionInfoStruct.VarFileInfos,Length(fVersionInfoStruct.VarFileInfos) + 1);
            with fVersionInfoStruct.VarFileInfos[High(fVersionInfoStruct.VarFileInfos)] do
              begin
                Address := TempBlock.Address;
                Size := TempBlock.Size;
                Key := TempBlock.Key;
                ValueType := TempBlock.ValueType;
                ValueSize := TempBlock.ValueSize;
                // parse-out variables
                while CheckPointer(CurrentAddress,@fVersionInfoStruct.VarFileInfos[High(fVersionInfoStruct.VarFileInfos)]) do
                  begin
                    SetLength(Vars,Length(Vars) + 1);
                    ParseBlock(CurrentAddress,@Vars[High(Vars)]);
                    Vars[High(Vars)].Value := CurrentAddress;
                  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                    CurrentAddress := Align32bit(Pointer(PtrUInt(Vars[High(Vars)].Address) + Vars[High(Vars)].Size));
                  {$IFDEF FPCDWM}{$POP}{$ENDIF}
                  end;
              end;
          end
        else raise EWFIProcessingError.CreateFmt('TWinFileInfo.VersionInfo_Parse: Unknown block (%s).',[TempBlock.Key]);
      end;
    fVersionInfoParsed := True;
  except
    fVersionInfoParsed := False;
    fVersionInfoStruct.Key := '';
    SetLength(fVersionInfoStruct.StringFileInfos,0);
    SetLength(fVersionInfoStruct.VarFileInfos,0);
    FillChar(fVersionInfoStruct,SizeOf(fVersionInfoStruct),0);
  end
else fVersionInfoParsed := False;
end;

//------------------------------------------------------------------------------

procedure TWinFileInfo.VersionInfo_ExtractTranslations;
var
  i,Table:  Integer;

  Function TranslationIsListed(const LanguageStr: String): Boolean;
  var
    ii: Integer;
  begin
    Result := False;
    For ii := Low(fVersionInfoStringTables) to High(fVersionInfoStringTables) do
      If AnsiSameText(fVersionInfoStringTables[ii].Translation.LanguageStr,LanguageStr) then
        begin
          Result := True;
          Break;
        end;
  end;

begin
For i := Low(fVersionInfoStruct.StringFileInfos) to High(fVersionInfoStruct.StringFileInfos) do
  If WideSameText(fVersionInfoStruct.StringFileInfos[i].Key,WideString('StringFileInfo')) then
    For Table := Low(fVersionInfoStruct.StringFileInfos[i].StringTables) to High(fVersionInfoStruct.StringFileInfos[i].StringTables) do
      If not TranslationIsListed(WideToStr(fVersionInfoStruct.StringFileInfos[i].StringTables[Table].Key)) then
        begin
          SetLength(fVersionInfoStringTables,Length(fVersionInfoStringTables) + 1);
          with fVersionInfoStringTables[High(fVersionInfoStringTables)].Translation do
            begin
              LanguageStr := WideToStr(fVersionInfoStruct.StringFileInfos[i].StringTables[Table].Key);
              Language := StrToIntDef('$' + Copy(LanguageStr,1,4),0);
              CodePage := StrToIntDef('$' + Copy(LanguageStr,5,4),0);
              SetLength(LanguageName,256);
              SetLength(LanguageName,VerLanguageName(Translation,PChar(LanguageName),Length(LanguageName)));
              LanguageName := WinToStr(LanguageName);
            end;
        end;
end;

//------------------------------------------------------------------------------

procedure TWinFileInfo.VersionInfo_EnumerateKeys;
var
  Table:  Integer;
  i,j,k:  Integer;
begin
For i := Low(fVersionInfoStruct.StringFileInfos) to High(fVersionInfoStruct.StringFileInfos) do
  If WideSameText(fVersionInfoStruct.StringFileInfos[i].Key,WideString('StringFileInfo')) then
    For Table := Low(fVersionInfoStringTables) to High(fVersionInfoStringTables) do
      with fVersionInfoStruct.StringFileInfos[i] do
        begin
          For j := Low(StringTables) to High(StringTables) do
            If WideSameText(StringTables[j].Key,StrToWide(fVersionInfoStringTables[Table].Translation.LanguageStr)) then
              begin
                SetLength(fVersionInfoStringTables[Table].Strings,Length(StringTables[j].Strings));
                For k := Low(StringTables[j].Strings) to High(StringTables[j].Strings) do
                  fVersionInfoStringTables[Table].Strings[k].Key := WideToString(StringTables[j].Strings[k].Key,fVersionInfoStringTables[Table].Translation.CodePage);
              end;
          If (Length(fVersionInfoStringTables[Table].Strings) <= 0) and (lsaVerInfoPredefinedKeys in fLoadingStrategy) then
            begin
              SetLength(fVersionInfoStringTables[Table].Strings,Length(WFI_VERINFO_PREDEF_KEYS));
              For j := Low(WFI_VERINFO_PREDEF_KEYS) to High(WFI_VERINFO_PREDEF_KEYS) do
                fVersionInfoStringTables[Table].Strings[j].Key := WFI_VERINFO_PREDEF_KEYS[j];
            end;
        end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF Windows}{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}{$ENDIF}
procedure TWinFileInfo.LoadBasicInfo;
{$IFDEF Windows}

  Function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  var
    SystemTime: TSystemTime;
  begin
    If FileTimeToSystemTime(FileTime,SystemTime) then
      Result := SystemTimeToDateTime(SystemTime)
    else raise EWFISystemError.CreateFmt('TWinFileInfo.LoadBasicInfo.FileTimeToDateTime: ' +
                                         'Failed to convert file time to system time (%d).',[GetLastError]);
  end;

  Function FileTimeToLocalDateTime(FileTime: TFileTime): TDateTime;
  var
    LocalTime:  TFileTime;
    SystemTime: TSystemTime;
  begin
    If FileTimeToLocalFileTime(FileTime,LocalTime) then
      begin
        If FileTimeToSystemTime(LocalTime,SystemTime) then
          Result := SystemTimeToDateTime(SystemTime)
        else raise EWFISystemError.CreateFmt('TWinFileInfo.LoadBasicInfo.FileTimeToLocalDateTime: ' +
                                             'Failed to convert file time to system time (%d).',[GetLastError]);
      end
    else raise EWFISystemError.CreateFmt('TWinFileInfo.LoadBasicInfo.FileTimeToLocalDateTime: ' +
                                         'Failed to convert to local time (%d).',[GetLastError]);
  end;

var
  Info: TByHandleFileInformation;
begin
FillChar(Info,SizeOf(TByHandleFileInformation),0);
If GetFileInformationByHandle(fFileHandle,Info) then
  begin
    fSize := (UInt64(Info.nFileSizeHigh) shl 32) or UInt64(Info.nFileSizeLow);
    fCreationTimeRaw := FileTimeToDateTime(Info.ftCreationTime);
    fLastAccessTimeRaw := FileTimeToDateTime(Info.ftLastAccessTime);
    fLastWriteTimeRaw := FileTimeToDateTime(Info.ftLastWriteTime);
    fCreationTime := FileTimeToLocalDateTime(Info.ftCreationTime);
    fLastAccessTime := FileTimeToLocalDateTime(Info.ftLastAccessTime);
    fLastWriteTime := FileTimeToLocalDateTime(Info.ftLastWriteTime);
    fNumberOfLinks := Info.nNumberOfLinks;
    fVolumeSerialNumber := Info.dwVolumeSerialNumber;
    fFileID := (UInt64(Info.nFileIndexHigh) shl 32) or UInt64(Info.nFileIndexLow);
    fAttributesFlags := Info.dwFileAttributes;
  end
else raise EWFISystemError.CreateFmt('TWinFileInfo.LoadBasicInfo: Failed to obtain file info (%d).',[GetLastError]);
end;
{$ELSE}
const
  NanoSecsPerDay = 24 * 3600 * 1000000000;
var
  FileStat: stat;
begin
FillChar(Addr(FileStat)^,SizeOf(stat),0);
If FpFStat(fFileHandle,FileStat) = 0 then
  begin
    fSize := UInt64(FileStat.st_size);
    fLastAccessTimeRaw := UnixToDateTime(FileStat.st_atime) + (FileStat.st_atime_nsec / NanoSecsPerDay);
    fLastModificationTimeRaw := UnixToDateTime(FileStat.st_mtime) + (FileStat.st_mtime_nsec / NanoSecsPerDay);
    fLastStatusChangeTimeRaw := UnixToDateTime(FileStat.st_ctime) + (FileStat.st_ctime_nsec / NanoSecsPerDay);
    fLastAccessTime := UniversalTimeToLocal(fLastAccessTimeRaw);
    fLastModificationTime := UniversalTimeToLocal(fLastModificationTimeRaw);
    fLastStatusChangeTime := UniversalTimeToLocal(fLastStatusChangeTimeRaw);
    fNumberOfHardLinks := PtrUInt(FileStat.st_nlink);
    fDeviceID := UInt64(FileStat.st_dev);
    fiNodeNumber := UInt64(FileStat.st_ino);
    fBlockSize := PtrUInt(FileStat.st_blksize);
    fBlocks := UInt64(FileStat.st_blocks);
    fOwnerUserID := UInt32(FileStat.st_uid);
    fOwnerGroupID := UInt32(FileStat.st_gid);
    fMode := UInt32(FileStat.st_mode);
  end
else raise EWFISystemError.CreateFmt('TWinFileInfo.LoadBasicInfo: Failed to obtain file stat (%d)',[errno]);
end;
{$ENDIF}
{$IFDEF Windows}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

procedure TWinFileInfo.DecodeBasicInfo;
{$IFDEF Windows}

  // This function also fills AttributesStr and AttributesText strings
  Function ProcessAttribute(AttributeFlag: DWORD): Boolean;
  var
    i:  Integer;
  begin
    Result := (fAttributesFlags and AttributeFlag) <> 0;
    If Result then
      For i := Low(WFI_FILE_ATTR_STRS) to High(WFI_FILE_ATTR_STRS) do
        If WFI_FILE_ATTR_STRS[i].Flag = AttributeFlag then
          begin
            fAttributesStr := fAttributesStr + WFI_FILE_ATTR_STRS[i].Str;
            If fAttributesText = '' then
              fAttributesText := WFI_FILE_ATTR_STRS[i].Text
            else
              fAttributesText := Format('%s, %s',[fAttributesText,WFI_FILE_ATTR_STRS[i].Text]);
            Break;
          end;
  end;

begin
fSizeStr := FileSizeToStr(fSize,fFormatSettings);
// attributes
fAttributesDecoded.Archive           := ProcessAttribute(FILE_ATTRIBUTE_ARCHIVE);
fAttributesDecoded.Compressed        := ProcessAttribute(FILE_ATTRIBUTE_COMPRESSED);
fAttributesDecoded.Device            := ProcessAttribute(FILE_ATTRIBUTE_DEVICE);
fAttributesDecoded.Directory         := ProcessAttribute(FILE_ATTRIBUTE_DIRECTORY);
fAttributesDecoded.Encrypted         := ProcessAttribute(FILE_ATTRIBUTE_ENCRYPTED);
fAttributesDecoded.Hidden            := ProcessAttribute(FILE_ATTRIBUTE_HIDDEN);
fAttributesDecoded.IntegrityStream   := ProcessAttribute(FILE_ATTRIBUTE_INTEGRITY_STREAM);
fAttributesDecoded.Normal            := ProcessAttribute(FILE_ATTRIBUTE_NORMAL);
fAttributesDecoded.NotContentIndexed := ProcessAttribute(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED);
fAttributesDecoded.NoScrubData       := ProcessAttribute(FILE_ATTRIBUTE_NO_SCRUB_DATA);
fAttributesDecoded.Offline           := ProcessAttribute(FILE_ATTRIBUTE_OFFLINE);
fAttributesDecoded.ReadOnly          := ProcessAttribute(FILE_ATTRIBUTE_READONLY);
fAttributesDecoded.ReparsePoint      := ProcessAttribute(FILE_ATTRIBUTE_REPARSE_POINT);
fAttributesDecoded.SparseFile        := ProcessAttribute(FILE_ATTRIBUTE_SPARSE_FILE);
fAttributesDecoded.System            := ProcessAttribute(FILE_ATTRIBUTE_SYSTEM);
fAttributesDecoded.Temporary         := ProcessAttribute(FILE_ATTRIBUTE_TEMPORARY);
fAttributesDecoded.Virtual           := ProcessAttribute(FILE_ATTRIBUTE_VIRTUAL);
end;
{$ELSE}

  procedure AddToString(var Str: String; const Addend: String);
  begin
    If Length(Addend) > 0 then
      begin
        If Length(Str) > 0 then
          Str := Str + ' ' + Addend
        else
          Str := Addend;
      end;
  end;

  Function GetRWXStr(CheckedPerms: array of TWFIFilePermission; const Prefix: String): String;
  begin
    Result := '';
    If Length(CheckedPerms) >= 3 then
      begin
        If CheckedPerms[0] in fPermissions then
          Result := Result + 'R';
        If CheckedPerms[1] in fPermissions then
          Result := Result + 'W';
        If CheckedPerms[2] in fPermissions then
          Result := Result + 'X';
      end;
    If Length(Result) > 0 then
      Result := Prefix + Result;
  end;

var
  i:  TWFIFilePermission;
begin
fSizeStr := FileSizeToStr(fSize,fFormatSettings);
// decode mode... file type
case (fMode and S_IFMT) of
  S_IFIFO:  fFileType := ftFIFO;
  S_IFCHR:  fFileType := ftCharacterDevice;
  S_IFDIR:  fFileType := ftDirectory;
  S_IFBLK:  fFileType := ftBlockDevice;
  S_IFREG:  fFileType := ftRegularFile;
  S_IFLNK:  fFileType := ftSymbolicLink;
  S_IFSOCK: fFileType := ftSocket;
else
  fFileType := ftUnknown;
end;
fFileTypeStr := WFI_FILE_TYPE_STRS[fFileType];
// file permissions
fPermissions := [];
For i := Low(TWFIFilePermission) to High(TWFIFilePermission) do
  If fMode and WFI_PERMISSION_FLAGS[i] <> 0 then
    Include(fPermissions,i);
// permissions to text
fPermissionsStr := '';
If (fPermissions * [fpUserRead,fpUserWrite,fpUserExecute]) <> [] then
  AddToString(fPermissionsStr,GetRWXStr([fpUserRead,fpUserWrite,fpUserExecute],'U-'));
If (fPermissions * [fpGroupRead,fpGroupWrite,fpGroupExecute]) <> [] then
  AddToString(fPermissionsStr,GetRWXStr([fpGroupRead,fpGroupWrite,fpGroupExecute],'G-'));
If (fPermissions * [fpOthersRead,fpOthersWrite,fpOthersExecute]) <> [] then
  AddToString(fPermissionsStr,GetRWXStr([fpOthersRead,fpOthersWrite,fpOthersExecute],'O-'));
If fpSetUserID in fPermissions then
  AddToString(fPermissionsStr,'SUID');
If fpSetGroupID in fPermissions then
  AddToString(fPermissionsStr,'SGID');
If fpSticky in fPermissions then
  AddToString(fPermissionsStr,'STCK');
end;
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF Windows}

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TWinFileInfo.LoadVersionInfo;
var
  Dummy:  DWORD;
begin
fVerInfoSize := GetFileVersionInfoSize(PChar(StrToWin(fLongName)),Dummy);
fVersionInfoPresent := fVerInfoSize > 0;
If fVersionInfoPresent then
  begin
    fVerInfoData := AllocMem(fVerInfoSize);
    If GetFileVersionInfo(PChar(StrToWin(fLongName)),0,fVerInfoSize,fVerInfoData) then
      begin
        VersionInfo_LoadTranslations;
        // parsing must be done here, before strings loading
        If lsaParseVersionInfo in fLoadingStrategy then
          begin
            VersionInfo_Parse;
            If lsaVerInfoExtractTranslations in fLoadingStrategy then
              VersionInfo_ExtractTranslations;
            VersionInfo_EnumerateKeys;
          end;
        VersionInfo_LoadStrings;
      end
    else
      begin
        FreeMem(fVerInfoData,fVerInfoSize);
        fVerInfoData := nil;
        fVerInfoSize := 0;
        fVersionInfoPresent := False;
      end;
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TWinFileInfo.LoadFixedFileInfo;
var
  FFIPtr:   Pointer;
  FFISize:  UInt32;
begin
fVersionInfoFFIPresent := VerQueryValue(fVerInfoData,'\',FFIPtr,FFISize);
If fVersionInfoFFIPresent then
  begin
    If FFISize = SizeOf(TVSFixedFileInfo) then
      fVersionInfoFFI := PVSFixedFileInfo(FFIPtr)^
    else
      raise EWFIProcessingError.CreateFmt('TWinFileInfo.LoadFixedFileInfo: Wrong size of fixed file information (got %d, expected %d).',[FFISize,SizeOf(TVSFixedFileInfo)]);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TWinFileInfo.DecodeFixedFileInfo;
var
  FFIWorkFileFlags: DWORD;

  Function VersionToStr(Low,High: DWORD): String;
  begin
    Result := Format('%d.%d.%d.%d',[High shr 16,High and $FFFF,Low shr 16,Low and $FFFF]);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function GetFlagText(Flag: DWORD; Data: array of TWFIFlagText; const NotFound: String): String;
  var
    i:  Integer;
  begin
    Result := NotFound;
    For i := Low(Data) to High(Data) do
      If Data[i].Flag = Flag then
        begin
          Result := Data[i].Text;
          Break;
        end;
  end;

begin
with fVersionInfoFFIDecoded,fVersionInfoFFI do
  begin
    FileVersionFull := (UInt64(dwFileVersionMS) shl 32) or UInt64(fVersionInfoFFI.dwFileVersionLS);
    FileVersionMembers.Major := dwFileVersionMS shr 16;
    FileVersionMembers.Minor := dwFileVersionMS and $FFFF;
    FileVersionMembers.Release := dwFileVersionLS shr 16;
    FileVersionMembers.Build := dwFileVersionLS and $FFFF;
    FileVersionStr := VersionToStr(dwFileVersionLS,dwFileVersionMS);
    ProductVersionFull := (UInt64(dwProductVersionMS) shl 32) or UInt64(dwProductVersionLS);
    ProductVersionMembers.Major := dwProductVersionMS shr 16;
    ProductVersionMembers.Minor := dwProductVersionMS and $FFFF;
    ProductVersionMembers.Release := dwProductVersionLS shr 16;
    ProductVersionMembers.Build := dwProductVersionLS and $FFFF;
    ProductVersionStr := VersionToStr(dwProductVersionLS,dwProductVersionMS);
    // mask flags
    FFIWorkFileFlags := dwFileFlags and dwFileFlagsMask;
    // decode masked flags
    FileFlags.Debug        := ((FFIWorkFileFlags) and VS_FF_DEBUG) <> 0;
    FileFlags.InfoInferred := ((FFIWorkFileFlags) and VS_FF_INFOINFERRED) <> 0;
    FileFlags.Patched      := ((FFIWorkFileFlags) and VS_FF_PATCHED) <> 0;
    FileFlags.Prerelease   := ((FFIWorkFileFlags) and VS_FF_PRERELEASE) <> 0;
    FileFlags.PrivateBuild := ((FFIWorkFileFlags) and VS_FF_PRIVATEBUILD) <> 0;
    FileFlags.SpecialBuild := ((FFIWorkFileFlags) and VS_FF_SPECIALBUILD) <> 0;
    FileOSStr := GetFlagText(dwFileOS,WFI_FFI_FILE_OS_STRS,'Unknown');
    FileTypeStr := GetFlagText(dwFileType,WFI_FFI_FILE_TYPE_STRS,'Unknown');
    case fVersionInfoFFI.dwFileType of
      VFT_DRV:  FileSubTypeStr := GetFlagText(dwFileType,WFI_FFI_FILE_SUBTYPE_DRV_STRS,'Unknown');
      VFT_FONT: FileSubTypeStr := GetFlagText(dwFileType,WFI_FFI_FILE_SUBTYPE_FONT_STRS,'Unknown');
      VFT_VXD:  FileSubTypeStr := IntToHex(dwFileSubtype,8);
    else
      FileSubTypeStr := '';
    end;
    FileDateFull := (UInt64(dwFileDateMS) shl 32) or UInt64(dwFileDateLS);
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

procedure TWinFileInfo.Clear;
begin
// do not clear file handle, existence indication and names
fSize := 0;
fSizeStr := '';
{$IFDEF Windows}
fCreationTimeRaw := 0;
fLastAccessTimeRaw := 0;
fLastWriteTimeRaw := 0;
fCreationTime := 0;
fLastAccessTime := 0;
fLastWriteTime := 0;
fNumberOfLinks := 0;
fVolumeSerialNumber := 0;
fFileID := 0;
// attributes
fAttributesFlags := 0;
fAttributesStr := '';
fAttributesText := '';
FillChar(fAttributesDecoded,SizeOf(fAttributesDecoded),0);
// version info unparsed data
If Assigned(fVerInfoData) and (fVerInfoSize <> 0) then
  FreeMem(fVerInfoData,fVerInfoSize);
fVerInfoData := nil;
fVerInfoSize := 0;
// version info
fVersionInfoPresent := False;
// fixed file info
fVersionInfoFFIPresent := False;
FillChar(fVersionInfoFFI,SizeOf(fVersionInfoFFI),0);
fVersionInfoFFIDecoded.FileVersionStr := '';
fVersionInfoFFIDecoded.ProductVersionStr := '';
fVersionInfoFFIDecoded.FileOSStr := '';
fVersionInfoFFIDecoded.FileTypeStr := '';
fVersionInfoFFIDecoded.FileSubTypeStr := '';
FillChar(fVersionInfoFFIDecoded,SizeOf(fVersionInfoFFIDecoded),0);
// version info partially parsed data
fVersionInfoStruct.Key := '';
SetLength(fVersionInfoStruct.StringFileInfos,0);
SetLength(fVersionInfoStruct.VarFileInfos,0);
FillChar(fVersionInfoStruct,SizeOf(fVersionInfoStruct),0);
{
  do not free fVersionInfoStruct.FixedFileInfo pointer, it was pointing into
  fVerInfoData memory block which was already freed
}
// version info fully parsed data
fVersionInfoParsed := False;
SetLength(fVersionInfoStringTables,0);
{$ELSE}
fLastAccessTimeRaw := 0;
fLastModificationTimeRaw := 0;
fLastStatusChangeTimeRaw := 0;
fLastAccessTime := 0;
fLastModificationTime := 0;
fLastStatusChangeTime := 0;
fNumberOfHardLinks := 0;
fDeviceID := 0;
fiNodeNumber := 0;
fBlockSize := 0;
fBlocks := 0;
fOwnerUserID := 0;
fOwnerGroupID := 0;
fMode := 0;
// decoded mode
fFileType := ftUnknown;
fFileTypeStr := '';
fPermissions := [];
fPermissionsStr := '';
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFNDEF Windows}{$IFDEF FPCDWM}{$PUSH}W6058{$ENDIF}{$ENDIF}
procedure TWinFileInfo.Initialize(const FileName: String);
var
  LastError:  {$IFDEF Windows}Integer{$ELSE}cint{$ENDIF};
begin
{$IFDEF Windows}
// set file-name-paths
fName := FileName;
fLongName := RTLToStr(ExpandFileName(StrToRTL(FileName)));
SetLength(fShortName,MAX_PATH);
SetLength(fShortName,GetShortPathName(PChar(StrToWin(fLongName)),PChar(fShortName),Length(fShortName)));
fShortName := WinToStr(fShortName);
// open file and check its existence (with this arguments it cannot open a directory, so that one is good)
fFileHandle := CreateFile(PChar(StrToWin(fLongName)),0,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
If fFileHandle <> INVALID_HANDLE_VALUE then
  begin
    // file was successfully opened, we can assume it exists
    fExists := True;
    // and now for stages of loading and parsing
    If lsaLoadBasicInfo in fLoadingStrategy then
      begin
        LoadBasicInfo;
        If lsaDecodeBasicInfo in fLoadingStrategy then
          DecodeBasicInfo;
      end;
    If lsaLoadVersionInfo in fLoadingStrategy then
      begin
        LoadVersionInfo;  // also do parsing and other stuff
        If fVersionInfoPresent then
          begin
            // following can be done only if version info is loaded
            If lsaLoadFixedFileInfo in fLoadingStrategy then
              begin
                LoadFixedFileInfo;
                If fVersionInfoFFIPresent and (lsaDecodeFixedFileInfo in fLoadingStrategy) then
                  DecodeFixedFileInfo;
              end;
          end;
      end;
    If not(lsaKeepOpen in fLoadingStrategy) then
      begin
        CloseHandle(fFileHandle);
        fFileHandle := INVALID_HANDLE_VALUE;
      end;
  end
else
  begin
    // CreateFile has failed
    LastError := GetLastError;
    If LastError = ERROR_FILE_NOT_FOUND then
      fExists := False
    else
      raise EWFISystemError.CreateFmt('TWinFileInfo.Initialize: Failed to open requested file (%d).',[LastError]);
  end;
{$ELSE}
fName := FileName;
fLongName := ExpandFileName(FileName);
// first try opening the file as directory, to see what it is...
fFileHandle := FpOpen(PChar(fLongName),O_RDONLY or O_DIRECTORY);
If fFileHandle <> -1 then
  begin
    // the diven path is a directory :(
    FpClose(fFileHandle);
    fFileHandle := -1;
    errno := ESysEISDIR;
  end
// the opening failed, so we assume the file is not a directory
else fFileHandle := FpOpen(PChar(fLongName),O_RDONLY or O_NOATIME or O_PATH);
If fFileHandle <> -1 then
  begin
    // file was successfully opened
    fExists := True;
    If lsaLoadBasicInfo in fLoadingStrategy then
      begin
        LoadBasicInfo;
        If lsaDecodeBasicInfo in fLoadingStrategy then
          DecodeBasicInfo;
      end;
    If not(lsaKeepOpen in fLoadingStrategy) then
      begin
        FpClose(fFileHandle);
        fFileHandle := -1;
      end;
  end
else
  begin
    // FpOpen has failed
    LastError := errno;
    If LastError = ESysENOENT then
      fExists := False
    else
      raise EWFISystemError.CreateFmt('TWinFileInfo.Initialize: Failed to open requested file (%d)',[LastError]);
  end;
{$ENDIF}
end;
{$IFNDEF Windows}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

procedure TWinFileInfo.Finalize;
begin
Clear;
If lsaKeepOpen in fLoadingStrategy then
  begin
  {$IFDEF Windows}
    CloseHandle(fFileHandle);
    fFileHandle := INVALID_HANDLE_VALUE;
  {$ELSE}
    FpClose(fFileHandle); // ignore errors
    fFileHandle := -1;
  {$ENDIF}
  end;
end;

{-------------------------------------------------------------------------------
    TWinFileInfo - public methods
-------------------------------------------------------------------------------}

constructor TWinFileInfo.Create(LoadingStrategy: TWFILoadingStrategy = WFI_LS_All);
var
  ModuleFileName: String;
{$IFDEF Windows}
begin
ModuleFileName := '';
SetLength(ModuleFileName,MAX_PATH);
SetLength(ModuleFileName,GetModuleFileNameEx(GetCurrentProcess,hInstance,PChar(ModuleFileName),Length(ModuleFileName)));
ModuleFileName := WinToStr(ModuleFileName);
{$ELSE}
  Info:           dl_info;
begin
ModuleFileName := '';
FillChar(Addr(Info)^,SizeOf(dl_info),0);
If dladdr(@SameFile{a function in this unit},@Info) <> 0 then
  If Assigned(Info.dli_fname) then
    ModuleFileName := ExpandFileName(String(Info.dli_fname));
{$ENDIF}
Create(ModuleFileName,LoadingStrategy);
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

constructor TWinFileInfo.Create(const FileName: String; LoadingStrategy: TWFILoadingStrategy = WFI_LS_All);
begin
inherited Create;
fLoadingStrategy := LoadingStrategy;
InitFormatSettings(fFormatSettings);
Initialize(FileName);
end;

//------------------------------------------------------------------------------

destructor TWinFileInfo.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TWinFileInfo.Refresh;
begin
Finalize; // calls Clear
Initialize(fName);
end;

//------------------------------------------------------------------------------

procedure TWinFileInfo.CreateReport(Strings: TStrings);
{$IFDEF Windows}
var
  i,j:  Integer;
  Len:  Integer;
{$ENDIF}
begin
Strings.Add('=== TWinInfoFile report, created on ' + DateTimeToStr(Now,fFormatSettings) + ' ===');
{$IFDEF Windows}
Strings.Add(sLineBreak + '--- General info ---' + sLineBreak);
Strings.Add('  Exists:     ' + BoolToStr(fExists,True));
Strings.Add('  Long name:  ' + LongName);
Strings.Add('  Short name: ' + ShortName);
Strings.Add(sLineBreak + 'Size:' + sLineBreak);
Strings.Add('  Size:    ' + IntToStr(fSize));
Strings.Add('  SizeStr: ' + SizeStr);
Strings.Add(sLineBreak + 'Time:' + sLineBreak);
Strings.Add(Format('  Created:     %s (%s)',[DateTimeToStr(fCreationTime),DateTimeToStr(fCreationTimeRaw)]));
Strings.Add(Format('  Last access: %s (%s)',[DateTimeToStr(fLastAccessTime),DateTimeToStr(fLastAccessTimeRaw)]));
Strings.Add(Format('  Last write:  %s (%s)',[DateTimeToStr(fLastWriteTime),DateTimeToStr(fLastWriteTimeRaw)]));
Strings.Add(sLineBreak + 'Others:' + sLineBreak);
Strings.Add('  Number of links:      ' + IntToStr(fNumberOfLinks));
Strings.Add('  Volume serial number: ' + IntToHex(fVolumeSerialNumber,8));
Strings.Add('  File ID:              ' + IntToHex(fFileID,16));
Strings.Add(sLineBreak + 'Attributes:' + sLineBreak);
Strings.Add('  Attributes flags:  ' + IntToHex(fAttributesFlags,8));
Strings.Add('  Attributes string: ' + fAttributesStr);
Strings.Add('  Attributes text:   ' + fAttributesText);
Strings.Add(sLineBreak + '  Attributes decoded:');
Strings.Add('    Archive:             ' + BoolToStr(fAttributesDecoded.Archive,True));
Strings.Add('    Compressed:          ' + BoolToStr(fAttributesDecoded.Compressed,True));
Strings.Add('    Device:              ' + BoolToStr(fAttributesDecoded.Device,True));
Strings.Add('    Directory:           ' + BoolToStr(fAttributesDecoded.Directory,True));
Strings.Add('    Encrypted:           ' + BoolToStr(fAttributesDecoded.Encrypted,True));
Strings.Add('    Hidden:              ' + BoolToStr(fAttributesDecoded.Hidden,True));
Strings.Add('    Integrity stream:    ' + BoolToStr(fAttributesDecoded.IntegrityStream,True));
Strings.Add('    Normal:              ' + BoolToStr(fAttributesDecoded.Normal,True));
Strings.Add('    Not content indexed: ' + BoolToStr(fAttributesDecoded.NotContentIndexed,True));
Strings.Add('    No scrub data:       ' + BoolToStr(fAttributesDecoded.NoScrubData,True));
Strings.Add('    Offline:             ' + BoolToStr(fAttributesDecoded.Offline,True));
Strings.Add('    Read only:           ' + BoolToStr(fAttributesDecoded.ReadOnly,True));
Strings.Add('    Reparse point:       ' + BoolToStr(fAttributesDecoded.ReparsePoint,True));
Strings.Add('    Sparse file:         ' + BoolToStr(fAttributesDecoded.SparseFile,True));
Strings.Add('    System:              ' + BoolToStr(fAttributesDecoded.System,True));
Strings.Add('    Temporary:           ' + BoolToStr(fAttributesDecoded.Temporary,True));
Strings.Add('    Virtual:             ' + BoolToStr(fAttributesDecoded.Virtual,True));
If fVersionInfoPresent then
  begin
    Strings.Add(sLineBreak + '--- File version info ---');
    If fVersionInfoFFIPresent then
      begin
        Strings.Add(sLineBreak + 'Fixed file info:' + sLineBreak);
        Strings.Add('  Signature:         ' + IntToHex(fVersionInfoFFI.dwSignature,8));
        Strings.Add('  Struct version:    ' + IntToHex(fVersionInfoFFI.dwStrucVersion,8));
        Strings.Add('  File version H:    ' + IntToHex(fVersionInfoFFI.dwFileVersionMS,8));
        Strings.Add('  File version L:    ' + IntToHex(fVersionInfoFFI.dwFileVersionLS,8));
        Strings.Add('  Product version H: ' + IntToHex(fVersionInfoFFI.dwProductVersionMS,8));
        Strings.Add('  Product version L: ' + IntToHex(fVersionInfoFFI.dwProductVersionLS,8));
        Strings.Add('  File flags mask :  ' + IntToHex(fVersionInfoFFI.dwFileFlagsMask,8));
        Strings.Add('  File flags:        ' + IntToHex(fVersionInfoFFI.dwFileFlags,8));
        Strings.Add('  File OS:           ' + IntToHex(fVersionInfoFFI.dwFileOS,8));
        Strings.Add('  File type:         ' + IntToHex(fVersionInfoFFI.dwFileType,8));
        Strings.Add('  File subtype:      ' + IntToHex(fVersionInfoFFI.dwFileSubtype,8));
        Strings.Add('  File date H:       ' + IntToHex(fVersionInfoFFI.dwFileDateMS,8));
        Strings.Add('  File date L:       ' + IntToHex(fVersionInfoFFI.dwFileDateLS,8));
        Strings.Add(sLineBreak + '  Fixed file info decoded:');
        Strings.Add('    File version full:      ' + IntToHex(fVersionInfoFFIDecoded.FileVersionFull,16));
        Strings.Add('    File version members:');
        Strings.Add('      Major:   ' + IntToStr(fVersionInfoFFIDecoded.FileVersionMembers.Major));
        Strings.Add('      Minor:   ' + IntToStr(fVersionInfoFFIDecoded.FileVersionMembers.Minor));
        Strings.Add('      Release: ' + IntToStr(fVersionInfoFFIDecoded.FileVersionMembers.Release));
        Strings.Add('      Build:   ' + IntToStr(fVersionInfoFFIDecoded.FileVersionMembers.Build));
        Strings.Add('    File version string:    ' + fVersionInfoFFIDecoded.FileVersionStr);
        Strings.Add('    Product version full:   ' + IntToHex(fVersionInfoFFIDecoded.ProductVersionFull,16));
        Strings.Add('    Product version members:');
        Strings.Add('      Major:   ' + IntToStr(fVersionInfoFFIDecoded.ProductVersionMembers.Major));
        Strings.Add('      Minor:   ' + IntToStr(fVersionInfoFFIDecoded.ProductVersionMembers.Minor));
        Strings.Add('      Release: ' + IntToStr(fVersionInfoFFIDecoded.ProductVersionMembers.Release));
        Strings.Add('      Build:   ' + IntToStr(fVersionInfoFFIDecoded.ProductVersionMembers.Build));
        Strings.Add('    Product version string: ' + fVersionInfoFFIDecoded.ProductVersionStr);
        Strings.Add('    File flags:');
        Strings.Add('      Debug:         ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.Debug,True));
        Strings.Add('      Info inferred: ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.InfoInferred,True));
        Strings.Add('      Patched:       ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.Patched,True));
        Strings.Add('      Prerelease:    ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.Prerelease,True));
        Strings.Add('      Private build: ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.PrivateBuild,True));
        Strings.Add('      Special build: ' + BoolToStr(fVersionInfoFFIDecoded.FileFlags.SpecialBuild,True));
        Strings.Add('    File OS string:         ' + fVersionInfoFFIDecoded.FileOSStr);
        Strings.Add('    File type string:       ' + fVersionInfoFFIDecoded.FileTypeStr);
        Strings.Add('    File subtype string:    ' + fVersionInfoFFIDecoded.FileSubTypeStr);
        Strings.Add('    File date full:         ' + IntToHex(fVersionInfoFFIDecoded.FileDateFull,16));
      end
    else Strings.Add(sLineBreak + 'Fixed file info not present.');
    If VersionInfoTranslationCount > 0 then
      begin
        Strings.Add(sLineBreak + 'Version info translations:' + sLineBreak);
        Strings.Add('  Translation count: ' + IntToStr(VersionInfoTranslationCount));
        For i := 0 to Pred(VersionInfoTranslationCount) do
          begin
            Strings.Add(sLineBreak + Format('  Translation %d:',[i]));
            Strings.Add('    Language:        ' + IntToStr(VersionInfoTranslations[i].Language));
            Strings.Add('    Code page:       ' + IntToStr(VersionInfoTranslations[i].CodePage));
            Strings.Add('    Translation:     ' + IntToHex(VersionInfoTranslations[i].Translation,8));
            Strings.Add('    Language string: ' + VersionInfoTranslations[i].LanguageStr);
            Strings.Add('    Language name:   ' + VersionInfoTranslations[i].LanguageName);
          end;
        end
      else Strings.Add(sLineBreak + 'No translation found.');
    If VersionInfoStringTableCount > 0 then
      begin
        Strings.Add(sLineBreak + 'Version info string tables:' + sLineBreak);
        Strings.Add('  String table count: ' + IntToStr(VersionInfoStringTableCount));
        For i := 0 to Pred(VersionInfoStringTableCount) do
          begin
            Strings.Add(sLineBreak + Format('  String table %d (%s):',[i,fVersionInfoStringTables[i].Translation.LanguageName]));
            Len := 0;
            For j := 0 to Pred(VersionInfoStringCount[i]) do
              If Length(VersionInfoStrings[i,j].Key) > Len then Len := Length(VersionInfoStrings[i,j].Key);
            For j := 0 to Pred(VersionInfoStringCount[i]) do
              Strings.Add(Format('    %s: %s%s',[VersionInfoStrings[i,j].Key,
                StringOfChar(' ',Len - Length(VersionInfoStrings[i,j].Key)),
                VersionInfoStrings[i,j].Value]));
          end;
        end
      else Strings.Add(sLineBreak + 'No string table found.');
  end
else Strings.Add(sLineBreak + 'File version information not present.');
{$ELSE}
Strings.Add(sLineBreak + '--- General info ---' + sLineBreak);
Strings.Add('  Exists:     ' + BoolToStr(fExists,True));
Strings.Add('  Long name:  ' + LongName);
Strings.Add(sLineBreak + 'Size:' + sLineBreak);
Strings.Add('  Size:    ' + IntToStr(fSize));
Strings.Add('  SizeStr: ' + SizeStr);
Strings.Add(sLineBreak + 'Time:' + sLineBreak);
Strings.Add(Format('  Last access:        %s (%s)',[DateTimeToStr(fLastAccessTime),DateTimeToStr(fLastAccessTimeRaw)]));
Strings.Add(Format('  Last modification:  %s (%s)',[DateTimeToStr(fLastModificationTime),DateTimeToStr(fLastModificationTimeRaw)]));
Strings.Add(Format('  Last status change: %s (%s)',[DateTimeToStr(fLastStatusChangeTime),DateTimeToStr(fLastStatusChangeTimeRaw)]));
Strings.Add(sLineBreak + 'Others:' + sLineBreak);
Strings.Add('  Number of hard links: ' + IntToStr(fNumberOfHardLinks));
Strings.Add('  Device ID:            ' + IntToStr(fDeviceID));
Strings.Add('  iNode number:         ' + IntToStr(fiNodeNumber));
Strings.Add('  Block Size:           ' + IntToStr(fBlockSize));
Strings.Add('  Block count:          ' + IntToStr(fBlocks));
Strings.Add('  Owner user ID:        ' + IntToStr(fOwnerUserID));
Strings.Add('  Owner group ID:       ' + IntToStr(fOwnerGroupID));
Strings.Add('  File mode:            ' + IntToHex(fMode,8));
Strings.Add(sLineBreak + 'File type:' + sLineBreak);
Strings.Add('  File type:        ' + IntToStr(Ord(fFileType)));
Strings.Add('  File type string: ' + fFileTypeStr);
Strings.Add(sLineBreak + 'Permissions:' + sLineBreak);
Strings.Add('  User can read:      ' + BoolToStr(fpUserRead in fPermissions,True));
Strings.Add('  User can write:     ' + BoolToStr(fpUserWrite in fPermissions,True));
Strings.Add('  User can execute:   ' + BoolToStr(fpUserExecute in fPermissions,True));
Strings.Add('  Group can read:     ' + BoolToStr(fpGroupRead in fPermissions,True));
Strings.Add('  Group can write:    ' + BoolToStr(fpGroupWrite in fPermissions,True));
Strings.Add('  Group can execute:  ' + BoolToStr(fpGroupExecute in fPermissions,True));
Strings.Add('  Others can read:    ' + BoolToStr(fpOthersRead in fPermissions,True));
Strings.Add('  Others can write:   ' + BoolToStr(fpOthersWrite in fPermissions,True));
Strings.Add('  Others can execute: ' + BoolToStr(fpOthersExecute in fPermissions,True));
Strings.Add('  Set user ID:        ' + BoolToStr(fpSetUserID in fPermissions,True));
Strings.Add('  Set group ID:       ' + BoolToStr(fpSetGroupID in fPermissions,True));
Strings.Add('  Sticky bit:         ' + BoolToStr(fpSticky in fPermissions,True));
Strings.Add(sLineBreak + '  File permissions string: ' + fPermissionsStr);
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TWinFileInfo.CreateReport: String;
var
  Strings:  TStringList;
begin
Strings := TStringList.Create;
try
  CreateReport(Strings);
  Result := Strings.Text;
finally
  Strings.Free;
end;
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}

Function TWinFileInfo.IndexOfVersionInfoStringTable(Translation: DWORD): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(fVersionInfoStringTables) to High(fVersionInfoStringTables) do
  If fVersionInfoStringTables[i].Translation.Translation = Translation then
    begin
      Result := i;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

Function TWinFileInfo.IndexOfVersionInfoString(Table: Integer; const Key: String): Integer;
var
  i:  Integer;
begin
Result := -1;
with GetVersionInfoStringTable(Table) do
  For i := Low(Strings) to High(Strings) do
    If AnsiSameText(Strings[i].Key,Key) then
      begin
        Result := i;
        Exit;
      end;
end;

{$ENDIF}

end.
