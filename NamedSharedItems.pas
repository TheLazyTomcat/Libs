{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Shared named items

    Provides a class (TNamedSharedItem) to be used for creation of small named
    shared (system-wide) memory blocks (here named items).

    It is inteded only for shared objects or medium-size data (at most 1KiB,
    this limit is enforced) - since each item has significant overhead
    (32 bytes at minimum), it should not be used for simple shared variables.
    Note that items are not allowed to have a size of zero.

    All created items are placed in one shared memory, so it is desirable to
    split them into smaller groups that will not interfere.
    There is a NameSpace parameter for this purpose. It is a short string
    (maximum length of 32 characters) that, along with item size, creates
    distinct groups of items.
    So, two items can be in the same group only if their namespace and size
    both matches, otherwise they are created in a separate groups.
    Given the abovementioned, do not use this library to create many items with
    differing sizes or namespaces, as each group has somewhat high overhead
    (128KiB+).

    If you create a new item, it is in the same group and has the same name as
    already existing one, then both items will occupy the same memory, allowing
    for effective sharing of data (note that a reference count is managed
    internally).

    The idividual items are in fact not discerned by their full name, they are
    discerned by a cryptographic hash (SHA1) of their name. So there is a very
    small, but still non-zero, posibility of name conflicts. Be aware of that.

    Length of the item name is not explicitly limited, but is not recommended
    to be zero.

  Version 1.1.4 (2024-05-03)

  Last change 2024-09-09

  ©2021-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.NamedSharedItems

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    SHA1               - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol NamedSharedItems_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit NamedSharedItems;
{
  NamedSharedItems_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  NamedSharedItems_UseAuxExceptions to achieve this.
}
{$IF Defined(NamedSharedItems_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses, SHA1, SharedMemoryStream
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exception
===============================================================================}
type
  ENSIException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ENSIInvalidValue        = class(ENSIException);
  ENSIOutOfResources      = class(ENSIException);
  ENSIItemAllocationError = class(ENSIException);

{===============================================================================
--------------------------------------------------------------------------------
                                TNamedSharedItem
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TNamedSharedItem - class declaration
===============================================================================}
type
  TNamedSharedItem = class(TCustomObject)
  protected
    fCreated:           Boolean;
    fNameSpace:         String;
    fName:              String;
    fNameHash:          TSHA1;
    fSize:              TMemSize;
    fInfoSection:       TSharedMemory;
    fDataSectionIndex:  Integer;
    fDataSection:       TSimpleSharedMemory;  // no locking (to save some resources)
    fItemMemory:        Pointer;
    fPayloadMemory:     Pointer;
    // some helper fields
    fFullItemSize:      TMemSize;
    fItemsPerSection:   UInt32;
    Function GetInfoSectionName: String; virtual;
    Function GetDataSectionName(Index: Integer): String; virtual;
    procedure FindOrAllocateItem; virtual;
    procedure AllocateItem(HoldLock: Boolean); virtual;
    procedure DeallocateItem; virtual;
    procedure Initialize(const Name: String; Size: TMemSize; const NameSpace: String; HoldLock: Boolean); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(const Name: String; Size: TMemSize; const NameSpace: String = '');
  {
    Use CreateLocked when you need to create and intialize the item in an
    atomic manner. The entire underlying system stays globally locked (as if
    method GlobalLock was called) when the constructor returns (unless an
    exception is raised in the constructor, in which case the lock is not kept),
    so no new item can be created or removed anywhere in the system. Do the
    initialization quickly to prevent unnecessary lockups.

      WARNING - remember to unlock the item using method GlobalUnlock when you
                are done!
  }
    constructor CreateLocked(const Name: String; Size: TMemSize; const NameSpace: String = ''{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    destructor Destroy; override;
    procedure GlobalLock; virtual;
    procedure GlobalUnlock; virtual;
  {
    Created is true if the item was newly created, false when it already
    existed and was only opened.
  }
    property Created: Boolean read fCreated;
    property NameSpace: String read fNameSpace;
    property Name: String read fName;
    property Size: TMemSize read fSize;
    property Memory: Pointer read fPayloadMemory;
  end;

implementation

uses
  StrRect, BitOps;

{===============================================================================
--------------------------------------------------------------------------------
                                TNamedSharedItem
--------------------------------------------------------------------------------
===============================================================================}
const
  NSI_SHAREDMEMORY_NAMESPACE_MAXLEN = 32;

{-------------------------------------------------------------------------------
    TNamedSharedItem - info section types and constants
-------------------------------------------------------------------------------}
const
  NSI_SHAREDMEMORY_INFOSECT_MAXCOUNT = 16 * 1024; // total 1GiB of memory with 64KiB data sections

type
  TNSIDataSectionInfo = packed record
    ItemCount:  UInt32; // number of taken item slots within this data section
    Flags:      UInt32; // unused atm
  end;

  TNSIDataSectionsArray = packed array[0..Pred(NSI_SHAREDMEMORY_INFOSECT_MAXCOUNT)] of TNSIDataSectionInfo;

  TNSIInfoSection = packed record
    Flags:        UInt32;
    Reserved:     array[0..27] of Byte;
    DataSections: TNSIDataSectionsArray;
  end;
  PNSIInfoSection = ^TNSIInfoSection;

const
  NSI_SHAREDMEMORY_INFOSECT_NAME = 'nsi_section_%s_%d_info';   // namespace, size
  NSI_SHAREDMEMORY_INFOSECT_SIZE = SizeOf(TNSIInfoSection);

  NSI_INFOSECT_FLAG_ACTIVE = UInt32($00000001);

{-------------------------------------------------------------------------------
    TNamedSharedItem - data section types and constants
-------------------------------------------------------------------------------}
type
  TNSIItemPayload = record end; // zero-size placeholder

  TNSIItemHeader = packed record
    RefCount: UInt32;               // reference counter
    Flags:    UInt32;               // currently unused
    Hash:     TSHA1;                // 20 bytes
    Reserved: array[0..3] of Byte;  // right now only alignment padding
    Payload:  TNSIItemPayload       // aligned to 32-byte boundary
  end;
  PNSIItemHeader = ^TNSIItemHeader;

const
  NSI_SHAREDMEMORY_DATASECT_ALIGNMENT   = 32;                     // 256 bits
  NSI_SHAREDMEMORY_DATASECT_MAXITEMSIZE = 1024;                   // 1KiB
  NSI_SHAREDMEMORY_DATASECT_SIZE        = 64 * 1024;              // 64KiB
  NSI_SHAREDMEMORY_DATASECT_NAME        = 'nsi_section_%s_%d_%d';  // namespace, size, index

{===============================================================================
    TNamedSharedItem - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TNamedSharedItem - protected methods
-------------------------------------------------------------------------------}

Function TNamedSharedItem.GetInfoSectionName: String;
begin
Result := Format(NSI_SHAREDMEMORY_INFOSECT_NAME,[fNameSpace,fSize])
end;

//------------------------------------------------------------------------------

Function TNamedSharedItem.GetDataSectionName(Index: Integer): String;
begin
Result := Format(NSI_SHAREDMEMORY_DATASECT_NAME,[fNameSpace,fSize,Index])
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.FindOrAllocateItem;
var
  InfoSectionPtr:       PNSIInfoSection;
  i,j:                  Integer;
  SectionFirstUnused:   Integer;
  SectionFirstFreeSlot: Integer;
  ProbedSection:        TSimpleSharedMemory;
  ProbedItem:           PNSIItemHeader;
begin
// info section should be already locked and prepared by this point
InfoSectionPtr := PNSIInfoSection(fInfoSection.Memory);
SectionFirstUnused := -1;
SectionFirstFreeSlot := -1;
{
  Traverse all sections and, in those containing at least one item, search for
  occurence of item with the same hash as has this item. Along the way, look
  for empty slots and unused sections.

  Since all data sections are traversed, if the item is not found, there should
  be at least one section unused and/or one with free slot. If not, it indicates
  that all resources (cca. 1GiB of memory) has been consumed, in that case just
  raise exception.
}
For i := Low(InfoSectionPtr^.DataSections) to High(InfoSectionPtr^.DataSections) do
  begin
    If InfoSectionPtr^.DataSections[i].ItemCount > 0 then
      begin
        // this section is used...
        If (InfoSectionPtr^.DataSections[i].ItemCount < fItemsPerSection) then
          // ...and is not full
          If SectionFirstFreeSlot < 0 then
            SectionFirstFreeSlot := i;
        // probe this section for this item
        ProbedSection := TSimpleSharedMemory.Create(NSI_SHAREDMEMORY_DATASECT_SIZE,GetDataSectionName(i));
        try
          ProbedItem := PNSIItemHeader(ProbedSection.Memory);
          For j := 1 to fItemsPerSection do
            begin
              If ProbedItem^.RefCount > 0 then
                If SameSHA1(ProbedItem^.Hash,fNameHash) then
                  begin
                    // item found
                    Inc(ProbedItem^.RefCount);
                    fCreated := False;
                    fDataSectionIndex := i;
                    fDataSection := ProbedSection;
                    fItemMemory := Pointer(ProbedItem);
                    Exit; // also breaks out of the for cycle
                  end;
              PtrAdvanceVar(Pointer(ProbedItem),fFullItemSize);
            end;
          FreeAndNil(ProbedSection);  // not called if the item was found
        except
          // in case something bad happens during probing
          FreeAndNil(ProbedSection);
          raise;
        end;
      end
    else
      begin
        // this section is unused
        If SectionFirstUnused < 0 then
          SectionFirstUnused := i;
      end;
  end;
{
  If here then the item was not found, add it as a new one.
  
  First try adding it into already used section, so we don't have to allocate
  new section.
}
If SectionFirstFreeSlot >= 0 then
  begin
    fDataSectionIndex := SectionFirstFreeSlot;
    ProbedSection := TSimpleSharedMemory.Create(NSI_SHAREDMEMORY_DATASECT_SIZE,GetDataSectionName(fDataSectionIndex));
    try
      ProbedItem := PNSIItemHeader(ProbedSection.Memory);
      For j := 1 to fItemsPerSection do
        begin
          If ProbedItem^.RefCount <= 0 then
            begin
              FillChar(ProbedItem^,fFullItemSize,0);
              ProbedItem^.RefCount := 1;
              ProbedItem^.Hash := fNameHash;
              fDataSection := ProbedSection;
              fItemMemory := Pointer(ProbedItem);
              Inc(InfoSectionPtr^.DataSections[fDataSectionIndex].ItemCount);
              Exit;
            end;
          PtrAdvanceVar(Pointer(ProbedItem),fFullItemSize);
        end;
      raise ENSIOutOfResources.CreateFmt('TNamedSharedItem.FindOrAllocateItem: No free item slot found in given section (%d).',[fDataSectionIndex]);
    except
      FreeAndNil(ProbedSection);
      raise;
    end;
  end
else If SectionFirstUnused >= 0 then
  begin
    fDataSectionIndex := SectionFirstUnused;
    fDataSection := TSimpleSharedMemory.Create(NSI_SHAREDMEMORY_DATASECT_SIZE,GetDataSectionName(fDataSectionIndex));
    try
      fItemMemory := fDataSection.Memory;
      PNSIItemHeader(fItemMemory)^.RefCount := 1;
      PNSIItemHeader(fItemMemory)^.Hash := fNameHash;
      InfoSectionPtr^.DataSections[fDataSectionIndex].ItemCount := 1;
      Exit;
    except
      FreeAndNil(fDataSection);
      raise;
    end;
  end;
raise ENSIOutOfResources.Create('TNamedSharedItem.FindOrAllocateItem: No free item slot found.');
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.AllocateItem(HoldLock: Boolean);
begin
// get info section, initialize it if necessary
fInfoSection := TSharedMemory.Create(NSI_SHAREDMEMORY_INFOSECT_SIZE,GetInfoSectionName);
fInfoSection.Lock;
try
  try
    If PNSIInfoSection(fInfoSection.Memory)^.Flags and NSI_INFOSECT_FLAG_ACTIVE = 0 then
      begin
        // section not initialized, initialize it
        PNSIInfoSection(fInfoSection.Memory)^.Flags := NSI_INFOSECT_FLAG_ACTIVE;
        FillChar(PNSIInfoSection(fInfoSection.Memory)^.DataSections,SizeOf(TNSIDataSectionsArray),0);
      end;
    FindOrAllocateItem;
    If (fDataSectionIndex < 0) or not Assigned(fDataSection) or not Assigned(fItemMemory) then
      raise ENSIItemAllocationError.Create('TNamedSharedItem.AllocateItem: No free item slot found.');
    fPayloadMemory := Addr(PNSIItemHeader(fItemMemory)^.Payload);
  except
    HoldLock := False;
    raise;
  end;
finally
  If not HoldLock then
    fInfoSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.DeallocateItem;
begin
If Assigned(fInfoSection) then
  begin
    fInfoSection.Lock;
    try
      If Assigned(fDataSection) then
        begin
          Dec(PNSIItemHeader(fItemMemory)^.RefCount);
          If PNSIItemHeader(fItemMemory)^.RefCount <= 0 then
            begin
              PNSIItemHeader(fItemMemory)^.RefCount := 0;
              Dec(PNSIInfoSection(fInfoSection.Memory)^.DataSections[fDataSectionIndex].ItemCount);
            end;
          FreeAndNil(fDataSection);
        end;
    finally
      fInfoSection.Unlock;
    end;
    FreeAndNil(fInfoSection);
  end;
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.Initialize(const Name: String; Size: TMemSize; const NameSpace: String; HoldLock: Boolean);
begin
fCreated := True;
If Length(NameSpace) <= 0 then
  fNameSpace := ''
else If Length(NameSpace) <= NSI_SHAREDMEMORY_NAMESPACE_MAXLEN then
  fNameSpace := NameSpace
else
  raise ENSIInvalidValue.Create('TNamedSharedItem.Initialize: Namespace string too long.');
fName := Name;
fNameHash := WideStringSHA1(StrToWide(fName));
If (Size > 0) and (Size <= NSI_SHAREDMEMORY_DATASECT_MAXITEMSIZE) then
  fSize := Size
else
  raise ENSIInvalidValue.CreateFmt('TNamedSharedItem.Initialize: Invalid item size (%d).',[Size]);
fInfoSection := nil;
fDataSectionIndex := -1;
fDataSection := nil;
fItemMemory := nil;
fPayloadMemory := nil;
{
  Get size of item with everything (header, padding, ...).

  It is part of section name and is also used for calculation of direct memory
  address of items within the shared memory section.
}
fFullItemSize := (TMemSize(SizeOf(TNSIItemHeader)) + fSize +
   TMemSize(Pred(NSI_SHAREDMEMORY_DATASECT_ALIGNMENT))) and not
  TMemSize(Pred(NSI_SHAREDMEMORY_DATASECT_ALIGNMENT));
{
  fItemsPerSection serves for comparison whether there is a free "slot" in
  given data section.
}
fItemsPerSection := NSI_SHAREDMEMORY_DATASECT_SIZE div fFullItemSize;
AllocateItem(HoldLock);
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.Finalize;
begin
DeallocateItem;
end;

{-------------------------------------------------------------------------------
    TNamedSharedItem - public methods
-------------------------------------------------------------------------------}

constructor TNamedSharedItem.Create(const Name: String; Size: TMemSize; const NameSpace: String = '');
begin
inherited Create;
Initialize(Name,Size,NameSpace,False);
end;

//------------------------------------------------------------------------------

constructor TNamedSharedItem.CreateLocked(const Name: String; Size: TMemSize; const NameSpace: String = ''{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
Initialize(Name,Size,NameSpace,True);
end;

//------------------------------------------------------------------------------

destructor TNamedSharedItem.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.GlobalLock;
begin
fInfoSection.Lock;
end;

//------------------------------------------------------------------------------

procedure TNamedSharedItem.GlobalUnlock;
begin
fInfoSection.Unlock;
end;

end.
