{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Memory vector

    This library provides base class for implementing memory vectors - that is,
    a contiguous memory containing an array of items.
    Most of the functinonality is the same as for normal dynamic arrays or
    lists, but it provides some more advanced features like item initialization
    and finalization and provides better memory locality (everything is
    together). The most important feature is, that the items are truly
    contiguous in the memory, there is no padding or other potential issues
    lists or arrays can have (memory fragmentation, problematic reallocation).

    Although the base class (TMemVector) can be used directly, it is intended
    to be inherited from in a descendant class that implements vector for a
    specific item type. An integer vector is implemented as an example.

  Version 1.2.3 (2023-11-05)

  Last change 2024-02-03

  ©2016-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MemVector

  Dependencies:
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    ListSorters         - github.com/TheLazyTomcat/Lib.ListSorters
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect

  BinaryStreamingLite can be replaced by full BinaryStreaming.

===============================================================================}
(*******************************************************************************

  Not implemented as a generic class mainly because of backward compatibility.
  To create a derived/specialized class from base class, replace @ClassName@
  with a class identifier and @Type@ with identifier of used type in the
  following template. Also remember to implement proper comparison function
  for a chosen type.
  Optional methods are not required to be implemented, but they might be usefull
  in some instances (eg. when item contains reference-counted types, pointers
  or object references).

==  Declaration  ===============================================================
--------------------------------------------------------------------------------

  @ClassName@ = class(TMemVector)
  protected
    Function GetItem(Index: Integer): @Type@; virtual;
    procedure SetItem(Index: Integer; Value: @Type@); virtual;
  //procedure ItemInit(Item: Pointer); override;
  //procedure ItemFinal(Item: Pointer); override;
  //procedure ItemCopy(SrcItem,DstItem: Pointer); override;           
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  //Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
  //procedure ItemWrite(Item: Pointer; Stream: TStream); override;
  //procedure ItemRead(Item: Pointer; Stream: TStream); override;
  public
    constructor Create; overload;
    constructor Create(Memory: Pointer; Count: Integer); overload;
    Function First: @Type@; reintroduce;
    Function Last: @Type@; reintroduce;
    Function IndexOf(Item: @Type@): Integer; reintroduce;
    Function Add(Item: @Type@): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: @Type@); reintroduce;
    Function Remove(Item: @Type@): Integer; reintroduce;
    Function Extract(Item: @Type@): @Type@; reintroduce;
    property Items[Index: Integer]: @Type@ read GetItem write SetItem; default;
  end;

==  Implementation  ============================================================
--------------------------------------------------------------------------------

Function @ClassName@.GetItem(Index: Integer): @Type@;
begin
Result := @Type@(GetItemPtr(Index)^);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.SetItem(Index: Integer; Value: @Type@);
begin
SetItemPtr(Index,@Value);
end;

//------------------------------------------------------------------------------

// Method called for each item that is implicitly (eg. when changing the Count
// property to a higher number) added to the vector.
// Item is filled with zeroes in default implementation.

//procedure @ClassName@.ItemInit(Item: Pointer);
//begin
//{$MESSAGE WARN 'Implement item initialization to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called for each item that is implicitly (e.g. when changing the Count
// property to a lower number) removed from the vector.
// No default behavior.

//procedure @ClassName@.ItemFinal(Item: Pointer);
//begin
//{$MESSAGE WARN 'Implement item finalization to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Called when an item is copied to the vector from an external source and
// ManagedCopy is set to true. Called only by methods that has parameter
// ManagedCopy.
// Item is copied without any further processing in default implementation.

//procedure @ClassName@.ItemCopy(SrcItem,DstItem: Pointer);
//begin
//{$MESSAGE WARN 'Implement item copy to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// This method is called when there is a need to compare two items, for example
// when sorting the vector.
// Must return positive number when Item1 is higher/larger than Item2, zero when
// they are equal and negative number when Item1 is lower/smaller than Item2.
// No default implementation.
// This method must be implemented in derived classes!

Function @ClassName@.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{$MESSAGE WARN 'Implement comparison to suit actual type.'}
end;

//------------------------------------------------------------------------------

// Called when two items are compared for equality (e.g. when searching for a
// particular item).
// In default implementation, it calls ItemCompare and when it returns zero,
// items are considered to be equal.

//Function @ClassName@.ItemEquals(Item1,Item2: Pointer): Boolean;
//begin
//{$MESSAGE WARN 'Implement equality comparison to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called for each item being written to the stream.
// Default implementation direcly writes ItemSize bytes from the item memory
// to the stream, with no further processing.

//procedure @ClassName@.ItemWrite(Item: Pointer; Stream: TStream);
//begin
//{$MESSAGE WARN 'Implement item write to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called for each item being read from the stream.
// Default implementation reads ItemSize bytes directly to the item memory with
// no further processing.

//procedure @ClassName@.ItemRead(Item: Pointer; Stream: TStream);
//begin
//{$MESSAGE WARN 'Implement item read to suit actual type.'}
//end;

//==============================================================================

constructor @ClassName@.Create;
begin
inherited Create(SizeOf(@Type@));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor @ClassName@.Create(Memory: Pointer; Count: Integer);
begin
inherited Create(Memory,Count,SizeOf(@Type@));
end;

//------------------------------------------------------------------------------

Function @ClassName@.First: @Type@;
begin
Result := @Type@(inherited First^);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Last: @Type@;
begin
Result := @Type@(inherited Last^);
end;

//------------------------------------------------------------------------------

Function @ClassName@.IndexOf(Item: @Type@): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Add(Item: @Type@): Integer;
begin
Result := inherited Add(@Item);
end;
  
//------------------------------------------------------------------------------

procedure @ClassName@.Insert(Index: Integer; Item: @Type@);
begin
inherited Insert(Index,@Item);
end;
 
//------------------------------------------------------------------------------

Function @ClassName@.Remove(Item: @Type@): Integer;
begin
Result := inherited Remove(@Item);
end;
 
//------------------------------------------------------------------------------

Function @ClassName@.Extract(Item: @Type@): @Type@;
var
  TempPtr:  Pointer;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := @Type@(TempPtr^)
else
  Result := {$MESSAGE WARN 'Set to some invalid value (eg. 0, nil, '''', ...).'};
end;

*******************************************************************************)
unit MemVector;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMVException = class(Exception);

  EMVIndexOutOfBounds  = class(EMVException);
  EMVForeignMemory     = class(EMVException);
  EMVInvalidValue      = class(EMVException);
  EMVIncompatibleClass = class(EMVException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TMemVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMemVector - class declaration
===============================================================================}
type
  TMemVector = class(TCustomListObject)
  protected
    fItemSize:          TMemSize;
    fOwnsMemory:        Boolean;
    fMemory:            Pointer;
    fCapacity:          Integer;
    fCount:             Integer;
    fUpdateCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    fTempItem:          Pointer;
    fLoading:           Boolean;
    // getters, setters
    Function GetItemPtr(Index: Integer): Pointer; virtual;
    procedure SetItemPtr(Index: Integer; Value: Pointer); virtual;
    Function GetSize: TMemSize; virtual;
    Function GetAllocatedSize: TMemSize; virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // item management
    procedure ItemInit(Item: Pointer); virtual;
    procedure ItemFinal(Item: Pointer); virtual;
    procedure ItemCopy(SrcItem,DstItem: Pointer); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; virtual;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; virtual;
    procedure ItemWrite(Item: Pointer; Stream: TStream); virtual;
    procedure ItemRead(Item: Pointer; Stream: TStream); virtual;
    // utility and macro methods
    Function CheckIndexAndRaise(Index: Integer; CallingMethod: String = 'CheckIndexAndRaise'): Boolean; virtual;
    Function GetNextItemPtr(ItemPtr: Pointer): Pointer; virtual;
    Function CompareItems(Index1,Index2: Integer): Integer; virtual;
    procedure FinalizeAllItems; virtual;
    procedure DoChange; virtual;
    procedure ReadFromStreamInternal(Stream: TStream); virtual;
  public
    constructor Create(ItemSize: TMemSize); overload;
    constructor Create(Memory: Pointer; Count: Integer; ItemSize: TMemSize); overload;
    destructor Destroy; override;
    // updates
    procedure BeginUpdate; virtual;
    Function EndUpdate: Integer; virtual;
    // first/last
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function First: Pointer; virtual;
    Function Last: Pointer; virtual;
    // list methods
    Function IndexOf(Item: Pointer): Integer; virtual;
    Function Add(Item: Pointer): Integer; virtual;
    procedure Insert(Index: Integer; Item: Pointer); virtual;
    procedure Move(SrcIndex,DstIndex: Integer); virtual;
    procedure Exchange(Index1,Index2: Integer); virtual;
    Function Extract(Item: Pointer): Pointer; virtual;  // does not call ItemFinal
    Function Remove(Item: Pointer): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    // list manipulation
    procedure Reverse; virtual;
    procedure Sort(Reversed: Boolean = False); virtual;
    // comparations
    Function IsEqual(Vector: TMemVector): Boolean; virtual;
    Function IsEqualBinary(Vector: TMemVector): Boolean; virtual;
    // list assigning
    procedure Assign(Data: Pointer; Count: Integer; ManagedCopy: Boolean = False); overload; virtual;
    procedure Assign(Vector: TMemVector; ManagedCopy: Boolean = False); overload; virtual;
    procedure Append(Data: Pointer; Count: Integer; ManagedCopy: Boolean = False); overload; virtual;
    procedure Append(Vector: TMemVector; ManagedCopy: Boolean = False); overload; virtual;
    // streaming
  {
    Write* methods write only the vector data, whereas Save* methods first
    write item count and then the data.

      NOTE - count is saved as a 32bit signed integer with little endianness.

    When calling Read* method, current count (property Count) of items is read.
    When Load* is called, the count is read first, the vector is reallocated to
    that count and then the data are read.
  }
    procedure WriteToStream(Stream: TStream); virtual;
    procedure ReadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure WriteToFile(const FileName: String); virtual;
    procedure ReadFromFile(const FileName: String); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    // properties
    property ItemSize: TMemSize read fItemSize;
    property OwnsMemory: Boolean read fOwnsMemory write fOwnsMemory;
    property Memory: Pointer read fMemory;
    property Size: TMemSize read GetSize;
    property AllocatedSize: TMemSize read GetAllocatedSize;
    property Pointers[Index: Integer]: Pointer read GetItemPtr;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TIntegerVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerVector - class declaration
===============================================================================}
type
  TIntegerVector = class(TMemVector)
  protected
    Function GetItem(Index: Integer): Integer; virtual;
    procedure SetItem(Index: Integer; Value: Integer); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
    procedure ItemWrite(Item: Pointer; Stream: TStream); override;
    procedure ItemRead(Item: Pointer; Stream: TStream); override;
  public
    constructor Create; overload;
    constructor Create(Memory: Pointer; Count: Integer); overload;
    Function First: Integer; reintroduce;
    Function Last: Integer; reintroduce;
    Function IndexOf(Item: Integer): Integer; reintroduce;
    Function Add(Item: Integer): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    Function Remove(Item: Integer): Integer; reintroduce;
    Function Extract(Item: Integer): Integer; reintroduce;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

implementation

uses
  StrRect, ListSorters, BinaryStreamingLite;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TMemVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMemVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMemVector - protected methods
-------------------------------------------------------------------------------}

Function TMemVector.GetItemPtr(Index: Integer): Pointer;
begin
If CheckIndexAndRaise(Index,'GetItemPtr') then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := Pointer(PtrUInt(fMemory) + PtrUInt(TMemSize(Index) * fItemSize))
else
  Result := nil;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TMemVector.SetItemPtr(Index: Integer; Value: Pointer);
var
  ItemPtr:  Pointer;
begin
If CheckIndexAndRaise(Index,'SetItemPtr') then
  begin
    ItemPtr := GetItemPtr(Index);
    System.Move(ItemPtr^,fTempItem^,fItemSize);
    System.Move(Value^,ItemPtr^,fItemSize);
    If not ItemEquals(fTempItem,Value) then
      DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TMemVector.GetSize: TMemSize;
begin
Result := TMemSize(fCount) * fItemSize;
end;

//------------------------------------------------------------------------------

Function TMemVector.GetAllocatedSize: TMemSize;
begin
Result := TMemSize(fCapacity) * fItemSize;
end;

//------------------------------------------------------------------------------

Function TmemVector.GetCapacity: Integer;
begin
Result := fCapacity;
end;

//------------------------------------------------------------------------------

procedure TMemVector.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If fOwnsMemory then
  begin
    If (Value <> fCapacity) and (Value >= 0) then
      begin
        If (Value < fCount) and not fLoading then
          For i := Value to HighIndex do
            ItemFinal(GetItemPtr(i));
        If fCount <= 0 then
          begin
            // there is no item, so we do not need to copy existing data
            FreeMem(fMemory,TMemSize(fCapacity) * fItemSize);
            fMemory := AllocMem(TMemSize(Value) * fItemSize);
          end
        else ReallocMem(fMemory,TMemSize(Value) * fItemSize);
        fCapacity := Value;
        If Value < fCount then
          begin
            fCount := Value;
            DoChange;
          end;
      end;
  end
else raise EMVForeignMemory.Create('TMemVector.SetCapacity: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

Function TmemVector.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

procedure TMemVector.SetCount(Value: Integer);
var
  OldCount: Integer;
  i:        Integer;
begin
If fOwnsMemory then
  begin
    If (Value <> fCount) and (Value >= 0) then
      begin
        BeginUpdate; 
        try
          If Value > fCapacity then
            SetCapacity(Value);
          If Value > fCount then
            begin
              OldCount := fCount;
              fCount := Value;
              If not fLoading then
                For i := OldCount to HighIndex do
                  ItemInit(GetItemPtr(i));
            end
          else
            begin
              If not fLoading then
                For i := HighIndex downto Value do
                  ItemFinal(GetItemPtr(i));
              fCount := Value;
            end;
          DoChange;
        finally
          EndUpdate;
        end;
      end;
  end
else raise EMVForeignMemory.Create('TMemVector.SetCount: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.ItemInit(Item: Pointer);
begin
FillChar(Item^,fItemSize,0);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMemVector.ItemFinal(Item: Pointer);
begin
// nothing to do here
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMemVector.ItemCopy(SrcItem,DstItem: Pointer);
begin
System.Move(SrcItem^,DstItem^,fItemSize);
end;

//------------------------------------------------------------------------------

Function TMemVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If PtrUInt(Item1) < PtrUInt(Item2) then
  Result := -1
else If PtrUInt(Item1) > PtrUInt(Item2) then
  Result := +1
else
  Result := 0;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMemVector.ItemEquals(Item1,Item2: Pointer): Boolean;
begin
Result := ItemCompare(Item1,Item2) = 0;
end;

//------------------------------------------------------------------------------

procedure TMemVector.ItemWrite(Item: Pointer; Stream: TStream);
begin
Stream.WriteBuffer(Item^,fItemSize);
end;

//------------------------------------------------------------------------------

procedure TMemVector.ItemRead(Item: Pointer; Stream: TStream);
begin
Stream.ReadBuffer(Item^,fItemSize);
end;

//------------------------------------------------------------------------------

Function TMemVector.CheckIndexAndRaise(Index: Integer; CallingMethod: String = 'CheckIndexAndRaise'): Boolean;
begin
Result := CheckIndex(Index);
If not Result then
  raise EMVIndexOutOfBounds.CreateFmt('TMemVector.%s: Index (%d) out of bounds.',[CallingMethod,Index]);
end;

//------------------------------------------------------------------------------

Function TMemVector.GetNextItemPtr(ItemPtr: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(ItemPtr) + PtrUInt(fItemSize));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMemVector.CompareItems(Index1,Index2: Integer): Integer;
begin
Result := ItemCompare(GetItemPtr(Index1),GetItemPtr(Index2));
end;

//------------------------------------------------------------------------------

procedure TMemVector.FinalizeAllItems;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  ItemFinal(GetItemPtr(i));
end;

//------------------------------------------------------------------------------

procedure TMemVector.DoChange;
begin
fChanged := True;
If (fUpdateCounter <= 0) then
  begin
    If Assigned(fOnChangeEvent) then
      fOnChangeEvent(Self)
    else If Assigned(fOnChangeCallback) then
      fOnChangeCallback(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.ReadFromStreamInternal(Stream: TStream);
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  ItemRead(GetItemPtr(i),Stream);
DoChange;
end;

{-------------------------------------------------------------------------------
    TMemVector - public methods
-------------------------------------------------------------------------------}

constructor TMemVector.Create(ItemSize: TMemSize);
begin
inherited Create;
If ItemSize > 0 then
  begin
    fItemSize := ItemSize;
    fOwnsMemory := True;
    fMemory := nil;
    fCapacity := 0;
    fCount := 0;
    fUpdateCounter := 0;
    fChanged := False;
    fOnChangeEvent := nil;
    fOnChangeCallback := nil;
    GetMem(fTempItem,fItemSize);
    fLoading := False;
  end
else raise EMVInvalidValue.CreateFmt('TMemVector.Create: Invalid item size (%d).',[ItemSize]);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMemVector.Create(Memory: Pointer; Count: Integer; ItemSize: TMemSize);
begin
Create(ItemSize);
If Assigned(Memory) then
  begin
    If Count >= 0 then
      begin
        fOwnsMemory := False;
        fMemory := Memory;
        fCapacity := Count;
        fCount := Count;
      end
    else raise EMVInvalidValue.CreateFmt('TMemVector.Create: Invalid item count (%d).',[Count]);
  end
else raise EMVInvalidValue.Create('TMemVector.Create: Nil memory not allowed.');
end;

//------------------------------------------------------------------------------

destructor TMemVector.Destroy;
begin
FinalizeAllItems;
FreeMem(fTempItem,fItemSize);
If fOwnsMemory then
  FreeMem(fMemory,TMemSize(fCapacity) * fItemSize);
inherited;
end;

//------------------------------------------------------------------------------

procedure TMemVector.BeginUpdate;
begin
If fUpdateCounter <= 0 then
  fChanged := False;
Inc(fUpdateCounter);
end;

//------------------------------------------------------------------------------

Function TMemVector.EndUpdate: Integer;
begin
Dec(fUpdateCounter);
If fUpdateCounter <= 0 then
  begin
    fUpdateCounter := 0;
    If fChanged then
      DoChange;
    fChanged := False;
  end;
Result := fUpdateCounter;
end;

//------------------------------------------------------------------------------

Function TMemVector.LowIndex: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TMemVector.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TMemVector.First: Pointer;
begin
Result := GetItemPtr(LowIndex);
end;

//------------------------------------------------------------------------------

Function TMemVector.Last: Pointer;
begin
Result := GetItemPtr(HighIndex);
end;

//------------------------------------------------------------------------------

Function TMemVector.IndexOf(Item: Pointer): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If ItemEquals(Item,GetItemPtr(i)) then
    begin
      Result := i;
      Exit;
    end;
end;
 
//------------------------------------------------------------------------------

Function TMemVector.Add(Item: Pointer): Integer;
begin
If fOwnsMemory then
  begin
    Grow;
    Result := fCount;
    Inc(fCount);    
    System.Move(Item^,GetItemPtr(Result)^,fItemSize);
    DoChange;
  end
else raise EMVForeignMemory.Create('TMemVector.Add: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Insert(Index: Integer; Item: Pointer);
var
  InsertPtr:  Pointer;
begin
If fOwnsMemory then
  begin
    If CheckIndex(Index) then
      begin
        Grow;
        InsertPtr := GetItemPtr(Index);
        System.Move(InsertPtr^,GetNextItemPtr(InsertPtr)^,fItemSize * TMemSize(fCount - Index));
        System.Move(Item^,InsertPtr^,fItemSize);
        Inc(fCount);
        DoChange;
      end
    else If Index = fCount then
      Add(Item)
    else
      raise EMVIndexOutOfBounds.CreateFmt('TMemVector.Insert: Index (%d) out of bounds.',[Index]);
  end
else raise EMVForeignMemory.Create('TMemVector.Insert: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Move(SrcIndex,DstIndex: Integer);
var
  SrcPtr: Pointer;
  DstPtr: Pointer;
begin
If CheckIndexAndRaise(SrcIndex,'Move') and CheckIndexAndRaise(DstIndex,'Move') then
  If SrcIndex <> DstIndex then
    begin
      SrcPtr := GetItemPtr(SrcIndex);
      DstPtr := GetItemPtr(DstIndex);
      System.Move(SrcPtr^,fTempItem^,fItemSize);
      If SrcIndex < DstIndex then
        System.Move(GetNextItemPtr(SrcPtr)^,SrcPtr^,fItemSize * TMemSize(DstIndex - SrcIndex))
      else
        System.Move(DstPtr^,GetNextItemPtr(DstPtr)^,fItemSize * TMemSize(SrcIndex - DstIndex));
      System.Move(fTempItem^,DstPtr^,fItemSize);
      DoChange;
    end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.Exchange(Index1,Index2: Integer);
var
  Idx1Ptr:  Pointer;
  Idx2Ptr:  Pointer;
begin
If CheckIndexAndRaise(Index1,'Exchange') and CheckIndexAndRaise(Index2,'Exchange') then
  If Index1 <> Index2 then
    begin
      Idx1Ptr := GetItemPtr(Index1);
      Idx2Ptr := GetItemPtr(Index2);
      System.Move(Idx1Ptr^,fTempItem^,fItemSize);
      System.Move(Idx2Ptr^,Idx1Ptr^,fItemSize);
      System.Move(fTempItem^,Idx2Ptr^,fItemSize);
      DoChange;
    end;
end;

//------------------------------------------------------------------------------

Function TMemVector.Extract(Item: Pointer): Pointer;
var
  Index:    Integer;
  ItemPtr:  Pointer;
begin
If fOwnsMemory then
  begin
    Index := IndexOf(Item);
    If CheckIndex(Index) then
      begin
        ItemPtr := GetItemPtr(Index);
        // move the item to temp so it is valid after its removal from the vector
        System.Move(ItemPtr^,fTempItem^,fItemSize);
        Result := fTempItem;
        // delete the item
        If Index < HighIndex then
          System.Move(GetNextItemPtr(ItemPtr)^,ItemPtr^,fItemSize * TMemSize(Pred(fCount - Index)));
        Dec(fCount);
        Shrink;
        DoChange;
      end
    else Result := nil;
  end
else raise EMVForeignMemory.Create('TMemVector.Extract: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

Function TMemVector.Remove(Item: Pointer): Integer;
begin
If fOwnsMemory then
  begin
    Result := IndexOf(Item);
    If CheckIndex(Result) then
      Delete(Result);
  end
else raise EMVForeignMemory.Create('TMemVector.Remove: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Delete(Index: Integer);
var
  DeletePtr: Pointer;
begin
If fOwnsMemory then
  begin
    If CheckIndexAndRaise(Index,'Delete') then
      begin
        DeletePtr := GetItemPtr(Index);
        ItemFinal(DeletePtr);
        If Index < HighIndex then
          System.Move(GetNextItemPtr(DeletePtr)^,DeletePtr^,fItemSize * TMemSize(Pred(fCount - Index)));
        Dec(fCount);
        Shrink;
        DoChange;
      end;
  end
else raise EMVForeignMemory.Create('TMemVector.Delete: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Clear;
var
  OldCount: Integer;
begin
If fOwnsMemory then
  begin
    OldCount := fCount;
    FinalizeAllItems;
    fCount := 0;
    Shrink;
    If OldCount > 0 then
      DoChange;
  end
else raise EMVForeignMemory.Create('TMemVector.Clear: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Reverse;
var
  i:  Integer;
begin
If fCount > 1 then
  begin
    BeginUpdate;
    try
      For i := LowIndex to Pred(fCount shr 1) do
        Exchange(i,Pred(fCount - i));
      DoChange;
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.Sort(Reversed: Boolean = False);
var
  Sorter: TListQuickSorter;
begin
If fCount > 1 then
  begin
    BeginUpdate;
    try
      Sorter := TListQuickSorter.Create(CompareItems,Exchange);
      try
        Sorter.Reversed := Reversed;
        Sorter.Stabilized := True;
        Sorter.Sort(LowIndex,HighIndex);
      finally
        Sorter.Free;
      end;
      DoChange;
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

Function TMemVector.IsEqual(Vector: TMemVector): Boolean;
var
  i:  Integer;
begin
Result := False;
If Vector is Self.ClassType then
  begin
    If Vector.Count = fCount then
      begin
        For i := LowIndex to HighIndex do
          If not ItemEquals(GetItemPtr(i),Vector.Pointers[i]) then
            Exit;
        Result := True;  
      end;
  end
else raise EMVIncompatibleClass.CreateFmt('TMemVector.IsEqual: Vector is of incompatible class (%s).',[Vector.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMemVector.IsEqualBinary(Vector: TMemVector): Boolean;
var
  i:  PtrUInt;
begin
Result := False;
If Size = Vector.Size then
  begin
    If Size > 0 then
      For i := 0 to Pred(Size) do
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        If PByte(PtrUInt(fMemory) + i)^ <> PByte(PtrUInt(Vector.Memory) + i)^ then
          Exit;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.Assign(Data: Pointer; Count: Integer; ManagedCopy: Boolean = False);
var
  i:  Integer;
begin
If fOwnsMemory then
  begin
    BeginUpdate;
    try
      FinalizeAllItems;
      fCount := 0;
      If Count > fCapacity then
        SetCapacity(Count);
      fCount := Count;
      If ManagedCopy then
        For i := 0 to Pred(Count) do
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          ItemCopy(Pointer(PtrUInt(Data) + PtrUInt(TMemSize(i) * fItemSize)),GetItemPtr(i))
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
      else
        If Count > 0 then
          System.Move(Data^,fMemory^,TMemSize(Count) * fItemSize);
      DoChange;
    finally
      EndUpdate;
    end;
  end
else raise EMVForeignMemory.Create('TMemVector.Assign: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Assign(Vector: TMemVector; ManagedCopy: Boolean = False);
begin
If fOwnsMemory then
  begin
    If Vector is Self.ClassType then
      Assign(Vector.Memory,Vector.Count,ManagedCopy)
    else
      raise EMVIncompatibleClass.CreateFmt('TMemVector.Assign: Vector is of incompatible class (%s).',[Vector.ClassName]);
  end
else raise EMVForeignMemory.Create('TMemVector.Assign: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Append(Data: Pointer; Count: Integer; ManagedCopy: Boolean = False);
var
  i:  Integer;
begin
If fOwnsMemory then
  begin
    BeginUpdate;
    try
      If (fCount + Count) > fCapacity then
        SetCapacity(fCount + Count);
      fCount := fCount + Count;
      If ManagedCopy then
        For i := 0 to Pred(Count) do
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          ItemCopy(Pointer(PtrUInt(Data) + PtrUInt(TMemSize(i) * fItemSize)),GetItemPtr((fCount - Count) + i))
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
      else
        If Count > 0 then
          System.Move(Data^,GetItemPtr(fCount - Count)^,TMemSize(Count) * fItemSize);
      DoChange;
    finally
      EndUpdate;
    end;
  end
else raise EMVForeignMemory.Create('TMemVector.Append: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.Append(Vector: TMemVector; ManagedCopy: Boolean = False);
begin
If fOwnsMemory then
  begin
    If Vector is Self.ClassType then
      Append(Vector.Memory,Vector.Count,ManagedCopy)
    else
      raise EMVIncompatibleClass.CreateFmt('TMemVector.Append: Vector is of incompatible class (%s).',[Vector.ClassName]);
  end
else raise EMVForeignMemory.Create('TMemVector.Assign: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.WriteToStream(Stream: TStream);
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  ItemWrite(GetItemPtr(i),Stream);
end;

//------------------------------------------------------------------------------

procedure TMemVector.ReadFromStream(Stream: TStream);
begin
If fOwnsMemory then
  begin
    BeginUpdate;
    try
      FinalizeAllItems;
      ReadFromStreamInternal(Stream);
    finally
      EndUpdate;
    end;
  end
else raise EMVForeignMemory.Create('TMemVector.ReadFromStream: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.SaveToStream(Stream: TStream);
begin
Stream_WriteInt32(Stream,Int32(fCount));
WriteToStream(Stream);
end;

//------------------------------------------------------------------------------

procedure TMemVector.LoadFromStream(Stream: TStream);
begin
If fOwnsMemory then
  begin
    BeginUpdate;
    try
      FinalizeAllItems;
    {
      Following deactivates calls to ItemFinal and ItemInit for the following
      setup of Count. Item finaliyation was done in previous step, and there
      is no need to initialize new items as thez will be rewritten.
    }
      fLoading := True;
      try
        Count := Integer(Stream_GetInt32(Stream));
      finally
        fLoading := False;
      end;
      ReadFromStreamInternal(Stream);
    finally
      EndUpdate;
    end;
  end
else raise EMVForeignMemory.Create('TMemVector.LoadFromStream: Operation not alloved on foreign memory.');
end;

//------------------------------------------------------------------------------

procedure TMemVector.WriteToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareExclusive);
try
  FileStream.Seek(0,soBeginning);
  WriteToStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.ReadFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  ReadFromStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.SaveToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareExclusive);
try
  FileStream.Seek(0,soBeginning);
  SaveToStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TMemVector.LoadFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  LoadFromStream(FileStream);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TIntegerVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerVector - class implementation
===============================================================================} 
{-------------------------------------------------------------------------------
    TIntegerVector - private methods
-------------------------------------------------------------------------------}

Function TIntegerVector.GetItem(Index: Integer): Integer;
begin
Result := Integer(GetItemPtr(Index)^);
end;

//------------------------------------------------------------------------------

procedure TIntegerVector.SetItem(Index: Integer; Value: Integer);
begin
SetItemPtr(Index,@Value);
end;

//------------------------------------------------------------------------------

Function TIntegerVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
Result := Integer(Item1^) - Integer(Item2^);
end;

//------------------------------------------------------------------------------

procedure TIntegerVector.ItemWrite(Item: Pointer; Stream: TStream);
begin
Stream_WriteInt32(Stream,Int32(Integer(Item^)));
end;

//------------------------------------------------------------------------------

procedure TIntegerVector.ItemRead(Item: Pointer; Stream: TStream);
begin
Integer(Item^) := Integer(Stream_GetInt32(Stream));
end;

{-------------------------------------------------------------------------------
    TIntegerVector - public methods
-------------------------------------------------------------------------------}

constructor TIntegerVector.Create;
begin
inherited Create(SizeOf(Integer));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TIntegerVector.Create(Memory: Pointer; Count: Integer);
begin
inherited Create(Memory,Count,SizeOf(Integer));
end;

//------------------------------------------------------------------------------

Function TIntegerVector.First: Integer;
begin
Result := Integer(inherited First^);
end;

//------------------------------------------------------------------------------

Function TIntegerVector.Last: Integer;
begin
Result := Integer(inherited Last^);
end;

//------------------------------------------------------------------------------

Function TIntegerVector.IndexOf(Item: Integer): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerVector.Add(Item: Integer): Integer;
begin
Result := inherited Add(@Item);
end;
  
//------------------------------------------------------------------------------

procedure TIntegerVector.Insert(Index: Integer; Item: Integer);
begin
inherited Insert(Index,@Item);
end;
 
//------------------------------------------------------------------------------

Function TIntegerVector.Remove(Item: Integer): Integer;
begin
Result := inherited Remove(@Item);
end;
 
//------------------------------------------------------------------------------

Function TIntegerVector.Extract(Item: Integer): Integer;
var
  TempPtr:  Pointer;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := Integer(TempPtr^)
else
  Result := 0;
end;

end.
