{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Linked List Array

    This unit provides base class (TLinkedListArray) for implementing a simple
    doubly-linked list which is completely stored inside an array.

    TLinkedListArray class should not be directly instantiated. Instead,
    a derived class, which will specialize this base class for a selected type,
    should be created.

    A specialized class (TIntegerLinkedListArray) with Integer item type is
    implemented and provided as an example.

  Version 1.0.2 (2023-01-25)

  Last change 2023-09-04

  ©2018-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.LinkedListArray

  Dependencies:
    AuxClasses      - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes        - github.com/TheLazyTomcat/Lib.AuxTypes
    BinaryStreaming - github.com/TheLazyTomcat/Lib.BinaryStreaming
    ListSorters     - github.com/TheLazyTomcat/Lib.ListSorters
    StrRect         - github.com/TheLazyTomcat/Lib.StrRect

===============================================================================}
(*******************************************************************************

  Not implemented as a generic class because of backward compatibility with
  older Delphi.
  To create a derived/specialized class from base class, use following template
  and replace @ClassName@ with a class identifier and @Type@ with identifier of
  used type. Also remember to implement proper comparison function for a chosen
  type.
  Optional methods are not required to be implemented, but they might be usefull
  in some instances (eg. when item contains reference-counted types, pointers
  or object references).

==  Declaration template  ======================================================
--------------------------------------------------------------------------------

  @ClassName@ = class(TLinkedListArray)
  protected
    Function GetItem(ListIndex: TLLAListIndex): @Type@; virtual;
    procedure SetItem(ListIndex: TLLAListIndex; Value: @Type@); virtual;
  //procedure PayloadInit(Payload: PLLAPayload); override;
  //procedure PayloadAdded(Payload: PLLAPayload); override;
  //procedure PayloadFinal(Payload: PLLAPayload); override;
  //procedure PayloadCopy(SrcPayload,DstPayload: PLLAPayload); override;
    Function PayloadCompare(Payload1,Payload2: PLLAPayload): Integer; override;
  //Function PayloadEquals(Payload1,Payload2: PLLAPayload): Boolean; override;
  //procedure PayloadWrite(Payload: PLLAPayload; Stream: TStream); override;
  //procedure PayloadRead(Payload: PLLAPayload; Stream: TStream); override;
  public
    constructor Create;
    Function First: @Type@; reintroduce;
    Function Last: @Type@; reintroduce;
    Function IndicesOf(Item: @Type@; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean; reintroduce;
    Function ArrayIndexOf(Item: @Type@): TLLAArrayIndex; reintroduce;
    Function ListIndexOf(Item: @Type@): TLLAListIndex; reintroduce;
    Function Add(Item: @Type@; out ArrayIndex: TLLAArrayIndex): TLLAListIndex; reintroduce; overload;
    Function Add(Item: @Type@): TLLAListIndex; reintroduce; overload;
    procedure Insert(ListIndex: TLLAListIndex; Item: @Type@); reintroduce;
    Function Extract(Item: @Type@): @Type@; reintroduce;
    Function Remove(Item: @Type@): Integer; reintroduce;
    property Items[ListIndex: TLLAListIndex]: @Type@ read GetItem write SetItem; default;
  end;

==  Implementation template ====================================================
--------------------------------------------------------------------------------

Function @ClassName@.GetItem(ListIndex: TLLAListIndex): @Type@;
begin
Result := @Type@(Pointer(GetPayloadPtrListIndex(ListIndex))^);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.SetItem(ListIndex: TLLAListIndex; Value: @Type@);
begin
SetPayloadPtrListIndex(ListIndex,@Value);
end;

//------------------------------------------------------------------------------

// Method called for each payload that is implicitly (eg. when changing the
// Count property to a higher number) added to the list. It is NOT called when
// payload is added explicitly (e.g. using method Add).
// Payload is filled with zeroes in default implementation.

//procedure @ClassName@.PayloadInit(Payload: PLLAPayload);
//begin
//{$MESSAGE WARN 'Implement payload initialization to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called when a payload is explicitly (ie. using methods Add, Insert,
// LoadFromStream or LoadFromFile) added to the list.
// Nothing is done in default implementation.

//procedure @ClassName@.PayloadAdded(Payload: PLLAPayload);
//begin
//{$MESSAGE WARN 'Implement payload addition to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called for each payload that is implicitly (e.g. when changing the
// Count property to a lower number) or explicitly (e.g. using method Delete)
// removed from the list.
// Nothing is done with the payload in default behavior.

//procedure @ClassName@.PayloadFinal(Payload: PLLAPayload);
//begin
//{$MESSAGE WARN 'Implement payload finalization to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Called when an payload is copied to the list from an external source.
// Can be used for example to creaty copies of objects instead of just copying
// their instance reference.
// Payload is copied without any further processing in default implementation.

//procedure @ClassName@.PayloadCopy(SrcPayload,DstPayload: PLLAPayload);
//begin
//{$MESSAGE WARN 'Implement payload copy to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// This method is called when there is a need to compare two payloads, for
// example when sorting the list.
// Must return positive number when Payload1 is higher/larger than Payload2
// (ie. they are in wrong order), zero when they are equal and negative number
// when Payload1 is lower/smaller than Payload2 (when they are in correct order).
// Has no default implementation.
// This method must be implemented in derived classes!

Function @ClassName@.PayloadCompare(Payload1,Payload2: PLLAPayload): Integer;
begin
{$MESSAGE WARN 'Implement comparison to suit actual type.'}
end;

//------------------------------------------------------------------------------

// Called when two payloads are compared for equality (e.g. when searching for a
// particular item).
// In default implementation, it calls PayloadCompare and when it returns zero,
// payloads are considered to be equal.
  
//Function @ClassName@.PayloadEquals(Payload1,Payload2: PLLAPayload): Boolean;
//begin
//{$MESSAGE WARN 'Implement equality comparison to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Called for each item being written to the stream.
// Default implementation direcly writes PayloadSize bytes from the payload
// memory to the stream, with no further processing.

//procedure @ClassName@.PayloadWrite(Payload: PLLAPayload; Stream: TStream);
//begin
//{$MESSAGE WARN 'Implement payload write to suit actual type.'}
//end;

//------------------------------------------------------------------------------

// Method called for each item being read from the stream.
// Default implementation reads PayloadSize bytes directly to the payload
// memory with no further processing.

//procedure @ClassName@.PayloadRead(Payload: PLLAPayload; Stream: TStream);
//begin
//{$MESSAGE WARN 'Implement payload read to suit actual type.'}
//end;

//==============================================================================

constructor @ClassName@.Create;
begin
inherited Create(SizeOf(@Type@));
end;

//------------------------------------------------------------------------------

Function @ClassName@.First: @Type@;
begin
Result := @Type@(Pointer(inherited First)^);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Last: @Type@;
begin
Result := @Type@(Pointer(inherited Last)^);
end;

//------------------------------------------------------------------------------

Function @ClassName@.IndicesOf(Item: @Type@; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean;
begin
Result := inherited IndicesOf(@Item,ArrayIndex,ListIndex);
end;

//------------------------------------------------------------------------------

Function @ClassName@.ArrayIndexOf(Item: @Type@): TLLAArrayIndex;
begin
Result := inherited ArrayIndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.ListIndexOf(Item: @Type@): TLLAListIndex;
begin
Result := inherited ListIndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Add(Item: @Type@; out ArrayIndex: TLLAArrayIndex): TLLAListIndex;
begin
Result := inherited Add(@Item,ArrayIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function @ClassName@.Add(Item: @Type@): TLLAListIndex;
begin
Result := inherited Add(@Item);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.Insert(ListIndex: TLLAListIndex; Item: @Type@);
begin
inherited Insert(ListIndex,@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Extract(Item: @Type@): @Type@;
var
  TempPtr:  PLLAPayload;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := @Type@(Pointer(TempPtr)^)
else
  Result := {$MESSAGE WARN 'Set to some invalid value (eg. 0, nil, '''', ...).'};
end;

//------------------------------------------------------------------------------

Function @ClassName@.Remove(Item: @Type@): TLLAListIndex;
begin
Result := inherited Remove(@Item);
end;

*******************************************************************************)
unit LinkedListArray;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH CLASSICPROCVARS+}
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
  ELLAException = class(Exception);

  ELLAIncompatibleClass = class(ELLAException);

  ELLAIndexError            = class(ELLAException);
  ELLAArrayIndexOutOfBounds = class(ELLAIndexError);
  ELLAListIndexOutOfBounds  = class(ELLAIndexError);

{===============================================================================
--------------------------------------------------------------------------------
                                TLinkedListArray
--------------------------------------------------------------------------------
===============================================================================}
type
  TLLAListIndex  = Integer;
  TLLAArrayIndex = Integer;

  TLLAFlags = Integer;

  TLLAPayload = record end;
  PLLAPayload = ^TLLAPayload;

  TLLAItem = record
    Prev:       TLLAArrayIndex;
    Next:       TLLAArrayIndex;
    Flags:      Integer;
  {$IF SizeOf(Integer) = 4}
    Padding:    Integer;
  {$IFEND}
    Payload:    TLLAPayload;  // zero-size placeholder 
  end;
  PLLAItem = ^TLLAItem;

{===============================================================================
    TLinkedListArray - class declaration
===============================================================================}
type
  TLinkedListArray = class(TCustomListObject)
  protected
    fPayloadSize:       TMemSize;
    fPayloadOffset:     PtrUInt;
    fItemSize:          TMemSize;
    fMemory:            Pointer;
    fCapacity:          Integer;
    fCount:             Integer;
    fUpdateCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    fTempPayload:       Pointer;
    fFirstFree:         TLLAArrayIndex;
    fLastFree:          TLLAArrayIndex;
    fFirstUsed:         TLLAArrayIndex;
    fLastUsed:          TLLAArrayIndex;
    fLoading:           Boolean;
    // item flags management
    class Function GetItemFlagValue(const Item: TLLAItem; Flag: Integer): Boolean; virtual;
    class Function SetItemFlagValue(var Item: TLLAItem; Flag: Integer; NewValue: Boolean): Boolean; virtual;
    // pointer conversion
    Function PayloadPtrFromItemPtr(ItemPtr: PLLAItem): PLLAPayload; virtual;
    Function ItemPtrFromPayloadPtr(PayloadPtr: PLLAPayload): PLLAItem; virtual;
    // getters, setters
    Function GetItemPtr(ArrayIndex: TLLAArrayIndex): PLLAItem; virtual;
    Function GetPayloadPtrArrayIndex(ArrayIndex: TLLAArrayIndex): PLLAPayload; virtual;
    procedure SetPayloadPtrArrayIndex(ArrayIndex: TLLAArrayIndex; Value: PLLAPayload); virtual;
    Function GetPayloadPtrListIndex(ListIndex: TLLAListIndex): PLLAPayload; virtual;
    procedure SetPayloadPtrListIndex(ListIndex: TLLAListIndex; Value: PLLAPayload); virtual;
    // custom list overrides
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // indices checking
    Function CheckArrayIndex(ArrayIndex: TLLAArrayIndex): Boolean; virtual;
    Function CheckListIndex(ListIndex: TLLAListIndex): Boolean; virtual;
    Function CheckArrayIndexAndRaise(ArrayIndex: TLLAArrayIndex; CallingMethod: String = 'CheckArrayIndexAndRaise'): Boolean; virtual;
    Function CheckListIndexAndRaise(ListIndex: TLLAListIndex; CallingMethod: String = 'CheckListIndexAndRaise'): Boolean; virtual;
    // payload management
    procedure PayloadInit(Payload: PLLAPayload); virtual;
    procedure PayloadAdded(Payload: PLLAPayload); virtual;
    procedure PayloadFinal(Payload: PLLAPayload); virtual;
    procedure PayloadCopy(SrcPayload,DstPayload: PLLAPayload); virtual;
    Function PayloadCompare(Payload1,Payload2: PLLAPayload): Integer; virtual;
    Function PayloadEquals(Payload1,Payload2: PLLAPayload): Boolean; virtual;
    procedure PayloadWrite(Payload: PLLAPayload; Stream: TStream); virtual;
    procedure PayloadRead(Payload: PLLAPayload; Stream: TStream); virtual;
    // sorting and defragmentation utilities
    Function SortCompare(ListIndex1,ListIndex2: Integer): Integer; virtual;
    procedure SortExchange(ListIndex1,ListIndex2: Integer); virtual;
    Function DefragCompare(Index1,Index2: Integer): Integer; virtual;
    procedure DefragExchange(Index1,Index2: Integer); virtual;
    // general utility and macro methods
    procedure DoChange; virtual;    
    procedure Decouple(ArrayIndex: TLLAArrayIndex); virtual;
    procedure InternalDelete(ArrayIndex: TLLAArrayIndex); virtual;
    procedure ArrayIndices(ListIndex1,ListIndex2: TLLAListIndex; out ArrayIndex1,ArrayIndex2: TLLAArrayIndex); virtual;
    procedure FinalizeAllItems; virtual;
    procedure ReadFromStreamInternal(Stream: TStream; Buffered: Boolean); virtual;
  public
    constructor Create(PayloadSize: TMemSize);
    destructor Destroy; override;
    // updates
    procedure BeginUpdate; virtual;
    Function EndUpdate: Integer; virtual;
    // indices bounds
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function LowArrayIndex: TLLAArrayIndex; virtual;
    Function HighArrayIndex: TLLAArrayIndex; virtual;
    Function LowListIndex: TLLAListIndex; virtual;
    Function HighListIndex: TLLAListIndex; virtual;
  {
    Following will return an array index of previous or next item, where item
    can be selected either by its array or list index. If such item does not
    exists or the array item is not used, it will return -1.
  }
    Function PreviousFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex; virtual;
    Function NextFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex; virtual;
    Function PreviousFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    Function NextFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    // first/last item
    Function FirstArrayIndex: TLLAArrayIndex; virtual;  // returns array index of first listed item
    Function LastArrayIndex: TLLAArrayIndex; virtual;   // ^^... of last listed item
    Function First: PLLAPayload; virtual;
    Function Last: PLLAPayload; virtual;
    // index checking
    Function CheckIndex(Index: Integer): Boolean; override;
    Function GetArrayIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    Function GetListIndex(ArrayIndex: TLLAArrayIndex): TLLAListIndex; virtual;
    // searching
    Function IndicesOf(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean; virtual;
    Function ArrayIndexOf(Item: PLLAPayload): TLLAArrayIndex; virtual;
    Function ListIndexOf(Item: PLLAPayload): TLLAListIndex; virtual;
    // list methods
    Function Add(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex): TLLAListIndex; overload; virtual;
    Function Add(Item: PLLAPayload): TLLAListIndex; overload; virtual;
    procedure Insert(ListIndex: TLLAListIndex; Item: PLLAPayload); virtual;
    procedure Move(SrcListIndex,DstListIndex: TLLAListIndex); virtual;
    procedure Exchange(ListIndex1,ListIndex2: TLLAListIndex); virtual;
    Function Extract(Item: PLLAPayload): PLLAPayload; virtual;  // does not call PayloadFinal
    Function Remove(Item: PLLAPayload): TLLAListIndex; virtual;
    procedure Delete(ListIndex: TLLAListIndex); virtual;
    procedure Clear; virtual;
    // list manipulation
    procedure Reverse; virtual;
    procedure Sort(Reversed: Boolean = False); virtual;
    procedure Defragment; virtual;
    // low-level items access
    Function ArrayItemIsUsed(ArrayIndex: TLLAArrayIndex): Boolean; virtual;
    // list object access
    Function IsEqual(List: TLinkedListArray): Boolean; virtual;
    procedure Assign(List: TLinkedListArray); virtual;
    procedure Append(List: TLinkedListArray); virtual;
  {
    streaming

    Note that the items (their payload) are stored in the order they are present
    in the linked list, not by their order in the array.

    If parameter Buffered is set to false (default), then items are saved one
    by one.
    If it is set to true, then entire data stream is constructed in the memory
    and then saved as a whole. This might not be possible in case of memory
    shortage, that is why this parameter is by default set to false. Also, if
    the Stream is a descendant of TCustomMemoryStream, the buffering is not
    done as it is assumed the writes/reads goes into memory anyway.

    Read/Write methods are only saving/loading the payloads, nothing more.
    This, among others, means you have to set count to appropriate number
    before doing the read.
    Load/Save methods are first storing number of items (Count) and only then
    the payloads. When loading, the list is first cleared, then the Count is
    set to a stored count and this number of items is then read.
  }
    procedure WriteToStream(Stream: TStream; Buffered: Boolean = False); virtual;
    procedure ReadFromStream(Stream: TStream; Buffered: Boolean = False); virtual;
    procedure SaveToStream(Stream: TStream; Buffered: Boolean = False); virtual;
    procedure LoadFromStream(Stream: TStream; Buffered: Boolean = False); virtual;
    procedure WriteToFile(const FileName: String; Buffered: Boolean = False); virtual;
    procedure ReadFromFile(const FileName: String; Buffered: Boolean = False); virtual;
    procedure SaveToFile(const FileName: String; Buffered: Boolean = False); virtual;
    procedure LoadFromFile(const FileName: String; Buffered: Boolean = False); virtual;
    // properties
    property PayloadSize: TMemSize read fPayloadSize;
    property ArrayPointers[ArrayIndex: TLLAArrayIndex]: PLLAPayload read GetPayloadPtrArrayIndex;
    property ListPointers[ListIndex: TLLAListIndex]: PLLAPayload read GetPayloadPtrListIndex;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TIntegerLinkedListArray
--------------------------------------------------------------------------------
===============================================================================} 
{===============================================================================
    TIntegerLinkedListArray - class declaration
===============================================================================}
type
  TIntegerLinkedListArray = class(TLinkedListArray)
  protected
    Function GetItem(ListIndex: TLLAListIndex): Integer; virtual;
    procedure SetItem(ListIndex: TLLAListIndex; Value: Integer); virtual;
    Function PayloadCompare(Payload1,Payload2: PLLAPayload): Integer; override;
    procedure PayloadWrite(Payload: PLLAPayload; Stream: TStream); override;
    procedure PayloadRead(Payload: PLLAPayload; Stream: TStream); override;
  public
    constructor Create;
    Function First: Integer; reintroduce;
    Function Last: Integer; reintroduce;
    Function IndicesOf(Item: Integer; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean; reintroduce;
    Function ArrayIndexOf(Item: Integer): TLLAArrayIndex; reintroduce;
    Function ListIndexOf(Item: Integer): TLLAListIndex; reintroduce;
    Function Add(Item: Integer; out ArrayIndex: TLLAArrayIndex): TLLAListIndex; reintroduce; overload;
    Function Add(Item: Integer): TLLAListIndex; reintroduce; overload;
    procedure Insert(ListIndex: TLLAListIndex; Item: Integer); reintroduce;
    Function Extract(Item: Integer): Integer; reintroduce;
    Function Remove(Item: Integer): TLLAListIndex; reintroduce;
    property Items[ListIndex: TLLAListIndex]: Integer read GetItem write SetItem; default;
  end;

implementation

uses
  Math,
  ListSorters, StrRect, BinaryStreaming;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                TLinkedListArray
--------------------------------------------------------------------------------
===============================================================================}

const
  LLA_FLAGS_INIT = $00000000;

  LLA_FLAG_USED = $00000001;

{===============================================================================
    TLinkedListArray - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TLinkedListArray - protected methods
-------------------------------------------------------------------------------}

class Function TLinkedListArray.GetItemFlagValue(const Item: TLLAItem; Flag: Integer): Boolean;
begin
Result := (Item.Flags and Flag) = Flag;
end;

//------------------------------------------------------------------------------

class Function TLinkedListArray.SetItemFlagValue(var Item: TLLAItem; Flag: Integer; NewValue: Boolean): Boolean;
begin
Result := (Item.Flags and Flag) = Flag;
If NewValue then
  Item.Flags := Item.Flags or Flag
else
  Item.Flags := Item.Flags and not Flag;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PayloadPtrFromItemPtr(ItemPtr: PLLAItem): PLLAPayload;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PLLAPayload(PtrUInt(ItemPtr) + fPayloadOffset);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.ItemPtrFromPayloadPtr(PayloadPtr: PLLAPayload): PLLAItem;
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
Result := PLLAItem(PtrUInt(PayloadPtr) - fPayloadOffset);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetItemPtr(ArrayIndex: TLLAArrayIndex): PLLAItem;
begin
Result := nil;
If CheckArrayIndexAndRaise(ArrayIndex,'GetItemPtr') then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PLLAItem(PtrUInt(fMemory) + (PtrUInt(ArrayIndex) * (PtrUInt(fItemSize))));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetPayloadPtrArrayIndex(ArrayIndex: TLLAArrayIndex): PLLAPayload;
begin
Result := PayloadPtrFromItemPtr(GetItemPtr(ArrayIndex));
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetPayloadPtrArrayIndex(ArrayIndex: TLLAArrayIndex; Value: PLLAPayload);
var
  PayloadPtr: PLLAPayload;
begin
PayloadPtr := GetPayloadPtrArrayIndex(ArrayIndex);
System.Move(PayloadPtr^,fTempPayload^,fPayloadSize);
System.Move(Value^,PayloadPtr^,fPayloadSize);
If not PayloadEquals(fTempPayload,Value) then
  DoChange;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetPayloadPtrListIndex(ListIndex: TLLAListIndex): PLLAPayload;
begin
Result := nil;
If CheckListIndexAndRaise(ListIndex,'GetPayloadPtrListIndex') then
  Result := GetPayloadPtrArrayIndex(GetArrayIndex(ListIndex));
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetPayloadPtrListIndex(ListIndex: TLLAListIndex; Value: PLLAPayload);
begin
If CheckListIndexAndRaise(ListIndex,'SetPayloadPtrListIndex') then
  SetPayloadPtrArrayIndex(GetArrayIndex(ListIndex),Value);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetCapacity: Integer;
begin
Result := fCapacity;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetCapacity(Value: Integer);
var
  OldCap:   Integer;
  i:        TLLAArrayIndex;
  ItemPtr:  PLLAItem;
begin
If (Value <> fCapacity) and (Value >= 0) then
  begin
    If Value > fCapacity then
      begin
        // add new free items
        OldCap := fCapacity;
        ReallocMem(fMemory,TMemSize(Value) * TMemSize(fItemSize));
        fCapacity := Value;
        For i := TLLAArrayIndex(OldCap) to HighArrayIndex do
          begin
            ItemPtr := GetItemPtr(i);
            If i = TLLAArrayIndex(OldCap) then ItemPtr^.Prev := -1
              else ItemPtr^.Prev := i - 1;
            If i = HighArrayIndex then ItemPtr^.Next := -1
              else ItemPtr^.Next := i + 1;
            ItemPtr^.Flags := LLA_FLAGS_INIT;
          end;
        If not CheckArrayIndex(fFirstFree) then
          fFirstFree := OldCap;
        If CheckArrayIndex(fLastFree) then
          begin
            GetItemPtr(fLastFree)^.Next := OldCap;
            GetItemPtr(OldCap)^.Prev := fLastFree;
          end;
        fLastFree := HighArrayIndex;
      end
    else
      begin
        BeginUpdate;
        try
          Defragment;
          // remove existing items
          If Value < fCount then
            begin
              // some used items will be removed
              If not fLoading then
                For i := HighArrayIndex downto TLLAArrayIndex(Value) do
                  If GetItemFlagValue(GetItemPtr(i)^,LLA_FLAG_USED) then
                    PayloadFinal(GetPayloadPtrArrayIndex(i));
              ReallocMem(fMemory,TMemSize(Value) * TMemSize(fItemSize));
              fCapacity := Value;
              fCount := Value;
              If CheckArrayIndex(HighArrayIndex) then
                GetItemPtr(HighArrayIndex)^.Next := -1;
              // there is no free item anymore
              fFirstFree := -1;
              fLastFree := -1;
              If fCapacity <= 0 then
                fFirstUsed := -1;
              fLastUsed := HighArrayIndex;
              DoChange;
            end
          else
            begin
              // no used item is removed
              ReallocMem(fMemory,TMemSize(Value) * TMemSize(fItemSize));
              fCapacity := Value;
              If CheckArrayIndex(HighArrayIndex) then
                GetItemPtr(HighArrayIndex)^.Next := -1;
              If fCount = fCapacity then
                begin
                  fFirstFree := -1;
                  fLastFree := -1;
                  If fCapacity <= 0 then
                    fFirstUsed := -1;
                  fLastUsed := HighArrayIndex;
                end
              else fLastFree := HighArrayIndex;
            end;
        finally
          EndUpdate;
        end;    
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetCount: Integer;
begin
Result := fCount;
end;
 
//------------------------------------------------------------------------------

procedure TLinkedListArray.SetCount(Value: Integer);
var
  Temp:       Integer;
  ArrayIndex: TLLAArrayIndex;
  TempIndex:  TLLAArrayIndex;
  ItemPtr:    PLLAItem;
begin
If (Value <> fCount) and (Value >= 0) then
  begin
    If Value > fCount then
      begin
        // new items will be added
        If Value > fCapacity then
          SetCapacity(Value);
        Temp := Value - fCount; // number of newly added items
        ArrayIndex := fFirstFree;
        GetItemPtr(ArrayIndex)^.Prev := fLastUsed;
        If CheckArrayIndex(fLastUsed) then
          GetItemPtr(fLastUsed)^.Next := fFirstFree;
        If not CheckArrayIndex(fFirstUsed) then
          fFirstUsed := ArrayIndex;
        repeat
          ItemPtr := GetItemPtr(ArrayIndex);
          SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,True);
          If not fLoading then
            PayloadInit(PayloadPtrFromItemPtr(ItemPtr));
          fLastUsed := ArrayIndex;
          ArrayIndex := ItemPtr^.Next;
          fFirstFree := ArrayIndex;
          Dec(Temp);
        until Temp <= 0;
        GetItemPtr(fLastUsed)^.Next := -1;
        If CheckArrayIndex(fFirstFree) then
          GetItemPtr(fFirstFree)^.Prev := -1
        else
          fLastFree := -1;   
      end
    else
      begin
        // items will be removed
        Temp := fCount - Value;   // number of items to be removed
        ArrayIndex := fLastUsed;
        If CheckArrayIndex(fLastFree) then
          GetItemPtr(fLastFree)^.Next := ArrayIndex;
        If not CheckArrayIndex(fFirstFree) then
          fFirstFree := ArrayIndex;
        repeat
          ItemPtr := GetItemPtr(ArrayIndex);
          If not fLoading then
            PayloadFinal(PayloadPtrFromItemPtr(ItemPtr));
          SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,False);
          TempIndex := fLastFree;
          fLastFree := ArrayIndex;
          ArrayIndex := ItemPtr^.Prev;
          ItemPtr^.Prev := TempIndex;
          ItemPtr^.Next := ArrayIndex;
          fLastUsed := ArrayIndex;
          Dec(Temp);
        until Temp <= 0;
        GetItemPtr(fLastFree)^.Next := -1;
        If CheckArrayIndex(fLastUsed) then
          GetItemPtr(fLastUsed)^.Next := -1
        else
          fFirstUsed := -1;
      end;
    fCount := Value;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckArrayIndex(ArrayIndex: TLLAArrayIndex): Boolean;
begin
Result := (ArrayIndex >= LowArrayIndex) and (ArrayIndex <= HighArrayIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckListIndex(ListIndex: TLLAListIndex): Boolean;
begin
Result := (ListIndex >= LowListIndex) and (ListIndex <= HighListIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckArrayIndexAndRaise(ArrayIndex: TLLAArrayIndex; CallingMethod: String = 'CheckArrayIndexAndRaise'): Boolean;
begin
Result := CheckArrayIndex(ArrayIndex);
If not Result then
  raise ELLAArrayIndexOutOfBounds.CreateFmt('TLinkedListArray.%s: Array index (%d) out of bounds.',[CallingMethod,ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckListIndexAndRaise(ListIndex: TLLAListIndex; CallingMethod: String = 'CheckListIndexAndRaise'): Boolean;
begin
Result := CheckListIndex(ListIndex);
If not Result then
  raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.%s: List index (%d) out of bounds.',[CallingMethod,ListIndex]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.PayloadInit(Payload: PLLAPayload);
begin
FillChar(Payload^,fPayloadSize,0);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TLinkedListArray.PayloadAdded(Payload: PLLAPayload);
begin
// nothing to do here
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TLinkedListArray.PayloadFinal(Payload: PLLAPayload);
begin
// nothing to do here
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TLinkedListArray.PayloadCopy(SrcPayload,DstPayload: PLLAPayload);
begin
System.Move(SrcPayload^,DstPayload^,fPayloadSize);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PayloadCompare(Payload1,Payload2: PLLAPayload): Integer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If PtrUInt(Payload1) < PtrUInt(Payload2) then
  Result := -1
else If PtrUInt(Payload1) > PtrUInt(Payload2) then
  Result := +1
else
  Result := 0;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PayloadEquals(Payload1,Payload2: PLLAPayload): Boolean;
begin
Result := PayloadCompare(Payload1,Payload2) = 0;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.PayloadWrite(Payload: PLLAPayload; Stream: TStream);
begin
Stream.WriteBuffer(Payload^,fPayloadSize);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.PayloadRead(Payload: PLLAPayload; Stream: TStream);
begin
Stream.ReadBuffer(Payload^,fPayloadSize);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.SortCompare(ListIndex1,ListIndex2: Integer): Integer;
begin
Result := PayloadCompare(GetPayloadPtrListIndex(TLLAListIndex(ListIndex1)),
                         GetPayloadPtrListIndex(TLLAListIndex(ListIndex2)));
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SortExchange(ListIndex1,ListIndex2: Integer);
begin
Exchange(TLLAListIndex(ListIndex1),TLLAListIndex(ListIndex2));
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.DefragCompare(Index1,Index2: Integer): Integer;
var
  Item1Ptr: PLLAItem;
  Item2Ptr: PLLAItem;
begin
Item1Ptr := GetItemPtr(TLLAArrayIndex(Index1));
Item2Ptr := GetItemPtr(TLLAArrayIndex(Index2));
If Item1Ptr^.Prev < Item2Ptr^.Prev then
  Result := -1
else If Item1Ptr^.Prev > Item2Ptr^.Prev then
  Result := +1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.DefragExchange(Index1,Index2: Integer);
var
  Item1Ptr,Item2Ptr:  PLLAItem;
  Temp:               Integer; 
begin
If Index1 <> Index2 then
  begin
    Item1Ptr := GetItemPtr(TLLAArrayIndex(Index1));
    Item2Ptr := GetItemPtr(TLLAArrayIndex(Index2));
    // exchange indices
    Temp := Integer(Item1Ptr^.Prev);
    Item1Ptr^.Prev := Item2Ptr^.Prev;
    Item2Ptr^.Prev := TLLAArrayIndex(Temp);
    // exchange flags
    Temp := Integer(Item1Ptr^.Flags);
    Item1Ptr^.Flags := Item2Ptr^.Flags;
    Item2Ptr^.Flags := TLLAArrayIndex(Temp);
    // exchange data
    System.Move(PayloadPtrFromItemPtr(Item1Ptr)^,fTempPayload^,fPayloadSize);
    System.Move(PayloadPtrFromItemPtr(Item2Ptr)^,PayloadPtrFromItemPtr(Item1Ptr)^,fPayloadSize);
    System.Move(fTempPayload^,PayloadPtrFromItemPtr(Item2Ptr)^,fPayloadSize);
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.DoChange;
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

procedure TLinkedListArray.Decouple(ArrayIndex: TLLAArrayIndex);
var
  ItemPtr:  PLLAItem;
begin
If CheckArrayIndexAndRaise(ArrayIndex,'Decouple') then
  begin
    ItemPtr := GetItemPtr(ArrayIndex);
    If CheckArrayIndex(ItemPtr^.Prev) then
      GetItemPtr(ItemPtr^.Prev)^.Next := ItemPtr^.Next;
    If CheckArrayIndex(ItemPtr^.Next) then
      GetItemPtr(ItemPtr^.Next)^.Prev := ItemPtr^.Prev;
    If ArrayIndex = fFirstFree then
      fFirstFree := ItemPtr^.Next;
    If ArrayIndex = fLastFree then
      fLastFree := ItemPtr^.Prev;      
    If ArrayIndex = fFirstUsed then
      fFirstUsed := ItemPtr^.Next;
    If ArrayIndex = fLastUsed then
      fLastUsed := ItemPtr^.Prev; 
    ItemPtr^.Prev := -1;
    ItemPtr^.Next := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.InternalDelete(ArrayIndex: TLLAArrayIndex);
var
  ItemPtr:  PLLAItem;
begin
If CheckArrayIndexAndRaise(ArrayIndex,'InternalDelete') then
  begin
    ItemPtr := GetItemPtr(ArrayIndex);
    If GetItemFlagValue(ItemPtr^,LLA_FLAG_USED) then
      begin
        PayloadFinal(PayloadPtrFromItemPtr(ItemPtr));
        // remove from list of used items
        Decouple(ArrayIndex);
        // add to list of free items
        ItemPtr^.Prev := fLastFree;
        ItemPtr^.Next := -1;
        If CheckArrayIndex(fLastFree) then
          GetItemPtr(fLastFree)^.Next := ArrayIndex;
        If not CheckArrayIndex(fFirstFree) then
          fFirstFree := ArrayIndex;
        fLastFree := ArrayIndex;
        // final touches
        SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,False);
        Dec(fCount);
        Shrink;
        DoChange;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.ArrayIndices(ListIndex1,ListIndex2: TLLAListIndex; out ArrayIndex1,ArrayIndex2: TLLAArrayIndex);
var
  TempArrayIndex: TLLAArrayIndex;
  TempListIndex:  TLLAListIndex;
begin
ArrayIndex1 := -1;
ArrayIndex2 := -1;
If not CheckListIndex(ListIndex1) then
  ArrayIndex2 := GetArrayIndex(ListIndex2)
else If not CheckListIndex(ListIndex2) then
  ArrayIndex1 := GetArrayIndex(ListIndex1)
else
  begin
    // both list indices are valid
    TempArrayIndex := fFirstUsed;
    TempListIndex := LowListIndex;
    while CheckArrayIndex(TempArrayIndex) and
     (TempListIndex <= Max(ListIndex1,ListIndex2)) do
      begin
        If TempListIndex = ListIndex1 then
          ArrayIndex1 := TempArrayIndex;
        If TempListIndex = ListIndex2 then
          ArrayIndex2 := TempArrayIndex;
        TempArrayIndex := GetItemPtr(TempArrayIndex)^.Next;
        Inc(TempListIndex);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.FinalizeAllItems;
var
  i:        TLLAArrayIndex;
  ItemPtr:  PLLAItem;
begin
If fCount > 0 then
  For i := LowArrayIndex to HighArrayIndex do
    begin
      ItemPtr := GetItemPtr(i);
      If GetItemFlagValue(ItemPtr^,LLA_FLAG_USED) then
        PayloadFinal(PayloadPtrFromItemPtr(ItemPtr));
    end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.ReadFromStreamInternal(Stream: TStream; Buffered: Boolean);
var
  BufferStream: TMemoryStream;
  ArrayIndex:   TLLAArrayIndex;  
  i:            TLLAListIndex;
  PayloadPtr:   PLLAPayload;
begin
If fCount > 0 then
  begin
    If Buffered and not (Stream is TCustomMemoryStream) then
      begin
        BufferStream := TMemoryStream.Create;
        try
          BufferStream.Size := fCount * Int64(fPayloadSize);
          Stream.ReadBuffer(BufferStream.Memory^,BufferStream.Size);
          BufferStream.Seek(0,soBeginning);
          ArrayIndex := FirstArrayIndex;
          For i := LowListIndex to HighListIndex do
            begin
              PayloadPtr := GetPayloadPtrArrayIndex(ArrayIndex);
              PayloadRead(PayloadPtr,BufferStream);
              PayloadAdded(PayloadPtr);
              ArrayIndex := NextFromArrayIndex(ArrayIndex);
            end;
        finally
          BufferStream.Free;
        end;
      end
    else
      begin
        ArrayIndex := FirstArrayIndex;
        For i := LowListIndex to HighListIndex do
          begin
            PayloadPtr := GetPayloadPtrArrayIndex(ArrayIndex);
            PayloadRead(PayloadPtr,Stream);
            PayloadAdded(PayloadPtr);
            ArrayIndex := NextFromArrayIndex(ArrayIndex);
          end;
      end;
  end;
DoChange;
end;

{-------------------------------------------------------------------------------
    TLinkedListArray - public methods
-------------------------------------------------------------------------------}

constructor TLinkedListArray.Create(PayloadSize: TMemSize);
begin
inherited Create;
fPayloadSize := PayloadSize;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
fPayloadOffset := PtrUInt(Addr(PLLAItem(nil)^.Payload));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
fItemSize := fPayloadOffset + PayloadSize;
fMemory := nil;
fCapacity := 0;
fCount := 0;
fUpdateCounter := 0;
fChanged := False;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
GetMem(fTempPayload,fPayloadSize);
fFirstFree := -1;
fLastFree := -1;
fFirstUsed := -1;
fLastUsed := -1;
fLoading := False;
end;

//------------------------------------------------------------------------------

destructor TLinkedListArray.Destroy;
begin
FinalizeAllItems;
FreeMem(fTempPayload,fPayloadSize);
FreeMem(fMemory,TMemSize(fCapacity) * TMemSize(fItemSize));
inherited;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.BeginUpdate;
begin
If fUpdateCounter <= 0 then
  fChanged := False;
Inc(fUpdateCounter);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.EndUpdate: Integer;
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

Function TLinkedListArray.LowIndex: Integer;
begin
Result := LowListIndex;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.HighIndex: Integer;
begin
Result := HighListIndex;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.LowArrayIndex: TLLAArrayIndex;
begin
Result := 0;
end;

//------------------------------------------------------------------------------


Function TLinkedListArray.HighArrayIndex: TLLAArrayIndex;
begin
Result := Pred(fCapacity);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.LowListIndex: TLLAListIndex;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.HighListIndex: TLLAListIndex;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PreviousFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex;
var
  ItemPtr:  PLLAItem;
begin
Result := -1;
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := GetItemPtr(ArrayIndex);
    If GetItemFlagValue(ItemPtr^,LLA_FLAG_USED) then
      Result := ItemPtr^.Prev;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.NextFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex;
var
  ItemPtr:  PLLAItem;
begin
Result := -1;
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := GetItemPtr(ArrayIndex);
    If GetItemFlagValue(ItemPtr^,LLA_FLAG_USED)then
      Result := ItemPtr^.Next;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PreviousFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
Result := GetArrayIndex(ListIndex);
If CheckArrayIndex(Result) then
  Result := GetItemPtr(Result)^.Prev
else
  Result := -1;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.NextFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
Result := GetArrayIndex(ListIndex);
If CheckArrayIndex(Result) then
  Result := GetItemPtr(Result)^.Next
else
  Result := -1;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.FirstArrayIndex: Integer;
begin
Result := fFirstUsed;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.LastArrayIndex: Integer;
begin
Result := fLastUsed;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.First: PLLAPayload;
begin
Result := GetPayloadPtrArrayIndex(fFirstUsed);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Last: PLLAPayload;
begin
Result := GetPayloadPtrArrayIndex(fLastUsed);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckIndex(Index: Integer): Boolean;
begin
Result := CheckListIndex(TLLAListIndex(Index));
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetArrayIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
If CheckListIndex(ListIndex) then
  begin
    Result := fFirstUsed;
    while CheckArrayIndex(Result) and (ListIndex > 0) do
      begin
        Result := GetItemPtr(Result)^.Next;
        Dec(ListIndex);
      end;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetListIndex(ArrayIndex: TLLAArrayIndex): TLLAListIndex;
var
  TempIndex:  TLLAArrayIndex;
begin
If CheckArrayIndex(ArrayIndex) then
  begin
    If GetItemFlagValue(GetItemPtr(ArrayIndex)^,LLA_FLAG_USED) then
      begin
        Result := LowListIndex;
        TempIndex := fFirstUsed;
        while CheckArrayIndex(TempIndex) and (TempIndex <> ArrayIndex) do
          begin
            TempIndex := GetItemPtr(TempIndex)^.Next;
            Inc(Result);
          end;
        If TempIndex <> ArrayIndex then
          Result := -1;
      end
    else Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.IndicesOf(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean;
begin
Result := False;
// traverse list of used items
ArrayIndex := fFirstUsed;
ListIndex := LowListIndex;
while CheckArrayIndex(ArrayIndex) do
  begin
    If PayloadEquals(Item,GetPayloadPtrArrayIndex(ArrayIndex)) then
      begin
        Result := True;
        Break{while...};
      end
    else ArrayIndex := GetItemPtr(ArrayIndex)^.Next;
    Inc(ListIndex);
  end;
If not Result then
  begin
    ListIndex := -1;
    ArrayIndex := -1;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.ArrayIndexOf(Item: PLLAPayload): TLLAArrayIndex;
var
  ListIndex:  TLLAListIndex;
begin
IndicesOf(Item,Result,ListIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.ListIndexOf(Item: PLLAPayload): TLLAListIndex;
var
  ArrayIndex: TLLAArrayIndex;
begin
IndicesOf(Item,ArrayIndex,Result);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Add(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex): TLLAListIndex;
var
  ItemPtr:  PLLAItem;
begin
Grow;
// at this point, there MUST be at least one free item
ArrayIndex := fFirstFree;
ItemPtr := GetItemPtr(ArrayIndex);
// remove from list of free items
Decouple(ArrayIndex);
// add to list of used items
If not CheckArrayIndex(fFirstUsed) then
  fFirstUsed := ArrayIndex;
If CheckArrayIndex(fLastUsed) then
  GetItemPtr(fLastUsed)^.Next := ArrayIndex;
ItemPtr^.Prev := fLastUsed;
ItemPtr^.Next := -1;
fLastUsed := ArrayIndex;
// add the data and set flags
System.Move(Item^,ItemPtr^.Payload,fPayloadSize);
SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,True);
PayloadAdded(PayloadPtrFromItemPtr(ItemPtr));
Result := fCount;
Inc(fCount);
DoChange;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TLinkedListArray.Add(Item: PLLAPayload): TLLAListIndex;
var
  ArrayIndex: TLLAArrayIndex;
begin
Result := Add(Item,ArrayIndex);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Insert(ListIndex: TLLAListIndex; Item: PLLAPayload);
var
  NewItemArrayIndex:  Integer;
  OldItemArrayIndex:  Integer;
  NewItemPtr:         PLLAItem;
  OldItemPtr:         PLLAItem;
begin
If CheckListIndex(ListIndex) then
  begin
    OldItemArrayIndex := GetArrayIndex(ListIndex);
    If CheckArrayIndex(OldItemArrayIndex) then
      begin
        Grow;
        NewItemArrayIndex := fFirstFree;
        NewItemPtr := GetItemPtr(NewItemArrayIndex);
        OldItemPtr := GetItemPtr(OldItemArrayIndex);
        Decouple(NewItemArrayIndex);
        // insert to the list of used items
        NewItemPtr^.Prev := OldItemPtr^.Prev;
        NewItemPtr^.Next := OldItemArrayIndex;
        If CheckArrayIndex(OldItemPtr^.Prev) then
          GetItemPtr(OldItemPtr^.Prev)^.Next := NewItemArrayIndex;
        OldItemPtr^.Prev := NewItemArrayIndex;
        If fFirstUsed = OldItemArrayIndex then
          fFirstUsed := NewItemArrayIndex;
        // add the data and set flags
        System.Move(Item^,NewItemPtr^.Payload,fPayloadSize);
        SetItemFlagValue(NewItemPtr^,LLA_FLAG_USED,True);
        PayloadAdded(PayloadPtrFromItemPtr(NewItemPtr));
        Inc(fCount);
        DoChange;
      end;
  end
else Add(Item);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Move(SrcListIndex,DstListIndex: TLLAListIndex);
var
  SrcArrayIndex,DstArrayIndex:  TLLAArrayIndex;
  SrcItemPtr,DstItemPtr:        PLLAItem;
begin
If SrcListIndex <> DstListIndex then
  begin
    CheckListIndexAndRaise(SrcListIndex,'Move');
    CheckListIndexAndRaise(DstListIndex,'Move');
    // get pointers
    ArrayIndices(SrcListIndex,DstListIndex,SrcArrayIndex,DstArrayIndex);
    SrcItemPtr := GetItemPtr(SrcArrayIndex);
    DstItemPtr := GetItemPtr(DstArrayIndex);
    // remove moved item from old position
    Decouple(SrcArrayIndex);
    // insert to new position
    If DstListIndex > SrcListIndex then
      begin
        // item is moved up
        SrcItemPtr^.Prev := DstArrayIndex;
        SrcItemPtr^.Next := DstItemPtr^.Next;
        If CheckArrayIndex(DstItemPtr^.Next) then
          GetItemPtr(DstItemPtr^.Next)^.Prev := SrcArrayIndex;
        DstItemPtr^.Next := SrcArrayIndex;
        If fLastUsed = DstArrayIndex then
          fLastUsed := SrcArrayIndex
      end
    else
      begin
        // item is moved down
        SrcItemPtr^.Prev := DstItemPtr^.Prev;
        SrcItemPtr^.Next := DstArrayIndex;
        If CheckArrayIndex(DstItemPtr^.Prev) then
          GetItemPtr(DstItemPtr^.Prev)^.Next := SrcArrayIndex;
        DstItemPtr^.Prev := SrcArrayIndex;
        If fFirstUsed = DstArrayIndex then
          fFirstUsed := SrcArrayIndex;        
      end;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Exchange(ListIndex1,ListIndex2: TLLAListIndex);
var
  TempListIndex:            TLLAListIndex;
  ArrayIndex1,ArrayIndex2:  TLLAArrayIndex;
  ItemPtr1,ItemPtr2:        PLLAItem;
  TempIndex1,TempIndex2:    TLLAArrayIndex;
begin
If ListIndex1 <> ListIndex2 then
  begin
    CheckListIndexAndRaise(ListIndex1,'Exchange');
    CheckListIndexAndRaise(ListIndex2,'Exchange');
    If ListIndex2 < ListIndex1 then
      begin
        TempListIndex := ListIndex1;
        ListIndex1 := ListIndex2;
        ListIndex2 := TempListIndex;
      end;
    // get pointers to items (indices are checked in GetItemPtr)
    ArrayIndices(ListIndex1,ListIndex2,ArrayIndex1,ArrayIndex2);
    ItemPtr1 := GetItemPtr(ArrayIndex1);
    ItemPtr2 := GetItemPtr(ArrayIndex2);
    // corrections when items are on any of the list ends
    If ArrayIndex1 = fFirstUsed then
      fFirstUsed := ArrayIndex2
    else If ArrayIndex2 = fFirstUsed then
      fFirstUsed := ArrayIndex1;
    If ArrayIndex1 = fLastUsed then
      fLastUsed := ArrayIndex2
    else If ArrayIndex2 = fLastUsed then
      fLastUsed := ArrayIndex1;
    // do exchange only by swapping indices
    If Abs(ListIndex1 - ListIndex2) > 1 then
      begin
         // far items
        If CheckArrayIndex(ItemPtr1^.Prev) then
          GetItemPtr(ItemPtr1^.Prev)^.Next := ArrayIndex2;
        If CheckArrayIndex(ItemPtr1^.Next) then
          GetItemPtr(ItemPtr1^.Next)^.Prev := ArrayIndex2;
        If CheckArrayIndex(ItemPtr2^.Prev) then
          GetItemPtr(ItemPtr2^.Prev)^.Next := ArrayIndex1;
        If CheckArrayIndex(ItemPtr2^.Next) then
          GetItemPtr(ItemPtr2^.Next)^.Prev := ArrayIndex1;
        TempIndex1 := ItemPtr1^.Prev;
        TempIndex2 := ItemPtr1^.Next;
        ItemPtr1^.Prev := ItemPtr2^.Prev;
        ItemPtr1^.Next := ItemPtr2^.Next;
        ItemPtr2^.Prev := TempIndex1;
        ItemPtr2^.Next := TempIndex2;
      end
    else
      begin
        // close items
        If CheckArrayIndex(ItemPtr1^.Prev) then
          GetItemPtr(ItemPtr1^.Prev)^.Next := ArrayIndex2;
        If CheckArrayIndex(ItemPtr2^.Next) then
          GetItemPtr(ItemPtr2^.Next)^.Prev := ArrayIndex1;
        TempIndex1 := ItemPtr1^.Prev;
        TempIndex2 := ItemPtr2^.Next;
        ItemPtr1^.Prev := ItemPtr1^.Next;
        ItemPtr1^.Next := TempIndex2;
        ItemPtr2^.Next := ItemPtr2^.Prev;
        ItemPtr2^.Prev := TempIndex1;
      end;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Extract(Item: PLLAPayload): PLLAPayload;
var
  ArrayIndex: TLLAArrayIndex;
  ItemPtr:    PLLAItem;
begin
ArrayIndex := ArrayIndexOf(Item);
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := GetItemPtr(ArrayIndex);
    System.Move(ItemPtr^,fTempPayload^,fPayloadSize);
    Result := PayloadPtrFromItemPtr(fTempPayload);
    Decouple(ArrayIndex);
    // add to list of free items
    ItemPtr^.Prev := fLastFree;
    ItemPtr^.Next := -1;
    If CheckArrayIndex(fLastFree) then
      GetItemPtr(fLastFree)^.Next := ArrayIndex;
    If not CheckArrayIndex(fFirstFree) then
      fFirstFree := ArrayIndex;
    fLastFree := ArrayIndex;
    // reset flag and final touches
    SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,False);
    Dec(fCount);
    Shrink;
    DoChange;
  end
else Result := nil;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Remove(Item: PLLAPayload): TLLAListIndex;
var
  ArrayIndex: TLLAArrayIndex;
begin
If IndicesOf(Item,ArrayIndex,Result) then
  InternalDelete(ArrayIndex);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Delete(ListIndex: TLLAListIndex);
begin
If CheckListIndexAndRaise(ListIndex,'Delete') then
  InternalDelete(GetArrayIndex(ListIndex));
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Clear;
var
  i:  TLLAArrayIndex;
begin
If fCapacity > 0 then
  begin
    FinalizeAllItems;
    For i := LowArrayIndex to HighArrayIndex do
      with GetItemPtr(i)^ do
        begin
          If i > LowArrayIndex then Prev := i - 1
            else Prev := -1;
          If i < HighArrayIndex then Next := i + 1
            else Next := -1;
          Flags := 0;
        end;
    fCount := 0;
    fFirstFree := LowArrayIndex;
    fLastFree := HighArrayIndex;
    fFirstUsed := -1;
    fLastUsed := -1;
    Shrink;  
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Reverse;
var
  TempIndex:  TLLAArrayIndex;
begin
If fCount > 1 then
  begin
    TempIndex := fFirstUsed;
    fFirstUsed := fLastUsed;
    fLastUsed := TempIndex;
    while CheckArrayIndex(TempIndex) do
      with GetItemPtr(TempIndex)^ do
        begin
          TempIndex := Next;
          Next := Prev;
          Prev := TempIndex;
        end;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Sort(Reversed: Boolean = False);
var
  Sorter: TListSorter;
begin
If fCount > 1 then
  begin
    BeginUpdate;
    try
      Sorter := TListQuickSorter.Create(SortCompare,SortExchange);
      try
        Sorter.Stabilized := True;
        Sorter.Reversed := Reversed;
        Sorter.Sort(LowListIndex,HighListIndex);
      finally
        Sorter.Free;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Defragment;
var
  ArrayIndex: TLLAArrayIndex;
  ListIndex:  TLLAListIndex;
  ItemIndex:  TLLAArrayIndex;
  ItemPtr:    PLLAItem;
  ArrayPtr:   PLLAItem;  
  i:          TLLAArrayIndex;
begin
If fCount > 0 then
  begin
    // in all list items, set prev to list index and next to -1
    ArrayIndex := fFirstUsed;
    ListIndex := 0;
    while CheckArrayIndex(ArrayIndex) do
      begin
        ItemPtr := GetItemPtr(ArrayIndex);
        ArrayIndex := ItemPtr^.Next;
        ItemPtr^.Prev := ListIndex;
        ItemPtr^.Next := -1;
        Inc(ListIndex);
      end;
    {
      compress items - move them all down in the array, so there is no unused
      item between used ones
    }
    If fCount <> fCapacity then
      begin
        ArrayIndex := LowArrayIndex;
        ItemIndex := HighArrayIndex;
        repeat
          // find first empty item
          while CheckArrayIndex(ArrayIndex) do
            If GetItemFlagValue(GetItemPtr(ArrayIndex)^,LLA_FLAG_USED) then
              Inc(ArrayIndex)
            else
              Break{while};
          // find last used item
          while CheckArrayIndex(ItemIndex) do
            If not GetItemFlagValue(GetItemPtr(ItemIndex)^,LLA_FLAG_USED) then
              Dec(ItemIndex)
            else
              Break{while};
          // move item to empty space    
          If CheckArrayIndex(ArrayIndex) and CheckArrayIndex(ItemIndex) and (ArrayIndex < ItemIndex) then
            begin
              ArrayPtr := GetItemPtr(ArrayIndex);
              ItemPtr := GetItemPtr(ItemIndex);
              System.Move(ItemPtr^.Payload,ArrayPtr^.Payload,fPayloadSize);
              ArrayPtr^.Flags := ItemPtr^.Flags;
              ItemPtr^.Flags := LLA_FLAGS_INIT;
              ArrayPtr^.Prev := ItemPtr^.Prev;
              ItemPtr^.Prev := -1;
              Inc(ArrayIndex);
              Dec(ItemIndex);
            end
          else Break{Repeat};
        until not CheckArrayIndex(ArrayIndex) or not CheckArrayIndex(ItemIndex);
      end;
    // sort items
    with TListQuickSorter.Create(DefragCompare,DefragExchange) do
    try
      Sort(LowArrayIndex,HighListIndex);
    finally
      Free;
    end;
    // reinitialize all indices (first for used, then for free items)
    For i := LowArrayIndex to TLLAArrayIndex(HighListIndex) do
      with GetItemPtr(i)^ do
        begin
          If i > LowArrayIndex then Prev := i - 1
            else Prev := -1;
          If i < TLLAArrayIndex(HighListIndex) then Next := i + 1
            else Next := -1;
        end;
    For i := TLLAArrayIndex(Succ(HighListIndex)) to HighArrayIndex do
      with GetItemPtr(i)^ do
        begin
          If i > TLLAArrayIndex(Succ(HighListIndex)) then Prev := i - 1
            else Prev := -1;
          If i < HighArrayIndex then Next := i + 1
            else Next := -1;
        end;
    If fCount <> fCapacity then
      begin
        fFirstFree := TLLAArrayIndex(Succ(HighListIndex));
        fLastFree := HighArrayIndex;
      end;
    fFirstUsed := LowArrayIndex;
    fLastUsed := TLLAArrayIndex(HighListIndex);
    DoChange;
  end
else Clear; // this will just reinitialize the empty space
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.ArrayItemIsUsed(ArrayIndex: TLLAArrayIndex): Boolean;
begin
Result := False;
If CheckArrayIndexAndRaise(ArrayIndex,'ArrayItemIsUsed') then
  Result := GetItemFlagValue(GetItemPtr(ArrayIndex)^,LLA_FLAG_USED);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.IsEqual(List: TLinkedListArray): Boolean;
var
  i:              TLLAListIndex;
  SelfArrayIndex: TLLAArrayIndex;
  ListArrayIndex: TLLAArrayIndex;
begin
Result := False;
If List is Self.ClassType then
  begin
    If List.Count = fCount then
      begin
        SelfArrayIndex := FirstArrayIndex;
        ListArrayIndex := List.FirstArrayIndex;
        For i := LowListIndex to HighListIndex do
          If PayloadEquals(GetPayloadPtrArrayIndex(SelfArrayIndex),
                           List.ArrayPointers[ListArrayIndex]) then
            begin
              SelfArrayIndex := NextFromArrayIndex(SelfArrayIndex);
              ListArrayIndex := List.NextFromArrayIndex(ListArrayIndex);
            end
          else Exit;
        Result := True;
      end;
  end
else raise ELLAIncompatibleClass.CreateFmt('TLinkedListArray.IsEqual: Object is of incompatible class (%s).',[List.ClassName]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Assign(List: TLinkedListArray);
var
  i:          TLLAListIndex;
  ArrayIndex: TLLAArrayIndex;
  ItemPtr:    PLLAItem;
begin
Clear;
If List.Count > 0 then
  begin
    If fCapacity < List.Count then
      SetCapacity(List.Count);
    ArrayIndex := List.FirstArrayIndex;
    For i := List.LowListIndex to List.HighListIndex do
      begin
        ItemPtr := GetItemPtr(TLLAArrayIndex(i));
        SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,True);
        PayloadCopy(List.ArrayPointers[ArrayIndex],PayloadPtrFromItemPtr(ItemPtr));
        ArrayIndex := List.NextFromArrayIndex(ArrayIndex);
      end;
    fCount := List.Count;
    If fCapacity > fCount then
      begin
        fFirstFree := TLLAArrayIndex(Succ(HighListIndex));
        fLastFree := HighArrayIndex;
      end
    else
      begin
        fFirstFree := -1;
        fLastFree := -1;
      end;
    fFirstUsed := LowArrayIndex;
    fLastUsed := TLLAArrayIndex(HighListIndex);
    If CheckArrayIndex(fFirstFree) then
      GetItemPtr(fFirstFree)^.Prev := -1;
    If CheckArrayIndex(fLastFree) then
      GetItemPtr(fLastFree)^.Next := -1;
    GetItemPtr(fFirstUsed)^.Prev := -1;
    GetItemPtr(fLastUsed)^.Next := -1;
  end;
DoChange;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Append(List: TLinkedListArray);
var
  i:              TLLAListIndex;
  SelfArrayIndex: TLLAArrayIndex;
  ListArrayIndex: TLLAArrayIndex;
  ItemPtr:        PLLAItem;
begin
If List.Count > 0 then
  begin
    If (fCount + List.Count) > fCapacity then
      SetCapacity(fCount + List.Count);
    SelfArrayIndex := fFirstFree;
    ListArrayIndex := List.FirstArrayIndex;
    If CheckArrayIndex(fLastUsed) then
      GetItemPtr(fLastUsed)^.Next := SelfArrayIndex;
    If not CheckArrayIndex(fFirstUsed) then
      fFirstUsed := SelfArrayIndex;
    For i := List.LowListIndex to List.HighListIndex do
      begin
        ItemPtr := GetItemPtr(SelfArrayIndex);
        SetItemFlagValue(ItemPtr^,LLA_FLAG_USED,True);
        PayloadCopy(List.ArrayPointers[ListArrayIndex],PayloadPtrFromItemPtr(ItemPtr));
        ItemPtr^.Prev := fLastUsed;
        fLastUsed := SelfArrayIndex;
        SelfArrayIndex := ItemPtr^.Next;
        ListArrayIndex := List.NextFromArrayIndex(ListArrayIndex);
        fFirstFree := SelfArrayIndex;
      end;
    GetItemPtr(fLastUsed)^.Next := -1;
    If CheckArrayIndex(fFirstFree) then
      GetItemPtr(fFirstFree)^.Prev := -1
    else
      fLastFree := -1;           
    fCount := fCount + List.Count;
    DoChange;  
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.WriteToStream(Stream: TStream; Buffered: Boolean = False);
var
  BufferStream: TMemoryStream;
  i:            TLLAListIndex;
  ArrayIndex:   TLLAArrayIndex;
begin
If fCount > 0 then
  begin
    If Buffered and not (Stream is TCustomMemoryStream) then
      begin
        // first buffer everything in memory, then write in one call
        BufferStream := TMemoryStream.Create;
        try
          BufferStream.Size := fCount * Int64(fPayloadSize); // preallocate memory
          BufferStream.Seek(0,soBeginning);
          ArrayIndex := FirstArrayIndex;
          For i := LowListIndex to HighListIndex do
            begin
              PayloadWrite(GetPayloadPtrArrayIndex(ArrayIndex),BufferStream);
              ArrayIndex := NextFromArrayIndex(ArrayIndex);
            end;
          Stream.WriteBuffer(BufferStream.Memory^,BufferStream.Position);
        finally
          BufferStream.Free;
        end;
      end
    else
      begin
        ArrayIndex := FirstArrayIndex;
        For i := LowListIndex to HighListIndex do
          begin
            PayloadWrite(GetPayloadPtrArrayIndex(ArrayIndex),Stream);
            ArrayIndex := NextFromArrayIndex(ArrayIndex);
          end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.ReadFromStream(Stream: TStream; Buffered: Boolean = False);
begin
BeginUpdate;
try
  FinalizeAllItems;
  ReadFromStreamInternal(Stream,Buffered);
finally
  EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SaveToStream(Stream: TStream; Buffered: Boolean = False);
begin
Stream_WriteInt32(Stream,Int32(fCount));
WriteToStream(Stream,Buffered);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.LoadFromStream(Stream: TStream; Buffered: Boolean = False);
begin
BeginUpdate;
try
  FinalizeAllItems;
  // disable calling of PayloadInit and PayloadFinal from SetCount and SetCapacity
  fLoading := True;
  try
    Count := Integer(Stream_GetInt32(Stream));
  finally
    fLoading := False;
  end;
  ReadFromStreamInternal(Stream,Buffered);
finally
  EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.WriteToFile(const FileName: String; Buffered: Boolean = False);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareExclusive);
try
  FileStream.Seek(0,soBeginning);
  WriteToStream(FileStream,Buffered);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.ReadFromFile(const FileName: String; Buffered: Boolean = False);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  ReadFromStream(FileStream,Buffered);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SaveToFile(const FileName: String; Buffered: Boolean = False);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareExclusive);
try
  FileStream.Seek(0,soBeginning);
  SaveToStream(FileStream,Buffered);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.LoadFromFile(const FileName: String; Buffered: Boolean = False);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  LoadFromStream(FileStream,Buffered);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                            TIntegerLinkedListArray
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerLinkedListArray - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TIntegerLinkedListArray - protected methods
-------------------------------------------------------------------------------}

Function TIntegerLinkedListArray.GetItem(ListIndex: TLLAListIndex): Integer;
begin
Result := Integer(Pointer(GetPayloadPtrListIndex(ListIndex))^);
end;

//------------------------------------------------------------------------------

procedure TIntegerLinkedListArray.SetItem(ListIndex: TLLAListIndex; Value: Integer);
begin
SetPayloadPtrListIndex(ListIndex,@Value);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.PayloadCompare(Payload1,Payload2: PLLAPayload): Integer;
begin
Result := Integer(Pointer(Payload1)^) - Integer(Pointer(Payload2)^);
end;

//------------------------------------------------------------------------------

procedure TIntegerLinkedListArray.PayloadWrite(Payload: PLLAPayload; Stream: TStream);
begin
Stream_WriteInt32(Stream,Int32(Integer(Pointer(Payload)^)));
end;

//------------------------------------------------------------------------------

procedure TIntegerLinkedListArray.PayloadRead(Payload: PLLAPayload; Stream: TStream);
begin
Integer(Pointer(Payload)^) := Integer(Stream_GetInt32(Stream));
end;

{-------------------------------------------------------------------------------
    TIntegerLinkedListArray - public methods
-------------------------------------------------------------------------------}

constructor TIntegerLinkedListArray.Create;
begin
inherited Create(SizeOf(Integer));
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.First: Integer;
begin
Result := Integer(Pointer(inherited First)^);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.Last: Integer;
begin
Result := Integer(Pointer(inherited Last)^);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.IndicesOf(Item: Integer; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean;
begin
Result := inherited IndicesOf(@Item,ArrayIndex,ListIndex);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.ArrayIndexOf(Item: Integer): TLLAArrayIndex;
begin
Result := inherited ArrayIndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.ListIndexOf(Item: Integer): TLLAListIndex;
begin
Result := inherited ListIndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.Add(Item: Integer; out ArrayIndex: TLLAArrayIndex): TLLAListIndex;
begin
Result := inherited Add(@Item,ArrayIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerLinkedListArray.Add(Item: Integer): TLLAListIndex;
begin
Result := inherited Add(@Item);
end;

//------------------------------------------------------------------------------

procedure TIntegerLinkedListArray.Insert(ListIndex: TLLAListIndex; Item: Integer);
begin
inherited Insert(ListIndex,@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.Extract(Item: Integer): Integer;
var
  TempPtr:  PLLAPayload;
begin
TempPtr := inherited Extract(@Item);
If Assigned(TempPtr) then
  Result := Integer(Pointer(TempPtr)^)
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.Remove(Item: Integer): TLLAListIndex;
begin
Result := inherited Remove(@Item);
end;

end.
