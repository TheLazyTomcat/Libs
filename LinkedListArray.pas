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

    A specialized class (TIntegerLinkedListArray) with Integer as item type is
    implemented and provided as an example.

  Version 1.1 (2023-11-19)

  Last change 2024-02-03

  ©2018-2024 František Milt

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
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    ListSorters         - github.com/TheLazyTomcat/Lib.ListSorters
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect

  BinaryStreamingLite can be replaced by full BinaryStreaming.    

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
    Function ArrayFind(Item: @Type@; out ArrayIndex: TLLAArrayIndex): Boolean; reintroduce;
    Function ListFind(Item: @Type@; out ListIndex: TLLAListIndex): Boolean; reintroduce;
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
Result := @Type@(Pointer(GetPayloadPtrByListIndex(ListIndex))^);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.SetItem(ListIndex: TLLAListIndex; Value: @Type@);
begin
SetPayloadPtrByListIndex(ListIndex,@Value);
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

// Method called when a payload is explicitly (ie. using methods Add or Insert)
// added to the list.
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

// Called when payload is copied to the list from an external source (eg. in
// methods Assign and Append).
// Can be used for example to create copies of objects instead of just copying
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
// In default implementation, it calls PayloadCompare and when that returns
// zero, payloads are considered to be equal.
  
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
// If any processing must be done when loading a payload, it must be performed
// here, because PayloadAdded is NOT called.

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

Function @ClassName@.ArrayFind(Item: @Type@; out ArrayIndex: TLLAArrayIndex): Boolean;
begin
Result := inherited ArrayFind(@Item,ArrayIndex);
end;

//------------------------------------------------------------------------------

Function @ClassName@.ListFind(Item: @Type@; out ListIndex: TLLAListIndex): Boolean;
begin
Result := inherited ListFind(@Item,ListIndex);
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
  {$MODESWITCH ClassicProcVars+}
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

  ELLAInvalidValue       = class(ELLAException);
  ELLAIncompatibleObject = class(ELLAException);

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

  TLLAPayload = record end; // zero-size placeholder
  PLLAPayload = ^TLLAPayload;

//------------------------------------------------------------------------------
{
  Following types are intended mainly for debugging purposes where they are
  used for passing of otherwise internal informations.
}
type
  TLLAGlobalInfo = record
    ItemSize:   TMemSize;
    MemorySize: TMemSize;
    Memory:     Pointer;
    FirstFree:  TLLAArrayIndex;
    LastFree:   TLLAArrayIndex;
    FirstUsed:  TLLAArrayIndex;
    LastUsed:   TLLAArrayIndex;
  end;

  TLLAItemInfo = record
    Prev:       TLLAListIndex;
    Next:       TLLAListIndex;
    ListIndex:  TLLAListIndex;
    ArrayIndex: TLLAArrayIndex;
  end;

{===============================================================================
    TLinkedListArray - class declaration
===============================================================================}
type
  TLinkedListArray = class(TCustomListObject)
  protected
    fPayloadSize:       TMemSize;
    fItemSize:          TMemSize;
    fMemorySize:        TMemSize;
    fMemory:            Pointer;
    fCount:             Integer;
    fUpdateCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    fLoading:           Boolean;
    fTempPayload:       Pointer;
    fFirstFree:         TLLAArrayIndex;
    fLastFree:          TLLAArrayIndex;
    fFirstUsed:         TLLAArrayIndex;
    fLastUsed:          TLLAArrayIndex;
    // getters, setters
    Function GetItemPtr_LL(ArrayIndex: TLLAArrayIndex): Pointer; virtual; // does no check index for validity
    Function GetItemPtr(ArrayIndex: TLLAArrayIndex): Pointer; virtual;
    Function GetPayloadPtrByArrayIndex(ArrayIndex: TLLAArrayIndex): PLLAPayload; virtual;
    procedure SetPayloadPtrByArrayIndex(ArrayIndex: TLLAArrayIndex; Value: PLLAPayload); virtual;
    Function GetPayloadPtrByListIndex(ListIndex: TLLAListIndex): PLLAPayload; virtual;
    procedure SetPayloadPtrByListIndex(ListIndex: TLLAListIndex; Value: PLLAPayload); virtual;
    // custom list overrides
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
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
    // changes
    procedure DoChange; virtual;
    // initialization/finalization
    procedure Initialize(PayloadSize: TMemSize); virtual;
    procedure Finalize; virtual;
    procedure InitializeAllItems; virtual;
    procedure FinalizeAllItems; virtual;
    // internals
    procedure InternalDecouple(ArrayIndex: TLLAArrayIndex); virtual;
    procedure InternalDelete(ArrayIndex: TLLAArrayIndex); virtual;
    Function InternalCompatible(List: TLinkedListArray): Boolean; virtual;
    procedure InternalReadFromStream(Stream: TStream; Buffered: Boolean); virtual;
  public
    constructor Create(PayloadSize: TMemSize); overload;
    destructor Destroy; override;
    // updates
    procedure BeginUpdate; virtual;
    Function EndUpdate: Integer; virtual;
    // indices bounds (LowIndex, HighIndex and CheckIndex are returning/accepting list indices)
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function CheckIndex(Index: Integer): Boolean; override;
    Function LowArrayIndex: TLLAArrayIndex; virtual;
    Function HighArrayIndex: TLLAArrayIndex; virtual;
    Function CheckArrayIndex(ArrayIndex: TLLAArrayIndex): Boolean; virtual;
    Function LowListIndex: TLLAListIndex; virtual;
    Function HighListIndex: TLLAListIndex; virtual;
    Function CheckListIndex(ListIndex: TLLAListIndex): Boolean; virtual;
    // indices conversion and manipulation
    Function GetArrayIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    Function GetListIndex(ArrayIndex: TLLAArrayIndex): TLLAListIndex; virtual;
    // previous/next item in the linked list
  {
    Following will return an array index of previous or next item, where item
    can be selected either by its array or list index.
  }
    Function PreviousFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex; virtual;
    Function NextFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex; virtual;
    Function PreviousFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    Function NextFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex; virtual;
    // first/last item
    // returns array index of first listed item, -1 when none is listed
    Function FirstArrayIndex: TLLAArrayIndex; virtual;
    // returns array index of last listed item, -1 when none is listed
    Function LastArrayIndex: TLLAArrayIndex; virtual;
    Function First: PLLAPayload; virtual;
    Function Last: PLLAPayload; virtual;
    // searching
    Function IndicesOf(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean; virtual;
    Function ArrayIndexOf(Item: PLLAPayload): TLLAArrayIndex; virtual;
    Function ListIndexOf(Item: PLLAPayload): TLLAListIndex; virtual;
    Function ArrayFind(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex): Boolean; virtual;
    Function ListFind(Item: PLLAPayload; out ListIndex: TLLAListIndex): Boolean; virtual;
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
    // debugging
    procedure DbgGetGlobalInfo(out GlobalInfo: TLLAGlobalInfo); virtual;
    Function DbgGetArrayItemInfo(ArrayIndex: TLLAArrayIndex; out ItemInfo: TLLAItemInfo): Boolean; virtual;
    Function DbgGetListItemInfo(ListIndex: TLLAListIndex; out ItemInfo: TLLAItemInfo): Boolean; virtual;
    Function DbgArrayItemIsUsed(ArrayIndex: TLLAArrayIndex): Boolean; virtual;
    // properties
    property PayloadSize: TMemSize read fPayloadSize;
    property ArrayPointers[ArrayIndex: TLLAArrayIndex]: PLLAPayload read GetPayloadPtrByArrayIndex;
    property ListPointers[ListIndex: TLLAListIndex]: PLLAPayload read GetPayloadPtrByListIndex;
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
    Function ArrayFind(Item: Integer; out ArrayIndex: TLLAArrayIndex): Boolean; reintroduce;
    Function ListFind(Item: Integer; out ListIndex: TLLAListIndex): Boolean; reintroduce;
    Function Add(Item: Integer; out ArrayIndex: TLLAArrayIndex): TLLAListIndex; reintroduce; overload;
    Function Add(Item: Integer): TLLAListIndex; reintroduce; overload;
    procedure Insert(ListIndex: TLLAListIndex; Item: Integer); reintroduce;
    Function Extract(Item: Integer): Integer; reintroduce;
    Function Remove(Item: Integer): Integer; reintroduce;
    property Items[ListIndex: TLLAListIndex]: Integer read GetItem write SetItem; default;
  end;

implementation

uses
  ListSorters, StrRect, BinaryStreamingLite;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                TLinkedListArray
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLinkedListArray - internals
===============================================================================}
type
  TLLAItem = record
    Prev:       TLLAArrayIndex;
    Next:       TLLAArrayIndex;
  {
    Negative ListIndex marks an unused item.
  }
    ListIndex:  TLLAListIndex;
  {
    ArrayIndex field does not belong to the actual item it is stored in. It
    belongs to an item with list index equal to array index of storing item.

    This way, a list-array indices map is constructed for fast access when
    using list index - when an item of specific list index is requested, its
    array index is stored at item whose array index is equal to the requested
    list index.
  }
    ArrayIndex: TLLAArrayIndex;
    Payload:    TLLAPayload;
  end;
  PLLAItem = ^TLLAItem;

{===============================================================================
    TLinkedListArray - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TLinkedListArray - protected methods
-------------------------------------------------------------------------------}

Function TLinkedListArray.GetItemPtr_LL(ArrayIndex: TLLAArrayIndex): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(fMemory) + (PtrUInt(ArrayIndex) * PtrUInt(fItemSize)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetItemPtr(ArrayIndex: TLLAArrayIndex): Pointer;
begin
If CheckArrayIndex(ArrayIndex) then
  Result := GetItemPtr_LL(ArrayIndex)
else
  raise ELLAArrayIndexOutOfBounds.CreateFmt('TLinkedListArray.GetItemPtr: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetPayloadPtrByArrayIndex(ArrayIndex: TLLAArrayIndex): PLLAPayload;
begin
Result := Addr(PLLAItem(GetItemPtr(ArrayIndex))^.Payload);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetPayloadPtrByArrayIndex(ArrayIndex: TLLAArrayIndex; Value: PLLAPayload);
var
  PayloadPtr: PLLAPayload;
begin
PayloadPtr := GetPayloadPtrByArrayIndex(ArrayIndex);  // raises exception on invalid index
System.Move(PayloadPtr^,fTempPayload^,fPayloadSize);
System.Move(Value^,PayloadPtr^,fPayloadSize);
If not PayloadEquals(fTempPayload,Value) then
  DoChange;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetPayloadPtrByListIndex(ListIndex: TLLAListIndex): PLLAPayload;
begin
If CheckListIndex(ListIndex) then
  Result := GetPayloadPtrByArrayIndex(GetArrayIndex(ListIndex))
else
  raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.GetPayloadPtrByListIndex: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetPayloadPtrByListIndex(ListIndex: TLLAListIndex; Value: PLLAPayload);
begin
If CheckListIndex(ListIndex) then
  SetPayloadPtrByArrayIndex(GetArrayIndex(ListIndex),Value)
else
  raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.SetPayloadPtrByListIndex: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetCapacity: Integer;
begin
Result := fMemorySize div fItemSize;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.SetCapacity(Value: Integer);
var
  OldCapacity:  Integer;
  i:            TLLAArrayIndex;
  ItemPtr:      PLLAItem;
begin
If Value >= 0 then
  begin
    If Value <> Capacity then
      begin
        If Value > Capacity then
          begin
            // increasing capacity, add new free items
            OldCapacity := Capacity;
            fMemorySize := TMemSize(Value) * fItemSize;
            ReallocMem(fMemory,fMemorySize);
            For i := TLLAArrayIndex(OldCapacity) to HighArrayIndex do
              with PLLAItem(GetItemPtr_LL(i))^ do
                begin
                  Prev := Pred(i);
                  Next := Succ(i);
                  ListIndex := -1;
                  ArrayIndex := -1;
                end;
            PLLAItem(GetItemPtr(HighArrayIndex))^.Next := -1;
            If not CheckArrayIndex(fFirstFree) then
              fFirstFree := OldCapacity;
            If CheckArrayIndex(fLastFree) then
              begin
                PLLAItem(GetItemPtr_LL(fLastFree))^.Next := OldCapacity;
                PLLAItem(GetItemPtr(OldCapacity))^.Prev := fLastFree;
              end
            else PLLAItem(GetItemPtr(OldCapacity))^.Prev := -1;
            fLastFree := HighArrayIndex;
          end
        else
          begin
            // capacity is decreased, remove existing items
            BeginUpdate;  // defer change notifications from Defragment
            try
            {
              Defragment ensures that all used items are at the beginning of
              the array and that all items are in the order they apear in their
              linked lists.
            }
              Defragment;
              If Value < fCount then
                begin
                  // removing some used items
                  If not fLoading then
                    For i := HighArrayIndex downto TLLAArrayIndex(Value) do
                      begin
                        ItemPtr := PLLAItem(GetItemPtr_LL(i));
                        If CheckListIndex(ItemPtr^.ListIndex) then
                          PayloadFinal(Addr(ItemPtr^.Payload));
                      end;
                  fMemorySize := TMemSize(Value) * fItemSize;
                  ReallocMem(fMemory,fMemorySize);
                  fCount := Value;
                  If Value > 0 then
                    PLLAItem(GetItemPtr_LL(HighArrayIndex))^.Next := -1;
                  // there is no free item anymore
                  fFirstFree := -1;
                  fLastFree := -1;
                  If Value <= 0 then
                    fFirstUsed := -1;
                  fLastUsed := HighArrayIndex;  // -1 for empty array
                  DoChange;
                end
              else
                begin
                  // removing only unused items
                  fMemorySize := TMemSize(Value) * fItemSize;
                  ReallocMem(fMemory,fMemorySize);
                  If Value > 0 then
                    PLLAItem(GetItemPtr_LL(HighArrayIndex))^.Next := -1;
                  If Value = fCount then
                    begin
                      fFirstFree := -1;
                      fLastFree := -1;
                    end
                  else fLastFree := HighArrayIndex;
                end;
            finally
              EndUpdate;
            end;
          end;
      end;
  end
else raise ELLAInvalidValue.CreateFmt('TLinkedListArray.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetCount: Integer;
begin
Result := fCount;
end;
 
//------------------------------------------------------------------------------

procedure TLinkedListArray.SetCount(Value: Integer);
var
  ListIndex:  TLLAListIndex;
  ArrayIndex: TLLAArrayIndex;
  TempIndex:  TLLAArrayIndex;
  ItemPtr:    PLLAItem;
begin
If Value >= 0 then
  begin
    If Value <> fCount then
      begin
        If Value > fCount then
          begin
            // new items will be added
            If Value > Capacity then
              SetCapacity(Value);
            PLLAItem(GetItemPtr(fFirstFree))^.Prev := fLastUsed;
            If CheckArrayIndex(fLastUsed) then
              PLLAItem(GetItemPtr_LL(fLastUsed))^.Next := fFirstFree;
            If not CheckArrayIndex(fFirstUsed) then
              fFirstUsed := fFirstFree;
            ListIndex := TLLAListIndex(fCount);
            ArrayIndex := fFirstFree;
            repeat
              ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
              ItemPtr^.ListIndex := ListIndex;
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := ArrayIndex;
              If not fLoading then
                PayloadInit(Addr(ItemPtr^.Payload));
              fLastUsed := ArrayIndex;
              ArrayIndex := ItemPtr^.Next;
              fFirstFree := ArrayIndex;
              Inc(ListIndex);
            until ListIndex >= Value;   
            PLLAItem(GetItemPtr(fLastUsed))^.Next := -1;
            If CheckArrayIndex(fFirstFree) then
              PLLAItem(GetItemPtr_LL(fFirstFree))^.Prev := -1
            else
              fLastFree := -1;
          end
        else
          begin
            // items will be removed
            If CheckArrayIndex(fLastFree) then
              PLLAItem(GetItemPtr_LL(fLastFree))^.Next := fLastUsed;
            If not CheckArrayIndex(fFirstFree) then
              fFirstFree := fLastUsed;
            ListIndex := HighListIndex;
            ArrayIndex := fLastUsed;
            repeat
              ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
              If not fLoading then
                PayloadFinal(Addr(ItemPtr^.Payload));
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := -1;
              ItemPtr^.ListIndex := -1;
              TempIndex := fLastFree;
              fLastFree := ArrayIndex;
              ArrayIndex := ItemPtr^.Prev;
              ItemPtr^.Prev := TempIndex;
              ItemPtr^.Next := ArrayIndex;
              fLastUsed := ArrayIndex;
              Dec(ListIndex);
            until ListIndex < Value;   
            PLLAItem(GetItemPtr(fLastFree))^.Next := -1;
            If CheckArrayIndex(fLastUsed) then
              PLLAItem(GetItemPtr_LL(fLastUsed))^.Next := -1
            else
              fFirstUsed := -1;            
          end;
        fCount := Value;
        DoChange;
      end;
  end
else raise ELLAInvalidValue.CreateFmt('TLinkedListArray.SetCount: Invalid count (%d).',[Value]);
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
Result := PayloadCompare(GetPayloadPtrByListIndex(TLLAListIndex(ListIndex1)),
                         GetPayloadPtrByListIndex(TLLAListIndex(ListIndex2)));
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
Item1Ptr := GetItemPtr_LL(TLLAArrayIndex(Index1));
Item2Ptr := GetItemPtr_LL(TLLAArrayIndex(Index2));
If Item1Ptr^.ListIndex < Item2Ptr^.ListIndex then
  Result := -1
else If Item1Ptr^.ListIndex > Item2Ptr^.ListIndex then
  Result := +1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.DefragExchange(Index1,Index2: Integer);
var
  Item1Ptr,Item2Ptr:  PLLAItem;
  Temp:               TLLAArrayIndex;
begin
If Index1 <> Index2 then
  begin
    Item1Ptr := GetItemPtr_LL(TLLAArrayIndex(Index1));
    Item2Ptr := GetItemPtr_LL(TLLAArrayIndex(Index2));
    // exchange indices
    Temp := Item1Ptr^.ListIndex;
    Item1Ptr^.ListIndex := Item2Ptr^.ListIndex;
    Item2Ptr^.ListIndex := Temp;
    // exchange data
    System.Move(Item1Ptr^.Payload,fTempPayload^,fPayloadSize);
    System.Move(Item2Ptr^.Payload,Item1Ptr^.Payload,fPayloadSize);
    System.Move(fTempPayload^,Item2Ptr^.Payload,fPayloadSize);
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

procedure TLinkedListArray.Initialize(PayloadSize: TMemSize);
begin
fPayloadSize := PayloadSize;
fItemSize := SizeOf(TLLAItem) + fPayloadSize;
fMemorySize := 0;
fMemory := nil;
fCount := 0;
fUpdateCounter := 0;
fChanged := False;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
fLoading := False;
fTempPayload := AllocMem(fPayloadSize);
fFirstFree := -1;
fLastFree := -1;
fFirstUsed := -1;
fLastUsed := -1;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Finalize;
begin
FinalizeAllItems;
FreeMem(fTempPayload,fPayloadSize);
FreeMem(fMemory,fMemorySize);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.InitializeAllItems;
var
  i:  Integer;
begin
// all items are made part of list of free items, irrespective of their state
If Capacity > 0 then
  begin
    For i := LowArrayIndex to HighArrayIndex do
      with PLLAItem(GetItemPtr_LL(i))^ do
        begin
          Prev := Pred(i);
          Next := Succ(i);
          ListIndex := -1;
          ArrayIndex := -1;
        end;
    PLLAItem(GetItemPtr(LowArrayIndex))^.Prev := -1;
    PLLAItem(GetItemPtr(HighArrayIndex))^.Next := -1;
    fFirstFree := LowArrayIndex;
    fLastFree := HighArrayIndex;
  end
else
  begin
    fFirstFree := -1;
    fLastFree := -1;
  end;
fFirstUsed := -1;
fLastUsed := -1;
fCount := 0;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.FinalizeAllItems;
var
  i:        TLLAArrayIndex;
  ItemPtr:  PLLAItem;
begin
// PayloadFinal is called for all used items, nothing else is done
If fCount > 0 then
  For i := LowArrayIndex to HighArrayIndex do
    begin
      ItemPtr := PLLAItem(GetItemPtr_LL(i));
      If CheckListIndex(ItemPtr^.ListIndex) then
        PayloadFinal(Addr(ItemPtr^.Payload));
    end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.InternalDecouple(ArrayIndex: TLLAArrayIndex);
var
  ItemPtr:  PLLAItem;
begin
ItemPtr := PLLAItem(GetItemPtr(ArrayIndex));
If CheckArrayIndex(ItemPtr^.Prev) then
  PLLAItem(GetItemPtr_LL(ItemPtr^.Prev))^.Next := ItemPtr^.Next;
If CheckArrayIndex(ItemPtr^.Next) then
  PLLAItem(GetItemPtr_LL(ItemPtr^.Next))^.Prev := ItemPtr^.Prev;
If fFirstFree = ArrayIndex then
  fFirstFree := ItemPtr^.Next;
If fLastFree = ArrayIndex then
  fLastFree := ItemPtr^.Prev;
If fFirstUsed = ArrayIndex then
  fFirstUsed := ItemPtr^.Next;
If fLastUsed = ArrayIndex then
  fLastUsed := ItemPtr^.Prev;
ItemPtr^.Prev := -1;
ItemPtr^.Next := -1;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.InternalDelete(ArrayIndex: TLLAArrayIndex);
var
  ItemPtr:  PLLAItem;
  Index:    Integer;
begin
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
    If CheckListIndex(ItemPtr^.ListIndex) then
      begin
        PayloadFinal(Addr(ItemPtr^.Payload));
        // reindex
        Index := ItemPtr^.Next;
        while CheckArrayIndex(Index) do
          with PLLAItem(GetItemPtr_LL(Index))^ do
            begin
              Dec(ListIndex);
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := Index;
              Index := Next;
            end;
        PLLAItem(GetItemPtr(TLLAArrayIndex(HighListIndex)))^.ArrayIndex := -1;     
        // remove from list of used items
        InternalDecouple(ArrayIndex);
        // add to list of free items    
        ItemPtr^.Prev := fLastFree;
        ItemPtr^.Next := -1;
        If CheckArrayIndex(fLastFree) then
          PLLAItem(GetItemPtr_LL(fLastFree))^.Next := ArrayIndex;
        If not CheckArrayIndex(fFirstFree) then
          fFirstFree := ArrayIndex;
        fLastFree := ArrayIndex;
        // final touches
        ItemPtr^.ListIndex := -1;
        Dec(fCount);
        Shrink;
        DoChange;
      end;
  end
else raise ELLAArrayIndexOutOfBounds.CreateFmt('TLinkedListArray.InternalDelete: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.InternalCompatible(List: TLinkedListArray): Boolean;
begin
Result := ((List is Self.ClassType) or (Self is List.ClassType)) and (fPayloadSize = List.PayloadSize);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.InternalReadFromStream(Stream: TStream; Buffered: Boolean);
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
              PayloadPtr := GetPayloadPtrByArrayIndex(ArrayIndex);
              PayloadRead(PayloadPtr,BufferStream);
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
            PayloadPtr := GetPayloadPtrByArrayIndex(ArrayIndex);
            PayloadRead(PayloadPtr,Stream);
            ArrayIndex := NextFromArrayIndex(ArrayIndex);
          end;
      end;
  end;
end;

{-------------------------------------------------------------------------------
    TLinkedListArray - public methods
-------------------------------------------------------------------------------}

constructor TLinkedListArray.Create(PayloadSize: TMemSize);
begin
inherited Create;
Initialize(PayloadSize);
end;

//------------------------------------------------------------------------------

destructor TLinkedListArray.Destroy;
begin
Finalize;
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

Function TLinkedListArray.CheckIndex(Index: Integer): Boolean;
begin
Result := CheckListIndex(TLLAListIndex(Index));
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.LowArrayIndex: TLLAArrayIndex;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.HighArrayIndex: TLLAArrayIndex;
begin
Result := Pred(Capacity);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.CheckArrayIndex(ArrayIndex: TLLAArrayIndex): Boolean;
begin
Result := (ArrayIndex >= LowArrayIndex) and (ArrayIndex <= HighArrayIndex);
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

Function TLinkedListArray.CheckListIndex(ListIndex: TLLAListIndex): Boolean;
begin
Result := (ListIndex >= LowListIndex) and (ListIndex <= HighListIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetArrayIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
If CheckListIndex(ListIndex) then
  begin
    If CheckArrayIndex(TLLAArrayIndex(ListIndex)) then
      Result := PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex
    else
      Result := -1;
  end
else raise ELLAListIndexOutOfbounds.CreateFmt('TLinkedListArray.GetArrayIndex: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.GetListIndex(ArrayIndex: TLLAArrayIndex): TLLAListIndex;
begin
If CheckArrayIndex(ArrayIndex) then
  Result := PLLAItem(GetItemPtr_LL(ArrayIndex))^.ListIndex
else
  raise ELLAArrayIndexOutOfbounds.CreateFmt('TLinkedListArray.GetListIndex: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PreviousFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex;
var
  ItemPtr:  PLLAItem;
begin
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
    If CheckListIndex(ItemPtr^.ListIndex) then
      Result := ItemPtr^.Prev
    else
      Result := -1;
  end
else raise ELLAArrayIndexOutOfbounds.CreateFmt('TLinkedListArray.PreviousFromArrayIndex: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.NextFromArrayIndex(ArrayIndex: TLLAArrayIndex): TLLAArrayIndex;
var
  ItemPtr:  PLLAItem;
begin
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
    If CheckListIndex(ItemPtr^.ListIndex) then
      Result := ItemPtr^.Next
    else
      Result := -1;      
  end
else raise ELLAArrayIndexOutOfbounds.CreateFmt('TLinkedListArray.NextFromArrayIndex: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.PreviousFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
If CheckListIndex(ListIndex) then
  Result := PLLAItem(GetItemPtr(GetArrayIndex(ListIndex)))^.Prev
else
  raise ELLAListIndexOutOfbounds.CreateFmt('TLinkedListArray.PreviousFromListIndex: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.NextFromListIndex(ListIndex: TLLAListIndex): TLLAArrayIndex;
begin
If CheckListIndex(ListIndex) then
  Result := PLLAItem(GetItemPtr(GetArrayIndex(ListIndex)))^.Next
else
  raise ELLAListIndexOutOfbounds.CreateFmt('TLinkedListArray.NextFromListIndex: List index (%d) out of bounds.',[ListIndex]);
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
Result := GetPayloadPtrByArrayIndex(fFirstUsed);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Last: PLLAPayload;
begin
Result := GetPayloadPtrByArrayIndex(fLastUsed);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.IndicesOf(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex; out ListIndex: TLLAListIndex): Boolean;
var
  ItemPtr:  PLLAItem;
begin
Result := False;
ArrayIndex := fFirstUsed;
// traverse linked list
while CheckArrayIndex(ArrayIndex) do
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
    If PayloadEquals(Item,Addr(ItemPtr^.Payload)) then
      begin
        ListIndex := ItemPtr^.ListIndex;
        Result := True;
        Break{while...};
      end
    else ArrayIndex := ItemPtr^.Next;
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

Function TLinkedListArray.ArrayFind(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex): Boolean;
var
  ListIndex:  TLLAListIndex;
begin
Result := IndicesOf(Item,ArrayIndex,ListIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.ListFind(Item: PLLAPayload; out ListIndex: TLLAListIndex): Boolean;
var
  ArrayIndex: TLLAArrayIndex;
begin
Result := IndicesOf(Item,ArrayIndex,ListIndex);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Add(Item: PLLAPayload; out ArrayIndex: TLLAArrayIndex): TLLAListIndex;
var
  ItemPtr:  PLLAItem;
begin
Grow;
// at this point, there MUST be at least one free item
ArrayIndex := fFirstFree;
ItemPtr := PLLAItem(GetItemPtr(ArrayIndex));
// remove from list of free items
InternalDecouple(ArrayIndex);
// add to list of used items
If not CheckArrayIndex(fFirstUsed) then
  fFirstUsed := ArrayIndex;
If CheckArrayIndex(fLastUsed) then
  PLLAItem(GetItemPtr_LL(fLastUsed))^.Next := ArrayIndex;
ItemPtr^.Prev := fLastUsed;
ItemPtr^.Next := -1;
fLastUsed := ArrayIndex;
// add the data and set indices
Result := fCount;
System.Move(Item^,ItemPtr^.Payload,fPayloadSize);
ItemPtr^.ListIndex := Result;
PLLAItem(GetItemPtr(TLLAArrayIndex(Result)))^.ArrayIndex := ArrayIndex;
PayloadAdded(Addr(ItemPtr^.Payload));
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
  NewItemArrayIndex:  TLLAArrayIndex;
  OldItemArrayIndex:  TLLAArrayIndex;
  NewItemPtr:         PLLAItem;
  OldItemPtr:         PLLAItem;
  ReArrayIndex:       TLLAArrayIndex; // reindexing
begin
If CheckListIndex(ListIndex) then
  begin
    OldItemArrayIndex := GetArrayIndex(ListIndex);
    // following check should always succeed (unless there is some bug)
    If CheckArrayIndex(OldItemArrayIndex) then
      begin
        Grow;
        NewItemArrayIndex := fFirstFree;
        NewItemPtr := PLLAItem(GetItemPtr(NewItemArrayIndex));
        OldItemPtr := PLLAItem(GetItemPtr(OldItemArrayIndex));
        InternalDecouple(NewItemArrayIndex);
        // insert to the list of used items
        NewItemPtr^.Prev := OldItemPtr^.Prev;
        NewItemPtr^.Next := OldItemArrayIndex;
        If CheckArrayIndex(OldItemPtr^.Prev) then
          PLLAItem(GetItemPtr_LL(OldItemPtr^.Prev))^.Next := NewItemArrayIndex;
        OldItemPtr^.Prev := NewItemArrayIndex;
        If fFirstUsed = OldItemArrayIndex then
          fFirstUsed := NewItemArrayIndex;
        // reindex 
        NewItemPtr^.ListIndex := ListIndex;
        PLLAItem(GetItemPtr(TLLAArrayIndex(ListIndex)))^.ArrayIndex := NewItemArrayIndex;
        ReArrayIndex := OldItemArrayIndex;
        while CheckArrayIndex(ReArrayIndex) do
          with PLLAItem(GetItemPtr_LL(ReArrayIndex))^ do
            begin
              Inc(ListIndex);
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := ReArrayIndex;
              ReArrayIndex := Next;
            end;
        // add data
        System.Move(Item^,NewItemPtr^.Payload,fPayloadSize);
        PayloadAdded(Addr(NewItemPtr^.Payload));
        Inc(fCount);
        DoChange;        
      end;
  end
else If ListIndex = fCount then
  Add(Item)
else
  raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.Insert: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Move(SrcListIndex,DstListIndex: TLLAListIndex);
var
  SrcArrayIndex,DstArrayIndex:  TLLAArrayIndex;
  SrcItemPtr,DstItemPtr:        PLLAItem;
  ReListIndex:                  TLLAListIndex;
  ReArrayIndex:                 TLLAArrayIndex;
begin
If SrcListIndex <> DstListIndex then
  begin
    If not CheckListIndex(SrcListIndex) then
      raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.Move: Source list index (%d) out of bounds.',[SrcListIndex]);
    If not CheckListIndex(DstListIndex) then
      raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.Move: Destination list index (%d) out of bounds.',[DstListIndex]);
    // get pointers
    SrcArrayIndex := GetArrayIndex(SrcListIndex);
    DstArrayIndex := GetArrayIndex(DstListIndex);
    SrcItemPtr := PLLAItem(GetItemPtr(SrcArrayIndex));
    DstItemPtr := PLLAItem(GetItemPtr(DstArrayIndex));
    // insert to new position
    If DstListIndex > SrcListIndex then
      begin
        // following must be here!
        ReArrayIndex := SrcItemPtr^.Next;
        InternalDecouple(SrcArrayIndex);
        // item is moved up
        SrcItemPtr^.Prev := DstArrayIndex;
        SrcItemPtr^.Next := DstItemPtr^.Next;
        If CheckArrayIndex(DstItemPtr^.Next) then
          PLLAItem(GetItemPtr_LL(DstItemPtr^.Next))^.Prev := SrcArrayIndex;
        DstItemPtr^.Next := SrcArrayIndex;
        If fLastUsed = DstArrayIndex then
          fLastUsed := SrcArrayIndex;
        // reindex
        ReListIndex := SrcListIndex;
        while ReListIndex <= DstListIndex do
          with PLLAItem(GetItemPtr_LL(ReArrayIndex))^ do
            begin
              ListIndex := ReListIndex;
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ReListIndex)))^.ArrayIndex := ReArrayIndex;
              ReArrayIndex := Next;
              Inc(ReListIndex);
            end;
      end
    else
      begin
        InternalDecouple(SrcArrayIndex);
        // item is moved down
        SrcItemPtr^.Prev := DstItemPtr^.Prev;
        SrcItemPtr^.Next := DstArrayIndex;
        If CheckArrayIndex(DstItemPtr^.Prev) then
          PLLAItem(GetItemPtr_LL(DstItemPtr^.Prev))^.Next := SrcArrayIndex;
        DstItemPtr^.Prev := SrcArrayIndex;
        If fFirstUsed = DstArrayIndex then
          fFirstUsed := SrcArrayIndex;
        // reindex
        ReListIndex := DstListIndex;
        ReArrayIndex := SrcArrayIndex;
        while ReListIndex <= SrcListIndex do
          with PLLAItem(GetItemPtr_LL(ReArrayIndex))^ do
            begin
              ListIndex := ReListIndex;
              PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ReListIndex)))^.ArrayIndex := ReArrayIndex;
              ReArrayIndex := Next;
              Inc(ReListIndex);
            end;
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
    If not CheckListIndex(ListIndex1) then
      raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.Exchange: First list index (%d) out of bounds.',[ListIndex1]);
    If not CheckListIndex(ListIndex2) then
      raise ELLAListIndexOutOfBounds.CreateFmt('TLinkedListArray.Exchange: Second list index (%d) out of bounds.',[ListIndex2]);
    // simplify the exchange by assuring the indices are ordered
    If ListIndex2 < ListIndex1 then
      begin
        TempListIndex := ListIndex1;
        ListIndex1 := ListIndex2;
        ListIndex2 := TempListIndex;
      end;
    // get pointers to items
    ArrayIndex1 := GetArrayIndex(ListIndex1);
    ArrayIndex2 := GetArrayIndex(ListIndex2);
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
    If (ListIndex2 - ListIndex1) > 1 then
      begin
         // remote items
        If CheckArrayIndex(ItemPtr1^.Prev) then
          PLLAItem(GetItemPtr_LL(ItemPtr1^.Prev))^.Next := ArrayIndex2;
        If CheckArrayIndex(ItemPtr1^.Next) then
          PLLAItem(GetItemPtr_LL(ItemPtr1^.Next))^.Prev := ArrayIndex2;
        If CheckArrayIndex(ItemPtr2^.Prev) then
          PLLAItem(GetItemPtr_LL(ItemPtr2^.Prev))^.Next := ArrayIndex1;
        If CheckArrayIndex(ItemPtr2^.Next) then
          PLLAItem(GetItemPtr_LL(ItemPtr2^.Next))^.Prev := ArrayIndex1;
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
          PLLAItem(GetItemPtr_LL(ItemPtr1^.Prev))^.Next := ArrayIndex2;
        If CheckArrayIndex(ItemPtr2^.Next) then
          PLLAItem(GetItemPtr_LL(ItemPtr2^.Next))^.Prev := ArrayIndex1;
        TempIndex1 := ItemPtr1^.Prev;
        TempIndex2 := ItemPtr2^.Next;
        ItemPtr1^.Prev := ItemPtr1^.Next;
        ItemPtr1^.Next := TempIndex2;
        ItemPtr2^.Next := ItemPtr2^.Prev;
        ItemPtr2^.Prev := TempIndex1;
      end;
    // reindex
    ItemPtr1^.ListIndex := ListIndex2;
    PLLAItem(GetItemPtr(TLLAArrayIndex(ListIndex2)))^.ArrayIndex := ArrayIndex1;
    ItemPtr2^.ListIndex := ListIndex1;
    PLLAItem(GetItemPtr(TLLAArrayIndex(ListIndex1)))^.ArrayIndex := ArrayIndex2;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.Extract(Item: PLLAPayload): PLLAPayload;
var
  ArrayIndex: TLLAArrayIndex;
  ItemPtr:    PLLAItem;
  Index:      Integer;
begin
If ArrayFind(Item,ArrayIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr(ArrayIndex));
    System.Move(ItemPtr^.Payload,fTempPayload^,fPayloadSize);
    Result := fTempPayload;
    // reindex
    Index := ItemPtr^.Next;
    while CheckArrayIndex(Index) do
      with PLLAItem(GetItemPtr_LL(Index))^ do
        begin
          Dec(ListIndex);
          PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := Index;
          Index := Next;
        end;
    PLLAItem(GetItemPtr(TLLAArrayIndex(HighListIndex)))^.ArrayIndex := -1;
    // remove from list of used items
    InternalDecouple(ArrayIndex);
    // add to list of free items
    ItemPtr^.Prev := fLastFree;
    ItemPtr^.Next := -1;
    If CheckArrayIndex(fLastFree) then
      PLLAItem(GetItemPtr_LL(fLastFree))^.Next := ArrayIndex;
    If not CheckArrayIndex(fFirstFree) then
      fFirstFree := ArrayIndex;
    fLastFree := ArrayIndex;
    // final touches
    ItemPtr^.ListIndex := -1;
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
If CheckListIndex(ListIndex) then
  InternalDelete(GetArrayIndex(ListIndex))
else
  raise ELLAListIndexOutOfbounds.CreateFmt('TLinkedListArray.Delete: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Clear;
begin
If Capacity > 0 then
  begin
    FinalizeAllItems;
    InitializeAllItems;
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
      with PLLAItem(GetItemPtr_LL(TempIndex))^ do
        begin
          ListIndex := Pred(TLLAListIndex(fCount) - ListIndex);
          PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ListIndex)))^.ArrayIndex := TempIndex;
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
  UnusedIndex:  TLLAArrayIndex;
  UsedIndex:    TLLAArrayIndex;
  UnusedPtr:    PLLAItem;
  UsedPtr:      PLLAItem;
  i:            TLLAArrayIndex;
begin
If fCount > 0 then
  begin
  {
    Compress items - move them all down in the array, so there is no unused
    item between used ones.
  }
    If fCount <> Capacity then
      begin
        UnusedIndex := LowArrayIndex;
        UsedIndex := HighArrayIndex;
        repeat
          // find first empty item
          while CheckArrayIndex(UnusedIndex) do
            If CheckListIndex(PLLAItem(GetItemPtr_LL(UnusedIndex))^.ListIndex) then
              Inc(UnusedIndex)
            else
              Break{while};
          // find last used item
          while CheckArrayIndex(UsedIndex) do
            If not CheckListIndex(PLLAItem(GetItemPtr_LL(UsedIndex))^.ListIndex) then
              Dec(UsedIndex)
            else
              Break{while};
          // move item to empty space    
          If CheckArrayIndex(UnusedIndex) and CheckArrayIndex(UsedIndex) and (UnusedIndex < UsedIndex) then
            begin
              UnusedPtr := PLLAItem(GetItemPtr_LL(UnusedIndex));
              UsedPtr := PLLAItem(GetItemPtr_LL(UsedIndex));
              UnusedPtr^.ListIndex := UsedPtr^.ListIndex;
              System.Move(UsedPtr^.Payload,UnusedPtr^.Payload,fPayloadSize);
              // do not copy other indices, they will be replaced in the last step
              Inc(UnusedIndex);
              Dec(UsedIndex);
            end
          else Break{Repeat};
        until UnusedIndex >= UsedIndex;
      end;
    // sort items
    with TListQuickSorter.Create(DefragCompare,DefragExchange) do
    try
      Sort(LowArrayIndex,HighListIndex);
    finally
      Free;
    end;
    // reinitialize all indices, first for used...
    fFirstUsed := LowArrayIndex;
    fLastUsed := TLLAArrayIndex(HighListIndex);
    For i := fFirstUsed to fLastUsed do
      with PLLAItem(GetItemPtr_LL(i))^ do
        begin
          Prev := Pred(i);
          Next := Succ(i);
          ArrayIndex := i;
        end;
    PLLAItem(GetItemPtr_LL(fFirstUsed))^.Prev := -1;
    PLLAItem(GetItemPtr_LL(fLastUsed))^.Next := -1;
    // and now for unused, if the are any...
    If Capacity > fCount then
      begin
        fFirstFree := TLLAArrayIndex(Succ(HighListIndex));
        fLastFree := HighArrayIndex;
        For i := fFirstFree to fLastFree do
          with PLLAItem(GetItemPtr_LL(i))^ do
            begin
              Prev := Pred(i);
              Next := Succ(i);
              ListIndex := -1;
              ArrayIndex := -1;
            end;
        PLLAItem(GetItemPtr_LL(fFirstFree))^.Prev := -1;
        PLLAItem(GetItemPtr_LL(fLastFree))^.Next := -1;
      end;
    DoChange;
  end
else InitializeAllItems;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.IsEqual(List: TLinkedListArray): Boolean;
var
  i:              TLLAListIndex;
  SelfArrayIndex: TLLAArrayIndex;
  ListArrayIndex: TLLAArrayIndex;
begin
If InternalCompatible(List) then
  begin
    Result := False;
    If List.Count = fCount then
      begin
        SelfArrayIndex := Self.FirstArrayIndex;
        ListArrayIndex := List.FirstArrayIndex;
        For i := LowListIndex to HighListIndex do
          If PayloadEquals(Self.ArrayPointers[SelfArrayIndex],List.ArrayPointers[ListArrayIndex]) then
            begin
              SelfArrayIndex := Self.NextFromArrayIndex(SelfArrayIndex);
              ListArrayIndex := List.NextFromArrayIndex(ListArrayIndex);
            end
          else Exit;
        Result := True;
      end;
  end
else raise ELLAIncompatibleObject.CreateFmt('TLinkedListArray.IsEqual: Incompatible list (%s(%d)).',[List.ClassName,List.PayloadSize]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Assign(List: TLinkedListArray);
var
  i:          TLLAListIndex;
  ArrayIndex: TLLAArrayIndex;
  ItemPtr:    PLLAItem;
begin
If InternalCompatible(List) then
  begin
    FinalizeAllItems;
    InitializeAllItems;
    If List.Count > 0 then
      begin
        // expand capacity if necessary
        If Capacity < List.Count then
          SetCapacity(List.Count);
        // copy payloads
        ArrayIndex := List.FirstArrayIndex;
        For i := List.LowListIndex to List.HighListIndex do
          begin
            ItemPtr := PLLAItem(GetItemPtr_LL(TLLAArrayIndex(i)));
            ItemPtr^.ListIndex := i;
            ItemPtr^.ArrayIndex := TLLAArrayIndex(i);
            PayloadCopy(List.ArrayPointers[ArrayIndex],Addr(ItemPtr^.Payload));
            ArrayIndex := List.NextFromArrayIndex(ArrayIndex);
          end;
        fCount := List.Count; // must be set here
        // set indices        
        fFirstUsed := LowArrayIndex;
        fLastUsed := TLLAArrayIndex(HighListIndex);
        If Capacity > fCount then
          begin
            fFirstFree := TLLAArrayIndex(Succ(HighListIndex));
            fLastFree := HighArrayIndex;
          end
        else
          begin
            fFirstFree := -1;
            fLastFree := -1;
          end;
        PLLAItem(GetItemPtr(fFirstUsed))^.Prev := -1;
        PLLAItem(GetItemPtr(fLastUsed))^.Next := -1;
        If CheckArrayIndex(fFirstFree) then
          PLLAItem(GetItemPtr_LL(fFirstFree))^.Prev := -1;
        If CheckArrayIndex(fLastFree) then
          PLLAItem(GetItemPtr_LL(fLastFree))^.Next := -1;
      end;
    DoChange;
  end
else raise ELLAIncompatibleObject.CreateFmt('TLinkedListArray.Assign: Incompatible list (%s(%d)).',[List.ClassName,List.PayloadSize]);
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.Append(List: TLinkedListArray);
var
  i:              TLLAListIndex;
  SelfArrayIndex: TLLAArrayIndex;
  ListArrayIndex: TLLAArrayIndex;
  ItemPtr:        PLLAItem;
begin
If InternalCompatible(List) then
  begin
    If List.Count > 0 then
      begin
        // following code should work even if List is self
        If (fCount + List.Count) > Capacity then
          SetCapacity(fCount + List.Count);
        SelfArrayIndex := fFirstFree;
        ListArrayIndex := List.FirstArrayIndex;
        If CheckArrayIndex(fLastUsed) then
          PLLAItem(GetItemPtr_LL(fLastUsed))^.Next := SelfArrayIndex;
        If not CheckArrayIndex(fFirstUsed) then
          fFirstUsed := SelfArrayIndex;
        For i := List.LowListIndex to List.HighListIndex do
          begin
            ItemPtr := PLLAItem(GetItemPtr_LL(SelfArrayIndex));
            ItemPtr^.ListIndex := TLLAListIndex(fCount) + i;
            PLLAItem(GetItemPtr_LL(TLLAArrayIndex(ItemPtr^.ListIndex)))^.ArrayIndex := SelfArrayIndex;
            ItemPtr^.Prev := fLastUsed;
            fLastUsed := SelfArrayIndex;
            SelfArrayIndex := ItemPtr^.Next;
            fFirstFree := SelfArrayIndex;
            PayloadCopy(List.ArrayPointers[ListArrayIndex],Addr(ItemPtr^.Payload));
            ListArrayIndex := List.NextFromArrayIndex(ListArrayIndex);
          end;  
        PLLAItem(GetItemPtr(fLastUsed))^.Next := -1;
        If CheckArrayIndex(fFirstFree) then
          PLLAItem(GetItemPtr_LL(fFirstFree))^.Prev := -1
        else
          fLastFree := -1;
        fCount := fCount + List.Count;
        DoChange;
      end;
  end
else raise ELLAIncompatibleObject.CreateFmt('TLinkedListArray.Append: Incompatible list (%s(%d)).',[List.ClassName,List.PayloadSize]);
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
              PayloadWrite(GetPayloadPtrByArrayIndex(ArrayIndex),BufferStream);
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
            PayloadWrite(GetPayloadPtrByArrayIndex(ArrayIndex),Stream);
            ArrayIndex := NextFromArrayIndex(ArrayIndex);
          end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TLinkedListArray.ReadFromStream(Stream: TStream; Buffered: Boolean = False);
begin
FinalizeAllItems;
InternalReadFromStream(Stream,Buffered);
DoChange;
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
  FinalizeAllItems; // no need to call Clear
  // disable calling of PayloadInit and PayloadFinal from SetCount and SetCapacity
  fLoading := True;
  try
    SetCount(Integer(Stream_GetInt32(Stream)));
  finally
    fLoading := False;
  end;
  InternalReadFromStream(Stream,Buffered);
  DoChange;
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

//------------------------------------------------------------------------------

procedure TLinkedListArray.DbgGetGlobalInfo(out GlobalInfo: TLLAGlobalInfo);
begin
GlobalInfo.ItemSize := fItemSize;
GlobalInfo.MemorySize := fMemorySize;
GlobalInfo.Memory := fMemory;
GlobalInfo.FirstFree := fFirstFree;
GlobalInfo.LastFree := fLastFree;
GlobalInfo.FirstUsed := fFirstUsed;
GlobalInfo.LastUsed := fLastUsed;
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.DbgGetArrayItemInfo(ArrayIndex: TLLAArrayIndex; out ItemInfo: TLLAItemInfo): Boolean;
var
  ItemPtr:  PLLAItem;
begin
If CheckArrayIndex(ArrayIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(ArrayIndex));
    ItemInfo.Prev := ItemPtr^.Prev;
    ItemInfo.Next := ItemPtr^.Next;
    ItemInfo.ListIndex := ItemPtr^.ListIndex;
    ItemInfo.ArrayIndex := ItemPtr^.ArrayIndex;
    Result := True;
  end
else raise ELLAArrayIndexOutOfbounds.CreateFmt('TLinkedListArray.DbgGetArrayItemInfo: Array index (%d) out of bounds.',[ArrayIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.DbgGetListItemInfo(ListIndex: TLLAListIndex; out ItemInfo: TLLAItemInfo): Boolean;
var
  ItemPtr:  PLLAItem;
begin
If CheckListIndex(ListIndex) then
  begin
    ItemPtr := PLLAItem(GetItemPtr_LL(GetArrayIndex(ListIndex)));
    ItemInfo.Prev := ItemPtr^.Prev;
    ItemInfo.Next := ItemPtr^.Next;
    ItemInfo.ListIndex := ItemPtr^.ListIndex;
    ItemInfo.ArrayIndex := ItemPtr^.ArrayIndex;
    Result := True;
  end
else raise ELLAListIndexOutOfbounds.CreateFmt('TLinkedListArray.DbgGetListItemInfo: List index (%d) out of bounds.',[ListIndex]);
end;

//------------------------------------------------------------------------------

Function TLinkedListArray.DbgArrayItemIsUsed(ArrayIndex: TLLAArrayIndex): Boolean;
begin
If CheckArrayIndex(ArrayIndex) then
  Result := CheckListIndex(PLLAItem(GetItemPtr_LL(ArrayIndex))^.ListIndex)
else
  raise ELLAArrayIndexOutOfbounds.CreateFmt('TLinkedListArray.DbgArrayItemIsUsed: Array index (%d) out of bounds.',[ArrayIndex]);
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
Result := Integer(Pointer(GetPayloadPtrByListIndex(ListIndex))^);
end;

//------------------------------------------------------------------------------

procedure TIntegerLinkedListArray.SetItem(ListIndex: TLLAListIndex; Value: Integer);
begin
SetPayloadPtrByListIndex(ListIndex,@Value);
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

Function TIntegerLinkedListArray.ArrayFind(Item: Integer; out ArrayIndex: TLLAArrayIndex): Boolean;
begin
Result := inherited ArrayFind(@Item,ArrayIndex);
end;

//------------------------------------------------------------------------------

Function TIntegerLinkedListArray.ListFind(Item: Integer; out ListIndex: TLLAListIndex): Boolean;
begin
Result := inherited ListFind(@Item,ListIndex);
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
