{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SequentialVectors

    This unit provides base class (TSequentialVector) that can be used to
    implement both FIFO (queue) and LIFO (stack) vectors with items/entries
    of specific type.

    Generics were not used because of backward compatibility with old
    compilers - instead a template is provided. Also a complete derived vector
    classes with items of type Integer are provided (you can refer to them for
    more details).

    Note that the vectors have property MaxCount (maximum count, can be set
    only in constructors) which limits amount of items each vector can hold.
    Normally, when adding new items, the vector is grown as needed. But when
    the vector already contains number of items equal to MaxCount, then it is
    no longer grown and new items are not added to it - instead, the vector
    changes behaviout to a circular buffer, meaning newly added items are not
    added, they rewrite the oldest ones and current count is kept.

      NOTE - When setting MaxCount to zero or lower, then it is instead
             converted to a value of High(Integer), as maximum count of
             zero or less has no meaning.

  Version 1.1 (2025-02-05)

  Last change (2025-02-05)

  ©2024-2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SequentialVectors

  Dependencies:
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions       - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SequentialVectors_UseAuxExceptions for details).

  BinaryStreamingLite can be replaced by full BinaryStreaming.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{
  Use the following template to create a derived classes for specific item type.

  Simply copy the code and replace occurences of string @ClassName@ with a name
  of common class (common to both FIFO and LIFO), @FIFOClassName@ with a name
  for FIFO class, @LIFOClassName@ with a name for LIFO class and finally @Type@
  with an identifier of type you want to use for items.

  Integer vectors provided by this unit were completely created using this
  template, so you can look there for an example.

  Also note that, if you do not need both FIFO and LIFO vectors, you can create
  complete implementation directly, without using two-level inheritance. All
  you need is a small tweak to the constructor - remove the OperationMode
  argument, and, in the constructor implementation, select one of the vector
  types directly by passing omFIFO or omLIFO to inherited constructor.
}
(*******************************************************************************
==  Declaration  ===============================================================
--------------------------------------------------------------------------------

  @ClassName@ = class(TSequentialVector)
  protected
    Function GetItem(Index: Integer): @Type@; reintroduce;
    procedure SetItem(Index: Integer; NewValue: @Type@); reintroduce;
  //procedure ItemInit(Item: Pointer); override;
  //procedure ItemFinal(Item: Pointer); override;
  //procedure ItemDrop(Item: Pointer); override;
  //procedure ItemAssign(SrcItem,DstItem: Pointer); override;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  //Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
  //procedure ItemWrite(Item: Pointer; Stream: TStream); override;
  //procedure ItemRead(Item: Pointer; Stream: TStream); override;
  //class Function ManagedItemStreaming: Boolean; override;
  public
    constructor Create(OperationMode: TSVOperationMode; MaxCount: Integer = -1);
    Function IndexOf(Item: @Type@): Integer; reintroduce;
    Function Find(Item: @Type@; out Index: Integer): Boolean; reintroduce;
    procedure Push(Item: @Type@); reintroduce;
    Function Peek: @Type@; reintroduce;
    Function Pop: @Type@; reintroduce;
    Function Pick(Index: Integer): @Type@; reintroduce;
    property Items[Index: Integer]: @Type@ read GetItem write SetItem;
  end;

  @FIFOClassName@ = class(@ClassName@)
  public
    constructor Create(MaxCount: Integer = -1);
  end;

  @LIFOClassName@ = class(@ClassName@)
  public
    constructor Create(MaxCount: Integer = -1);
  end;

==  Implementation  ============================================================
--------------------------------------------------------------------------------

Function @ClassName@.GetItem(Index: Integer): @Type@;
begin
inherited GetItem(Index,@Result);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.SetItem(Index: Integer; NewValue: @Type@);
begin
inherited SetItem(Index,@NewValue);
end;

//------------------------------------------------------------------------------

//  ItemInit is called whenever a new item is implicitly (without explicit
//  action of the used) added to the vector. This can be eg. when changing
//  Count property to a higher number - new empty items are added and this
//  method is called for each of them.
//  Default implementation fills the item memory with zeroes.

//procedure @ClassName@.ItemInit(Item: Pointer); virtual;
//begin
//end;

//------------------------------------------------------------------------------

//  Itemfinal is called whenever any item is implicitly (without explicit
//  action of the user) removed from the vector - this includes actions such as
//  clearing, freeing non-empty list or loading the list (where existing items
//  are discarded). It is not called when Pop-ing, as that is functionally
//  equivalent to Extract in lists.
//  Default action is a no-op. Implement only when the items really need to be
//  finalized (objects, interfaces, dynamically allocated memory, dynamic
//  arrays, strings, etc.).

//procedure @ClassName@.ItemFinal(Item: Pointer);
//begin
//end;

//------------------------------------------------------------------------------

//  ItemDrop is called when existing item is being replaced by a new one (note
//  it is called for the existing item). This happens only when vector reached
//  its max count and is now operating in circular mode where each push will,
//  instead of growing the vector, just replace the oldest item.
//  Default implementation calls ItemFinal for the given item.

//procedure @ClassName@.ItemDrop(Item: Pointer);
//begin
//end;

//------------------------------------------------------------------------------

//  ItemAssign is called when value of an item is assigned.
//  Default implementation uses RTL procedure Move to copy the item's memory,
//  but you can reimplement it to provide simpler and faster assigning (eg. for
//  primitive types like integers or floats, where it is enough to just use
//  assignment operator := and compiler optimizes the operation).

//procedure @ClassName@.ItemAssign(SrcItem,DstItem: Pointer);
//begin
//end;

//------------------------------------------------------------------------------

//  ItemCompare is used to compare values of two items. This method must be
//  always implemented to suit the actual type. There is no default
//  implementation as it is an abstract method.
//  When Item1 is larger than Item2, the function must return a positive value,
//  when they are equal, zero must be returned, and when Item1 is smaller than
//  Item2, then a negative value must be returned.

Function @ClassName@.ItemCompare(Item1,Item2: Pointer): Integer;
begin
{$MESSAGE ERROR 'Implement for actual type!'}
end;

//------------------------------------------------------------------------------

//  ItemEquals is used to compare value of two items for equality.
//  Default implementation uses ItemCompare to do the comparison. Implement it
//  when full value comparison is not needed or direct equality check can be
//  done much faster.
//  When the two items have values that can be considered equal, return True,
//  otherwise set the result to False.

//Function @ClassName@.ItemEquals(Item1,Item2: Pointer): Boolean;
//begin
//end;

//------------------------------------------------------------------------------

//  ItemWrite is called for each item that is being written to a stream (or
//  file), but only when method ManagedItemStreaming (see further) returns true.
//  Default implementation merely stores item's memory as is. Implement this
//  method if you wish to eg. ensure endianness, store complex objects that
//  cannot be written directly, and so on.

//procedure @ClassName@.ItemWrite(Item: Pointer; Stream: TStream);
//begin
//end;

//------------------------------------------------------------------------------

//  ItemRead is called for each item that is being read from a stream (or file),
//  but only when method ManagedItemStreaming returns true.
//  Default implementation reads the stream directly into the item's memory,
//  without any further processing. You can use this method to do reading of
//  complex objects or when binary compatibility (eg. endianness) needs to be
//  ensured. 

//procedure @ClassName@.ItemRead(Item: Pointer; Stream: TStream);
//begin
//end;

//------------------------------------------------------------------------------

//  If you want items to be streamed using methods ItemWrite and ItemRead,
//  return true. If false is returned (default behavior), then the items are
//  streamed directly from/to memory, without calling mentioned functions.

//class Function @ClassName@.ManagedItemStreaming: Boolean;
//begin
//end;

//==============================================================================

constructor @ClassName@.Create(OperationMode: TSVOperationMode; MaxCount: Integer = -1);
begin
inherited Create(OperationMode,SizeOf(@Type@),MaxCount);
end;

//------------------------------------------------------------------------------

Function @ClassName@.IndexOf(Item: @Type@): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Find(Item: @Type@; out Index: Integer): Boolean;
begin
Result := inherited Find(@Item,Index);
end;

//------------------------------------------------------------------------------

procedure @ClassName@.Push(Item: @Type@);
begin
inherited Push(@Item);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Peek: @Type@;
begin
inherited Peek(@Result);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Pop: @Type@;
begin
inherited Pop(@Result);
end;

//------------------------------------------------------------------------------

Function @ClassName@.Pick(Index: Integer): @Type@;
begin
inherited Pick(Index,@Result);
end;

//==============================================================================

constructor @FIFOClassName@.Create(MaxCount: Integer = -1);
begin
inherited Create(omFIFO,MaxCount);
end;

//==============================================================================

constructor @LIFOClassName@.Create(MaxCount: Integer = -1);
begin
inherited Create(omLIFO,MaxCount);
end;

*******************************************************************************)
unit SequentialVectors;
{
  SequentialVectors_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SequentialVectors_UseAuxExceptions to achieve this.
}
{$IF Defined(SequentialVectors_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESVException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESVIndexOutOfBounds = class(ESVException);
  ESVInvalidOperation = class(ESVException);
  ESVInvalidValue     = class(ESVException);
  ESVNoItem           = class(ESVException);

{===============================================================================
--------------------------------------------------------------------------------
                                TSequentialVector
--------------------------------------------------------------------------------
===============================================================================}
type
  TSVOperationMode = (omFIFO,omLIFO);

{===============================================================================
    TSequentialVector - class declaration
===============================================================================}
type
  TSequentialVector = class(TCustomListObject)
  protected
    fOperationMode:     TSVOperationMode;
    fPeekMethod:        procedure(ItemPtr: Pointer) of object;
    fPopMethod:         procedure(ItemPtr: Pointer) of object;
    fItemSize:          TMemSize;
    fMemory:            Pointer;
    fMemorySize:        TMemSize;
    fCapacity:          Integer;
    fCount:             Integer;
    fHighMemory:        Pointer;  // fMemory + fMemorySize
    fFirstItemPosition: Integer;
    fMaxCount:          Integer;
    fUpdateCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    // getters, setters
  {$IFDEF Debug}
    Function GetPositionPtr(Index: Integer): Pointer; virtual;
  {$ENDIF}  
    Function GetItemPtr(Index: Integer): Pointer; virtual;
    procedure GetItem(Index: Integer; DstPtr: Pointer); virtual;
    procedure SetItem(Index: Integer; SrcPtr: Pointer); virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // items management
    procedure ItemInit(Item: Pointer); virtual;
    procedure ItemFinal(Item: Pointer); virtual;
    procedure ItemDrop(Item: Pointer); virtual;
    procedure ItemAssign(SrcItem,DstItem: Pointer); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; virtual; abstract;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; virtual;
    procedure ItemWrite(Item: Pointer; Stream: TStream); virtual;
    procedure ItemRead(Item: Pointer; Stream: TStream); virtual;
    // init/final
    procedure Initialize(OperationMode: TSVOperationMode; ItemSize: TMemSize; MaxCount: Integer); virtual;
    procedure Finalize; virtual;
    // internals
    Function ItemsMemorySize(Count: Integer): TMemSize; virtual;
    Function NextItemPtr(ItemPtr: Pointer): Pointer; virtual;
    procedure DoChange; virtual;
    procedure FinalizeAllItems; virtual;
    class Function ManagedItemStreaming: Boolean; virtual;
    procedure PeekFirst(ItemPtr: Pointer); virtual;
    procedure PeekLast(ItemPtr: Pointer); virtual;
    procedure PopFirst(ItemPtr: Pointer); virtual;
    procedure PopLast(ItemPtr: Pointer); virtual;
    procedure InternalReadFromStream(Stream: TStream); virtual;
  public
    constructor Create(OperationMode: TSVOperationMode; ItemSize: TMemSize; MaxCount: Integer = -1);
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    // vector control
    Function IndexOf(ItemPtr: Pointer): Integer; virtual;
    Function Find(ItemPtr: Pointer; out Index: Integer): Boolean; virtual;
    procedure Push(ItemPtr: Pointer); virtual;
    procedure Peek(ItemPtr: Pointer); virtual;
    procedure Pop(ItemPtr: Pointer); virtual;
    procedure Pick(Index: Integer; ItemPtr: Pointer); virtual;
    procedure Clear; virtual;
    // I/O
    procedure WriteToStream(Stream: TStream); virtual;
    procedure ReadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure WriteToFile(const FileName: String); virtual;
    procedure ReadFromFile(const FileName: String); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
  {$IFDEF Debug}
    Function IsItemAtPosition(Index: Integer): Boolean; virtual;
    property FirstItemPosition: Integer read fFirstItemPosition;
    property PositionPtrs[Index: Integer]: Pointer read GetPositionPtr; // 0..Pred(Capacity)
  {$ENDIF}
    // properties
    property ItemSize: TMemSize read fItemSize;
    property Memory: Pointer read fMemory;
    property MemorySize: TMemSize read fMemorySize;
    property MaxCount: Integer read fMaxCount;
    property Pointers[Index: Integer]: Pointer read GetItemPtr;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TIntegerSequentialVector
--------------------------------------------------------------------------------
===============================================================================}
{$IF SizeOf(Integer) <> 4}
  {$MESSAGE WARN 'Incompatible integer size (expected 4B).'}
{$IFEND}
{===============================================================================
    TIntegerSequentialVector - class declaration
===============================================================================}
type
  TIntegerSequentialVector = class(TSequentialVector)
  protected
    Function GetItem(Index: Integer): Integer; reintroduce;
    procedure SetItem(Index: Integer; NewValue: Integer); reintroduce;
    procedure ItemInit(Item: Pointer); override;
    procedure ItemFinal(Item: Pointer); override;
    procedure ItemAssign(SrcItem,DstItem: Pointer); override;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
    procedure ItemWrite(Item: Pointer; Stream: TStream); override;
    procedure ItemRead(Item: Pointer; Stream: TStream); override;
    class Function ManagedItemStreaming: Boolean; override;
  public
    constructor Create(OperationMode: TSVOperationMode; MaxCount: Integer = -1);
    Function IndexOf(Item: Integer): Integer; reintroduce;
    Function Find(Item: Integer; out Index: Integer): Boolean; reintroduce;
    procedure Push(Item: Integer); reintroduce;
    Function Peek: Integer; reintroduce;
    Function Pop: Integer; reintroduce;
    Function Pick(Index: Integer): Integer; reintroduce;
    property Items[Index: Integer]: Integer read GetItem write SetItem;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerFIFOVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerFIFOVector - class declaration
===============================================================================}
type
  TIntegerFIFOVector = class(TIntegerSequentialVector)
  public
    constructor Create(MaxCount: Integer = -1);
  end;

  // aliasses
  TIntegerFirstInFirstOutVector = TIntegerFIFOVector;
  TIntegerQueueVector = TIntegerFIFOVector;

{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerLIFOVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerLIFOVector - class declaration
===============================================================================}
type
  TIntegerLIFOVector = class(TIntegerSequentialVector)
  public
    constructor Create(MaxCount: Integer = -1);
  end;

  // aliasses
  TIntegerLastInFirstOutVector = TIntegerLIFOVector;
  TIntegerStackVector = TIntegerLIFOVector;

implementation

uses
  StrRect, BinaryStreamingLite;

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ELSE}
  {$UNDEF OverflowChecks}
{$ENDIF}

{===============================================================================
    Auxiliary functions
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function PtrAdvance(Ptr: Pointer; Offset: TMemSize): Pointer;
begin
{$IFDEF FPC}{$PUSH}{$WARN 4055 OFF}{$ENDIF} // Conversion between ordinals and pointers is not portable
Result := Pointer(PtrUInt(Ptr) + PtrUInt(Offset));
{$IFDEF FPC}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

Function PtrCompare(A,B: Pointer): Integer;
begin
{$IFDEF FPC}{$PUSH}{$WARN 4055 OFF}{$ENDIF}
If PtrUInt(A) < PtrUInt(B) then
  Result := -1
else If PtrUInt(A) > PtrUInt(B) then
  Result := +1
else
  Result := 0;
{$IFDEF FPC}{$POP}{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSequentialVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSequentialVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSequentialVector - protected methods
-------------------------------------------------------------------------------}
{$IFDEF Debug}
Function TSequentialVector.GetPositionPtr(Index: Integer): Pointer;
begin
If (Index >= 0) and (Index < fCapacity) then
  Result := PtrAdvance(fMemory,PtrInt(ItemsMemorySize(Index)))
else
  raise ESVIndexOutOfBounds.CreateFmt('TSequentialVector.GetPositionPtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------
{$ENDIF}

Function TSequentialVector.GetItemPtr(Index: Integer): Pointer;
begin
If CheckIndex(Index) then
  begin
    // convert item index to item position and then to its address
    If Index >= (fCapacity - fFirstItemPosition) then
      Result := PtrAdvance(fMemory,ItemsMemorySize(Index - (fCapacity - fFirstItemPosition)))
    else
      Result := PtrAdvance(fMemory,ItemsMemorySize(fFirstItemPosition + Index));
  end
else raise ESVIndexOutOfBounds.CreateFmt('TSequentialVector.GetItemPtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.GetItem(Index: Integer; DstPtr: Pointer);
begin
ItemAssign(GetItemPtr(Index),DstPtr);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.SetItem(Index: Integer; SrcPtr: Pointer);
var
  ItemPtr:  Pointer;
begin
ItemPtr := GetItemPtr(Index);
If not ItemEquals(ItemPtr,SrcPtr) then
  begin
    ItemAssign(SrcPtr,ItemPtr);
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.GetCapacity: Integer;
begin
Result := fCapacity;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.SetCapacity(Value: Integer);
var
  ItemsToMove:  Integer;
begin
If Value < 0 then
  raise ESVInvalidValue.CreateFmt('TSequentialVector.SetCapacity: Invalid new capacity (%d).',[Value])
else If Value > fMaxCount then
  Value := fMaxCount;
If (Value <> fCapacity) then
  begin
    If Value <= 0 then
      begin
        // new capacity will be zero, just clear everything
        FinalizeAllItems;
        FreeMem(fMemory,fMemorySize);
        fMemorySize := 0;
        fMemory := nil;
        fCapacity := 0;
        fCount := 0;
        fHighMemory := nil;
        fFirstItemPosition := 0;
      end
    else If fCount <= 0 then
      begin
        // there are no items, do not use realloc to prevent copying
        FreeMem(fMemory,fMemorySize);
        fMemorySize := ItemsMemorySize(Value);
        fMemory := AllocMem(fMemorySize);
        fCapacity := Value;
        fHighMemory := PtrAdvance(fMemory,fMemorySize);
        fFirstItemPosition := 0;
      end
    else If Value > fCapacity then
      begin
      {
        We are adding the capacity - simple case, just reallocate and if items
        are split on high address, move the block that touches the high address
        so that it is again touching it in the new memory space.
      }
        If fCount > (fCapacity - fFirstItemPosition) then
          ItemsToMove := fCapacity - fFirstItemPosition
        else
          ItemsToMove := 0;
        fMemorySize := ItemsMemorySize(Value);
        ReallocMem(fMemory,fMemorySize);
        fCapacity := Value;
        fHighMemory := PtrAdvance(fMemory,fMemorySize);
        If ItemsToMove > 0 then
          begin
            Move(PtrAdvance(fMemory,ItemsMemorySize(fFirstItemPosition))^,
                 PtrAdvance(fMemory,ItemsMemorySize(fCapacity - ItemsToMove))^,
                 ItemsMemorySize(ItemsToMove));
            fFirstItemPosition := fCapacity - ItemsToMove;
          end;
        // first item position is not changed if no item is moved
      end
    else
      begin
        // lowering the capacity while there are some items stored
        If Value < fCount then
          SetCount(Value);  // we need to remove some items (changes fCount)
        If fCount > (fCapacity - fFirstItemPosition) then
          begin
          {
            Item are split into two blocks, move the one at end so that it will
            fit into new capacity.
          }
            ItemsToMove := fCapacity - fFirstItemPosition;
            Move(PtrAdvance(fMemory,ItemsMemorySize(fFirstItemPosition))^,
                 PtrAdvance(fMemory,ItemsMemorySize(Value - ItemsToMove))^,
                 ItemsMemorySize(ItemsToMove));
            fFirstItemPosition := Value - ItemsToMove;
          end
        else If fCount > (Value - fFirstItemPosition) then
          begin
          {
            All items are in one contiguous block but it will not fit into new
            space (it would overflow high memory), move them to the memory base.
          }
            Move(PtrAdvance(fMemory,ItemsMemorySize(fFirstItemPosition))^,fMemory^,ItemsMemorySize(fCount));
            fFirstItemPosition := 0;
          end;
        // items position is rectified, now just reallocate
        fMemorySize := ItemsMemorySize(Value);
        ReallocMem(fMemory,fMemorySize);
        fCapacity := Value;
        fHighMemory := PtrAdvance(fMemory,fMemorySize);
      end;
  {$IFDEF Debug}
    DoChange;
  {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.SetCount(Value: Integer);
var
  WorkPtr:  Pointer;
  i:        Integer;
begin
If Value < 0 then
  raise ESVInvalidValue.CreateFmt('TSequentialVector.SetCount: Invalid new count (%d).',[Value])
else If Value > fMaxCount then
  Value := fMaxCount;
If Value <> fCount then
  begin
    If Value > fCapacity then
      SetCapacity(Value);
    If Value < fCount then
      begin
        // removing existing items (note that fCount cannot be 0 here)
        case fOperationMode of
          omFIFO: begin
                    WorkPtr := GetItemPtr(LowIndex);
                    For i := 0 to Pred(fCount - Value) do
                      begin
                        ItemFinal(WorkPtr);
                        WorkPtr := NextItemPtr(WorkPtr);
                      end;
                    Inc(fFirstItemPosition,fCount - Value);
                    If fFirstItemPosition >= fCapacity then
                      fFirstItemPosition := fFirstItemPosition - fCapacity;
                    fCount := Value;
                  end;
          omLIFO: begin
                    WorkPtr := GetItemPtr(Value);
                    For i := 0 to Pred(fCount - Value) do
                      begin
                        ItemFinal(WorkPtr);
                        WorkPtr := NextItemPtr(WorkPtr);
                      end;
                    fCount := Value;
                  end;
        else
          raise ESVInvalidValue.CreateFmt('TSequentialVector.SetCount: Invalid operation mode (%d).',[Ord(fOperationMode)]);
        end;
        If fCount <= 0 then
          fFirstItemPosition := 0;
      end
    else
      begin
        // adding new empty items
        If fCount > 0 then
          WorkPtr := NextItemPtr(GetItemPtr(HighIndex))
        else
          WorkPtr := fMemory;
        repeat
        {
          Inc must be up here so that the initialized item is already validly
          in the list.
        }
          Inc(fCount);
          ItemInit(WorkPtr);
          WorkPtr := NextItemPtr(WorkPtr);
        until fCount >= Value;
      end;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemInit(Item: Pointer);
begin
FillChar(Item^,fItemSize,0);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemFinal(Item: Pointer);
begin
// item cannot have zero size, so following is secure (and I know it is a no-op)
PByte(Item)^ := PByte(Item)^;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemDrop(Item: Pointer);
begin
ItemFinal(Item);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemAssign(SrcItem,DstItem: Pointer);
begin
Move(SrcItem^,DstItem^,fItemSize);
end;

//------------------------------------------------------------------------------

Function TSequentialVector.ItemEquals(Item1,Item2: Pointer): Boolean;
begin
Result := ItemCompare(Item1,Item2) = 0;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemWrite(Item: Pointer; Stream: TStream);
begin
Stream.WriteBuffer(Item^,fItemSize);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ItemRead(Item: Pointer; Stream: TStream);
begin
Stream.ReadBuffer(Item^,fItemSize);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Initialize(OperationMode: TSVOperationMode; ItemSize: TMemSize; MaxCount: Integer);
begin
fOperationMode := OperationMode;
case fOperationMode of
  omFIFO: begin
            fPeekMethod := PeekFirst;
            fPopMethod := PopFirst;
          end;
  omLIFO: begin
            fPeekMethod := PeekLast;
            fPopMethod := PopLast;
          end;
else
  raise ESVInvalidValue.CreateFmt('TSequentialVector.Initialize: Invalid operation mode (%d).',[Ord(fOperationMode)]);
end;
fItemSize := ItemSize;
If fItemSize <= 0 then
  raise ESVInvalidValue.Create('TSequentialVector.Initialize: Item cannot have size of zero.');
fMemory := nil;
fMemorySize := 0;
fCapacity := 0;
fCount := 0;
fHighMemory := fMemory;
fFirstItemPosition := 0;
If MaxCount <= 0 then
  fMaxCount := High(Integer)
else
  fMaxCount := MaxCount;
fUpdateCounter := 0;
fChanged := False;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Finalize;
begin
FinalizeAllItems;
FreeMem(fMemory,fMemorySize);
end;

//------------------------------------------------------------------------------

Function TSequentialVector.ItemsMemorySize(Count: Integer): TMemSize;
begin
Result := PtrUInt(Count) * PtrUInt(fItemSize);
end;

//------------------------------------------------------------------------------

Function TSequentialVector.NextItemPtr(ItemPtr: Pointer): Pointer;
begin
Result := PtrAdvance(ItemPtr,fItemSize);
If PtrCompare(Result,fHighMemory) >= 0 then
  Result := fMemory;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.DoChange;
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

procedure TSequentialVector.FinalizeAllItems;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
If fCount > 0 then
  begin
    TempPtr := GetItemPtr(LowIndex);
    For i := LowIndex to HighIndex do
      begin
        ItemFinal(TempPtr);
        TempPtr := NextItemPtr(TempPtr);
      end;
  end;
end;

//------------------------------------------------------------------------------

class Function TSequentialVector.ManagedItemStreaming: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.PeekFirst(ItemPtr: Pointer);
begin
ItemAssign(GetItemPtr(LowIndex),ItemPtr);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.PeekLast(ItemPtr: Pointer);
begin
ItemAssign(GetItemPtr(HighIndex),ItemPtr);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.PopFirst(ItemPtr: Pointer);
begin
ItemAssign(GetItemPtr(LowIndex),ItemPtr);
Inc(fFirstItemPosition);
Dec(fCount);
If (fFirstItemPosition >= fCapacity) or (fCount <= 0) then
  fFirstItemPosition := 0;
Shrink;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.PopLast(ItemPtr: Pointer);
begin
ItemAssign(GetItemPtr(HighIndex),ItemPtr);
Dec(fCount);
If fCount <= 0 then
  fFirstItemPosition := 0;
Shrink;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.InternalReadFromStream(Stream: TStream);
var
  i:        Integer;
  TempPtr:  Pointer;
begin
// all items are finalized by this point, count > 0 and first item pos. is 0
fFirstItemPosition := 0;
If ManagedItemStreaming then
  begin
    // managed IO
    TempPtr := GetItemPtr(LowIndex);
    For i := LowIndex to HighIndex do
      begin
        ItemRead(TempPtr,Stream);
        TempPtr := NextItemPtr(TempPtr);
      end;
  end
// unmanaged IO, read everything in one go
else Stream.ReadBuffer(fMemory^,ItemsMemorySize(fCount));
DoChange;
end;

{-------------------------------------------------------------------------------
    TSequentialVector - public methods
-------------------------------------------------------------------------------}

constructor TSequentialVector.Create(OperationMode: TSVOperationMode; ItemSize: TMemSize; MaxCount: Integer = -1);
begin
inherited Create;
Initialize(OperationMode,ItemSize,MaxCount);
end;

//------------------------------------------------------------------------------

destructor TSequentialVector.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.BeginUpdate;
begin
If fUpdateCounter <= 0 then
  begin
    fUpdateCounter := 0;
    fChanged := False;
  end;
Inc(fUpdateCounter);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.EndUpdate;
begin
Dec(fUpdateCounter);
If fUpdateCounter <= 0 then
  begin
    fUpdateCounter := 0;
    If fChanged then
      DoChange;
    fChanged := False;
  end;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.LowIndex: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TSequentialVector.IndexOf(ItemPtr: Pointer): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
Result := -1;
If fCount > 0 then
  begin
    TempPtr := GetItemPtr(LowIndex);
    For i := LowIndex to HighIndex do
      If ItemEquals(ItemPtr,TempPtr) then
        begin
          Result := i;
          Break{For i};
        end
      else TempPtr := NextItemPtr(TempPtr);
  end;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.Find(ItemPtr: Pointer; out Index: Integer): Boolean;
begin
Index := IndexOf(ItemPtr);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Push(ItemPtr: Pointer);
var
  ChangedItem:  Pointer;
begin
// push is the same for lifo and fifo
If fCount >= fMaxCount then
  begin
    // operate as circular buffer - remove oldest item and replace it with a new one
    ChangedItem := GetItemPtr(LowIndex);
    ItemDrop(ChangedItem);
    ItemAssign(ItemPtr,ChangedItem);
    Inc(fFirstItemPosition);
    If fFirstItemPosition >= fCapacity then
      fFirstItemPosition := 0;    
  end
else
  begin
    // vanilla stuff
    Grow;
    Inc(fCount);
    ItemAssign(ItemPtr,GetItemPtr(HighIndex));
  end;
DoChange;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Peek(ItemPtr: Pointer);
begin
If fCount > 0 then
  fPeekMethod(ItemPtr)
else
  raise ESVNoItem.Create('TSequentialVector.Peek: Cannot peek an empty vector.');
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Pop(ItemPtr: Pointer);
begin
If fCount > 0 then
  begin
    fPopMethod(ItemPtr);
    DoChange;
  end
else raise ESVNoItem.Create('TSequentialVector.Pop: Cannot pop an empty vector.');
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Pick(Index: Integer; ItemPtr: Pointer);
var
  CurrItem: Pointer;
  NextItem: Pointer;
  i:        Integer;
begin
If CheckIndex(Index) then
  begin
    If Index = LowIndex then
      PopFirst(ItemPtr)
    else If Index = HighIndex then
      PopLast(ItemPtr)
    else
      begin
        // if we are here, it means there are at least 3 items!
        ItemAssign(GetItemPtr(Index),ItemPtr);
        CurrItem := GetItemPtr(Index);
        NextItem := NextItemPtr(CurrItem);
        For i := Index to Pred(HighIndex) do
          begin
            Move(NextItem^,CurrItem^,fItemSize);
            // following should be faster than constantly calling GetItemPtr
            CurrItem := NextItem;
            NextItem := NextItemPtr(CurrItem);
          end;
        Dec(fCount);
        Shrink;
      end;
    DoChange;
  end
else raise ESVIndexOutOfBounds.CreateFmt('TSequentialVector.Pick: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.Clear;
begin
If fCount > 0 then
  begin
    FinalizeAllItems;
    fCount := 0;
    fFirstItemPosition := 0;
    Shrink;
    DoChange;
  end
else fCount := 0;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.WriteToStream(Stream: TStream);
var
  i:          Integer;
  TempPtr:    Pointer;
  ItemCount:  Integer;
begin
If fCount > 0 then
  begin
    If ManagedItemStreaming then
      begin
        // managed IO, each item is written separately using ItemWrite method
        TempPtr := GetItemPtr(LowIndex);
        For i := LowIndex to HighIndex do
          begin
            ItemWrite(TempPtr,Stream);
            TempPtr := NextItemPtr(TempPtr);
          end;
      end
    else
      begin
        // unmanaged IO, items are written in as large blocks as possible
        If fCount > (fCapacity - fFirstItemPosition) then
          begin
            // the items are split into two blocks
            ItemCount := fCapacity - fFirstItemPosition;
            Stream.WriteBuffer(GetItemPtr(LowIndex)^,ItemsMemorySize(ItemCount));
            Stream.WriteBuffer(fMemory^,ItemsMemorySize(fCount - ItemCount));
          end
        // item are in one contiguous block
        else Stream.WriteBuffer(GetItemPtr(LowIndex)^,ItemsMemorySize(fCount));
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.ReadFromStream(Stream: TStream);
begin
If fCount > 0 then
  begin
    FinalizeAllItems;
    fFirstItemPosition := 0;
    InternalReadFromStream(Stream);
  end;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.SaveToStream(Stream: TStream);
begin
Stream_WriteInt32(Stream,Int32(fCount));
WriteToStream(Stream);
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.LoadFromStream(Stream: TStream);
begin
BeginUpdate;  // SetCapacity(0) might call DoUpdate
try
  // free current memory to prevent copying in reallocation
  SetCapacity(0); // also finalizes all existing items
  SetCapacity(Stream_GetInt32(Stream));
  fCount := fCapacity;
  If fCount > 0 then
    InternalReadFromStream(Stream);
finally
  EndUpdate;
end;
end;

//------------------------------------------------------------------------------

procedure TSequentialVector.WriteToFile(const FileName: String);
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

procedure TSequentialVector.ReadFromFile(const FileName: String);
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

procedure TSequentialVector.SaveToFile(const FileName: String);
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

procedure TSequentialVector.LoadFromFile(const FileName: String);
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

{$IFDEF Debug}
//------------------------------------------------------------------------------

Function TSequentialVector.IsItemAtPosition(Index: Integer): Boolean;
begin
If (Index >= 0) and (Index < fCapacity) then
  begin
    If fCount > (fCapacity - fFirstItemPosition) then
      Result := ((Index >= fFirstItemPosition) and (Index < fCapacity)) or
                ((Index >= 0) and (Index < (fCount - (fCapacity - fFirstItemPosition))))
    else
      Result := (Index >= fFirstItemPosition) and (Index < (fFirstItemPosition + fCount));
  end
else raise ESVIndexOutOfBounds.CreateFmt('TSequentialVector.IsItemAtPosition: Index (%d) out of bounds.',[Index]);
end;  
{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                            TIntegerSequentialVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerSequentialVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TIntegerSequentialVector - protected methods
-------------------------------------------------------------------------------}

Function TIntegerSequentialVector.GetItem(Index: Integer): Integer;
begin
inherited GetItem(Index,@Result);
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.SetItem(Index: Integer; NewValue: Integer);
begin
inherited SetItem(Index,@NewValue);
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.ItemInit(Item: Pointer); 
begin
Integer(Item^) := 0;
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.ItemFinal(Item: Pointer);
begin
Integer(Item^) := 0;
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.ItemAssign(SrcItem,DstItem: Pointer);
begin
Integer(DstItem^) := Integer(SrcItem^);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.ItemCompare(Item1,Item2: Pointer): Integer;
begin
Result := Integer(Item1^) - Integer(Item2^);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.ItemEquals(Item1,Item2: Pointer): Boolean;
begin
Result := Integer(Item1^) = Integer(Item2^);
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.ItemWrite(Item: Pointer; Stream: TStream);
begin
Stream_WriteInt32(Stream,Integer(Item^));
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.ItemRead(Item: Pointer; Stream: TStream);
begin
Integer(Item^) := Stream_GetInt32(Stream);
end;

//------------------------------------------------------------------------------

class Function TIntegerSequentialVector.ManagedItemStreaming: Boolean;
begin
// we are setting it to true here to ensure endianness of the streamed primitives
Result := True;
end;

{-------------------------------------------------------------------------------
    TIntegerSequentialVector - public methods
-------------------------------------------------------------------------------}

constructor TIntegerSequentialVector.Create(OperationMode: TSVOperationMode; MaxCount: Integer = -1);
begin
inherited Create(OperationMode,SizeOf(Integer),MaxCount);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.IndexOf(Item: Integer): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.Find(Item: Integer; out Index: Integer): Boolean;
begin
Result := inherited Find(@Item,Index);
end;

//------------------------------------------------------------------------------

procedure TIntegerSequentialVector.Push(Item: Integer);
begin
inherited Push(@Item);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.Peek: Integer;
begin
inherited Peek(@Result);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.Pop: Integer;
begin
inherited Pop(@Result);
end;

//------------------------------------------------------------------------------

Function TIntegerSequentialVector.Pick(Index: Integer): Integer;
begin
inherited Pick(Index,@Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerFIFOVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerFIFOVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TIntegerFIFOVector - public methods
-------------------------------------------------------------------------------}

constructor TIntegerFIFOVector.Create(MaxCount: Integer = -1);
begin
inherited Create(omFIFO,MaxCount);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerLIFOVector
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerLIFOVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TIntegerLIFOVector - public methods
-------------------------------------------------------------------------------}

constructor TIntegerLIFOVector.Create(MaxCount: Integer = -1);
begin
inherited Create(omLIFO,MaxCount);
end;


end.
