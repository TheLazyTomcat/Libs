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

    Note that any of the vectors can be created as invariant-size circular/ring
    buffer (if vector is full, its oldest item is replaced by any newly added
    one). You only need to set constructor's CircularCapacity parameter to
    desired capacity of the buffer (a number larger than zero).

      WARNING - if the vector is created in this mode, you cannot change its
                capacity allocated during creation.

  Version 1.0 (2024-07-15)

  Last change (2024-07-15)

  ©2024 František Milt

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
  //procedure ItemFinal(Item: Pointer); override;
  //procedure ItemAssign(SrcItem,DstItem: Pointer); override;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
  //Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
  //procedure ItemWrite(Item: Pointer; Stream: TStream); override;
  //procedure ItemRead(Item: Pointer; Stream: TStream); override;
  //class Function ManagedItemStreaming: Boolean; override;
  public
    constructor Create(OperationMode: TSVOperationMode; CircularCapacity: Integer = -1);
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
    constructor Create(CircularCapacity: Integer = -1);
  end;

  @LIFOClassName@ = class(@ClassName@)
  public
    constructor Create(CircularCapacity: Integer = -1);
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

constructor @ClassName@.Create(OperationMode: TSVOperationMode; CircularCapacity: Integer = -1);
begin
inherited Create(OperationMode,SizeOf(@Type@),CircularCapacity);
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

constructor @FIFOClassName@.Create(CircularCapacity: Integer = -1);
begin
inherited Create(omFIFO,CircularCapacity);
end;

//==============================================================================

constructor @LIFOClassName@.Create(CircularCapacity: Integer = -1);
begin
inherited Create(omLIFO,CircularCapacity);
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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
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
    fHighMemory:        Pointer;
    fFirstItemPosition: Integer;
    fCircularCapacity:  Integer;
    fUpdateCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    // getters, setters
    Function GetItemPtr(Index: Integer): Pointer; virtual;
    procedure GetItem(Index: Integer; DstPtr: Pointer); virtual;
    procedure SetItem(Index: Integer; SrcPtr: Pointer); virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // items management
    procedure ItemFinal(Item: Pointer); virtual;
    procedure ItemAssign(SrcItem,DstItem: Pointer); virtual;
    Function ItemCompare(Item1,Item2: Pointer): Integer; virtual; abstract;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; virtual;
    procedure ItemWrite(Item: Pointer; Stream: TStream); virtual;
    procedure ItemRead(Item: Pointer; Stream: TStream); virtual;
    // init/final
    procedure Initialize(OperationMode: TSVOperationMode; ItemSize: TMemSize; CircularCapacity: Integer); virtual;
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
    constructor Create(OperationMode: TSVOperationMode; ItemSize: TMemSize; CircularCapacity: Integer = -1);
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
    // properties
    // change inherited list RW properties to read-only
    property Capacity read GetCapacity;
    property Count read GetCount;
    property ItemSize: TMemSize read fItemSize;
    property Memory: Pointer read fMemory;
    property MemorySize: TMemSize read fMemorySize;
    property CircularCapacity: Integer read fCircularCapacity;
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
    procedure ItemAssign(SrcItem,DstItem: Pointer); override;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
    procedure ItemWrite(Item: Pointer; Stream: TStream); override;
    procedure ItemRead(Item: Pointer; Stream: TStream); override;
    class Function ManagedItemStreaming: Boolean; override;
  public
    constructor Create(OperationMode: TSVOperationMode; CircularCapacity: Integer = -1);
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
    constructor Create(CircularCapacity: Integer = -1);
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
    constructor Create(CircularCapacity: Integer = -1);
  end;

  // aliasses
  TIntegerLastInFirstOutVector = TIntegerLIFOVector;
  TIntegerStackVector = TIntegerLIFOVector;

implementation

uses
  StrRect, BinaryStreamingLite;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

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

Function TSequentialVector.GetItemPtr(Index: Integer): Pointer;
begin
If CheckIndex(Index) then
  begin
    // convert item index to item position and then to its address
    If Index >= (fCapacity - fFirstItemPosition) then
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      Result := Pointer(PtrUInt(fMemory) + (PtrUInt(ItemsMemorySize(Index - (fCapacity - fFirstItemPosition)))))
    else
      Result := Pointer(PtrUInt(fMemory) + (PtrUInt(ItemsMemorySize(fFirstItemPosition + Index))));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
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
  raise ESVInvalidValue.CreateFmt('TSequentialVector.SetCapacity: Invalid new capacity (%d).',[Value]);
If Value < fCount then
  raise ESVInvalidOperation.CreateFmt('TSequentialVector.SetCapacity: Cannot lower capacity (%d) below count (%d).',[Value,fCount]);
If (Value <> fCapacity) and (fCircularCapacity <= 0) then
  begin
    If fCount > 0 then
      begin
        // there are some items, we need to copy them to newly allocated memory
        If fCount > (fCapacity - fFirstItemPosition) then
          ItemsToMove := fCapacity - fFirstItemPosition
        else
          ItemsToMove := 0;
        fMemorySize := ItemsMemorySize(Value);
        ReallocMem(fMemory,fMemorySize);
        fCapacity := Value;
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        fHighMemory := Pointer(PtrUInt(fMemory) + fMemorySize);
        If ItemsToMove > 0 then
          Move(Pointer(PtrUInt(fMemory) + ItemsMemorySize(fFirstItemPosition))^,
               Pointer(PtrUInt(PtrUInt(fHighMemory) - PtrUInt(ItemsMemorySize(ItemsToMove))))^,
               ItemsMemorySize(ItemsToMove));
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        fFirstItemPosition := fCapacity - ItemsToMove;
      end
    else
      begin
        // do not make copy if there is no item
        FreeMem(fMemory,fMemorySize);
        fMemorySize := ItemsMemorySize(Value);
        fMemory := AllocMem(fMemorySize);
        fCapacity := Value;
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        fHighMemory := Pointer(PtrUInt(fMemory) + fMemorySize);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        fFirstItemPosition := 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TSequentialVector.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSequentialVector.SetCount(Value: Integer);
begin
raise ESVInvalidOperation.Create('TSequentialVector.SetCount: Explicitly setting count is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSequentialVector.ItemFinal(Item: Pointer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

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

procedure TSequentialVector.Initialize(OperationMode: TSVOperationMode; ItemSize: TMemSize; CircularCapacity: Integer);
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
fMemory := nil;
fMemorySize := 0;
fCapacity := 0;
fCount := 0;
fHighMemory := fMemory;
fFirstItemPosition := 0;
fCircularCapacity := 0;
If CircularCapacity > 0 then
  SetCapacity(CircularCapacity);
fCircularCapacity := CircularCapacity;
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
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(ItemPtr) + PtrUInt(fItemSize));
If PtrUInt(Result) >= PtrUInt(fHighMemory) then
  Result := fMemory;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
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
    For i := 1 to fCount do
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
// there should be no valid item in the vector by now...
fFirstItemPosition := 0;
If ManagedItemStreaming then
  begin
    // managed IO
    TempPtr := GetItemPtr(LowIndex);
    For i := 1 to fCount do
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

constructor TSequentialVector.Create(OperationMode: TSVOperationMode; ItemSize: TMemSize; CircularCapacity: Integer = -1);
begin
inherited Create;
Initialize(OperationMode,ItemSize,CircularCapacity);
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
    For i := 1 to fCount do
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
If (fCircularCapacity > 0) and (fCount >= fCircularCapacity) and (fCircularCapacity = fCapacity) then
  begin
  {
    Operating as circular buffer with limited size - remove oldest item and
    replace it with the new one. Remember to do implicit item cleanup.
  }
    ChangedItem := GetItemPtr(LowIndex);
    ItemFinal(ChangedItem);
    ItemAssign(ItemPtr,ChangedItem);
    Inc(fFirstItemPosition);
    If fFirstItemPosition >= fCapacity then
      fFirstItemPosition := 0;
    DoChange;
  end
else
  begin
    // vanilla stuff
    Grow;
    Inc(fCount);
    ItemAssign(ItemPtr,GetItemPtr(HighIndex));
    DoChange;
  end;
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
        For i := 1 to fCount do
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
            // the items are split into two blocks (possible only in FIFO)
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
FinalizeAllItems;
fCount := 0;  // setting capacity below count would raise an exception
SetCapacity(Stream_GetInt32(Stream));
fCount := fCapacity;
If fCount > 0 then
  InternalReadFromStream(Stream);
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

constructor TIntegerSequentialVector.Create(OperationMode: TSVOperationMode; CircularCapacity: Integer = -1);
begin
inherited Create(OperationMode,SizeOf(Integer),CircularCapacity);
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

constructor TIntegerFIFOVector.Create(CircularCapacity: Integer = -1);
begin
inherited Create(omFIFO,CircularCapacity);
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

constructor TIntegerLIFOVector.Create(CircularCapacity: Integer = -1);
begin
inherited Create(omLIFO,CircularCapacity);
end;


end.
