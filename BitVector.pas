{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitVector

    Provides classes that can be used to access individual bits of memory in
    a list-like manner.

  Version 1.4.3 (2024-04-14)

  Last change 2024-04-14

  ©2015-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BitVector

  Dependencies:
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions       - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    BitOps              - github.com/TheLazyTomcat/Lib.BitOps
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BitVector_UseAuxExceptions for details).

  BinaryStreamingLite can be replaced by full BinaryStreaming.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    BasicUIM    - github.com/TheLazyTomcat/Lib.BasicUIM
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit BitVector;
{
  BitVector_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  BitVector_UseAuxExceptions to achieve this.
}
{$IF Defined(BitVector_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------ 

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
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
  EBVException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EBVIndexOutOfBounds       = class(EBVException);
  EBVNonReallocatableMemory = class(EBVException);
  EBVInvalidValue           = class(EBVException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TBitVector                                   
--------------------------------------------------------------------------------
===============================================================================}
type
  TBVOperations = record
    BoolOperation:  Function(A,B: Boolean): Boolean;
    ByteOperation:  Function(A,B: Byte): Byte;
    WordOperation:  Function(A,B: NativeUInt): NativeUInt;
  end;

{===============================================================================
    TBitVector - class declaration
===============================================================================}
type
  TBitVector = class(TCustomListObject)
  protected
    fConstructorSetup:  Boolean;  // to allow SetCount and SetCapacity in static vectors
    fOwnsMemory:        Boolean;
    fMemSize:           TMemSize;
    fMemory:            Pointer;
    fCount:             Integer;
    fPopCount:          Integer;
    fStatic:            Boolean;  // when true, the memory cannot be reallocated, but can be written into
    fChangeCounter:     Integer;
    fChanged:           Boolean;
    fOnChangeEvent:     TNotifyEvent;
    fOnChangeCallback:  TNotifyCallback;
    // following four methods do not check index for validity
    Function GetBytePtrBitIdx(BitIndex: Integer): PByte; virtual;
    Function GetBytePtrByteIdx(ByteIndex: Integer): PByte; virtual;
    Function GetBit_LL(Index: Integer): Boolean; virtual;
    Function SetBit_LL(Index: Integer; Value: Boolean): Boolean; virtual;  // returns old value
    Function GetBit(Index: Integer): Boolean; virtual;
    procedure SetBit(Index: Integer; Value: Boolean); virtual;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    Function MemoryCanBeReallocated: Boolean; virtual;
    procedure ShiftDown(Idx1,Idx2: Integer); virtual;
    procedure ShiftUp(Idx1,Idx2: Integer); virtual;
    procedure ScanForPopCount; virtual;
    procedure CombineInternal(Memory: Pointer; Count: Integer; Operations: TBVOperations); virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure DoChange; virtual;
  public
    constructor Create(Memory: Pointer; Count: Integer); overload; virtual;
    constructor Create(InitialCount: Integer = 0; InitialValue: Boolean = False); overload; virtual;
    destructor Destroy; override;   
    procedure BeginChanging;
    Function EndChanging: Integer;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function First: Boolean; virtual;
    Function Last: Boolean; virtual;
    Function Add(Value: Boolean): Integer; virtual;
    procedure Insert(Index: Integer; Value: Boolean); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Move(SrcIdx, DstIdx: Integer); virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure Assign(Memory: Pointer; Count: Integer); overload; virtual;
    procedure Assign(Vector: TBitVector); overload; virtual;
    procedure Append(Memory: Pointer; Count: Integer); overload; virtual;
    procedure Append(Vector: TBitVector); overload; virtual;
    procedure Put(Index: Integer; Memory: Pointer; Count: Integer); overload; virtual;
    procedure Put(Index: Integer; Vector: TBitVector); overload; virtual;
    procedure Fill(FromIdx,ToIdx: Integer; Value: Boolean); overload; virtual;
    procedure Fill(Value: Boolean); overload; virtual;
    procedure Complement(FromIdx,ToIdx: Integer); overload; virtual;
    procedure Complement; overload; virtual;
    procedure Reverse; virtual;
    procedure Combine(Memory: Pointer; Count: Integer; Operations: TBVOperations); overload; virtual;
    procedure Combine(Vector: TBitVector; Operations: TBVOperations); overload; virtual;
    procedure CombineAND(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineAND(Vector: TBitVector); overload; virtual;
    procedure CombineOR(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineOR(Vector: TBitVector); overload; virtual;
    procedure CombineXOR(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineXOR(Vector: TBitVector); overload; virtual;
    Function IsEmpty: Boolean; virtual;
    Function IsFull: Boolean; virtual;
    Function IsEqual(Vector: TBitVector): Boolean; virtual;
    Function FirstSet: Integer; virtual;
    Function FirstClean: Integer; virtual;
    Function LastSet: Integer; virtual;
    Function LastClean: Integer; virtual;
    procedure WriteToStream(Stream: TStream); virtual;
    procedure ReadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure WriteToFile(const FileName: String); virtual;
    procedure ReadFromFile(const FileName: String); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property OwnsMemory: Boolean read fOwnsMemory;
    property MemorySize: TMemSize read fMemSize;
    property Memory: Pointer read fMemory;
    property PopCount: Integer read fPopCount;
    property Static: Boolean read fStatic;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TBitVectorStatic
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVectorStatic - class declaration
===============================================================================}
type
  TBitVectorStatic = class(TBitVector)
  protected
    procedure Initialize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TBitVectorStatic32                               
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVectorStatic32 - class declaration
===============================================================================}
type
  TBitVectorStatic32 = class(TBitVectorStatic)
  public
    constructor Create(Memory: Pointer; Count: Integer); overload; override;
    constructor Create(InitialCount: Integer = 0; InitialValue: Boolean = False); overload; override;
    Function FirstSet: Integer; override;
    Function FirstClean: Integer; override;
    Function LastSet: Integer; override;
    Function LastClean: Integer; override;
  end;

implementation

uses
  Math,
  BitOps, StrRect, BinaryStreamingLite;

{===============================================================================
--------------------------------------------------------------------------------
                                   TBitVector                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVector - auxiliaty types, constants and functions
===============================================================================}
const
  BV_ALLOCDELTA_BITS  = 128;  // allocation granularity
  BV_ALLOCDELTA_BYTES = BV_ALLOCDELTA_BITS div 8;

{$IF SizeOf(NativeUInt) = 8}
  BV_NATINT_BYTES = 8;
  BV_NATINT_BITS  = 64;
  BV_NATINT_MAX   = NativeUInt($FFFFFFFFFFFFFFFF);
{$ELSEIF SizeOf(NativeUInt) = 4}
  BV_NATINT_BYTES = 4;
  BV_NATINT_BITS  = 32;
  BV_NATINT_MAX   = NativeUInt($FFFFFFFF);
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}

//==============================================================================

Function BoolFillValue(Value: Boolean): NativeUInt;
begin
If Value then
  Result := BV_NATINT_MAX
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function BoolOperation_AND(A,B: Boolean): Boolean;
begin
Result := A and B;
end;

//------------------------------------------------------------------------------

Function BoolOperation_OR(A,B: Boolean): Boolean;
begin
Result := A or B;
end;
 
//------------------------------------------------------------------------------

Function BoolOperation_XOR(A,B: Boolean): Boolean;
begin
Result := A xor B;
end;

//------------------------------------------------------------------------------

Function ByteOperation_AND(A,B: Byte): Byte;
begin
Result := A and B;
end;

//------------------------------------------------------------------------------

Function ByteOperation_OR(A,B: Byte): Byte;
begin
Result := A or B;
end;
 
//------------------------------------------------------------------------------

Function ByteOperation_XOR(A,B: Byte): Byte;
begin
Result := A xor B;
end;

//------------------------------------------------------------------------------

Function WordOperation_AND(A,B: NativeUInt): NativeUInt;
begin
Result := A and B;
end;

//------------------------------------------------------------------------------

Function WordOperation_OR(A,B: NativeUInt): NativeUInt;
begin
Result := A or B;
end;
 
//------------------------------------------------------------------------------

Function WordOperation_XOR(A,B: NativeUInt): NativeUInt;
begin
Result := A xor B;
end;

//==============================================================================
const
  AndOps: TBVOperations = (
    BoolOperation:  BoolOperation_AND;
    ByteOperation:  ByteOperation_AND;
    WordOperation:  WordOperation_AND);

  OrOps: TBVOperations = (
    BoolOperation:  BoolOperation_OR;
    ByteOperation:  ByteOperation_OR;
    WordOperation:  WordOperation_OR);

  XorOps: TBVOperations = (
    BoolOperation:  BoolOperation_XOR;
    ByteOperation:  ByteOperation_XOR;
    WordOperation:  WordOperation_XOR);

//==============================================================================
// endianness correction

Function EndCor32(Value: UInt32): UInt32; {$IFDEF CanInline} inline;{$ENDIF}
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(Value);
{$ELSE}
Result := Value;
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function EndCor(Value: NativeUInt): NativeUInt; {$IFDEF CanInline} inline;{$ENDIF}
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(Value);
{$ELSE}
Result := Value;
{$ENDIF}
end;

{===============================================================================
    TBitVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBitVector - protected methods
-------------------------------------------------------------------------------}

Function TBitVector.GetBytePtrBitIdx(BitIndex: Integer): PByte;
begin
Result := PByte(PtrAdvance(fMemory,PtrInt(BitIndex shr 3)));
end;

//------------------------------------------------------------------------------

Function TBitVector.GetBytePtrByteIdx(ByteIndex: Integer): PByte;
begin
Result := PByte(PtrAdvance(fMemory,PtrInt(ByteIndex)));
end;

//------------------------------------------------------------------------------

Function TBitVector.GetBit_LL(Index: Integer): Boolean;
begin
Result := BT(GetBytePtrBitIdx(Index)^,Index and 7);
end;

//------------------------------------------------------------------------------

Function TBitVector.SetBit_LL(Index: Integer; Value: Boolean): Boolean;
begin
Result := BitSetTo(GetBytePtrBitIdx(Index)^,Index and 7,Value);
end;

//------------------------------------------------------------------------------

Function TBitVector.GetBit(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := GetBit_LL(Index)
else
  raise EBVIndexOutOfBounds.CreateFmt('TBitVector.GetBit: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TBitVector.SetBit(Index: Integer; Value: Boolean);
begin
If CheckIndex(Index) then
  begin
    If Value <> SetBit_LL(Index,Value) then
      begin
        If Value then
          Inc(fPopCount)
        else
          Dec(fPopCount);
        DoChange;
      end;
  end
else raise EBVIndexOutOfBounds.CreateFmt('TBitVector.SetBit: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TBitVector.GetCapacity: Integer;
begin
Result := Integer(fMemSize shl 3);
end;

//------------------------------------------------------------------------------

procedure TBitVector.SetCapacity(Value: Integer);
var
  NewMemSize: TMemSize;
begin
If MemoryCanBeReallocated then
  begin
    If Value >= 0 then
      begin
        NewMemSize := Ceil(Value / BV_ALLOCDELTA_BITS) * BV_ALLOCDELTA_BYTES;
        If fMemSize <> NewMemSize then
          begin
            ReallocMem(fMemory,NewMemSize);
            fMemSize := NewMemSize;
            // adjust count if capacity gets below it
            If Capacity < fCount then
              begin
                fCount := Capacity;
                ScanForPopCount;
                DoChange;
              end;
          end;
      end
    else raise EBVInvalidValue.CreateFmt('TBitVector.SetCapacity: Invalid capacity (%d).',[Value]);
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.SetCapacity: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

Function TBitVector.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

procedure TBitVector.SetCount(Value: Integer);
var
  i:  Integer;
begin
If MemoryCanBeReallocated then
  begin
    If Value >= 0 then
      begin
        If Value <> fCount then
          begin
            BeginChanging;
            try
              If Value > Capacity then
                SetCapacity(Value); // alloc new capacity
              If Value > fCount then
                begin
                  // add new bits, and reset them (pop count is not changed)
                  // partial byte...
                  If (fCount and 7) <> 0 then
                    SetBitsValue(GetBytePtrBitIdx(Pred(fCount))^,0,fCount and 7,7,False);
                  // full bytes...
                  For i := ((fCount + 7) shr 3) to (Pred(Value) shr 3) do
                    GetBytePtrByteIdx(i)^ := 0;
                  fCount := Value;
                end
              else
                begin
                  // remove existing bits
                  fCount := Value;
                  ScanForPopCount;
                end;
              DoChange;
            finally
              EndChanging;
            end;
          end;
      end
    else raise EBVInvalidValue.CreateFmt('TBitVector.SetCount: Invalid count (%d).',[Value]);;
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.SetCount: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

Function TBitVector.MemoryCanBeReallocated: Boolean;
begin
Result := (fOwnsMemory and not fStatic) or fConstructorSetup;
end;

//------------------------------------------------------------------------------

procedure TBitVector.ShiftDown(Idx1,Idx2: Integer);
var
  ByteCount:  Integer;
  Carry:      Boolean;
  MovingPtr:  Pointer;
begin
If Idx2 > Idx1 then
  begin
    If (Idx1 shr 3) <> (Idx2 shr 3)  then
      begin
        // shift is done across at least one byte boundary
        ByteCount := Pred((Idx2 shr 3) - (Idx1 shr 3));
        MovingPtr := GetBytePtrBitIdx(Idx2);
        // shift last byte and preserve shifted-out bit
        Carry := GetBit_LL(Idx2 and not 7); // bit 0 of last byte
        SetBitsValue(PByte(MovingPtr)^,PByte(MovingPtr)^ shr 1,0,Idx2 and 7,False);
        // shift native words
        while ByteCount >= BV_NATINT_BYTES do
          begin
            Dec(PNativeUInt(MovingPtr));
            Dec(ByteCount,BV_NATINT_BYTES);
            PNativeUInt(MovingPtr)^ := EndCor(RCRCarry(EndCor(PNativeUInt(MovingPtr)^),1,Carry));
          end;
        // shift whole bytes
        while ByteCount > 0 do
          begin
            Dec(PByte(MovingPtr));
            Dec(ByteCount);
            RCRValueCarry(PByte(MovingPtr)^,1,Carry);
          end;
        // shift first byte and store carry
        Dec(PByte(MovingPtr));
        SetBitsValue(PByte(MovingPtr)^,PByte(MovingPtr)^ shr 1,Idx1 and 7,7,False);
        SetBit_LL(Idx1 or 7,Carry);
      end
    // shift is done within a single byte
    else SetBitsValue(GetBytePtrBitIdx(Idx1)^,GetBytePtrBitIdx(Idx1)^ shr 1,Idx1 and 7,Idx2 and 7,False);
  end
else raise EBVInvalidValue.CreateFmt('TBitVector.ShiftDown: Invalid indices (%d, %d).',[Idx1,Idx2]);
end;

//------------------------------------------------------------------------------

procedure TBitVector.ShiftUp(Idx1,Idx2: Integer);
var
  ByteCount:  Integer;
  Carry:      Boolean;
  MovingPtr:  Pointer;
begin
If Idx2 > Idx1 then
  begin
    If (Idx1 shr 3) <> (Idx2 shr 3)  then
      begin
        // shift is done across at least one byte boundary
        ByteCount := Pred((Idx2 shr 3) - (Idx1 shr 3));
        MovingPtr := GetBytePtrBitIdx(Idx1);        
        // shift first byte and preserve shifted-out bit
        Carry := GetBit_LL(Idx1 or 7);
        SetBitsValue(PByte(MovingPtr)^,Byte(PByte(MovingPtr)^ shl 1),Idx1 and 7,7,False);
        Inc(PByte(MovingPtr));
        // shift native words
        while ByteCount >= BV_NATINT_BYTES do
          begin
            PNativeUInt(MovingPtr)^ := EndCor(RCLCarry(EndCor(PNativeUInt(MovingPtr)^),1,Carry));
            Inc(PNativeUInt(MovingPtr));
            Dec(ByteCount,BV_NATINT_BYTES);
          end;
        // shift whole bytes
        while ByteCount > 0 do
          begin
            RCLValueCarry(PByte(MovingPtr)^,1,Carry);
            Inc(PByte(MovingPtr));
            Dec(ByteCount);
          end;
        // shift last byte and store carry
        SetBitsValue(PByte(MovingPtr)^,Byte(PByte(MovingPtr)^ shl 1),0,Idx2 and 7,False);
        SetBit_LL(Idx2 and not 7,Carry);
      end
    // shift is done inside of one byte
    else SetBitsValue(GetBytePtrBitIdx(Idx1)^,Byte(GetBytePtrBitIdx(Idx1)^ shl 1),Idx1 and 7,Idx2 and 7,False);
  end
else raise EBVInvalidValue.CreateFmt('TBitVector.ShiftUp: Invalid indices (%d, %d).',[Idx1,Idx2]);
end;

//------------------------------------------------------------------------------

procedure TBitVector.ScanForPopCount;
var
  BitCount:   Integer;
  MovingPtr:  Pointer;
begin
fPopCount := 0;
BitCount := fCount;
If BitCount > 0 then
  begin
    MovingPtr := fMemory;
    // full natives...
    while BitCount >= BV_NATINT_BITS do
      begin
        Inc(fPopCount,BitOps.PopCount(PNativeUInt(MovingPtr)^));
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtr));
      end;
    // full bytes...
    while BitCount >= 8 do
      begin
        Inc(fPopCount,BitOps.PopCount(PByte(MovingPtr)^));
        Dec(BitCount,8);
        Inc(PByte(MovingPtr));
      end;
    // partial byte...
    If BitCount > 0 then
      Inc(fPopCount,BitOps.PopCount(Byte(PByte(MovingPtr)^ and ($FF shr (8 - (BitCount and 7))))));
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.CombineInternal(Memory: Pointer; Count: Integer; Operations: TBVOperations);
var
  MovingPtr:  Pointer;
  TempA:      Byte;
  TempB:      Byte;
  TempR:      Byte;
  i:          Integer;
begin
If Count > 0 then
  begin
    If Count > fCount then
      Count := fCount;
    MovingPtr := fMemory;
    // combine whole natives
    while Count >= BV_NATINT_BITS do
      begin
        PNativeUInt(MovingPtr)^ := Operations.WordOperation(PNativeUInt(MovingPtr)^,PNativeUInt(Memory)^);
        Inc(PNativeUInt(Memory));
        Inc(PNativeUInt(MovingPtr));
        Dec(Count,BV_NATINT_BITS);
      end;
    // combine whole bytes
    while Count >= 8 do
      begin
        PByte(MovingPtr)^ := Operations.ByteOperation(PByte(MovingPtr)^,PByte(Memory)^);
        Inc(PByte(Memory));
        Inc(PByte(MovingPtr));
        Dec(Count,8);
      end;
    // combine remaining bits
    If Count > 0 then
      begin
        TempA := PByte(MovingPtr)^;
        TempB := PByte(Memory)^;
        TempR := 0;
        For i := 0 to Pred(Count) do
          TempR := TempR or Byte(IfThen(Operations.BoolOperation(
            ((TempA shr i) and 1) <> 0,((TempB shr i) and 1) <> 0),1,0) shl i);
        SetBitsValue(PByte(MovingPtr)^,TempR,0,Pred(Count and 7),False);
      end;
    ScanForPopCount;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Initialize;
begin
fConstructorSetup := False;
fOwnsMemory := True;
fMemSize := 0;
fMemory := nil;
fCount := 0;
fPopCount := 0;
fStatic := False;
fChangeCounter := 0;
fChanged := False;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Finalize;
begin
If fOwnsMemory then
  FreeMem(fMemory,fMemSize);
end;

//------------------------------------------------------------------------------

procedure TBitVector.DoChange;
begin
fChanged := True;
If (fChangeCounter <= 0) then
  begin
    If Assigned(fOnChangeEvent) then
      fOnChangeEvent(Self)
    else If Assigned(fOnChangeCallback) then
      fOnChangeCallback(Self);
  end;
end;

{-------------------------------------------------------------------------------
    TBitVector - public methods
-------------------------------------------------------------------------------}

constructor TBitVector.Create(Memory: Pointer; Count: Integer);
begin
inherited Create;
Initialize;
fOwnsMemory := False;
fMemSize := (Count + 7) shr 3;
fMemory := Memory;
fCount := Count;
ScanForPopCount;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitVector.Create(InitialCount: Integer = 0; InitialValue: Boolean = False);
begin
inherited Create;
Initialize;
fOwnsMemory := True;
fConstructorSetup := True;
try
  SetCount(InitialCount); // sets capacity and therefore also fMemSize and fMemory
finally
  fConstructorSetup := False;
end;
{
  No need to call Fill when InitialValue is false since the memory was
  implicitly cleared in a call to SetCount.
}
If InitialValue then
  Fill(True);
end;

//------------------------------------------------------------------------------

destructor TBitVector.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TBitVector.BeginChanging;
begin
If fChangeCounter <= 0 then
  fChanged := False;
Inc(fChangeCounter);
end;

//------------------------------------------------------------------------------

Function TBitVector.EndChanging: Integer;
begin
Dec(fChangeCounter);
If fChangeCounter <= 0 then
  begin
    fChangeCounter := 0;
    If fChanged then
      DoChange;
  end;
Result := fChangeCounter;  
end;

//------------------------------------------------------------------------------

Function TBitVector.LowIndex: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TBitVector.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TBitVector.First: Boolean;
begin
Result := GetBit(LowIndex);
end;

//------------------------------------------------------------------------------

Function TBitVector.Last: Boolean;
begin
Result := GetBit(HighIndex);
end;

//------------------------------------------------------------------------------

Function TBitVector.Add(Value: Boolean): Integer;
begin
If MemoryCanBeReallocated then
  begin
    Grow;
    Inc(fCount);
    SetBit_LL(HighIndex,Value);
    If Value then
      Inc(fPopCount);
    Result := HighIndex;
    DoChange;
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Add: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

procedure TBitVector.Insert(Index: Integer; Value: Boolean);
begin
If MemoryCanBeReallocated then
  begin
    If CheckIndex(Index) then
      begin
        Grow;
        Inc(fCount);  // must be here because of shifting
        ShiftUp(Index,HighIndex);
        SetBit_LL(Index,Value);
        If Value then
          Inc(fPopCount);
        DoChange;
      end
    else If Index = fCount then
      Add(Value)
    else
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Insert: Index (%d) out of bounds.',[Index]);
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Insert: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

procedure TBitVector.Exchange(Index1,Index2: Integer);
begin
If Index1 <> Index2 then
  begin
    If not CheckIndex(Index1) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Exchange: Index #1 (%d) out of bounds.',[Index1]);
    If not CheckIndex(Index2) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Exchange: Index #2 (%d) out of bounds.',[Index2]);
    SetBit_LL(Index2,SetBit_LL(Index1,GetBit_LL(Index2)));
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Move(SrcIdx, DstIdx: Integer);
var
  Temp: Boolean;
begin
If SrcIdx <> DstIdx then
  begin
    If not CheckIndex(SrcIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Exchange: Source index (%d) out of bounds.',[SrcIdx]);
    If not CheckIndex(DstIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Exchange: Destination index (%d) out of bounds.',[DstIdx]);
    Temp := GetBit_LL(SrcIdx);
    If SrcIdx < DstIdx then
      ShiftDown(SrcIdx,DstIdx)
    else
      ShiftUp(DstIdx,SrcIdx);
    SetBit_LL(DstIdx,Temp);
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Delete(Index: Integer);
begin
If MemoryCanBeReallocated then
  begin
    If CheckIndex(Index) then
      begin
        If GetBit_LL(Index) then
          Dec(fPopCount);
        If Index < HighIndex then
          ShiftDown(Index,HighIndex);
        Dec(fCount);
        Shrink;
        DoChange;
      end
    else raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Delete: Index (%d) out of bounds.',[Index]);
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Delete: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

procedure TBitVector.Clear;
begin
If MemoryCanBeReallocated then
  begin
    fCount := 0;
    fPopCount := 0;
    Shrink;
    DoChange;
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Clear: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

procedure TBitVector.Assign(Memory: Pointer; Count: Integer);
begin
If MemoryCanBeReallocated or (Count = fCount) then
  begin
    BeginChanging;
    try
      If Count <> fCount then
        SetCount(Count);  // also sets fCount
      // whole bytes
      System.Move(Memory^,fMemory^,Count shr 3);
      // remaining bits
      If (Count and 7) <> 0 then
        SetBitsValue(GetBytePtrByteIdx(Count shr 3)^,
                     PByte(PtrAdvance(Memory,PtrInt(Count shr 3)))^,
                     0,Pred(Count and 7),False);
      ScanForPopCount;
      DoChange;
    finally
      EndChanging;
    end;
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Assign: Memory cannot be reallocated.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Assign(Vector: TBitVector);
begin
Assign(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.Append(Memory: Pointer; Count: Integer);
var
  BytesCount:   Integer;
  RShift:       Integer;
  LShift:       Integer;
  MovingPtr:    Pointer;
  NextBytePtr:  Pointer;
begin
If MemoryCanBeReallocated then
  begin
    If Count > 0 then
      begin
        If (fCount and 7) <> 0 then
          begin
            // last byte is partial
          {
            Copy everything into newly allocated whole bytes and then shift this
            memory down so it "touches" the current last bit.
          }
            SetCapacity(Succ(fCount or 7) + Count);
            BytesCount := (Count + 7) shr 3;
            System.Move(Memory^,GetBytePtrByteIdx(Succ(fCount shr 3))^,BytesCount);
            LShift := fCount and 7;
            RShift := 8 - LShift;
            MovingPtr := GetBytePtrBitIdx(HighIndex);
            while BytesCount >= BV_NATINT_BYTES do
              begin
                NextBytePtr := PtrAdvance(MovingPtr,1);
                SetBitsValue(PByte(MovingPtr)^,Byte(PByte(NextBytePtr)^ shl LShift),LShift,7,False);
                PNativeUInt(NextBytePtr)^ := EndCor(EndCor(PNativeUInt(NextBytePtr)^) shr RShift);
                Inc(PNativeUInt(MovingPtr));
                Dec(BytesCount,BV_NATINT_BYTES);
              end;
            while BytesCount > 0 do
              begin
                NextBytePtr := PtrAdvance(MovingPtr,1);
                SetBitsValue(PByte(MovingPtr)^,Byte(PByte(NextBytePtr)^ shl LShift),LShift,7,False);
                PByte(NextBytePtr)^ := PByte(NextBytePtr)^ shr RShift;
                Inc(PByte(MovingPtr));
                Dec(BytesCount);
              end;
          end
        else
          begin
            // currently contains only whole bytes
            SetCapacity(fCount + Count);
            System.Move(Memory^,GetBytePtrByteIdx(fCount shr 3)^,(Count + 7) shr 3);
          end;
        Inc(fCount,Count);
        ScanForPopCount;
        DoChange;
      end;
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Append: Memory cannot be reallocated.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Append(Vector: TBitVector);
begin
Append(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.Put(Index: Integer; Memory: Pointer; Count: Integer);
var
  LShift:         Integer;
  RShift:         Integer;
  MovingPtrSelf:  Pointer;
  MovingPtr:      Pointer;
  PartialEndFill: Boolean;
begin
If MemoryCanBeReallocated or ((Index + Count) <= fCount) then
  begin
    If CheckIndex(Index) or ((fCount <= 0) and (Index = 0)) then
      begin
        If Count > 0 then
          begin
            BeginChanging;
            try
              If Index + Count > fCount then
                SetCount(Index + Count);
              If (Index and 7) <> 0 then
                begin
                  // complex copy with shift
                  LShift := Index and 7;
                  RShift := 8 - LShift;
                  MovingPtrSelf := GetBytePtrByteIdx(Index shr 3);
                  MovingPtr := Memory;
                  PartialEndFill := RShift > (Count and 7);
                  // full native words
                  while Count >= BV_NATINT_BITS do
                    begin
                      SetBitsValue(PByte(MovingPtrSelf)^,Byte(PByte(MovingPtr)^ shl LShift),LShift,7,False);
                      Inc(PByte(MovingPtrSelf));
                      Dec(Count,BV_NATINT_BITS);
                      If (Count <= 0) or ((Count < 8{this must be 8!}) and PartialEndFill) then
                        PNativeUInt(MovingPtrSelf)^ := EndCor(SetBits(EndCor(PNativeUInt(MovingPtrSelf)^),
                          EndCor(PNativeUInt(MovingPtr)^) shr RShift,0,Pred(BV_NATINT_BITS - RShift),False))
                      else
                        PNativeUInt(MovingPtrSelf)^ := EndCor(EndCor(PNativeUInt(MovingPtr)^) shr RShift); 
                      Inc(PNativeUInt(MovingPtr));
                      Inc(PByte(MovingPtrSelf),Pred(BV_NATINT_BYTES));
                    end;
                  // full bytes
                  while Count >= 8 do
                    begin
                      SetBitsValue(PByte(MovingPtrSelf)^,Byte(PByte(MovingPtr)^ shl LShift),LShift,7,False);
                      Inc(PByte(MovingPtrSelf));
                      Dec(Count,8);
                      If (Count <= 0) or ((Count < 8) and PartialEndFill) then
                        SetBitsValue(PByte(MovingPtrSelf)^,PByte(MovingPtr)^ shr RShift,0,Pred(LShift),False)
                      else
                        PByte(MovingPtrSelf)^ := Byte(PByte(MovingPtr)^ shr RShift);
                      Inc(PByte(MovingPtr));
                    end;
                  // trailing bits
                  If Count <> 0 then
                    begin
                      If not PartialEndFill then
                        begin
                          SetBitsValue(PByte(MovingPtrSelf)^,Byte(PByte(MovingPtr)^ shl LShift),LShift,7,False);
                          Dec(Count,RShift);
                          If Count > 0 then
                            begin
                              Inc(PByte(MovingPtrSelf));
                              SetBitsValue(PByte(MovingPtrSelf)^,PByte(MovingPtr)^ shr RShift,0,Pred(Count),False);
                            end;
                        end
                      else SetBitsValue(PByte(MovingPtrSelf)^,Byte(PByte(MovingPtr)^ shl LShift),LShift,Pred(LShift + Count),False);
                    end;
                end
              else
                begin
                  // simple copy, no shift needed, copy integral bytes
                  System.Move(Memory^,GetBytePtrByteIdx(Index shr 3)^,Count shr 3);
                  // and now the remaining bits
                  If (Count and 7) <> 0 then
                    SetBitsValue(GetBytePtrByteIdx((Index + Count) shr 3)^,
                                 PByte(PtrAdvance(Memory,PtrInt(Count shr 3)))^,
                                 0,Pred(Count and 7),False);
                end;
              ScanForPopCount;
              DoChange;
            finally
              EndChanging;
            end;
          end;
      end
    else raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Put: Index (%d) out of bounds.',[Index]);
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.Put: Memory cannot be reallocated.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Put(Index: Integer; Vector: TBitVector);
begin
Put(Index,Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.Fill(FromIdx,ToIdx: Integer; Value: Boolean);
var
  FillValue:  NativeUInt;
  BitCount:   Integer;
  MovingPtr:  Pointer;
  ByteBits:   Integer;
begin
If (FromIdx <= ToIdx) and (fCount > 0) then
  begin
    If not CheckIndex(FromIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Fill: From index (%d) out of bounds.',[FromIdx]);
    If not CheckIndex(ToIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Fill: To index (%d) out of bounds.',[ToIdx]);
    If FromIdx <> ToIdx then
      begin
        FillValue := BoolFillValue(Value);
        BitCount := Succ(ToIdx - FromIdx);
        MovingPtr := GetBytePtrBitIdx(FromIdx);
        // partial first byte
        If (FromIdx and 7) <> 0 then
          begin
            ByteBits := Min(BitCount,8 - (FromIdx and 7));
            SetBitsValue(PByte(MovingPtr)^,Byte(FillValue),FromIdx and 7,Pred((FromIdx and 7) + ByteBits),False);
            Dec(BitCount,ByteBits);
            Inc(PByte(MovingPtr));
          end;
        // full natives  
        while BitCount >= BV_NATINT_BITS do
          begin
            PNativeUInt(MovingPtr)^ := FillValue;
            Dec(BitCount,BV_NATINT_BITS);
            Inc(PNativeUInt(MovingPtr));
          end;
        // full bytes
        while BitCount >= 8 do
          begin
            PByte(MovingPtr)^ := Byte(FillValue);
            Dec(BitCount,8);
            Inc(PByte(MovingPtr));
          end;
        // partial last byte
        If BitCount > 0 then
          SetBitsValue(PByte(MovingPtr)^,Byte(FillValue),0,ToIdx and 7,False);
        ScanForPopCount;
        DoChange;
      end
    else SetBit(FromIdx,Value);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Fill(Value: Boolean);
var
  FillValue:  NativeUInt;
  BitCount:   Integer;
  MovingPtr:  Pointer;
begin  
If fCount > 0 then
  begin
    FillValue := BoolFillValue(Value);
    BitCount := fCount;
    MovingPtr := fMemory;
    while BitCount >= BV_NATINT_BITS do
      begin
        PNativeUInt(MovingPtr)^ := FillValue;
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtr));
      end;
    while BitCount >= 8 do
      begin
        PByte(MovingPtr)^ := Byte(FillValue);
        Dec(BitCount,8);
        Inc(PByte(MovingPtr));
      end;
    If BitCount > 0 then
      SetBitsValue(PByte(MovingPtr)^,Byte(FillValue),0,Pred(fCount and 7),False);
    fPopCount := IfThen(Value,fCount,0);
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Complement(FromIdx,ToIdx: Integer);
var
  BitCount:   Integer;
  MovingPtr:  Pointer;
  ByteBits:   Integer;
begin
If (FromIdx <= ToIdx) and (fCount > 0) then
  begin
    If not CheckIndex(FromIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Complement: From index (%d) out of bounds.',[FromIdx]);
    If not CheckIndex(ToIdx) then
      raise EBVIndexOutOfBounds.CreateFmt('TBitVector.Complement: To index (%d) out of bounds.',[ToIdx]);
    If FromIdx <> ToIdx then
      begin
        BitCount := Succ(ToIdx - FromIdx);
        MovingPtr := GetBytePtrBitIdx(FromIdx);
        If (FromIdx and 7) <> 0 then
          begin
            ByteBits := Min(BitCount,8 - (FromIdx and 7));
            SetBitsValue(PByte(MovingPtr)^,not PByte(MovingPtr)^,FromIdx and 7,Pred((FromIdx and 7) + ByteBits),False);
            Dec(BitCount,ByteBits);
            Inc(PByte(MovingPtr));
          end;
        while BitCount >= BV_NATINT_BITS do
          begin
            PNativeUInt(MovingPtr)^ := not PNativeUInt(MovingPtr)^;
            Dec(BitCount,BV_NATINT_BITS);
            Inc(PNativeUInt(MovingPtr));
          end;
        while BitCount >= 8 do
          begin
            PByte(MovingPtr)^ := not PByte(MovingPtr)^;
            Dec(BitCount,8);
            Inc(PByte(MovingPtr));
          end;
        If BitCount > 0 then
          SetBitsValue(PByte(MovingPtr)^,not PByte(MovingPtr)^,0,ToIdx and 7,False);
        ScanForPopCount;
        DoChange;
      end
    else SetBit(FromIdx,not GetBit_LL(FromIdx));
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Complement;
var
  BitCount:   Integer;
  MovingPtr:  Pointer;
begin
If fCount > 0 then
  begin
    BitCount := fCount;
    MovingPtr := fMemory;
    while BitCount >= BV_NATINT_BITS do
      begin
        PNativeUInt(MovingPtr)^ := not PNativeUInt(MovingPtr)^;
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtr));
      end;
    while BitCount >= 8 do
      begin
        PByte(MovingPtr)^ := not PByte(MovingPtr)^;
        Dec(BitCount,8);
        Inc(PByte(MovingPtr));
      end;
    If BitCount > 0 then
      SetBitsValue(PByte(MovingPtr)^,not PByte(MovingPtr)^,0,Pred(fCount and 7),False);
    fPopCount := fCount - fPopCount;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Reverse;
var
  i:    Integer;
  Temp: Byte;
begin
If fCount > 1 then
  begin
    If (fCount and 7) = 0 then
      begin
        // there are only integral bytes
        For i := 0 to Pred(fCount shr 4) do
          begin
            Temp := GetBytePtrByteIdx(i)^;
            GetBytePtrByteIdx(i)^ := ReverseBits(GetBytePtrByteIdx(Pred(fCount shr 3) - i)^);
            GetBytePtrByteIdx(Pred(fCount shr 3) - i)^ := ReverseBits(Temp);
          end;
        // make sure the center byte - if any - is also reversed
        If (fCount shr 3) and 1 <> 0 then
          GetBytePtrByteIdx(fCount shr 4)^ := ReverseBits(GetBytePtrByteIdx(fCount shr 4)^);
      end
    else
      begin
        // there is a partial byte at the end
        For i := 0 to Pred(fCount shr 1) do
          SetBit_LL(i,SetBit_LL(Pred(fCount) - i,GetBit_LL(i)));
      end;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Combine(Memory: Pointer; Count: Integer; Operations: TBVOperations);
begin
If not Assigned(Operations.BoolOperation) then
  raise EBVInvalidValue.Create('TBitVector.Combine: Bool operation not assigned.');
If not Assigned(Operations.ByteOperation) then
  raise EBVInvalidValue.Create('TBitVector.Combine: Byte operation not assigned.');
If not Assigned(Operations.WordOperation) then
  raise EBVInvalidValue.Create('TBitVector.Combine: Word operation not assigned.');
CombineInternal(Memory,Count,Operations);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.Combine(Vector: TBitVector; Operations: TBVOperations);
begin
Combine(Vector.Memory,Vector.Count,Operations);
end;

//------------------------------------------------------------------------------

procedure TBitVector.CombineAND(Memory: Pointer; Count: Integer);
begin
CombineInternal(Memory,Count,AndOps);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.CombineAND(Vector: TBitVector);
begin
CombineAND(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.CombineOR(Memory: Pointer; Count: Integer);
begin
CombineInternal(Memory,Count,OrOps);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.CombineOR(Vector: TBitVector);
begin
CombineOR(Vector.Memory,Vector.Count);
end;
 
//------------------------------------------------------------------------------

procedure TBitVector.CombineXOR(Memory: Pointer; Count: Integer);
begin
CombineInternal(Memory,Count,XorOps);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.CombineXOR(Vector: TBitVector);
begin
CombineXOR(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

Function TBitVector.IsEmpty: Boolean;
begin
Result := fPopCount = 0;
end;

//------------------------------------------------------------------------------

Function TBitVector.IsFull: Boolean;
begin
Result := (fCount > 0) and (fPopCount >= fCount);
end;

//------------------------------------------------------------------------------

Function TBitVector.IsEqual(Vector: TBitVector): Boolean;
var
  BitCount:       Integer;
  MovingPtrSelf:  Pointer;
  MovingPtr:      Pointer;
  i:              Integer;
begin
Result := False;
If (fCount = Vector.Count) and (fPopCount = Vector.PopCount) then
  begin
    BitCount := fCount;
    MovingPtrSelf := fMemory;
    MovingPtr := Vector.Memory;
    // compare natives
    while BitCount >= BV_NATINT_BITS do
      begin
        If PNativeUInt(MovingPtrSelf)^ <> PNativeUInt(MovingPtr)^ then
          Exit;
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtrSelf));
        Inc(PNativeUInt(MovingPtr));
      end;
    // compare whole bytes
    while BitCount >= 8 do
      begin
        If PByte(MovingPtrSelf)^ <> PByte(MovingPtr)^ then
          Exit;
        Dec(BitCount,8);
        Inc(PByte(MovingPtrSelf));
        Inc(PByte(MovingPtr));
      end;
    // compare last partial byte if any
    If BitCount > 0 then
      For i := (fCount and not 7) to Pred(fCount) do
        If GetBit_LL(i) <> Vector[i] then
          Exit;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

Function TBitVector.FirstSet: Integer;
var
  BitCount:   Integer;
  MovingPtr:  Pointer;
  i:          Integer;
begin
If fCount > 0 then
  begin
    BitCount := fCount;
    MovingPtr := fMemory;
    // whole natives
    while BitCount >= BV_NATINT_BITS do
      begin
        If PNativeUInt(MovingPtr)^ <> 0 then
          begin
            Result := (fCount - BitCount) +
              BSF(EndCor(PNativeUInt(MovingPtr)^));
            Exit;
          end;
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtr));
      end;
    // whole bytes
    while BitCount >= 8 do
      begin
        If PByte(MovingPtr)^ <> 0 then
          begin
            Result := (fCount - BitCount) + BSF(PByte(MovingPtr)^);
            Exit;
          end;
        Dec(BitCount,8);
        Inc(PByte(MovingPtr));
      end;
    // last partial byte if any
    If BitCount > 0 then
      For i := (fCount and not 7) to Pred(fCount) do
        If GetBit_LL(i) then
          begin
            Result := i;
            Exit;
          end;
    Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TBitVector.FirstClean: Integer;
var
  BitCount:   Integer;
  MovingPtr:  Pointer;
  i:          Integer;
begin
If fCount > 0 then
  begin
    BitCount := fCount;
    MovingPtr := fMemory;
    while BitCount >= BV_NATINT_BITS do
      begin
        If PNativeUInt(MovingPtr)^ <> BV_NATINT_MAX then
          begin
            Result := (fCount - BitCount) +
              BSF(EndCor(not PNativeUInt(MovingPtr)^));
            Exit;
          end;
        Dec(BitCount,BV_NATINT_BITS);
        Inc(PNativeUInt(MovingPtr));
      end;
    while BitCount >= 8 do
      begin
        If PByte(MovingPtr)^ <> $FF then
          begin
            Result := (fCount - BitCount) + BSF(not PByte(MovingPtr)^);
            Exit;
          end;
        Dec(BitCount,8);
        Inc(PByte(MovingPtr));
      end;
    If BitCount > 0 then
      For i := (fCount and not 7) to Pred(fCount) do
        If not GetBit_LL(i) then
          begin
            Result := i;
            Exit;
          end;
    Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TBitVector.LastSet: Integer;
var
  i:          Integer;
  BitCount:   Integer;
  MovingPtr:  Pointer;
begin
If fCount > 0 then
  begin
    For i := Pred(fCount) downto (fCount and not 7) do
      If GetBit_LL(i) then
        begin
          Result := i;
          Exit;
        end;
    BitCount := fCount and not 7;
    MovingPtr := GetBytePtrBitIdx(fCount);
    while BitCount >= BV_NATINT_BITS do
      begin
        Dec(PNativeUInt(MovingPtr));
        Dec(BitCount,BV_NATINT_BITS);
        If PNativeUInt(MovingPtr)^ <> 0 then
          begin
            Result := BitCount + BSR(EndCor(PNativeUInt(MovingPtr)^));
            Exit;
          end;
      end;
    while BitCount >= 8 do
      begin
        Dec(PByte(MovingPtr));
        Dec(BitCount,8);
        If PByte(MovingPtr)^ <> 0 then
          begin
            Result := BitCount + BSR(PByte(MovingPtr)^);
            Exit;
          end;
      end;
    Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

Function TBitVector.LastClean: Integer;
var
  i:          Integer;
  BitCount:   Integer;
  MovingPtr:  Pointer;
begin
If fCount > 0 then
  begin
    For i := Pred(fCount) downto (fCount and not 7) do
      If not GetBit_LL(i) then
        begin
          Result := i;
          Exit;
        end;   
    BitCount := fCount and not 7;
    MovingPtr := GetBytePtrBitIdx(fCount);
    while BitCount >= BV_NATINT_BITS do
      begin
        Dec(PNativeUInt(MovingPtr));
        Dec(BitCount,BV_NATINT_BITS);
        If PNativeUInt(MovingPtr)^ <> BV_NATINT_MAX then
          begin
            Result := BitCount + BSR(EndCor(not PNativeUInt(MovingPtr)^));
            Exit;
          end;
      end;
    while BitCount >= 8 do
      begin
        Dec(PByte(MovingPtr));
        Dec(BitCount,8);
        If PByte(MovingPtr)^ <> $FF then
          begin
            Result := BitCount + BSR(not PByte(MovingPtr)^);
            Exit;
          end;
      end;
    Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

procedure TBitVector.WriteToStream(Stream: TStream);
var
  Temp: Byte;
begin
If fCount > 0 then
  begin
    // write whole bytes
    Stream.WriteBuffer(fMemory^,fCount shr 3);
    // prepare and write last partial byte, if any
    If (fCount and 7) <> 0 then
      begin
        Temp := SetBits(0,GetBytePtrByteIdx(fCount shr 3)^,0,Pred(fCount and 7),False);
        Stream.WriteBuffer(Temp,1);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.ReadFromStream(Stream: TStream);
var
  Temp: Byte;
begin
If fCount > 0 then
  begin
  {
    Note - here, the memory does not need to be reallocatable as we are reading
           only as many bits as is already present.
  }
    // read only data, first whole bytes...
    Stream.ReadBuffer(fMemory^,fCount shr 3);
    // ...and now remaining bits
    If (fCount and 7) <> 0 then
      begin
        Stream.ReadBuffer(Addr(Temp)^,1);
        SetBitsValue(GetBytePtrByteIdx(fCount shr 3)^,Temp,0,Pred(fCount and 7),False);
      end;
    ScanForPopCount;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.SaveToStream(Stream: TStream);
begin
// write number of bits first
Stream_WriteInt32(Stream,Int32(fCount));
WriteToStream(Stream);
end;

//------------------------------------------------------------------------------

procedure TBitVector.LoadFromStream(Stream: TStream);
begin
If MemoryCanBeReallocated then
  begin
    BeginChanging;
    try
      // read and set number of bits
      SetCount(Integer(Stream_GetInt32(Stream)));
      // read data (no need to do special read for trailing partial byte)
      Stream.ReadBuffer(fMemory^,(fCount + 7) shr 3);
      ScanForPopCount;
      DoChange;
    finally
      EndChanging;
    end;  
  end
else raise EBVNonReallocatableMemory.Create('TBitVector.LoadFromStream: Memory cannot be reallocated.');
end;

//------------------------------------------------------------------------------

procedure TBitVector.WriteToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  WriteToStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.ReadFromFile(const FileName: String);
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

procedure TBitVector.SaveToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  SaveToStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.LoadFromFile(const FileName: String);
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
                                TBitVectorStatic
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVectorStatic - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBitVectorStatic - protected methods
-------------------------------------------------------------------------------}

procedure TBitVectorStatic.Initialize;
begin
inherited;
fStatic := True;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TBitVectorStatic32
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVectorStatic32 - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBitVectorStatic32 - public methods
-------------------------------------------------------------------------------}

constructor TBitVectorStatic32.Create(Memory: Pointer; Count: Integer);
begin
If (Count and 31) = 0 then
  inherited Create(Memory,Count)
else
  raise EBVInvalidValue.CreateFmt('TBitVectorStatic32.Create: Count (%d) must be divisible by 32.',[Count]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBitVectorStatic32.Create(InitialCount: Integer = 0; InitialValue: Boolean = False);
begin
If (Count and 31) = 0 then
  inherited Create(InitialCount,InitialValue)
else
  raise EBVInvalidValue.CreateFmt('TBitVectorStatic32.Create: Count (%d) must be divisible by 32.',[Count]);
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.FirstSet: Integer;
var
  i:          Integer;
  MovingPtr:  PUInt32;
begin
Result := -1;
If fCount > 0 then
  begin
    MovingPtr := fMemory;
    For i := 0 to Pred(fCount shr 5) do
      begin
        If MovingPtr^ <> 0 then
          begin
            Result := (i * 32) + BSF(EndCor32(MovingPtr^));
            Break{For i};
          end;
        Inc(MovingPtr);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.FirstClean: Integer;
var
  i:          Integer;
  MovingPtr:  PUInt32;
begin
Result := -1;
If fCount > 0 then
  begin
    MovingPtr := fMemory;
    For i := 0 to Pred(fCount shr 5) do
      begin
        If MovingPtr^ <> $FFFFFFFF then
          begin
            Result := (i * 32) + BSF(EndCor32(not MovingPtr^));
            Break{For i};
          end;
        Inc(MovingPtr);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.LastSet: Integer;
var
  i:          Integer;
  MovingPtr:  PUInt32;
begin
Result := -1;
If fCount > 0 then
  begin
    MovingPtr := PtrAdvance(fMemory,PtrInt(fCount shr 3));
    For i := 0 to Pred(fCount shr 5) do
      begin
        Dec(MovingPtr);
        If MovingPtr^ <> 0 then
          begin
            Result := fCount - (Succ(i) * 32) + BSR(EndCor32(MovingPtr^));
            Break{For i};
          end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.LastClean: Integer;
var
  i:          Integer;
  MovingPtr:  PUInt32;
begin
Result := -1;
If fCount > 0 then
  begin
    MovingPtr := PtrAdvance(fMemory,PtrInt(fCount shr 3));
    For i := 0 to Pred(fCount shr 5) do
      begin
        Dec(MovingPtr);
        If MovingPtr^ <> $FFFFFFFF then
          begin
            Result := fCount - (Succ(i) * 32) + BSR(EndCor32(not MovingPtr^));
            Break{For i};
          end;
      end;
  end;
end;

end.
