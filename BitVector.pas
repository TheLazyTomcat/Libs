{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitVector classes

  Version 1.3.4 (2023-01-25)

  Last change 2023-05-01

  ©2015-2023 František Milt

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
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreaming    - github.com/TheLazyTomcat/Lib.BinaryStreaming
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect

    SimpleCPUID might not be needed, see BitOps library for details.

===============================================================================}
unit BitVector;

interface

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}

type
  EBVException = class(Exception);

  EBVIndexOutOfBounds = class(EBVException);
  EBVMemoryNotEditable = class(EBVException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TBitVector                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVector - class declaration
===============================================================================}
type
  TBitVector = class(TCustomListObject)
  protected
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
    procedure RaiseError(const MethodName, ErrorMessage: String; Values: array of const; ErrorType: Integer = -1); overload; virtual;
    procedure RaiseError(const MethodName, ErrorMessage: String; ErrorType: Integer = -1); overload; virtual;
    Function CheckIndexAndRaise(Index: Integer; const MethodName: String = 'CheckIndex'): Boolean; virtual;
    Function CheckMemoryEditable(const MethodName: String = 'MemoryEditable'; RaiseException: Boolean = True): Boolean; virtual;
    procedure ShiftDown(Idx1,Idx2: Integer); virtual;
    procedure ShiftUp(Idx1,Idx2: Integer); virtual;
    procedure ScanForPopCount; virtual;
    procedure Combine(Memory: Pointer; Count: Integer; Op: Integer); virtual;
    procedure Initialize; virtual;
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
    procedure Fill(FromIdx, ToIdx: Integer; Value: Boolean); overload; virtual;
    procedure Fill(Value: Boolean); overload; virtual;
    procedure Complement(FromIdx, ToIdx: Integer); overload; virtual;    
    procedure Complement; overload; virtual;
    procedure Clear; virtual;
    procedure Reverse; virtual;
    Function IsEmpty: Boolean; virtual;
    Function IsFull: Boolean; virtual;
    Function FirstSet: Integer; virtual;
    Function FirstClean: Integer; virtual;
    Function LastSet: Integer; virtual;
    Function LastClean: Integer; virtual;
    procedure Append(Memory: Pointer; Count: Integer); overload; virtual;
    procedure Append(Vector: TBitVector); overload; virtual;
    procedure Assign(Memory: Pointer; Count: Integer); overload; virtual;
    procedure Assign(Vector: TBitVector); overload; virtual;
    procedure CombineOR(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineOR(Vector: TBitVector); overload; virtual;
    procedure CombineAND(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineAND(Vector: TBitVector); overload; virtual;
    procedure CombineXOR(Memory: Pointer; Count: Integer); overload; virtual;
    procedure CombineXOR(Vector: TBitVector); overload; virtual;
    Function IsEqual(Vector: TBitVector): Boolean; virtual;
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
  BitOps, StrRect, BinaryStreaming;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TBitVector                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBitVector - auxiliaty constants and functions
===============================================================================}

const
  AllocDeltaBits  = 128;
  AllocDeltaBytes = AllocDeltaBits div 8;

  BV_COMBINE_OPERATOR_OR  = 0;
  BV_COMBINE_OPERATOR_AND = 1;
  BV_COMBINE_OPERATOR_XOR = 2;

  BV_ERROR_TYPE_IOOB = 1;
  BV_ERROR_TYPE_MENE = 2;

//------------------------------------------------------------------------------

Function BooleanOrd(Value: Boolean): Integer;
begin
If Value then Result := 1
  else Result := 0;
end;

{===============================================================================
    TBitVector - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBitVector - protected methods
-------------------------------------------------------------------------------}

Function TBitVector.GetBytePtrBitIdx(BitIndex: Integer): PByte;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PByte(PtrUInt(fMemory) + PtrUInt(BitIndex shr 3));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TBitVector.GetBytePtrByteIdx(ByteIndex: Integer): PByte;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PByte(PtrUInt(fMemory) + PtrUInt(ByteIndex));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
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
Result := False;
If CheckIndexAndRaise(Index,'GetBit') then
  Result := GetBit_LL(Index);
end;

//------------------------------------------------------------------------------

procedure TBitVector.SetBit(Index: Integer; Value: Boolean);
begin
If CheckIndexAndRaise(Index,'SetBit') then
  begin
    If Value <> SetBit_LL(Index,Value) then
      begin
        If Value then Inc(fPopCount)
          else Dec(fPopCount);
        DoChange;
      end;
  end;
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
If CheckMemoryEditable('SetCapacity') then
  begin
    If Value >= 0 then
      begin
        NewMemSize := Ceil(Value / AllocDeltaBits) * AllocDeltaBytes;
        If fMemSize <> NewMemSize then
          begin
            ReallocMem(fMemory,NewMemSize);
            fMemSize := NewMemSize;
            // adjust count is capacity gets below it
            If Capacity < fCount then
              begin
                fCount := Capacity;
                ScanForPopCount;
                DoChange;
              end;
          end;
      end
    else RaiseError('SetCapacity','Negative capacity not allowed.');
  end;
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
If CheckMemoryEditable('SetCount') then
  begin
    If Value >= 0 then
      begin
        If Value <> fCount then
          begin
            BeginChanging;
            try
              If Value > Capacity then
                Capacity := Value;  // alloc new capacity
              If Value > fCount then
                begin
                  // add new bits, and reset them
                  // partial byte...
                  If (fCount and 7) <> 0 then
                    SetBitsValue(GetBytePtrBitIdx(Pred(fCount))^,0,fCount and 7,7);
                  // full bytes...
                  For i := Ceil(fCount / 8) to (Pred(Value) shr 3) do
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
    else RaiseError('SetCount','Negative count not allowed.');
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.RaiseError(const MethodName, ErrorMessage: String; Values: array of const; ErrorType: Integer = -1);
begin
case ErrorType of
  BV_ERROR_TYPE_IOOB: raise EBVIndexOutOfBounds.CreateFmt(Format('%s.%s: %s',[Self.ClassName,MethodName,ErrorMessage]),Values);
  BV_ERROR_TYPE_MENE: raise EBVMemoryNotEditable.CreateFmt(Format('%s.%s: %s',[Self.ClassName,MethodName,ErrorMessage]),Values);
else
  raise EBVException.CreateFmt(Format('%s.%s: %s',[Self.ClassName,MethodName,ErrorMessage]),Values);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBitVector.RaiseError(const MethodName, ErrorMessage: String; ErrorType: Integer = -1);
begin
RaiseError(MethodName,ErrorMessage,[],ErrorType);
end;

//------------------------------------------------------------------------------

Function TBitVector.CheckIndexAndRaise(Index: Integer; const MethodName: String = 'CheckIndex'): Boolean;
begin
Result := CheckIndex(Index);
If not Result then
  RaiseError(MethodName,'Index (%d) out of bounds.',[Index],BV_ERROR_TYPE_IOOB);
end;

//------------------------------------------------------------------------------

Function TBitVector.CheckMemoryEditable(const MethodName: String = 'MemoryEditable'; RaiseException: Boolean = True): Boolean;
begin
Result := fOwnsMemory and not fStatic;
If RaiseException then
  begin
    If fStatic then
      RaiseError(MethodName,'Method not allowed for a static vector.',BV_ERROR_TYPE_MENE);
    If not fOwnsMemory then
      RaiseError(MethodName,'Method not allowed for not owned memory.',BV_ERROR_TYPE_MENE);
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.ShiftDown(Idx1,Idx2: Integer);
var
  Carry:  Boolean;
  i:      Integer;
  Buffer: UInt16;  
begin
If Idx2 > Idx1 then
  begin
    If (Idx1 shr 3) = (Idx2 shr 3)  then
      begin
        // shift is done inside of one byte
        SetBitsValue(GetBytePtrBitIdx(Idx1)^,GetBytePtrBitIdx(Idx1)^ shr 1,Idx1 and 7,Idx2 and 7);
      end
    else
      begin
        // shift is done across at least one byte boundary
        // shift last byte and preserve shifted-out bit
        Carry := GetBit_LL(Idx2 and not 7); // bit 0 of last byte
        SetBitsValue(GetBytePtrBitIdx(Idx2)^,GetBytePtrBitIdx(Idx2)^ shr 1,0,Idx2 and 7);
        // shift whole bytes
        For i := Pred(Idx2 shr 3) downto Succ(Idx1 shr 3) do
          begin
            Buffer := GetBytePtrByteIdx(i)^;
            BitSetTo(Buffer,8,Carry);
            Carry := (Buffer and 1) <> 0;
            GetBytePtrByteIdx(i)^ := Byte(Buffer shr 1);
          end;
        // shift first byte and store carry
        SetBitsValue(GetBytePtrBitIdx(Idx1)^,GetBytePtrBitIdx(Idx1)^ shr 1,Idx1 and 7,7);
        SetBit_LL(Idx1 or 7,Carry);
      end;
  end
else RaiseError('ShiftDown','First index (%d) must be smaller or equal to the second index (%d).',[Idx1,Idx2]);
end;

//------------------------------------------------------------------------------

procedure TBitVector.ShiftUp(Idx1,Idx2: Integer);
var
  Carry:  Boolean;
  i:      Integer;
  Buffer: UInt16;
begin
If Idx2 > Idx1 then
  begin
    If (Idx1 shr 3) = (Idx2 shr 3)  then
      begin
        // shift is done inside of one byte
        SetBitsValue(GetBytePtrBitIdx(Idx1)^,Byte(GetBytePtrBitIdx(Idx1)^ shl 1),Idx1 and 7,Idx2 and 7);
      end
    else
      begin
        // shift is done across at least one byte boundary
        // shift first byte and preserve shifted-out bit
        Carry := GetBit_LL(Idx1 or 7);
        SetBitsValue(GetBytePtrBitIdx(Idx1)^,Byte(GetBytePtrBitIdx(Idx1)^ shl 1),Idx1 and 7,7);
        // shift whole bytes
        For i := Succ(Idx1 shr 3) to Pred(Idx2 shr 3) do
          begin
            Buffer := UInt16(GetBytePtrByteIdx(i)^ shl 1);
            BitSetTo(Buffer,0,Carry);
            Carry := (Buffer and $100) <> 0;
            GetBytePtrByteIdx(i)^ := Byte(Buffer);
          end;
        // shift last byte and store carry
        SetBitsValue(GetBytePtrBitIdx(Idx2)^,Byte(GetBytePtrBitIdx(Idx2)^ shl 1),0,Idx2 and 7);
        SetBit_LL(Idx2 and not 7,Carry);
      end;
  end
else RaiseError('ShiftDown','First index (%d) must be smaller or equal to the second index (%d).',[Idx1,Idx2]);
end;

//------------------------------------------------------------------------------

procedure TBitVector.ScanForPopCount;
var
  i:        Integer;
begin
fPopCount := 0;
If fCount > 0 then
  begin
    // full bytes...
    For i := 0 to Pred(fCount shr 3) do
      Inc(fPopCount,BitOps.PopCount(GetBytePtrByteIdx(i)^));
    // partial byte...
    If (fCount and 7) > 0 then
      Inc(fPopCount,BitOps.PopCount(Byte(GetBytePtrBitIdx(fCount)^ and ($FF shr (8 - (fCount and 7))))));
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Combine(Memory: Pointer; Count: Integer; Op: Integer);
var
  i:  Integer;

  Function CombineBytes(A,B: Byte): Byte;
  begin
    case Op of
      BV_COMBINE_OPERATOR_AND:  Result := A and B;
      BV_COMBINE_OPERATOR_XOR:  Result := A xor B;
    else
     {BV_COMBINE_OPERATOR_OR}
      Result := A or B;
    end;
  end;

  Function CombineBool(A,B: Boolean): Boolean;
  begin
    case Op of
      BV_COMBINE_OPERATOR_AND:  Result := A and B;
      BV_COMBINE_OPERATOR_XOR:  Result := A xor B;
    else
     {BV_COMBINE_OPERATOR_OR}
      Result := A or B;
    end;
  end;
  
begin
If CheckMemoryEditable('Combine') and (Count > 0) then
  begin
    BeginChanging;
    try
      If Count > fCount then
        begin
          i := fCount;
          Self.Count := Count;
          If Op = BV_COMBINE_OPERATOR_AND then
            Fill(i,Pred(fCount),True) // set new bits (so when combined using AND they will take value from Memory)
          else
            Fill(i,HighIndex,False);  // clear new bits
        end;
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      // whole bytes
      For i := 0 to Pred(Count shr 3) do
        GetBytePtrByteIdx(i)^ := CombineBytes(GetBytePtrByteIdx(i)^,PByte(PtrUInt(Memory) + PtrUInt(i))^);
      // partial bytes if any
      If (Count and 7) <> 0 then
        For i := (Count and not 7) to Pred(Count) do
          SetBit_LL(i,CombineBool(GetBit_LL(i),((PByte(PtrUInt(Memory) + PtrUInt(Count shr 3))^ shr (i and 7)) and 1 <> 0)));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
      ScanForPopCount;
      DoChange;
    finally
      EndChanging;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Initialize;
begin
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
fMemSize := Ceil(Count / 8);
fMemory := Memory;
fCount := Count;
ScanForPopCount;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TBitVector.Create(InitialCount: Integer = 0; InitialValue: Boolean = False);
begin
inherited Create;
Initialize;
fOwnsMemory := True;
Capacity := InitialCount; // sets fMemSize and fMemory
fCount := InitialCount;
Fill(InitialValue);
end;

//------------------------------------------------------------------------------

destructor TBitVector.Destroy;
begin
If fOwnsMemory then
  FreeMem(fMemory,fMemSize);
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
Result := fCount - 1;
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
If CheckMemoryEditable('Add') then
  begin
    Grow;
    Inc(fCount);
    SetBit_LL(HighIndex,Value);
    If Value then
      Inc(fPopCount);
    Result := HighIndex;
    DoChange;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Insert(Index: Integer; Value: Boolean);
begin
If CheckMemoryEditable('Insert') then
  begin
    If Index < fCount then
      begin
        If CheckIndexAndRaise(Index,'Insert') then
          begin
            Grow;
            Inc(fCount);
            ShiftUp(Index,HighIndex);
            SetBit_LL(Index,Value);
            If Value then
              Inc(fPopCount);
            DoChange;
          end;
      end
    else Add(Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Exchange(Index1, Index2: Integer);
begin
If Index1 <> Index2 then
  If CheckIndexAndRaise(Index1,'Exchange') and CheckIndexAndRaise(Index2,'Exchange') then
    begin
      SetBit_LL(Index2,SetBit_LL(Index1,GetBit_LL(Index2)));
      DoChange;
    end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Move(SrcIdx, DstIdx: Integer);
var
  Value:  Boolean;
begin
If SrcIdx <> DstIdx then
  If CheckIndexAndRaise(SrcIdx,'Move') and CheckIndexAndRaise(DstIdx,'Move') then
    begin
      Value := GetBit_LL(SrcIdx);
      If SrcIdx < DstIdx then
        ShiftDown(SrcIdx,DstIdx)
      else
        ShiftUp(DstIdx,SrcIdx);
      SetBit_LL(DstIdx,Value);
      DoChange;
    end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Delete(Index: Integer);
begin
If CheckMemoryEditable('Delete') then
  If CheckIndexAndRaise(Index,'Delete') then
    begin
      If GetBit_LL(Index) then
        Dec(fPopCount);
      If Index < HighIndex then
        ShiftDown(Index,HighIndex);
      Dec(fCount);
      Shrink;
      DoChange;
    end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Fill(FromIdx, ToIdx: Integer; Value: Boolean);
var
  i:  Integer;
begin
If (FromIdx <= ToIdx) and (fCount > 0) then
  If CheckIndexAndRaise(FromIdx,'Fill') and CheckIndexAndRaise(ToIdx,'Fill') then
    begin
      If FromIdx <> ToIdx then
        begin
          // partial first byte or within one byte (also partial)
          If ((FromIdx and 7) <> 0) or ((ToIdx - FromIdx) < 7) then
            SetBitsValue(GetBytePtrBitIdx(FromIdx)^,$FF * BooleanOrd(Value),FromIdx and 7,Min(ToIdx - (FromIdx and not 7),7));
          // full bytes
          For i := Ceil(FromIdx / 8) to Pred(Succ(ToIdx) shr 3) do
            GetBytePtrByteIdx(i)^ := $FF * BooleanOrd(Value);
          // partial last byte and not in the first byte
          If ((ToIdx and 7) < 7) and ((ToIdx and not 7) > FromIdx) then
            SetBitsValue(GetBytePtrBitIdx(ToIdx)^,$FF * BooleanOrd(Value),0,ToIdx and 7);
          ScanForPopCount;
          DoChange;
        end
      else SetBit(FromIdx,Value);
    end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.Fill(Value: Boolean);
var
  i:  Integer;
begin
If fCount > 0 then
  begin
    For i := 0 to (Pred(fCount) shr 3) do
      GetBytePtrByteIdx(i)^ := $FF * BooleanOrd(Value);
    fPopCount := fCount * BooleanOrd(Value);
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Complement(FromIdx, ToIdx: Integer);
var
  i:  Integer;
begin
If (FromIdx <= ToIdx) and (fCount > 0) then
  If CheckIndexAndRaise(FromIdx,'Complement') and CheckIndexAndRaise(ToIdx,'Complement') then
    begin
      If FromIdx <> ToIdx then
        begin
          If ((FromIdx and 7) <> 0) or ((ToIdx - FromIdx) < 7) then
            SetBitsValue(GetBytePtrBitIdx(FromIdx)^,not GetBytePtrBitIdx(FromIdx)^,FromIdx and 7,Min(ToIdx - (FromIdx and not 7),7));
          For i := Ceil(FromIdx / 8) to Pred(Succ(ToIdx) shr 3) do
            GetBytePtrByteIdx(i)^ := not GetBytePtrByteIdx(i)^;
          If ((ToIdx and 7) < 7) and ((ToIdx and not 7) > FromIdx) then
            SetBitsValue(GetBytePtrBitIdx(ToIdx)^,not GetBytePtrBitIdx(ToIdx)^,0,ToIdx and 7);
          ScanForPopCount;
          DoChange;
        end
      else SetBit(FromIdx,not GetBit_LL(FromIdx));
    end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.Complement;
var
  i:  Integer;
begin
If fCount > 0 then
  begin
    For i := 0 to (Pred(fCount) shr 3) do
      GetBytePtrByteIdx(i)^ := not GetBytePtrByteIdx(i)^;
    fPopCount := fCount - fPopCount;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Clear;
begin
If CheckMemoryEditable('Clear') then
  begin
    fCount := 0;
    fPopCount := 0;
    Shrink;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Reverse;
var
  i:  Integer;
begin
If fCount > 1 then
  begin
    For i := 0 to Pred(fCount shr 1) do
      SetBit_LL(i,SetBit_LL(Pred(fCount) - i,GetBit_LL(i)));
    DoChange;  
  end;
end;

//------------------------------------------------------------------------------

Function TBitVector.IsEmpty: Boolean;
begin
Result := (fCount > 0) and (fPopCount = 0);
end;

//------------------------------------------------------------------------------

Function TBitVector.IsFull: Boolean;
begin
Result := (fCount > 0) and (fPopCount >= fCount);
end;

//------------------------------------------------------------------------------

Function TBitVector.FirstSet: Integer;
var
  i:        Integer;
  WorkByte: Byte;
begin
If fCount > 0 then
  begin
    // whole bytes
    For i := 0 to Pred(fCount shr 3) do
      begin
        WorkByte := GetBytePtrByteIdx(i)^;
        If WorkByte <> 0 then
          begin
            Result := (i * 8) + BSF(WorkByte);
            Exit;
          end;
      end;
    // last partial byte if any
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
  i:        Integer;
  WorkByte: Byte;
begin
If fCount > 0 then
  begin
    For i := 0 to Pred(fCount shr 3) do
      begin
        WorkByte := GetBytePtrByteIdx(i)^;
        If WorkByte <> $FF then
          begin
            Result := (i * 8) + BSF(not WorkByte);
            Exit;
          end;
      end;
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
  i:        Integer;
  WorkByte: Byte;
begin
If fCount > 0 then
  begin
    For i := Pred(fCount) downto (fCount and not 7) do
      If GetBit_LL(i) then
        begin
          Result := i;
          Exit;
        end;
    For i := Pred(fCount shr 3) downto 0 do
      begin
        WorkByte := GetBytePtrByteIdx(i)^;
        If WorkByte <> 0 then
          begin
            Result := (i * 8) + BSR(WorkByte);
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
  i:        Integer;
  WorkByte: Byte;
begin
If fCount > 0 then
  begin
    For i := Pred(fCount) downto (fCount and not 7) do
      If not GetBit_LL(i) then
        begin
          Result := i;
          Exit;
        end;
    For i := Pred(fCount shr 3) downto 0 do
      begin
        WorkByte := GetBytePtrByteIdx(i)^;
        If WorkByte <> $FF then
          begin
            Result := (i * 8) + BSR(not WorkByte);
            Exit;
          end;
      end;
    Result := -1;
  end
else Result := -1;
end;

//------------------------------------------------------------------------------

procedure TBitVector.Append(Memory: Pointer; Count: Integer);
var
  Shift:      Integer;
  i:          Integer;
  ByteBuff:   PByte;
begin
If CheckMemoryEditable('Append') and (Count > 0) then
  begin
    If (fCount and 7) = 0 then
      begin
        // currently contains only whole bytes
        Capacity := fCount + Count;
        System.Move(Memory^,GetBytePtrByteIdx(fCount shr 3)^,Ceil(Count / 8));
      end
    else
      begin
        // last byte is partial
        Capacity := Succ(fCount or 7) + Count;
        System.Move(Memory^,GetBytePtrByteIdx(Succ(fCount shr 3))^,Ceil(Count / 8));
        Shift := 8 - (fCount and 7);
        For i := (fCount shr 3) to Pred((fCount shr 3) + Ceil(Count / 8)) do
          begin
            ByteBuff := GetBytePtrByteIdx(i + 1);
            SetBitsValue(GetBytePtrByteIdx(i)^,Byte(ByteBuff^ shl (8 - Shift)),8 - Shift,7);
            ByteBuff^ := ByteBuff^ shr Shift;
          end;
      end;
    Inc(fCount,Count);
    ScanForPopCount;
    DoChange;
  end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.Append(Vector: TBitVector);
begin
Append(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.Assign(Memory: Pointer; Count: Integer);
begin
If CheckMemoryEditable('Assign') then
  begin
    If Count > Capacity then
      Capacity := Count;
    System.Move(Memory^,fMemory^,Ceil(Count / 8));
    fCount := Count;
    ScanForPopCount;
    DoChange;
  end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.Assign(Vector: TBitVector);
begin
Assign(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.CombineOR(Memory: Pointer; Count: Integer);
begin
Combine(Memory,Count,BV_COMBINE_OPERATOR_OR);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.CombineOR(Vector: TBitVector);
begin
CombineOR(Vector.Memory,Vector.Count);
end;
 
//------------------------------------------------------------------------------

procedure TBitVector.CombineAND(Memory: Pointer; Count: Integer);
begin
Combine(Memory,Count,BV_COMBINE_OPERATOR_AND);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.CombineAND(Vector: TBitVector);
begin
CombineAND(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

procedure TBitVector.CombineXOR(Memory: Pointer; Count: Integer);
begin
Combine(Memory,Count,BV_COMBINE_OPERATOR_XOR);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TBitVector.CombineXOR(Vector: TBitVector);
begin
CombineXOR(Vector.Memory,Vector.Count);
end;

//------------------------------------------------------------------------------

Function TBitVector.IsEqual(Vector: TBitVector): Boolean;
var
  i:  Integer;
begin
Result := False;
If (fCount = Vector.Count) and (fPopCount = Vector.PopCount) then
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    // compare whole bytes
    For i := 0 to Pred(fCount shr 3) do
      If GetBytePtrByteIdx(i)^ <> PByte(PtrUInt(Vector.Memory) + PtrUInt(i))^ then Exit;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    // compare last partial byte if any
    If (fCount and 7) <> 0 then
      For i := (fCount and not 7) to Pred(fCount) do
        If GetBit_LL(i) <> Vector[i] then Exit;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.WriteToStream(Stream: TStream);
var
  TempByte: Byte;
begin
// write whole bytes
Stream.WriteBuffer(fMemory^,fCount shr 3);
// prepare and write last partial byte, if any
If (fCount and 7) <> 0 then
  begin
    TempByte := SetBits(0,GetBytePtrByteIdx(fCount shr 3)^,0,Pred(fCount and 7));
    Stream.WriteBuffer(TempByte,1);
  end;
end;

//------------------------------------------------------------------------------

procedure TBitVector.ReadFromStream(Stream: TStream);
begin
If CheckMemoryEditable('LoadFromStream') then
  begin
    BeginChanging;  // not needed, but meh
    try
      // read only data
      Stream.ReadBuffer(fMemory^,Ceil(fCount / 8));
      ScanForPopCount;
      DoChange;
    finally
      EndChanging;
    end;
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
If CheckMemoryEditable('LoadFromStream') then
  begin
    BeginChanging;
    try
      // read and set number of bits
      Count := Integer(Stream_ReadInt32(Stream));
      // read data
      Stream.ReadBuffer(fMemory^,Ceil(fCount / 8));
      ScanForPopCount;
      DoChange;
    finally
      EndChanging;
    end;
  end;
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
  RaiseError('Create','Count must be divisible by 32.');
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TBitVectorStatic32.Create(InitialCount: Integer = 0; InitialValue: Boolean = False);
begin
If (Count and 31) = 0 then
  inherited Create(InitialCount,InitialValue)
else
  RaiseError('Create','Count must be divisible by 32.');
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.FirstSet: Integer;
var
  i:      Integer;
  Buffer: UInt32;
begin
Result := -1;
If fCount > 0 then
  For i := 0 to Pred(fCount shr 5) do
    begin
    {$IFDEF ENDIAN_BIG}
      Buffer := EndianSwap(PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^);
    {$ELSE}
      Buffer := PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^;
    {$ENDIF}
      If Buffer <> 0 then
        begin
          Result := (i * 32) + BSF(Buffer);
          Break;
        end;
    end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.FirstClean: Integer;
var
  i:      Integer;
  Buffer: UInt32;
begin
Result := -1;
If fCount > 0 then
  For i := 0 to Pred(fCount shr 5) do
    begin
    {$IFDEF ENDIAN_BIG}
      Buffer := EndianSwap(PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^);
    {$ELSE}
      Buffer := PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^;
    {$ENDIF}
      If Buffer <> $FFFFFFFF then
        begin
          Result := (i * 32) + BSF(not Buffer);
          Break;
        end;
    end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.LastSet: Integer;
var
  i:      Integer;
  Buffer: UInt32;
begin
Result := -1;
If fCount > 0 then
  For i := Pred(fCount shr 5) downto 0 do
    begin
    {$IFDEF ENDIAN_BIG}
      Buffer := EndianSwap(PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^);
    {$ELSE}
      Buffer := PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^;
    {$ENDIF}
      If Buffer <> 0 then
        begin
          Result := (i * 32) + BSR(Buffer);
          Break;
        end;
    end;
end;

//------------------------------------------------------------------------------

Function TBitVectorStatic32.LastClean: Integer;
var
  i:      Integer;
  Buffer: UInt32;
begin
Result := -1;
If fCount > 0 then
  For i := Pred(fCount shr 5) downto 0 do
    begin
    {$IFDEF ENDIAN_BIG}
      Buffer := EndianSwap(PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^);
    {$ELSE}
      Buffer := PUInt32(GetBytePtrByteIdx(i * SizeOf(UInt32)))^;
    {$ENDIF}
      If Buffer <> $FFFFFFFF then
        begin
          Result := (i * 32) + BSR(not Buffer);
          Break;
        end;
    end;
end;

end.
