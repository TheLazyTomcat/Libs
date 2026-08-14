{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  PrimitiveHashMap

    Provides TPrimitiveHashMap class that can be used as a base for very
    simple hash map.

    Almost no documentation is provided as the code and its use should be
    self-explanatory. But if any demand arises, I might add some.

  Version 1.0 (2026-08-15)

  Last change 2026-08-15

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.PrimitiveHashMap

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol PrimitiveHashMap_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    ListUtils   - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit PrimitiveHashMap;
{
  PrimitiveHashMap_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  PrimitiveHashMap_UseAuxExceptions to achieve this.
}
{$IF Defined(PrimitiveHashMap_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
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
  SysUtils,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EPHMException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EPHMInvalidValue     = class(EPHMException);
  EPHMInvalidState     = class(EPHMException);
  EPHMOutOfResources   = class(EPHMException);
  EPHMDuplicateEntry   = class(EPHMException);
  EPHMIndexOutOfBounds = class(EPHMException);

{===============================================================================
--------------------------------------------------------------------------------
                                TPrimitiveHashMap
--------------------------------------------------------------------------------
===============================================================================}
type
  TEntryPayload = record case Integer of
    0: (IntValue: PtrInt);
    1: (FltValue: PtrFloat);
    2: (PtrValue: Pointer);
    3: (ObjValue: TObject);
  end;

{
  Payload

  Use these functions as sort-of inline constructors of payload record when
  passing simple type, for example:

    HashMap.Add(Payload(IntegerValue));

  The same could be achieved by overloading methods accepting the payload
  (providing overloads accepting simple types), but this is cleaner.
}
Function Payload(const Value: PtrInt): TEntryPayload; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Payload(const Value: PtrFloat): TEntryPayload; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Payload(const Value: Pointer): TEntryPayload; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Payload(const Value: TObject): TEntryPayload; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
type
  THashMapEntry = record
    KeyHash:  UInt32;
  {$IF SizeOf(TEntryPayload) = 8}
    Reserved: UInt32; // explicit padding
  {$IFEND}
    Payload:  TEntryPayload;
  end;
  PHashMapEntry = ^THashMapEntry;

  THashMapBucket = record
    Taken:      Boolean;
    Entry:      THashMapEntry;
    Collision:  record
      Entries:    array of THashMapEntry;
      Count:      Integer;
    end;
  end;
  PHashMapBucket = ^THashMapBucket;

  THashMapArray = array of THashMapBucket;

//------------------------------------------------------------------------------  
type
  TGetHashEvent    = Function(Sender: TObject; const Payload: TEntryPayload): UInt32 of object;
  TGetHashCallback = Function(Sender: TObject; const Payload: TEntryPayload): UInt32;

  TSameKeysEvent    = Function(Sender: TObject; const A,B: TEntryPayload): Boolean of object;
  TSameKeysCallback = Function(Sender: TObject; const A,B: TEntryPayload): Boolean;

  TDuplicatesEvent    = Function(Sender: TObject; const PayloadCurrent,PayloadNew: TEntryPayload): Boolean of object;
  TDuplicatesCallback = Function(Sender: TObject; const PayloadCurrent,PayloadNew: TEntryPayload): Boolean;

{===============================================================================
    TPrimitiveHashMap - class declaration
===============================================================================}
{
  TPrimitiveHashMap

  This class implements only partial functionality of hash map, and is
  inteded only as a base for specialized implementation.

  It does not store nor expects keys - it is assumed the key or equivalent
  data is part of payload (is stored withing it), or can be inferred from it.
  Therefore it only accepts payload as argument when searching for, adding
  or removing an entry.

  As such, it also does not contain any key hashing or key comparison. This
  functionality must be provided by specialized implementation, either by
  assigning handlers to OnGetHash* and OnSameKeys* events/callbacks, or by
  overriding DoGetHash and DoSameKeys protected methods (note that current
  implementation of these methods is responsible for calling the mentioned
  events - so, by not calling inherited code, you will disable those events).

  You can also override method DoDuplicates to handle duplicate entries if
  you do not want to handle it using corresponding event or callback.

  Colliding entries (entries hashed to the same bucket as existing entry but
  not recognized as duplicates) are managed using separate chaining - they
  are stored in a small dynamic array managed within bucket with wich they
  collide.
}
type
  TPrimitiveHashMap = class(TCustomObject)
  protected
    fBuckets:             THashMapArray;
    fTakenBucketCount:    Integer;
    fEntryCount:          Integer;
    fIndexMask:           UInt32;
    fGetHashEvent:        TGetHashEvent;
    fGetHashCallback:     TGetHashCallback;
    fSameKeysEvent:       TSameKeysEvent;
    fSameKeysCallback:    TSameKeysCallback;
    fDuplicatesEvent:     TDuplicatesEvent;
    fDuplicatesCallback:  TDuplicatesCallback;
    // getters, setters
    Function GetBucket(Index: Integer): THashMapBucket; virtual;
    Function GetBucketCount: Integer; virtual;
    procedure SetBucketCount(Value: Integer); virtual;
    Function GetLoadFactor: Double; virtual;
    // init, final
    procedure Initialize(InitialCapacity: Integer); virtual;
    procedure Finalize; virtual;
    // event/callback callers
    Function DoGetHash(const Payload: TEntryPayload): UInt32; virtual;
    Function DoSameKeys(const A,B: TEntryPayload): Boolean; virtual;
    Function DoDuplicates(const PayloadCurrent,PayloadNew: TEntryPayload): Boolean; virtual;
    // helper methods
    Function HashToIndex(const Payload: TEntryPayload; out Hash: UInt32): Integer; virtual;
    procedure EntryInit(var Entry: THashMapEntry);
    Function EntryMatch(const Entry: THashMapEntry; KeyHash: UInt32; const Payload: TEntryPayload): Boolean; virtual;
    Function EntryDuplicate(var Entry: THashMapEntry; KeyHash: UInt32; const Payload: TEntryPayload): Boolean; virtual;
    procedure BucketInit(var Bucket: THashMapBucket); virtual;
    procedure BucketTake(var Bucket: THashMapBucket; KeyHash: UInt32; const Payload: TEntryPayload); virtual;
    procedure BucketGrow(var Bucket: THashMapBucket); virtual;
    procedure MapRehash(BucketCount: Integer); virtual;
  public
    constructor Create(InitialCapacity: Integer = 0);
    destructor Destroy; override;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    Function Find(const Payload: TEntryPayload; out Entry: THashMapEntry): Boolean; virtual;
    Function Add(const Payload: TEntryPayload): PHashMapEntry; virtual;
    Function Remove(const Payload: TEntryPayload): Boolean; virtual;
    procedure Clear; virtual;
    property Buckets[Index: Integer]: THashMapBucket read GetBucket; default;
    property BucketCount: Integer read GetBucketCount write SetBucketCount;
    property TakenBucketCount: Integer read fTakenBucketCount;
    property EntryCount: Integer read fEntryCount;
  {
    LoadFactor

    Indicates utilization of the hash map, and is calculated as number of
    stored entries divided by number of buckets. If no bucket is allocated,
    it reads as 0.0.

      NOTE - separate chaining is used to handle collisions, meaning
             LoadFactor can have any value from zero up, it is not
             limited to 1.0.
  }
    property LoadFactor: Double read GetLoadFactor;
    property OnGetHash: TGetHashEvent read fGetHashEvent write fGetHashEvent;
    property OnGetHashEvent: TGetHashEvent read fGetHashEvent write fGetHashEvent;
    property OnGetHashCallback: TGetHashCallback read fGetHashCallback write fGetHashCallback;
    property OnSameKeys: TSameKeysEvent read fSameKeysEvent write fSameKeysEvent;
    property OnSameKeysEvent: TSameKeysEvent read fSameKeysEvent write fSameKeysEvent;
    property OnSameKeysCallback: TSameKeysCallback read fSameKeysCallback write fSameKeysCallback;
    property OnDuplicates: TDuplicatesEvent read fDuplicatesEvent write fDuplicatesEvent;
    property OnDuplicatesEvent: TDuplicatesEvent read fDuplicatesEvent write fDuplicatesEvent;
    property OnDuplicatesCallback: TDuplicatesCallback read fDuplicatesCallback write fDuplicatesCallback;
  end;

implementation

{===============================================================================
--------------------------------------------------------------------------------
                                TPrimitiveHashMap                                
--------------------------------------------------------------------------------
===============================================================================}
const
  PHM_SUBARRAY_INITCOUNT = 8;

{===============================================================================
    TPrimitiveHashMap - public helper functions
===============================================================================}

Function Payload(const Value: PtrInt): TEntryPayload;
begin
Result.IntValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Payload(const Value: PtrFloat): TEntryPayload;
begin
Result.FltValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Payload(const Value: Pointer): TEntryPayload;
begin
Result.PtrValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Payload(const Value: TObject): TEntryPayload;
begin
Result.ObjValue := Value;
end;

{===============================================================================
    TPrimitiveHashMap - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TPrimitiveHashMap - protected methods
-------------------------------------------------------------------------------}

Function TPrimitiveHashMap.GetBucket(Index: Integer): THashMapBucket;
begin
If CheckIndex(Index) then
  Result := fBuckets[Index]
else
  raise EPHMIndexOutOfBounds.CreateFmt('TPrimitiveHashMap.GetBucket: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.GetBucketCount: Integer;
begin
Result := Length(fBuckets);
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.SetBucketCount(Value: Integer);
var
  i:      Integer;
  OldCap: Integer;
begin
If Value <> Length(fBuckets) then
  begin
    // sanity checks
    If (Value < 0) or ((fEntryCount > 0) and (Value <= 0)) then
      raise EPHMInvalidValue.CreateFmt('TPrimitiveHashMap.SetCapacity: Invalid new capacity (%d).',[Value]);
  {
    If allocated capacity is an integral power of 2, set index mask to speed-up
    index calculations. This must be done here in case InternalRehash is called
    (it uses index mask).
  }
    fIndexMask := 0;
    If Value > 1 then
      If (Value and Pred(Value)) = 0 then
        fIndexMask := Pred(Value);
    // and now the resizing
    If Length(fBuckets) <= 0 then
      begin
      {
        Zero-length main array, setting to non-zero length - enlarge and init
        all buckets.
      }
        SetLength(fBuckets,Value);
        For i := Low(fBuckets) to High(fBuckets) do
          BucketInit(fBuckets[i]);
      end
    else If Value = 0 then
      begin
      {
        Non-zero length main array, setting to zero-length - just set main
        array to nil (no need to explicitly clear buckets).
      }
        fBuckets := nil;  
      end
    else If fEntryCount <= 0 then
      begin
      {
        Non-zero length main array, but without entries - resize and init
        newly added buckets (if enlarging).
      }
        OldCap := Length(fBuckets);
        SetLength(fBuckets,Value);
        // if new capacity is smaller than old, then following cycle is not executed
        For i := OldCap to High(fBuckets) do
          BucketInit(fBuckets[i]);
      end
    // non-empty map with entries - we need to rehash the whole map
    else MapRehash(Value);
  end;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.GetLoadFactor: Double;
begin
If Length(fBuckets) > 0 then
  Result := fEntryCount / Length(fBuckets)
else
  Result := 0;
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.Initialize(InitialCapacity: Integer);
begin
fBuckets := nil;
fTakenBucketCount := 0;
fEntryCount := 0;
fIndexMask := 0;
fGetHashEvent := nil;
fGetHashCallback := nil;
fSameKeysEvent := nil;
fSameKeysCallback := nil;
fDuplicatesEvent := nil;
fDuplicatesCallback := nil;
If InitialCapacity > 0 then
  SetBucketCount(InitialCapacity);
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.Finalize;
begin
// following is technically not needed, but let's be pedantic...
fBuckets := nil;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.DoGetHash(const Payload: TEntryPayload): UInt32;
begin
If Assigned(fGetHashEvent) then
  Result := fGetHashEvent(Self,Payload)
else If Assigned(fGetHashCallback) then
  Result := fGetHashCallback(Self,Payload)
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.DoSameKeys(const A,B: TEntryPayload): Boolean;
begin
If Assigned(fSameKeysEvent) then
  Result := fSameKeysEvent(Self,A,B)
else If Assigned(fSameKeysCallback) then
  Result := fSameKeysCallback(Self,A,B)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.DoDuplicates(const PayloadCurrent,PayloadNew: TEntryPayload): Boolean;
begin
If Assigned(fDuplicatesEvent) then
  Result := fDuplicatesEvent(Self,PayloadCurrent,PayloadNew)
else If Assigned(fDuplicatesCallback) then
  Result := fDuplicatesCallback(Self,PayloadCurrent,PayloadNew)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.HashToIndex(const Payload: TEntryPayload; out Hash: UInt32): Integer;
begin
Hash := DoGetHash(Payload);
If fIndexMask <> 0 then
  Result := Integer(Hash and fIndexMask)
else
  Result := Integer(Hash mod UInt32(Length(fBuckets)));
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.EntryInit(var Entry: THashMapEntry);
begin
FillChar(Entry,SizeOf(THashMapEntry),0);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.EntryMatch(const Entry: THashMapEntry; KeyHash: UInt32; const Payload: TEntryPayload): Boolean;
begin
If Entry.KeyHash = KeyHash then
  Result := DoSameKeys(Entry.Payload,Payload)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.EntryDuplicate(var Entry: THashMapEntry; KeyHash: UInt32; const Payload: TEntryPayload): Boolean;
begin
If EntryMatch(Entry,KeyHash,Payload) then
  begin
    If DoDuplicates(Entry.Payload,Payload) then
      Entry.Payload := Payload;
    Result := True;  
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.BucketInit(var Bucket: THashMapBucket);
begin
Bucket.Collision.Entries := nil;
FillChar(Bucket,SizeOf(THashMapBucket),0);
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.BucketTake(var Bucket: THashMapBucket; KeyHash: UInt32; const Payload: TEntryPayload);
begin
Bucket.Taken := True;
Bucket.Entry.KeyHash := KeyHash;
Bucket.Entry.Payload := Payload;
Bucket.Collision.Entries := nil;
Bucket.Collision.Count := 0;
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.BucketGrow(var Bucket: THashMapBucket);
begin
If Length(Bucket.Collision.Entries) <= Bucket.Collision.Count then
  If Length(Bucket.Collision.Entries) > 0 then
    begin
      If Length(Bucket.Collision.Entries) <= (High(Integer) div 2) then
        SetLength(Bucket.Collision.Entries,Length(Bucket.Collision.Entries) * 2)
      else If Length(Bucket.Collision.Entries) < High(Integer) then
        SetLength(Bucket.Collision.Entries,High(Integer))
      else
        raise EPHMOutOfResources.CreateFmt('TPrimitiveHashMap.BucketGrow: Separate chaining depleted (%d).',
          [Length(Bucket.Collision.Entries)]);
    end
  else SetLength(Bucket.Collision.Entries,PHM_SUBARRAY_INITCOUNT);
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.MapRehash(BucketCount: Integer);

  procedure CopyEntry(const Source: THashMapEntry);
  var
    BucketIndex:  Integer;
    BucketPtr:    PHashMapBucket;
  begin
    If fIndexMask <> 0 then
      BucketIndex := Integer(Source.KeyHash and fIndexMask)
    else
      BucketIndex := Integer(Source.KeyHash mod UInt32(Length(fBuckets)));
    BucketPtr := PHashMapBucket(Addr(fBuckets[BucketIndex]));
    If BucketPtr^.Taken then
      begin
        // collission (enlarge collission array if necessary)
        BucketGrow(BucketPtr^);
        BucketPtr^.Collision.Entries[BucketPtr^.Collision.Count] := Source;
        Inc(BucketPtr^.Collision.Count);
      end
    else BucketPtr^.Entry := Source;
    BucketPtr^.Taken := True;
  end;

var
  TempBuckets:  THashMapArray;
  BucketPtr:    PHashMapBucket;
  i,j:          Integer;
begin
TempBuckets := fBuckets;  // this only assigns pointer
// prevent copying of existing data, also decouples TempBuckets from fBuckets
fBuckets := nil;
SetLength(fBuckets,BucketCount);
// explicitly init all buckets
For i := Low(fBuckets) to High(fBuckets) do
  BucketInit(fBuckets[i]);
// traverse all existing entries and copy them one-by-one to their new locations
For i := Low(TempBuckets) to High(TempBuckets) do
  begin
    BucketPtr := PHashMapBucket(Addr(TempBuckets[i]));
    If BucketPtr^.Taken then
      begin
        // copy primary entry...
        CopyEntry(BucketPtr^.Entry);
        // ...and collissions if any are there
        For j := Low(BucketPtr^.Collision.Entries) to Pred(BucketPtr^.Collision.Count) do
          CopyEntry(BucketPtr^.Collision.Entries[j]);
      end;
  end;
TempBuckets := nil; 
end;

{-------------------------------------------------------------------------------
    TPrimitiveHashMap - public methods
-------------------------------------------------------------------------------}

constructor TPrimitiveHashMap.Create(InitialCapacity: Integer = 0);
begin
inherited Create;
Initialize(InitialCapacity);
end;

//------------------------------------------------------------------------------

destructor TPrimitiveHashMap.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.LowIndex: Integer;
begin
Result := Low(fBuckets);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.HighIndex: Integer;
begin
Result := High(fBuckets);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.Find(const Payload: TEntryPayload; out Entry: THashMapEntry): Boolean;
var
  KeyHash:      UInt32;
  BucketIndex:  Integer;
  BucketPtr:    PHashMapBucket;
  i:            Integer;
begin
Result := False;
If fEntryCount > 0 then
  begin
    BucketIndex := HashToIndex(Payload,KeyHash);
    BucketPtr := PHashMapBucket(Addr(fBuckets[BucketIndex]));
    If BucketPtr^.Taken then
      begin
        If EntryMatch(BucketPtr^.Entry,KeyHash,Payload) then
          begin
            Entry := BucketPtr^.Entry;
            Result := True;
            Exit;
          end;
        // primary entry does not match, try collisions
        For i := Low(BucketPtr^.Collision.Entries) to Pred(BucketPtr^.Collision.Count) do
          If EntryMatch(BucketPtr^.Collision.Entries[i],KeyHash,Payload) then
            begin
              Entry := BucketPtr^.Collision.Entries[i];
              Result := True;
              Exit;
            end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.Add(const Payload: TEntryPayload): PHashMapEntry;
var
  KeyHash:      UInt32;
  BucketIndex:  Integer;
  BucketPtr:    PHashMapBucket;
  i:            Integer;
begin
Result := nil;
If Length(fBuckets) > 0 then
  begin
    BucketIndex := HashToIndex(Payload,KeyHash);
    BucketPtr := PHashMapBucket(Addr(fBuckets[BucketIndex]));
    If BucketPtr^.Taken then
      begin
        If not EntryDuplicate(BucketPtr^.Entry,KeyHash,Payload) then
          begin
            // not a duplicate of primary entry, look at collisions
            For i := Low(BucketPtr^.Collision.Entries) to Pred(BucketPtr^.Collision.Count) do
              If EntryDuplicate(BucketPtr^.Collision.Entries[i],KeyHash,Payload) then
                begin
                  // duplicate of collision
                  Result := PHashMapEntry(Addr(BucketPtr^.Collision.Entries[i]));
                  Exit;
                end;
            // no duplicate in colliding entries, add as new collision
            BucketGrow(BucketPtr^);
            BucketPtr^.Collision.Entries[BucketPtr^.Collision.Count].KeyHash := KeyHash;
            BucketPtr^.Collision.Entries[BucketPtr^.Collision.Count].Payload := Payload;
            Result := PHashMapEntry(Addr(BucketPtr^.Collision.Entries[BucketPtr^.Collision.Count]));
            Inc(BucketPtr^.Collision.Count);
            Inc(fEntryCount);
          end
        // duplicate of existing primary entry
        else Result := PHashMapEntry(Addr(BucketPtr^.Entry));
      end
    else
      begin
        // bucket not taken, add as primary entry
        BucketTake(BucketPtr^,KeyHash,Payload);
        Inc(fTakenBucketCount);
        Inc(fEntryCount);
      end;
  end
else raise EPHMOutOfResources.CreateFmt('TPrimitiveHashMap.Add: No capacity for new entry (%d).',[Length(fBuckets)]);
end;

//------------------------------------------------------------------------------

Function TPrimitiveHashMap.Remove(const Payload: TEntryPayload): Boolean;
var
  KeyHash:      UInt32;
  BucketIndex:  Integer;
  BucketPtr:    PHashMapBucket;
  i,j:          Integer;
begin
Result := False;
If fEntryCount > 0 then
  begin
    BucketIndex := HashToIndex(Payload,KeyHash);
    BucketPtr := PHashMapBucket(Addr(fBuckets[BucketIndex]));
    If BucketPtr^.Taken then
      begin
        If EntryMatch(BucketPtr^.Entry,KeyHash,Payload) then
          begin
          {
            Primary entry matches, remove it - if it has colliding entries,
            take one and use it as primary.
          }
            If BucketPtr^.Collision.Count > 0 then
              begin
                BucketPtr^.Entry := BucketPtr^.Collision.Entries[Low(BucketPtr^.Collision.Entries)];
                For i := Succ(Low(BucketPtr^.Collision.Entries)) to Pred(BucketPtr^.Collision.Count) do
                  BucketPtr^.Collision.Entries[i - 1] := BucketPtr^.Collision.Entries[i];
                EntryInit(BucketPtr^.Collision.Entries[Pred(BucketPtr^.Collision.Count)]);
                Dec(BucketPtr^.Collision.Count);
              end
            else
              begin
                // no colliding entries, just remove the primary
                BucketInit(BucketPtr^);
                Dec(fTakenBucketCount);
              end;
            Dec(fEntryCount);  
          end
        else
          begin
            // primary entry does not match, look in collisions
            For i := Low(BucketPtr^.Collision.Entries) to Pred(BucketPtr^.Collision.Count) do
              If EntryMatch(BucketPtr^.Collision.Entries[i],KeyHash,Payload) then
                begin
                  For j := Succ(i) to Pred(BucketPtr^.Collision.Count) do
                    BucketPtr^.Collision.Entries[j - 1] := BucketPtr^.Collision.Entries[j];
                  EntryInit(BucketPtr^.Collision.Entries[Pred(BucketPtr^.Collision.Count)]);
                  Dec(BucketPtr^.Collision.Count);
                  Dec(fEntryCount);
                  Break{For i};
                end;
          end;
      end;   
  end;
end;

//------------------------------------------------------------------------------

procedure TPrimitiveHashMap.Clear;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  BucketInit(fBuckets[i]);
fTakenBucketCount := 0;
fEntryCount := 0;
end;

end.
