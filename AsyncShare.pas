{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Asynchronous data share

    Very simple library that provides few asynchronous (lock-free) mechanisms
    for sharing data between two or more threads or processes.

      WARNING - implementation details can change in future versions, so
                always make sure you use the same version of this library
                in all sharing endpoints.

  version 1.0.1 (2026-06-18)

  Last change 2026-06-18

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.AsyncShare

  Dependencies:
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps         - github.com/TheLazyTomcat/Lib.BitOps
    CRCLite        - github.com/TheLazyTomcat/Lib.CRCLite
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
    MD5            - github.com/TheLazyTomcat/Lib.MD5
    SHA2           - github.com/TheLazyTomcat/Lib.SHA2

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol AsyncShare_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit AsyncShare;
{
  AsyncShare_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and only this unit will be compiled in PurePascal mode.
}
{$IFDEF AsyncShare_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  AsyncShare_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  AsyncShare_UseAuxExceptions to achieve this.
}
{$IF Defined(BitOps_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}  
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EASException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EASIvalidValue = class(EASException);

{===============================================================================
--------------------------------------------------------------------------------
                              Asynchronous sharing
--------------------------------------------------------------------------------
===============================================================================}
{
  Asynchronous sharing

  Use this mechanism when you need to share data between two or more threads
  or, if using shared memory as storage, between processes where asynchronous
  (non-blocking) execution is more important than data integrity.

  Data are passed through pre-allocated memory storage (they are written to it
  and read from it). Thread that wants to provide shared data stores them to
  this storage using AsyncStore, thread or threads that want to read shared
  data do it by loading them from the storage using AsyncLoad.
  This storage must be allocated by the caller and must be large enough to
  accomodate the shared data along with their checksum (see further). Use
  function AsyncStorageSize to get how big the storage needs to be for given
  size of shared data and used checksum. How you allocate it is up to you.
  You can use global variable, shared memory, memory mapped file, etc. -
  whatever suits the desired sharing scheme.

    NOTE - both writing and reading thread(s) must use the same size and
           checksum type when writing and reading, otherwise the behavior
           is undefined.

  There is absolutely no interlocking in this mchanism. Data are written to
  the storage and read from it using normal (non-interlocked) instructions.
  But to provide for at least partial data protection, a checksum or hash of
  shared data is stored along with them. When reading thread loads the data,
  it also loads the stored checksum and compares it with checksum it calculates
  from the previously read data. If they match, then the data are assumed to
  be undamaged. If not, then AsyncLoad signals it by returning false and caller
  should decide what to do next (eg. repeat the reading or drop the data and
  doing something else).
  That being said, do not use this on shared data that are continuously being
  changed, as successful read here is pretty much a random occurence and such
  situation would mean wery small chance of that happening.

    WARNING - it should be clear from the description above that the read
              data might still be corrupted even when successfully loaded.
              It is highly unlikely that damaged data will match with stored
              checksum (which can also be damaged), but it cannot be
              discounted.

  This mechanism should only be used for uni-directional sharing (ie. one
  thread is writing, others are only reading). Bi-directional communication
  should be routed through two separate storages, each handling one direction.
  Also, there should be only one thread that writes to the storage, but
  multiple threads can read from it.
}
{===============================================================================
    Asynchronous sharing - declaration
===============================================================================}
{
  TASChecksumType

  Use this enumeration to select which type of checksum or hash will be used
  for data integrity check (see above).

  If you use csAuto, the actual type will be selected automatically depending
  on size of shared data. Currently, sizes below 256 bytes use csCRC32, from
  256 to 1023 use csCRC64, 1024 to 4095 use csMD5, 4096 to 16383 use csSHA256
  (variant of SHA-2) and larger use csSHA512 (also SHA-2). You can use function
  AsyncChecksumType to resolve which checksum type will be used for given size
  of shared data when csAuto is selected.
}
type
  TASChecksumType = (csAuto,csCRC32,csCRC64,csMD5,csSHA256,csSHA512);

//------------------------------------------------------------------------------
{
  AsyncChecksumType

  Returns checksum type that should be used for given size of shared data (this
  is only a suggestion, you can always override it and use a different one).

  This also corresponds to checksum type actually used when you select csAuto.
}
Function AsyncChecksumType(Size: TMemSize): TASChecksumType;

{
  AsyncStorageSize

  Returns size of storage (in bytes) needed for sharing of given amount of data
  (Size) while using selected checksum type. Use this function when allocating
  shared storage.
}
Function AsyncStorageSize(Size: TMemSize; ChecksumType: TASChecksumType = csAuto): TMemSize;

//------------------------------------------------------------------------------
{
  AsyncStore

  Stores Size-number of bytes from Source buffer to provided Storage memory
  space for use in asynchronous sharing - see description of asynchronous
  sharing for more details.

  Storage must be large enough to accomodate all provided data along with
  their checksum, of which type is selected by parameter ChecksumType.
  You can use function AsyncStorageSize to obtain minimum size for storage.

  If checksum type is set to csAuto, then the actual checksum type is selected
  automatically based on number of stored bytes (Size).
}
procedure AsyncStore(const Source; Size: TMemSize; out Storage; ChecksumType: TASChecksumType = csAuto);

{
  AsyncLoad

  Loads Size-number of bytes from shared memory Storage into provided buffer
  (Destination). Returns true if the read data match with checksum stored with
  them, false otherwise (meaning data, checksum, or both are corrupted) - see
  description of asynchronous sharing for more details.

  You must use the same size and checksum type as was used when storing the
  data, otherwise the resulting behavior is undefined.

    NOTE - the data are always read into the Destination buffer in full,
           even when this function returns false (corrupted data), replacing
           whatever was there before.
}
Function AsyncLoad(const Storage; out Destination; Size: TMemSize; ChecksumType: TASChecksumType = csAuto): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                 Guarded sharing
--------------------------------------------------------------------------------
===============================================================================}
{
  Guarded sharing

  Use this mechanism when you need guaranteed data integrity but still fully
  asynchronous non-interlocked execution is highly important.

  Externally, it works almost the same as asynchronous sharing (see there for
  more details), but there are four very important distinctions:

    There can be only one writing thread and one reading thread, no more.
    This limitation is not enforced, meaning it is your responsibility to
    strictly adhere to it. Failing to do so will corrupt this mechanism
    and its internal state beyond recovery.

    Storage memory space must be prepared before first use. If it is reused
    later then it must be prepared again. It is enough to clear it (zero it),
    or use function GuardedStoragePrepare for that purpose.

    Not only reading, but writing can also fail. It can happen when reader is
    in process of reading data (which should not take too much time, depending
    on amount of shared data) when you try to write. If this happens, simply
    wait for a moment and reapeat the writing.

    Unlike in asynchronous sharing, here read (loaded) data will be always
    complete and uncorrupted as long as you use the mechanim correctly (one
    writer, one reader).

  Internally, it uses two guard bytes (hence the name) in the memory storage
  (just behind the data) that are atomically written and read and memory fences
  to provide proper instruction ordering. There is not much else to it, you can
  consult the source code for more details.
}
{===============================================================================
    Guarded sharing - declaration
===============================================================================}
{
  GuardedStorageSize

  Returns minimum size of storage space (in bytes) needed for sharing of given
  amount of data (Size). Use this function when allocating shared storage.
}
Function GuardedStorageSize(Size: TMemSize): TMemSize;

{
  GuardedStoragePrepare

  Prepares given storage memory space for use in guarded sharing.

  Call it before you first use a newly allocated storage or when you plan to
  reuse old storage in new round of sharing.

    WARNING - size given here is not size of data to be shared, but full
              true size of memory space allocated for shared storage.
}
procedure GuardedStoragePrepare(var Storage; StorageSize: TMemSize);

//------------------------------------------------------------------------------
{
  GuardedStore

  Stores Size-number of bytes from Source buffer to provided Storage memory
  space for use in guarded sharing. Returns true if the data were successfully
  stored and are ready to be read, false otherwise, in which case you should
  try to write the data again a little later.

  Storage must be large enough to accomodate all provided data along with
  internal state used for data protection. Use function GuardedStorageSize
  to obtain minimum size for storage.
}
Function GuardedStore(const Source; Size: TMemSize; out Storage): Boolean;

{
  GuardedLoad

  Loads Size-number of bytes from shared memory Storage into Destination
  buffer. Returns true if the read went without problem, false otherwise.

  If the reading fails, it means the data cannot be read at the moment
  (nothing is copied into Destination), so you can try to read them again
  a moment later.

  You must use the same size as was used when storing the data, otherwise the
  resulting behavior is undefined.
}
Function GuardedLoad(const Storage; out Destination; Size: TMemSize): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                Protected sharing
--------------------------------------------------------------------------------
===============================================================================}
{
  Protected sharing

  From the user view, this mechanism works exactly the same as guarded sharing
  (see there for more info), except it can support an arbitrary number of both
  writers and readers.

  Internally it uses interlocked operations (ie. lock-prefixed instructions)
  on a single state byte stored with the data in shared storage to create a
  primitive asynchronous mutex.
}
{===============================================================================
    Protected sharing - declaration
===============================================================================}
{
  ProtectedStorageSize

  Returns minimum size of storage space (in bytes) needed for sharing of given
  amount of data (Size). Use this function when allocating shared storage.
}
Function ProtectedStorageSize(Size: TMemSize): TMemSize;

{
  ProtectedStoragePrepare

  Prepares given storage memory space for use in protected sharing.

  Call it before you first use a newly allocated storage or when you plan to
  reuse old storage in new round of sharing.

    WARNING - size given here is not size of data to be shared, but full
              true size of memory space allocated for shared storage.
}
procedure ProtectedStoragePrepare(var Storage; StorageSize: TMemSize);

//------------------------------------------------------------------------------
{
  ProtectedStore

  Stores Size-number of bytes from Source buffer to provided Storage memory
  space for use in protected sharing. Returns true afte successful store,
  false otherwise (try to write the data again later).

  Use function ProtectedStoragePrepare to obtain minimum size the storage
  needs to have to accomodate internal state along with the shared data.
}
Function ProtectedStore(const Source; Size: TMemSize; out Storage): Boolean;

{
  ProtectedLoad

  Loads Size-number of bytes from shared memory Storage into Destination
  buffer. Returns true if the read is successful, false otherwise.

  If the reading fails, it means the data cannot be read at the moment
  (nothing is written into Destination). Try the read again a moment later.

  You must use the same size as was used when storing the data, otherwise the
  resulting behavior is undefined.
}
Function ProtectedLoad(const Storage; out Destination; Size: TMemSize): Boolean;

implementation

uses
  CRCLite, MD5, SHA2, BitOps, InterlockedOps;

{===============================================================================
--------------------------------------------------------------------------------
                              Asynchronous sharing
--------------------------------------------------------------------------------
===============================================================================}
type
  TASChecksum = record
    case ChecksumType: TASChecksumType of
      csAuto:   (Dummy:   record end);
      csCRC32:  (CRC32:   TCRC32);
      csCRC64:  (CRC64:   TCRC64);
      csMD5:    (MD5:     TMD5);
      csSHA256: (SHA256:  TSHA256);
      csSHA512: (SHA512:  TSHA512);
  end;

{===============================================================================
    Asynchronous sharing - internals
===============================================================================}

Function AsyncChecksumResolve(Size: TMemSize; Checksum: TASChecksumType): TASChecksumType;
begin
If Checksum = csAuto then
  Result := AsyncChecksumType(Size)
else
  Result := Checksum;
end;

//------------------------------------------------------------------------------

Function AsyncChecksumSize(ChecksumType: TASChecksumType): TMemSize;
begin
case ChecksumType of
  csCRC32:  Result := SizeOf(TCRC32);
  csCRC64:  Result := SizeOf(TCRC64);
  csMD5:    Result := SizeOf(TMD5);
  csSHA256: Result := SizeOf(TSHA256);
  csSHA512: Result := SizeOf(TSHA512);
else
  raise EASIvalidValue.CreateFmt('AsyncChecksumSize: Invalid checksum type (%d).',[Ord(ChecksumType)]);
end;
end;

//------------------------------------------------------------------------------

Function AsyncChecksumCalculate(const Buffer; Size: TMemsize; ChecksumType: TASChecksumType): TASChecksum;
begin
Result.ChecksumType := ChecksumType;
case ChecksumType of
  csCRC32:  Result.CRC32 := BufferCRC32(Buffer,Size);
  csCRC64:  Result.CRC64 := BufferCRC64(Buffer,Size);
  csMD5:    Result.MD5 := BufferMD5(Buffer,Size);
  csSHA256: Result.SHA256 := BufferSHA2(fnSHA256,Buffer,Size).SHA256;
  csSHA512: Result.SHA512 := BufferSHA2(fnSHA512,Buffer,Size).SHA512;
else
  raise EASIvalidValue.CreateFmt('AsyncChecksumCalculate: Invalid checksum type (%d).',[Ord(ChecksumType)]);
end;
end;

//------------------------------------------------------------------------------

Function AsyncChecksumSame(const A,B: TASChecksum): Boolean;
begin
If A.ChecksumType = B.ChecksumType then
  case A.ChecksumType of
    csCRC32:  Result := SameCRC32(A.CRC32,B.CRC32);
    csCRC64:  Result := SameCRC64(A.CRC64,B.CRC64);
    csMD5:    Result := SameMD5(A.MD5,B.MD5);
    csSHA256: Result := SameSHA2(A.SHA256,B.SHA256);
    csSHA512: Result := SameSHA2(A.SHA512,B.SHA512);
  else
    raise EASIvalidValue.CreateFmt('AsyncChecksumSame: Invalid checksum type (%d).',[Ord(A.ChecksumType)]);
  end
else Result := False;
end;

{===============================================================================
    Asynchronous sharing - implementation
===============================================================================}

Function AsyncChecksumType(Size: TMemSize): TASChecksumType;
const
  KiB = 1024;
begin
If Size >= (16 * KiB) then
  Result := csSHA512
else If Size >= (4 * KiB) then
  Result := csSHA256
else If Size >= (1 * KiB) then
  Result := csMD5
else If Size >= (KiB div 4) then
  Result := csCRC64
else
  Result := csCRC32;
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function AsyncStorageSize(Size: TMemSize; ChecksumType: TASChecksumType = csAuto): TMemSize;
begin
Result := Size + AsyncChecksumSize(AsyncChecksumResolve(Size,ChecksumType));
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure AsyncStore(const Source; Size: TMemSize; out Storage; ChecksumType: TASChecksumType = csAuto);
var
  Checksum: TASChecksum;
begin
// resolve possible csAuto
ChecksumType := AsyncChecksumResolve(Size,ChecksumType);
// calculate checksum of stored data
Checksum := AsyncChecksumCalculate(Source,Size,ChecksumType);
// store the data
CopyMemory(@Storage,@Source,Size);
// store checksum just behind the data
Move(Checksum.Dummy,PtrAdvance(@Storage,TMemOff(Size))^,AsyncChecksumSize(ChecksumType));
end;

//------------------------------------------------------------------------------

Function AsyncLoad(const Storage; out Destination; Size: TMemSize; ChecksumType: TASChecksumType = csAuto): Boolean;
var
  Checksum: TASChecksum;
begin
// resolve possible csAuto
ChecksumType := AsyncChecksumResolve(Size,ChecksumType);
// first load shared data into destination
CopyMemory(@Destination,@Storage,Size);
// immediatelly load the checksum
Move(PtrAdvance(@Storage,TMemOff(Size))^,Addr(Checksum.Dummy)^,AsyncChecksumSize(ChecksumType));
Checksum.ChecksumType := ChecksumType;
// calculate checksum of loaded data and compare it with the one loaded
Result := AsyncChecksumSame(AsyncChecksumCalculate(Destination,Size,ChecksumType),Checksum);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Guarded sharing
--------------------------------------------------------------------------------
===============================================================================}
const
  ASYNC_LOCK_UNLOCKED = 0;  // must always be zero, do not change
  ASYNC_LOCK_LOCKED   = 1;

type
  TASGuards = packed record
    WriteGuard: Byte;
    ReadGuard:  Byte;
  end;
  PASGuards = ^TASGuards;

{===============================================================================
    Guarded sharing - internals
===============================================================================}

{$IFNDEF PurePascal}

Function GuardsWriteLock(Guards: PASGuards): Boolean; register; assembler;
asm
{
  This function is here only to eliminate a single function call (to
  WriteBarrier, which just issues SFENCE), otherwise it does not bring
  much benefit.

  Also note that we do no need to check whether current system supports
  SSE extension (SFENCE) since it is done by InterlockedOps library used
  here.
}
{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                  win32 + lin32     win64         lin64
      Guards           EAX           RCX           RDI
      Result            AL            AL            AL
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     byte ptr [RCX], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [RCX + 1], 0
  {$ELSE}
    MOV     byte ptr [RDI], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [RDI + 1], 0
  {$ENDIF}
{$ELSE}
    MOV     byte ptr [EAX], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [EAX + 1], 0
{$ENDIF}
    SETE    AL
end;

//------------------------------------------------------------------------------

Function GuardsReadLock(Guards: PASGuards): Boolean; register; assembler;
asm
{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                  win32 + lin32     win64         lin64
      Guards           EAX           RCX           RDI
      Result            AL            AL            AL
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     byte ptr [RCX + 1], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [RCX], 0
  {$ELSE}
    MOV     byte ptr [RDI + 1], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [RDI], 0
  {$ENDIF}
{$ELSE}
    MOV     byte ptr [EAX + 1], ASYNC_LOCK_LOCKED
    SFENCE
    CMP     byte ptr [EAX], 0
{$ENDIF}
    SETE    AL
end;

{$ENDIF}

{===============================================================================
    Guarded sharing - implementation
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function GuardedStorageSize(Size: TMemSize): TMemSize;
begin
Result := Size + SizeOf(TASGuards);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure GuardedStoragePrepare(var Storage; StorageSize: TMemSize);
begin
PWord(PtrAdvance(@Storage,TMemOff(StorageSize) - 2))^ := 0;
WriteBarrier;
end;

//------------------------------------------------------------------------------

Function GuardedStore(const Source; Size: TMemSize; out Storage): Boolean;
var
  Guards: PASGuards;
begin
Result := False;
Guards := PASGuards(PtrAdvance(@Storage,TMemOff(Size)));
{$IFDEF PurePascal}
Guards^.WriteGuard := ASYNC_LOCK_LOCKED;
{
  Issuing write barrier (instruction SFENCE) to ensure previous memory store
  is globally wisible.

  SFENCE is not serialized with memory loads, but it is with other SFENCE
  instructions, and since reader also issues it at corresponding place, it
  gets serialized with it and memory load that follows it there should, in
  combination with fact that byte stores and loads are always atomic, see
  what we have stored here.
}
WriteBarrier;
If Guards^.ReadGuard = ASYNC_LOCK_UNLOCKED then
{$ELSE}
If GuardsWriteLock(Guards) then
{$ENDIF}
  begin
    CopyMemory(@Storage,@Source,Size);
    Result := True;
  end;
Guards^.WriteGuard := ASYNC_LOCK_UNLOCKED;
end;

//------------------------------------------------------------------------------

Function GuardedLoad(const Storage; out Destination; Size: TMemSize): Boolean;
var
  Guards: PASGuards;
begin
Result := False;
Guards := PASGuards(PtrAdvance(@Storage,TMemOff(Size)));
{$IFDEF PurePascal}
Guards^.ReadGuard := ASYNC_LOCK_LOCKED;
WriteBarrier;
If Guards^.WriteGuard = ASYNC_LOCK_UNLOCKED then
{$ELSE}
If GuardsReadLock(Guards) then
{$ENDIF}
  begin
    CopyMemory(@Destination,@Storage,Size);
    Result := True;
  end;
Guards^.ReadGuard := ASYNC_LOCK_UNLOCKED;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Protected sharing                                
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Protected sharing - implementation
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function ProtectedStorageSize(Size: TMemSize): TMemSize;
begin
Result := Size + 1;
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure ProtectedStoragePrepare(var Storage; StorageSize: TMemSize);
begin
InterlockedStore(PByte(PtrAdvance(@Storage,TMemOff(StorageSize) - 1))^,0);
end;

//------------------------------------------------------------------------------

Function ProtectedStore(const Source; Size: TMemSize; out Storage): Boolean;
var
  Interlock:  PByte;
begin
Interlock := PByte(PtrAdvance(@Storage,TMemOff(Size)));
If InterlockedCompareExchange(Interlock^,ASYNC_LOCK_LOCKED,ASYNC_LOCK_UNLOCKED) = ASYNC_LOCK_UNLOCKED then
  try
    CopyMemory(@Storage,@Source,Size);
    Result := True;
  finally
    InterlockedStore(Interlock^,ASYNC_LOCK_UNLOCKED);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function ProtectedLoad(const Storage; out Destination; Size: TMemSize): Boolean;
var
  Interlock:  PByte;
begin
Interlock := PByte(PtrAdvance(@Storage,TMemOff(Size)));
If InterlockedCompareExchange(Interlock^,ASYNC_LOCK_LOCKED,ASYNC_LOCK_UNLOCKED) = ASYNC_LOCK_UNLOCKED then
  try
    CopyMemory(@Destination,@Storage,Size);
    Result := True;    
  finally
    InterlockedStore(Interlock^,ASYNC_LOCK_UNLOCKED);
  end
else Result := False;
end;

end.
