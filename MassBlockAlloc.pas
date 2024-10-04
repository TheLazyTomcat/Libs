{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  MassBlockAlloc

    This library was designed for situation, where a large number of relatively
    small blocks of equal size is rapidly allocated and deallocated in multi
    thread environment and where performance is important.

    As rapid (de)allocation of small data can put default memory manager under
    a significant load, this library was created to offload this operation from
    it and (potentially) improve performance.

    The blocks are not trully allocated and deallocated. The allocator (an
    instance of class TMassBlockAlloc) allocates memory in large chunks called
    segments, where each segment can hold many blocks. When a new block is
    required, it is only selected from a list of unused blocks in a segment.
    When block is freed, it is only marked as unused in its parent segment.

    Since the memory is managed completely within this library, it is also
    possible to force the allocator to allocate all the blocks with specific
    address alignment (eg. for use in vector instructions such as SSE or AVX).

  Version 1.0 (2024-04-14)

  Last change 2024-10-04

  ©2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MassBlockAlloc

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxMath       - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps        - github.com/TheLazyTomcat/Lib.BitOps
    BitVector     - github.com/TheLazyTomcat/Lib.BitVector

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol MassBlockAlloc_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo         - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit MassBlockAlloc;
{
  MassBlockAlloc_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  MassBlockAlloc_UseAuxExceptions to achieve this.
}
{$IF Defined(MassBlockAlloc_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  AllowLargeSegments

  Quadruples maximum allowed size of segment.

    Segment size is normally limited to 256MiB (1GiB in 64bit builds), so by
    defining this symbol this limit is increased to 1GiB (4GiB in 64bit builds).

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol MassBlockAlloc_AllowLargeSegments_On.
}
{$UNDEF AllowLargeSegments}
{$IFDEF MassBlockAlloc_AllowLargeSegments_On}
  {$DEFINE AllowLargeSegments}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  AuxTypes, AuxClasses, BitVector, BitOps
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exception
===============================================================================}
type
  EMBAException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EMBASystemError      = class(EMBAException);
  EMBAInvalidValue     = class(EMBAException);
  EMBAInvalidState     = class(EMBAException);
  EMBAInvalidAddress   = class(EMBAException);
  EMBAInvalidAction    = class(EMBAException);
  EMBAIndexOutOfBounds = class(EMBAException);
  EMBAOutOfResources   = class(EMBAException);

{===============================================================================
    Public constants
===============================================================================}
const
  OneKiB = TMemSize(1024);          // one kibibyte (2^10, kilobyte for you oldschools :P)
  OneMiB = TMemSize(1024 * OneKiB); // one mebibyte (2^20, megabyte)
  OneGiB = TMemSize(1024 * OneMiB); // one gibibyte (2^30, gigabyte)

const
  // maximum size of segment (differs according to system and AllowLargeSegments symbol)
  MBA_MAX_SEG_SZ = TMemSize({$IFDEF AllowLargeSegments}4 * {$ENDIF}{$IFDEF CPU64bit}OneGiB{$ELSE}256 * OneMiB{$ENDIF});
  // maximum size of one block
  MBA_MAX_BLK_SZ = TMemSize(128 * OneMiB);

{===============================================================================
--------------------------------------------------------------------------------
                                   TMBASegment
--------------------------------------------------------------------------------
===============================================================================}
type
  // used to return burst allocation
  TMBAPointerArray = array of Pointer;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
{
  TMBASegmentSizingStyle

  Selects method calculating size of segment when one is being created.

    szMinCount  - Size is selected so that the segment will be large enough to
                  fit at least the prescribed number of blocks. Note that the
                  segment size is calculated with page size granularity.
                  BlockCount must be larger than zero and resulting size must
                  not exceed MBA_MAX_SEG_SZ.

    szMinSize   - Segment will be at least as large as prescribed (ie. never
                  smaller). Calculated with page size granularity.
                  MemorySize must be larger than zero and smaller or equal to
                  MBA_MAX_SEG_SZ.

    szExactSize - Segment will have exactly the given size, irrespective of its
                  value or page size.
                  The prescribed size must be large enough to accomodate at
                  least one block (note that, if MapInSegment is true, this
                  also includes the allocation map - which is one byte per 8
                  blocks) and smaller or equal to MBA_MAX_SEG_SZ.
}
  TMBASegmentSizingStyle = (szMinCount,szMinSize,szExactSize);

{
  TMBASegmentSettings

  Used to store and pass settings of a segment (instance of TMBASegment).

    FailOnUnfreed - When the segment is being destroyed, there are still some
                    allocated/unfreed blocks and this option is true, then an
                    exception of class EMBAInvalidState is raised in destructor.
                    When false, then the unfreed blocks are ignored and
                    destructor proceeds normally.

                      default value - True

    MapInSegment  - When true, then the internal map of allocated blocks is
                    stored in the segment memory (that is, in the same memory
                    space as blocks).
                    Otherwise memory for the map is allocated separately.

                      default value - False

    BlockSize     - Requested size of the block, in bytes. Note that size
                    reserved in the segment for one block might be (much)
                    larger, depending on selected memory alignment (padding).
                    Must be larger than zero and smaller than MBA_MAX_BLK_SZ.

                      default value - 0 - Must be set by the user!

    Alignment     - Each block within the segment is guaranteed to have this
                    memory alignment.
                    If block size is not an integral multiple of alignment
                    bytes, then a padding is created between the blocks to
                    ensure proper alignment of consecutive blocks - this
                    creates a wasted space, so be aware of it!
                    You can use this eg. when allocating vectors used in calls
                    to SSE/AVX.

                      default value - maNone (no alignment)

    SizingStyle   - See description of type TMBASegmentSizingStyle.

                      default value - szMinCount

    BlockCount    - Requested number of blocks. Observed only for SizingStyle
                    of szMinCount (see description of TMBASegmentSizingStyle
                    for details).

                      default value - 0 - Must be set by the user!

    MemorySize    - Requested size of segment memory. Observed only for
                    SizingStyle of szMinSize and szExactSize (see description
                    of TMBASegmentSizingStyle for details).

                      default value - <none> (not observed for szMinCount)
}
  TMBASegmentSettings = record
    FailOnUnfreed:  Boolean;
    MapInSegment:   Boolean;
    BlockSize:      TMemSize;
    Alignment:      TMemoryAlignment;
    case SizingStyle: TMBASegmentSizingStyle of
      szMinCount:   (BlockCount:  Integer);
      szMinSize,
      szExactSize:  (MemorySize:  TMemSize);
  end;

{===============================================================================
    TMBASegment - class declaration
===============================================================================}
type
  TMBASegment = class(TCustomListObject)
  protected
    fSettings:          TMBASegmentSettings;
    fReservedBlockSize: TMemSize;
    fBlockCount:        Integer;
    fMemorySize:        TMemSize;
    fMemory:            Pointer;
    fAllocationMap:     TBitVectorStatic;
    fLowAddress:        Pointer;  // address of first block
    fHighAddress:       Pointer;  // address BEHIND the last block (reserved size)
    // getters, setters
    Function GetIsAllocated(Index: Integer): Boolean; virtual;
    Function GetBlockAddress(Index: Integer): Pointer; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // init/final
    Function CalculateMemorySize: TMemSize; virtual;
    Function CalculateBlockCount: Integer; virtual;
    procedure Initialize(const Settings: TMBASegmentSettings); virtual;
    procedure Finalize; virtual;
    // auxiliary methods and utilities
    Function FindSpaceForBuffer(BufferSize: TMemSize; out RequiredBlockCount: Integer): Integer; virtual;
  public
  {
    MemoryPageSize

    Returns size of memory page (in bytes) as indicated by operating system for
    the calling process.
  }
    class Function MemoryPageSize: TMemSize; virtual;
    constructor Create(const Settings: TMBASegmentSettings);
    destructor Destroy; override;
  {
    LowIndex

    Index of first block in Blocks array property.
  }
    Function LowIndex: Integer; override;
  {
    HighIndex

    Index of last block in Blocks array property.
  }
    Function HighIndex: Integer; override;
  {
    BufferBlockCount

    Returns number of blocks withing this segment needed to allocate a buffer
    of given size (note that the full RESERVED block size is taken into account
    in this calculation).

      NOTE - it might return a number larger than BlockCount, this is not
             checked as this function is considered to be only informative.
  }
    Function BufferBlockCount(BufferSize: TMemSize): Integer; overload; virtual;
    //- address checking -------------------------------------------------------
  {
    AddressOwned

    Returns true when the given address is within any present block (points to
    a byte that is part of the block), false otherwise.

    When strict is true, then the address must be within its indicated size
    (Settings.BlockSize), when strict is false then it can lay anywhere within
    reserved block size (ie. it can point into block padding).
  }
    Function AddressOwned(Address: Pointer; Strict: Boolean = False): Boolean; virtual;
  {
    AddressIndexOf

    Returns index of block to which the given address points.

    If strict is true, then only indicated block size is observed, otherwise
    the address can be anywhere within reserved memory of the block.

    When the address does not point to any present block (or points to padding
    when Strict is true), then a negative value is returned.
  }
    Function AddressIndexOf(Address: Pointer; Strict: Boolean = False): Integer; virtual;
  {
    Finds block to which given address points, stores its index in Index output
    parameter and returns true. If no block overlapping with the given address
    can be found, then the function returns false and value of Index is
    undefined.

    Refer to method AddressIndexOf for explanation of parameter Strict.
  }
    Function AddressFind(Address: Pointer; Strict: Boolean; out Index: Integer): Boolean; virtual;
  {
    BlockOwned

    Returns true when the given address points to any present block (its
    starting address), false otherwise.
  }
    Function BlockOwned(Block: Pointer): Boolean; virtual;
  {
    BlockIndexOf

    Returns index of block with the given starting address. If the address does
    not point to any block, then a negative value is returned.
  }
    Function BlockIndexOf(Block: Pointer): Integer; virtual;
  {
    BlockFind

    Finds block with the given starting address, stores its index in Index
    output parameter and returns true. If no block with given address can be
    found, then the function returns false and value of Index is undefined.
  }
    Function BlockFind(Block: Pointer; out Index: Integer): Boolean; virtual;
    //- basic (de)allocation ---------------------------------------------------
  {
    AllocateBlock

    Selects first unallocated block, marks it as allocated and sets Block param
    to its address. If no block can be allocated, then an exception of class
    EMBAOutOfResources is raised.

    Can also raise an EMBAInvalidState exception if internal data are somehow
    damaged.

    When init memory is set to true, then the block memory is cleared (filled
    with zeroes), otherwise its content is completely undefined and might
    contain content from when it was previously allocated. 
  }
    procedure AllocateBlock(out Block: Pointer; InitMemory: Boolean); virtual;
  {
    FreeBlock

    Marks given block as not allocated and sets Block parameter to nil.

    If given pointer does not point to a block withing this segment, then an
    exception of class EMBAInvalidAddress is raised.

    If freeing buffer that is not allocated, then an EMBAInvalidState exception
    is raised.
  }
    procedure FreeBlock(var Block: Pointer); virtual;
  {
    CanAllocateBuffer

    Returns true if this segment can allocate buffer of given size, false
    otherwise.
  }
    Function CanAllocateBuffer(BufferSize: TMemSize): Boolean; virtual;
  {
    TryAllocateBuffer

    Tries to allocate buffer of given size. True is returned when it succeeds,
    false otherwise - in which case nothing is allocated and value of Buffer is
    undefined.
  }
    Function TryAllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean): Boolean; virtual;
  {
    AllocateBuffer

    Allocates buffer of given size.

    Buffer size must be larger than zero, otherwise an EMBAInvalidValue
    exception is raised.

    If it cannot be allocated (eg. because there is not enough contiguous free
    blocks), then EMBAOutOfResources exception is raised.
  }
    procedure AllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean); virtual;
  {
    FreeBuffer

    Deallocates all constituent blocks of given buffer and sets parameter Buffer
    to nil.

    If the Buffer does not point to any block within this segment, then an
    EMBAInvalidAddress exception is raised.

    BufferSize must be larger than zero, otherwise an EMBAInvalidValue exception
    is raised.

    If the buffer size is not a valid number (eg. it is not the same number as
    was used during allocation), then an EMBAInvalidValue or EMBAInvalidState
    exception can be raised.
  }
    procedure FreeBuffer(var Buffer: Pointer; BufferSize: TMemSize); virtual;
  {
    AllocateAll

    If the segment is empty, then it marks all blocks as allocated and returns
    their addresses in Blocks output parameter (in the order they appear in the
    segment) - practically allocating all blocks in one go.
    If this segment is not empty, then an EMBAInvalidState exception is raised.

    InitMemory set to true ensures that all blcck will be zeroed, otherwise
    their memory can contain bogus data.
  }
    procedure AllocateAll(out Blocks: TMBAPointerArray; InitMemory: Boolean); virtual;
    //- informative methods (names should be self-explanatory) -----------------
    Function IsFull: Boolean; virtual;
    Function IsEmpty: Boolean; virtual;
    Function AllocatedBlockCount: Integer; virtual;
    Function FreeBlockCount: Integer; virtual;
    //- memory statistics ------------------------------------------------------
    Function ReservedMemory: TMemSize; virtual;   // memory reserved for all blocks (including potential block padding)
    Function BlocksMemory: TMemSize; virtual;     // memory of all blocks (excluding padding, so only BlockCount * BlockSize)
    Function AllocatedMemory: TMemSize; virtual;  // memory of allocated blocks (excluding padding)
    Function WastedMemory: TMemSize; virtual;     // all padding (including padding of individual blocks)
    Function MemoryEfficiency: Double; virtual;   // (MemorySize - WastedMemory) / MemorySize (ignores unused space of buffers/vectors)
    Function MemoryUtilization: Double; virtual;  // AllocatedMemory / BlocksMemory (ignores unused space of buffers/vectors)
    //- properties -------------------------------------------------------------
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Settings: TMBASegmentSettings read fSettings;
    property ReservedBlockSize: TMemSize read fReservedBlockSize;
    property BlockCount: Integer read fBlockCount;
    property MemorySize: TMemSize read fMemorySize;
    property Memory: Pointer read fMemory;
    property AllocationMap[Index: Integer]: Boolean read GetIsAllocated;
    property Blocks[Index: Integer]: Pointer read GetBlockAddress; default;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TMassBlockAlloc
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TMBAAllocatorSettings

  Used to store and pass settings of an allocator (TMassBlockAlloc instance).

    FreeEmptySegments   - When a block is freed and a segment in which it was
                          freed becomes empty (ie. has no more allocated
                          blocks), then this segment is immediately freed and
                          removed when this option is set to true.
                          When it is set to false, then the empty segment is
                          kept for further use.
                          You should carefully consider whether to enable or
                          disable this option. It can significantly increase
                          performance, but at the cost of memory space - decide
                          what is more important to your particular use case.

                            default value - False

    ThreadProtection    - Enables thread protection of the allocator state. You
                          should always leave this option enabled (true). Only
                          if you are 100% sure the allocator will be used
                          within a signle thread, then you might disable it.
                          Must be set to true if asynchronous cache filling is
                          to be enabled.

                            default value - True

    BlockCacheSettings  - Settings for block caching.

                          This mechanism pre-allocates number of blocks (count
                          depends on cache length) and stores them in the cache.
                          Later, when allocating a block using cached methods,
                          it is only taken from the cache and returned - this
                          is usually much faster than normally allocating it.
                          This also applies to block freeing - instead of
                          returning it to the segment, is is only placed into
                          the cache for later re-use.

                          But note that the cache is not automatically refilled
                          or emptied (when overfilled) - this must be done
                          manually by calling CacheFill method, but you can
                          select a convenient time when to do this.

      .Enable             - Enables (true) or disables (false) block caching.

                              default value - True

      .Length             - Length of the block cache (number of blocks it can
                            store).
                            Note that the actual size of the cache is double of
                            this number, but only this number of blocks can ever
                            be pre-allocated. This is to leave a space for
                            blocks returned by cache-freeing them.
                            Must be bigger than zero (when caching is enabled).

                              default value - 128

      .TrustedReturns     - When true, the blocks returned to cache-enabled
                            freeing are just put into the cache without checking
                            them for validity. When set to false, all blocks,
                            before being put to the cache, are checked.
                            Make sure you enable this option if pointers
                            returned are not 100% guaranteed to be valid block
                            pointers.

                              default value - True

      .AsynchronousFill   - Settings for asynchronous cache filling.

                            A background thread is spawned and this thread will,
                            either on-demand (a call to CacheFill) or after a
                            timeout (CycleLength), automatically fill the cache.

                            For this to work, the cache must be enabled and also
                            the thread protection must be enabled.

        ..Enable            - Enables asynchronous filling.

                                default value - False

        ..Interruptable     - When true, then the async. cache filling (blocks
                              allocation and deallocation) can be interrupted,
                              otherwise it will run until completion.

                              The async. filling runs in a thread and is
                              protected by thread lock, the same lock that
                              de/allocating functions are also acquiring. This
                              means that, while this is running (which might be
                              relatively long time), no allocating or
                              deallocating function can enter the section and
                              so they will block the call. This might be
                              undesirable, and for this an iterrupts are
                              implemented.

                              When enabled - if an async. filling is in process
                              and a de/allocating function is called, then this
                              call will set a flag that is constantly checked by
                              the filling.
                              When the filling evaluates the flag as being set
                              (meaning de/alloc. function is waiting to enter),
                              it will immediatelly stop operation and exit,
                              allowing the d/a function to continue and do its
                              work.
                              The async. filling is then completed in next
                              iteration of its cycle (after a timeout or when
                              demanded).

                                default value - True

        ..CycleLength       - Length (in milliseconds) or timeout of one async.
                              cache filling cycle.
                              The filling thread waits until a manual demand to
                              do the filling is made (by calling CacheFill), but
                              this waiting is not infinite. It will timeout
                              after the CycleLength milliseconds and perform the
                              filling at that point automatically (if needed).

                                default value - 1000 (ms, one second)

        ..PassExceptions    - Enables passing of cache filling exceptions from
                              asynchronous filling thread to the allocator. See
                              cache exceptions for more details.

                                default value - False
}
  TMBAAllocatorSettings = record
    FreeEmptySegments:  Boolean;
    ThreadProtection:   Boolean;
    BlockCacheSettings: record
      Enable:             Boolean;
      Length:             Integer;
      TrustedReturns:     Boolean;
      AsynchronousFill:   record
        Enable:             Boolean;
        Interruptable:      Boolean;
        CycleLength:        UInt32;
        PassExceptions:     Boolean;
      end;
    end;
    SegmentSettings:  TMBASegmentSettings;
  end;

const
  DefaultAllocatorSettings: TMBAAllocatorSettings = (
    FreeEmptySegments:  False;
    ThreadProtection:   True;
    BlockCacheSettings: (
      Enable:             True;
      Length:             128;
      TrustedReturns:     True;
      AsynchronousFill:   (
        Enable:             False;
        Interruptable:      True;
        CycleLength:        1000;
        PassExceptions:     False));
    SegmentSettings:    (
      FailOnUnfreed:      True;
      MapInSegment:       False;
      BlockSize:          0;
      Alignment:          maNone;
      SizingStyle:        szMinCount;
      BlockCount:         0));

//------------------------------------------------------------------------------
type
  // used to return information when validating a block
  TMBAValidationInfo = record
    Address:        Pointer;
    Owned:          Boolean;
    Allocated:      Boolean;
    SegmentIndex:   Integer;      // negative for not-owned blocks
    SegmentObject:  TMBASegment;  // nil for not-owned blocks
    BlockIndex:     Integer;      // negative for not-owned blocks
  end;

  TMBAValidationInfoArray = array of TMBAValidationInfo;

{===============================================================================
    TMassBlockAlloc - class declaration
===============================================================================}
type
  TMassBlockAlloc = class(TCustomListObject)
  protected
    fSettings:      TMBAAllocatorSettings;
    fSegments:      array of TMBASegment;
    fSegmentCount:  Integer;
    fThreadLock:    TCriticalSection;
    fCache:         record
      Enabled:        Boolean;
      Data:           array of Pointer; // must be thread protected
      Count:          Integer;          // -||-
      AsyncFill:      record
        Enabled:        Boolean;
        Interrupts:     Boolean;
        InterruptFlag:  Integer;        // interlocked access only
        CycleEvent:     TEvent;
        FillerThread:   TThread;
        Exceptions:     record          // thread protect entire subrecord
          Objects:        array[0..255] of TObject;
          Count:          Integer;
          Start:          Integer;
        end;
      end;
    end;
    // getters, setters
    Function GetSegment(Index: Integer): TMBASegment; virtual;
    // inherited list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // internal list management
    Function AddSegment: Integer; virtual;
    procedure DeleteSegment(Index: Integer); virtual;
    procedure ClearSegments; virtual;
    // init/final
    procedure Initialize(Settings: TMBAAllocatorSettings); virtual;
    procedure Finalize; virtual;
    // other internals
    Function InternalCheckBlocks(var Blocks: array of Pointer; out FaultIndex: Integer): Boolean; virtual;
    procedure InternalAllocateBlock(out Block: Pointer; InitMemory: Boolean); virtual;
    procedure InternalFreeBlock(var Block: Pointer); virtual;
    procedure InternalAllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean); virtual;
    procedure InternalFreeBlocks(var Blocks: array of Pointer); virtual;
    Function InternalCacheFill(AsyncFill: Boolean): Integer; virtual;
    // cache exceptions intarnals
    procedure CacheExceptionsAdd(ExceptObject: TObject); virtual;
  public
    constructor Create(Settings: TMBAAllocatorSettings); overload;
  {
    Create(BlockSize,MinBlocksPerSegment,MemoryAlingment)

    Other settings (those not given by the function arguments) are set from
    DefaultAllocatorSettings constant.
  }
    constructor Create(BlockSize: TMemSize; MinBlocksPerSegment: Integer; MemoryAlignment: TMemoryAlignment = maNone); overload;
    destructor Destroy; override;
  {
    LowIndex

    Lowest valid index for Segments array property.
  }
    Function LowIndex: Integer; override;
  {
    HighIndex

    Highest valid index for Segments array property.
  }
    Function HighIndex: Integer; override;
  {
    ThreadLockAcquire

    Locks the thread protection - a critical section protecting data integrity
    of the allocator in multi-threaded environment. While the lock is in effect,
    no other thread can lock it - a try to do so will block until the lock is
    realeased by a thread that holds the lock.

    Can be called recursively in single thread, but each call to
    ThreadLockAcquire must be paired by a call to ThreadLockRelease to
    successfully unlock the section.

    Use this mechanism in multi-threaded environment when you are about to do
    number of calls and don't want the sequence to be interrupted by requests
    from other threads.
  }
    procedure ThreadLockAcquire; virtual;
  {
    ThreadLockRelease

    Unlocks the thread protection.

    See ThreadLockAcquire for more details.
  }
    procedure ThreadLockRelease; virtual;
  {
    AllocationAcquire

    Causes interrupt to asynchronous cache filling (if enabled)] and locks the
    thread protection (see ThreadLockAcquire for more details).

    Can be called recursively, but each call must be paired by a call to
    AllocationRelease to release the lock and disable interrupt.
  }
    procedure AllocationAcquire; virtual;
  {
    AllocationRelease

    Releases thread lock and decrements async. fill interrupt counter (in that
    order).
  }
    procedure AllocationRelease; virtual;
    //- block validation (checking) --------------------------------------------
  {
    ValidateBlock

    Returns validation information of the given block in ValidationInfo output
    argument.

      WARNING - the returned data might not be valid by the time the function
                returns if the allocator is used by multiple threads. If you
                want to work with the data (and especially with the segment
                object) make sure to do the call and data processing inside
                a thread lock (ThreadLockAcquire, ThreadLockRelease).
  }
    procedure ValidateBlock(Block: Pointer; out ValidationInfo: TMBAValidationInfo); overload; virtual;
  {
    ValidateBlock

    Returns true when the block is deemed valid, which means it is both owned
    by this allocator (ie. the address actually points to start of a block
    within existing segment owned by this object) and allocated (was previously
    allocated by this allocator), false otherwise.
  }
    Function ValidateBlock(Block: Pointer): Boolean; overload; virtual;
  {
    ValidateBlocks

    Returns validation information for all given blocks in ValidationInfo
    output array argument.

    The ValidationInfo array will have the same length as Blocks and
    information for each block will be placed at corresponding index in the
    output (Block[n] -> ValidationInfo[n]).

    See ValidateBlock for information about multi-threading issues.
  }
    procedure ValidateBlocks(const Blocks: array of Pointer; out ValidationInfo: TMBAValidationInfoArray); overload; virtual;
  {
    ValidateBlocks

    Returns true when all given blocks are owned by this allocator and are also
    all allocated, false otherwise.

    If no block is passed in the Block array, then false is returned.
  }
    Function ValidateBlocks(const Blocks: array of Pointer): Boolean; overload; virtual;
    //- single block (de)allocation --------------------------------------------
  {
    AllocateBlock

    Finds not allocated (free) block in existing segments and returns its
    address. If no free block is found, then new segment is added and the block
    is allocated in this segment.

    If InitMemory is set to true, then the returned block is zeroed, otherwise
    its content is completely undefined and may contain any data.
  }
    procedure AllocateBlock(out Block: Pointer; InitMemory: Boolean = False); overload; virtual;
    Function AllocateBlock(InitMemory: Boolean = False): Pointer; overload; virtual;
  {
    FreeBlock

    Finds segment that owns the passed block and frees it using this segment.
    The variable pointed to by Block argument is set to nil.

    If the given address does not point to any block within existing segments,
    then an EMBAInvalidAddress exception is raised.
  }
    procedure FreeBlock(var Block: Pointer); virtual;
    //- multiple blocks (de)allocation -----------------------------------------
  {
    AllocateBlocks

    Allocates multiple blocks to fill the given array.

    Init memory set to true ensures memory of all allocated blocks is zeroed.

    Note that it is more efficient (faster) to use this function instead of
    multiple calls to AllocateBlock.
  }
    procedure AllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean = False); virtual;
  {
    FreeBlocks

    Frees given blocks and sets them to nil.

    All given blocks are first checked for validity. If any is deemed invalid
    (eg. does not belong to any existing segment), then an EMBAInvalidAddress
    exception is raised and NONE of the blocks is freed.
    
    Note that there is a slight possibility that an exception will be raised
    even is some of the blocks are freed. You can easily discern that this
    happened because the exception will be of class EMBAInvalidValue. Also,
    you can probe which blocks were freed and which were not - the freed ones
    will be already set to nil, the unfreed will still have their addresses.
  }
    procedure FreeBlocks(var Blocks: array of Pointer); virtual;
    //- buffer (de)allocation --------------------------------------------------
  {
    AllocateBuffer

    Allocates general memory buffer using the segments.

    The allocation is done by finding contiguous sequence of blocks that
    together give the required size - they are then all marked as allocated and
    the buffer is simply overlayed on those blocks.    

    The buffer size must be larger than zero, otherwise an EMBAInvalidValue
    exception is raised. It must also fit into the segments (its size must be
    smaller or equal to reserved memory of a single segment), otherwise an
    exception of class EMBAOutOfResources is raised.

    If InitMemory is set to true, then the buffer memory is cleared (set to all
    zero), otherwise it can contain bogus data.

    Note that the buffer (the address) will be aligned according to memory
    alignment given in settings.

      WARNING - to free the buffer, do NOT use standard memory management
                functions or methods for blocks freeing, always use FreeBuffer
                method and make sure you pass the same size there as is passed
                here.
  }
    procedure AllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean = False); virtual;
  {
    FreeBuffer

    Frees the general memory buffer that was previously allocated using
    AllocateBuffer.

    Make sure you pass the same size as was passed when allocating the buffer.
  }
    procedure FreeBuffer(var Buffer: Pointer; BufferSize: TMemSize); virtual;
    //- block vector (de)allocation --------------------------------------------
  {
    AllocateBlockVector

    Allocates a contigous array (vector) of blocks, each of the size given in
    settings (SegmentSettings.BlockSize) - this is equivalent to allocating
    a buffer of size VectorLength * BlockSize.

    The blocks are byte-packed, there is no padding between them. Therefore,
    only the first block (the address of vector) is guaranteed to have proper
    alignment, consecutive blocks start just where the previous ones ended.

    The VectorLength must be larger than zero, otherwise an EMBAInvalidValue
    exception is raised. Also, all limits in effect for AllocateBuffer apply
    to this call.

      WARNING - always use FreeBlockVector to free the returned address and
                make sure to pass the same length as was used in allocation.
  }
    procedure AllocateBlockVector(out Vector: Pointer; VectorLength: Integer; InitMemory: Boolean = False); virtual;
  {
    FreeBlockVector

    Frees vector of blocks allocated by AllocateBlockVector - equivalent to
    calling FreeBuffer with BufferSize set to VectorLength * BlockSize.
  }
    procedure FreeBlockVector(var Vector: Pointer; VectorLength: Integer); virtual;
    //- cache management -------------------------------------------------------
  {
    --- Block cache ---

    This mechanism is here to speed-up short allocation and deallocation bursts
    (also works for random scattered de/allocations).

    It works by pre-allocating given number of blocks and storing them in a
    simple array (the cache). Then, when cached allocation is called, it merely
    takes this pre-allocated block from the array and returns it, which is
    usually much faster than full allocation (search for free block, marking
    it allocated, ...).
    Deallocation works the same way - the returned block is only put into the
    cache and left there for future use, it is not classically freed.

    But note that the cache must be also re-filled/cleaned from time to time.
    You can either do it manually (by calling CacheFill), or, by enabling
    asynchronous cache filling, leave it on a background thread.
  }
  {
    CacheCount

    Returns number of pre-allocated blocks available in block cache.

    Note that, if the allocator is used by multiple threads, the number might
    not reflect reality by the time the function returns, simply because other
    threads can use the cache immediately as it is unlocked.
    To ensure the number stays correct, you have to thread-lock the allocator
    (ThreadLockAcquire) prior to calling CacheCount (remember to unlock it
    when you are done by calling ThreadLockRelease, otherwise the allocator
    could not serve other threads).
  }
    Function CacheCount: Integer; virtual;
  {
    CacheFill

    Checks number of pre-allocated blocks present in the block cache. If it is
    lower than cache length given in settings, then new blocks are allocated
    and placed in the cache. If it is higher, then blocks over the given number
    are freed.
    To put it simpy, this function ensures that the cache contains exactly the
    number of pre-allocated blocks as indicated in settings.

    If ForceSynchronous is true or when asynchronous filling is disabled, then
    the action is done directly in the calling context (ie. "here and now").
    If ForceSynchronous is false and asynchronous filling is enabled, then no
    action is performed, the call only signals filler thread to do the filling.

    Note that if the cache is disabled, this function does nothing and returns
    immediately.
  }
    procedure CacheFill(ForceSynchronous: Boolean = False); virtual;
    //- cache eceptions management ---------------------------------------------
  {
    Cache exceptions

    If any exception is raised by cache filling routine when it is called by
    the asynchronous filling thread, then it cannot be left unhandled as that
    would prematurely kill the thread. And because the thread is not under
    control of the user, it might be undesirable to just silently drop it.

    This mechanism is here to inform code using the allocator about exceptions
    that occured in the async. filling thread.

    When an exception is raised during filling, it is intercepted, its object
    is acquired (ie. ensured that it will not be automatically destroyed) and
    then passed to the allocator which stores this object in an internal list.

    Exception objects in this list can then be popped from this list or
    re-raised in the context of thread that is using the allocator.

      NOTE - the list has maximum length that cannot exceeded (currently 256
             items). If it becomes full and new exception is added to it, then
             the oldest stored exception is freed and the one replaces it.
  }
  {
    CacheExceptionsCount

    Returns number of exception objects stored in the internal list.

    Note that this number might not reflect reality by the time the function
    returns. If you plan to use it, make sure to call it while the allocator
    is thread-locked (ThreadLockAcquire/ThreadLockRelease).
  }
    Function CacheExceptionsCount: Integer; virtual;
  {
    CacheExceptionsPop

    Returns oldest stored exception object and removes it from the list.

    If no object is stored, then nil is returned.

      WARNING - as the object is removed from the internal list, you are
                henceforth responsible for its destruction (free it!).
  }
    Function CacheExceptionsPop: TObject; virtual;
  {
    CacheExceptionsRaise

    Removes oldest exception object from the list and raises it.

    If no object is stored, then nothing happens and the method just exits.
  }
    procedure CacheExceptionsRaise; virtual;
  {
    CacheExceptionsClear

    Frees and removes all stored exception objects.
  }
    procedure CacheExceptionsClear; virtual;
    //- cached (de)allocation --------------------------------------------------
  {
    CacheAllocateBlock

    Takes pre-allocated block from cache and returns it.

    If InitMemory is true, then the blcok memory is zeroed, otherwise it is
    left as is and might contain bogus data.

    If the cache is disabled or is empty, then normal allocation is called
    instead.
  }
    procedure CacheAllocateBlock(out Block: Pointer; InitMemory: Boolean = False); overload; virtual;
    Function CacheAllocateBlock(InitMemory: Boolean = False): Pointer; overload; virtual;
  {
    CacheFreeBlock

    Puts the given block into cache and sets variable pointed to by Block
    argument to nil.

    If Settings.BlockCacheSettings.TrustedReturns is false, then the block
    is first checked for validity (eg. whether it belongs to any existing
    segments) - when this test fails, then an EMBAInvalidAddress exception is
    raised.
    If mentioned settings is true (dafault), then the address is not checked.
    Therefore be careful what you pass here if trusted returns are true, as you
    might create disgusting bugs when invalid addresses are passed to this
    function. Also note that the invalid addresses will eventually be caught
    when the cache is emptied - then you might get seemingly nonsensical
    exception at any random time.

    If the cache is disabled or is full, then normal freeing is called instead.
  }
    procedure CacheFreeBlock(var Block: Pointer); virtual;
  {
    CacheAllocateBlocks

    Takes multiple blocks from block cache to fill the given array.

    If there is not enough blocks in cache (or the cache is disabled), then
    blocks not yet allocated are obtained using normal allocation
    (AllocateBlock[s]).

    If InitMemory is true then memory of all allocated blocks is zeroed.
  }
    procedure CacheAllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean = False); virtual;
  {
    CacheFreeBlocks

    Places passed blocks into cache for future use and sets their addresses
    to nil.

    Blocks that cannot fit into the cache are immediately freed using normal
    means (FreeBlock[s]).

    Note that, if Settings.BlockCacheSettings.TrustedReturns is false, all
    blocks are first checked for validity. If any is avaluated as invalid then
    an EMBAInvalidAddress exception is raised and none of the blocks is freed
    or re-cached.
    If the option is true, then no checks are performed - see CacheFreeBlock
    for description of potential problems that migh arise in this case.
  }
    procedure CacheFreeBlocks(var Blocks: array of Pointer); virtual;
    //- other allocation routines ----------------------------------------------
  {
    PrepareFor

    Ensures that there is at least Count number of free (unallocated) blocks
    in existing segments, creating new segments if necessary.

    Returns number of free blocks after the call. Note that in multi-threaded
    environment this number might be incorrect by the time the method returns.
    Use thread locking abound the call if you want to work with returned value.
  }
    Function PrepareFor(Count: Integer): Integer; virtual;
  {
    BurstAllocateBlocks

    Finds an empty segment or, when none is found, creates a new one, and then
    allocates all its blocks and returns their addresses in Blocks array.

    This function always allocates entire segment, it will newer use partially
    full segments (even is only one block is allocated there).

    Length of the returned array will be close to a value returned by method
    BlocksPerSegment, but it cannot be guaranteed it will be the same (see
    BlocksPerSegment for explanation).

    If InitMemory is true then memory of all allocated blocks is zeroed.
  }
    procedure BurstAllocateBlocks(out Blocks: TMBAPointerArray; InitMemory: Boolean = False); virtual;
    //- informations and statistics --------------------------------------------
    // NOTE - all following functions are subject to thread locking
  {
    SegmentSize

    Returns size of memory (in bytes) a single segment allocates for blocks
    (and potentially also for allocation map, see MapInSegment option in
    segment settings).
    This number should be, unlike BlocksPerSegment, the same for all segments.

    If no segment is present, then one is created, because only the segment can
    know its own size.
  }
    Function SegmentSize: TMemSize; virtual;
  {
    BlocksPerSegment

    Returns number of blocks in the first segment. If no segment is currently
    present, then one is created and the number obtained from it.

      WARNING - it is entirely possible that each segment will contain
                different number of blocks, be aware of this and do not take
                number returned by this function as some immutable constant,
                it is here only to provide a close estimate (true value will
                probably not differ by more than 1).
  }
    Function BlocksPerSegment: Integer; virtual;
  {
    AllocatedBlockCount

    Returns number of allocated blocks in all segments.

    While the number is accurate, it might not correspond to reality by the
    time the function returns if more than one thread is using the allocator.
    If you want to use this number in capacity other than informative, then
    make sure to thread-lock the allocator before calling it (and unlock it
    after you are done).
  }
    Function AllocatedBlockCount: Integer; virtual;
  {
    FreeBlockCount

    Returns number of free (unallocated) blocks in all segments.

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function FreeBlockCount: Integer; virtual;
  {
    TotalReservedMemory

    Returns size of memory (in bytes) reserved for all blocks (both allocated
    and unallocated) in all segments (including padding of individual blocks).

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function TotalReservedMemory: TMemSize; virtual;
  {
    TotalBlocksMemory

    Returns size (in bytes) of all blocks (allocated and unallocated) in all
    segments (excluding padding of individual blocks).

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function TotalBlocksMemory: TMemSize; virtual;
  {
    TotalAllocatedMemory

    Returns size (in bytes) of all allocated blocks in all segments (excluding
    padding of individual blocks).

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function TotalAllocatedMemory: TMemSize; virtual;
  {
    TotalWastedMemory

    Returns size (in bytes) of all wasted space (all padding) in all segments.

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function TotalWastedMemory: TMemSize; virtual;
  {
    TotalMemorySize

    Returns size (in bytes) of all memory allocated by all segments for blocks
    and potentialy also allocation maps.

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function TotalMemorySize: TMemSize; virtual;
  {
    MemoryEfficiency

    Returns efficiency of allocated memory - ie. how much of it is actually
    used for useful stuff. Returned as a normalized value, so in interval [0,1],
    and calculated as:

      (TotalMemorySize - TotalWastedMemory) / TotalMemorySize

    Note that this ignores unused space of allocated buffers and vectors.

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function MemoryEfficiency: Double; virtual;
  {
    MemoryUtilization

    Indicates how much of the blocks memory is being used for allocated blocks.
    Returned as a normalized value (interval [0,1]) and calculated as:

      TotalAllocatedMemory / TotalBlocksMemory

    Ignores unused space of allocated buffers and vectors.

    See AllocatedBlockCount for info about thread-related issues.
  }
    Function MemoryUtilization: Double; virtual;
    //- properties -------------------------------------------------------------
    property Settings: TMBAAllocatorSettings read fSettings;
    property Count: Integer read GetCount;
    property SegmentCount: Integer read GetCount;
  {
    Segments

      WARNING - while working on segment taken from this property, make sure
                you thread-lock the allocator (BEFORE obtaining the object) and
                do not call any allocating or deallocating methods.
  }
    property Segments[Index: Integer]: TMBASegment read GetSegment; default;
  end;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}baseunix,{$ENDIF} 
  AuxMath;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Externals
===============================================================================}

{$IFDEF Linux}

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function sysconf(name: cInt): cLong; cdecl; external;

const
  _SC_PAGESIZE = 30;

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TMBASegment
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMBASegment - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMBASegment - protected methods
-------------------------------------------------------------------------------}

Function TMBASegment.GetIsAllocated(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := fAllocationMap[Index]
else
  raise EMBAIndexOutOfBounds.CreateFmt('TMBASegment.GetIsAllocated: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TMBASegment.GetBlockAddress(Index: Integer): Pointer;
begin
If CheckIndex(Index) then
  Result := PtrAdvance(fLowAddress,Index,fReservedBlockSize)
else
  raise EMBAIndexOutOfBounds.CreateFmt('TMBASegment.GetBlockAddress: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TMBASegment.GetCapacity: Integer;
begin
Result := fBlockCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMBASegment.SetCapacity(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TMBASegment.GetCount: Integer;
begin
Result := fBlockCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMBASegment.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TMBASegment.CalculateMemorySize: TMemSize;
var
  PredAlign:      TMemSize;
  PredAlignBytes: TMemSize;
  PageSize:       TMemSize;
  TempMemorySize: TMemSize;
begin
{
  Calculation legend:

    P ... page size
    A ... alignment bytes (expected to be integral power of two)
    B ... reserved block size

    m ... minimal memory size
    n ... size of allocation map
    c ... minimal block count

    M ... memory size (result)
}
PredAlign := AlignmentBytes(fSettings.Alignment);
PredAlignBytes := Pred(PredAlign);
PageSize := MemoryPageSize;
case fSettings.SizingStyle of
  szMinCount:
    begin
      If fSettings.BlockCount <= 0 then
        raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateProperties: Invalid minimal block count (%d).',[fSettings.BlockCount]);
      // maximum size check
      If (fReservedBlockSize * TMemSize(fSettings.BlockCount)) > MBA_MAX_SEG_SZ then
        raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateProperties: Segment would be too large (%u).',
          [PtrInt(fReservedBlockSize * TMemSize(fSettings.BlockCount))]);
      If fSettings.MapInSegment then
      {
        Calculate memory size with page granularity. Minimum size of memory
        is requested number of blocks multiplied by reserved block size plus
        worst-case alignment padding and size of allocation map.

            M = ceil(m / P) * P

              m = pred(A) + (c * B) + n

                n = ceil(c / 8)
      }
        Result := uDivCeilPow2(PredAlignBytes + uDivCeilPow2NC(CvtI2U(fSettings.BlockCount),8) +
          (TMemSize(fSettings.BlockCount) * fReservedBlockSize),PageSize) * PageSize
      else
      {
        Here, the minimum memory size is just number of blocks multiplied by
        reserved block size plus worst-case padding. Memory size is again
        selected with page granurality.

            M = ceil(m / P) * P

              m = pred(A) + (c * B)
      }
        Result := uDivCeilPow2(PredAlignBytes + (TMemSize(fSettings.BlockCount) * fReservedBlockSize),PageSize) * PageSize;
    end;
  szMinSize:
    begin
      If (fSettings.MemorySize <= 0) or (fSettings.MemorySize > MBA_MAX_SEG_SZ) then
        raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateMemorySize: Invalid memory size (%u).',[PtrInt(fSettings.MemorySize)]);
      // ensure the memory is large enough  
      If fSettings.MemorySize < (fReservedBlockSize + PredAlignBytes + uIfThen(fSettings.MapInSegment,TMemSize(1),0)) then
        TempMemorySize := fReservedBlockSize + PredAlignBytes + uIfThen(fSettings.MapInSegment,TMemSize(1),0)
      else
        TempMemorySize := fSettings.MemorySize;
    {
      Allocate with page granularity...

          M = ceil(m / P) * P
    }
      Result := uDivCeilPow2(TempMemorySize,PageSize) * PageSize;
    end;
  szExactSize:
    begin
    {
      The memory must be large enough to accomodate at least one block with
      worst-case padding and optionally also its map (one bit for one block,
      therefore one byte).

          M >= (pred(A) + B) + 1
    }
      If fSettings.MemorySize < (fReservedBlockSize + PredAlignBytes + uIfThen(fSettings.MapInSegment,TMemSize(1),0)) then
        raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateMemorySize: Memory size (%u) too small.',[PtrInt(fSettings.MemorySize)]);
      If fSettings.MemorySize > MBA_MAX_SEG_SZ then
        raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateMemorySize: Invalid memory size (%u).',[PtrInt(fSettings.MemorySize)]);
      Result := fSettings.MemorySize;
    end;
else
  raise EMBAInvalidValue.CreateFmt('TMBASegment.CalculateMemorySize: Unknown sizing style (%d).',[Ord(fSettings.SizingStyle)]);
end;
end;

//------------------------------------------------------------------------------

Function TMBASegment.CalculateBlockCount: Integer;
var
  TempQuotient:   TMemSize;
  TempRemainder:  TMemSize;
begin
{
  Calculation legend:

    B ... reserved block size
    M ... memory size

    p ... padding for the first block (alignment offset)
    a ... misalignment of the allocated memory

    C ... block count (result)
}
If fSettings.MapInSegment then
  begin
  {
    Subtract padding from the memory size and divide remaining bits(!) by
    size of block in bits plus one bit in allocation map.

        C = floor(8((M - p) / (8B + 1)))

    A note on the actual imlementation - it is done this way to limit a risk of
    overflow in multiplication (mainly 8 * M, as M can be a large number).
  }
    uDivMod(fMemorySize - AlignmentOffset(fMemory,fSettings.Alignment),(8 * fReservedBlockSize) + 1,TempQuotient,TempRemainder);
    Result := CvtU2I32((8 * TempQuotient) + uDivFloor(8 * TempRemainder,(8 * fReservedBlockSize) + 1));
  end
{
  Here it is simple, just subtract alignment offset from memory size and divide
  what is left by reserved block size.

      C = floor((M - p) / B)
}
else Result := CvtU2I32(uDivFloorPow2(fMemorySize - AlignmentOffset(fMemory,fSettings.Alignment),fReservedBlockSize));
end;

//------------------------------------------------------------------------------

procedure TMBASegment.Initialize(const Settings: TMBASegmentSettings);
type
  TBitVectorClass = class of TBitVectorStatic;

  Function GetAllocationMapClass: TBitVectorClass;
  begin
    If (fBlockCount and 31) <> 0 then
      Result := TBitVectorStatic
    else
      Result := TBitVectorStatic32;
  end;

var
  AlignBytes: TMemSize;
begin
fSettings := Settings;
If (fSettings.BlockSize <= 0) or (fSettings.BlockSize > MBA_MAX_BLK_SZ) then
  raise EMBAInvalidValue.CreateFmt('TMBASegment.Initialize: Invalid block size (%u).',[PtrInt(fSettings.BlockSize)]);
// reserved block size must be a multiple of alignment bytes
AlignBytes := AlignmentBytes(fSettings.Alignment);
fReservedBlockSize := uDivCeilPow2(fSettings.BlockSize,AlignBytes) * AlignBytes;
// calculate memory size and allocate memory accordingly
fMemorySize := CalculateMemorySize;
If fMemorySize <= 0 then
  raise EMBAInvalidValue.CreateFmt('TMBASegment.Initialize: Invalid memory size (%u).',[PtrInt(fMemorySize)]);
GetMem(fMemory,fMemorySize);
fLowAddress := AlignedMemory(fMemory,fSettings.Alignment);
// calculate how many blocks we can use
fBlockCount := CalculateBlockCount;
If fBlockCount <= 0 then
  raise EMBAInvalidValue.CreateFmt('TMBASegment.Initialize: Invalid block count (%d).',[fBlockCount]);
// prepare allocation map
fHighAddress := PtrAdvance(fLowAddress,PtrInt(TMemSize(fBlockCount) * fReservedBlockSize));
If fSettings.MapInSegment then
  begin
    // allocation map resides in segment's memory
    fAllocationMap := GetAllocationMapClass.Create(fHighAddress,fBlockCount);
    fAllocationMap.Fill(False);
  end
// allocation map manages its own memory buffer
else fAllocationMap := GetAllocationMapClass.Create(fBlockCount,False);
end;

//------------------------------------------------------------------------------

procedure TMBASegment.Finalize;
begin
If Assigned(fAllocationMap) then
  begin
    If not fAllocationMap.IsEmpty and fSettings.FailOnUnfreed then
      raise EMBAInvalidState.CreateFmt('TMBASegment.Finalize: Not all blocks were freed (%d).',[fAllocationMap.PopCount]);
    fAllocationMap.Free;
  end;
If Assigned(fMemory) and (fMemorySize <> 0) then
  FreeMem(fMemory,fMemorySize);
end;

//------------------------------------------------------------------------------

Function TMBASegment.FindSpaceForBuffer(BufferSize: TMemSize; out RequiredBlockCount: Integer): Integer;
var
  i,j:            Integer;
  SequenceFound:  Boolean;
begin
If BufferSize > fReservedBlockSize then
  begin
  {
    Find out how many consecutive blocks is needed (note that padding between
    blocks, if present, is used too).
  }
    RequiredBlockCount := BufferBlockCount(BufferSize);
    If RequiredBlockCount > 1 then 
      begin
        Result := -1;
        // check if there is required number of free blocks
        If RequiredBlockCount <= FreeBlockCount then
          begin
            // find if there is a consecutive sequence of free blocks of required length
            i := fAllocationMap.FirstClean;
            repeat
              If not fAllocationMap[i] then
                begin
                  SequenceFound := True;
                  For j := Succ(i){because i-th item was already checked} to Pred(i + RequiredBlockCount) do
                    If fAllocationMap[j] then
                      begin
                        SequenceFound := False;
                        i := j; // note it will still be inc()-ed further down (so do not use j + 1)
                        Break{For j};
                      end;
                  If SequenceFound then
                    begin
                      Result := i;
                      Break{repeat-until};
                    end;
                end;
              Inc(i);
            until i > (fBlockCount - RequiredBlockCount);
          end;
      end
    else Result := fAllocationMap.FirstClean;
  end
else If BufferSize > 0 then
  begin
    RequiredBlockCount := 1;
    Result := fAllocationMap.FirstClean;
  end
else raise EMBAInvalidValue.CreateFmt('TMBASegment.FindSpaceForBuffer: Invalid buffer size (%u).',[PtrInt(BufferSize)]);
end;

{-------------------------------------------------------------------------------
    TMBASegment - public methods
-------------------------------------------------------------------------------}

class Function TMBASegment.MemoryPageSize: TMemSize;
{$IFDEF Windows}
var
  SystemInfo: TSystemInfo;
begin
FillChar(Addr(SystemInfo)^,SizeOf(TSystemInfo),0);
GetSystemInfo(SystemInfo);
Result := TMemSize(SystemInfo.dwPageSize);
{$ELSE}
var
  Temp: cLong;
begin
Temp := sysconf(_SC_PAGESIZE);
If Temp <> -1 then
  Result := TMemSize(Temp)
else
  raise EMBASystemError.CreateFmt('TMBASegment.MemoryPageSize: Failed to obtain page size (%d).',[errno_ptr^]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

constructor TMBASegment.Create(const Settings: TMBASegmentSettings);
begin
inherited Create;
Initialize(Settings);
end;

//------------------------------------------------------------------------------

destructor TMBASegment.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TMBASegment.LowIndex: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function TMBASegment.HighIndex: Integer;
begin
Result := Pred(fBlockCount);
end;

//------------------------------------------------------------------------------

Function TMBASegment.BufferBlockCount(BufferSize: TMemSize): Integer;
begin
Result := CvtU2I32(uDivCeilPow2(BufferSize,fReservedBlockSize))
end;

//------------------------------------------------------------------------------

Function TMBASegment.AddressOwned(Address: Pointer; Strict: Boolean = False): Boolean;
var
  Index:  Integer;
begin
Result := False;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Address) >= PtrUInt(fLowAddress)) and (PtrUInt(Address) < PtrUInt(fHighAddress)) then
  begin
    If Strict then
      begin
        // get index of block to which the address points
        Index := Integer((PtrUInt(Address) - PtrUInt(fLowAddress)) div PtrUInt(fReservedBlockSize));
        If CheckIndex(Index) then
          // check whether the address is within the block size (NOT reserved block size)
          Result := (PtrUInt(Address) - PtrUInt(PtrAdvance(fLowAddress,Index,fReservedBlockSize))) < fSettings.BlockSize;
      end
    else Result := True;
  end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMBASegment.AddressIndexOf(Address: Pointer; Strict: Boolean = False): Integer;
var
  Index:  Integer;
begin
Result := -1;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Address) >= PtrUInt(fLowAddress)) and (PtrUInt(Address) < PtrUInt(fHighAddress)) then
  begin
    Index := Integer((PtrUInt(Address) - PtrUInt(fLowAddress)) div PtrUInt(fReservedBlockSize));
    If CheckIndex(Index) then
      If not Strict or ((PtrUInt(Address) - PtrUInt(PtrAdvance(fLowAddress,Index,fReservedBlockSize))) < fSettings.BlockSize) then
        Result := Index;
  end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMBASegment.AddressFind(Address: Pointer; Strict: Boolean; out Index: Integer): Boolean;
begin
Index := AddressIndexOf(Address,Strict);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TMBASegment.BlockOwned(Block: Pointer): Boolean;
var
  Index:  Integer;
begin
Result := False;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Block) >= PtrUInt(fLowAddress)) and (PtrUInt(Block) < PtrUInt(fHighAddress)) then
  begin
    Index := Integer((PtrUInt(Block) - PtrUInt(fLowAddress)) div PtrUInt(fReservedBlockSize));
    If CheckIndex(Index) then
      Result := Block = PtrAdvance(fLowAddress,Index,fReservedBlockSize);
  end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMBASegment.BlockIndexOf(Block: Pointer): Integer;
var
  Index:  Integer;
begin
Result := -1;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Block) >= PtrUInt(fLowAddress)) and (PtrUInt(Block) < PtrUInt(fHighAddress)) then
  begin
    Index := Integer((PtrUInt(Block) - PtrUInt(fLowAddress)) div PtrUInt(fReservedBlockSize));
    If CheckIndex(Index) and (Block = PtrAdvance(fLowAddress,Index,fReservedBlockSize)) then
      Result := Index;
  end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMBASegment.BlockFind(Block: Pointer; out Index: Integer): Boolean;
begin
Index := BlockIndexOf(Block);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

procedure TMBASegment.AllocateBlock(out Block: Pointer; InitMemory: Boolean);
var
  Index:  Integer;
begin
If not fAllocationMap.IsFull then
  begin
    Index := fAllocationMap.FirstClean;
    If CheckIndex(Index) then
      begin
        // paranoia check
        If fAllocationMap[Index] then
          raise EMBAInvalidState.CreateFmt('TMBASegment.AllocateBlock: Block %d already allocated.',[Index]);
        fAllocationMap[Index] := True;
        Block := PtrAdvance(fLowAddress,Index,fReservedBlockSize);
        If InitMemory then
          FillChar(Block^,fReservedBlockSize{yes, better clear it all},0);
      end
    else raise EMBAOutOfResources.Create('TMBASegment.AllocateBlock: Unable to allocate block.');
  end
else raise EMBAOutOfResources.Create('TMBASegment.AllocateBlock: No free block to allocate.');
end;

//------------------------------------------------------------------------------

procedure TMBASegment.FreeBlock(var Block: Pointer);
var
  Index:  Integer;
begin
If BlockFind(Block,Index) then
  begin
    If not fAllocationMap[Index] then
      raise EMBAInvalidState.CreateFmt('TMBASegment.FreeBlock: Block %d not allocated.',[Index]);
    fAllocationMap[Index] := False;
    Block := nil;
  end
else raise EMBAInvalidAddress.CreateFmt('TMBASegment.FreeBlock: Invalid address (%p).',[Block]);
end;

//------------------------------------------------------------------------------

Function TMBASegment.CanAllocateBuffer(BufferSize: TMemSize): Boolean;
var
  RequiredBlockCount: Integer;
begin
Result := CheckIndex(FindSpaceForBuffer(BufferSize,RequiredBlockCount));
end;

//------------------------------------------------------------------------------

Function TMBASegment.TryAllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean): Boolean;
var
  RequiredBlockCount: Integer;
  Index,i:            Integer;
begin
Result := False;
If BufferSize > 0 then
  begin
    Index := FindSpaceForBuffer(BufferSize,RequiredBlockCount);
    If CheckIndex(Index) and (RequiredBlockCount > 0) then
      begin
      {
        Let's be super paranoid and check whether FindSpaceForBuffer returned
        valid data.
      }
        For i := Index to Pred(Index + RequiredBlockCount) do
          If fAllocationMap[i] then
            Exit;
        // mark all blocks as allocated
        For i := Index to Pred(Index + RequiredBlockCount) do
          fAllocationMap[i] := True;
        // get the actual allocation
        Buffer := PtrAdvance(fLowAddress,Index,fReservedBlockSize);
        If InitMemory then
          FillChar(Buffer^,TMemSize(RequiredBlockCount) * fReservedBlockSize,0);
        Result := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMBASegment.AllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean);
begin
If BufferSize > 0 then
  begin
    If not TryAllocateBuffer(Buffer,BufferSize,InitMemory) then
      raise EMBAOutOfResources.CreateFmt('TMBASegment.AllocateBuffer: Unable to allocate buffer of size %u.',[PtrInt(BufferSize)]);
  end
else raise EMBAInvalidValue.CreateFmt('TMBASegment.AllocateBuffer: Invalid buffer size (%u).',[PtrInt(BufferSize)]);
end;

//------------------------------------------------------------------------------

procedure TMBASegment.FreeBuffer(var Buffer: Pointer; BufferSize: TMemSize);
var
  RequiredBlockCount: Integer;
  Index,i:            Integer;
begin
If BufferSize > 0 then
  begin
    If Blockfind(Buffer,Index) then
      begin
        RequiredBlockCount := BufferBlockCount(BufferSize);
        If RequiredBlockCount <= (fBlockCount - Index) then
          begin
            // check if the buffer is really allocated in full length
            For i := Index to Pred(Index + RequiredBlockCount) do
              If not fAllocationMap[i] then
                raise EMBAInvalidState.CreateFmt('TMBASegment.FreeBuffer: Block %d not allocated.',[i]);
            For i := Index to Pred(Index + RequiredBlockCount) do
              fAllocationMap[i] := False;
            Buffer := nil;
          end
        else raise EMBAInvalidValue.CreateFmt('TMBASegment.FreeBuffer: Buffer too large (%u).',[PtrInt(BufferSize)]);
      end
    else raise EMBAInvalidAddress.CreateFmt('TMBASegment.FreeBuffer: Invalid address (%p).',[Buffer]);
  end
else raise EMBAInvalidValue.CreateFmt('TMBASegment.FreeBuffer: Invalid buffer size (%u).',[PtrInt(BufferSize)]);
end;

//------------------------------------------------------------------------------

procedure TMBASegment.AllocateAll(out Blocks: TMBAPointerArray; InitMemory: Boolean);
var
  i:  Integer;
begin
If IsEmpty then
  begin
    Blocks := nil;
    SetLength(Blocks,fBlockCount);
    Blocks[Low(Blocks)] := fLowAddress;
    For i := Succ(Low(Blocks)) to High(Blocks) do
      Blocks[i] := PtrAdvance(Blocks[Pred(i)],fReservedBlockSize);
    fAllocationMap.Fill(True);
    If InitMemory then
      FillChar(fLowAddress^,TMemSize(Length(Blocks)) * fReservedBlockSize,0);
  end
else EMBAInvalidState.CreateFmt('TMBASegment.AllocateAll: Some (%d) blocks are already allocated.',[fAllocationMap.PopCount]);
end;

//------------------------------------------------------------------------------

Function TMBASegment.IsFull: Boolean;
begin
Result := fAllocationMap.IsFull;
end;

//------------------------------------------------------------------------------

Function TMBASegment.IsEmpty: Boolean;
begin
Result := fAllocationMap.IsEmpty;
end;

//------------------------------------------------------------------------------

Function TMBASegment.AllocatedBlockCount: Integer;
begin
Result := fAllocationMap.PopCount;
end;

//------------------------------------------------------------------------------

Function TMBASegment.FreeBlockCount: Integer;
begin
Result := fAllocationMap.Count - fAllocationMap.PopCount;
end;

//------------------------------------------------------------------------------

Function TMBASegment.ReservedMemory: TMemSize;
begin
Result := TMemSize(fBlockCount) * fReservedBlockSize;
end;

//------------------------------------------------------------------------------

Function TMBASegment.BlocksMemory: TMemSize;
begin
Result := TMemSize(fBlockCount) * fSettings.BlockSize;
end;

//------------------------------------------------------------------------------

Function TMBASegment.AllocatedMemory: TMemSize;
begin
Result := TMemSize(AllocatedBlockCount) * fSettings.BlockSize;
end;

//------------------------------------------------------------------------------

Function TMBASegment.WastedMemory: TMemSize;
begin
Result := fMemorySize - BlocksMemory - uIfThen(fSettings.MapInSegment,fAllocationMap.MemorySize,0);
end;

//------------------------------------------------------------------------------

Function TMBASegment.MemoryEfficiency: Double;
begin
If fMemorySize <> 0 then
  Result := (fMemorySize - WastedMemory) / fMemorySize
else
  Result := 0.0;
end;

//------------------------------------------------------------------------------

Function TMBASegment.MemoryUtilization: Double;
begin
If BlocksMemory <> 0 then
  Result := AllocatedMemory / BlocksMemory
else
  Result := 0.0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TMBAAsyncFillThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMBAAsyncFillThread - class declaration
===============================================================================}
type
  TMBAAsyncFillThread = class(TThread)
  protected
    fAllocator:       TMassBlockAlloc;
    fTimeout:         UInt32;
    fPassExceptions:  Boolean;
    fCycleEvent:      TEvent;
    fTerminateFlag:   Integer;
    procedure Execute; override;
  public
    constructor Create(Allocator: TMassBlockAlloc);
    procedure FlagTerminate; virtual;
  end;

{===============================================================================
    TMBAAsyncFillThread - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMBAAsyncFillThread - protected methods
-------------------------------------------------------------------------------}

procedure TMBAAsyncFillThread.Execute;
begin
while InterlockedExchangeAdd(fTerminateFlag,0) = 0 do
  try
    If fCycleEvent.WaitFor(fTimeout) <> wrError then
      begin
        fAllocator.ThreadLockAcquire;
        try
          try
            fAllocator.InternalCacheFill(True);
          except
            on E: Exception do
              If fPassExceptions then
                begin
                  // prevent automatic destruction of current exception object
                  AcquireExceptionObject;
                  fAllocator.CacheExceptionsAdd(E);
                end;
          end;
        finally
          fAllocator.ThreadLockRelease;
        end;
      end;
  except
    // eat all exceptions, do not let them bubble out of the thread
  end;
end;

{-------------------------------------------------------------------------------
    TMBAAsyncFillThread - public methods
-------------------------------------------------------------------------------}

constructor TMBAAsyncFillThread.Create(Allocator: TMassBlockAlloc);
begin
inherited Create(False);
FreeOnTerminate := False;
Priority := tpLowest;
fAllocator := Allocator;
fTimeout := fAllocator.Settings.BlockCacheSettings.AsynchronousFill.CycleLength;
fPassExceptions := fAllocator.Settings.BlockCacheSettings.AsynchronousFill.PassExceptions;
fCycleEvent := fAllocator.fCache.AsyncFill.CycleEvent;
fTerminateFlag := 0;
end;

//------------------------------------------------------------------------------

procedure TMBAAsyncFillThread.FlagTerminate;
begin
InterlockedIncrement(fTerminateFlag);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TMassBlockAlloc                                                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMassBlockAlloc - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMassBlockAlloc - protected methods
-------------------------------------------------------------------------------}

Function TMassBlockAlloc.GetSegment(Index: Integer): TMBASegment;
begin
Result := nil;
ThreadLockAcquire;
try
  If CheckIndex(Index) then
    Result := fSegments[Index]
  else
    raise EMBAIndexOutOfBounds.CreateFmt('TMassBlockAlloc.GetSegment: Index (%d) out of bounds.',[Index]);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.GetCapacity: Integer;
begin
ThreadLockAcquire;
try
  Result := Length(fSegments);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.SetCapacity(Value: Integer);
begin
If Value >= 0 then
  begin
    ThreadLockAcquire;
    try
      If Value <> Length(fSegments) then
        begin
          If Value < fSegmentCount then
            raise EMBAInvalidAction.Create('TMassBlockAlloc.SetCapacity: Cannot lower capacity below count.');
          SetLength(fSegments,Value);
        end;
    finally
      ThreadLockRelease;
    end;
  end
else raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.GetCount: Integer;
begin
ThreadLockAcquire;
try
  Result := fSegmentCount;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMassBlockAlloc.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TMassBlockAlloc.AddSegment: Integer;
begin
Grow;
Result := fSegmentCount;
fSegments[Result] := TMBASegment.Create(fSettings.SegmentSettings);
Inc(fSegmentCount);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.DeleteSegment(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    fSegments[Index].Free;
    For i := Index to Pred(HighIndex) do
      fSegments[i] := fSegments[i + 1];
    fSegments[HighIndex] := nil;
    Dec(fSegmentCount);
    Shrink;
  end
else raise EMBAIndexOutOfBounds.CreateFmt('TMassBlockAlloc.DeleteSegment: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.ClearSegments;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  fSegments[i].Free;
SetLength(fSegments,0);
fSegmentCount := 0;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.Initialize(Settings: TMBAAllocatorSettings);
begin
// first do settings sanity checks
If Settings.BlockCacheSettings.Enable and (Settings.BlockCacheSettings.Length <= 0) then
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.Initialize: Cache length too small (%d).',[Settings.BlockCacheSettings.Length]);
If (Settings.SegmentSettings.BlockSize <= 0) or (Settings.SegmentSettings.BlockSize > MBA_MAX_BLK_SZ) then
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.Initialize: Invalid block size (%u).',[PtrInt(Settings.SegmentSettings.BlockSize)]);
case Settings.SegmentSettings.SizingStyle of
  szMinCount:
    If Settings.SegmentSettings.BlockCount <= 0 then
      raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.Initialize: Invalid block count (%d).',[Settings.SegmentSettings.BlockCount]);
  szMinSize,
  szExactSize:
    If (Settings.SegmentSettings.MemorySize <= 0) or (Settings.SegmentSettings.MemorySize > MBA_MAX_SEG_SZ) then
      raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.Initialize: Invalid memory size (%u).',[PtrInt(Settings.SegmentSettings.MemorySize)]);
else
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.Initialize: Unknown sizing style (%d).',[Ord(Settings.SegmentSettings.SizingStyle)]);
end;
fSettings := Settings;
fSegments := nil;
fSegmentCount := 0;
If fSettings.ThreadProtection then
  fThreadLock := SyncObjs.TCriticalSection.Create
else
  fThreadLock := nil;
// prepare cache
FillChar(fCache,SizeOf(fCache),0);
If fSettings.BlockCacheSettings.Enable then
  begin
    fCache.Enabled := True;
    SetLength(fCache.Data,fSettings.BlockCacheSettings.Length * 2);
    fCache.Count := 0;
    InternalCacheFill(False);
    If fSettings.BlockCacheSettings.AsynchronousFill.Enable and fSettings.ThreadProtection then
      begin
        // prepare asynchronous filling
        fCache.AsyncFill.Enabled := True;
        fCache.AsyncFill.Interrupts := fSettings.BlockCacheSettings.AsynchronousFill.Interruptable;
        fCache.AsyncFill.InterruptFlag := 0;
        fCache.AsyncFill.CycleEvent := TEvent.Create(nil,False,False,'');
        fCache.AsyncFill.FillerThread := TMBAAsyncFillThread.Create(Self);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.Finalize;
var
  i:  Integer;
begin
// check the objects, not only whether the async. fill is enabled (rollback in case of constructor exception)
If Assigned(fCache.AsyncFill.CycleEvent) and Assigned(fCache.AsyncFill.FillerThread) then
  begin
    // finalize asynch filling
    TMBAAsyncFillThread(fCache.AsyncFill.FillerThread).FlagTerminate;
    fCache.AsyncFill.CycleEvent.SetEvent;
    fCache.AsyncFill.FillerThread.WaitFor;
    FreeAndNil(fCache.AsyncFill.FillerThread);
    FreeAndNil(fCache.AsyncFill.CycleEvent);
    CacheExceptionsClear;
  end;
// empty cache and free remaining segments
ThreadLockAcquire;
try
  // there is no need to check whether cache is enabled
  For i := Low(fCache.Data) to Pred(fCache.Count) do
    try
      InternalFreeBlock(fCache.Data[i]);
    except
      // eat exceptions from invalid blocks, we are ending, so... \('_')/
    end;
  SetLength(fCache.Data,0);
  ClearSegments;
finally
  ThreadLockRelease;
end;
FreeAndNil(fThreadLock);
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.InternalCheckBlocks(var Blocks: array of Pointer; out FaultIndex: Integer): Boolean;
var
  i,j: Integer;
begin
Result := False;
FaultIndex := -1;
For i := Low(Blocks) to High(Blocks) do
  begin
    Result := False;
    For j := LowIndex to HighIndex do
      If fSegments[j].BlockOwned(Blocks[i]) then
        begin
          Result := True;
          Break{For j};
        end;
    If not Result then
      begin
        FaultIndex := i;
        Break{For i};
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.InternalAllocateBlock(out Block: Pointer; InitMemory: Boolean);
var
  i:  Integer;
begin
// first try to allocate in existing segments
For i := HighIndex downto LowIndex do
  If not fSegments[i].IsFull then
    begin
      fSegments[i].AllocateBlock(Block,InitMemory);
      Exit;
    end;
// no free block in existing segments, add new one
i := AddSegment;
fSegments[i].AllocateBlock(Block,InitMemory);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.InternalFreeBlock(var Block: Pointer);
var
  i:  Integer;
begin
For i := HighIndex downto LowIndex do
  If fSegments[i].BlockOwned(Block) then
    begin
      fSegments[i].FreeBlock(Block);
      If fSegments[i].IsEmpty and fSettings.FreeEmptySegments then
        DeleteSegment(i);
      Exit;
    end;
raise EMBAInvalidAddress.CreateFmt('TMassBlockAlloc.InternalFreeBlock: Unable to free block (%p).',[Block]);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.InternalAllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean);
var
  Index:  Integer;
  i:      Integer;

  procedure AllocateFromSegment(SegmentIndex: Integer);
  var
    Burst:  TMBAPointerArray;
    j:      Integer;
  begin
    If ((Length(Blocks) - Index) >= fSegments[SegmentIndex].BlockCount) and fSegments[SegmentIndex].IsEmpty then
      begin
        // burst allocation
        fSegments[SegmentIndex].AllocateAll(Burst,InitMemory);
        For j := Low(Burst) to High(Burst) do
           Blocks[Index + j] := Burst[j];
        Inc(Index,Length(Burst));
      end
    else while (Index <= High(Blocks)) and not fSegments[SegmentIndex].IsFull do
      begin
        fSegments[SegmentIndex].AllocateBlock(Blocks[Index],InitMemory);
        Inc(Index);
      end;
  end;

begin
Index := Low(Blocks);
For i := HighIndex downto LowIndex do
  begin
    AllocateFromSegment(i);
    If Index > High(Blocks) then
      Exit;
  end;
while Index <= High(Blocks) do
  begin
    i := AddSegment;
    AllocateFromSegment(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.InternalFreeBlocks(var Blocks: array of Pointer);
var
  Index,i:  Integer;
  Counter:  Integer;
begin
If not InternalCheckBlocks(Blocks,Index) then
  raise EMBAInvalidAddress.CreateFmt('TMassBlockAlloc.InternalAllocateBlocks: Invalid block address ([%d]%p).',[Index,Blocks[Index]]);
Counter := Length(Blocks);
For i := HighIndex downto LowIndex do
  begin
    For Index := Low(Blocks) to High(Blocks) do
      If Assigned(Blocks[Index]) and fSegments[i].BlockOwned(Blocks[Index]) then
        begin
          fSegments[i].FreeBlock(Blocks[Index]);  // sets the pointer to nil
          Dec(Counter);
          If fSegments[i].IsEmpty then
            begin
              If fSettings.FreeEmptySegments then
                DeleteSegment(i);
              Break{For Index};
            end;
          If Counter <= 0 then
            Exit;
        end;
  end;
// final paranoia check
If Counter > 0 then
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.InternalFreeBlocks: Not all blocks were freed (%d).',[Counter]);
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.InternalCacheFill(AsyncFill: Boolean): Integer;
var
  i:  Integer;
begin
Result := 0;
// thread lock must be acquired before calling this function
If fCache.Count < (Length(fCache.Data) shr 1) then
  begin
    // cache is underfilled
    For i := fCache.Count to Pred(Length(fCache.Data) div 2) do
      begin
        If AsyncFill and fCache.AsyncFill.Interrupts then
          If InterlockedExchangeAdd(fCache.AsyncFill.InterruptFlag,0) <> 0 then
            Exit; // interrupt filling
        InternalAllocateBlock(fCache.Data[i],False);
        Inc(fCache.Count);
        Inc(Result);
      end;
  end
else If fCache.Count > (Length(fCache.Data) shr 1) then
  begin
    // cache is overfilled
    For i := Pred(fCache.Count) downto (Length(fCache.Data) div 2) do
      begin
        If AsyncFill and fCache.AsyncFill.Interrupts then
          If InterlockedExchangeAdd(fCache.AsyncFill.InterruptFlag,0) <> 0 then
            Exit;
        try
          InternalFreeBlock(fCache.Data[i]);
        finally
          // always remove the block, even when it raises an exception
          Dec(fCache.Count);
          Dec(Result);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheExceptionsAdd(ExceptObject: TObject);
begin
      writeln( fCache.AsyncFill.Exceptions.start,' ', fCache.AsyncFill.Exceptions.count);
// thread lock is expected to be already acquired
with fCache.AsyncFill.Exceptions do
  If Count >= Length(Objects) then
    begin
      FreeAndNil(Objects[Start]);
      Objects[Start] := ExceptObject;
      Start := Succ(Start) mod Length(Objects);
    end
  else
    begin
      Objects[(Start + Count) mod Length(Objects)] := ExceptObject;
      Inc(Count);
    end;
end;

{-------------------------------------------------------------------------------
    TMassBlockAlloc - public methods
-------------------------------------------------------------------------------}

constructor TMassBlockAlloc.Create(Settings: TMBAAllocatorSettings);
begin
inherited Create;
Initialize(Settings);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMassBlockAlloc.Create(BlockSize: TMemSize; MinBlocksPerSegment: Integer; MemoryAlignment: TMemoryAlignment = maNone);
var
  TempSettings: TMBAAllocatorSettings;
begin
TempSettings := DefaultAllocatorSettings;
TempSettings.SegmentSettings.BlockSize := BlockSize;
TempSettings.SegmentSettings.Alignment := MemoryAlignment;
TempSettings.SegmentSettings.SizingStyle := szMinCount;
TempSettings.SegmentSettings.BlockCount := MinBlocksPerSegment;
Create(TempSettings);
end;

//------------------------------------------------------------------------------

destructor TMassBlockAlloc.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.LowIndex: Integer;
begin
ThreadLockAcquire;
try
  Result := Low(fSegments);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.HighIndex: Integer;
begin
ThreadLockAcquire;
try
  Result := Pred(fSegmentCount);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.ThreadLockAcquire;
begin
If Assigned(fThreadLock) then
  fThreadLock.Enter;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.ThreadLockRelease;
begin
If Assigned(fThreadLock) then
  fThreadLock.Leave;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocationAcquire;
begin
If Assigned(fThreadLock) then
  begin
    If fCache.AsyncFill.Interrupts then
      InterlockedIncrement(fCache.AsyncFill.InterruptFlag);
    fThreadLock.Enter;
  end;
end;


//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocationRelease;
begin
If Assigned(fThreadLock) then
  begin
    fThreadLock.Leave;
    If fCache.AsyncFill.Interrupts then
      InterlockedDecrement(fCache.AsyncFill.InterruptFlag);
  end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.ValidateBlock(Block: Pointer; out ValidationInfo: TMBAValidationInfo);
var
  i,Index:  Integer;
begin
// init output
ValidationInfo.Address := Block;
ValidationInfo.Owned := False;
ValidationInfo.Allocated := False;
ValidationInfo.SegmentIndex := -1;
ValidationInfo.SegmentObject := nil;
ValidationInfo.BlockIndex := -1;
// and now the fun part...
For i := LowIndex to HighIndex do
  begin
    If fSegments[i].BlockFind(Block,Index) then
      begin
        ValidationInfo.Owned := True;
        ValidationInfo.Allocated := fSegments[i].AllocationMap[Index];
        ValidationInfo.SegmentIndex := i;
        ValidationInfo.SegmentObject := fSegments[i];
        ValidationInfo.BlockIndex := Index;
        Break{For i};
      end
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMassBlockAlloc.ValidateBlock(Block: Pointer): Boolean;
var
  ValInfo:  TMBAValidationInfo;
begin
ValidateBlock(Block,ValInfo);
Result := ValInfo.Owned and ValInfo.Allocated;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.ValidateBlocks(const Blocks: array of Pointer; out ValidationInfo: TMBAValidationInfoArray);
var
  i,j:    Integer;
  Index:  Integer;
begin
ValidationInfo := nil;
SetLength(ValidationInfo,Length(Blocks));
For i := Low(Blocks) to High(Blocks) do
  begin
    ValidationInfo[i].Address := Blocks[i];
    For j := LowIndex to HighIndex do
      begin
        If fSegments[i].BlockFind(Blocks[i],Index) then
          begin
            ValidationInfo[i].Owned := True;
            ValidationInfo[i].Allocated := fSegments[j].AllocationMap[Index];
            ValidationInfo[i].SegmentIndex := j;
            ValidationInfo[i].SegmentObject := fSegments[j];
            ValidationInfo[i].BlockIndex := Index;
            Break{For j};
          end;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMassBlockAlloc.ValidateBlocks(const Blocks: array of Pointer): Boolean;
var
  ValInfo:  TMBAValidationInfoArray;
  i:        Integer;
begin
If Length(Blocks) > 0 then
  begin
    ValidateBlocks(Blocks,ValInfo);
    Result := True;
    For i := Low(ValInfo) to High(ValInfo) do
      If not(ValInfo[i].Owned and ValInfo[i].Allocated) then
        begin
          Result := False;
          Break{For i};
        end;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocateBlock(out Block: Pointer; InitMemory: Boolean = False);
begin
AllocationAcquire;
try
  InternalAllocateBlock(Block,InitMemory);
finally
  AllocationRelease;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMassBlockAlloc.AllocateBlock(InitMemory: Boolean = False): Pointer;
begin
AllocateBlock(Result,InitMemory);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.FreeBlock(var Block: Pointer);
begin
AllocationAcquire;
try
  InternalFreeBlock(Block);
finally
  AllocationRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean = False);
begin
If Length(Blocks) > 0 then
  begin
    AllocationAcquire;
    try
      InternalAllocateBlocks(Blocks,InitMemory);
    finally
      AllocationRelease;
    end;
  end;    
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.FreeBlocks(var Blocks: array of Pointer);
begin
If Length(Blocks) > 0 then
  begin
    AllocationAcquire;
    try
      InternalFreeBlocks(Blocks);
    finally
      AllocationRelease;
    end;
  end;    
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocateBuffer(out Buffer: Pointer; BufferSize: TMemSize; InitMemory: Boolean = False);
var
  i:  Integer;
begin
If BufferSize > 0 then
  begin
    AllocationAcquire;
    try
      // we need at least one segment to exist for checks
      If fSegmentCount <= 0 then
        AddSegment;
      If fSegments[LowIndex].BufferBlockCount(BufferSize) > fSegments[LowIndex].BlockCount then
        raise EMBAOutOfResources.CreateFmt('TMassBlockAlloc.AllocateBuffer: Buffer is too large (%u).',[PtrInt(BufferSize)]);
      // try to alocate it somewhere
      For i := HighIndex downto LowIndex do
        If fSegments[i].TryAllocateBuffer(Buffer,BufferSize,InitMemory) then
          Exit;
      // buffer not allocated in existing segments, add a new one and allocate there
      i := AddSegment;
      fSegments[i].AllocateBuffer(Buffer,BufferSize,InitMemory);
    finally
      AllocationRelease;
    end;
  end
else raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.AllocateBuffer: Invalid buffer size (%u).',[PtrInt(BufferSize)]);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.FreeBuffer(var Buffer: Pointer; BufferSize: TMemSize);
var
  i:  Integer;
begin
If BufferSize > 0 then
  begin
    AllocationAcquire;
    try
      // note that buffer size is checked by the segment (FreeBuffer)
      For i := HighIndex downto LowIndex do
        If fSegments[i].BlockOwned(Buffer) then
          begin
            fSegments[i].FreeBuffer(Buffer,BufferSize);
            If fSegments[i].IsEmpty and fSettings.FreeEmptySegments then
              DeleteSegment(i);
            Exit;
          end;
      raise EMBAInvalidAddress.CreateFmt('TMassBlockAlloc.FreeBuffer: Unable to free buffer (%p).',[Buffer]);
    finally
      AllocationRelease;
    end;
  end
else raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.FreeBuffer: Invalid buffer size (%u).',[PtrInt(BufferSize)]);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.AllocateBlockVector(out Vector: Pointer; VectorLength: Integer; InitMemory: Boolean = False);
begin
If VectorLength > 0 then
  AllocateBuffer(Vector,TMemSize(VectorLength) * fSettings.SegmentSettings.BlockSize,InitMemory)
else
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.AllocateBlockVector: Invalid vector length (%d).',[VectorLength]);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.FreeBlockVector(var Vector: Pointer; VectorLength: Integer);
begin
If VectorLength > 0 then
  FreeBuffer(Vector,TMemSize(VectorLength) * fSettings.SegmentSettings.BlockSize)
else
  raise EMBAInvalidValue.CreateFmt('TMassBlockAlloc.FreeBlockVector: Invalid vector length (%d).',[VectorLength]);
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.CacheCount: Integer;
begin
ThreadLockAcquire;
try
  Result := fCache.Count;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheFill(ForceSynchronous: Boolean = False);
begin
If fCache.Enabled then
  begin
    If ForceSynchronous or not fCache.AsyncFill.Enabled then
      begin
        ThreadLockAcquire;
        try
          InternalCacheFill(False);
        finally
          ThreadLockRelease;
        end;
      end
    else fCache.AsyncFill.CycleEvent.SetEvent;
  end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.CacheExceptionsCount: Integer;
begin
ThreadLockAcquire;
try
  Result := fCache.AsyncFill.Exceptions.Count;
finally
  ThreadLockRelease;
end;  
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.CacheExceptionsPop: TObject;
begin
ThreadLockAcquire;
try
  with fCache.AsyncFill.Exceptions do
    If Count > 0 then
      begin
        Result := Objects[Start];
        Objects[Start] := nil;
        Dec(Count);
        Start := Succ(Start) mod Length(Objects);
      end
    else Result := nil;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheExceptionsRaise;
var
  ExceptObject: TObject;
begin
// no need to do locking here as we are not directly accesing internal state
ExceptObject := CacheExceptionsPop;
If Assigned(ExceptObject) then
  raise ExceptObject;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheExceptionsClear;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  with fCache.AsyncFill.Exceptions do
    begin
      For i := 0 to Pred(Count) do
        FreeAndNil(Objects[(Start + i) mod Length(Objects)]);
      Count := 0;
      Start := 0;
    end;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheAllocateBlock(out Block: Pointer; InitMemory: Boolean = False);
begin
AllocationAcquire;
try
  If fCache.Count > 0 then
    begin
      Block := fCache.Data[Pred(fCache.Count)];
      fCache.Data[Pred(fCache.Count)] := nil;
      Dec(fCache.Count);
      If InitMemory then
        FillChar(Block^,fSegments[LowIndex].ReservedBlockSize,0);
    end
  else InternalAllocateBlock(Block,InitMemory);
finally
  AllocationRelease;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMassBlockAlloc.CacheAllocateBlock(InitMemory: Boolean = False): Pointer;
begin
CacheAllocateBlock(Result,InitMemory);
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheFreeBlock(var Block: Pointer);

  Function CheckBlockValidity: Boolean;
  var
    i:  Integer;
  begin
    Result := False;
    For i := LowIndex to HighIndex do
      If fSegments[i].BlockOwned(Block) then
        begin
          Result := True;
          Break{For ii};
        end;
  end;

begin
AllocationAcquire;
try
  If fCache.Count < Length(fCache.Data) then
    begin
      If not fSettings.BlockCacheSettings.TrustedReturns then
        If not CheckBlockValidity then
          raise EMBAInvalidAddress.CreateFmt('TMassBlockAlloc.CacheFreeBlock: Invalid block address (%p).',[Block]);
      fCache.Data[fCache.Count] := Block;
      Inc(fCache.Count);
      Block := nil;
    end
  else InternalFreeBlock(Block);
finally
  AllocationRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheAllocateBlocks(out Blocks: array of Pointer; InitMemory: Boolean = False);
var
  Index:      Integer;
  TempBlocks: array of Pointer;
  i:          Integer;
begin
If Length(Blocks) > 0 then
  begin
    If fCache.Enabled then
      begin
        AllocationAcquire;
        try
          Index := Low(Blocks);
          while fCache.Count > 0 do
            begin
              Blocks[Index] := fCache.Data[Pred(fCache.Count)];
              fCache.Data[Pred(fCache.Count)] := nil;
              Dec(fCache.Count);
              If InitMemory then
                FillChar(Blocks[Index]^,fSegments[LowIndex].ReservedBlockSize,0);
              Inc(Index);
              If Index > High(Blocks) then
                Break{while};
            end;
          If Index < High(Blocks) then
            begin
              // more than one block needs to be allocated
              TempBlocks := nil;
              SetLength(TempBlocks,Length(Blocks) - Index);
              InternalAllocateBlocks(TempBlocks,InitMemory);
              For i := Low(TempBlocks) to High(TempBlocks) do
                Blocks[Index + i] := TempBlocks[i];
            end
          else If Index = High(Blocks) then
            // only one block needs to be allocated
            InternalAllocateBlock(Blocks[Index],InitMemory);
        finally
          AllocationRelease;
        end;
      end
    else AllocateBlocks(Blocks,InitMemory);
  end
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.CacheFreeBlocks(var Blocks: array of Pointer);
var
  Index:      Integer;
  TempBlocks: array of Pointer;
  i:          Integer;
begin
If Length(Blocks) > 0 then
  begin
    If fCache.Enabled then
      begin
        AllocationAcquire;
        try
          If not fSettings.BlockCacheSettings.TrustedReturns then
            If not InternalCheckBlocks(Blocks,Index) then
              raise EMBAInvalidAddress.CreateFmt('TMassBlockAlloc.CacheFreeBlocks: Invalid block address ([%d]%p).',[Index,Blocks[Index]]);
          Index := Low(Blocks);
          while fCache.Count < Length(fCache.Data) do
            begin
              fCache.Data[fCache.Count] := Blocks[Index];
              Blocks[Index] := nil;
              Inc(fCache.Count);
              Inc(Index);
              If Index > High(Blocks) then
                Break{while};
            end;
          If Index < High(Blocks) then
            begin
              // several blocks needs to be freed
              TempBlocks := nil;
              SetLength(TempBlocks,Length(Blocks) - Index);
              For i := Low(TempBlocks) to High(TempBlocks) do
                begin
                  TempBlocks[i] := Blocks[Index + i];
                  Blocks[Index + i] := nil;
                end;
              InternalFreeBlocks(TempBlocks);
            end
          else If Index = High(Blocks) then
            // only one block needs to be freed
            InternalFreeBlock(Blocks[Index]);
        finally
          AllocationRelease;
        end;
      end
    else FreeBlocks(Blocks);
  end
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.PrepareFor(Count: Integer): Integer;
var
  FreeBlocksCnt:  Integer;
  SegmentIndex:   Integer;
begin
ThreadLockAcquire;
try
  FreeBlocksCnt := FreeBlockCount;
  while FreeBlocksCnt < Count do
    begin
      SegmentIndex := AddSegment;
      Inc(FreeBlocksCnt,fSegments[SegmentIndex].BlockCount);
    end;
  Result := FreeBlocksCnt;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

procedure TMassBlockAlloc.BurstAllocateBlocks(out Blocks: TMBAPointerArray; InitMemory: Boolean = False);
var
  i:  Integer;
begin
AllocationAcquire;
try
  // first look if there is some empty segment already present
  For i := HighIndex downto LowIndex do
    If fSegments[i].IsEmpty then
      begin
        fSegments[i].AllocateAll(Blocks,InitMemory);
        Exit;
      end;
  // if here, there was no empty segment, so create one and allocate there
  i := AddSegment;
  fSegments[i].AllocateAll(Blocks,InitMemory);
finally
  AllocationRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.SegmentSize: TMemSize;
begin
ThreadLockAcquire;
try
  If fSegmentCount <= 0 then
    AddSegment;
  Result := fSegments[LowIndex].MemorySize;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.BlocksPerSegment: Integer;
begin
ThreadLockAcquire;
try
  If fSegmentCount <= 0 then
    AddSegment;
  Result := fSegments[LowIndex].BlockCount;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.AllocatedBlockCount: Integer;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].AllocatedBlockCount);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.FreeBlockCount: Integer;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].FreeBlockCount);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.TotalReservedMemory: TMemSize;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].ReservedMemory);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.TotalBlocksMemory: TMemSize;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].BlocksMemory);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.TotalAllocatedMemory: TMemSize;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].AllocatedMemory);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.TotalWastedMemory: TMemSize;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].WastedMemory);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.TotalMemorySize: TMemSize;
var
  i:  Integer;
begin
ThreadLockAcquire;
try
  Result := 0;
  For i := LowIndex to HighIndex do
    Inc(Result,fSegments[i].MemorySize);
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.MemoryEfficiency: Double;
var
  MemorySize: TMemSize;
begin
ThreadLockAcquire;
try
  MemorySize := TotalMemorySize;
  If MemorySize <> 0 then
    Result := (MemorySize - TotalWastedMemory) / MemorySize
  else
    Result := 0.0;
finally
  ThreadLockRelease;
end;
end;

//------------------------------------------------------------------------------

Function TMassBlockAlloc.MemoryUtilization: Double;
var
  BlocksMemory: TMemSize;
begin
ThreadLockAcquire;
try
  BlocksMemory := TotalBlocksMemory;
  If BlocksMemory <> 0 then
    Result := TotalAllocatedMemory / BlocksMemory
  else
    Result := 0.0;
finally
  ThreadLockRelease;
end;
end;

end.
