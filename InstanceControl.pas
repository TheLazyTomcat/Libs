{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Instance control

    Provides a class TInstanceControl that is designed to be used for
    system-wide tracking of number of created instances of itself.
    It is typically used to limit number of running instances of a program.

    In essence, it is just a counter stored in a named shared memory. Name
    of this memory corresponds to an identifier given during creation - this
    means that, as long as you use the same identifier, you will be accessing
    the same counter. The identifier is case-insensitive.
    The counter is incremented when an instance of TInstanceControl is created
    and decremented when it is freed.

    Typical use is to create an object of TInstanceControl class, checking
    the InitialCount property and to continue if it is low enough, or aborting
    further operation if is is too high. But note that the counter does not
    have any inherent meaning assigned, it is on you how to interpret it.

  Version 1.0.1 (2022-10-25)

  Last change 2022-10-25

  ©2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.InstanceControl

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
  * SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex

  Some libraries might not be needed, depending on targer operating system and
  other settings, see library SharedMemoryStream for details.

===============================================================================}
unit InstanceControl;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  AuxTypes, AuxClasses, SharedMemoryStream;

{===============================================================================
--------------------------------------------------------------------------------
                                TInstanceControl
--------------------------------------------------------------------------------
===============================================================================}
type
  TICLockedReactionEvent = Function(Sender: TObject): Boolean of object;
  TICLockedReactionCallback = Function(Sender: TObject): Boolean;

{===============================================================================
    TInstanceControl - class declaration
===============================================================================}
type
  TInstanceControl = class(TCustomObject)
  protected
    fSection:     TSharedMemory;
    fInitCount:   UInt32;
    fProcCounter: Boolean;
    // getters, setters
    Function GetIdentifier: String; virtual;
    Function GetInstanceCount: UInt32; virtual;
    Function GetCreatorPID: UInt32; virtual;
    Function GetCreationTime: TDateTime; virtual;
    Function GetLastAccessTime: TDateTime; virtual;
    Function GetSharedUserDataSize: TMemSize; virtual;
    Function GetSharedUserDataPtr: Pointer; virtual;
    Function CurrentProcessID: UInt32; virtual;
    // object init/final
    procedure Initialize(const Identifier: String; LREvent: TICLockedReactionEvent; LRCallback: TICLockedReactionCallback); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(const Identifier: String); overload;
  {
    Use following constructors and LockedReaction event/callback to react to
    instance counter while it is still inside locked section during creation.

    It is intended for situations where it is desirable to conditionaly prevent
    counter increment during construction.
    If you return True from LockedReaction then the creation will continue as
    usual.
    But if you return false, then counter will not be incremented (and also
    decremented when you destroy the object later) - note that creator pid and
    other fields will also stay unaffected as if nothing happened.

      WARNING - do NOT free/destroy the object from within the LockedReaction.
  }
    constructor Create(const Identifier: String; LockedReaction: TICLockedReactionEvent); overload;
    constructor Create(const Identifier: String; LockedReaction: TICLockedReactionCallback); overload;
    destructor Destroy; override;
  {
    LockSharedMemory and UnlockSharedMemory can be used to protect access to
    shared user data.
    But note that the same lock is also used to protect the internal data.
    This means, among others, that any internal access to the shared data,
    including creation of new TInstanceControl object, will block until the
    lock is released.
  }
    procedure LockSharedMemory; virtual;
    procedure UnlockSharedMemory; virtual;
  {
    Following two methods can be used to explicitly manipulate the instance
    counter, use them with caution.

    Returned value is the counter before the operation.
  }
    Function IncrementCount: UInt32; virtual;
    Function DecrementCount: UInt32; virtual;
  {
    Identifier given during creation - note that might slightly differ from
    string passed to constructor, since it can, and usually will, be rectified
    for use as a name of internally used mapped memory.
  }
    property Identifier: String read GetIdentifier;
  {
    Properties InstanceCount through LastAccessTime are directly accessing data
    stored in shared memory, and this access is protected by a lock (so it is
    thread safe) - but note that whatever is returned is a value that WAS
    stored during the lock, it might have been changed by the time the property
    getter returns.

    InstanceCount contains number of existing instances of TInstanceControl
    with givent identifier within the system.

    CreatorPID contains process id of process that first created instance
    control object with the given identifier.

      Note that if CreatorID is UInt32(-1), it means the process no longer
      exists.

    CreationTime is time when the instance control with given identifier was
    firstly created.

    LastAccessTime contains time at which last instance control with given
    identifier was created.
  }
    property InstanceCount: UInt32 read GetInstanceCount;
    property CreatorPID: UInt32 read GetCreatorPID;
    property CreationTime: TDateTime read GetCreationTime;
    property LastAccessTime: TDateTime read GetLastAccessTime;
  {
    InitialCount contains number of instances with a given identifier that were
    present at the creation of this object (before incrementing the counter).

    It is recommended to use only this value when discerning whether to allow
    current instance to continue or not.
  }
    property InitialCount: Uint32 read fInitCount;
  {
    You can use shared data to store anything you want to be wisible to all
    instance controls with given identifier.
  }
    property SharedUserDataSize: TMemSize read GetSharedUserDataSize;
    property SharedUserDataPtr: Pointer read GetSharedUserDataPtr;
  end;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}BaseUnix,{$ENDIF} SysUtils;

{===============================================================================
--------------------------------------------------------------------------------
                                TInstanceControl
--------------------------------------------------------------------------------
===============================================================================}
const
  IC_SHAREDMEM_SIZE = 4 * 1024;  // 4KiB, ie. usuall size of memory page

type
  TICSharedData = packed record
    InstanceCount:  UInt32;
    CreatorPID:     UInt32;
    CreationTime:   TDateTime;
    LastAccessTime: TDateTime;
    Filler:         array[0..39] of Byte;
    UserData:       record end; // zero-size field
  end;
  PICSharedData = ^TICSharedData;

{===============================================================================
    TInstanceControl - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TInstanceControl - protected methods
-------------------------------------------------------------------------------}

Function TInstanceControl.GetIdentifier: String;
begin
Result := fSection.Name;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetInstanceCount: UInt32;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.InstanceCount;
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetCreatorPID: UInt32;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.CreatorPID;
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetCreationTime: TDateTime;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.CreationTime;
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetLastAccessTime: TDateTime;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.LastAccessTime;
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetSharedUserDataSize: TMemSize;
begin
Result := fSection.Size - SizeOf(TICSharedData);
end;

//------------------------------------------------------------------------------

Function TInstanceControl.GetSharedUserDataPtr: Pointer;
begin
fSection.Lock;
try
  Result := Addr(PICSharedData(fSection.Memory)^.UserData);
finally
  fSection.Unlock;
end;
end;
 
//------------------------------------------------------------------------------

Function TInstanceControl.CurrentProcessID: UInt32;
begin
Result := UInt32({$IFDEF Windows}GetCurrentProcessID{$ELSE}fpgetpid{$ENDIF});
end;

//------------------------------------------------------------------------------

procedure TInstanceControl.Initialize(const Identifier: String; LREvent: TICLockedReactionEvent; LRCallback: TICLockedReactionCallback);
var
  SharedDataPtr:  PICSharedData;
begin
fSection := TSharedMemory.Create(IC_SHAREDMEM_SIZE,Identifier);
fSection.Lock;
try
  SharedDataPtr := fSection.Memory;
  fInitCount := SharedDataPtr^.InstanceCount;
  If Assigned(LREvent) then
    fProcCounter := LREvent(Self)
  else If Assigned(LRCallback) then
    fProcCounter := LRCallback(Self)
  else
    fProcCounter := True;
  If fProcCounter then
    begin
      Inc(SharedDataPtr^.InstanceCount);
      SharedDataPtr^.LastAccessTime := Now;
      If PICSharedData(fSection.Memory)^.InstanceCount = 1 then
        begin
          SharedDataPtr^.CreatorPID := CurrentProcessID;
          SharedDataPtr^.CreationTime := SharedDataPtr^.LastAccessTime;
        end;
    end;
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TInstanceControl.Finalize;
var
  SharedDataPtr:  PICSharedData;
begin
If Assigned(fSection) then
  begin
    fSection.Lock;
    try
      SharedDataPtr := fSection.Memory;
      If fProcCounter then
        begin
          Dec(SharedDataPtr^.InstanceCount);
          If SharedDataPtr^.CreatorPID = CurrentProcessID then
            SharedDataPtr^.CreatorPID := UInt32(-1);
        end;
    finally
      fSection.Unlock;
    end;
  end;
fSection.Free;
end;

{-------------------------------------------------------------------------------
    TInstanceControl - public methods
-------------------------------------------------------------------------------}

constructor TInstanceControl.Create(const Identifier: String);
begin
inherited Create;
Initialize(Identifier,nil,nil);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TInstanceControl.Create(const Identifier: String; LockedReaction: TICLockedReactionEvent);
begin
inherited Create;
Initialize(Identifier,LockedReaction,nil);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TInstanceControl.Create(const Identifier: String; LockedReaction: TICLockedReactionCallback);
begin
inherited Create;
Initialize(Identifier,nil,LockedReaction);
end;

//------------------------------------------------------------------------------

destructor TInstanceControl.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TInstanceControl.LockSharedMemory;
begin
fSection.Lock;
end;

//------------------------------------------------------------------------------

procedure TInstanceControl.UnlockSharedMemory;
begin
fSection.Unlock;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.IncrementCount: UInt32;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.InstanceCount;
  Inc(PICSharedData(fSection.Memory)^.InstanceCount);
finally
  fSection.Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TInstanceControl.DecrementCount: UInt32;
begin
fSection.Lock;
try
  Result := PICSharedData(fSection.Memory)^.InstanceCount;
  Dec(PICSharedData(fSection.Memory)^.InstanceCount);
finally
  fSection.Unlock;
end;
end;

end.
