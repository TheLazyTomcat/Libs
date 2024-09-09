{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SyncThread

    Provides a mean of synchronizing methods between any two cooperating
    threads. Synchronization here means running a method of synchronizing
    thread within a context of different (target) thread, and waiting for it
    to finish.

    To synchronize between any threads, use class TSyncThreadSynchronizer.
    Both threads must have an instance of this class.

      The synchronizing thread will use method Synchronize, giving a method to
      be synchronized and a target (synchronizer running within a thread you
      are synchronizing with).

      The thread you are sychnronizing with must repeatedly call method
      DoSynchronization of its synchronizer, as the synchronization itself is
      done inside of this method.

    To simplify, you can use provided class TSyncThread (a descendant of
    TThread). It provides the same methods a synchronizer (which is internally
    used anyway), so the process is the same (remember to call DoSynchronization
    in a thread you are synchronizing with).

      WARNING - methods DoSynchronization are synchronous, meaning they will
                not return until a synchronization occurs or a timeout elapses.

    Note that you cannot call method Synchronize from within a method that is
    currently being synchronized. This is to prevent deadlocks.
    
  Version 2.0.1 (2024-05-03)

  Last change 2024-09-09

  ©2018-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SyncThread

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    Messanger     - github.com/TheLazyTomcat/Lib.Messanger

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SyncThread_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    BitOps              - github.com/TheLazyTomcat/Lib.BitOps
    BitVector           - github.com/TheLazyTomcat/Lib.BitVector
    CrossSyncObjs       - github.com/TheLazyTomcat/Lib.CrossSyncObjs
    HashBase            - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps      - github.com/TheLazyTomcat/Lib.InterlockedOps
    LinSyncObjs         - github.com/TheLazyTomcat/Lib.LinSyncObjs
    ListSorters         - github.com/TheLazyTomcat/Lib.ListSorters
    MemVector           - github.com/TheLazyTomcat/Lib.MemVector
    NamedSharedItems    - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1                - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream  - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
    SimpleFutex         - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream  - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo         - github.com/TheLazyTomcat/Lib.WinFileInfo
    WinSyncObjs         - github.com/TheLazyTomcat/Lib.WinSyncObjs

===============================================================================}
unit SyncThread;
{
  SyncThread_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SyncThread_UseAuxExceptions to achieve this.
}
{$IF Defined(SyncThread_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

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
  AuxTypes, AuxClasses, Messanger{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exception
===============================================================================}
type
  ESTException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESTNoGlobalManager   = class(ESTException);
  ESTCannotSynchronize = class(ESTException);

{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadSynchronizer - class declaration
===============================================================================}
type
  TSyncThreadSynchronizer = class(TCustomObject)
  protected
    fEndpoint:      TMessangerEndpoint;
    fSynchronizing: Boolean;
    procedure MessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags); virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod; Target: TSyncThreadSynchronizer); virtual;
    procedure DoSynchronization(Timeout: UInt32 = 0); virtual;
    property Synchronizing: Boolean read fSynchronizing;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSyncThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThread - class declaration
===============================================================================}
type
  TSyncThread = class(TThread)
  protected
    fSynchronizer: TSyncThreadSynchronizer;
    Function GetSynchronizing: Boolean; virtual;
    procedure Synchronize(Method: TThreadMethod; Target: TSyncThreadSynchronizer); overload; virtual;
    procedure Synchronize(Method: TThreadMethod; Target: TSyncThread); overload; virtual;
    procedure DoSynchronization(Timeout: UInt32 = 0); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property Synchronizing: Boolean read GetSynchronizing;
  end;

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Internal constants
===============================================================================}

const
  ST_SYNC_MESSAGE_ID = $51235707; // just a random number

{===============================================================================
    Global messanger management
===============================================================================}

var
  GlobalMessanger: TMessanger = nil;

//------------------------------------------------------------------------------

Function CreateEndpoint: TMessangerEndpoint;
begin
If Assigned(GlobalMessanger) then
  Result := GlobalMessanger.CreateEndpoint
else
  raise ESTNoGlobalManager.Create('CreateEndpoint: There is  no global SyncThread manager.');
end;

//------------------------------------------------------------------------------

procedure DestroyEndpoint(Endpoint: TMessangerEndpoint);
begin
If Assigned(GlobalMessanger) then
  Endpoint.Free
else
  raise ESTNoGlobalManager.Create('DestroyEndpoint: There is  no global SyncThread manager.');
end;

//------------------------------------------------------------------------------

procedure UnitInitialization;
begin
GlobalMessanger := TMessanger.Create(1024);
end;

//------------------------------------------------------------------------------

procedure UnitFinalization;
begin
try
{
  Following can fail. If it does, leave it hanging - the unit is being
  finalized anyway, meaning the program is ending and potential memory leak
  should not pose any problem.
}
  GlobalMessanger.Free;
except
  // eat the possible exception
  on E: EMsgrInvalidState do
    // nothing
  else
    raise;  // re-raise other excetions
end;
GlobalMessanger := nil;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TSyncThreadSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThreadSynchronizer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSyncThreadSynchronizer - protected methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W4055 W5024{$ENDIF}
procedure TSyncThreadSynchronizer.MessageHandler(Sender: TObject; Msg: TMsgrMessageIn; var Flags: TMsgrDispatchFlags);
var
  Method: TMethod;
begin
If (Msg.Parameter1 = ST_SYNC_MESSAGE_ID) and (mdfSentMessage in Flags) then
  begin
    // get code and data of the synchronized method from message params
    Method.Code := Pointer(Msg.Parameter2);
    Method.Data := Pointer(Msg.Parameter3);
    // execute the method in current context
    try
      TThreadMethod(Method);
    except
      PPointer(Msg.Parameter4)^ := AcquireExceptionObject;
    end;
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSyncThreadSynchronizer.Initialize;
begin
fEndpoint := CreateEndpoint;
fEndpoint.OnMessage := MessageHandler;
fSynchronizing := False;
end;

//------------------------------------------------------------------------------

procedure TSyncThreadSynchronizer.Finalize;
begin
fEndpoint.Free;
end;

{-------------------------------------------------------------------------------
    TSyncThreadSynchronizer - public methods
-------------------------------------------------------------------------------}

constructor TSyncThreadSynchronizer.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSyncThreadSynchronizer.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSyncThreadSynchronizer.Synchronize(Method: TThreadMethod; Target: TSyncThreadSynchronizer);
var
  SyncException:  Exception;
begin
If not fSynchronizing then
  begin
    SyncException := nil;
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    // following call will block until the target is done processing it
    fEndpoint.SendMessage(Target.fEndpoint.EndpointID,
                          ST_SYNC_MESSAGE_ID,
                          TMSGRParam(TMethod(Method).Code),
                          TMSGRParam(TMethod(Method).Data),
                          TMSGRParam(@SyncException));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    // check if exception occurred, and if so, raise it
    If Assigned(SyncException) then
      raise SyncException;
  end
else raise ESTCannotSynchronize.Create('TSyncThreadSynchronizer.Synchronize: Nested synchronization not allowed.');
end;

//------------------------------------------------------------------------------

procedure TSyncThreadSynchronizer.DoSynchronization(Timeout: UInt32 = 0);
begin
fSynchronizing := True;
try
  fEndpoint.Cycle(Timeout);
finally
  fSynchronizing := False;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSyncThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSyncThread - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSyncThread - protected methods
-------------------------------------------------------------------------------}

Function TSyncThread.GetSynchronizing: Boolean;
begin
Result := fSynchronizer.Synchronizing;
end;

//------------------------------------------------------------------------------

procedure TSyncThread.Synchronize(Method: TThreadMethod; Target: TSyncThreadSynchronizer);
begin
fSynchronizer.Synchronize(Method,Target);
end;

//------------------------------------------------------------------------------

procedure TSyncThread.Synchronize(Method: TThreadMethod; Target: TSyncThread);
begin
fSynchronizer.Synchronize(Method,Target.fSynchronizer);
end;

//------------------------------------------------------------------------------

procedure TSyncThread.DoSynchronization(Timeout: UInt32 = 0);
begin
fSynchronizer.DoSynchronization(Timeout);
end;

//------------------------------------------------------------------------------

procedure TSyncThread.AfterConstruction;
begin
inherited;
fSynchronizer := TSyncThreadSynchronizer.Create
end;

//------------------------------------------------------------------------------

procedure TSyncThread.BeforeDestruction;
begin
fSynchronizer.Free;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization                       
--------------------------------------------------------------------------------
===============================================================================}

initialization
  UnitInitialization;

finalization
  UnitFinalization;

end.
