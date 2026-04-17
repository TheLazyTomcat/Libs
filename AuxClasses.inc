{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

    This file contains main classes template used by AuxClasses library.

  Version 1.6 (2026-04-15)

  Last change 2026-04-17

  ©2018-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.AuxClasses

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    ListUtils     - github.com/TheLazyTomcat/Lib.ListUtils

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol AuxClasses_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{!tun_end!} // ignore this line

{===============================================================================
********************************************************************************

                                  Declarations

********************************************************************************
===============================================================================}

{$IFDEF AC_Include_Declaration}
{===============================================================================
--------------------------------------------------------------------------------
                        TCustom((Simple)Interfaced)Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)Object  - class declaration
===============================================================================}
{
  Base for all classes provided by this library. It is just a simple descendant
  of (interfaced) TObject with few added fields/properties that can be used for
  any purpose along with some methods (eg. macros for exception raising).
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedObject = class(TInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedObject = class(TSimpleInterfacedObject)
{$ELSE}
  TCustomObject = class(TObject)
{$IFEND}
  protected
    fUserIntData: PtrInt;
    fUserPtrData: Pointer;
    procedure RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String); overload; virtual;
    procedure RaiseError(const Method,Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(const Method,Msg: String); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Msg: String); overload; virtual;
    procedure RaiseError(const Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(const Msg: String); overload; virtual;
  public
    constructor Create;
    Function InstanceString: String; virtual;
    property UserIntData: PtrInt read fUserIntData write fUserIntData;
    property UserPtrData: Pointer read fUserPtrData write fUserPtrData;
    property UserData: PtrInt read fUserIntData write fUserIntData;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)RefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)RefCountedObject - class declaration
===============================================================================}
{
  Reference counted object. Counting is not automatic, you have to call methods
  Acquire and Release for it to work.
  
  When FreeOnRelease is set to true (by default set to false), then the object
  is automatically freed inside of function Release when reference counter upon
  entry to this function is 1 (ie. it reaches 0 in that call).
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedRefCountedObject = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedRefCountedObject = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomRefCountedObject = class(TCustomObject)
{$IFEND}
  protected
    fReferenceCount:  Int32;
    fFreeOnRelease:   Boolean;
    Function GetReferenceCount: Int32; virtual;
  public
    constructor Create;
    Function Acquire: Int32; virtual;
    Function Release: Int32; virtual;
    property ReferenceCount: Int32 read GetReferenceCount;
    property FreeOnRelease: Boolean read fFreeOnRelease write fFreeOnRelease;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                    TCustom((Simple)Interfaced)ChangingObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ChangingObject - class declaration
===============================================================================}
{
  Base class for objects that need to track and report changes (typical example
  would be lists). See description of individual methods and properties for
  more information.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedChangingObject = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedChangingObject = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomChangingObject = class(TCustomObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fDisableChangeTracking: Boolean;
    fCallAllChangeHandlers: Boolean;
    fChangeState:           TChangesTrackingState;
    fUpdateCounter:         Integer;
    fChangingEvent:         TNotifyEvent;
    fChangingCallback:      TNotifyCallback;
    fChangeEvent:           TNotifyEvent;
    fChangeCallback:        TNotifyCallback;
    // getters/setters
    procedure SetDisableChangeTracking(Value: Boolean); virtual;
    Function GetChanged: Boolean; virtual;
    Function GetUpdating: Boolean; virtual;
  {
    DoChanging

    Call this method just before doing any tracked change.

    Parameter ImmediateCall indicates whether the method is called directly
    from a code doing the change or not.

    If update block is not in effect, or it is but this method was not yet
    called within the current one, then it calls handler of OnChanging(Event)
    and/or OnChangingCallback and returns true. Otherwise (when within update
    block or when already called) it does not call handlers and returns false.

    If DisableChangeTracking is set to true, then this method does nothing and
    returns false.
  }
    Function DoChanging(ImmediateCall: Boolean = True): Boolean; virtual;
  {
    DoChange

    Call this method after a tracked change is completed.

    Parameter ImmediateCall indicates whether the method is called directly
    from a code doing the change (True) or from some other place (False), eg.
    when update block is ending.

    When ImmediateCall is true then it increments ChangeCounter (see further).
    If this counter is at its maximum (High(Integer)) then it is left as is so
    it does not overflow.

    If UpdateCounter is zero or lower (ie. not within an active update block),
    then it calls handler of OnChange(Event) and/or OnChangeCallback, resets
    ChangeCounter and returns true. If UpdateCounter is above zero, then it
    does not call any handler and returns false (ChangeCounter is left as is,
    ie. possibly incremented).

    If DisableChangeTracking is set to true, then this method does nothing and
    returns false.    
  }
    Function DoChange(ImmediateCall: Boolean = True): Boolean; virtual;
  {
    ClearStates

    Called when there is a need to clear update state and all change states.
    Should be overriden in descendants that introduce new state(s) where it
    should unconditionally clear them.

    Note that this method is called from constructor to initialize the states.
    So when creating derived classes you do not need to do that again there,
    it is enough to properly override this method.
  }
    procedure ClearStates; virtual;
  {
    BeginningUpdate

    Called from method BeginUpdate when it starts an actual update block (ie.
    UpdateCounter is zero).

    It clears change state and should also do so in descendants (override it)
    if they introduce new change states.
  }
    procedure BeginningUpdate; virtual;
  {
    EndingUpdate

    Called from method EndUpdate when an outer-most update block is ended.

    It clears change state and, if there were any changes made during the
    update block, it reports them. Remember to do the same in descendants
    if new change states are introduced.
  }
    procedure EndingUpdate; virtual;
  {
    BreakingUpdate

    Called from BreakUpdate when it actually ends an update block.

    Works exatly the same as EndingUpdate, but the change reporting is omitted
    if ReportChanges parameter is set to false.
  }
    procedure BreakingUpdate(ReportChanges: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  {
    BeginUpdate

    Starts an update block in which tracked changes are not reported (events
    are not called). All changes occuring within this block are counted and
    then reported in a single call to handlers when the block ends.

    Thanks to using a counter (UpdateCounter), the blocks can be deeply nested
    (there is technical limit of about two billion levels), only the outer-most
    block will do the report when it ends.

    Make sure you pair each call to this method with a call to EndUpdate,
    otherwise the block is never ended and changes will not be reported.

    Can raise an EACInvalidValue exception if UpdateCounter is already at
    High(Integer).

    Returns new value of UpdateCounter.
  }
    Function BeginUpdate: Integer; virtual;
  {
    EndUpdate

    Ends current update block by decrementing UpdateCounter. Because the blocks
    can be nested, it ends only the inner-most one.
    If UpdateCounter is already zero or lower, then it does nothing (counter is
    explicitly set to zero to correct possible erroneous states).

    If UpdateCounter reaches zero and there are unreported pending changes
    (ChangeCounter > 0), it will call method DoChange (see its description for
    more info).

    Returns new value of UpdateCounter.
  }
    Function EndUpdate: Integer; virtual;
  {
    BreakUpdate

    Breaks out of all levels of update blocks by settings UpdateCounter to
    zero.

    If ReportChanges is set to false (default), then pending unreported changes
    are discarded and not reported, otherwise they are reported in the same
    manner as from EndUpdate.

    Returns old value of UpdateCounter (value it had upon calling the function).

      WARNING - use this function very carefully and make sure you are aware
                of all its effects.
  }
    Function BreakUpdate(ReportChanges: Boolean = False): Integer; virtual;
  {
    DiscardChange

    Discards all pending unreported changes (if not in update block, it will
    do nothing).

    Returns true if any change is discarded (ie. there was at least one pending
    unreported change), false otherwise.
  }
    Function DiscardChanges: Boolean; virtual;
  {$IFDEF Debug}
    // calls DoChanging and DoChange, in that order
    procedure MakeChange; virtual;
  {$ENDIF}
  {
    DisableChangeTracking

    When set to true, it disables change tracking and reporting - methods
    DoChanging and DoChange return without performing any action (their
    result will be set to false).

      WARNING - changing this value will always clear internal update and
                change state (counters) by calling method ClearStates.
                Be very carefull when you change this value - eg. doing it in
                an update block is equivalent to calling DiscardChanges and
                BreakUpdate(False).

    It is intended for situations where a class uses change tracking, but its
    descendant wants to disable it (eg. for performance reasons) - it can be
    done by setting this property to true in constructor (note that constructor
    of this class is explicitly setting it to false, so you have to change it
    after inherited contructor is complete).
  }
    property DisableChangeTracking: Boolean read fDisableChangeTracking write SetDisableChangeTracking;
  {
    CallAllChangeHandlers

    Affects how handlers of OnChanging* and OnChange* events/callbacks are
    called when both event and callback are assigned for a particular action.
    Note that it does not have effect when only event or only callback, or none
    of them, is assigned.

    When set to false (default), then only the event is called, callback is
    ignored and is not called. When set to true, then both are called - first
    the event and then the callback. Note that if you do anything in the event
    handler, it will affect the callback (it is safe to change or deassign
    callback handler within event handler, both calls are separate).
  }
    property CallAllChangeHandlers: Boolean read fCallAllChangeHandlers write fCallAllChangeHandlers;
    property ChangeCounter: Integer read fChangeState.ChangeCounter;
    property Changed: Boolean read GetChanged;
    property UpdateCounter: Integer read fUpdateCounter;
    property Updating: Boolean read GetUpdating;
    property OnChanging: TNotifyEvent read fChangingEvent write fChangingEvent;
    property OnChangingEvent: TNotifyEvent read fChangingEvent write fChangingEvent;
    property OnChangingCallback: TNotifyCallback read fChangingCallback write fChangingCallback;
    property OnChange: TNotifyEvent read fChangeEvent write fChangeEvent;
    property OnChangeEvent: TNotifyEvent read fChangeEvent write fChangeEvent;
    property OnChangeCallback: TNotifyCallback read fChangeCallback write fChangeCallback;
  end;


{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)ListCommonObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListCommonObject - class declaration
===============================================================================}
{
  This class serves as a common ancestor for all list objects - both simple
  lists and multi lists.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedListCommonObject = class(TCustomInterfacedChangingObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedListCommonObject = class(TCustomSimpleInterfacedChangingObject)
{$ELSE}
  TCustomListCommonObject = class(TCustomChangingObject)
{$IFEND}
  protected
    fItemChangeStateSink:   TChangesTrackingState;
    fPropagateListChanges:  Boolean;
    fPropagateItemChanges:  TItemChangePropagation;
    procedure ClearStates; override;
  public
    constructor Create;
  {
    PropagateListChanges

    When set to true, list changes (see derived classes for more info, eg.
    TCustom*ListBaseObject) are propagated to global changes (ie. functions
    DoListChanging and DoListChange are calling DoChanging and DoChange
    respectively). When false then no propagation occurs.

    Note that this propagation also affects change counters - so list change
    increments not only list change counter, but, if this proprty is true, also
    the global change counter.
  }
    property PropagateListChanges: Boolean read fPropagateListChanges write fPropagateListChanges;
  {
    PropagateItemChanges

    Similarly to PropagateListchanges, this property controls whether and how
    are item changes propagated to global or list change reporting.

      NOTE - list changes itself are implemented in derived classes (eg.
             TCustom*ListObject or TCustom*MultiListObject), not here.

    See description of type TItemChangePropagation for more details.
  }
    property PropagateItemChanges: TItemChangePropagation read fPropagateItemChanges write fPropagateItemChanges;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                    TCustom((Simple)Interfaced)ListBaseObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListBaseObject - class declaration
===============================================================================}
{
  Base class for simple lists (ie. not multi-lists).

  It descends from TCustom*ChangingObject and adds a new change state - list
  change. This change state should be used when the whole list is changed (eg.
  item is added, removed, moved, ...).

    NOTE - derived classes are NOT required to use this state. They can use the
           global change state inherited from TCustom*ChangingObject or none at
           all.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedListBaseObject = class(TCustomInterfacedListCommonObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedListBaseObject = class(TCustomSimpleInterfacedListCommonObject)
{$ELSE}
  TCustomListBaseObject = class(TCustomListCommonObject)
{$IFEND}
  protected
    fListChangeState:       TChangesTrackingState;
    fListChangingEvent:     TNotifyEvent;
    fListChangingCallback:  TNotifyCallback;
    fListChangeEvent:       TNotifyEvent;
    fListChangeCallback:    TNotifyCallback;
    Function GetListChanged: Boolean; virtual;
  {
    DoListChanging

    Works similarly to inherited DoChanging, see there for more info.

    Parameter ImmediateCall here also affects, in conjunction with property
    PropagateListChanges, whether changes are propagated to global change
    reporting (they are not when set to false, PropagateListChanges is ignored).
  }
    Function DoListChanging(ImmediateCall: Boolean = True): Boolean; virtual;
  {
    DoListChange

    Again, it is very similar to inherited DoChange, and, as in the case
    of DoListChanging, parameter ImmediateCall affects change reporting
    propagation.
  }
    Function DoListChange(ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
  {$IFDEF Debug}
    // calls DoListChanging amd DoListChange
    procedure MakeListChange; virtual;
  {$ENDIF}
    property ListChangeCounter: Integer read fListChangeState.ChangeCounter;
    property ListChanged: Boolean read GetListChanged;
    property OnListChanging: TNotifyEvent read fListChangingEvent write fListChangingEvent;
    property OnListChangingEvent: TNotifyEvent read fListChangingEvent write fListChangingEvent;
    property OnListChangingCallback: TNotifyCallback read fListChangingCallback write fListChangingCallback;
    property OnListChange: TNotifyEvent read fListChangeEvent write fListChangeEvent;
    property OnListChangeEvent: TNotifyEvent read fListChangeEvent write fListChangeEvent;
    property OnListChangeCallback: TNotifyCallback read fListChangeCallback write fListChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                      TCustom((Simple)Interfaced)ListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListObject - class declaration
===============================================================================}
{
  This class is intended as an ancestor for lists or vectors. It currently
  implements methods for advanced parametrized growing and shrinking and
  change tracking and reporting.

  Expects derived class to properly implement capacity and count properties
  (both getters and setters) and LowIndex and HighIndex functions. Also, if
  item change tracking and reporting is to be used, GetItemChangeStatePtr
  must be overridden to provide pointer to real change state of given item.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedListObject = class(TCustomInterfacedListBaseObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedListObject = class(TCustomSimpleInterfacedListBaseObject)
{$ELSE}
  TCustomListObject = class(TCustomListBaseObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListResizeSettings:    TListResizeSettings;
    fPropagatedItem:        Integer;
    fItemChangingEvent:     TIndexEvent;
    fItemChangingCallback:  TIndexCallback;
    fItemChangeEvent:       TIndexEvent;
    fItemChangeCallback:    TIndexCallback;
    Function GetCapacity: Integer; virtual; abstract;
    procedure SetCapacity(Value: Integer); virtual; abstract;
    Function GetCount: Integer; virtual; abstract;
    procedure SetCount(Value: Integer); virtual; abstract;
    Function GetListResizeSettingsPtr: PListResizeSettings; virtual;
    Function GetGrowSettingsPtr: PGrowSettings; virtual;
    Function GetShrinkSettingsPtr: PShrinkSettings; virtual;
    procedure Grow(MinSpace: Integer = 1); virtual;
    procedure Shrink; virtual;
  {
    GetItemChangeStatePtr

    Returns pointer to change state of selected item - note that index is NOT
    checked for validity, if necessary do check before the call.

    This function must be overridden (do NOT call inherited code in that case)
    if the descending class supports per-item change states (which is not
    mandatory).
    Default implementation provided here will returns pointer to a fall-back
    common buffer (field fItemChangeStateSink), irrespective of passed index.
    Also, each call will clear this buffer before returning, so do not use this
    buffer to store any data and do not depend on it having any particular
    value.
  }
    Function GetItemChangeStatePtr(Index: Integer): PChangesTrackingState; virtual;
    Function GetItemChangeCounter(Index: Integer): Integer; virtual;
    Function GetItemChanged(Index: Integer): Boolean; virtual;
    Function DoItemChanging(Index: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    Function DoItemChange(Index: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; virtual; abstract;
    Function HighIndex: Integer; virtual; abstract;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedListObject{$ELSE}TCustomListObject{$IFEND}); virtual;
  {$IFDEF Debug}
    // calls DoItemChanging amd DoItemChange
    procedure MakeItemChange(Index: Integer); virtual;
  {$ENDIF}      
    // resizing proprties
    property ListResizeSettings: TListResizeSettings read fListResizeSettings write fListResizeSettings;
    property ListResizeSettingsPtr: PListResizeSettings read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings: TGrowSettings read fListResizeSettings.Grow write fListResizeSettings.Grow;
    property GrowSettingsPtr: PGrowSettings read GetGrowSettingsPtr;
    property GrowMode: TGrowMode read fListResizeSettings.Grow.Mode write fListResizeSettings.Grow.Mode;
    property GrowInit: Integer read fListResizeSettings.Grow.Init write fListResizeSettings.Grow.Init;
    property GrowDelta: Integer read fListResizeSettings.Grow.Delta write fListResizeSettings.Grow.Delta;
    property GrowFactor: Double read fListResizeSettings.Grow.Factor write fListResizeSettings.Grow.Factor;
    property GrowLimit: Integer read fListResizeSettings.Grow.Limit write fListResizeSettings.Grow.Limit;
    // shrink settings
    property ShrinkSettings: TShrinkSettings read fListResizeSettings.Shrink write fListResizeSettings.Shrink;
    property ShrinkSettingsPtr: PShrinkSettings read GetShrinkSettingsPtr;
    property ShrinkMode: TShrinkMode read fListResizeSettings.Shrink.Mode write fListResizeSettings.Shrink.Mode;
    property ShrinkDelta: Integer read fListResizeSettings.Shrink.Delta write fListResizeSettings.Shrink.Delta;
    property ShrinkFactor: Double read fListResizeSettings.Shrink.Factor write fListResizeSettings.Shrink.Factor;
    property ShrinkLimit: Integer read fListResizeSettings.Shrink.Limit write fListResizeSettings.Shrink.Limit;
    // capacity, count properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    // item change properties
    property ItemChangeCounter[Index: Integer]: Integer read GetItemChangeCounter;
    property ItemChanged[Index: Integer]: Boolean read GetItemChanged;
  {
    PropagatedItem

    When an item change is propagated to list or global change reporting, this
    property will be set to index of the propagated item during the event calls.
    Other times it is set to an invalid value (-1).

    Can also be used to discern whether global/list change event was called as
    a response to item change propagation (this property will contain valid
    item index) or not.
  }
    property PropagatedItem: Integer read fPropagatedItem;
    property OnItemChanging: TIndexEvent read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingEvent: TIndexEvent read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingCallback: TIndexCallback read fItemChangingCallback write fItemChangingCallback;
    property OnItemChange: TIndexEvent read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeEvent: TIndexEvent read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeCallback: TIndexCallback read fItemChangeCallback write fItemChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                     TCustom((Simple)Interfaced)List64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)List64Object - class declaration
===============================================================================}
{
  This class is intended as a base for massive lists that need to have more
  than 2^31 (about two billion) items - their indices, count and capacity are
  accessed using 64bit integer instead of usual 32bit type.
  I know such lists will be extremely rare and implementable pretty much only
  on 64bit systems, but if anyone needs it, here it is.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedList64Object = class(TCustomInterfacedListBaseObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedList64Object = class(TCustomSimpleInterfacedListBaseObject)
{$ELSE}
  TCustomList64Object = class(TCustomListBaseObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListResizeSettings:    TListResizeSettings64;
    fPropagatedItem:        Int64;
    fItemChangingEvent:     TIndex64Event;
    fItemChangingCallback:  TIndex64Callback;
    fItemChangeEvent:       TIndex64Event;
    fItemChangeCallback:    TIndex64Callback;
    Function GetCapacity: Int64; virtual; abstract;
    procedure SetCapacity(Value: Int64); virtual; abstract;
    Function GetCount: Int64; virtual; abstract;
    procedure SetCount(Value: Int64); virtual; abstract;
    Function GetListResizeSettingsPtr: PListResizeSettings64; virtual;
    Function GetGrowSettingsPtr: PGrowSettings64; virtual;
    Function GetShrinkSettingsPtr: PShrinkSettings64; virtual;
    procedure Grow(MinSpace: Int64 = 1); virtual;
    procedure Shrink; virtual;
    Function GetItemChangeStatePtr(Index: Int64): PChangesTrackingState; virtual;
    Function GetItemChangeCounter(Index: Int64): Integer; virtual;
    Function GetItemChanged(Index: Int64): Boolean; virtual;
    Function DoItemChanging(Index: Int64; ImmediateCall: Boolean = True): Boolean; virtual;
    Function DoItemChange(Index: Int64; ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Int64; virtual; abstract;
    Function HighIndex: Int64; virtual; abstract;
    Function CheckIndex(Index: Int64): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedList64Object
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedList64Object{$ELSE}TCustomList64Object{$IFEND}); virtual;
  {$IFDEF Debug}
    procedure MakeItemChange(Index: Int64); virtual;
  {$ENDIF}
    property ListResizeSettings: TListResizeSettings64 read fListResizeSettings write fListResizeSettings;
    property ListResizeSettingsPtr: PListResizeSettings64 read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings: TGrowSettings64 read fListResizeSettings.Grow write fListResizeSettings.Grow;
    property GrowSettingsPtr: PGrowSettings64 read GetGrowSettingsPtr;
    property GrowMode: TGrowMode read fListResizeSettings.Grow.Mode write fListResizeSettings.Grow.Mode;
    property GrowInit: Int64 read fListResizeSettings.Grow.Init write fListResizeSettings.Grow.Init;
    property GrowDelta: Int64 read fListResizeSettings.Grow.Delta write fListResizeSettings.Grow.Delta;
    property GrowFactor: Double read fListResizeSettings.Grow.Factor write fListResizeSettings.Grow.Factor;
    property GrowLimit: Int64 read fListResizeSettings.Grow.Limit write fListResizeSettings.Grow.Limit;
    // shrink settings
    property ShrinkSettings: TShrinkSettings64 read fListResizeSettings.Shrink write fListResizeSettings.Shrink;
    property ShrinkSettingsPtr: PShrinkSettings64 read GetShrinkSettingsPtr;
    property ShrinkMode: TShrinkMode read fListResizeSettings.Shrink.Mode write fListResizeSettings.Shrink.Mode;
    property ShrinkDelta: Int64 read fListResizeSettings.Shrink.Delta write fListResizeSettings.Shrink.Delta;
    property ShrinkFactor: Double read fListResizeSettings.Shrink.Factor write fListResizeSettings.Shrink.Factor;
    property ShrinkLimit: Int64 read fListResizeSettings.Shrink.Limit write fListResizeSettings.Shrink.Limit;
    // capacity, count properties
    property Capacity: Int64 read GetCapacity write SetCapacity;
    property Count: Int64 read GetCount write SetCount;      
    // item change properties
    property ItemChangeCounter[Index: Int64]: Integer read GetItemChangeCounter;
    property ItemChanged[Index: Int64]: Boolean read GetItemChanged;
    property PropagatedItem: Int64 read fPropagatedItem;
    property OnItemChanging: TIndex64Event read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingEvent: TIndex64Event read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingCallback: TIndex64Callback read fItemChangingCallback write fItemChangingCallback;
    property OnItemChange: TIndex64Event read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeEvent: TIndex64Event read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeCallback: TIndex64Callback read fItemChangeCallback write fItemChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiListBaseObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListBaseObject - class declaration
===============================================================================}
{
  This is a base class intended as a common ancestor for derived multi-list
  classes. It implements change tracking and reporting separately for each
  list.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedMultiListBaseObject = class(TCustomInterfacedListCommonObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedMultiListBaseObject = class(TCustomSimpleInterfacedListCommonObject)
{$ELSE}
  TCustomMultiListBaseObject = class(TCustomListCommonObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListCount:             Integer;
    fListChangeStates:      array of TChangesTrackingState;
    fPropagatedList:        Integer;
    fListChangingEvent:     TListEvent;
    fListChangingCallback:  TListCallback;
    fListChangeEvent:       TListEvent;
    fListChangeCallback:    TListCallback;
    // multi-list getters/setters
    procedure SetListCount(Value: Integer); virtual;
  {
    ResizeListArrays

    This method is called whenever there is a need to resize arrays of metadata
    (that is, when number of lists is changed).
  }
    procedure ResizeListArrays(NewLength: Integer); virtual;
    // change tracking getters/setters
    Function GetListChangeCounter(List: Integer): Integer; virtual;
    Function GetListChanged(List: Integer): Boolean; virtual;
    // change tracking and reporting methods
    Function DoListChanging(List: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    Function DoListChange(List: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create(ListCount: Integer);
    destructor Destroy; override;
    // multi-list methods
    Function LowList: Integer; virtual;
    Function HighList: Integer; virtual;
    Function CheckList(List: Integer): Boolean; virtual;
  {$IFDEF Debug}
    procedure MakeListChange(List: Integer); virtual;
  {$ENDIF}
    // multi-list properties
    property ListCount: Integer read fListCount write SetListCount;
    // change tracking properties
    property ListChangeCounter[List: Integer]: Integer read GetListChangeCounter;
    property ListChanged[List: Integer]: Boolean read GetListChanged;
  {
    PropagatedList

    When a list change is propagated to global change reporting, this property
    will be set to index of the propagated list during the event/callback
    calls. Otherwise it is set to an invalid value (-1).

    As such, this property can also be used to discern whether global change
    event was called as a response to change propagation (this property will
    contain valid list index) or not.
  }
    property PropagatedList: Integer read fPropagatedList;
    property OnListChanging: TListEvent read fListChangingEvent write fListChangingEvent;
    property OnListChangingEvent: TListEvent read fListChangingEvent write fListChangingEvent;
    property OnListChangingCallback: TListCallback read fListChangingCallback write fListChangingCallback;
    property OnListChange: TListEvent read fListChangeEvent write fListChangeEvent;
    property OnListChangeEvent: TListEvent read fListChangeEvent write fListChangeEvent;
    property OnListChangeCallback: TListCallback read fListChangeCallback write fListChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)MultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListObject - class declaration
===============================================================================}
{
  Very similar to TCustom((Simple)Interfaced)ListObject, but this class can
  support multiple separate lists that are selected by integer index.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedMultiListObject = class(TCustomInterfacedMultiListBaseObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedMultiListObject = class(TCustomSimpleInterfacedMultiListBaseObject)
{$ELSE}
  TCustomMultiListObject = class(TCustomMultiListBaseObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListResizeSettings:    array of TListResizeSettings;
    fPropagatedItem:        Integer;
    fItemChangingEvent:     TListIndexEvent;
    fItemChangingCallback:  TListIndexCallback;
    fItemChangeEvent:       TListIndexEvent;
    fItemChangeCallback:    TListIndexCallback;
    procedure ResizeListArrays(NewLength: Integer); override;
    Function GetCapacity(List: Integer): Integer; virtual; abstract;
    procedure SetCapacity(List,Value: Integer); virtual; abstract;
    Function GetCount(List: Integer): Integer; virtual; abstract;
    procedure SetCount(List,Value: Integer); virtual; abstract;
    // getters/setters for grow and shrink settings
    Function GetListResizeSettings(List: Integer): TListResizeSettings; virtual;
    procedure SetListResizeSettings(List: Integer; Value: TListResizeSettings); virtual;
    Function GetListResizeSettingsPtr(List: Integer): PListResizeSettings; virtual;
    Function GetGrowSettings(List: Integer): TGrowSettings; virtual;
    procedure SetGrowSettings(List: Integer; Value: TGrowSettings); virtual;
    Function GetGrowSettingsPtr(List: Integer): PGrowSettings; virtual;
    Function GetGrowSettings_Mode(List: Integer): TGrowMode; virtual;
    procedure SetGrowSettings_Mode(List: Integer; Value: TGrowMode); virtual;
    Function GetGrowSettings_Init(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Init(List: Integer; Value: Integer); virtual;
    Function GetGrowSettings_Delta(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Delta(List: Integer; Value: Integer); virtual;
    Function GetGrowSettings_Factor(List: Integer): Double; virtual;
    procedure SetGrowSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetGrowSettings_Limit(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Limit(List: Integer; Value: Integer); virtual; 
    Function GetShrinkSettings(List: Integer): TShrinkSettings; virtual;
    procedure SetShrinkSettings(List: Integer; Value: TShrinkSettings); virtual;
    Function GetShrinkSettingsPtr(List: Integer): PShrinkSettings; virtual;
    Function GetShrinkSettings_Mode(List: Integer): TShrinkMode; virtual;
    procedure SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode); virtual;
    Function GetShrinkSettings_Delta(List: Integer): Integer; virtual;
    procedure SetShrinkSettings_Delta(List: Integer; Value: Integer); virtual;
    Function GetShrinkSettings_Factor(List: Integer): Double; virtual;
    procedure SetShrinkSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetShrinkSettings_Limit(List: Integer): Integer; virtual;
    procedure SetShrinkSettings_Limit(List: Integer; Value: Integer); virtual;
    procedure Grow(List: Integer; MinSpace: Integer = 1); virtual;
    procedure Shrink(List: Integer); virtual;
    Function GetItemChangeStatePtr(List: Integer; Index: Integer): PChangesTrackingState; virtual;
    Function GetItemChangeCounter(List: Integer; Index: Integer): Integer; virtual;
    Function GetItemChanged(List: Integer; Index: Integer): Boolean; virtual;
    Function DoItemChanging(List: Integer; Index: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    Function DoItemChange(List: Integer; Index: Integer; ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create(ListCount: Integer);
    destructor Destroy; override;
    Function LowIndex(List: Integer): Integer; virtual; abstract;
    Function HighIndex(List: Integer): Integer; virtual; abstract;
    Function CheckIndex(List,Index: Integer): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedMultiListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}); virtual;
  {$IFDEF Debug}
    procedure MakeItemChange(List: Integer; Index: Integer); virtual;
  {$ENDIF}
    property ListResizeSettings[List: Integer]: TListResizeSettings read GetListResizeSettings write SetListResizeSettings;
    property ListResizeSettingsPtr[List: Integer]: PListResizeSettings read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings[List: Integer]: TGrowSettings read GetGrowSettings write SetGrowSettings;
    property GrowSettingsPtr[List: Integer]: PGrowSettings read GetGrowSettingsPtr;
    property GrowMode[List: Integer]: TGrowMode read GetGrowSettings_Mode write SetGrowSettings_Mode;
    property GrowInit[List: Integer]: Integer read GetGrowSettings_Init write SetGrowSettings_Init;
    property GrowDelta[List: Integer]: Integer read GetGrowSettings_Delta write SetGrowSettings_Delta;
    property GrowFactor[List: Integer]: Double read GetGrowSettings_Factor write SetGrowSettings_Factor;
    property GrowLimit[List: Integer]: Integer read GetGrowSettings_Limit write SetGrowSettings_Limit;
    // shrink settings
    property ShrinkSettings[List: Integer]: TShrinkSettings read GetShrinkSettings write SetShrinkSettings;
    property ShrinkSettingsPtr[List: Integer]: PShrinkSettings read GetShrinkSettingsPtr;
    property ShrinkMode[List: Integer]: TShrinkMode read GetShrinkSettings_Mode write SetShrinkSettings_Mode;
    property ShrinkDelta[List: Integer]: Integer read GetShrinkSettings_Delta write SetShrinkSettings_Delta;
    property ShrinkFactor[List: Integer]: Double read GetShrinkSettings_Factor write SetShrinkSettings_Factor;
    property ShrinkLimit[List: Integer]: Integer read GetShrinkSettings_Limit write SetShrinkSettings_Limit;
    // capacity, count properties
    property Capacity[List: Integer]: Integer read GetCapacity write SetCapacity;
    property Count[List: Integer]: Integer read GetCount write SetCount;
    // item change properties
    property ItemChangeCounter[List: Integer; Index: Integer]: Integer read GetItemChangeCounter;
    property ItemChanged[List: Integer; Index: Integer]: Boolean read GetItemChanged;
  {
    PropagatedItem

    When item change is propagated to list change or global change, this
    property holds index of the changed item during the event call. Also,
    property PropagatedList holds index of list in which the changed item
    resides.
  }
    property PropagatedItem: Integer read fPropagatedItem;
    property OnItemChanging: TListIndexEvent read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingEvent: TListIndexEvent read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingCallback: TListIndexCallback read fItemChangingCallback write fItemChangingCallback;
    property OnItemChange: TListIndexEvent read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeEvent: TListIndexEvent read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeCallback: TListIndexCallback read fItemChangeCallback write fItemChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiList64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiList64Object - class declaration
===============================================================================}
{
  This class is just an 64bit-indexed variant of "normal" multi-list (eg. class
  TCustom*MultiListObject).
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedMultiList64Object = class(TCustomInterfacedMultiListBaseObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedMultiList64Object = class(TCustomSimpleInterfacedMultiListBaseObject)
{$ELSE}
  TCustomMultiList64Object = class(TCustomMultiListBaseObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListResizeSettings:    array of TListResizeSettings64;
    fPropagatedItem:        Int64;
    fItemChangingEvent:     TListIndex64Event;
    fItemChangingCallback:  TListIndex64Callback;
    fItemChangeEvent:       TListIndex64Event;
    fItemChangeCallback:    TListIndex64Callback;
    procedure ResizeListArrays(NewLength: Integer); override;    
    Function GetCapacity(List: Integer): Int64; virtual; abstract;
    procedure SetCapacity(List: Integer; Value: Int64); virtual; abstract;
    Function GetCount(List: Integer): Int64; virtual; abstract;
    procedure SetCount(List: Integer; Value: Int64); virtual; abstract;
    // getters/setters for grow and shrink settings
    Function GetListResizeSettings(List: Integer): TListResizeSettings64; virtual;
    procedure SetListResizeSettings(List: Integer; Value: TListResizeSettings64); virtual;
    Function GetListResizeSettingsPtr(List: Integer): PListResizeSettings64; virtual;
    Function GetGrowSettings(List: Integer): TGrowSettings64; virtual;
    procedure SetGrowSettings(List: Integer; Value: TGrowSettings64); virtual;
    Function GetGrowSettingsPtr(List: Integer): PGrowSettings64; virtual;
    Function GetGrowSettings_Mode(List: Integer): TGrowMode; virtual;
    procedure SetGrowSettings_Mode(List: Integer; Value: TGrowMode); virtual;
    Function GetGrowSettings_Init(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Init(List: Integer; Value: Int64); virtual;
    Function GetGrowSettings_Delta(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Delta(List: Integer; Value: Int64); virtual;
    Function GetGrowSettings_Factor(List: Integer): Double; virtual;
    procedure SetGrowSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetGrowSettings_Limit(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Limit(List: Integer; Value: Int64); virtual;
    Function GetShrinkSettings(List: Integer): TShrinkSettings64; virtual;
    procedure SetShrinkSettings(List: Integer; Value: TShrinkSettings64); virtual;
    Function GetShrinkSettingsPtr(List: Integer): PShrinkSettings64; virtual;
    Function GetShrinkSettings_Mode(List: Integer): TShrinkMode; virtual;
    procedure SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode); virtual;
    Function GetShrinkSettings_Delta(List: Integer): Int64; virtual;
    procedure SetShrinkSettings_Delta(List: Integer; Value: Int64); virtual;
    Function GetShrinkSettings_Factor(List: Integer): Double; virtual;
    procedure SetShrinkSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetShrinkSettings_Limit(List: Integer): Int64; virtual;
    procedure SetShrinkSettings_Limit(List: Integer; Value: Int64); virtual;
    procedure Grow(List: Integer; MinSpace: Int64 = 1); virtual;
    procedure Shrink(List: Integer); virtual;
    Function GetItemChangeStatePtr(List: Integer; Index: Int64): PChangesTrackingState; virtual;
    Function GetItemChangeCounter(List: Integer; Index: Int64): Integer; virtual;
    Function GetItemChanged(List: Integer; Index: Int64): Boolean; virtual;
    Function DoItemChanging(List: Integer; Index: Int64; ImmediateCall: Boolean = True): Boolean; virtual;
    Function DoItemChange(List: Integer; Index: Int64; ImmediateCall: Boolean = True): Boolean; virtual;
    procedure ClearStates; override;
    procedure BeginningUpdate; override;
    procedure EndingUpdate; override;
    procedure BreakingUpdate(ReportChanges: Boolean); override;
  public
    constructor Create(ListCount: Integer);
    destructor Destroy; override;  
    Function LowIndex(List: Integer): Int64; virtual; abstract;
    Function HighIndex(List: Integer): Int64; virtual; abstract;
    Function CheckIndex(List: Integer; Index: Int64): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedMultiList64Object
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}); virtual;
  {$IFDEF Debug}
    procedure MakeItemChange(List: Integer; Index: Int64); virtual;
  {$ENDIF}
    property ListResizeSettings[List: Integer]: TListResizeSettings64 read GetListResizeSettings write SetListResizeSettings;
    property ListResizeSettingsPtr[List: Integer]: PListResizeSettings64 read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings[List: Integer]: TGrowSettings64 read GetGrowSettings write SetGrowSettings;
    property GrowSettingsPtr[List: Integer]: PGrowSettings64 read GetGrowSettingsPtr;
    property GrowMode[List: Integer]: TGrowMode read GetGrowSettings_Mode write SetGrowSettings_Mode;
    property GrowInit[List: Integer]: Int64 read GetGrowSettings_Init write SetGrowSettings_Init;
    property GrowDelta[List: Integer]: Int64 read GetGrowSettings_Delta write SetGrowSettings_Delta;
    property GrowFactor[List: Integer]: Double read GetGrowSettings_Factor write SetGrowSettings_Factor;
    property GrowLimit[List: Integer]: Int64 read GetGrowSettings_Limit write SetGrowSettings_Limit;
    // shrink settings
    property ShrinkSettings[List: Integer]: TShrinkSettings64 read GetShrinkSettings write SetShrinkSettings;
    property ShrinkSettingsPtr[List: Integer]: PShrinkSettings64 read GetShrinkSettingsPtr;
    property ShrinkMode[List: Integer]: TShrinkMode read GetShrinkSettings_Mode write SetShrinkSettings_Mode;
    property ShrinkDelta[List: Integer]: Int64 read GetShrinkSettings_Delta write SetShrinkSettings_Delta;
    property ShrinkFactor[List: Integer]: Double read GetShrinkSettings_Factor write SetShrinkSettings_Factor;
    property ShrinkLimit[List: Integer]: Int64 read GetShrinkSettings_Limit write SetShrinkSettings_Limit;
    // capacity, count properties
    property Capacity[List: Integer]: Int64 read GetCapacity write SetCapacity;
    property Count[List: Integer]: Int64 read GetCount write SetCount;
    // item change properties
    property ItemChangeCounter[List: Integer; Index: Int64]: Integer read GetItemChangeCounter;
    property ItemChanged[List: Integer; Index: Int64]: Boolean read GetItemChanged;
    property PropagatedItem: Int64 read fPropagatedItem;
    property OnItemChanging: TListIndex64Event read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingEvent: TListIndex64Event read fItemChangingEvent write fItemChangingEvent;
    property OnItemChangingCallback: TListIndex64Callback read fItemChangingCallback write fItemChangingCallback;
    property OnItemChange: TListIndex64Event read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeEvent: TListIndex64Event read fItemChangeEvent write fItemChangeEvent;
    property OnItemChangeCallback: TListIndex64Callback read fItemChangeCallback write fItemChangeCallback;
  end;

//==============================================================================
// shortened aliases - used in implementation to limit verboseness

{$IF Defined(AC_Include_Interfaced)}
type
  TCustomIObject              = TCustomInterfacedObject;
  TCustomIRefCountedObject    = TCustomInterfacedRefCountedObject;
  TCustomIChangingObject      = TCustomInterfacedChangingObject;
  TCustomIListCommonObject    = TCustomInterfacedListCommonObject;
  TCustomIListBaseObject      = TCustomInterfacedListBaseObject;
  TCustomIListObject          = TCustomInterfacedListObject;
  TCustomIList64Object        = TCustomInterfacedList64Object;
  TCustomIMultiListBaseObject = TCustomInterfacedMultiListBaseObject;
  TCustomIMultiListObject     = TCustomInterfacedMultiListObject;
  TCustomIMultiList64Object   = TCustomInterfacedMultiList64Object;
{$ELSEIF Defined(AC_Include_SInterfaced)}
type
  TCustomSIObject              = TCustomSimpleInterfacedObject;
  TCustomSIRefCountedObject    = TCustomSimpleInterfacedRefCountedObject;
  TCustomSIChangingObject      = TCustomSimpleInterfacedChangingObject;
  TCustomSIListCommonObject    = TCustomSimpleInterfacedListCommonObject;
  TCustomSIListBaseObject      = TCustomSimpleInterfacedListBaseObject;
  TCustomSIListObject          = TCustomSimpleInterfacedListObject;
  TCustomSIList64Object        = TCustomSimpleInterfacedList64Object;
  TCustomSIMultiListBaseObject = TCustomSimpleInterfacedMultiListBaseObject;
  TCustomSIMultiListObject     = TCustomSimpleInterfacedMultiListObject;
  TCustomSIMultiList64Object   = TCustomSimpleInterfacedMultiList64Object;
{$IFEND}

{$ENDIF}

{===============================================================================
********************************************************************************

                                 Implementations

********************************************************************************
===============================================================================}

{$IFDEF AC_Include_Implementation}

{$UNDEF AC_I_I}
{$UNDEF AC_I_SI}
{$IF Defined(AC_Include_Interfaced)}
  {$DEFINE AC_I_I}
{$ELSEIF Defined(AC_Include_SInterfaced)}
  {$DEFINE AC_I_SI}
{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                        TCustom((Simple)Interfaced)Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)Object - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s.%s: %s',[InstanceString,Method,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String);
begin
RaiseError(ExceptionClass,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Method,Msg: String; const Args: array of const);
begin
RaiseError(Exception,Method,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Method,Msg: String);
begin
RaiseError(Exception,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s: %s',[InstanceString,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Msg: String);
begin
RaiseError(ExceptionClass,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Msg: String; const Args: array of const);
begin
RaiseError(Exception,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Msg: String);
begin
RaiseError(Exception,Msg,[]);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  Create;
begin
inherited;
fUserIntData := 0;
fUserPtrData := nil;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  InstanceString: String;
begin
Result := GetInstanceString(Self);
end;


{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)RefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)RefCountedObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)RefCountedObject - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  GetReferenceCount: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,0);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)RefCountedObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Create;
begin
inherited Create;
fReferenceCount := 0;
fFreeOnRelease := False;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Acquire: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,1) + 1;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Release: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,-1) - 1;
If fFreeOnRelease and (Result <= 0) then
  Self.Free;
end;


{===============================================================================
--------------------------------------------------------------------------------
                    TCustom((Simple)Interfaced)ChangingObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ChangingObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ChangingObject - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedChangingObject'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedChangingObject'
{$ELSE}
  'TCustomChangingObject'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ChangingObject - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  SetDisableChangeTracking(Value: Boolean);
begin
If Value <> fDisableChangeTracking then
  begin
    ClearStates;
    fDisableChangeTracking := Value;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  GetChanged: Boolean;
begin
Result := fChangeState.ChangeCounter > 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  GetUpdating: Boolean;
begin
Result := fUpdateCounter > 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  DoChanging(ImmediateCall: Boolean = True): Boolean;
begin
ConsumeArgs([ImmediateCall]);
Result := False;
If not fDisableChangeTracking and not fChangeState.ChangingDone then
  begin
    If Assigned(fChangingEvent) then
      begin
        fChangingEvent(Self);
        If fCallAllChangeHandlers and Assigned(fChangingCallback) then
          fChangingCallback(Self);
      end
    else If Assigned(fChangingCallback) then
      fChangingCallback(Self);
    fChangeState.ChangingDone := fUpdateCounter > 0;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  DoChange(ImmediateCall: Boolean = True): Boolean;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    // prevent overflow...
    If ImmediateCall and (fChangeState.ChangeCounter < High(Integer)) then
      Inc(fChangeState.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fChangeEvent) then
          begin
            fChangeEvent(Self);
            If fCallAllChangeHandlers and Assigned(fChangeCallback) then
              fChangeCallback(Self);
          end
        else If Assigned(fChangeCallback) then
          fChangeCallback(Self);
        // pending unreported changes were reported, clear the counter  
        fChangeState.ChangeCounter := 0;
        Result := True;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  ClearStates;
begin
fChangeState := ChangesTrackingStateInit;
fUpdateCounter := 0;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  BeginningUpdate;
begin
fChangeState := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  EndingUpdate;
begin
// report pending unreported changes, if any
If fChangeState.ChangeCounter > 0 then
  DoChange(False);
fChangeState := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
begin
{
  This is the same as EndingUpdate, but pending change reporting happens only
  if ReportChanges parameter is true.
}
If ReportChanges and (fChangeState.ChangeCounter > 0) then
  DoChange(False);
fChangeState := ChangesTrackingStateInit;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ChangingObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  Create;
begin
inherited;
fDisableChangeTracking := False;
fCallAllChangeHandlers := False;
ClearStates;
fChangingEvent := nil;
fChangingCallback := nil;
fChangeEvent := nil;
fChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  Destroy;
begin
// make sure events are not fired during destruction
fChangingEvent := nil;
fChangingCallback := nil;
fChangeEvent := nil;
fChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  BeginUpdate: Integer;
begin
// overflow protection
If fUpdateCounter >= High(Integer) then
  raise EACInvalidValue.CreateFmt('%s.BeginUpdate: Update counter too high.',[StaticClassName]);
If fUpdateCounter <= 0 then
  begin
    fUpdateCounter := 0;
    BeginningUpdate;
  end;
Inc(fUpdateCounter);
Result := fUpdateCounter;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  EndUpdate: Integer;
begin
{
  Following check is here to ensure EndingUpdate is not called when this method
  is erroneously called outside of an actual update block.
}
If fUpdateCounter > 0 then
  begin
    Dec(fUpdateCounter);
    If fUpdateCounter <= 0 then
      begin
        fUpdateCounter := 0;
        EndingUpdate;
      end;
  end
else fUpdateCounter := 0;
Result := fUpdateCounter;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  BreakUpdate(ReportChanges: Boolean = False): Integer;
begin
Result := fUpdateCounter;
If fUpdateCounter > 0 then
  begin
    fUpdateCounter := 0;
    BreakingUpdate(ReportChanges);
  end
else fUpdateCounter := 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  DiscardChanges: Boolean;
begin
Result := fChangeState.ChangeCounter > 0;
fChangeState := ChangesTrackingStateInit;
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIChangingObject{$ELSEIF Defined(AC_I_SI)}TCustomSIChangingObject{$ELSE}TCustomChangingObject{$IFEND}.
  MakeChange;
begin
DoChanging;
DoChange;
end;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)ListCommonObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListCommonObject - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListCommonObject - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIListCommonObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListCommonObject{$ELSE}TCustomListCommonObject{$IFEND}.
  ClearStates;
begin
inherited;
fItemChangeStateSink := ChangesTrackingStateInit;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListCommonObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIListCommonObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListCommonObject{$ELSE}TCustomListCommonObject{$IFEND}.
  Create;
begin
inherited;
fPropagateListChanges := False;
fPropagateItemChanges := prpNone;
end;


{===============================================================================
--------------------------------------------------------------------------------
                    TCustom((Simple)Interfaced)ListBaseObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListBaseObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListBaseObject - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  GetListChanged: Boolean;
begin
Result := fListChangeState.ChangeCounter > 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  DoListChanging(ImmediateCall: Boolean = True): Boolean;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not fListChangeState.ChangingDone then
      begin
        If Assigned(fListChangingEvent) then
          begin
            fListChangingEvent(Self);
            If fCallAllChangeHandlers and Assigned(fListChangingCallback) then
              fListChangingCallback(Self);
          end
        else If Assigned(fListChangingCallback) then
          fListChangingCallback(Self);
        fListChangeState.ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    If ImmediateCall and fPropagateListChanges then
      DoChanging;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  DoListChange(ImmediateCall: Boolean = True): Boolean;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If ImmediateCall and (fListChangeState.ChangeCounter < High(Integer)) then
      Inc(fListChangeState.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fListChangeEvent) then
          begin
            fListChangeEvent(Self);
            If fCallAllChangeHandlers and Assigned(fListChangeCallback) then
              fListChangeCallback(Self);
          end
        else If Assigned(fListChangeCallback) then
          fListChangeCallback(Self);
        fListChangeState.ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall and fPropagateListChanges then
      DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  ClearStates;
begin
inherited;
fListChangeState := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  BeginningUpdate;
begin
inherited;
fListChangeState := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  EndingUpdate;
begin
If fListChangeState.ChangeCounter > 0 then
  DoListChange(False);
fListChangeState := ChangesTrackingStateInit;
inherited;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
begin
If ReportChanges and (fListChangeState.ChangeCounter > 0) then
  DoListChange(False);
fListChangeState := ChangesTrackingStateInit;
inherited BreakingUpdate(ReportChanges);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListBaseObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  Create;
begin
inherited;
fListChangingEvent := nil;
fListChangingCallback := nil;
fListChangeEvent := nil;
fListChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  Destroy;
begin
fListChangingEvent := nil;
fListChangingCallback := nil;
fListChangeEvent := nil;
fListChangeCallback := nil;
inherited;
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------
procedure {$IF Defined(AC_I_I)}TCustomIListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListBaseObject{$ELSE}TCustomListBaseObject{$IFEND}.
  MakeListChange;
begin
DoListChanging;
DoListChange;
end;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                      TCustom((Simple)Interfaced)ListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedListObject'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedListObject'
{$ELSE}
  'TCustomListObject'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetListResizeSettingsPtr: PListResizeSettings;
begin
Result := Addr(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetGrowSettingsPtr: PGrowSettings;
begin
Result := Addr(fListResizeSettings.Grow);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetShrinkSettingsPtr: PShrinkSettings;
begin
Result := Addr(fListResizeSettings.Shrink);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Grow(MinSpace: Integer = 1);
var
  NewCapacity:  Integer;
begin
If GrowResolve(Capacity,Count,MinSpace,fListResizeSettings.Grow,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Shrink;
var
  NewCapacity:  Integer;
begin
If ShrinkResolve(Capacity,Count,fListResizeSettings.Shrink,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetItemChangeStatePtr(Index: Integer): PChangesTrackingState;
begin
// this is purely internal method, therefore index is not checked here
ConsumeArgs([Index]);
fItemChangeStateSink := ChangesTrackingStateInit;
Result := @fItemChangeStateSink;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetItemChangeCounter(Index: Integer): Integer;
begin
If CheckIndex(Index) then
  Result := GetItemChangeStatePtr(Index)^.ChangeCounter
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Index (%d) out of bounds.',[StaticClassName,Index]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetItemChanged(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := GetItemChangeStatePtr(Index)^.ChangeCounter > 0
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Index (%d) out of bounds.',[StaticClassName,Index]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  DoItemChanging(Index: Integer; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedItem:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckIndex(Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(Index);
    If not ItemChangeStatePtr^.ChangingDone then
      begin
        If Assigned(fItemChangingEvent) then
          begin
            fItemChangingEvent(Self,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangingCallback) then
              fItemChangingCallback(Self,Index);
          end
        else If Assigned(fItemChangingCallback) then
          fItemChangingCallback(Self,Index);
        ItemChangeStatePtr^.ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    // change propagation
    If ImmediateCall then
      begin
        OldPropagatedItem := fPropagatedItem;
        try
          fPropagatedItem := Index;
          case fPropagateItemChanges of
            prpToGlobal:  DoChanging;
            prpToList:    DoListChanging;
            prpToBoth:    begin
                            DoListChanging;
                            If not fPropagateListChanges then
                              DoChanging;
                          end;
          else
           {prpNone}  // do nothing
          end;
        finally
          fPropagatedItem := OldPropagatedItem;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  DoItemChange(Index: Integer; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedItem:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckIndex(Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(Index);
    If ImmediateCall and (ItemChangeStatePtr^.ChangeCounter < High(Integer)) then
      Inc(ItemChangeStatePtr^.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fItemChangeEvent) then
          begin
            fItemChangeEvent(Self,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangeCallback) then
              fItemChangeCallback(Self,Index);
          end
        else If Assigned(fItemChangeCallback) then
          fItemChangeCallback(Self,Index);
        ItemChangeStatePtr^.ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall then
      begin
        OldPropagatedItem := fPropagatedItem;
        try
          fPropagatedItem := Index;
          case fPropagateItemChanges of
            prpToGlobal:  DoChange;
            prpToList:    DoListChange;
            prpToBoth:    begin
                            DoListChange;
                            If not fPropagateListChanges then
                              DoChange;
                          end;
          else
           {prpNone}  // do nothing
          end;
        finally
          fPropagatedItem := OldPropagatedItem;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  ClearStates;
var
  i:  Integer;
begin
inherited;
For i := LowIndex to HighIndex do
  GetItemChangeStatePtr(i)^ := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  BeginningUpdate;
var
  i:  Integer;
begin
inherited;
For i := LowIndex to HighIndex do
  GetItemChangeStatePtr(i)^ := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  EndingUpdate;
var
  i:                  Integer;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For i := LowIndex to HighIndex do
  begin
    ItemChangeStatePtr := GetItemChangeStatePtr(i);
    If ItemChangeStatePtr^.ChangeCounter > 0 then
      DoItemChange(i,False);
    ItemChangeStatePtr^ := ChangesTrackingStateInit;
  end;
inherited;
end;

//------------------------------------------------------------------------------
  
procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
var
  i:                  Integer;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For i := LowIndex to HighIndex do
  begin
    ItemChangeStatePtr := GetItemChangeStatePtr(i);
    If ReportChanges and (ItemChangeStatePtr^.ChangeCounter > 0) then
      DoItemChange(i,False);
    ItemChangeStatePtr^ := ChangesTrackingStateInit;
  end;
inherited BreakingUpdate(ReportChanges);  
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Create;
begin
inherited;
fListResizeSettings := DefaultListResizeSettings;
fPropagatedItem := -1;
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Destroy;
begin
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND});
begin
fListResizeSettings := Source.ListResizeSettings;
// no further processing needed
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  MakeItemChange(Index: Integer);
begin
If CheckIndex(Index) then
  begin
    DoItemChanging(Index);
    DoItemChange(Index);
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
end;
{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                     TCustom((Simple)Interfaced)List64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)List64Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)List64Object - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedList64Object'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedList64Object'
{$ELSE}
  'TCustomList64Object'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)List64Object - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetListResizeSettingsPtr: PListResizeSettings64;
begin
Result := Addr(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetGrowSettingsPtr: PGrowSettings64;
begin
Result := Addr(fListResizeSettings.Grow);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetShrinkSettingsPtr: PShrinkSettings64;
begin
Result := Addr(fListResizeSettings.Shrink);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Grow(MinSpace: Int64 = 1);
var
  NewCapacity:  Int64;
begin
If GrowResolve(Capacity,Count,MinSpace,fListResizeSettings.Grow,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Shrink;
var
  NewCapacity:  Int64;
begin
If ShrinkResolve(Capacity,Count,fListResizeSettings.Shrink,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetItemChangeStatePtr(Index: Int64): PChangesTrackingState;
begin
ConsumeArgs([Index]);
fItemChangeStateSink := ChangesTrackingStateInit;
Result := @fItemChangeStateSink;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetItemChangeCounter(Index: Int64): Integer;
begin
If CheckIndex(Index) then
  Result := GetItemChangeStatePtr(Index)^.ChangeCounter
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Index (%d) out of bounds.',[StaticClassName,Index]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetItemChanged(Index: Int64): Boolean;
begin
If CheckIndex(Index) then
  Result := GetItemChangeStatePtr(Index)^.ChangeCounter > 0
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Index (%d) out of bounds.',[StaticClassName,Index]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  DoItemChanging(Index: Int64; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedItem:  Int64;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckIndex(Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(Index);
    If not ItemChangeStatePtr^.ChangingDone then
      begin
        If Assigned(fItemChangingEvent) then
          begin
            fItemChangingEvent(Self,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangingCallback) then
              fItemChangingCallback(Self,Index);
          end
        else If Assigned(fItemChangingCallback) then
          fItemChangingCallback(Self,Index);
        ItemChangeStatePtr^.ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    If ImmediateCall then
      begin
        OldPropagatedItem := fPropagatedItem;
        try
          fPropagatedItem := Index;
          case fPropagateItemChanges of
            prpToGlobal:  DoChanging;
            prpToList:    DoListChanging;
            prpToBoth:    begin
                            DoListChanging;
                            If not fPropagateListChanges then
                              DoChanging;
                          end;
          else
           {prpNone}  // do nothing
          end;
        finally
          fPropagatedItem := OldPropagatedItem;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  DoItemChange(Index: Int64; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedItem:  Int64;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckIndex(Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(Index);
    If ImmediateCall and (ItemChangeStatePtr^.ChangeCounter < High(Integer)) then
      Inc(ItemChangeStatePtr^.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fItemChangeEvent) then
          begin
            fItemChangeEvent(Self,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangeCallback) then
              fItemChangeCallback(Self,Index);
          end
        else If Assigned(fItemChangeCallback) then
          fItemChangeCallback(Self,Index);
        ItemChangeStatePtr^.ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall then
      begin
        OldPropagatedItem := fPropagatedItem;
        try
          fPropagatedItem := Index;
          case fPropagateItemChanges of
            prpToGlobal:  DoChange;
            prpToList:    DoListChange;
            prpToBoth:    begin
                            DoListChange;
                            If not fPropagateListChanges then
                              DoChange;
                          end;
          else
           {prpNone}  // do nothing
          end;
        finally
          fPropagatedItem := OldPropagatedItem;
        end;
      end;;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  ClearStates;
var
  i:  Int64;
begin
inherited;
// cannot use int64 in for-loop (at least in 32b Delphi)
i := LowIndex;
while i <= HighIndex do
  begin
    GetItemChangeStatePtr(i)^ := ChangesTrackingStateInit;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  BeginningUpdate;
var
  i:  Int64;
begin
inherited;
i := LowIndex;
while i <= HighIndex do
  begin
    GetItemChangeStatePtr(i)^ := ChangesTrackingStateInit;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  EndingUpdate;
var
  i:                  Int64;
  ItemChangeStatePtr: PChangesTrackingState;
begin
i := LowIndex;
while i <= HighIndex do
  begin
    ItemChangeStatePtr := GetItemChangeStatePtr(i);
    If ItemChangeStatePtr^.ChangeCounter > 0 then
      DoItemChange(i,False);
    ItemChangeStatePtr^ := ChangesTrackingStateInit;
    Inc(i);
  end;
inherited;
end;

//------------------------------------------------------------------------------
  
procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
var
  i:                  Int64;
  ItemChangeStatePtr: PChangesTrackingState;
begin
i := LowIndex;
while i <= HighIndex do
  begin
    ItemChangeStatePtr := GetItemChangeStatePtr(i);
    If ReportChanges and (ItemChangeStatePtr^.ChangeCounter > 0) then
      DoItemChange(i,False);
    ItemChangeStatePtr^ := ChangesTrackingStateInit;
    Inc(i);
  end;
inherited BreakingUpdate(ReportChanges);  
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)List64Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Create;
begin
inherited;
fListResizeSettings := DefaultListResizeSettings64;
fPropagatedItem := -1;
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Destroy;
begin
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  CheckIndex(Index: Int64): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND});
begin
fListResizeSettings := Source.ListResizeSettings;
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  MakeItemChange(Index: Int64);
begin
If CheckIndex(Index) then
  begin
    DoItemChanging(Index);
    DoItemChange(Index);
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
end;
{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiListBaseObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListBaseObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListBaseObject - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedMultiListBaseObject'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedMultiListBaseObject'
{$ELSE}
  'TCustomMultiListBaseObject'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListBaseObject - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  SetListCount(Value: Integer);
begin
If Value < 0 then
  raise EACInvalidValue.CreateFmt('%s.SetListCount: Invalid list count (%d).',[StaticClassName,Value]);
If Value <> fListCount then
  begin
    DoChanging;
    ResizeListArrays(Value);
    fListCount := Value;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  ResizeListArrays(NewLength: Integer);
var
  OldLength:  Integer;
  i:          Integer;
begin
OldLength := Length(fListChangeStates);
SetLength(fListChangeStates,NewLength);
If NewLength > OldLength then
  For i := OldLength to High(fListChangeStates) do
    fListChangeStates[i] := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  GetListChangeCounter(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListChangeStates[List].ChangeCounter
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListChangeCounter: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  GetListChanged(List: Integer): Boolean;
begin
If CheckList(List) then
  Result := fListChangeStates[List].ChangeCounter > 0
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListChanged: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  DoListChanging(List: Integer; ImmediateCall: Boolean = True): Boolean;
var
  OldPropagatedList:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoListChanging: Invalid list (%d).',[StaticClassName,List]);
    If not fListChangeStates[List].ChangingDone then
      begin
        If Assigned(fListChangingEvent) then
          begin
            fListChangingEvent(Self,List);
            If fCallAllChangeHandlers and Assigned(fListChangingCallback) then
              fListChangingCallback(Self,List);
          end
        else If Assigned(fListChangingCallback) then
          fListChangingCallback(Self,List);
        fListChangeStates[List].ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    If ImmediateCall and fPropagateListChanges then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          DoChanging;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  DoListChange(List: Integer; ImmediateCall: Boolean = True): Boolean;
var
  OldPropagatedList:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoListChange: Invalid list (%d).',[StaticClassName,List]);
    If ImmediateCall and (fListChangeStates[List].ChangeCounter < High(Integer)) then
      Inc(fListChangeStates[List].ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fListChangeEvent) then
          begin
            fListChangeEvent(Self,List);
            If fCallAllChangeHandlers and Assigned(fListChangeCallback) then
              fListChangeCallback(Self,List);
          end
        else If Assigned(fListChangeCallback) then
          fListChangeCallback(Self,List);
        fListChangeStates[List].ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall and fPropagateListChanges then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          DoChange;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  ClearStates;
var
  i:  Integer;
begin
inherited;
For i := LowList to HighList do
  fListChangeStates[i] := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  BeginningUpdate;
var
  i:  Integer;
begin
inherited;
For i := LowList to HighList do
  fListChangeStates[i] := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  EndingUpdate;
var
  i:  Integer;  
begin
For i := LowList to HighList do
  begin
    If fListChangeStates[i].ChangeCounter > 0 then
      DoListChange(i,False);
    fListChangeStates[i] := ChangesTrackingStateInit;
  end;
inherited;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
var
  i:  Integer;  
begin
For i := LowList to HighList do
  begin
    If ReportChanges and (fListChangeStates[i].ChangeCounter > 0) then
      DoListChange(i,False);
    fListChangeStates[i] := ChangesTrackingStateInit;
  end;
inherited BreakingUpdate(ReportChanges);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListBaseObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  Create(ListCount: Integer);
begin
inherited Create;
fListCount := 0;
ResizeListArrays(ListCount);
fListCount := ListCount;
fPropagatedList := -1;
fListChangingEvent := nil;
fListChangingCallback := nil;
fListChangeEvent := nil;
fListChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  Destroy;
begin
ResizeListArrays(0);
fListCount := 0;
fListChangingEvent := nil;
fListChangingCallback := nil;
fListChangeEvent := nil;
fListChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  LowList: Integer;
begin
Result := 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  HighList: Integer;
begin
Result := Pred(fListCount);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  CheckList(List: Integer): Boolean;
begin
Result := (List >= LowList) and (List <= HighList);
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListBaseObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListBaseObject{$ELSE}TCustomMultiListBaseObject{$IFEND}.
  MakeListChange(List: Integer);
begin
If CheckList(List) then
  begin
    DoListChanging(List);
    DoListChange(List);
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.MakeListChange: Invalid list (%d).',[StaticClassName,List]);
end;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)MultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedMultiListObject'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedMultiListObject'
{$ELSE}
  'TCustomMultiListObject'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  ResizeListArrays(NewLength: Integer);
var
  OldLength:  Integer;
  i:          Integer;
begin
inherited;
OldLength := Length(fListResizeSettings);
SetLength(fListResizeSettings,NewLength);
If NewLength > OldLength then
  For i := OldLength to High(fListResizeSettings) do
    fListResizeSettings[i] := DefaultListResizeSettings;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListResizeSettings(List: Integer): TListResizeSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListResizeSettings(List: Integer; Value: TListResizeSettings);
begin
If CheckList(List) then
  fListResizeSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListResizeSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListResizeSettingsPtr(List: Integer): PListResizeSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings(List: Integer): TGrowSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings(List: Integer; Value: TGrowSettings);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettingsPtr(List: Integer): PGrowSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Grow)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Mode(List: Integer): TGrowMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Mode(List: Integer; Value: TGrowMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Init(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Init
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Init: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Init(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Init := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Init: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Delta(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Delta(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Limit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Limit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings(List: Integer): TShrinkSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings(List: Integer; Value: TShrinkSettings);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettingsPtr(List: Integer): PShrinkSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Shrink)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Mode(List: Integer): TShrinkMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Delta(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Delta(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Limit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Limit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Grow(List: Integer; MinSpace: Integer = 1);
var
  NewCapacity:  Integer;
begin
If CheckList(List) then
  begin
    If GrowResolve(Capacity[List],Count[List],MinSpace,fListResizeSettings[List].Grow,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Grow: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Shrink(List: Integer);
var
  NewCapacity:  Integer;
begin
If CheckList(List) then
  begin
    If ShrinkResolve(Capacity[List],Count[List],fListResizeSettings[List].Shrink,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Shrink: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetItemChangeStatePtr(List: Integer; Index: Integer): PChangesTrackingState;
begin
ConsumeArgs([List,Index]);
fItemChangeStateSink := ChangesTrackingStateInit;
Result := @fItemChangeStateSink;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetItemChangeCounter(List: Integer; Index: Integer): Integer;
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Index (%d) out of bounds.',[StaticClassName,Index]);
Result := GetItemChangeStatePtr(List,Index)^.ChangeCounter;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetItemChanged(List: Integer; Index: Integer): Boolean;
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Index (%d) out of bounds.',[StaticClassName,Index]);
Result := GetItemChangeStatePtr(List,Index)^.ChangeCounter > 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  DoItemChanging(List: Integer; Index: Integer; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedList:  Integer;
  OldPropagatedItem:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Invalid list (%d).',[StaticClassName,List]);
    If not CheckIndex(List,Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(List,Index);
    If not ItemChangeStatePtr^.ChangingDone then
      begin
        If Assigned(fItemChangingEvent) then
          begin
            fItemChangingEvent(Self,List,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangingCallback) then
              fItemChangingCallback(Self,List,Index);
          end
        else If Assigned(fItemChangingCallback) then
          fItemChangingCallback(Self,List,Index);
        ItemChangeStatePtr^.ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    // change propagation
    If ImmediateCall then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          OldPropagatedItem := fPropagatedItem;
          try
            fPropagatedItem := Index;
            case fPropagateItemChanges of
              prpToGlobal:  DoChanging;
              prpToList:    DoListChanging(List);
              prpToBoth:    begin
                              DoListChanging(List);
                              If not fPropagateListChanges then
                                DoChanging;
                            end;
            else
             {prpNone}  // do nothing
            end;
          finally
            fPropagatedItem := OldPropagatedItem;
          end;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  DoItemChange(List: Integer; Index: Integer; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedList:  Integer;
  OldPropagatedItem:  Integer;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Invalid list (%d).',[StaticClassName,List]);
    If not CheckIndex(List,Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(List,Index);
    If ImmediateCall and (ItemChangeStatePtr^.ChangeCounter < High(Integer)) then
      Inc(ItemChangeStatePtr^.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fItemChangeEvent) then
          begin
            fItemChangeEvent(Self,List,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangeCallback) then
              fItemChangeCallback(Self,List,Index);
          end
        else If Assigned(fItemChangeCallback) then
          fItemChangeCallback(Self,List,Index);
        ItemChangeStatePtr^.ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          OldPropagatedItem := fPropagatedItem;
          try
            fPropagatedItem := Index;
            case fPropagateItemChanges of
              prpToGlobal:  DoChange;
              prpToList:    DoListChange(List);
              prpToBoth:    begin
                              DoListChange(List);
                              If not fPropagateListChanges then
                                DoChange;
                            end;
            else
             {prpNone}  // do nothing
            end;
          finally
            fPropagatedItem := OldPropagatedItem;
          end;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  ClearStates;
var
  List: Integer;
  i:    Integer;
begin
inherited;
For List := LowList to HighList do
  For i := LowIndex(List) to HighIndex(List) do
    GetItemChangeStatePtr(List,i)^ := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  BeginningUpdate;
var
  List: Integer;
  i:    Integer;
begin
inherited;
For List := LowList to HighList do
  For i := LowIndex(List) to HighIndex(List) do
    GetItemChangeStatePtr(List,i)^ := ChangesTrackingStateInit;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  EndingUpdate;
var
  List:               Integer;
  i:                  Integer;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For List := LowList to HighList do
  For i := LowIndex(List) to HighIndex(List) do
    begin
      ItemChangeStatePtr := GetItemChangeStatePtr(List,i);
      If ItemChangeStatePtr^.ChangeCounter > 0 then
        DoItemChange(List,i,False);
      ItemChangeStatePtr^ := ChangesTrackingStateInit;
    end;
inherited;
end;

//------------------------------------------------------------------------------
  
procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
var
  List:               Integer;
  i:                  Integer;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For List := LowList to HighList do
  For i := LowIndex(List) to HighIndex(List) do
    begin
      ItemChangeStatePtr := GetItemChangeStatePtr(List,i);
      If ReportChanges and (ItemChangeStatePtr^.ChangeCounter > 0) then
        DoItemChange(List,i,False);
      ItemChangeStatePtr^ := ChangesTrackingStateInit;
    end;
inherited;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Create(ListCount: Integer);
begin
inherited Create(ListCount);
fPropagatedItem := -1;
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Destroy;
begin
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  CheckIndex(List,Index: Integer): Boolean;
begin
Result := (Index >= LowIndex(List)) and (Index <= HighIndex(List));
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND});
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListResizeSettings[i] := Source.ListResizeSettings[i];
  end
else raise EACIncompatibleClass.CreateFmt('%s.CopyListResizeSettings: List count mismatch (%d,%d).',[StaticClassName,Self.ListCount,Source.ListCount]);
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  MakeItemChange(List: Integer; Index: Integer);
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
DoItemChanging(List,Index);
DoItemChange(List,Index);
end;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiList64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiList64Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedMultiList64Object'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedMultiList64Object'
{$ELSE}
  'TCustomMultiList64Object'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  ResizeListArrays(NewLength: Integer);
var
  OldLength:  Integer;
  i:          Integer;
begin
inherited;
OldLength := Length(fListResizeSettings);
SetLength(fListResizeSettings,NewLength);
If NewLength > OldLength then
  For i := OldLength to High(fListResizeSettings) do
    fListResizeSettings[i] := DefaultListResizeSettings64;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetListResizeSettings(List: Integer): TListResizeSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetListResizeSettings(List: Integer; Value: TListResizeSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListResizeSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetListResizeSettingsPtr(List: Integer): PListResizeSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings(List: Integer): TGrowSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings(List: Integer; Value: TGrowSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettingsPtr(List: Integer): PGrowSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Grow)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Mode(List: Integer): TGrowMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Mode(List: Integer; Value: TGrowMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Init(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Init
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Init: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Init(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Init := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Init: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Delta(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Delta(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Limit(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Limit(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings(List: Integer): TShrinkSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings(List: Integer; Value: TShrinkSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettingsPtr(List: Integer): PShrinkSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Shrink)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettingsPtr: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Mode(List: Integer): TShrinkMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Mode: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Delta(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Delta(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Delta: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Factor: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Limit(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Limit(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Limit: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Grow(List: Integer; MinSpace: Int64 = 1);
var
  NewCapacity:  Int64;
begin
If CheckList(List) then
  begin
    If GrowResolve(Capacity[List],Count[List],MinSpace,fListResizeSettings[List].Grow,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Grow: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Shrink(List: Integer);
var
  NewCapacity:  Int64;
begin
If CheckList(List) then
  begin
    If ShrinkResolve(Capacity[List],Count[List],fListResizeSettings[List].Shrink,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Shrink: Invalid list (%d).',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetItemChangeStatePtr(List: Integer; Index: Int64): PChangesTrackingState;
begin
ConsumeArgs([List,Index]);
fItemChangeStateSink := ChangesTrackingStateInit;
Result := @fItemChangeStateSink;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetItemChangeCounter(List: Integer; Index: Int64): Integer;
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChangeCounter: Index (%d) out of bounds.',[StaticClassName,Index]);
Result := GetItemChangeStatePtr(List,Index)^.ChangeCounter;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetItemChanged(List: Integer; Index: Int64): Boolean;
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.GetItemChanged: Index (%d) out of bounds.',[StaticClassName,Index]);
Result := GetItemChangeStatePtr(List,Index)^.ChangeCounter > 0;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  DoItemChanging(List: Integer; Index: Int64; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedList:  Integer;
  OldPropagatedItem:  Int64;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Invalid list (%d).',[StaticClassName,List]);
    If not CheckIndex(List,Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChanging: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(List,Index);
    If not ItemChangeStatePtr^.ChangingDone then
      begin
        If Assigned(fItemChangingEvent) then
          begin
            fItemChangingEvent(Self,List,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangingCallback) then
              fItemChangingCallback(Self,List,Index);
          end
        else If Assigned(fItemChangingCallback) then
          fItemChangingCallback(Self,List,Index);
        ItemChangeStatePtr^.ChangingDone := fUpdateCounter > 0;
        Result := True;
      end;
    // change propagation
    If ImmediateCall then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          OldPropagatedItem := fPropagatedItem;
          try
            fPropagatedItem := Index;
            case fPropagateItemChanges of
              prpToGlobal:  DoChanging;
              prpToList:    DoListChanging(List);
              prpToBoth:    begin
                              DoListChanging(List);
                              If not fPropagateListChanges then
                                DoChanging;
                            end;
            else
             {prpNone}  // do nothing
            end;
          finally
            fPropagatedItem := OldPropagatedItem;
          end;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  DoItemChange(List: Integer; Index: Int64; ImmediateCall: Boolean = True): Boolean;
var
  ItemChangeStatePtr: PChangesTrackingState;
  OldPropagatedList:  Integer;
  OldPropagatedItem:  Int64;
begin
Result := False;
If not fDisableChangeTracking then
  begin
    If not CheckList(List) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Invalid list (%d).',[StaticClassName,List]);
    If not CheckIndex(List,Index) then
      raise EACIndexOutOfBounds.CreateFmt('%s.DoItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
    ItemChangeStatePtr := GetItemChangeStatePtr(List,Index);
    If ImmediateCall and (ItemChangeStatePtr^.ChangeCounter < High(Integer)) then
      Inc(ItemChangeStatePtr^.ChangeCounter);
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fItemChangeEvent) then
          begin
            fItemChangeEvent(Self,List,Index);
            If fCallAllChangeHandlers and Assigned(fItemChangeCallback) then
              fItemChangeCallback(Self,List,Index);
          end
        else If Assigned(fItemChangeCallback) then
          fItemChangeCallback(Self,List,Index);
        ItemChangeStatePtr^.ChangeCounter := 0;
        Result := True;
      end;
    If ImmediateCall then
      begin
        OldPropagatedList := fPropagatedList;
        try
          fPropagatedList := List;
          OldPropagatedItem := fPropagatedItem;
          try
            fPropagatedItem := Index;
            case fPropagateItemChanges of
              prpToGlobal:  DoChange;
              prpToList:    DoListChange(List);
              prpToBoth:    begin
                              DoListChange(List);
                              If not fPropagateListChanges then
                                DoChange;
                            end;
            else
             {prpNone}  // do nothing
            end;
          finally
            fPropagatedItem := OldPropagatedItem;
          end;
        finally
          fPropagatedList := OldPropagatedList;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  ClearStates;
var
  List: Integer;
  i:    Int64;
begin
inherited;
For List := LowList to HighList do
  begin
    i := LowIndex(List);
    while i <= HighIndex(List) do
      begin
        GetItemChangeStatePtr(List,i)^ := ChangesTrackingStateInit;
        Inc(i);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  BeginningUpdate;
var
  List: Integer;
  i:    Int64;
begin
inherited;
For List := LowList to HighList do
  begin
    i := LowIndex(List);
    while i <= HighIndex(List) do
      begin
        GetItemChangeStatePtr(List,i)^ := ChangesTrackingStateInit;
        Inc(i);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  EndingUpdate;
var
  List:               Integer;
  i:                  Int64;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For List := LowList to HighList do
  begin
    i := LowIndex(List);
    while i <= HighIndex(List) do
      begin
        ItemChangeStatePtr := GetItemChangeStatePtr(List,i);
        If ItemChangeStatePtr^.ChangeCounter > 0 then
          DoItemChange(List,i,False);
        ItemChangeStatePtr^ := ChangesTrackingStateInit;
      end;
  end;
inherited;
end;

//------------------------------------------------------------------------------
  
procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  BreakingUpdate(ReportChanges: Boolean);
var
  List:               Integer;
  i:                  Int64;
  ItemChangeStatePtr: PChangesTrackingState;
begin
For List := LowList to HighList do
  begin
    i := LowIndex(List);
    while i <= HighIndex(List) do
      begin
        ItemChangeStatePtr := GetItemChangeStatePtr(List,i);
        If ReportChanges and (ItemChangeStatePtr^.ChangeCounter > 0) then
          DoItemChange(List,i,False);
        ItemChangeStatePtr^ := ChangesTrackingStateInit;
    end;
  end;
inherited;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Create(ListCount: Integer);
begin
inherited Create(ListCount);
fPropagatedItem := -1;
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Destroy;
begin
fItemChangingEvent := nil;
fItemChangingCallback := nil;
fItemChangeEvent := nil;
fItemChangeCallback := nil;
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  CheckIndex(List: Integer; Index: Int64): Boolean;
begin
Result := (Index >= LowIndex(List)) and (Index <= HighIndex(List));
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND});
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListResizeSettings[i] := Source.ListResizeSettings[i];
  end
else raise EACIncompatibleClass.CreateFmt('%s.CopyListResizeSettings: List count mismatch (%d,%d).',[StaticClassName,Self.ListCount,Source.ListCount]);
end;

{$IFDEF Debug}
//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  MakeItemChange(List: Integer; Index: Int64);
begin
If not CheckList(List) then
  raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Invalid list (%d).',[StaticClassName,List]);
If not CheckIndex(List,Index) then
  raise EACIndexOutOfBounds.CreateFmt('%s.MakeItemChange: Index (%d) out of bounds.',[StaticClassName,Index]);
DoItemChanging(List,Index);
DoItemChange(List,Index);
end;

{$ENDIF}

{$ENDIF}
