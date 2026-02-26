{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  ListUtils

    Small library designed to provide utilities and some other auxiliry
    facilities for management of lists or arrays.

    Currently only parametrized list growing and shrinking is implemented,
    more may be added later.
    But be noted that there is no plan on merging with list sorting from
    library ListSorters (github.com/TheLazyTomcat/Lib.ListSorters).

  Version 1.0 (2026-02-25)

  Last change (2026-02-25)

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.ListUtils

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol ListUtils_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{!tun_end!} // ignore this line
unit ListUtils;
{
  ListUtils_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  ListUtils_UseAuxExceptions to achieve this.
}
{$IF Defined(ListUtils_UseAuxExceptions)}
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
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions     
===============================================================================}
type
  ELUException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ELUResizeException = class(ELUException);

  ELUInvalidValue     = class(ELUResizeException);
  ELUInvalidOperation = class(ELUResizeException);
  ELUInvalidResult    = class(ELUResizeException);
  ELUExceedingLimit   = class(ELUResizeException);
  ELUResultOverflow   = class(ELUResizeException);

{===============================================================================
--------------------------------------------------------------------------------
                           List growing and shrinking
--------------------------------------------------------------------------------
===============================================================================}
{
  When adding new items to a list, it might be necessary to first increase
  capacity (storage space) of the list, either because the current capacity is
  already full, or the list is empty and therefore has no capacity allocated
  yet.
  Naive approach would be to increase the capacity (grow the list) by number of
  items being added (often one). But since these operations usually require
  memory reallocation, which is slow, doing it this way (ie. seperately for
  each item added) is a bad idea, as it would incur serious performance
  degradation, and maybe even problems with memory fragmentation.
  The solution is to increase the capacity by (way) more than is needed at the
  moment, effectively preallocating space for future additions, trading memory
  consumption for better performance.

  Similar problem arises when items are deleted. Again, lowering capacity
  (shrinking) after each deletion is not good, so we instead keep the capacity
  until there is significantly more free space and only then do the resizing.

  Following types, constants and functions prowide a way of calculating whether
  there is time to grow or shrink the list and by how much (or, more precisely,
  they provide new capacity) given current state (count, capacity) and selected
  growing/shrinking algorithm and parameters.
}
//------------------------------------------------------------------------------
{
  TGrowMode

  This enumeration is used to select algorithm that will calculate new capacity
  for list that needs to be grown. The algorithms are as follows:

    gmDefault

      Corresponds to gmFast, see there for details.

    gmNone

      Disables growing. If list needs to be grow, then an ELUInvalidOperation
      exception is raised. When this mode is used but list does not need to
      be grown (ie. its capacity is already large enough to provide required
      minimum space), then nothing happens (no exception is raised).

    gmToLimit

      Sets new capacity to provided grow limit (TGrowSettings.Limit). If
      current count plus minimum required space is larger than this limit,
      then an ELUExceedingLimit exception is raised.

    gmSlow

      If current capacity is zero, then new capacity is set to grow init
      (TGrowSettings.Init) or to minimum required space, whichever is larger.
      If current capacity is non-zero, then new capacity is set to current
      count plus minimum required space (which is usually 1).

    gmSlowLimited

      Works the same as gmSlow, but if minimum required space plus current
      count exceeds grow limit, then an ELUExceedingLimit exception is raised.
      If current capacity is zero and grow limit is set to a lower value than
      grow init, then initial capacity is selected from this limit and minimum
      required space, whichever is larger (ie. grow init is ignored).

    gmLinear

      If current capacity is zero, then new capacity is set to grow init or
      to minimum required space, whichever is larger.
      For capacity above zero, the new capacity is calculated by adding as many
      grow delta (TGrowSettings.Delta) to current capacity as is needed for it
      to reach a value that is large enough to provide minimum required space.
      If this results to a situation where adding the last full grow delta
      would cause the new capacity to overflow, then the new capacity is set
      to absolute maximum it can hold (eg. High(Integer)).

    gmLinearLimited

      Similar to gmLinear, but if minimum required space plus current count
      exceeds grow limit, then an ELUExceedingLimit exception is raised.
      If adding the last grow delta in full would exceed grow limit, but the
      limit in itself provides enough free space, then new capacity is set to
      this limit.
      If current capacity is zero and grow limit is set to a lower value than
      grow init, then initial capacity is selected from this limit and minimum
      required space, whichever is larger (ie. grow init is ignored).

    gmLinearFixed

      If current count plus minimum required space is below or equal to grow
      init, then new capacity is set to grow init.
      If above grow init, then new capacity is calculated by adding grow delta
      to grow init as many times as is needed to produce a value that provides
      minimum required space.
      If this new value is larger than what the capacity type can hold (eg.
      larger then High(Integer)), then absolute maximum value a capacity can
      have is selected as a new capacity.
      Difference to gmLinear is this - gmLinear calculated new capacity relative
      to current capacity, whereas gmLinearFixed is using fixed points on number
      line given by grow init and grow delta, and current capacity is not
      affecting the calculation. This can be used eg. if you want the capacity
      to be grow always to some threshold, irrespective of whether capacity of
      the list was changed by other means to some arbitrary value or not.

    gmLinearFixedLimited

      Works the same as gmLinearFixed, but Count + MinSpace must not exceed
      grow limit, otherwise an ELUExceedingLimit exception is raised.
      Also, if adding full grow delta would exceed limit, but this limit in
      itself provide enough space, then new capacity is set to this grow limit.
      If grow limit is set to be smaller than grow init, then new capacity is
      always set to grow limit.

    gmFast

      For current capacity of zero, it selects initial capacity from grow init
      and minimum required space, whichever is larger.
      For non-zero capacity, it multiplies existing capacity by grow factor
      (TGrowSettings.Factor) as many times as is needed to reach a value that
      provides enough free space for passed minimum required space.
      If this multiplication produces number larger than maximum value for
      given type (eg. High(Integer)), then new capacity is set to this maximum.
      If grow factor is so small that multiplying current capacity by it would
      not produce larger integral number, then current capacity + 1 is taken
      instead.

        WARNING - since these calculations involve floating point numbers,
                  they might produce inprecise/unexpected results, but the
                  new capacity will always be set correctly as to satisfy
                  minimum required space.

    gmFastLimited

      Works similarly to gmFast with exactly the same differences to it as has
      gmLinearLimited when compared to gmLinear - see there for details.

    gmFastFixed

      If count plus minimum required space is smaller than or equal to grow
      init, then new capacity is set to grow init.
      If above the grow init, then the new capacity is calculated by repeatedly
      multiplying the grow init by grow factor until a value satisfying the
      required minimum space is obtained.
      If number larger than what the capacity can hold is obtained this way,
      then new capacity is set to absolute maximum the capacity can hold.
      If grow factor is so small that it could not produce meaningful result,
      then grow init plus one is taken instead and the multiplication repeats
      from there.

    gmFastFixedLimited

      Works identically to gmFastFixed with differences to it being the same as
      are between gmLinearFixedLimited and gmLinearFixed, see their description
      for details.

    gmFastAttenuated

      A combination of fast and linear growth. It grows the capacity using
      fast algorithm (see gmFast) until the grow limit is reached (it is not
      exceeded by this stage), then switches to linear growth (see gmLinear).

    gmLinearAccelerated

      Combination of linear and fast growth. Until the grow limit is reached,
      it grows the capacity linearly (limit is not exceeded by this stage),
      and from the limit up switches to fast growth.

    gmCustomCallback

      Function provided in field TGrowSettings.Custom.Callback is called with
      current state (count, capacity, minimum required space) and given context
      (TGrowSettings.Custom.Context) - you can use the context for you own
      purposes, it can point to anything.
      This function is expected to calcuate and return the new capacity. If
      this callback function is not assigned then an ELUInvalidValue exception
      is raised. If the returned new capacity is not large enough to satisfy
      minimum required space, then an ELUInvalidResult exception is raised.
      It is meant for situations where you need to supply your own growing
      algorithm.

    gmCustomEvent

      Works exactly the same as gmCustomCallback (see there for details),
      but instead of callback function it calls method/event provided in
      TGrowSettings.Custom.Event.
}
type
  TGrowMode = (gmDefault,gmNone,gmToLimit,gmSlow,gmSlowLimited,gmLinear,
               gmLinearLimited,gmLinearFixed,gmLinearFixedLimited,gmFast,
               gmFastLimited,gmFastFixed,gmFastFixedLimited,gmFastAttenuated,
               gmLinearAccelerated,gmCustomCallback,gmCustomEvent);

{
  TShrinkMode

  Enumeration used to select an algorithm that will compute new capacity when
  list should be shrinked.

    smDefault

      Corresponds to smFastLimitedZero, see there for details.

    smNone

      Disables shrinking. When Count of the list is lower than Capacity,
      meaning the list could be shrinked, then this algorithm raises an
      ELUInvalidOperation exception.
      If the list does not need to be shrinked (ie. Count and Capacity are
      equal), then nothing happens and no exception is raised.

    smKeepCap

      Keeps current capacity, ie. returned new capacity is always equal to
      value passed in parameter Capacity. This effectively disables shrinking,
      but, unlike smNone, without raising an exception.

    smToCount

      Returned new capacity is set to provided current count (value of
      parameter Count).

    smLinear

      If current count is zero, then new capacity is also set to zero.
      If larger than zero, then as many shrink deltas (TShrinkSettings.Delta)
      is subtracted from current capacity as is needed to bring it closer than
      one shrink delta to current count, sattisfying following inequality:

          (NewCapacity - Count) < TShrinkSettings.Delta

    smLinearLimited

      Works the same as smLinear (see there for details), but the new capacity
      is newer set lower than shrink limit (TShrinkSettings.Limit).
      If current capacity is already below shrink limit, then it is kept as is
      and is not changed.

    smLinearLimitedZero

      Works the same as smLinearLimited, but when Count is zero, then new
      capacity is also set to zero, even if shrink limit is non-zero and
      technically does not allow it.
      This exists for situation where the list is known to be empty most of
      its existence, as to lower memory consumption.

    smLinearFixed

      If current count is zero, then new capacity is also set to zero.
      When it is non-zero, then new capacity is set to a closest threshold
      that is not smaller than current count and is an integral multiple of
      shrink delta.

    smLinearFixedLimited

      If current count is at or below shrink limit, then new capacity is set
      to this limit.
      For current count above this limit, the capacity is set to a value that
      is not smaller than count and at the same time is an integral multiple
      of shrink delta when counted from given shrink limit.
      If current capacity is already below the limit, then it is not changed.

    smLinearFixedLimitedZero

      Works the same as smLinearFixedLimited, but for current count of zero
      sets new capacity also to zero.

    smFast

      Sets new capacity to zero for current count of zero.
      For count above zero, it multiplies current capacity with shrink factor
      (TShrinkSettings.Factor) as many times so that another multiplication
      would bring it below the count and returns this new value. Note that
      shrink factor must be from interval (0,1), meaning multiplying a number
      with it will effectively result in division by factor's reciprocal.
      If the algorithm encounters some problem (eg. becuase of rounding or
      precision errors), then a contingency calculation is run instead. This
      will technically work the same, but is done iteratively and each step
      is guaranteed to lower the capacity by at least one - for more details
      please refer to source code or contact the author.

        WARNING - as this algorithm works with floating point numbers, it can
                  produce inprecise or unexpected results, but the new capacity
                  will always be set as to not cause exceptions or some hidden
                  problems.

    smFastLimited

      Works the same as smFast, but the new capacity will never be set lower
      than shrink limit.
      If current capacity is already below this limit, then it is not changed.

    smFastLimitedZero

      Same as smFastLimited, the only difference is that for current count of
      zero it sets new capacity also to zero, circumventing the shrink limit.

    smCustomCallback

      Calls function provided in TShrinkSettings.Custom.Callback, passing
      current state (Count, Capacity) and value of context provided in field
      TShrinkSettings.Custom.Context. Context is an opaque pointer that is
      provided for your purposes, you can use it however you want.
      The called function must be assigned, otherwise an ELUInvalidValue
      exception is raised, and must calculate new capacity from provided data
      and return it in output parameted NewCapacity. Returned new capacity
      must not be larger than current capacity and also must not be smaller
      than current count. If it does not fulfill these expectations then an
      exception of class ELUInvalidResult is raised.
      This mode is here for situations where you need to provide your own
      implementation of list shrinking.

    smCustomEvent

      Works the same as smCustomCallback (see there for more information),
      but calls method provided in TShrinkSettings.Custom.Event instead of
      callback.
}
type
  TShrinkMode = (smDefault,smNone,smKeepCap,smToCount,smLinear,smLinearLimited,
                 smLinearLimitedZero,smLinearFixed,smLinearFixedLimited,
                 smLinearFixedLimitedZero,smFast,smFastLimited,smFastLimitedZero,
                 smCustomCallback,smCustomEvent);

//------------------------------------------------------------------------------
{
  TGrowCallback
  TGrowEvent

  Signatures for user-provided callback functions that are meant to calculate
  new capacity from provided state values and possibly some more data provided
  in context when gmCustomCallback or gmCustomEvent grow mode is selected.

  Argument context contains value stored in field TGrowSettings.Custom.Context.
  It can be set to any pointer, for example it can point back to grow settings
  structure itself if you want to use parameters stored there.
}
type
  TGrowCallback = procedure(Context: Pointer; Capacity,Count,MinSpace: Integer; out NewCapacity: Integer);
  TGrowEvent    = procedure(Context: Pointer; Capacity,Count,MinSpace: Integer; out NewCapacity: Integer) of object;

{
  TGrowSettings

  This structure is used to pass selected grow mode (algorithm) and its
  parameters to function calculating new capacity (see function GrowResolve).

  Field UserData is not, and will not be, used by any implemented algorithm,
  you can use it for your own purposes. For meaning and purpose of other fields,
  refer to description of types TGrowMode, TGrowCallback and TGrowEvent.

  When any field is used during calculation of new capacity, it is first
  checked for validity (only fields used by selected grow mode are checked,
  unused fields are ignored).
  Fields Init, Delta and Limit must be larger than zero, field Factor must be
  larger than one and smaller or equal to one million (very large Factor can
  lead to unexpected results and floating point exceptions) and fields Callback
  and/or Event must be assigned.
  An ELUInvalidValue exception is raised if stored value fails the check.
}
  TGrowSettings = record
    Mode:     TGrowMode;
    Init:     Integer;
    Delta:    Integer;
    Factor:   Double;
    Limit:    Integer;
    Custom:   record
      Context:        Pointer;
      case Integer of
        0: (Callback: TGrowCallback);
        1: (Event:    TGrowEvent);
    end;
    UserData: PtrInt;
  end;
  PGrowSettings = ^TGrowSettings;

const
  DefaultGrowSettings: TGrowSettings = (
    Mode:     gmDefault{gmFast};
    Init:     32;
    Delta:    16;                 // unused for selected mode
    Factor:   2.0;
    Limit:    128 * 1024 * 1024;  // unused, 512MiB for array of integers
    Custom:   (                   // unused
      Context:  nil;
      Event:    nil);             // fully overlays variant field Callback
    UserData: 0);

{
  TShrinkCallback
  TShrinkEvent

  Signatures for user-provided callback functions calculating new capacity
  when shrinking the list and either smCustomCallback or smCustomEvent shrink
  mode is selected.

  Similarly to grow callbacks, Context is set to value stored in field
  TShrinkSettings.Custom.Context and can be set to any pointer, eg. to point
  to the shrink settings structure itself.
}
type
  TShrinkCallback = procedure(Context: Pointer; Capacity,Count: Integer; out NewCapacity: Integer);
  TShrinkEvent    = procedure(Context: Pointer; Capacity,Count: Integer; out NewCapacity: Integer) of object;

{
  TShrinkSettings

  Structure storing selected shrink mode along with its parameters, used eg.
  to pass complete shrink settings into calcuating functions (see function
  ShrinkResolve).

  As in TGrowSettings, field UserData can be used for any purpose as it is not
  touched internally. For meaning and purpose of other fields, refer to
  description of types TShrinkMode, TShrinkCallback and TShrinkEvent.

  Fields used during calculation (only those needed by selected shrink mode)
  are checked for validity.
  Field Delta must be larger than zero, Limit must not be negative (it can
  be zero), Factor must be from open interval (0,1) (ie. 0 < Factor < 1) and
  fields Callback and/or Event must be assigned.
  An ELUInvalidValue exception is raised if stored value fails the check.
}
  TShrinkSettings = record
    Mode:     TShrinkMode;
    Delta:    Integer;
    Factor:   Double;
    Limit:    Integer;
    Custom:   record
      Context:        Pointer;
      case Integer of
        0: (Callback: TShrinkCallback);
        1: (Event:    TShrinkEvent);
    end;
    UserData: PtrInt;
  end;
  PShrinkSettings = ^TShrinkSettings;

const
  DefaultShrinkSettings: TShrinkSettings = (
    Mode:     smDefault{smFastLimitedZero};
    Delta:    16;       // unused for selected mode
    Factor:   0.5;
    Limit:    32;
    Custom:   (         // unused
      Context:  nil;
      Event:    nil);   // overlays field Callback
    UserData: 0);

{
  TListResizeSettings

  Simple structure that is just a conglomerate of grow and shrink settings.
  You can use it to store all in one place or for example to expose both
  settings as a single entity (eg. as object property).
}
type
  TListResizeSettings = record
    Grow:   TGrowSettings;
    Shrink: TShrinkSettings;
  end;
  PListResizeSettings = ^TListResizeSettings;

Function DefaultListResizeSettings: TListResizeSettings;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  64bit settings are intended for lists that need to have more than about
  two billion (2x10^9, exactly 2^31) entries/items, which is a limitation
  of normally used 32bit types - such lists will be rare, but I am sure they
  exist somewhere.

    WARNING - these types must NOT be seen as default types when compiling
              for 64bit platforms, they are really meant only for use on
              special massive lists.

  For description of individual types, refer to their corresponding standard
  (32bit) counterparts.
}
type
  TGrowCallback64 = procedure(Context: Pointer; Capacity,Count,MinSpace: Int64; out NewCapacity: Int64);
  TGrowEvent64    = procedure(Context: Pointer; Capacity,Count,MinSpace: Int64; out NewCapacity: Int64) of object;

  TGrowSettings64 = record
    Mode:     TGrowMode;
    Init:     Int64;
    Delta:    Int64;
    Factor:   Double;
    Limit:    Int64;
    Custom:   record
      Context:        Pointer;
      case Integer of
        0: (Callback: TGrowCallback64);
        1: (Event:    TGrowEvent64);
    end;
    UserData: PtrInt;
  end;
  PGrowSettings64 = ^TGrowSettings64;

const
  DefaultGrowSettings64: TGrowSettings64 = (
    Mode:     gmDefault{gmFast};
    Init:     32;
    Delta:    16;
    Factor:   2.0;
    Limit:    128 * 1024 * 1024;
    Custom:   (
      Context:  nil;
      Event:    nil);           
    UserData: 0);

type
  TShrinkCallback64 = procedure(Context: Pointer; Capacity,Count: Int64; out NewCapacity: Int64);
  TShrinkEvent64    = procedure(Context: Pointer; Capacity,Count: Int64; out NewCapacity: Int64) of object;

  TShrinkSettings64 = record
    Mode:     TShrinkMode;
    Delta:    Int64;
    Factor:   Double;
    Limit:    Int64;
    Custom:   record
      Context:        Pointer;
      case Integer of
        0: (Callback: TShrinkCallback64);
        1: (Event:    TShrinkEvent64);
    end;
    UserData: PtrInt;
  end;
  PShrinkSettings64 = ^TShrinkSettings64;

const
  DefaultShrinkSettings64: TShrinkSettings64 = (
    Mode:     smDefault{smFastLimitedZero};
    Delta:    16;
    Factor:   0.5;
    Limit:    32;
    Custom:   (
      Context:  nil;
      Event:    nil);  
    UserData: 0);

type
  TListResizeSettings64 = record
    Grow:   TGrowSettings64;
    Shrink: TShrinkSettings64;
  end;
  PListResizeSettings64 = ^TListResizeSettings64;

Function DefaultListResizeSettings64: TListResizeSettings64;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
    List growing and shrinking - grow declaration
===============================================================================}
{
  GrowResolve

  Calculates new capacity of a list or array that is required for addition
  of given number (MinSpace) of new elements depending on current state
  (Capacity, Count) and provided grow settings (GrowSettings).

  The calculated capacity is returned in output parameter NewCapacity and
  result of this function indicates whether this number actually differs
  from provided current capacity (when they differ, true is returend, false
  when they are the same) - if the current capacity is large enough for
  MinSpace (Count + MinSpace <= Capacity), then the new capacity can stay
  the same as old capacity and no calculation is actually performed (this
  is important, because the fact that nothing is calculated can have, depending
  on selected grow mode, some side effects).

  Arguments Capacity, Count and MinSpace are always checked for validity -
  Capacity and Count must be zero or larger, Capacity must be larger or equal
  to Count, MinSpace must be at least 1 and also must not be too large to cause
  overflow when added to Count. If any of these sanity checks fail, then an
  ELUInvalidValue exception is raised.

  Fields of GrowSettings record are also checked, but only if actual calculation
  takes place and then only those fields needed for selected grow mode (see
  description of type TGrowMode to see which fields are needed for which grow
  mode). For applicable limits, refer to description of type TGrowSettings.
}
Function GrowResolve(Capacity,Count,MinSpace: Integer; const GrowSettings: TGrowSettings; out NewCapacity: Integer): Boolean; overload;
Function GrowResolve(Capacity,Count,MinSpace: Int64; const GrowSettings: TGrowSettings64; out NewCapacity: Int64): Boolean; overload;

{===============================================================================
    List growing and shrinking - shrink declaration
===============================================================================}
{
  ShrinkResolve

  Use these funtion to calculate new capacity when there is a posibility to
  shrink the list, eg. when item was deleted. Result of this calculation is
  returned in output parameter NewCapacity and its value depends on current
  list state (Capacity, Count) and selected shrink settings (ShrinkSettings).
  Note that no calculation is even attempted if provided Count and Capacity
  match.

  Result indicates whether new capacity differs from current capacity (true is
  returned) or not (false is returned).

  As in case of GrowResolve, input arguments are checked for validity before
  anything is done - both Capacity and Count must be larger than zero and Count
  must not be larger than Capacity, otherwise an ELUInvalidValue exception is
  raised.

  Fields of ShrinkSettings are checked for validity only when any calculation
  takes place, and then only those used by it (see description of type
  TShrinkSettings for more info).
}
Function ShrinkResolve(Capacity,Count: Integer; const ShrinkSettings: TShrinkSettings; out NewCapacity: Integer): Boolean; overload;
Function ShrinkResolve(Capacity,Count: Int64; const ShrinkSettings: TShrinkSettings64; out NewCapacity: Int64): Boolean; overload;

implementation

uses
  Math;

// preventively undefine template symbols 
{$UNDEF List64}
{$UNDEF GrowResolve}
{$UNDEF ShrinkResolve}

{===============================================================================
--------------------------------------------------------------------------------
                           List growing and shrinking
--------------------------------------------------------------------------------
===============================================================================}

Function DefaultListResizeSettings: TListResizeSettings;
begin
Result.Grow := DefaultGrowSettings;
Result.Shrink := DefaultShrinkSettings;
end;

//------------------------------------------------------------------------------

Function DefaultListResizeSettings64: TListResizeSettings64;
begin
Result.Grow := DefaultGrowSettings64;
Result.Shrink := DefaultShrinkSettings64;
end;

{===============================================================================
    List growing and shrinking - grow implementation
===============================================================================}
type
  TGrowSettingsField = (gsfInit,gsfDelta,gsfFactor,gsfLimit,gsfCustomCallback,
                        gsfCustomEvent);
  TGrowSettingsFields = set of TGrowSettingsField;

//------------------------------------------------------------------------------

{$IF not Declared(Ceil64)}
Function Ceil64(const N: Extended): Int64;
begin
Result := Trunc(N);
If Frac(N) > 0 then
  Result := Result + 1;
end;
{$IFEND}

{-------------------------------------------------------------------------------
    List growing and shrinking - 32bit list growing
-------------------------------------------------------------------------------}

Function GrowResolve(Capacity,Count,MinSpace: Integer; const GrowSettings: TGrowSettings; out NewCapacity: Integer): Boolean;
// implementation included as a teplate
{$DEFINE GrowResolve}{$INCLUDE '.\ListUtils.inc'}{$UNDEF GrowResolve}

{-------------------------------------------------------------------------------
    List growing and shrinking - 64bit list growing
-------------------------------------------------------------------------------}

Function GrowResolve(Capacity,Count,MinSpace: Int64; const GrowSettings: TGrowSettings64; out NewCapacity: Int64): Boolean; 
{$DEFINE List64}{$DEFINE GrowResolve}{$INCLUDE '.\ListUtils.inc'}{$UNDEF GrowResolve}{$UNDEF List64}

{===============================================================================
    List growing and shrinking - shrink implementation
===============================================================================}
type
  TShrinkSettingsField  = (ssfDelta,ssfFactor,ssfLimit,ssfCustomCallback,
                           ssfCustomEvent);
  TShrinkSettingsFields = set of TShrinkSettingsField;

//------------------------------------------------------------------------------

{$IF not Declared(Floor64)}
Function Floor64(const N: Extended): Int64;
begin
Result := Trunc(N);
If Frac(N) < 0 then
  Result := Result - 1;
end;
{$IFEND}

{-------------------------------------------------------------------------------
    List growing and shrinking - 32bit list shrinking
-------------------------------------------------------------------------------}

Function ShrinkResolve(Capacity,Count: Integer; const ShrinkSettings: TShrinkSettings; out NewCapacity: Integer): Boolean;
{$DEFINE ShrinkResolve}{$INCLUDE '.\ListUtils.inc'}{$UNDEF ShrinkResolve}

{-------------------------------------------------------------------------------
    List growing and shrinking - 64bit list shrinking
-------------------------------------------------------------------------------}

Function ShrinkResolve(Capacity,Count: Int64; const ShrinkSettings: TShrinkSettings64; out NewCapacity: Int64): Boolean;
{$DEFINE List64}{$DEFINE ShrinkResolve}{$INCLUDE '.\ListUtils.inc'}{$UNDEF ShrinkResolve}{$UNDEF List64}

end.

