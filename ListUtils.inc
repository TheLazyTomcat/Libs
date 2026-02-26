{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  ListUtils

    This unit contains code templates that are conditionally included
    into main ListUtils unit to produce something similar to generics.

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

{$IFDEF GrowResolve}

  procedure CheckGrowSettingsFields(Fields: TGrowSettingsFields);
  begin
    If gsfInit in Fields then
      If GrowSettings.Init <= 0 then
        raise ELUInvalidValue.CreateFmt('GrowResolve.CheckGrowSettingsFields: Invalid grow initial value (%d).',[GrowSettings.Init]);
    If gsfDelta in Fields then
      If GrowSettings.Delta <= 0 then
        raise ELUInvalidValue.CreateFmt('GrowResolve.CheckGrowSettingsFields: Invalid grow delta (%d).',[GrowSettings.Delta]);
    If gsfFactor in Fields then
      If (GrowSettings.Factor <= 1.0) or (GrowSettings.Factor > 1e6) then
        raise ELUInvalidValue.CreateFmt('GrowResolve.CheckGrowSettingsFields: Invalid grow factor (%g).',[GrowSettings.Factor]);
    If gsfLimit in Fields then
      If GrowSettings.Limit <= 0 then
        raise ELUInvalidValue.CreateFmt('GrowResolve.CheckGrowSettingsFields: Invalid grow limit (%d).',[GrowSettings.Limit]);
    If gsfCustomCallback in Fields then
      If not Assigned(GrowSettings.Custom.Callback) then
        raise ELUInvalidValue.Create('GrowResolve.CheckGrowSettingsFields: Custom callback not assigned.');
    If gsfCustomEvent in Fields then
      If not Assigned(GrowSettings.Custom.Event) then
        raise ELUInvalidValue.Create('GrowResolve.CheckGrowSettingsFields: Custom event not assigned.');
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure CheckGrowSettingsField(Field: TGrowSettingsField);
  begin
    CheckGrowSettingsFields([Field]);
  end;

var
  MinDelta: {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF};
  FloatCap: Extended;
begin
Result := False;
NewCapacity := Capacity;
// sanity checks
If Capacity < 0 then
  raise ELUInvalidValue.CreateFmt('GrowResolve: Ivalid capacity (%d).',[Capacity]);
If Count < 0 then
  raise ELUInvalidValue.CreateFmt('GrowResolve: Ivalid count (%d).',[Count]);
If Capacity < Count then
  raise ELUInvalidValue.CreateFmt('GrowResolve: Capacity (%d) lower than count (%d).',[Capacity,Count]);
If MinSpace < 1 then
  raise ELUInvalidValue.CreateFmt('GrowResolve: Minimum space (%d) too small.',[MinSpace]);
// Count + MinSpace can overflow, so we need to check for that
If MinSpace > (High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) - Count) then
  raise ELUResultOverflow.CreateFmt('GrowResolve: Minimum space (%d) too large.',[MinSpace]);
// check whether we really need to grow the list...
If (Count + MinSpace) > Capacity then
  begin
    case GrowSettings.Mode of

      gmToLimit:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsField(gsfLimit);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          NewCapacity := GrowSettings.Limit;
        end;

      gmSlow:     //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsField(gsfInit);
          If Capacity > 0 then
            // overflow check for Count + MinSpace was already done before
            NewCapacity := Count + MinSpace
          else
            // if MinSpace is larger than Init then we must use IT for initial value
            NewCapacity := Max(GrowSettings.Init,MinSpace);
        end;

      gmSlowLimited:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfLimit]);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          If Capacity > 0 then
            NewCapacity := Count + MinSpace
          else
          {
            MinSpace here is always smaller or at most equal to grow limit.
            Min is used for situations where Init is erroneously set higher
            than Limit
          }
            NewCapacity := Max(Min(GrowSettings.Init,GrowSettings.Limit),MinSpace);
        end;

      gmLinear:   //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta]);
          If Capacity > 0 then
            begin
              MinDelta := (Count + MinSpace) - Capacity;
              If (MinDelta mod GrowSettings.Delta) <> 0 then
                begin
                {
                  Avoid overflows, increase capacity only by one less delta
                  than is needed and then add the last only if it does not
                  overflow. If it overflows, then just set new capacity to
                  absolute maximum (High(Integer)).
                }
                  NewCapacity := Capacity + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                  If (High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) - NewCapacity) < GrowSettings.Delta then
                    NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF})
                  else
                    NewCapacity := NewCapacity + GrowSettings.Delta;
                end
              else NewCapacity := Capacity + MinDelta;
            end
          else NewCapacity := Max(GrowSettings.Init,MinSpace);
        end;

      gmLinearLimited:    //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta,gsfLimit]);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          If Capacity > 0 then
            begin
              MinDelta := (Count + MinSpace) - Capacity;
              If (MinDelta mod GrowSettings.Delta) <> 0 then
                begin
                  NewCapacity := Capacity + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                  If (GrowSettings.Limit - NewCapacity) < GrowSettings.Delta then
                  {
                    Here, limit is lower than capacity plus last delta, but at
                    the same time cannot be lower than what is needed (Count +
                    MinSpace), therefore we can safely use it.
                  }
                    NewCapacity := GrowSettings.Limit
                  else
                    NewCapacity := NewCapacity + GrowSettings.Delta;
                end
              else NewCapacity := Capacity + MinDelta;
            end
          else NewCapacity := Max(Min(GrowSettings.Init,GrowSettings.Limit),MinSpace);
        end;

      gmLinearFixed:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta]);
          If (Count + MinSpace) > GrowSettings.Init then
            begin
              MinDelta := (Count + MinSpace) - GrowSettings.Init;
              If (MinDelta mod GrowSettings.Delta) <> 0 then
                begin
                  NewCapacity := GrowSettings.Init + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                  If (High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) - NewCapacity) < GrowSettings.Delta then
                    NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF})
                  else
                    NewCapacity := NewCapacity + GrowSettings.Delta;
                end
              else NewCapacity := GrowSettings.Init + MinDelta;
            end
          else NewCapacity := GrowSettings.Init;
        end;

      gmLinearFixedLimited:   //  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta,gsfLimit]);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          If (Count + MinSpace) > GrowSettings.Init then
            begin
              MinDelta := (Count + MinSpace) - GrowSettings.Init;
              If (MinDelta mod GrowSettings.Delta) <> 0 then
                begin
                  NewCapacity := GrowSettings.Init + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                  If (GrowSettings.Limit - NewCapacity) < GrowSettings.Delta then
                    NewCapacity := GrowSettings.Limit
                  else
                    NewCapacity := NewCapacity + GrowSettings.Delta;
                end
              else NewCapacity := GrowSettings.Init + MinDelta;   
            end
          // we need to select min value in case limit is set below init  
          else NewCapacity := Min(GrowSettings.Init,GrowSettings.Limit);
        end;

      gmDefault,gmFast:   //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfFactor]);
          If Capacity > 0 then
            begin
            {
              Following calculation might seem to be weird, so bear with me...

              We need to calculate how many times we need to multiply capacity
              by grow factor to get new capacity that is at least equal to
              Count + MinSpace. This can be seen as following calculation:

                  NewCapacity = Capacity * Factor * Factor * Factor ...

                  NewCapacity = Capacity * Factor^N

                  NewCapacity = Capacity * Power(Factor,N)

              We need to obtain the N, ie. an exponent - we use logarithm where
              factor is the base.

                  N = Log(Factor,NewCapacity / Capacity)

              Of course, we do not have the new capacity, but we know a value
              that is smaller or equal to it, Count + MinSpace, so:

                  N >= Log(Factor,(Count + MinSpace) / Capacity)

              This way we get (possibly) a decimal number. To get integral
              exponent that will give us new capacity that is at least equal
              to Count + MinSize, we just take its ceiling (function Ceil)
              and substitute it for N in the first formula.
            }
              FloatCap := Capacity * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                LogN(GrowSettings.Factor,(Count + MinSpace) / Capacity)));
              If FloatCap < High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) then
                begin
                  NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                {
                  Following code is here only as a contingency in case some
                  problem occured in previous calculations. Normally the cycle
                  should not execute.
                }
                  while NewCapacity < (Count + MinSpace) do
                    begin
                    {
                      Max is there for situation where factor is set in a
                      way that it will actually NOT change the capacity (eg.
                      1.000001 or similar), ensuring we are making progress
                      by at least one..
                    }
                      NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                        Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                    {
                      Check for overflow in multiplication, if it overflowed
                      then just use maximum possible value - Count + MinSpace
                      was already tested to not overflow it, so it suffices.
                    }
                      If NewCapacity < Capacity then
                        begin
                          NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});
                          Break{while};
                        end;
                    end;
                end
              {
                Calculated float capacity is larger than capacity's integer
                type can hold (it would overflow), so we just set new capacity
                to absolute maximum it can hold.
              }
              else NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});
            end
          else NewCapacity := Max(GrowSettings.Init,MinSpace);
        end;

      gmFastLimited:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfFactor,gsfLimit]);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          If Capacity > 0 then
            begin
              FloatCap := Capacity * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                LogN(GrowSettings.Factor,(Count + MinSpace) / Capacity)));
              If FloatCap < GrowSettings.Limit then
                begin
                  NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                  while NewCapacity < (Count + MinSpace) do
                    begin
                      NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                        Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                      If (NewCapacity < Capacity) or (NewCapacity > GrowSettings.Limit) then
                        begin
                          NewCapacity := GrowSettings.Limit;
                          Break{while};
                        end;
                    end;
                end
              // calculation exceeded the limit
              else NewCapacity := GrowSettings.Limit;
            end
          else NewCapacity := Max(Min(GrowSettings.Init,GrowSettings.Limit),MinSpace);
        end;

      gmFastFixed:    //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfFactor]);
          If (Count + MinSpace) > GrowSettings.Init then
            begin
              FloatCap := GrowSettings.Init * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                LogN(GrowSettings.Factor,(Count + MinSpace) / GrowSettings.Init)));
              If FloatCap < High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) then
                begin
                  NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                  If NewCapacity < (Count + MinSpace) then
                    NewCapacity := GrowSettings.Init;
                  while NewCapacity < (Count + MinSpace) do
                    begin
                      NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                        Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                      If NewCapacity < Capacity then
                        begin
                          NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});
                          Break{while};
                        end;
                    end;
                end
              else NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});               
            end
          else NewCapacity := GrowSettings.Init;
        end;

      gmFastFixedLimited: //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfFactor,gsfLimit]);
          If (Count + MinSpace) > GrowSettings.Limit then
            raise ELUExceedingLimit.Create('GrowResolve: Required capacity exceeds limit.');
          If (Count + MinSpace) > GrowSettings.Init then
            begin
              FloatCap := GrowSettings.Init * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                LogN(GrowSettings.Factor,(Count + MinSpace) / GrowSettings.Init)));
              If FloatCap < GrowSettings.Limit then
                begin
                  NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                  If NewCapacity < (Count + MinSpace) then
                    NewCapacity := GrowSettings.Init;
                  while NewCapacity < (Count + MinSpace) do
                    begin
                      NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                        Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                      If (NewCapacity < Capacity) or (NewCapacity > GrowSettings.Limit) then
                        begin
                          NewCapacity := GrowSettings.Limit;
                          Break{while};
                        end;
                    end;
                end
              else NewCapacity := GrowSettings.Limit;
            end
          else NewCapacity := Min(GrowSettings.Init,GrowSettings.Limit);
        end;

      gmFastAttenuated:   //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta,gsfFactor,gsfLimit]);
          If Capacity > 0 then
            begin
              NewCapacity := Capacity;
              If NewCapacity < GrowSettings.Limit then
                begin
                  // we are below grow limit, do fast growth
                  FloatCap := NewCapacity * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                    LogN(GrowSettings.Factor,(Count + MinSpace) / NewCapacity)));
                  If FloatCap < GrowSettings.Limit then
                    begin
                      NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                      while NewCapacity < (Count + MinSpace) do
                        begin
                          NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                            Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                          If (NewCapacity < Capacity) or (NewCapacity > GrowSettings.Limit) then
                            begin
                              NewCapacity := GrowSettings.Limit;
                              Break{while};
                            end;
                        end;
                      end
                  else NewCapacity := GrowSettings.Limit;
                end;
              // now do linear growth, but only if necessary
              If (NewCapacity - Count) < MinSpace then
                begin
                  MinDelta := (Count + MinSpace) - NewCapacity;
                  If (MinDelta mod GrowSettings.Delta) <> 0 then
                    begin
                      NewCapacity := NewCapacity + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                      If (High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) - NewCapacity) < GrowSettings.Delta then
                        NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF})
                      else
                        NewCapacity := NewCapacity + GrowSettings.Delta;
                    end
                  else NewCapacity := NewCapacity + MinDelta;
                end;
            end
          else NewCapacity := Max(GrowSettings.Init,MinSpace);
        end;

      gmLinearAccelerated:    //  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsFields([gsfInit,gsfDelta,gsfFactor,gsfLimit]);
          If Capacity > 0 then
            begin
              NewCapacity := Capacity;
              // first linear growth...
              If NewCapacity < GrowSettings.Limit then
                begin
                  MinDelta := (Count + MinSpace) - NewCapacity;
                  If (MinDelta mod GrowSettings.Delta) <> 0 then
                    begin
                      NewCapacity := NewCapacity + ((MinDelta div GrowSettings.Delta) * GrowSettings.Delta);
                    {
                      NewCapacity here can exceed grow limit, if that happens
                      then following subtraction produces a negative number
                      which will always be smaller than limit, setting the new
                      capacity to the said limit - so everything is ok in this
                      regard.
                    }
                      If (GrowSettings.Limit - NewCapacity) < GrowSettings.Delta then
                        NewCapacity := GrowSettings.Limit
                      else
                        NewCapacity := NewCapacity + GrowSettings.Delta;
                    end
                  {
                    We must use Min here, because it is entirely possible that
                    NewCapacity (which here is in fact the original capacity)
                    plust MinDelta exceeds grow limit.
                  }
                  else NewCapacity := Min(NewCapacity + MinDelta,GrowSettings.Limit);
                end;
              // and now fast...
              If (NewCapacity - Count) < MinSpace then
                begin
                  FloatCap := NewCapacity * Power(GrowSettings.Factor,{$IFDEF List64}Ceil64{$ELSE}Ceil{$ENDIF}(
                    LogN(GrowSettings.Factor,(Count + MinSpace) / NewCapacity)));
                  If FloatCap < High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}) then
                    begin
                      NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                      while NewCapacity < (Count + MinSpace) do
                        begin
                          NewCapacity := Max({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                            Trunc(NewCapacity * GrowSettings.Factor)),NewCapacity + 1);
                          If NewCapacity < Capacity then
                            begin
                              NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});
                              Break{while};
                            end;
                        end;
                    end
                  else NewCapacity := High({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF});
                end;
            end
          else NewCapacity := Max(GrowSettings.Init,MinSpace);
        end;

      gmCustomCallback:   //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsField(gsfCustomCallback);
          GrowSettings.Custom.Callback(GrowSettings.Custom.Context,Capacity,Count,MinSpace,NewCapacity);
          // do sanity checks
          If NewCapacity < Capacity then
            raise ELUInvalidResult.Create('GrowResolve: New capacity is smaller than old capacity.');
        {
          Following check is here (separate from the previous) simply to avoid
          overflow in the subtraction. All conditions under which this overflow
          can happen are checked earlier.
        }
          If (NewCapacity - Count) < MinSpace then
            raise ELUInvalidResult.Create('GrowResolve: New capacity is too small.');
        end;

      gmCustomEvent:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckGrowSettingsField(gsfCustomEvent);
          GrowSettings.Custom.Event(GrowSettings.Custom.Context,Capacity,Count,MinSpace,NewCapacity);
          If NewCapacity < Capacity then
            raise ELUInvalidResult.Create('GrowResolve: New capacity is smaller than old capacity.');
          If (NewCapacity - Count) < MinSpace then
            raise ELUInvalidResult.Create('GrowResolve: New capacity is too small.');
        end;

    else  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
     {gmNone}
      raise ELUInvalidOperation.CreateFmt('GrowResolve: Grow mode (%d) prevents operation.',[Ord(GrowSettings.Mode)]);
    end;
  {
    Final sanity check. If here, Count + MinSpace IS larger than Capacity, so
    no need to check whether NewCapacity >= Capacity - if it is larger than
    Count + MinSpace, then it must logically be also larger than Capacity.
  }
    If NewCapacity >= (Count + MinSpace) then
      // signal true only if new capacity differs from the old one
      Result := NewCapacity > Capacity
    else
      raise ELUInvalidResult.Create('GrowResolve: Invalid new capacity.');
  end;
end;
{$ENDIF}

//==============================================================================

{$IFDEF ShrinkResolve}

  procedure CheckShrinkSettingsFields(Fields: TShrinkSettingsFields);
  begin
    If ssfDelta in Fields then
      If ShrinkSettings.Delta <= 0 then
        raise ELUInvalidValue.CreateFmt('ShrinkResolve.CheckShrinkSettingsFields: Invalid shrink delta (%d).',[ShrinkSettings.Delta]);
    If ssfFactor in Fields then
      If (ShrinkSettings.Factor < 0.0) or (ShrinkSettings.Factor >= 1.0) then
        raise ELUInvalidValue.CreateFmt('ShrinkResolve.CheckShrinkSettingsFields: Invalid shrink factor (%g).',[ShrinkSettings.Factor]);
    If ssfLimit in Fields then
      If ShrinkSettings.Limit < 0 then
        raise ELUInvalidValue.CreateFmt('ShrinkResolve.CheckShrinkSettingsFields: Invalid shrink limit (%d).',[ShrinkSettings.Limit]);
    If ssfCustomCallback in Fields then
      If not Assigned(ShrinkSettings.Custom.Callback) then
        raise ELUInvalidValue.Create('ShrinkResolve.CheckShrinkSettingsFields: Custom callback not assigned.');
    If ssfCustomEvent in Fields then
      If not Assigned(ShrinkSettings.Custom.Event) then
        raise ELUInvalidValue.Create('ShrinkResolve.CheckShrinkSettingsFields: Custom event not assigned.');
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure CheckShrinkSettingsField(Field: TShrinkSettingsField);
  begin
    CheckShrinkSettingsFields([Field]);
  end;

var
  LimitDelta:   {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF};
  FloatCap:     Extended;
  NextCapacity: {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF};
begin
Result := False;
NewCapacity := Capacity;
If Capacity < 0 then
  raise ELUInvalidValue.CreateFmt('ShrinkResolve: Ivalid capacity (%d).',[Capacity]);
If Count < 0 then
  raise ELUInvalidValue.CreateFmt('ShrinkResolve: Ivalid count (%d).',[Count]);
If Capacity < Count then
  raise ELUInvalidValue.CreateFmt('ShrinkResolve: Capacity (%d) lower than count (%d).',[Capacity,Count]);
If Count < Capacity then
  begin
    case ShrinkSettings.Mode of
      smKeepCap:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        NewCapacity := Capacity;

      smToCount:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        NewCapacity := Count;

      smLinear:   //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsField(ssfDelta);
          If Count > 0 then
            NewCapacity := Capacity - (ShrinkSettings.Delta * ((Capacity - Count) div ShrinkSettings.Delta))
          else
            NewCapacity := 0;
        end;

      smLinearLimited:    //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfDelta,ssfLimit]);
          If Capacity > ShrinkSettings.Limit then
            NewCapacity := Max(ShrinkSettings.Limit,
              Capacity - (ShrinkSettings.Delta * ((Capacity - Count) div ShrinkSettings.Delta)));
        end;

      smLinearLimitedZero:    //  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfDelta,ssfLimit]);
          If Count > 0 then
            begin
              If Capacity > ShrinkSettings.Limit then
                NewCapacity := Max(ShrinkSettings.Limit,
                  Capacity - (ShrinkSettings.Delta * ((Capacity - Count) div ShrinkSettings.Delta)));
            end
          else NewCapacity := 0;
        end;

      smLinearFixed:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsField(ssfDelta);
          If Count > 0 then
            begin
              If (Count mod ShrinkSettings.Delta) <> 0 then
              {
                Min is used because using delta threshold can give higher value
                than what is current capacity.
              }
                NewCapacity := Min(Capacity,Succ(Count div ShrinkSettings.Delta) * ShrinkSettings.Delta)
              else
                NewCapacity := Count;
            end
          else NewCapacity := 0;
        end;

      smLinearFixedLimited:   //  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfDelta,ssfLimit]);
          If Count > ShrinkSettings.Limit then
            begin
              LimitDelta := Count - ShrinkSettings.Limit;
              If (LimitDelta mod ShrinkSettings.Delta) <> 0 then
                NewCapacity := Min(Capacity,ShrinkSettings.Limit +
                  (Succ(LimitDelta div ShrinkSettings.Delta) * ShrinkSettings.Delta))
              else
                NewCapacity := Count;
            end
          else NewCapacity := Min(Capacity,ShrinkSettings.Limit);
        end;

      smLinearFixedLimitedZero:   //  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfDelta,ssfLimit]);
          If Count > 0 then
            begin
              If Count > ShrinkSettings.Limit then
                begin
                  LimitDelta := Count - ShrinkSettings.Limit;
                  If (LimitDelta mod ShrinkSettings.Delta) <> 0 then
                    NewCapacity := Min(Capacity,ShrinkSettings.Limit +
                      (Succ(LimitDelta div ShrinkSettings.Delta) * ShrinkSettings.Delta))
                  else
                    NewCapacity := Count;
                end
              else NewCapacity := Min(Capacity,ShrinkSettings.Limit);
            end
          else NewCapacity := 0;
        end;

      smFast: //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsField(ssfFactor);
          If Count > 0 then
            begin
            {
              Following calculation is pretty much the same as in GrowResolve,
              mode gmFast (and its derivatives), see there for explanation.
            }
              FloatCap := Capacity * Power(ShrinkSettings.Factor,
                {$IFDEF List64}Floor64{$ELSE}Floor{$ENDIF}(LogN(ShrinkSettings.Factor,Count / Capacity)));
              If (FloatCap < Count) or (FloatCap > Capacity) then
                begin
                  // contingency in case something failed
                  NextCapacity := Capacity;
                  repeat
                    NewCapacity := NextCapacity;
                    NextCapacity := Min({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                      Trunc(NewCapacity * ShrinkSettings.Factor)),NewCapacity - 1);
                  until NextCapacity < Count;
                end
              else NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));   
            end
          else NewCapacity := 0;
        end;

      smFastLimited:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfFactor,ssfLimit]);
          If Count > 0 then
            begin
            {
              Following comparison catches cases where current capacity is
              already below shrink limit or at it.
            }
              If Capacity > ShrinkSettings.Limit then
                begin
                  FloatCap := Capacity * Power(ShrinkSettings.Factor,
                    {$IFDEF List64}Floor64{$ELSE}Floor{$ENDIF}(LogN(ShrinkSettings.Factor,Count / Capacity)));
                  If FloatCap > ShrinkSettings.Limit then
                    begin
                      If (FloatCap < Count) or (FloatCap > Capacity) then
                        begin
                          NextCapacity := Capacity;
                          repeat
                            NewCapacity := NextCapacity;
                            NextCapacity := Min({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                              Trunc(NewCapacity * ShrinkSettings.Factor)),NewCapacity - 1);
                          until (NextCapacity < Count) or (NextCapacity < ShrinkSettings.Limit);
                          If NewCapacity < ShrinkSettings.Limit then
                            NewCapacity := ShrinkSettings.Limit;
                        end
                      else NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                    end
                  else NewCapacity := ShrinkSettings.Limit;
                end;
            end
          else NewCapacity := ShrinkSettings.Limit;
        end;

      smDefault,smFastLimitedZero:    //  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsFields([ssfFactor,ssfLimit]);
          If Count > 0 then
            begin
              If Capacity > ShrinkSettings.Limit then
                begin
                  FloatCap := Capacity * Power(ShrinkSettings.Factor,
                    {$IFDEF List64}Floor64{$ELSE}Floor{$ENDIF}(LogN(ShrinkSettings.Factor,Count / Capacity)));
                  If FloatCap > ShrinkSettings.Limit then
                    begin
                      If (FloatCap < Count) or (FloatCap > Capacity) then
                        begin
                          NextCapacity := Capacity;
                          repeat
                            NewCapacity := NextCapacity;
                            NextCapacity := Min({$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(
                              Trunc(NewCapacity * ShrinkSettings.Factor)),NewCapacity - 1);
                          until (NextCapacity < Count) or (NextCapacity < ShrinkSettings.Limit);
                          If NewCapacity < ShrinkSettings.Limit then
                            NewCapacity := ShrinkSettings.Limit;
                        end
                      else NewCapacity := {$IFDEF List64}Int64{$ELSE}Integer{$ENDIF}(Trunc(FloatCap));
                    end
                  else NewCapacity := ShrinkSettings.Limit;
                end;
            end
          else NewCapacity := 0;
        end;

      smCustomCallback:   //  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsField(ssfCustomCallback);
          ShrinkSettings.Custom.Callback(ShrinkSettings.Custom.Context,Capacity,Count,NewCapacity);
          If NewCapacity > Capacity then
            raise ELUInvalidResult.Create('ShrinkResolve: New capacity is larger than old capacity.');
          If NewCapacity < Count then
            raise ELUInvalidResult.Create('ShrinkResolve: New capacity is too small.');
        end;

      smCustomEvent:  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        begin
          CheckShrinkSettingsField(ssfCustomEvent);
          ShrinkSettings.Custom.Event(ShrinkSettings.Custom.Context,Capacity,Count,NewCapacity);
          If NewCapacity > Capacity then
            raise ELUInvalidResult.Create('ShrinkResolve: New capacity is larger than old capacity.');
          If NewCapacity < Count then
            raise ELUInvalidResult.Create('ShrinkResolve: New capacity is too small.');
        end;

    else  //  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
     {smNone}
      raise ELUInvalidOperation.CreateFmt('ShrinkResolve: Shrink mode (%d) prevents operation.',[Ord(ShrinkSettings.Mode)]);
    end;
    If (NewCapacity <= Capacity) and (NewCapacity >= 0) then
      Result := NewCapacity < Capacity
    else
      raise ELUInvalidResult.Create('ShrinkResolve: Invalid new capacity.');
  end;
end;
{$ENDIF}
