{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CITY hash calculation - version 1.1.1

    Implementation of CITY hash version 1.1.1.

  Version 2.1 (2021-09-16)

  Last change 2023-01-24

  ©2016-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CityHash

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase  

  SimpleCPUID is required only when PurePascal symbol is not defined.
  Also, it might be needed by BitOps library, see there for details.

===============================================================================}
unit CITY_1_1_1;

{$INCLUDE 'CITY_defs.inc'}

interface

uses
  AuxTypes,
  CITY_Common;

{===============================================================================
    Utility functions - declaration
===============================================================================}

// Hash 128 input bits down to 64 bits of output.
// This is intended to be a reasonably good hash function.
Function Hash128to64(const x: UInt128): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
    Main hash functions - declaration
===============================================================================}

// Hash function for a byte array.  Most useful in 32-bit binaries.
Function CityHash32(s: Pointer; len: TMemSize): UInt32;

// Hash function for a byte array.
Function CityHash64(s: Pointer; len: TMemSize): UInt64;

// Hash function for a byte array.  For convenience, a 64-bit seed is also
// hashed into the result.
Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

// Hash function for a byte array.  For convenience, two seeds are also
// hashed into the result.
Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

// Hash function for a byte array.  For convenience, a 128-bit seed is also
// hashed into the result.
Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;

// Hash function for a byte array.
Function CityHash128(s: Pointer; len: TMemSize): UInt128;

{===============================================================================
    CRC hash functions - declaration
===============================================================================}

// Hash function for a byte array.  Sets result[0] ... result[3].
procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);

// Hash function for a byte array.  For convenience, a 128-bit seed is also
// hashed into the result.
Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;

// Hash function for a byte array.
Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;

implementation

{===============================================================================
    Utility functions - implementation
===============================================================================}

Function Hash128to64(const x: UInt128): UInt64;
begin
Result := CITY_Common.Hash128to64(x);
end;

{===============================================================================
    Main hash functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Main hash functions - internals
-------------------------------------------------------------------------------}

Function Hash32Len0to4(s: Pointer; len: TMemSize): UInt32;
var
  b,c:  UInt32;
  i:    TMemSize;
  v:    Int8;
begin
b := 0;
c := 9;
If Len > 0 then
  For i := 0 to Pred(Len) do
    begin
      v := PInt8(PTR_ADVANCE(s,i))^;
      b := UInt32((Int64(b) * c1) + v);
      c := c xor b;
    end;
Result := fmix(Mur(b,Mur(len,c)));    
end;

//------------------------------------------------------------------------------

Function Hash32Len5to12(s: Pointer; len: TMemSize): UInt32;
var
  a,b,c,d:  UInt32;
begin
a := len; b := len * 5; c := 9; d := b;
a := a + Fetch32(s);
b := b + Fetch32(s,len - 4);
c := c + Fetch32(s ,(len shr 1) and 4);
Result := fmix(Mur(c,Mur(b,Mur(a,d))));
end;

//------------------------------------------------------------------------------

Function Hash32Len13to24(s: Pointer; len: TMemSize): UInt32;
var
  a,b,c,d,e,f,h:  UInt32;
begin
a := Fetch32(s,PtrUInt(Int64(-4) + (len shr 1)));
b := Fetch32(s,4);
c := Fetch32(s,len - 8);
d := Fetch32(s,len shr 1);
e := Fetch32(s);
f := Fetch32(s,len - 4);
h := len;
result := fmix(Mur(f,Mur(e,Mur(d,Mur(c,Mur(b,Mur(a,h)))))));
end;

//------------------------------------------------------------------------------

Function HashLen0to16(s: Pointer; len: TMemSize): UInt64;
var
  a,b,c,d:  UInt64;
  mul:      UInt64;
  y,z:      UInt32;
begin
case len of
  1..3:
    begin
      a := PUInt8(s)^;
      b := PUInt8(PTR_ADVANCE(s,len shr 1))^;
      c := PUInt8(PTR_ADVANCE(s,len - 1))^;
      y := UInt32(a) + (UInt32(b) shl 8);
      z := len + (UInt32(c) shl 2);
      Result := ShiftMix((y * k2) xor (z * k0)) * k2;
    end;
  4..7:
    begin
      mul := k2 + UInt64(len) * 2;
      a := Fetch32(s);
      Result := HashLen16(len + (a shl 3),Fetch32(s,len - 4),mul);
    end;
  8..High(len):
    begin
      mul := k2 + UInt64(len) * 2;
      a := Fetch64(s) + k2;
      b := Fetch64(s,len - 8);
      c := Rotate(b,37) * mul + a;
      d := (Rotate(a,25) + b) * mul;
      Result := HashLen16(c,d,mul);
    end;
else
  Result := k2;
end;
end;

//------------------------------------------------------------------------------

// This probably works well for 16-byte strings as well, but it may be overkill
// in that case.
Function HashLen17to32(s: Pointer; len: TMemSize): UInt64;
var
  a,b,c,d:  UInt64;
  mul:      UInt64;
begin
mul := k2 + UInt64(len) * 2;
a := Fetch64(s) * k1;
b := Fetch64(s,8);
c := Fetch64(s,len - 8) * mul;
d := Fetch64(s,len - 16) * k2;
Result := HashLen16(Rotate(a + b,43) + Rotate(c,30) + d,a + Rotate(b + k2,18) + c,mul);
end;

//------------------------------------------------------------------------------

// Return a 16-byte hash for 48 bytes.  Quick and dirty.
// Callers do best to use "random-looking" values for a and b.
Function WeakHashLen32WithSeeds(w,x,y,z,a,b: UInt64): UInt128; overload;
var
  c:  UInt64;
begin
a := a + w;
b := Rotate(b + a + z,21);
c := a;
a := a + x;
a := a + y;
b := b + Rotate(a,44);
Result.First := a + z;
Result.Second := b + c;
end;

//------------------------------------------------------------------------------

// Return a 16-byte hash for s[0] ... s[31], a, and b.  Quick and dirty.
Function WeakHashLen32WithSeeds(s: Pointer; a,b: UInt64): UInt128; overload;
begin
Result := WeakHashLen32WithSeeds(Fetch64(s),Fetch64(s,8),Fetch64(s,16),Fetch64(s,24),a,b);
end;

//------------------------------------------------------------------------------

// Return an 8-byte hash for 33 to 64 bytes.
Function HashLen33to64(s: Pointer; len: TMemSize): UInt64;
var
  mul,a,b,c,d,e,f,g,h,u,v,w,x,y,z:  UInt64;
begin
mul := k2 + UInt64(len) * 2;
a := Fetch64(s) * k2;
b := Fetch64(s,8);
c := Fetch64(s,len - 24);
d := Fetch64(s,len - 32);
e := Fetch64(s,16) * k2;
f := Fetch64(s,24) * 9;
g := Fetch64(s,len - 8);
h := Fetch64(s,len - 16) * mul;
u := Rotate(a + g,43) + (Rotate(b,30) + c) * 9;
v := ((a + g) xor d) + f + 1;
w := EndianSwap((u + v) * mul) + h;
x := Rotate(e + f,42) + c;
y := (EndianSwap((v + w) * mul) + g) * mul;
z := e + f + c;
a := EndianSwap((x + z) * mul + y) + b;
b := ShiftMix((z + a) * mul + d + h) * mul;
Result := b + x;
end;

//------------------------------------------------------------------------------

// A subroutine for CityHash128().  Returns a decent 128-bit hash for strings
// of any length representable in signed long.  Based on City and Murmur.
Function CityMurmur(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  a,b,c,d:  UInt64;
  l:        PtrInt;
begin
a := UInt128Low64(seed);
b := UInt128High64(seed);
l := len - 16;
If l <= 0 then  // len <= 16
  begin
    a := ShiftMix(a * k1) * k1;
    c := b * k1 + HashLen0to16(s,len);
    If len >= 8 then
      d := ShiftMix(a + Fetch64(s))
    else
      d := ShiftMix(a + c);
  end
else
  begin
    c := HashLen16(Fetch64(s,len - 8) + k1,a);
    d := HashLen16(b + len,c + Fetch64(s,len - 16));
    a := a + d;
    repeat
      a := a xor (ShiftMix(Fetch64(s) * k1) * k1);
      a := a * k1;
      b := b xor a;
      c := c xor (ShiftMix(Fetch64(s,8) * k1) * k1);
      c := c * k1;
      d := d xor c;
      PTR_ADVANCEVAR(s,16);
      l := l - 16;
    until l <= 0;
  end;
a := HashLen16(a,c);
b := HashLen16(d,b);
Result := UInt128Make(a xor b,HashLen16(b,a));
end;

{-------------------------------------------------------------------------------
    Main hash functions - public functions
-------------------------------------------------------------------------------}

Function CityHash32(s: Pointer; len: TMemSize): UInt32;
var
  h,g,f:          UInt32;
  a0,a1,a2,a3,a4: UInt32;
  iters:          TMemSize;
begin
If len <= 24 then
  begin
    case len of
      0..4:   Result := Hash32Len0to4(s,len);
      5..12:  Result := Hash32Len5to12(s,len);
    else
      Result := Hash32Len13to24(s,len);
    end;
    Exit;
  end;
// len > 24
h := len; g := c1 * len; f := g;
a0 := Rotate32(Fetch32(s,len - 4) * c1,17) * c2;
a1 := Rotate32(Fetch32(s,len - 8) * c1,17) * c2;
a2 := Rotate32(Fetch32(s,len - 16) * c1,17) * c2;
a3 := Rotate32(Fetch32(s,len - 12) * c1,17) * c2;
a4 := Rotate32(Fetch32(s,len - 20) * c1,17) * c2;
h := h xor a0;
h := Rotate32(h,19);
h := h * 5 + $e6546b64;
h := h xor a2;
h := Rotate32(h,19);
h := h * 5 + $e6546b64;
g := g xor a1;
g := Rotate32(g,19);
g := g * 5 + $e6546b64;
g := g xor a3;
g := Rotate32(g,19);
g := g * 5 + $e6546b64;
f := f + a4;
f := Rotate32(f,19);
f := f * 5 + $e6546b64;
iters := (len - 1) div 20;
repeat
  a0 := Rotate32(Fetch32(s) * c1,17) * c2;
  a1 := Fetch32(s,4);
  a2 := Rotate32(Fetch32(s,8) * c1,17) * c2;
  a3 := Rotate32(Fetch32(s,12) * c1,17) * c2;
  a4 := Fetch32(s,16);
  h := h xor a0;
  h := Rotate32(h,18);
  h := h * 5 + $e6546b64;
  f := f + a1;
  f := Rotate32(f,19);
  f := f * c1;
  g := g + a2;
  g := Rotate32(g,18);
  g := g * 5 + $e6546b64;
  h := h xor (a3 + a1);
  h := Rotate32(h,19);
  h := h * 5 + $e6546b64;
  g := g xor a4;
  g := EndianSwap(g) * 5;
  h := h + (a4 * 5);
  h := EndianSwap(h);
  f := f + a0;
  PERMUTE3(f,h,g);
  PTR_ADVANCEVAR(s,20);
  Dec(iters);
until iters <= 0;
g := Rotate32(g,11) * c1;
g := Rotate32(g,17) * c1;
f := Rotate32(f,11) * c1;
f := Rotate32(f,17) * c1;
h := Rotate32(h + g, 19);
h := h * 5 + $e6546b64;
h := Rotate32(h,17) * c1;
h := Rotate32(h + f, 19);
h := h * 5 + $e6546b64;
h := Rotate32(h,17) * c1;
Result := h;
end;

//------------------------------------------------------------------------------

Function CityHash64(s: Pointer; len: TMemSize): UInt64;
var
  x,y,z:  UInt64;
  v,w:    UInt128;
begin
If len <= 64 then
  begin
    case len of
       0..16: Result := HashLen0To16(s,len);
      17..32: Result := HashLen17To32(s,len);
    else
     {33..64}
      Result := HashLen33to64(s,len);
    end;
    Exit;
  end;
// For strings over 64 bytes we hash the end first, and then as we
// loop we keep 56 bytes of state: v, w, x, y, and z.
x := Fetch64(s,len - 40);
y := Fetch64(s,len - 16) + Fetch64(s,len - 56);
z := HashLen16(Fetch64(s,len - 48) + len,Fetch64(s,len - 24));
v := WeakHashLen32WithSeeds(PTR_ADVANCE(s,len - 64),len,z);
w := WeakHashLen32WithSeeds(PTR_ADVANCE(s,len - 32),y + k1,x);
x := x * k1 + Fetch64(s);
// Decrease len to the nearest multiple of 64, and operate on 64-byte chunks.
len := (len - 1) and not TMemSize(63);
repeat
  x := Rotate(x + y + v.First + Fetch64(s,8), 37) * k1;
  y := Rotate(y + v.Second + Fetch64(s,48), 42) * k1;
  x := x xor w.Second;
  y := y + (v.First + Fetch64(s,40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds(PTR_ADVANCE(s,32),z + w.Second,y + Fetch64(s,16));
  SWAP(z,x);
  PTR_ADVANCEVAR(s,64);
  len := len - 64;
until len <= 0;
Result := HashLen16(HashLen16(v.First,w.First) + ShiftMix(y) * k1 + z,
                    HashLen16(v.Second,w.Second) + x);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;
begin
Result := CityHash64WithSeeds(s,len,k2,seed);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;
begin
Result := HashLen16(CityHash64(s,len) - seed0,seed1);
end;

//------------------------------------------------------------------------------

Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  v,w:        UInt128;
  x,y,z:      UInt64;
  tail_done:  TMemSize;
begin
If len < 128 then
  begin
    Result := CityMurmur(s,len,seed);
    Exit;
  end;
// We expect len >= 128 to be the common case.  Keep 56 bytes of state:
// v, w, x, y, and z.
x := UInt128Low64(seed);
y := UInt128High64(seed);
z := len * k1;
v.First := Rotate(y xor k1,49) * k1 + Fetch64(s);
v.Second := Rotate(v.First,42) * k1 + Fetch64(s,8);
w.First := Rotate(y + z,35) * k1 + x;
w.Second := Rotate(x + Fetch64(s,88),53) * k1;
// This is the same inner loop as CityHash64(), manually unrolled.
repeat
  x := Rotate(x + y + v.First + Fetch64(s,8),37) * k1;
  y := Rotate(y + v.Second + Fetch64(s,48),42) * k1;
  x := x xor w.second;
  y := y + (v.First + Fetch64(s,40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds(PTR_ADVANCE(s,32),z + w.Second,y + Fetch64(s,16));
  SWAP(z,x);
  PTR_ADVANCEVAR(s,64);
  x := Rotate(x + y + v.First + Fetch64(s,8),37) * k1;
  y := Rotate(y + v.Second + Fetch64(s,48),42) * k1;
  x := x xor w.second;
  y := y + (v.First + Fetch64(s,40));
  z := Rotate(z + w.First,33) * k1;
  v := WeakHashLen32WithSeeds(s,v.Second * k1,x + w.First);
  w := WeakHashLen32WithSeeds(PTR_ADVANCE(s,32),z + w.Second,y + Fetch64(s,16));
  SWAP(z,x);
  PTR_ADVANCEVAR(s,64);
  len := len - 128;
until len < 128;
x := x + Rotate(v.First + z,49) * k0;
y := y * k0 + Rotate(w.Second,37);
z := z * k0 + Rotate(w.First,27);
w.First := w.First * 9;
v.First := v.First * k0;
// If 0 < len < 128, hash up to 4 chunks of 32 bytes each from the end of s.
tail_done := 0;
while tail_done < len do
  begin
    tail_done := tail_done + 32;
    y := Rotate(x + y,42) * k0 + v.Second;
    w.First := w.First + Fetch64(s,len - tail_done + 16);
    x := x * k0 + w.First;
    z := z + (w.Second + Fetch64(s,len - tail_done));
    w.Second := w.Second + v.First;
    v := WeakHashLen32WithSeeds(PTR_ADVANCE(s,len - tail_done),v.First + z,v.Second);
    v.First := v.First * k0;
  end;
// At this point our 56 bytes of state should contain more than
// enough information for a strong 128-bit hash.  We use two
// different 56-byte-to-8-byte hashes to get a 16-byte final result.
x := HashLen16(x,v.First);
y := HashLen16(y + z,w.First);
Result := UInt128Make(HashLen16(x + v.Second,w.Second) + y,
                      HashLen16(x + w.Second,y + v.Second));
end;

//------------------------------------------------------------------------------

Function CityHash128(s: Pointer; len: TMemSize): UInt128;
begin
If Len >= 16 then
  Result := CityHash128WithSeed(PTR_ADVANCE(s,16),len - 16,UInt128Make(Fetch64(s),Fetch64(s,8) + k0))
else
  Result := CityHash128WithSeed(s,len,UInt128Make(k0,k1));
end;

{===============================================================================
    CRC hash functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    CRC hash functions - internals
-------------------------------------------------------------------------------}

// Requires len >= 240.
procedure CityHashCrc256Long(s: Pointer; len: TMemSize; seed: UInt32; out result: UInt256);
var
  a,b,c,d,e,f,g,h,x,y,z:  UInt64;
  iters:                  TMemSize;

  procedure CHUNK(r: Integer);
  begin
    PERMUTE3(x,z,y);
    b := b + Fetch64(s);
    c := c + Fetch64(s,8);
    d := d + Fetch64(s,16);
    e := e + Fetch64(s,24);
    f := f + Fetch64(s,32);
    a := a + b;
    h := h + f;
    b := b + c;
    f := f + d;
    g := g + e;
    e := e + z;
    g := g + x;
    z := _mm_crc32_u64(z,b + g);
    y := _mm_crc32_u64(y,e + h);
    x := _mm_crc32_u64(x,f + a);
    e := Rotate(e,r);
    c := c + e;
    PTR_ADVANCEVAR(s,40);
  end;

begin
a := Fetch64(s,56) + k0;
b := Fetch64(s,96) + k0;
result[0] := HashLen16(b,len);
c := result[0];
result[1] := Fetch64(s,120) * k0 + len;
d := result[1];
e := Fetch64(s,184) + seed;
f := 0;
g := 0;
h := c + d;
x := seed;
y := 0;
z := 0;
// 240 bytes of input per iter.
iters := len div 240;
len := len + TMemSize(-(Int64(iters) * 240));
repeat
  CHUNK(0);  PERMUTE3(a,h,c);
  CHUNK(33); PERMUTE3(a,h,f);
  CHUNK(0);  PERMUTE3(b,h,f);
  CHUNK(42); PERMUTE3(b,h,d);
  CHUNK(0);  PERMUTE3(b,h,e);
  CHUNK(33); PERMUTE3(a,h,e);
  Dec(iters);
until iters <= 0;
while len >= 40 do
  begin
    CHUNK(29);
    e := e xor Rotate(a,20);
    h := h + Rotate(b,30);
    g := g xor Rotate(c,40);
    f := f + Rotate(d,34);
    PERMUTE3(c,h,g);
    len := len - 40;
  end;
If len > 0 then
  begin
    PTR_ADVANCEVAR(s,len - 40);
    CHUNK(33);
    e := e xor Rotate(a,43);
    h := h + Rotate(b,42);
    g := g xor Rotate(c,41);
    f := f + Rotate(d,40);
  end;
result[0] := result[0] xor h;
result[1] := result[1] xor g;
g := g + h;
a := HashLen16(a,g + z);
x := x + (y shl 32);
b := b + x;
c := HashLen16(c,z) + h;
d := HashLen16(d,e + result[0]);
g := g + e;
h := h + HashLen16(x,f);
e := HashLen16(a,d) + g;
z := HashLen16(b,c) + a;
y := HashLen16(g,h) + c;
result[0] := e + z + y + x;
a := ShiftMix((a + y) * k0) * k0 + b;
result[1] := result[1] + a + result[0];
a := ShiftMix(a * k0) * k0 + c;
result[2] := a + result[1];
a := ShiftMix((a + e) * k0) * k0;
result[3] := a + result[2];
end;

//------------------------------------------------------------------------------

// Requires len < 240.
procedure CityHashCrc256Short(s: Pointer; len: TMemSize; out result: UInt256);
var
  buf:  array[0..239] of Byte;
begin
Move(s^,Addr(buf)^,len);
FillChar(buf[len],240 - len,0);
CityHashCrc256Long(@buf,240,not UInt32(len),result)
end;

{-------------------------------------------------------------------------------
    CRC hash functions - public functions
-------------------------------------------------------------------------------}

procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);
begin
If len >= 240 then
  CityHashCrc256Long(s,len,0,result)
else
  CityHashCrc256Short(s,len,result);
end;

//------------------------------------------------------------------------------

Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
var
  res:  UInt256;
  u,v:  UInt64;
begin
If len <= 900 then
  Result := CityHash128WithSeed(s,len,seed)
else
  begin
    CityHashCrc256(s,len,res);
    u := UInt128High64(seed) + res[0];
    v := UInt128Low64(seed) + res[1];
    Result := UInt128Make(HashLen16(u,v + res[2]),HashLen16(Rotate(v,32),u * k0 + res[3]));
  end;
end;

//------------------------------------------------------------------------------

Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;
var
  res:  UInt256;
begin
If len <= 900 then
  Result := CityHash128(s,len)
else
  begin
    CityHashCrc256(s,len,res);
    Result := UInt128Make(res[2],res[3]);
  end;
end;

end.
