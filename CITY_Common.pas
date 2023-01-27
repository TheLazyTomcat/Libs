{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CITY hash calculation - common

    Code common to all versions of the hash (functions, constants, ...)

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
unit CITY_Common;

{$INCLUDE 'CITY_defs.inc'}

interface

uses
  SysUtils,
  AuxTypes, HashBase;

{===============================================================================
    library exceptions
===============================================================================}
type
  ECITYException = class(EHASHException);

  ECITYUnknownFunction = class(ECITYException);

{===============================================================================
    common types - declaration
===============================================================================}
type
  UInt128 = packed record
    case Integer of
      0:(QWords:  array[0..1] of UInt64);
      1:(First:   UInt64;
         Second:  UInt64);
      2:(Low:     UInt64;
         High:    UInt64);
  end;
  PUInt128 = ^UInt128;

Function UInt128Make(Low,High: UInt64): UInt128;{$IFDEF CanInline} inline;{$ENDIF}

Function UInt128Low64(x: UInt128): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function UInt128High64(x: UInt128): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function Hash128to64(const x: UInt128): UInt64;

//------------------------------------------------------------------------------

type
  UInt256 = array[0..3] of UInt64;
  PUInt256 = ^UInt256;

{===============================================================================
    data manipulation functions - declaration
===============================================================================}

Function EndianSwap(x: UInt32): UInt32; overload;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}
Function EndianSwap(x: UInt64): UInt64; overload;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

procedure SWAP(var a,b: UInt64);

Function PTR_ADVANCE(const Ptr: Pointer; Offset: PtrUInt): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

procedure PTR_ADVANCEVAR(var Ptr: Pointer; Offset: PtrUInt);{$IFDEF CanInline} inline;{$ENDIF}
  
{===============================================================================
    data loading functions - declaration
===============================================================================}

Function UNALIGNED_LOAD32(Ptr: Pointer): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function UNALIGNED_LOAD32(Ptr: Pointer; Offset: PtrUInt): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function UNALIGNED_LOAD64(Ptr: Pointer): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function UNALIGNED_LOAD64(Ptr: Pointer; Offset: PtrUInt): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function uint32_in_expected_order(x: UInt32): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
Function uint64_in_expected_order(x: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function Fetch32(Ptr: Pointer): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Fetch32(Ptr: Pointer; Offset: PtrUInt): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Fetch64(Ptr: Pointer): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Fetch64(Ptr: Pointer; Offset: PtrUInt): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
    32bit hash functions - declaration
===============================================================================}

// Magic numbers for 32-bit hashing.  Copied from Murmur3.
const
  c1 = UInt32($cc9e2d51);
  c2 = UInt32($1b873593);

//------------------------------------------------------------------------------

// A 32-bit to 32-bit integer hash copied from Murmur3.
Function fmix(h: UInt32): UInt32;

Function Rotate32(val: UInt32; shift: Integer): UInt32;

procedure PERMUTE3(var a,b,c: UInt32); overload;
procedure PERMUTE3(var a,b,c: UInt64); overload;

Function Mur(a,h: UInt32): UInt32;

{===============================================================================
    64bit+ hash functions - declaration
===============================================================================}

// Some primes between 2^63 and 2^64 for various uses.
{$IF Defined(FPC) and (FPC_FULLVERSION < 30000)}
// following nonsense is here to prevent internal error 200706094 in older FPC
const
  _k0 = UInt64($c3a5c85c97cb3127);
  _k1 = UInt64($b492b66fbe98f273);
  _k2 = UInt64($9ae16a3b2f90404f);
  _k3 = UInt64($c949d7c7509e6557);

  k0: UInt64 = _k0;
  k1: UInt64 = _k1;
  k2: UInt64 = _k2;
  k3: UInt64 = _k3;
{$ELSE}
const
  k0 = UInt64($c3a5c85c97cb3127);
  k1 = UInt64($b492b66fbe98f273);
  k2 = UInt64($9ae16a3b2f90404f);
  k3 = UInt64($c949d7c7509e6557);
{$IFEND}

//------------------------------------------------------------------------------

// Bitwise right rotate.  Normally this will compile to a single
// instruction, especially if the shift is a manifest constant.
Function Rotate(val: UInt64; shift: Integer): UInt64;

// Equivalent to Rotate(), but requires the second arg to be non-zero.
// On x86-64, and probably others, it's possible for this to compile
// to a single instruction if both args are already in registers.
Function RotateByAtLeast1(val: UInt64; shift: Integer): UInt64;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

Function ShiftMix(val: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function HashLen16(u,v: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function HashLen16(u,v,mul: UInt64): UInt64; overload;

{===============================================================================
    Test constants
===============================================================================}

{$IF Defined(FPC) and (FPC_FULLVERSION < 30000)}
const
  _tk0 = UInt64($c3a5c85c97cb3127);
  kSeed0 = UInt64(1234567);
  kSeed1 = UInt64(_tk0);

  tk0: UInt64 = _tk0;
{$ELSE}
const
  tk0 = UInt64($c3a5c85c97cb3127);
  kSeed0 = UInt64(1234567);
  kSeed1 = UInt64(k0);
{$IFEND}

{===============================================================================
    CRC32 intrinsic - declaration
===============================================================================}

Function _mm_crc32_u64(crc,v: UInt64): UInt64;{$IF Defined(CanInline) and Defined(FPC)} inline;{$IFEND}

{===============================================================================
    Unit implementation management - declaration
===============================================================================}
{
  WARNING - be wery careful when changing the selected implementation, as there
            is absolutely no thread-safety protection.

  For full description of this section, please refer to the same section in
  BitOps library (github.com/TheLazyTomcat/Lib.BitOps), file BitOps.pas.
}

type
  TUIM_CityHash_Function = (fnCRC32Intrinsic);

  TUIM_CityHash_Implementation = (imNone,imPascal,imAssembly,imAccelerated);

  TUIM_CityHash_Implementations = set of TUIM_CityHash_Implementation;

//------------------------------------------------------------------------------

{
  Returns which implementations are available for the selected function.
}
Function UIM_CityHash_AvailableFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementations;

{
  Returns which implementations are supported and can be safely selected for
  a given function.
}
Function UIM_CityHash_SupportedFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementations;

{
  Returns value indicating what implementation of the selected function is
  executed when calling the function.
}
Function UIM_CityHash_GetFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementation;

{
  Routes selected function to a selected implementation.

  Returned value is the previous routing.

  NOTE - when asm implementation cannot be used, and you still select it,
         the function will be routed to a pascal version.

  WARNING - when selecting imNone as an implementation, the routing is set to
            nil, and because the routing mechanism, for the sake of speed, does
            not check validity, it will result in an exception when calling
            this function.

  WANRING - when selecting unsupported implementation, calling the function
            will almost certainly result in an system exception (invalid
            instruction).
}
Function UIM_CityHash_SetFuncImpl(Func: TUIM_CityHash_Function; NewImpl: TUIM_CityHash_Implementation): TUIM_CityHash_Implementation;


implementation

uses
  BitOps
{$IFNDEF PurePascal}
  , SimpleCPUID
{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    common types - implementation
===============================================================================}

Function UInt128Make(Low,High: UInt64): UInt128;
begin
Result.Low := Low;
Result.High := High;
end;

//------------------------------------------------------------------------------

Function UInt128Low64(x: UInt128): UInt64;
begin
Result := x.Low;
end;

//------------------------------------------------------------------------------

Function UInt128High64(x: UInt128): UInt64;
begin
Result := x.High;
end;

//------------------------------------------------------------------------------

Function Hash128to64(const x: UInt128): UInt64;
const
  kMul: UInt64 = UInt64($9ddfea08eb382d69);
var
  a,b:  UInt64;
begin
a := (UInt128Low64(x) xor UInt128High64(x)) * kMul;
a := a xor (a shr 47);
b := (UInt128High64(x) xor a) * kMul;
b := b xor (b shr 47);
b := b * kMul;
Result := b;
end;

{===============================================================================
    data manipulation functions - implementation
===============================================================================}

Function EndianSwap(x: UInt32): UInt32;
begin
Result := BitOps.EndianSwap(x);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function EndianSwap(x: UInt64): UInt64;
begin
Result := BitOps.EndianSwap(x);
end;

//------------------------------------------------------------------------------

procedure SWAP(var a,b: UInt64);
var
  Temp: UInt64;
begin
Temp := a;
a := b;
b := Temp;
end;

//------------------------------------------------------------------------------

Function PTR_ADVANCE(const Ptr: Pointer; Offset: PtrUInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(Ptr) + Offset);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure PTR_ADVANCEVAR(var Ptr: Pointer; Offset: PtrUInt);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Ptr := Pointer(PtrUInt(Ptr) + Offset);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{===============================================================================
    data loading functions - implementation
===============================================================================}

Function UNALIGNED_LOAD32(Ptr: Pointer): UInt32;
begin
Move(Ptr^,Addr(Result)^,SizeOf(Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function UNALIGNED_LOAD32(Ptr: Pointer; Offset: PtrUInt): UInt32;
begin
Move(PTR_ADVANCE(Ptr,Offset)^,Addr(Result)^,SizeOf(Result));
end;

//------------------------------------------------------------------------------

Function UNALIGNED_LOAD64(Ptr: Pointer): UInt64;
begin
Move(Ptr^,Addr(Result)^,SizeOf(Result));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function UNALIGNED_LOAD64(Ptr: Pointer; Offset: PtrUInt): UInt64;
begin
Move(PTR_ADVANCE(Ptr,Offset)^,Addr(Result)^,SizeOf(Result));
end;

//------------------------------------------------------------------------------

Function uint32_in_expected_order(x: UInt32): UInt32;
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(x);
{$ELSE}
Result := x;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function uint64_in_expected_order(x: UInt64): UInt64;
begin
{$IFDEF ENDIAN_BIG}
Result := EndianSwap(x);
{$ELSE}
Result := x;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Fetch32(Ptr: Pointer): UInt32;
begin
Result := uint32_in_expected_order(UNALIGNED_LOAD32(Ptr));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fetch32(Ptr: Pointer; Offset: PtrUInt): UInt32;
begin
Result := uint32_in_expected_order(UNALIGNED_LOAD32(Ptr,Offset));
end;

//------------------------------------------------------------------------------

Function Fetch64(Ptr: Pointer): UInt64;
begin
Result := uint64_in_expected_order(UNALIGNED_LOAD64(Ptr));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fetch64(Ptr: Pointer; Offset: PtrUInt): UInt64;
begin
Result := uint64_in_expected_order(UNALIGNED_LOAD64(Ptr,Offset));
end;

{===============================================================================
    32bit hash functions - implementation
===============================================================================}

Function fmix(h: UInt32): UInt32;
begin
h := h xor (h shr 16);
h := h * $85ebca6b;
h := h xor (h shr 13);
h := h * $c2b2ae35;
h := h xor (h shr 16);
Result := h;
end;

//------------------------------------------------------------------------------

Function Rotate32(val: UInt32; shift: Integer): UInt32;
begin
// Avoid shifting by 32: doing so yields an undefined result.
If shift = 0 then
  Result := val
else
  Result := ROR(Val,Byte(Shift));
end;

//------------------------------------------------------------------------------

procedure PERMUTE3(var a,b,c: UInt32); overload;
var
  Temp: UInt32;
begin
Temp := c;
c := b;
b := a;
a := Temp;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure PERMUTE3(var a,b,c: UInt64); overload;
var
  Temp: UInt64;
begin
Temp := c;
c := b;
b := a;
a := Temp;
end;

//------------------------------------------------------------------------------

Function Mur(a,h: UInt32): UInt32;
begin
// Helper from Murmur3 for combining two 32-bit values.
a := a * c1;
a := Rotate32(a,17);
a := a * c2;
h := h xor a;
h := Rotate32(h,19);
Result := h * 5 + $e6546b64;
end;

{===============================================================================
    64bit+ hash functions - implementation
===============================================================================}

Function Rotate(val: UInt64; shift: Integer): UInt64;
begin
// Avoid shifting by 64: doing so yields an undefined result.
If shift = 0 then
  Result := val
else
  Result := ROR(val,Byte(Shift));
end;

//------------------------------------------------------------------------------

Function RotateByAtLeast1(val: UInt64; shift: Integer): UInt64;
begin
Result := ROR(val,Byte(Shift));
end;

//------------------------------------------------------------------------------

Function ShiftMix(val: UInt64): UInt64;
begin
Result := val xor (val shr 47);
end;

//------------------------------------------------------------------------------

Function HashLen16(u,v: UInt64): UInt64;
begin
Result := Hash128to64(UInt128Make(u,v));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function HashLen16(u,v,mul: UInt64): UInt64;
var
  a,b:  UInt64;
begin
// Murmur-inspired hashing.
a := (u xor v) * mul;
a := a xor (a shr 47);
b := (v xor a) * mul;
b := b xor (b shr 47);
b := b * mul;
Result := b;
end;

{===============================================================================
    CRC32 intrinsic - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    CRC32 intrinsic - internals
-------------------------------------------------------------------------------}

// Software implemntation of the SSE4.2 intrinsic
Function _mm_crc32_u64_pas(crc,v: UInt64): UInt64;
const
  CRCTable: array[Byte] of UInt32 = (
    $00000000, $F26B8303, $E13B70F7, $1350F3F4, $C79A971F, $35F1141C, $26A1E7E8, $D4CA64EB,
    $8AD958CF, $78B2DBCC, $6BE22838, $9989AB3B, $4D43CFD0, $BF284CD3, $AC78BF27, $5E133C24,
    $105EC76F, $E235446C, $F165B798, $030E349B, $D7C45070, $25AFD373, $36FF2087, $C494A384,
    $9A879FA0, $68EC1CA3, $7BBCEF57, $89D76C54, $5D1D08BF, $AF768BBC, $BC267848, $4E4DFB4B,
    $20BD8EDE, $D2D60DDD, $C186FE29, $33ED7D2A, $E72719C1, $154C9AC2, $061C6936, $F477EA35,
    $AA64D611, $580F5512, $4B5FA6E6, $B93425E5, $6DFE410E, $9F95C20D, $8CC531F9, $7EAEB2FA,
    $30E349B1, $C288CAB2, $D1D83946, $23B3BA45, $F779DEAE, $05125DAD, $1642AE59, $E4292D5A,
    $BA3A117E, $4851927D, $5B016189, $A96AE28A, $7DA08661, $8FCB0562, $9C9BF696, $6EF07595,
    $417B1DBC, $B3109EBF, $A0406D4B, $522BEE48, $86E18AA3, $748A09A0, $67DAFA54, $95B17957,
    $CBA24573, $39C9C670, $2A993584, $D8F2B687, $0C38D26C, $FE53516F, $ED03A29B, $1F682198,
    $5125DAD3, $A34E59D0, $B01EAA24, $42752927, $96BF4DCC, $64D4CECF, $77843D3B, $85EFBE38,
    $DBFC821C, $2997011F, $3AC7F2EB, $C8AC71E8, $1C661503, $EE0D9600, $FD5D65F4, $0F36E6F7,
    $61C69362, $93AD1061, $80FDE395, $72966096, $A65C047D, $5437877E, $4767748A, $B50CF789,
    $EB1FCBAD, $197448AE, $0A24BB5A, $F84F3859, $2C855CB2, $DEEEDFB1, $CDBE2C45, $3FD5AF46,
    $7198540D, $83F3D70E, $90A324FA, $62C8A7F9, $B602C312, $44694011, $5739B3E5, $A55230E6,
    $FB410CC2, $092A8FC1, $1A7A7C35, $E811FF36, $3CDB9BDD, $CEB018DE, $DDE0EB2A, $2F8B6829,
    $82F63B78, $709DB87B, $63CD4B8F, $91A6C88C, $456CAC67, $B7072F64, $A457DC90, $563C5F93,
    $082F63B7, $FA44E0B4, $E9141340, $1B7F9043, $CFB5F4A8, $3DDE77AB, $2E8E845F, $DCE5075C,
    $92A8FC17, $60C37F14, $73938CE0, $81F80FE3, $55326B08, $A759E80B, $B4091BFF, $466298FC,
    $1871A4D8, $EA1A27DB, $F94AD42F, $0B21572C, $DFEB33C7, $2D80B0C4, $3ED04330, $CCBBC033,
    $A24BB5A6, $502036A5, $4370C551, $B11B4652, $65D122B9, $97BAA1BA, $84EA524E, $7681D14D,
    $2892ED69, $DAF96E6A, $C9A99D9E, $3BC21E9D, $EF087A76, $1D63F975, $0E330A81, $FC588982,
    $B21572C9, $407EF1CA, $532E023E, $A145813D, $758FE5D6, $87E466D5, $94B49521, $66DF1622,
    $38CC2A06, $CAA7A905, $D9F75AF1, $2B9CD9F2, $FF56BD19, $0D3D3E1A, $1E6DCDEE, $EC064EED,
    $C38D26C4, $31E6A5C7, $22B65633, $D0DDD530, $0417B1DB, $F67C32D8, $E52CC12C, $1747422F,
    $49547E0B, $BB3FFD08, $A86F0EFC, $5A048DFF, $8ECEE914, $7CA56A17, $6FF599E3, $9D9E1AE0,
    $D3D3E1AB, $21B862A8, $32E8915C, $C083125F, $144976B4, $E622F5B7, $F5720643, $07198540,
    $590AB964, $AB613A67, $B831C993, $4A5A4A90, $9E902E7B, $6CFBAD78, $7FAB5E8C, $8DC0DD8F,
    $E330A81A, $115B2B19, $020BD8ED, $F0605BEE, $24AA3F05, $D6C1BC06, $C5914FF2, $37FACCF1,
    $69E9F0D5, $9B8273D6, $88D28022, $7AB90321, $AE7367CA, $5C18E4C9, $4F48173D, $BD23943E,
    $F36E6F75, $0105EC76, $12551F82, $E03E9C81, $34F4F86A, $C69F7B69, $D5CF889D, $27A40B9E,
    $79B737BA, $8BDCB4B9, $988C474D, $6AE7C44E, $BE2DA0A5, $4C4623A6, $5F16D052, $AD7D5351);
var
  Input:  array[0..7] of Byte absolute v;
  Temp:   UInt32;
  i:      Integer;
begin
Temp := UInt32(crc);
For i := Low(Input) to High(Input) do
  Temp := CRCTable[Byte(Temp xor UInt32(Input[i]))] xor (Temp shr 8);
Result := UInt64(Temp);
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}
Function _mm_crc32_u64_asm(crc,v: UInt64): UInt64; register; assembler;
asm
{$IFDEF x64}
    CRC32   crc,  v
    MOV     RAX,  crc
{$ELSE}
    MOV     EAX,  dword ptr [CRC]

  {$IFDEF ASM_MachineCode}
    DB  $F2, $0F, $38, $F1, $45, $08    // CRC32   EAX,  dword ptr [EBP + 8]
    DB  $F2, $0F, $38, $F1, $45, $0C    // CRC32   EAX,  dword ptr [EPB + 12]
  {$ELSE}
    CRC32   EAX,  dword ptr [v]
    CRC32   EAX,  dword ptr [v + 4]
  {$ENDIF}

    XOR     EDX,  EDX
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

var
  _mm_crc32_u64_var:  Function(crc,v: UInt64): UInt64 = _mm_crc32_u64_pas;

{-------------------------------------------------------------------------------
    CRC32 intrinsic - public part
-------------------------------------------------------------------------------}

Function _mm_crc32_u64(crc,v: UInt64): UInt64;
begin
Result := _mm_crc32_u64_var(crc,v);
end;

{===============================================================================
    Unit implementation management - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Unit implementation management - internals
-------------------------------------------------------------------------------}
const
  UIM_CITYHASH_PASCAL_IMPL: array[TUIM_CityHash_Function] of Pointer = (@_mm_crc32_u64_pas);
{$IFNDEF PurePascal}
  UIM_CITYHASH_ASSEMBLY_IMPL: array[TUIM_CityHash_Function] of Pointer = (@_mm_crc32_u64_asm);
{$ENDIF}

//------------------------------------------------------------------------------

Function UIM_GetFunctionVarAddr(Func: TUIM_CityHash_Function): PPointer;
begin
case Func of
  fnCRC32Intrinsic: Result := Addr(@_mm_crc32_u64_var);
else
  raise ECITYUnknownFunction.CreateFmt('UIM_GetFunctionVarAddr: Unknown function %d.',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function UIM_CheckASMSupport(Func: TUIM_CityHash_Function): Boolean;
begin
Result := False;
{$IFNDEF PurePascal}
with TSimpleCPUID.Create do
try
  case Func of
    fnCRC32Intrinsic: Result := Info.SupportedExtensions.CRC32;
  else
    raise ECITYUnknownFunction.CreateFmt('UIM_CheckASMSupport: Unknown function (%d).',[Ord(Func)]);
  end;
finally
  Free;
end;
{$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    Unit implementation management - public functions
-------------------------------------------------------------------------------}

Function UIM_CityHash_AvailableFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementations;
begin
case Func of
  fnCRC32Intrinsic: Result := [imNone,imPascal{$IFNDEF PurePascal},imAssembly,imAccelerated{$ENDIF}];
else
  raise ECITYUnknownFunction.CreateFmt('UIM_CityHash_AvailableFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_CityHash_SupportedFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementations;
begin
case Func of
  fnCRC32Intrinsic:
    If UIM_CheckASMSupport(Func) then
      Result := [imNone,imPascal,imAssembly,imAccelerated]
    else
      Result := [imNone,imPascal];
else
  raise ECITYUnknownFunction.CreateFmt('UIM_CityHash_SupportedFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_CityHash_GetFuncImpl(Func: TUIM_CityHash_Function): TUIM_CityHash_Implementation;
var
  FuncVarAddr:  PPointer;
begin
Result := imNone;
FuncVarAddr := UIM_GetFunctionVarAddr(Func);
// no need to check FuncVarAddr for validity
If Assigned(FuncVarAddr^) then
  begin
    If FuncVarAddr^ = UIM_CITYHASH_PASCAL_IMPL[Func] then
      Result := imPascal
  {$IFNDEF PurePascal}
    else If FuncVarAddr^ = UIM_CITYHASH_ASSEMBLY_IMPL[Func] then
      Result := imAccelerated
  {$ENDIF};
  end;
end;

//------------------------------------------------------------------------------

Function UIM_CityHash_SetFuncImpl(Func: TUIM_CityHash_Function; NewImpl: TUIM_CityHash_Implementation): TUIM_CityHash_Implementation;
begin
Result := UIM_CityHash_GetFuncImpl(Func);
case NewImpl of
  imPascal:
    UIM_GetFunctionVarAddr(Func)^ := UIM_CITYHASH_PASCAL_IMPL[Func];
  imAssembly,
  imAccelerated:
{$IFDEF PurePascal}
    UIM_GetFunctionVarAddr(Func)^ := UIM_CITYHASH_PASCAL_IMPL[Func];
{$ELSE}
    UIM_GetFunctionVarAddr(Func)^ := UIM_CITYHASH_ASSEMBLY_IMPL[Func];
{$ENDIF}   
else
 {imNone}
  UIM_GetFunctionVarAddr(Func)^ := nil;
end;
end;

{===============================================================================
    Unit initialization
===============================================================================}

procedure Initialize;
var
  i:  TUIM_CityHash_Function;
begin
For i := Low(TUIM_CityHash_Function) to High(TUIM_CityHash_Function) do
  If UIM_CheckASMSupport(i) then
    UIM_CityHash_SetFuncImpl(i,imAccelerated)
  else
    UIM_CityHash_SetFuncImpl(i,imPascal);
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.
