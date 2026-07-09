{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple hash

    This library provides means of calculating very simple hash of almost any
    provided data. Two versions of this hash are provided - 32bit and 64bit.

    The hash was designed to be maximally simple and also fast to calculate,
    which means it is not particularly good in terms of security, collisions,
    diffusion and other desired hash properties - and it for sure is not a
    cryptographic hash.
    It is meant for situations where one simply needs SOME hash, and where
    security is of no great concern (eg. hash lists or hash maps).

    The implementation is pretty much random, but it was inspired by other
    code bases I have seen over the years - unfortunatelly I cannot name any,
    simply because I do not remember them. So if anyone thinks it is a stealed
    code, it is not, but that does not mean someone cannot recognize it, sorry!

  Version 1.1 (2026-07-04)

  Last change 2026-07-08

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleHash

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM    - github.com/TheLazyTomcat/Lib.BasicUIM
    HashBase    - github.com/TheLazyTomcat/Lib.HashBase
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SimpleHash;
{
  SimpleHash_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF SimpleHash_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

//------------------------------------------------------------------------------

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH CLASSICPROCVARS+}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
{$ENDIF}
{$H+}

interface

uses
  Classes,
  AuxTypes, BasicUIM, HashBase;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESHException = class(EHashException);

  ESHNoImplementation  = class(ESHException);
  ESHIncompatibleClass = class(ESHException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TSimpleHash32 and TSimpleHash64 are in memory always ordered from
  least significant byte to most significant byte (little endian).

  Types TSimpleHash32Sys and TSimpleHash64Sys have no such guarantee and their
  endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use methods SimpleHash32ToLE or SimpleHash64ToLe for little endian and
  SimpleHash32ToBE or SimpleHash64ToBE for big endian. Note that these methods
  are expecting the input value to be in default ordering, if it is not, the
  result will be wrong. Be careful when using them.
}  
type
  TSimpleHash32 = packed array[0..3] of UInt8;
  PSimpleHash32 = ^TSimpleHash32;

  TSimpleHash64 = packed array[0..7] of UInt8;
  PSimpleHash64 = ^TSimpleHash64;

  TSimpleHash32Sys = UInt32;
  PSimpleHash32Sys = ^TSimpleHash32Sys;

  TSimpleHash64Sys = UInt64;
  PSimpleHash64Sys = ^TSimpleHash64Sys;

const
{
  Initial 32bit value (0x87CD219F) is a simple hash obtained when hashing
  ASCII-encoded string "ROL and XOR, nothing more!" (without quotes) with
  initial value of zero.

  Similarly, 64bit initial value (0xC404A918F6E67CBD) is a hash of string
  "ROL and XOR, nothing more, in 64!" (without quotes).
}
  InitialSimpleHash32: TSimpleHash32 = ($9F,$21,$CD,$87);
  InitialSimpleHash64: TSimpleHash64 = ($BD,$7C,$E6,$F6,$18,$A9,$04,$C4);

  ZeroSimpleHash32: TSimpleHash32 = (0,0,0,0);
  ZeroSimpleHash64: TSimpleHash64 = (0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash32Base
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleHash32Base - class declaration
===============================================================================}
type
  TSimpleHash32Base = class(TStreamHash)
  protected
    fInitialValue:  TSimpleHash32Sys;
    fSimpleHash32:  TSimpleHash32Sys;
    fProcessBuffer: Function(Init: TSimpleHash32Sys; const Buffer; Size: TMemSize): TSimpleHash32Sys register;
    Function GetInitialValue: TSimpleHash32; virtual;
    Function GetSimpleHash32: TSimpleHash32; virtual;
    Function GetHashImplementation: THashImplementation; override;
    procedure SetHashImplementation(Value: THashImplementation); override;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function SimpleHash32ToSys(Hash: TSimpleHash32): TSimpleHash32Sys; virtual;
    class Function SimpleHash32FromSys(Hash: TSimpleHash32Sys): TSimpleHash32; virtual;
    class Function SimpleHash32ToLE(Hash: TSimpleHash32): TSimpleHash32; virtual;
    class Function SimpleHash32ToBE(Hash: TSimpleHash32): TSimpleHash32; virtual;
    class Function SimpleHash32FromLE(Hash: TSimpleHash32): TSimpleHash32; virtual;
    class Function SimpleHash32FromBE(Hash: TSimpleHash32): TSimpleHash32; virtual;
    class Function HashImplementationsAvailable: THashImplementations; override;
    class Function HashImplementationsSupported: THashImplementations; override;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSimpleHash32); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSimpleHash32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property InitialValue: TSimpleHash32 read GetInitialValue;
    property InitialValueSys: TSimpleHash32Sys read fInitialValue;
    property SimpleHash32: TSimpleHash32 read GetSimpleHash32;
    property SimpleHash32Sys: TSimpleHash32Sys read fSimpleHash32;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash32Init
--------------------------------------------------------------------------------
===============================================================================}
{
  This class provides the same processing (works with the same algorithm) as
  TSimpleHash32Base, the only diffrence is that initial value of the calculated
  hash is not set to zero, as is the case in base class, but to a predefined
  non-zero value (see source code for more details).
  It also allows you to change this initial value if you want to create your
  own variant. 
}
{===============================================================================
    TSimpleHash32Init - class declaration
===============================================================================}
type
  TSimpleHash32Init = class(TSimpleHash32Base)
  protected
    procedure Initialize; override;
    procedure SetInitialValue(NewValue: TSimpleHash32); virtual;
  public
    class Function HashName: String; override;
    property InitialValue: TSimpleHash32 read GetInitialValue write SetInitialValue;
    property InitialValueSys: TSimpleHash32Sys read fInitialValue write fInitialValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash64Base                                                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleHash64Base - class declaration
===============================================================================}
type
  TSimpleHash64Base = class(TStreamHash)
  protected
    fInitialValue:  TSimpleHash64Sys;
    fSimpleHash64:  TSimpleHash64Sys;
    fProcessBuffer: Function(Init: TSimpleHash64Sys; const Buffer; Size: TMemSize): TSimpleHash64Sys register;
    Function GetInitialValue: TSimpleHash64; virtual;
    Function GetSimpleHash64: TSimpleHash64; virtual;
    Function GetHashImplementation: THashImplementation; override;
    procedure SetHashImplementation(Value: THashImplementation); override;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function SimpleHash64ToSys(Hash: TSimpleHash64): TSimpleHash64Sys; virtual;
    class Function SimpleHash64FromSys(Hash: TSimpleHash64Sys): TSimpleHash64; virtual;
    class Function SimpleHash64ToLE(Hash: TSimpleHash64): TSimpleHash64; virtual;
    class Function SimpleHash64ToBE(Hash: TSimpleHash64): TSimpleHash64; virtual;
    class Function SimpleHash64FromLE(Hash: TSimpleHash64): TSimpleHash64; virtual;
    class Function SimpleHash64FromBE(Hash: TSimpleHash64): TSimpleHash64; virtual;
    class Function HashImplementationsAvailable: THashImplementations; override;
    class Function HashImplementationsSupported: THashImplementations; override;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSimpleHash64); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSimpleHash64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property InitialValue: TSimpleHash64 read GetInitialValue;
    property InitialValueSys: TSimpleHash64Sys read fInitialValue;
    property SimpleHash64: TSimpleHash64 read GetSimpleHash64;
    property SimpleHash64Sys: TSimpleHash64Sys read fSimpleHash64;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash64Init
--------------------------------------------------------------------------------
===============================================================================}
{
  Similarly to 32bit variant, this class differs from TSimpleHash64Base only
  in initial value of the hash.
}
{===============================================================================
    TSimpleHash64Init - class declaration
===============================================================================}
type
  TSimpleHash64Init = class(TSimpleHash64Base)
  protected
    procedure Initialize; override;
    procedure SetInitialValue(NewValue: TSimpleHash64); virtual;
  public
    class Function HashName: String; override;
    property InitialValue: TSimpleHash64 read GetInitialValue write SetInitialValue;
    property InitialValueSys: TSimpleHash64Sys read fInitialValue write fInitialValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
  Almost all in-here provided functions are calling direct implementation,
  the exceptions are functions StreamSimpleHash32/64 and FileSimpleHash32/64
  that are using instances of class TSimpleHash32/64Init and their methods to
  do the work.
}
{===============================================================================
    Procedural interface - 32bit hash declaration
===============================================================================}

Function SimpleHash32ToStr(const Hash: TSimpleHash32): String;
Function StrToSimpleHash32(const Str: String): TSimpleHash32;
Function TryStrToSimpleHash32(const Str: String; out Hash: TSimpleHash32): Boolean;
Function StrToSimpleHash32Def(const Str: String; Default: TSimpleHash32): TSimpleHash32;

Function CompareSimpleHash32(const A,B: TSimpleHash32): Integer;
Function SameSimpleHash32(const A,B: TSimpleHash32): Boolean;

//------------------------------------------------------------------------------

Function BufferSimpleHash32(const Hash: TSimpleHash32; const Buffer; Size: TMemSize): TSimpleHash32; overload;
Function BufferSimpleHash32(const Buffer; Size: TMemSize): TSimpleHash32; overload;

Function AnsiStringSimpleHash32(const Str: AnsiString): TSimpleHash32;
Function WideStringSimpleHash32(const Str: WideString): TSimpleHash32;
Function StringSimpleHash32(const Str: String): TSimpleHash32;

Function StreamSimpleHash32(Stream: TStream; Count: Int64 = -1): TSimpleHash32;
Function FileSimpleHash32(const FileName: String): TSimpleHash32;

//------------------------------------------------------------------------------
type
  TSimpleHash32Context = type TSimpleHash32Sys;

Function SimpleHash32_Init: TSimpleHash32Context;
procedure SimpleHash32_Update(var Context: TSimpleHash32Context; const Buffer; Size: TMemSize);
Function SimpleHash32_Final(var Context: TSimpleHash32Context; const Buffer; Size: TMemSize): TSimpleHash32; overload;
Function SimpleHash32_Final(var Context: TSimpleHash32Context): TSimpleHash32; overload;
Function SimpleHash32_Hash(const Buffer; Size: TMemSize): TSimpleHash32;

{===============================================================================
    Procedural interface - 64bit hash declaration
===============================================================================}

Function SimpleHash64ToStr(const Hash: TSimpleHash64): String;
Function StrToSimpleHash64(const Str: String): TSimpleHash64;
Function TryStrToSimpleHash64(const Str: String; out Hash: TSimpleHash64): Boolean;
Function StrToSimpleHash64Def(const Str: String; Default: TSimpleHash64): TSimpleHash64;

Function CompareSimpleHash64(const A,B: TSimpleHash64): Integer;
Function SameSimpleHash64(const A,B: TSimpleHash64): Boolean;

//------------------------------------------------------------------------------

Function BufferSimpleHash64(const Hash: TSimpleHash64; const Buffer; Size: TMemSize): TSimpleHash64; overload;
Function BufferSimpleHash64(const Buffer; Size: TMemSize): TSimpleHash64; overload;

Function AnsiStringSimpleHash64(const Str: AnsiString): TSimpleHash64;
Function WideStringSimpleHash64(const Str: WideString): TSimpleHash64;
Function StringSimpleHash64(const Str: String): TSimpleHash64;

Function StreamSimpleHash64(Stream: TStream; Count: Int64 = -1): TSimpleHash64;
Function FileSimpleHash64(const FileName: String): TSimpleHash64;

//------------------------------------------------------------------------------
type
  TSimpleHash64Context = type TSimpleHash64Sys;

Function SimpleHash64_Init: TSimpleHash64Context;
procedure SimpleHash64_Update(var Context: TSimpleHash64Context; const Buffer; Size: TMemSize);
Function SimpleHash64_Final(var Context: TSimpleHash64Context; const Buffer; Size: TMemSize): TSimpleHash64; overload;
Function SimpleHash64_Final(var Context: TSimpleHash64Context): TSimpleHash64; overload;
Function SimpleHash64_Hash(const Buffer; Size: TMemSize): TSimpleHash64;

implementation

uses
  SysUtils,
  UInt64Utils;

{===============================================================================
    UIM variables
===============================================================================}
var
  ImplManager:        TImplementationManager = nil;
  ProcessBufferDummy: Pointer = nil;  

{===============================================================================
    Main implementation
===============================================================================}

Function SimpleHash32_PAS(Init: TSimpleHash32Sys; const Buffer; Size: TMemSize): TSimpleHash32Sys; register;
var
  WorkPtr:  PByte;
  i:        TMemSize;
begin
Result := Init;
If Size > 0 then
  begin
    WorkPtr := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := TSimpleHash32Sys(TSimpleHash32Sys(Result shl 3) or
          (Result shr 29)){ROL(3)} xor TSimpleHash32Sys(WorkPtr^);
        Inc(WorkPtr);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function SimpleHash64_PAS(Init: TSimpleHash64Sys; const Buffer; Size: TMemSize): TSimpleHash64Sys; register;
var
  WorkPtr:  PByte;
  i:        TMemSize;
begin
Result := Init;
If Size > 0 then
  begin
    WorkPtr := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := TSimpleHash64Sys(TSimpleHash64Sys(Result shl 3) or
          (Result shr 61){ROL(3)}) xor TSimpleHash64Sys(WorkPtr^);
        Inc(WorkPtr);
      end;
  end;
end;

//------------------------------------------------------------------------------
{$IFNDEF PurePascal}

Function SimpleHash32_ASM(Init: TSimpleHash32Sys; const Buffer; Size: TMemSize): TSimpleHash32Sys; register; assembler;
asm
{-------------------------------------------------------------------------------
                      win32 & lin32     win64         lin64
           Init            EAX           ECX           EDI
        @Buffer            EDX           RDX           RSI
           Size            ECX            R8           RDX
           
         Result            EAX           EAX           EAX
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}

      TEST    R8, R8
      JZ      @RoutineEnd

  @ProcessingLoop:

      ROL     ECX, 3
      MOVZX   EAX, byte ptr [RDX]
      XOR     ECX, EAX

      INC     RDX
      DEC     R8
      JNZ     @ProcessingLoop

  @RoutineEnd:

      MOV     EAX, ECX  

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      TEST    RDX, RDX
      JZ      @RoutineEnd

  @ProcessingLoop:

      ROL     EDI, 3
      MOVZX   EAX, byte ptr [RSI]
      XOR     EDI, EAX

      INC     RSI
      DEC     RDX
      JNZ     @ProcessingLoop

  @RoutineEnd:

      MOV     EAX, EDI 

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      PUSH    EBX
      JECXZ   @RoutineEnd

  @ProcessingLoop:

      ROL     EAX, 3
      MOVZX   EBX, byte ptr [EDX]
      XOR     EAX, EBX

      INC     EDX
      DEC     ECX
      JNZ     @ProcessingLoop

  @RoutineEnd:
      POP     EBX
      
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function SimpleHash64_ASM(Init: TSimpleHash64Sys; const Buffer; Size: TMemSize): TSimpleHash64Sys; register; assembler;
asm
{-------------------------------------------------------------------------------
                      win32 & lin32     win64         lin64
           Init         (EBP + 8)        RCX           RDI
        @Buffer            EAX           RDX           RSI
           Size            EDX            R8           RDX

         Result          EDX:EAX         RAX           RAX
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}

      TEST    R8, R8
      JZ      @RoutineEnd

  @ProcessingLoop:

      ROL     RCX, 3
      MOVZX   RAX, byte ptr [RDX]
      XOR     RCX, RAX

      INC     RDX
      DEC     R8
      JNZ     @ProcessingLoop

  @RoutineEnd:

      MOV     RAX, RCX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      TEST    RDX, RDX
      JZ      @RoutineEnd

  @ProcessingLoop:

      ROL     RDI, 3
      MOVZX   RAX, byte ptr [RSI]
      XOR     RDI, RAX

      INC     RSI
      DEC     RDX
      JNZ     @ProcessingLoop

  @RoutineEnd:

      MOV     RAX, RDI 

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      PUSH    EBX
      PUSH    ESI
      
      MOV     ESI, EAX
      MOV     ECX, EDX
      MOV     EAX, dword ptr [Init]
      MOV     EDX, dword ptr [Init + 4]
      JECXZ   @RoutineEnd

  @ProcessingLoop:

      MOV     EBX, EDX
      SHLD    EDX, EAX, 3
      SHLD    EAX, EBX, 3
      MOVZX   EBX, byte ptr [ESI]
      XOR     EAX, EBX

      INC     ESI
      DEC     ECX
      JNZ     @ProcessingLoop

  @RoutineEnd:
      POP     ESI
      POP     EBX
            
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

Function SimpleHash32Compare(const A,B: TSimpleHash32Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function SimpleHash32Same(const A,B: TSimpleHash32Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function SimpleHash32AsString(const SimpleHash32: TSimpleHash32Sys): String;
begin
Result := IntToHex(SimpleHash32,8);
end;

//------------------------------------------------------------------------------

Function SimpleHash32FromString(const Str: String): TSimpleHash32Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSimpleHash32Sys(StrToInt(Str))
    else
      Result := TSimpleHash32Sys(StrToInt('$' + Str));
  end
else Result := TSimpleHash32Base.SimpleHash32ToSys(ZeroSimpleHash32);
end;

//==============================================================================

Function SimpleHash64Compare(const A,B: TSimpleHash64Sys): Integer;
begin
Result := CompareUInt64(A,B);
end;

//------------------------------------------------------------------------------

Function SimpleHash64Same(const A,B: TSimpleHash64Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function SimpleHash64AsString(const SimpleHash64: TSimpleHash64Sys): String;
begin
Result := IntToHex(SimpleHash64,16);
end;

//------------------------------------------------------------------------------

Function SimpleHash64FromString(const Str: String): TSimpleHash64Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSimpleHash64Sys(StrToInt64(Str))
    else
      Result := TSimpleHash64Sys(StrToInt64('$' + Str));
  end
else Result := TSimpleHash64Base.SimpleHash64ToSys(ZeroSimpleHash64);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash32Base                                                             
--------------------------------------------------------------------------------
===============================================================================}

Function SwapEndian(Value: TSimpleHash32Sys): TSimpleHash32Sys; overload;
begin
Result := TSimpleHash32Sys(
  ((Value and $000000FF) shl 24) or
  ((Value and $0000FF00) shl 8) or
  ((Value and $00FF0000) shr 8) or
  ((Value and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TSimpleHash32): TSimpleHash32; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TSimpleHash32(SwapEndian(TSimpleHash32Sys(Value)));
end;

{===============================================================================
    TSimpleHash32Base - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleHash32Base - protected methods
-------------------------------------------------------------------------------}

Function TSimpleHash32Base.GetInitialValue: TSimpleHash32;
begin
Result := SimpleHash32FromSys(fInitialValue);
end;

//------------------------------------------------------------------------------

Function TSimpleHash32Base.GetSimpleHash32: TSimpleHash32;
begin
Result := SimpleHash32FromSys(fSimpleHash32);
end;

//------------------------------------------------------------------------------

Function TSimpleHash32Base.GetHashImplementation: THashImplementation;
var
  Routing:  TUIMRouting;
  Index:    Integer;
begin
Routing := ImplManager.RoutingFindObj(0);
If Routing.Find(@fProcessBuffer,Index) then
  Result := THashImplementation(Routing[Index].ImplementationID)
else
  raise ESHNoImplementation.Create('TSimpleHash32Base.GetHashImplementation: No implementation selected.');
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.SetHashImplementation(Value: THashImplementation);
var
  Routing:  TUIMRouting;
  Index:    Integer;
begin
// do not call inherited
Routing := ImplManager.RoutingFindObj(0);
If Routing.Follow(TUIMIdentifier(Value),Index) then
  @fProcessBuffer := Routing[Index].ImplementorFunction
else
  raise ESHNoImplementation.CreateFmt('TSimpleHash32Base.SetHashImplementation: Selected implementation (%d) not found.',[Ord(Value)]);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fSimpleHash32 := fProcessBuffer(fSimpleHash32,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.Initialize;
begin
inherited;
fInitialValue := SimpleHash32ToSys(ZeroSimpleHash32);
fSimpleHash32 := fInitialValue;
HashImplementation := hiAssembly;
end;

{-------------------------------------------------------------------------------
    TSimpleHash32Base - public methods
-------------------------------------------------------------------------------}

class Function TSimpleHash32Base.SimpleHash32ToSys(Hash: TSimpleHash32): TSimpleHash32Sys;
begin
Result := TSimpleHash32Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.SimpleHash32FromSys(Hash: TSimpleHash32Sys): TSimpleHash32;
begin
Result := TSimpleHash32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.SimpleHash32ToLE(Hash: TSimpleHash32): TSimpleHash32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.SimpleHash32ToBE(Hash: TSimpleHash32): TSimpleHash32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.SimpleHash32FromLE(Hash: TSimpleHash32): TSimpleHash32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.SimpleHash32FromBE(Hash: TSimpleHash32): TSimpleHash32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashImplementationsAvailable: THashImplementations;
var
  i:  Integer;
begin
Result := [];
with ImplManager.RoutingFindObj(0) do
  For i := LowIndex to HighIndex do
    If ifAvailable in Implementations[i].ImplementationFlags then
      Include(Result,THashImplementation(Implementations[i].ImplementationID));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashImplementationsSupported: THashImplementations;
var
  i:  Integer;
begin
Result := [];
with ImplManager.RoutingFindObj(0) do
  For i := LowIndex to HighIndex do
    If [ifAvailable,ifSupported] <= Implementations[i].ImplementationFlags then
      Include(Result,THashImplementation(Implementations[i].ImplementationID));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashSize: TMemSize;
begin
Result := SizeOf(TSimpleHash32);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash32Base.HashName: String;
begin
Result := 'SimpleHash32Base';
end;

//------------------------------------------------------------------------------

constructor TSimpleHash32Base.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSimpleHash32Base then
  begin
    fInitialValue := TSimpleHash32Base(Hash).InitialValueSys;
    fSimpleHash32 := TSimpleHash32Base(Hash).SimpleHash32Sys;
  end
else raise ESHIncompatibleClass.CreateFmt('TSimpleHash32Base.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleHash32Base.CreateAndInitFrom(Hash: TSimpleHash32);
begin
CreateAndInit;
fSimpleHash32 := SimpleHash32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.Init;
begin
inherited;
fSimpleHash32 := fInitialValue;
end;

//------------------------------------------------------------------------------

Function TSimpleHash32Base.Compare(Hash: THashBase): Integer;
begin
If Hash is TSimpleHash32Base then
  Result := SimpleHash32Compare(fSimpleHash32,TSimpleHash32Base(Hash).SimpleHash32Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSimpleHash32Base.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSimpleHash32Base.Same(Hash: THashBase): Boolean;
begin
If Hash is TSimpleHash32Base then
  Result := SimpleHash32Same(fSimpleHash32,TSimpleHash32Base(Hash).SimpleHash32Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSimpleHash32Base.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSimpleHash32Base.AsString: String;
begin
Result := SimpleHash32AsString(fSimpleHash32);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.FromString(const Str: String);
begin
fSimpleHash32 := SimpleHash32FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.FromStringDef(const Str: String; const Default: TSimpleHash32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSimpleHash32 := SimpleHash32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSimpleHash32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SimpleHash32ToBE{$ELSE}SimpleHash32ToLE{$ENDIF}(SimpleHash32FromSys(fSimpleHash32));
  heLittle: Temp := SimpleHash32ToLE(SimpleHash32FromSys(fSimpleHash32));
  heBig:    Temp := SimpleHash32ToBE(SimpleHash32FromSys(fSimpleHash32));
else
 {heDefault}
  Temp := SimpleHash32FromSys(fSimpleHash32);
end;
Stream.WriteBuffer(Temp,SizeOf(TSimpleHash32));
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Base.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSimpleHash32;
begin
Temp := ZeroSimpleHash32;
Stream.ReadBuffer(Temp,SizeOf(TSimpleHash32));
case Endianness of
  heSystem: fSimpleHash32 := SimpleHash32ToSys({$IFDEF ENDIAN_BIG}SimpleHash32FromBE{$ELSE}SimpleHash32FromLE{$ENDIF}(Temp));
  heLittle: fSimpleHash32 := SimpleHash32ToSys(SimpleHash32FromLE(Temp));
  heBig:    fSimpleHash32 := SimpleHash32ToSys(SimpleHash32FromBE(Temp));
else
 {heDefault}
  fSimpleHash32 := SimpleHash32ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash32Init
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleHash32Init - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleHash32Init - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleHash32Init.Initialize;
begin
inherited;
fInitialValue := SimpleHash32ToSys(InitialSimpleHash32);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash32Init.SetInitialValue(NewValue: TSimpleHash32);
begin
fInitialValue := SimpleHash32ToSys(NewValue);
end;

{-------------------------------------------------------------------------------
    TSimpleHash32Init - public methods
-------------------------------------------------------------------------------}

class Function TSimpleHash32Init.HashName: String;
begin
Result := 'SimpleHash32Init';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash64Base                                                             
--------------------------------------------------------------------------------
===============================================================================}

Function SwapEndian(Value: TSimpleHash64Sys): TSimpleHash64Sys; overload;
begin
Int64Rec(Result).Hi := UInt32(SwapEndian(TSimpleHash32Sys(Int64Rec(Value).Lo)));
Int64Rec(Result).Lo := UInt32(SwapEndian(TSimpleHash32Sys(Int64Rec(Value).Hi)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TSimpleHash64): TSimpleHash64; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TSimpleHash64(SwapEndian(TSimpleHash64Sys(Value)));
end;

{===============================================================================
    TSimpleHash64Base - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleHash64Base - protected methods
-------------------------------------------------------------------------------}

Function TSimpleHash64Base.GetInitialValue: TSimpleHash64;
begin
Result := SimpleHash64FromSys(fInitialValue);
end;

//------------------------------------------------------------------------------

Function TSimpleHash64Base.GetSimpleHash64: TSimpleHash64;
begin
Result := SimpleHash64FromSys(fSimpleHash64);
end;

//------------------------------------------------------------------------------

Function TSimpleHash64Base.GetHashImplementation: THashImplementation;
var
  Routing:  TUIMRouting;
  Index:    Integer;
begin
Routing := ImplManager.RoutingFindObj(1);
If Routing.Find(@fProcessBuffer,Index) then
  Result := THashImplementation(Routing[Index].ImplementationID)
else
  raise ESHNoImplementation.Create('TSimpleHash64Base.GetHashImplementation: No implementation selected.');
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.SetHashImplementation(Value: THashImplementation);
var
  Routing:  TUIMRouting;
  Index:    Integer;
begin
Routing := ImplManager.RoutingFindObj(1);
If Routing.Follow(TUIMIdentifier(Value),Index) then
  @fProcessBuffer := Routing[Index].ImplementorFunction
else
  raise ESHNoImplementation.CreateFmt('TSimpleHash64Base.SetHashImplementation: Selected implementation (%d) not found.',[Ord(Value)]);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fSimpleHash64 := fProcessBuffer(fSimpleHash64,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.Initialize;
begin
inherited;
fInitialValue := SimpleHash64ToSys(ZeroSimpleHash64);
fSimpleHash64 := fInitialValue;
HashImplementation := hiAssembly;
end;

{-------------------------------------------------------------------------------
    TSimpleHash64Base - public methods
-------------------------------------------------------------------------------}

class Function TSimpleHash64Base.SimpleHash64ToSys(Hash: TSimpleHash64): TSimpleHash64Sys;
begin
Result := TSimpleHash64Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.SimpleHash64FromSys(Hash: TSimpleHash64Sys): TSimpleHash64;
begin
Result := TSimpleHash64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.SimpleHash64ToLE(Hash: TSimpleHash64): TSimpleHash64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.SimpleHash64ToBE(Hash: TSimpleHash64): TSimpleHash64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.SimpleHash64FromLE(Hash: TSimpleHash64): TSimpleHash64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.SimpleHash64FromBE(Hash: TSimpleHash64): TSimpleHash64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashImplementationsAvailable: THashImplementations;
var
  i:  Integer;
begin
Result := [];
with ImplManager.RoutingFindObj(0) do
  For i := LowIndex to HighIndex do
    If ifAvailable in Implementations[i].ImplementationFlags then
      Include(Result,THashImplementation(Implementations[i].ImplementationID));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashImplementationsSupported: THashImplementations;
var
  i:  Integer;
begin
Result := [];
with ImplManager.RoutingFindObj(0) do
  For i := LowIndex to HighIndex do
    If [ifAvailable,ifSupported] <= Implementations[i].ImplementationFlags then
      Include(Result,THashImplementation(Implementations[i].ImplementationID));
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashSize: TMemSize;
begin
Result := SizeOf(TSimpleHash64);
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

class Function TSimpleHash64Base.HashName: String;
begin
Result := 'SimpleHash64Base'
end;

//------------------------------------------------------------------------------

constructor TSimpleHash64Base.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSimpleHash64Base then
  begin
    fInitialValue := TSimpleHash64Base(Hash).InitialValueSys;
    fSimpleHash64 := TSimpleHash64Base(Hash).SimpleHash64Sys;
  end
else raise ESHIncompatibleClass.CreateFmt('TSimpleHash64Base.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleHash64Base.CreateAndInitFrom(Hash: TSimpleHash64);
begin
CreateAndInit;
fSimpleHash64 := SimpleHash64ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.Init;
begin
inherited;
fSimpleHash64 := fInitialValue;
end;

//------------------------------------------------------------------------------

Function TSimpleHash64Base.Compare(Hash: THashBase): Integer;
begin
If Hash is TSimpleHash64Base then
  Result := SimpleHash64Compare(fSimpleHash64,TSimpleHash64Base(Hash).SimpleHash64Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSimpleHash64Base.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSimpleHash64Base.Same(Hash: THashBase): Boolean;
begin
If Hash is TSimpleHash64Base then
  Result := SimpleHash64Same(fSimpleHash64,TSimpleHash64Base(Hash).SimpleHash64Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSimpleHash64Base.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSimpleHash64Base.AsString: String;
begin
Result := SimpleHash64AsString(fSimpleHash64);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.FromString(const Str: String);
begin
fSimpleHash64 := SimpleHash64FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.FromStringDef(const Str: String; const Default: TSimpleHash64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSimpleHash64 := SimpleHash64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSimpleHash64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SimpleHash64ToBE{$ELSE}SimpleHash64ToLE{$ENDIF}(SimpleHash64FromSys(fSimpleHash64));
  heLittle: Temp := SimpleHash64ToLE(SimpleHash64FromSys(fSimpleHash64));
  heBig:    Temp := SimpleHash64ToBE(SimpleHash64FromSys(fSimpleHash64));
else
 {heDefault}
  Temp := SimpleHash64FromSys(fSimpleHash64);
end;
Stream.WriteBuffer(Temp,SizeOf(TSimpleHash64));
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Base.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSimpleHash64;
begin
Temp := ZeroSimpleHash64;
Stream.ReadBuffer(Temp,SizeOf(TSimpleHash64));
case Endianness of
  heSystem: fSimpleHash64 := SimpleHash64ToSys({$IFDEF ENDIAN_BIG}SimpleHash64FromBE{$ELSE}SimpleHash64FromLE{$ENDIF}(Temp));
  heLittle: fSimpleHash64 := SimpleHash64ToSys(SimpleHash64FromLE(Temp));
  heBig:    fSimpleHash64 := SimpleHash64ToSys(SimpleHash64FromBE(Temp));
else
 {heDefault}
  fSimpleHash64 := SimpleHash64ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleHash64Init
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleHash64Init - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleHash64Init - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleHash64Init.Initialize;
begin
inherited;
fInitialValue := SimpleHash64ToSys(InitialSimpleHash64);
end;

//------------------------------------------------------------------------------

procedure TSimpleHash64Init.SetInitialValue(NewValue: TSimpleHash64);
begin
fInitialValue := SimpleHash64ToSys(NewValue);
end;

{-------------------------------------------------------------------------------
    TSimpleHash64Init - public methods
-------------------------------------------------------------------------------}

class Function TSimpleHash64Init.HashName: String;
begin
Result := 'SimpleHash64Init';
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - 32bit hash implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash utility functions
-------------------------------------------------------------------------------}

Function SimpleHash32ToStr(const Hash: TSimpleHash32): String;
begin
Result := SimpleHash32AsString(TSimpleHash32Base.SimpleHash32ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSimpleHash32(const Str: String): TSimpleHash32;
begin
Result := TSimpleHash32Base.SimpleHash32FromSys(SimpleHash32FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSimpleHash32(const Str: String; out Hash: TSimpleHash32): Boolean;
begin
try
  Hash := TSimpleHash32Base.SimpleHash32FromSys(SimpleHash32FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSimpleHash32Def(const Str: String; Default: TSimpleHash32): TSimpleHash32;
begin
If not TryStrToSimpleHash32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSimpleHash32(const A,B: TSimpleHash32): Integer;
begin
Result := SimpleHash32Compare(TSimpleHash32Base.SimpleHash32ToSys(A),TSimpleHash32Base.SimpleHash32ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSimpleHash32(const A,B: TSimpleHash32): Boolean;
begin
Result := SimpleHash32Same(TSimpleHash32Base.SimpleHash32ToSys(A),TSimpleHash32Base.SimpleHash32ToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash processing functions
-------------------------------------------------------------------------------}

Function BufferSimpleHash32(const Hash: TSimpleHash32; const Buffer; Size: TMemSize): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(Hash),Buffer,Size));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(Hash),Buffer,Size));
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSimpleHash32(const Buffer; Size: TMemSize): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(InitialSimpleHash32),Buffer,Size));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(InitialSimpleHash32),Buffer,Size));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function AnsiStringSimpleHash32(const Str: AnsiString): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(InitialSimpleHash32),
    PAnsiChar(Str)^,TMemSize(Length(Str) * SizeOf(AnsiChar))));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(InitialSimpleHash32),
    PAnsiChar(Str)^,TMemSize(Length(Str) * SizeOf(AnsiChar))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function WideStringSimpleHash32(const Str: WideString): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(InitialSimpleHash32),
    PWideChar(Str)^,TMemSize(Length(Str) * SizeOf(WideChar))));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(InitialSimpleHash32),
    PWideChar(Str)^,TMemSize(Length(Str) * SizeOf(WideChar))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function StringSimpleHash32(const Str: String): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(InitialSimpleHash32),
    PChar(Str)^,TMemSize(Length(Str) * SizeOf(Char))));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(InitialSimpleHash32),
    PChar(Str)^,TMemSize(Length(Str) * SizeOf(Char))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function StreamSimpleHash32(Stream: TStream; Count: Int64 = -1): TSimpleHash32;
var
  Hasher: TSimpleHash32Init;
begin
Hasher := TSimpleHash32Init.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.SimpleHash32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSimpleHash32(const FileName: String): TSimpleHash32;
var
  Hasher: TSimpleHash32Init;
begin
Hasher := TSimpleHash32Init.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.SimpleHash32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash context functions
-------------------------------------------------------------------------------}

Function SimpleHash32_Init: TSimpleHash32Context;
begin
Result := TSimpleHash32Context(TSimpleHash32Base.SimpleHash32ToSys(InitialSimpleHash32));
end;

//------------------------------------------------------------------------------

procedure SimpleHash32_Update(var Context: TSimpleHash32Context; const Buffer; Size: TMemSize);
begin
{$IFDEF PurePascal}
TSimpleHash32Sys(Context) := SimpleHash32_PAS(TSimpleHash32Sys(Context),Buffer,Size);
{$ELSE}
TSimpleHash32Sys(Context) := SimpleHash32_ASM(TSimpleHash32Sys(Context),Buffer,Size);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function SimpleHash32_Final(var Context: TSimpleHash32Context; const Buffer; Size: TMemSize): TSimpleHash32;
begin
SimpleHash32_Update(Context,Buffer,Size);
Result := SimpleHash32_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SimpleHash32_Final(var Context: TSimpleHash32Context): TSimpleHash32;
begin
Result := TSimpleHash32Base.SimpleHash32FromSys(TSimpleHash32Sys(Context));
TSimpleHash32Sys(Context) := TSimpleHash32Base.SimpleHash32ToSys(ZeroSimpleHash32);
end;

//------------------------------------------------------------------------------

Function SimpleHash32_Hash(const Buffer; Size: TMemSize): TSimpleHash32;
begin
with TSimpleHash32Base do
{$IFDEF PurePascal}
  Result := SimpleHash32FromSys(SimpleHash32_PAS(SimpleHash32ToSys(InitialSimpleHash32),Buffer,Size));
{$ELSE}
  Result := SimpleHash32FromSys(SimpleHash32_ASM(SimpleHash32ToSys(InitialSimpleHash32),Buffer,Size));
{$ENDIF}
end;

{===============================================================================
    Procedural interface - 64bit hash implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash utility functions
-------------------------------------------------------------------------------}

Function SimpleHash64ToStr(const Hash: TSimpleHash64): String;
begin
Result := SimpleHash64AsString(TSimpleHash64Base.SimpleHash64ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSimpleHash64(const Str: String): TSimpleHash64;
begin
Result := TSimpleHash64Base.SimpleHash64FromSys(SimpleHash64FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSimpleHash64(const Str: String; out Hash: TSimpleHash64): Boolean;
begin
try
  Hash := TSimpleHash64Base.SimpleHash64FromSys(SimpleHash64FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSimpleHash64Def(const Str: String; Default: TSimpleHash64): TSimpleHash64;
begin
If not TryStrToSimpleHash64(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSimpleHash64(const A,B: TSimpleHash64): Integer;
begin
Result := SimpleHash64Compare(TSimpleHash64Base.SimpleHash64ToSys(A),TSimpleHash64Base.SimpleHash64ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSimpleHash64(const A,B: TSimpleHash64): Boolean;
begin
Result := SimpleHash64Same(TSimpleHash64Base.SimpleHash64ToSys(A),TSimpleHash64Base.SimpleHash64ToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash processing functions
-------------------------------------------------------------------------------}

Function BufferSimpleHash64(const Hash: TSimpleHash64; const Buffer; Size: TMemSize): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(Hash),Buffer,Size));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(Hash),Buffer,Size));
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSimpleHash64(const Buffer; Size: TMemSize): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(InitialSimpleHash64),Buffer,Size));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(InitialSimpleHash64),Buffer,Size));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function AnsiStringSimpleHash64(const Str: AnsiString): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(InitialSimpleHash64),
    PAnsiChar(Str)^,TMemSize(Length(Str) * SizeOf(AnsiChar))));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(InitialSimpleHash64),
    PAnsiChar(Str)^,TMemSize(Length(Str) * SizeOf(AnsiChar))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function WideStringSimpleHash64(const Str: WideString): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(InitialSimpleHash64),
    PWideChar(Str)^,TMemSize(Length(Str) * SizeOf(WideChar))));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(InitialSimpleHash64),
    PWideChar(Str)^,TMemSize(Length(Str) * SizeOf(WideChar))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function StringSimpleHash64(const Str: String): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(InitialSimpleHash64),
    PChar(Str)^,TMemSize(Length(Str) * SizeOf(Char))));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(InitialSimpleHash64),
    PChar(Str)^,TMemSize(Length(Str) * SizeOf(Char))));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function StreamSimpleHash64(Stream: TStream; Count: Int64 = -1): TSimpleHash64;
var
  Hasher: TSimpleHash64Init;
begin
Hasher := TSimpleHash64Init.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.SimpleHash64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSimpleHash64(const FileName: String): TSimpleHash64;
var
  Hasher: TSimpleHash64Init;
begin
Hasher := TSimpleHash64Init.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.SimpleHash64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash context functions
-------------------------------------------------------------------------------}

Function SimpleHash64_Init: TSimpleHash64Context;
begin
Result := TSimpleHash64Context(TSimpleHash64Base.SimpleHash64ToSys(InitialSimpleHash64));
end;

//------------------------------------------------------------------------------

procedure SimpleHash64_Update(var Context: TSimpleHash64Context; const Buffer; Size: TMemSize);
begin
{$IFDEF PurePascal}
TSimpleHash64Sys(Context) := SimpleHash64_PAS(TSimpleHash64Sys(Context),Buffer,Size);
{$ELSE}
TSimpleHash64Sys(Context) := SimpleHash64_ASM(TSimpleHash64Sys(Context),Buffer,Size);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function SimpleHash64_Final(var Context: TSimpleHash64Context; const Buffer; Size: TMemSize): TSimpleHash64;
begin
SimpleHash64_Update(Context,Buffer,Size);
Result := SimpleHash64_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SimpleHash64_Final(var Context: TSimpleHash64Context): TSimpleHash64;
begin
Result := TSimpleHash64Base.SimpleHash64FromSys(TSimpleHash64Sys(Context));
TSimpleHash64Sys(Context) := TSimpleHash64Base.SimpleHash64ToSys(ZeroSimpleHash64);
end;

//------------------------------------------------------------------------------

Function SimpleHash64_Hash(const Buffer; Size: TMemSize): TSimpleHash64;
begin
with TSimpleHash64Base do
{$IFDEF PurePascal}
  Result := SimpleHash64FromSys(SimpleHash64_PAS(SimpleHash64ToSys(InitialSimpleHash64),Buffer,Size));
{$ELSE}
  Result := SimpleHash64FromSys(SimpleHash64_ASM(SimpleHash64ToSys(InitialSimpleHash64),Buffer,Size));
{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                         Unit implementation management                         
--------------------------------------------------------------------------------
===============================================================================}

procedure UnitInitialiaze;
begin
ImplManager := TImplementationManager.Create;
// TSimpleHash32Base...
AddRoutingSelect(ImplManager,TUIMIdentifier(0),ProcessBufferDummy,[
  ImplInfo(TUImIdentifier(hiPascal),@SimpleHash32_PAS),
{$IFNDEF PurePascal}
  ImplInfo(TUImIdentifier(hiAssembly),@SimpleHash32_ASM)],TUImIdentifier(hiAssembly));
{$ELSE}
  ImplInfo(TUImIdentifier(hiAssembly),@SimpleHash32_PAS,False,False)],TUImIdentifier(hiPascal));
{$ENDIF}
// TSimpleHash64Base...
AddRoutingSelect(ImplManager,TUIMIdentifier(1),ProcessBufferDummy,[
  ImplInfo(TUImIdentifier(hiPascal),@SimpleHash64_PAS),
{$IFNDEF PurePascal}
  ImplInfo(TUImIdentifier(hiAssembly),@SimpleHash64_ASM)],TUImIdentifier(hiAssembly));
{$ELSE}
  ImplInfo(TUImIdentifier(hiAssembly),@SimpleHash64_PAS,False,False)],TUImIdentifier(hiPascal));
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
FreeAndNil(ImplManager);
end;

//==============================================================================

initialization
  UnitInitialiaze;

finalization
  UnitFinalize;

end.
