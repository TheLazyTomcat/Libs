{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CipherBase

    Set of base classes for ciphers (encryption/decryption).

    At this moment, only base class for symmetric block cipher is implemented
    (used for Rijndael/AES), more will probably be implemented later.

  Version 1.0.5 (2022-10-25)

  Last change 2022-10-25

  ©2021-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CipherBase

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream

===============================================================================}
unit CipherBase;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Libray-specific exceptions
===============================================================================}
type
  ECipherException = class(Exception);

  ECipherInvalidState = class(ECipherException);
  ECipherInvalidValue = class(ECipherException);
  ECipherNoStream     = class(ECipherException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TCipherBase
--------------------------------------------------------------------------------
===============================================================================}
type
  TCipherMode = (cmUndefined,cmEncrypt,cmDecrypt);

  TCipherImplementation = (ciPascal,ciAssembly,ciAccelerated);

  TCipherImplementations = set of TCipherImplementation;

{===============================================================================
    TCipherBase - class declaration
===============================================================================}
type
  TCipherBase = class(TCustomObject)
  protected
    fMode:                TCipherMode;  // encrypt/decrypt
    fStreamBufferSize:    TMemSize;
    fBufferProgress:      Boolean;
    fProcessedBytes:      TMemSize;
    fBreakProcessing:     Boolean;
    fInitialized:         Boolean;
    fFinalized:           Boolean;
    fOnProgressEvent:     TFloatEvent;
    fOnProgressCallback:  TFloatCallback;
    // getters, setters
    Function GetCipherImplementation: TCipherImplementation; virtual;
    procedure SetCipherImplementation(Value: TCipherImplementation); virtual;
    Function GetMode: TCipherMode; virtual;
    procedure SetMode(Value: TCipherMode); virtual;
    procedure SetStreamBufferSize(Value: TMemSize); virtual;
    // progress reporting
    procedure DoProgress(Progress: Double); virtual;
    // main processing
    Function UpdateProcessing(const InBuff; InSize: TMemSize; out OutBuff): TMemSize; virtual; abstract;
    procedure FinalProcessing(const InBuff; InSize: TMemSize; out OutBuff); virtual; abstract;
    // instance initialization/finalization
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // cipher management methods
  {
    CiperInit should prepare the cipher for actual processing (eg. construct
    the key schedule) based on actual cipher setup.

    Called from method Init.
  }
    procedure CipherInit; virtual; abstract;
  {
    CipherFinal is there to do a potential cleanup after the processing is
    completed.

    Called from method Final.
  }
    procedure CipherFinal; virtual; abstract;
    // utility functions
  {
    IsRunning returns true when initialized and not finalized. It is used to
    check whether some fields can be changed, as changing some options during
    processing can lead to data corruption.
  }
    Function IsRunning: Boolean; virtual;
  {
    RectifyBufferSize normally returns the passed value.
    It is here for optimization sake - for example in block ciphers it is better
    when the buffer is a multiple of block size.
  }
    Function RectifyBufferSize(Value: TMemSize): TMemSize; virtual;
  public
    class Function CipherImplementationsAvailable: TCipherImplementations; virtual;
    class Function CipherImplementationsSupported: TCipherImplementations; virtual;
    class Function CipherName: String; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  {
    Init must be called before processing using Update and Final methods.
    Note that when a cipher is already initialized, calling Init again will
    completely re-initialize the cipher.

    Sets Initialized to true and Finalized to false.
  }
    procedure Init; virtual;
  {
    Update must be able to accept input buffer of any size and produce at most
    the same amount of output bytes - meaning OutBuff must be (at least) the
    same size as InBuff.

    It is allowable to pass the same buffer for input and output - the
    implementations must be written to account for this.

    Result is the amount of bytes written to OutBuff - can be zero, but cannot
    be larger than InSize.

    Property Initialized must be set to true and Finalized to false, otherwise
    the Update will raise an ECipherInvalidState exception.
  }
    Function Update(const InBuff; InSize: TMemSize; out OutBuff): TMemSize; virtual;
  {
    Final accepts buffer of any size, produces final processed data and
    finalizes the cipher.

    Use function FinalOutputSize just before calling Final to get how many bytes
    will be produced by this method (minimal required size of OutBuff).

    Sets Finalized to true.

    Property Initialized must be true and Finalized false, otherwise final
    raises an ECipherInvalidState exception.
  }
    procedure Final(const InBuff; InSize: TMemSize; out OutBuff); overload; virtual;
  {
    Following overload of Final only calls the first overload with nil InBuff
    and InSize of zero.
  }
    procedure Final(out OutBuff); overload; virtual;
  {
    FinalOutputSize calculates the required size of output buffer for a call
    to Final.
    You must call this method right before a call to Final, otherwise the
    returned value might be wrong.

    NOTE - default implementation returns InSize, if the result differs from
           InSize, do not call inherited code.
  }
    Function FinalOutputSize(InSize: TMemSize): TMemSize; virtual;
  {
    OutputSize returns the total size of output after processing all data,
    calculated from a given pre-processing size.

    WARNING - the output size might depend on current cipher setup, so be sure
              to first properly setup the cipher and then obtain this value.

    NOTE - default implementation returns InputSize, so if the result differs
           from input size, then do not call inherited.
  }
    Function OutputSize(InputSize: TMemSize): TMemSize; virtual;
    // macro methods
  {
    To get required size for OutBuff, use method OutputSize.

    Note that for single-buffer overload, the buffer must be large enough to
    fit the output, but the passed size is the size of INPUT data (which might
    be smaller).

    The same goes for ProcessMemory.
  }
    procedure ProcessBuffer(const InBuff; InSize: TMemSize; out OutBuff); overload; virtual;
    procedure ProcessBuffer(var Buffer; Size: TMemSize); overload; virtual;
    procedure ProcessMemory(InMem: Pointer; InSize: TMemSize; OutMem: Pointer); overload; virtual;
    procedure ProcessMemory(Memory: Pointer; Size: TMemSize); overload; virtual;
  {
    WARNING - paramter Count is meant for input, but the cipher can actually
              write beyond this limit!
  }    
    procedure ProcessStream(InStream, OutStream: TStream; Count: Int64 = -1); overload; virtual;
    procedure ProcessStream(Stream: TStream; Count: Int64 = -1); overload; virtual;
    procedure ProcessFile(const InFileName,OutFileName: String); overload; virtual;
    procedure ProcessFile(const FileName: String); overload; virtual;
  {
    ProcessFileMem writes the entire output into a memory stream and then
    rewrites the original file with content of this memory stream - note that
    it can and will also change the size of the file.

    ProcessFileTemp writes output into a temporary file located in the same
    directory as input file, then deletes the original file and renames the
    temp file to the same name the original had, effectively replacing it.  
  }
    procedure ProcessFileMem(const FileName: String); virtual;
    procedure ProcessFileTemp(const FileName: String); virtual;
  {
    For strings, the output is (re)allocated automatically.  
  }
    procedure ProcessString(const InStr: String; out OutStr: String); overload; virtual;
    procedure ProcessString(var Str: String); overload; virtual;
    procedure ProcessAnsiString(const InStr: AnsiString; out OutStr: AnsiString); overload; virtual;
    procedure ProcessAnsiString(var Str: AnsiString); overload; virtual;
    procedure ProcessWideString(const InStr: WideString; out OutStr: WideString); overload; virtual;
    procedure ProcessWideString(var Str: WideString); overload; virtual;
    // properties
    property Mode: TCipherMode read GetMode write SetMode;    
  {
    If cipher is implemented both in assembly and pascal, this property can be
    used to discern which implementation is currently used, and also to set
    which implementation is to be used next.

    Note that when the unit is compiled in PurePascal mode, asm implementation
    cannot be used and pascal implementation is always used instead,
    irrespective of how you set this property.
  }
    property CipherImplementation: TCipherImplementation read GetCipherImplementation write SetCipherImplementation;
  {
    StreamBufferSize is used when allocating read/write buffer for processings
    of streams.
  }
    property StreamBufferSize: TMemSize read fStreamBufferSize write SetStreamBufferSize;
  {
    When BufferProgress is set to false (default), the progress is reported only
    when processing stream or file. When set to true, the progress is reported
    from all macro methods.
    But note that progress is calculated and reported only on the boundary of
    read/write buffer, of which size is set in StreamBufferSize property. This
    means that, when processing data smaller than this buffer, no actual
    progress is reported, only 0% (0.0) and 100% (1.0).
  }
    property BufferProgress: Boolean read fBufferProgress write fBufferProgress;
  {
    ProcessedBytes is number of input bytes processed since a last call to
    method Init.  
  }
    property ProcessedBytes: TMemSize read fProcessedBytes write fProcessedBytes;
  {
    BreakProcessing, when set to true inside of progress event or callback,
    will cause premature termination of processing right after return from the
    call.
  }
    property BreakProcessing: Boolean read fBreakProcessing write fBreakProcessing;    
    property Initialized: Boolean read fInitialized;
    property Finalized: Boolean read fFinalized;
  {
    Progress is reported only from macro methods (ProcessStream, ProcessFile,
    ...).

    Progress value is normalized, meaning it is reported in the range <0,1>.

    If both event and callback are assigned, then only the event is called.
  }
    property OnProgress: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressEvent: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressCallback: TFloatCallback read fOnProgressCallback write fOnProgressCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TBlockCipher                                                                     
--------------------------------------------------------------------------------
===============================================================================}
type
  TBlockCipherModeOfOperation = (moECB,moCBC,moPCBC,moCFB,moOFB,moCTR);

  TBlockCipherPadding = (padZeroes,padPKCS7,padANSIX923,padISO10126,padISOIEC7816_4);

  TBlockCipherUpdateProc = procedure(const Input; out Output) of object;

{===============================================================================
    TBlockCipher - class declaration
===============================================================================}
type
  TBlockCipher = class(TCipherBase)
  protected
    fModeOfOperation:   TBlockCipherModeOfOperation;
    fPadding:           TBlockCipherPadding;
    fBlockBytes:        TMemSize;
    fInitVector:        Pointer;
    fKey:               Pointer;
    fKeyBytes:          TMemSize;
    // internals...
    fInTransferData:    Pointer;    // input data not consumed in previous processing
    fInTransferSize:    TMemSize;   // allocated size input transfer buffer
    fInTransferCount:   TMemSize;   // number of input bytes not consumed in previous processing
    fOutTransferData:   Pointer;    // output data not consumed in previous processing
    fOutTransferSize:   TMemSize;   // allocated size output transfer buffer
    fOutTransferCount:  TMemSize;   // number of output bytes not consumed in previous processing
    fTempBlock:         Pointer;    // used only in BlockUpdate_* methods, do not use anywhere else!
    fOutBuffer:         Pointer;
    fOutBufferSize:     TMemSize;
    fBlockUpdateProc:   TBlockCipherUpdateProc;
    // getters, setters
    procedure SetModeOfOperation(Value: TBlockCipherModeOfOperation); virtual;
    procedure SetBlockBytes(Value: TMemSize); virtual;
    Function GetBlockBits: Integer; virtual;
    procedure SetBlockBits(Value: Integer); virtual;
    procedure SetInitVector(Value: Pointer); virtual;
    procedure SetKey(Value: Pointer); virtual;
    procedure SetKeyBytes(Value: TMemSize); virtual;
    Function GetKeyBits: Integer; virtual;
    procedure SetKeyBits(Value: Integer); virtual;
    // block utility functions
    procedure BlockXOR(const Src1,Src2; out Dest); virtual;
    procedure BlockCopy(const Src; out Dest); virtual;
    procedure BlockPad(var Block; UsedBytes: TMemSize); virtual;
    // block update methods for different modes of operation
    procedure BlockUpdate_ECB(const Input; out Output); virtual;
    procedure BlockUpdate_CBC(const Input; out Output); virtual;
    procedure BlockUpdate_PCBC(const Input; out Output); virtual;
    procedure BlockUpdate_CFB(const Input; out Output); virtual;
    procedure BlockUpdate_OFB(const Input; out Output); virtual;
    procedure BlockUpdate_CTR(const Input; out Output); virtual;
    // processing of individual blocks
    procedure BlockEncrypt(const Input; out Output); virtual; abstract;
    procedure BlockDecrypt(const Input; out Output); virtual; abstract;
    // processing helpers
    Function PutToInTransfer(Buff: Pointer; Size: TMemSize): TMemSize; virtual;
    Function TakeFromOutTransfer(Buff: Pointer; Size: TMemSize; ShiftDown: Boolean = True): TMemSize; virtual;
    // main processing
    Function UpdateProcessingFast(const InBuff; InSize: TMemSize; out OutBuff): TMemSize; virtual;
    Function UpdateProcessingSlow(const InBuff; InSize: TMemSize; out OutBuff): TMemSize; virtual;
    Function UpdateProcessing(const InBuff; InSize: TMemSize; out OutBuff): TMemSize; override;
    procedure FinalProcessing(const InBuff; InSize: TMemSize; out OutBuff); override;
    // internal cipher setup
    procedure CipherSetup(Mode: TCipherMode; ModeOfOperation: TBlockCipherModeOfOperation; Key, InitVector: Pointer; KeyBytes, BlockBytes: TMemSize); virtual;
    // initialization/finalization
    procedure Initialize; override;
    procedure Finalize; override;
    // utility functions
    Function RectifyBufferSize(Value: TMemSize): TMemSize; override;
  public
    constructor CreateForEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    constructor CreateForEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    constructor CreateForEncryption(const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    constructor CreateForDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload; virtual;
    constructor CreateForDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload; virtual;
    constructor CreateForDecryption(const Key; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload; virtual;
    procedure SetupEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure SetupEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure SetupEncryption(const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure SetupDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure SetupDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure SetupDecryption(const Key; KeyBytes, BlockBytes: TMemSize); overload; virtual;
    procedure Init; override;
    Function FinalOutputSize(InSize: TMemSize): TMemSize; override;
    Function OutputSize(InputSize: TMemSize): TMemSize; override;
    property ModeOfOperation: TBlockCipherModeOfOperation read fModeOfOperation write SetModeOfOperation;
    property Padding: TBlockCipherPadding read fPadding write fPadding;
    property BlockBytes: TMemSize read fBlockBytes write SetBlockBytes;
    property BlockBits: Integer read GetBlockBits write SetBlockBits;
    property InitVector: Pointer read fInitVector write SetInitVector;
    property InitVectorBytes: TMemSize read fBlockBytes;
    property InitVectorBits: Integer read GetBlockBits;
    property Key: Pointer read fKey write SetKey;
    property KeyBytes: TMemSize read fKeyBytes write SetKeyBytes;
    property KeyBits: Integer read GetKeyBits write SetKeyBits;
  end;

implementation

uses
{$IF not Defined(FPC) and Defined(Windows)}Windows,{$IFEND}
  StrRect, StaticMemoryStream;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TCipherBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCipherBase - utility functions
===============================================================================}

// so there is no need to link Math unit
Function Min(A,B: Int64): Int64;
begin
If A < B then
  Result := A
else
  Result := B;
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function OffsetPtr(Ptr: Pointer; Offset: PtrInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(Ptr) + PtrUInt(Offset));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure ShiftBufferDown(Ptr: Pointer; Offset,BufferSize: TMemSize);
begin
If (Offset <> 0) and (Offset < BufferSize) then
  Move(OffsetPtr(Ptr,Offset)^,Ptr^,BufferSize - Offset);
end;

//------------------------------------------------------------------------------

Function CntrCorrectEndian(Value: UInt16): UInt16; overload;
begin
{$IFDEF ENDIAN_BIG}
Result := Value;
{$ELSE}
Result := ((Value and $FF00) shr 8) or ((Value and $00FF) shl 8);
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CntrCorrectEndian(Value: UInt32): UInt32; overload;
begin
{$IFDEF ENDIAN_BIG}
Result := Value;
{$ELSE}
Result := ((Value and $FF000000) shr 24) or ((Value and $00FF0000) shr 8) or
          ((Value and $0000FF00) shl 8) or ((Value and $000000FF) shl 24);
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CntrCorrectEndian(Value: UInt64): UInt64; overload;
begin
{$IFDEF ENDIAN_BIG}
Result := Value;
{$ELSE}
Int64Rec(Result).Lo := CntrCorrectEndian(Int64Rec(Value).Hi);
Int64Rec(Result).Hi := CntrCorrectEndian(Int64Rec(Value).Lo);
{$ENDIF}
end;

{===============================================================================
    TCipherBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCipherBase - protected methods
-------------------------------------------------------------------------------}

Function TCipherBase.GetCipherImplementation: TCipherImplementation;
begin
Result := ciPascal;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TCipherBase.SetCipherImplementation(Value: TCipherImplementation);
begin
// do nothing;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TCipherBase.GetMode: TCipherMode;
begin
Result := fMode;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.SetMode(Value: TCipherMode);
begin
If not IsRunning then
  begin
    If Value in [cmEncrypt,cmDecrypt] then
      fMode := Value
    else
      raise ECipherInvalidValue.CreateFmt('TCipherBase.SetMode: Invalid mode (%d).',[Ord(Value)]);
  end
else raise ECipherInvalidState.Create('TCipherBase.SetMode: Cannot change mode on running cipher.');
end;

//------------------------------------------------------------------------------

procedure TCipherBase.SetStreamBufferSize(Value: TMemSize);
begin
If Value > 0 then
  fStreamBufferSize := Value
else
  raise ECipherInvalidValue.CreateFmt('TCipherBase.SetStreamBufferSize: Invalid size (%d).',[Ord(Value)]);
end;

//------------------------------------------------------------------------------

procedure TCipherBase.DoProgress(Progress: Double);
begin
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure TCipherBase.Initialize;
begin
fMode := cmUndefined;
fStreamBufferSize := 1024 * 1024; // 1MiB
fBufferProgress := False;
fProcessedBytes := 0;
fBreakProcessing := False;
fInitialized := False;
fFinalized := False;
fOnProgressEvent := nil;
fOnProgressCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.Finalize;
begin
// nothing to do
end;

//------------------------------------------------------------------------------

Function TCipherBase.IsRunning: Boolean;
begin
Result := fInitialized and not fFinalized;
end;

//------------------------------------------------------------------------------

Function TCipherBase.RectifyBufferSize(Value: TMemSize): TMemSize;
begin
Result := Value;
end;

{-------------------------------------------------------------------------------
    TCipherBase - public methods
-------------------------------------------------------------------------------}

class Function TCipherBase.CipherImplementationsAvailable: TCipherImplementations;
begin
Result := [ciPascal];
end;

//------------------------------------------------------------------------------

class Function TCipherBase.CipherImplementationsSupported: TCipherImplementations;
begin
Result := [ciPascal];
end;

//------------------------------------------------------------------------------

constructor TCipherBase.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TCipherBase.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.Init;
begin
If fMode in [cmEncrypt,cmDecrypt] then
  begin
    fProcessedBytes := 0;
    fInitialized := True;
    fFinalized := False;
    CipherInit;
  end
else raise ECipherInvalidState.CreateFmt('TCipherBase.Init: Invalid cipher mode (%d).',[Ord(fMode)]);
end;

//------------------------------------------------------------------------------

Function TCipherBase.Update(const InBuff; InSize: TMemSize; out OutBuff): TMemSize;
begin
If fInitialized then
  begin
    If not fFinalized then
      begin
        Result := UpdateProcessing(InBuff,InSize,OutBuff);
        Inc(fProcessedBytes,InSize);
      end
    else raise ECipherInvalidState.Create('TCipherBase.Update: Cipher already finalized.');
  end
else raise ECipherInvalidState.Create('TCipherBase.Update: Cipher not initialized.');
end;

//------------------------------------------------------------------------------

procedure TCipherBase.Final(const InBuff; InSize: TMemSize; out OutBuff);
begin
If fInitialized then
  begin
    If not fFinalized then
      begin
        FinalProcessing(InBuff,InSize,OutBuff);
        Inc(fProcessedBytes,InSize);
        fFinalized := True;
        CipherFinal;
      end
    else raise ECipherInvalidState.Create('TCipherBase.Final: Cipher already finalized.');
  end
else raise ECipherInvalidState.Create('TCipherBase.Final: Cipher not initialized.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.Final(out OutBuff);
begin
Final(nil^,0,OutBuff);
end;

//------------------------------------------------------------------------------

Function TCipherBase.FinalOutputSize(InSize: TMemSize): TMemSize;
begin
Result := InSize;
end;

//------------------------------------------------------------------------------

Function TCipherBase.OutputSize(InputSize: TMemSize): TMemSize;
begin
Result := InputSize;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessBuffer(const InBuff; InSize: TMemSize; out OutBuff);
var
  InStream:   TStaticMemoryStream;
  OutStream:  TWritableStaticMemoryStream;
begin
If fBufferProgress then
  begin
    InStream := TStaticMemoryStream.Create(@InBuff,InSize);
    try
      OutStream := TWritableStaticMemoryStream.Create(@OutBuff,OutputSize(InSize));
      try
        ProcessStream(InStream,OutStream);
      finally
        OutStream.Free;
      end;
    finally
      InStream.Free;
    end;
  end
else
  begin
    Init;
    Final(InBuff,InSize,OutBuff);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessBuffer(var Buffer; Size: TMemSize);
var
  Stream: TWritableStaticMemoryStream;
begin
If fBufferProgress then
  begin
    Stream := TWritableStaticMemoryStream.Create(@Buffer,OutputSize(Size));
    try
      Stream.Seek(0,soBeginning);
      ProcessStream(Stream,Size);
    finally
      Stream.Free;
    end;
  end
else
  begin
    Init;
    Final(Buffer,Size,Buffer);
  end;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessMemory(InMem: Pointer; InSize: TMemSize; OutMem: Pointer);
begin
ProcessBuffer(InMem^,InSize,OutMem^);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessMemory(Memory: Pointer; Size: TMemSize);
begin
ProcessBuffer(Memory^,Size);
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessStream(InStream, OutStream: TStream; Count: Int64 = -1);
var
  Buffer:       Pointer;
  BuffSize:     TMemSize;
  BytesRead:    LongInt;
  BytesOutput:  TMemSize;
  InitCount:    Int64;
begin
If InStream = OutStream then
  ProcessStream(InStream)
else
  begin
    If not Assigned(InStream) then
      raise ECipherNoStream.Create('TCipherBase.ProcessStream: Input stream not assigned.');
    If not Assigned(OutStream) then
      raise ECipherNoStream.Create('TCipherBase.ProcessStream: Output stream not assigned.');
    If Count = 0 then
      Count := InStream.Size - InStream.Position;
    If Count < 0 then
      begin
        InStream.Seek(0,soBeginning);
        Count := InStream.Size;
      end;
    InitCount := Count;
    BuffSize := RectifyBufferSize(fStreamBufferSize);
    fBreakProcessing := False;
    DoProgress(0.0);
    If not fBreakProcessing then
      begin
        GetMem(Buffer,BuffSize);
        try
          Init;
          BytesRead := 0;
          If InitCount > 0 then
            repeat
              BytesRead := InStream.Read(Buffer^,Min(BuffSize,Count));
              // process only whole buffers
              If TMemSize(BytesRead) >= BuffSize then
                begin
                  BytesOutput := Update(Buffer^,BytesRead,Buffer^);
                {
                  Note that WriteBuffer can fail on static streams, but that is
                  a desired behaviour, as static stream are not supposed to be
                  used as output.
                }
                  OutStream.WriteBuffer(Buffer^,BytesOutput);
                  Dec(Count,BytesRead);
                  DoProgress((InitCount - Count) / InitCount);
                end;
            until (TMemSize(BytesRead) < BuffSize) or fBreakProcessing;
        {
          By now, the buffer is either empty, or contains less data than
          BuffSize. Realloc the memory so it can fit data from Final (original
          data are preserved).
        }
          If not fBreakProcessing then
            begin
              BuffSize := FinalOutputSize(BytesRead);
              ReallocMem(Buffer,BuffSize);
              // do final processing
              Final(Buffer^,BytesRead,Buffer^);
              OutStream.WriteBuffer(Buffer^,BuffSize);
              DoProgress(1.0);
            end;
        finally
          FreeMem(Buffer,BuffSize);
        end;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessStream(Stream: TStream; Count: Int64 = -1);
var
  Buffer:       Pointer;
  BuffSize:     TMemSize;
  BytesRead:    LongInt;
  BytesOutput:  TMemSize;
  InitCount:    Int64;
  WritePos:     Int64;
  ReadPos:      Int64;
begin
If Assigned(Stream) then
  begin
    If Count = 0 then
      Count := Stream.Size - Stream.Position;
    If Count < 0 then
      begin
        Stream.Seek(0,soBeginning);
        Count := Stream.Size;
      end;
    InitCount := Count;
    BuffSize := RectifyBufferSize(fStreamBufferSize);
    fBreakProcessing := False;
    DoProgress(0.0);
    If not fBreakProcessing then
      begin
        GetMem(Buffer,BuffSize);
        try
          Init;
          WritePos := Stream.Position;
          ReadPos := Stream.Position;
          BytesRead := 0;
          If InitCount > 0 then
            repeat
              Stream.Seek(ReadPos,soBeginning);
              BytesRead := Stream.Read(Buffer^,Min(BuffSize,Count));
              ReadPos := ReadPos + Int64(BytesRead);
              If TMemSize(BytesRead) >= BuffSize then
                begin
                  BytesOutput := Update(Buffer^,BytesRead,Buffer^);
                  If BytesOutput > 0 then
                    begin
                      Stream.Seek(WritePos,soBeginning);
                      Stream.WriteBuffer(Buffer^,BytesOutput);
                      WritePos := WritePos + Int64(BytesOutput);
                    end;
                  Dec(Count,BytesRead);
                  DoProgress((InitCount - Count) / InitCount);
                end;
            until (TMemSize(BytesRead) < BuffSize) or fBreakProcessing;
          If not fBreakProcessing then
            begin
              BuffSize := FinalOutputSize(BytesRead);
              ReallocMem(Buffer,BuffSize);
              Final(Buffer^,BytesRead,Buffer^);
              Stream.Seek(WritePos,soBeginning);
              Stream.WriteBuffer(Buffer^,BuffSize);
              DoProgress(1.0);
            end;
        finally
          FreeMem(Buffer,BuffSize);
        end; 
      end;
  end
else raise ECipherNoStream.Create('TCipherBase.ProcessStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessFile(const InFileName,OutFileName: String);
var
  InFileStream:   TFileStream;
  OutFileStream:  TFileStream;
begin
InFileStream := TFileStream.Create(StrToRTL(InFileName),fmOpenRead or fmShareDenyWrite);
try
  OutFileStream := TFileStream.Create(StrToRTL(OutFileName),fmCreate or fmShareDenyWrite);
  try
    ProcessStream(InFileStream,OutFileStream);
  finally
    OutFileStream.Free;
  end;
finally
  InFileStream.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenReadWrite or fmShareDenyWrite);
try
  ProcessStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessFileMem(const FileName: String);
var
  InFileStream: TFileStream;
  OutputStream: TMemoryStream;
begin
InFileStream := TFileStream.Create(StrToRTL(FileName),fmOpenReadWrite or fmShareDenyWrite);
try
  OutputStream := TMemoryStream.Create;
  try
    // preallocate the memory
    OutputStream.Size := Int64(OutputSize(InFileStream.Size));
    OutputStream.Seek(0,soBeginning);
    // processing...
    ProcessStream(InFileStream,OutputStream);
    // save result
    InFileStream.Seek(0,soBeginning);
    InFileStream.CopyFrom(OutputStream,0);
    If InFileStream.Size <> OutputStream.Size then
      InFileStream.Size := OutputStream.Size;
  finally
    OutputStream.Free;
  end;
finally
  InFileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessFileTemp(const FileName: String);
var
  TempFileName: String;
begin
TempFileName := FileName;
repeat
  TempFileName := TempFileName + '.tmp';
until not FileExists(StrToRTL(TempFileName));
ProcessFile(FileName,TempFileName);
If SysUtils.DeleteFile(FileName) then
  SysUtils.RenameFile(TempFileName,FileName);
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessString(const InStr: String; out OutStr: String);
var
  InLen:  TStrSize;
begin
InLen := Length(InStr);
SetLength(OutStr,OutputSize(InLen * SizeOf(Char)) div SizeOf(Char));
ProcessMemory(PChar(InStr),InLen * SizeOf(Char),PChar(OutStr));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessString(var Str: String);
var
  InLen:  TStrSize;
begin
InLen := Length(Str);
SetLength(Str,OutputSize(InLen * SizeOf(Char)) div SizeOf(Char));
ProcessMemory(PChar(Str),InLen * SizeOf(Char));
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessAnsiString(const InStr: AnsiString; out OutStr: AnsiString);
var
  InLen:  TStrSize;
begin
InLen := Length(InStr);
SetLength(OutStr,OutputSize(InLen * SizeOf(AnsiChar)) div SizeOf(AnsiChar));
ProcessMemory(PAnsiChar(InStr),InLen * SizeOf(AnsiChar),PAnsiChar(OutStr));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessAnsiString(var Str: AnsiString);
var
  InLen:  TStrSize;
begin
InLen := Length(Str);
SetLength(Str,OutputSize(InLen * SizeOf(AnsiChar)) div SizeOf(AnsiChar));
ProcessMemory(PAnsiChar(Str),InLen * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

procedure TCipherBase.ProcessWideString(const InStr: WideString; out OutStr: WideString);
var
  InLen:  TStrSize;
begin
InLen := Length(InStr);
SetLength(OutStr,OutputSize(InLen * SizeOf(WideChar)) div SizeOf(WideChar));
ProcessMemory(PWideChar(InStr),InLen * SizeOf(WideChar),PWideChar(OutStr));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCipherBase.ProcessWideString(var Str: WideString);
var
  InLen:  TStrSize;
begin
InLen := Length(Str);
SetLength(Str,OutputSize(InLen * SizeOf(WideChar)) div SizeOf(WideChar));
ProcessMemory(PWideChar(Str),InLen * SizeOf(WideChar));
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TBlockCipher                                                                     
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBlockCipher - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBlockCipher - protected methods
-------------------------------------------------------------------------------}

procedure TBlockCipher.SetModeOfOperation(Value: TBlockCipherModeOfOperation);
begin
If not IsRunning then
  begin
    If Value <> fModeOfOperation then
      begin
        fModeOfOperation := Value;
        case Value of
          moECB:  fBlockUpdateProc := BlockUpdate_ECB;
          moCBC:  fBlockUpdateProc := BlockUpdate_CBC;
          moPCBC: fBlockUpdateProc := BlockUpdate_PCBC;
          moCFB:  fBlockUpdateProc := BlockUpdate_CFB;
          moOFB:  fBlockUpdateProc := BlockUpdate_OFB;
          moCTR:  fBlockUpdateProc := BlockUpdate_CTR;
        else
          raise ECipherInvalidValue.CreateFmt('TBlockCipher.SetModeOfOperation: Unknown mode of operation (%d).',[Ord(Value)]);
        end;
      end;
  end
else raise ECipherInvalidState.Create('TBlockCipher.SetModeOfOperation: Cannot change mode of operation on running cipher.');
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetBlockBytes(Value: TMemSize);
begin
If not IsRunning then
  begin
    If Value > 0 then
      begin
        If Value <> fBlockBytes then
          begin
            fBlockBytes := Value;
            ReallocMem(fInitVector,fBlockBytes);
            ReallocMem(fTempBlock,fBlockBytes);
          end;
      end
    else raise ECipherInvalidValue.CreateFmt('TBlockCipher.SetBlockBytes: Invalid block size (%d).',[Value]);
  end
else raise ECipherInvalidState.Create('TBlockCipher.SetBlockBytes: Cannot change block size on running cipher.');
end;

//------------------------------------------------------------------------------

Function TBlockCipher.GetBlockBits: Integer;
begin
Result := fBlockBytes * 8;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetBlockBits(Value: Integer);
begin
If Value and 7 = 0 then
  SetBlockBytes(Value div 8)
else
  raise ECipherInvalidValue.CreateFmt('TBlockCipher.SetBlockBits: Invalid block bits (%d).',[Value]);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetInitVector(Value: Pointer);
begin
If not IsRunning then
  Move(Value^,fInitVector^,fBlockBytes)
else
  raise ECipherInvalidState.Create('TBlockCipher.SetInitVector: Cannot change init vector on running cipher.');
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetKey(Value: Pointer);
begin
If not IsRunning then
  Move(Value^,fKey^,fKeyBytes)
else
  raise ECipherInvalidState.Create('TBlockCipher.SetKey: Cannot change key on running cipher.');
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetKeyBytes(Value: TMemSize);
begin
If not IsRunning then
  begin
    If Value > 0 then
      begin
        If Value <> fKeyBytes then
          begin
            fKeyBytes := Value;
            ReallocMem(fKey,fKeyBytes);
          end;
      end
    else raise ECipherInvalidValue.CreateFmt('TBlockCipher.SetBlockBytes: Invalid key size (%d).',[Value]);
  end
else raise ECipherInvalidState.Create('TBlockCipher.SetKeyBytes: Cannot change key size on running cipher.');
end;

//------------------------------------------------------------------------------

Function TBlockCipher.GetKeyBits: Integer;
begin
Result := fKeyBytes * 8;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetKeyBits(Value: Integer);
begin
If Value and 7 = 0 then
  SetKeyBytes(Value div 8)
else
  raise ECipherInvalidValue.CreateFmt('TBlockCipher.SetKeyBits: Invalid key bits (%d).',[Value]);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockXOR(const Src1,Src2; out Dest);
var
  i:  TMemSize;
begin
If fBlockBytes > 0 then
  begin
  {$IFDEF CPU64bit}
    If fBlockBytes and 7 = 0 then
      For i := 0 to Pred(fBlockBytes shr 3) do
        PUInt64(OffsetPtr(@Dest,i shl 3))^ := PUInt64(OffsetPtr(@Src1,i shl 3))^ xor PUInt64(OffsetPtr(@Src2,i shl 3))^
    else
  {$ENDIF}
    If fBlockBytes and 3 = 0 then
      For i := 0 to Pred(fBlockBytes shr 2) do
        PUInt32(OffsetPtr(@Dest,i shl 2))^ := PUInt32(OffsetPtr(@Src1,i shl 2))^ xor PUInt32(OffsetPtr(@Src2,i shl 2))^
    else
      For i := 0 to Pred(fBlockBytes) do
        PUInt8(OffsetPtr(@Dest,i))^ := PUInt8(OffsetPtr(@Src1,i))^ xor PUInt8(OffsetPtr(@Src2,i))^
  end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockCopy(const Src; out Dest);
begin
Move(Src,Addr(Dest)^,fBlockBytes);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockPad(var Block; UsedBytes: TMemSize);
var
  PadPtr: Pointer;
  PadLen: TMemSize;
  i:      Integer;
begin
If (UsedBytes > 0) and (UsedBytes < fBlockBytes) then
  begin
    PadPtr := OffsetPtr(Addr(Block),UsedBytes);
    PadLen := fBlockBytes - UsedBytes;
    case fPadding of
      padPKCS7:         // PKCS#7
        FillChar(PadPtr^,PadLen,Byte(PadLen));
      padANSIX923:      // ANSI X.923
        begin
          FillChar(PadPtr^,PadLen,0);
          PByte(OffsetPtr(Addr(Block),fBlockBytes))^ := Byte(PadLen);
        end;
      padISO10126:      // ISO 10126
        begin
          Randomize;
          For i := 0 to Pred(PadLen) do
            PByte(OffsetPtr(PadPtr,i))^ := Byte(Random(256));
        end;
      padISOIEC7816_4:  // ISO/IEC 7816-4
        begin
          FillChar(PadPtr^,PadLen,0);
          PByte(PadPtr)^ := $80;
        end;
    else
     {padZeroes}
      FillChar(PadPtr^,PadLen,0);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockUpdate_ECB(const Input; out Output);
begin
BlockCopy(Input,fTempBlock^);  // in case input and output points to the same location
case fMode of
  cmEncrypt:  BlockEncrypt(fTempBlock^,Output);
  cmDecrypt:  BlockDecrypt(fTempBlock^,Output);
else
  raise Exception.CreateFmt('TBlockCipher.BlockUpdate_ECB: Invalid cipher mode (%d).',[Ord(fMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockUpdate_CBC(const Input; out Output);
begin
case fMode of
  cmEncrypt:
    begin
      BlockXOR(Input,fInitVector^,fTempBlock^);
      BlockEncrypt(fTempBlock^,Output);
      BlockCopy(Output,fInitVector^);
    end;
  cmDecrypt:
    begin
      BlockCopy(Input,fTempBlock^);
      BlockDecrypt(fTempBlock^,Output);
      BlockXOR(Output,fInitVector^,Output);
      BlockCopy(fTempBlock^,fInitVector^);
    end;
else
  raise Exception.CreateFmt('TBlockCipher.BlockUpdate_CBC: Invalid cipher mode (%d).',[Ord(fMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockUpdate_PCBC(const Input; out Output);
begin
case fMode of
  cmEncrypt:
    begin
      BlockXOR(Input,fInitVector^,fTempBlock^);
      BlockCopy(Input,fInitVector^);
      BlockEncrypt(fTempBlock^,Output);
      BlockXOR(Output,fInitVector^,fInitVector^);
    end;
  cmDecrypt:
    begin
      BlockDecrypt(Input,fTempBlock^);
      BlockXOR(fTempBlock^,fInitVector^,fTempBlock^);
      BlockXOR(Input,fTempBlock^,fInitVector^);
      BlockCopy(fTempBlock^,Output);
    end;
else
  raise Exception.CreateFmt('TBlockCipher.BlockUpdate_PCBC: Invalid cipher mode (%d).',[Ord(fMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockUpdate_CFB(const Input; out Output);
begin
case fMode of
  cmEncrypt:
    begin
      BlockEncrypt(fInitVector^,fTempBlock^);
      BlockXOR(fTempBlock^,Input,Output);
      BlockCopy(Output,fInitVector^);
    end;
  cmDecrypt:
    begin
      BlockEncrypt(fInitVector^,fTempBlock^);
      BlockCopy(Input,fInitVector^);
      BlockXOR(fTempBlock^,Input,Output);
    end;
else
  raise Exception.CreateFmt('TBlockCipher.BlockUpdate_CFB: Invalid cipher mode (%d).',[Ord(fMode)]);
end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.BlockUpdate_OFB(const Input; out Output);
begin
BlockEncrypt(fInitVector^,fTempBlock^);
BlockXOR(Input,fTempBlock^,Output);
BlockCopy(fTempBlock^,fInitVector^);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TBlockCipher.BlockUpdate_CTR(const Input; out Output);
var
  i:          Integer;
  CounterPtr: Pointer;
  Remainder:  PtrInt;
begin
{
  WARNING - this algorithm is sensitive for endianness.

  Counters are always stored in big-endian order.
}
BlockEncrypt(fInitVector^,fTempBlock^);
BlockXOR(Input,fTempBlock^,Output);
// counting...
CounterPtr := fInitVector;
Remainder := PtrInt(fBlockBytes);
For i := 0 to Pred(Remainder div 8) do
  begin
    UInt64(CounterPtr^) := CntrCorrectEndian(UInt64(CntrCorrectEndian(UInt64(CounterPtr^)) + 1));
    Inc(PUInt64(CounterPtr));
    Dec(Remainder,8);
  end;
For i := 0 to Pred(Remainder div 4) do
  begin
    UInt32(CounterPtr^) := CntrCorrectEndian(UInt32(CntrCorrectEndian(UInt32(CounterPtr^)) + 1));
    Inc(PUInt32(CounterPtr));
    Dec(Remainder,4);
  end;
For i := 0 to Pred(Remainder div 2) do
  begin
    UInt16(CounterPtr^) := CntrCorrectEndian(UInt16(CntrCorrectEndian(UInt16(CounterPtr^)) + 1));
    Inc(PUInt16(CounterPtr));
    Dec(Remainder,2);
  end;
For i := 0 to Pred(Remainder) do
  begin
    UInt8(CounterPtr^) := UInt8(CounterPtr^) + 1;
    Inc(PUInt8(CounterPtr));
  end;
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

Function TBlockCipher.PutToInTransfer(Buff: Pointer; Size: TMemSize): TMemSize;
var
  TempSize: TMemSize;
begin
If (fInTransferCount + Size) > fInTransferSize then
  begin
    ReallocMem(fInTransferData,fInTransferCount + Size);
    fInTransferSize := fInTransferCount + Size;
  end;
Move(Buff^,OffsetPtr(fInTransferData,fInTransferCount)^,Size);
fInTransferCount := fInTransferCount + Size;
// process whole blocks and copy the result to output transfer
TempSize := (fInTransferCount div fBlockBytes) * fBlockBytes;
If TempSize <> 0 then
  begin
    If (fOutTransferCount + TempSize) > fOutTransferSize then
      begin
        ReallocMem(fOutTransferData,fOutTransferCount + TempSize);
        fOutTransferSize := fOutTransferCount + TempSize;
      end;
    while fInTransferCount >= fBlockBytes do
      begin
        fBlockUpdateProc(fInTransferData^,OffsetPtr(fOutTransferData,fOutTransferCount)^);
        ShiftBufferDown(fInTransferData,fBlockBytes,fInTransferCount);
        fInTransferCount := fInTransferCount - fBlockBytes;
        fOutTransferCount := fOutTransferCount + fBlockBytes;
      end;
  end;
Result := fInTransferCount;
end;

//------------------------------------------------------------------------------

Function TBlockCipher.TakeFromOutTransfer(Buff: Pointer; Size: TMemSize; ShiftDown: Boolean = True): TMemSize;
begin
If Size <= fOutTransferCount then
  begin
    Move(fOutTransferData^,Buff^,Size);
    If ShiftDown then
      begin
        ShiftBufferDown(fOutTransferData,Size,fOutTransferCount);
        fOutTransferCount := fOutTransferCount - Size;
      end;
    Result := fOutTransferCount;
  end
else raise ECipherInvalidValue.CreateFmt('TBlockCipher.TakeFromOutTransfer: Invalid size (%d)',[Size]);
end;

//------------------------------------------------------------------------------

Function TBlockCipher.UpdateProcessingFast(const InBuff; InSize: TMemSize; out OutBuff): TMemSize;
var
  InPtr:  Pointer;
  OutPtr: Pointer;
begin
{
  InSize must be non-zero, both input and output transfers must be empty and
  are therefore ignored.

  Processing is done directly on parameter buffers - there should be no risk of
  input data overwrite as update methods are written to cope with input and
  output pointing to the same buffer.
}
Result := 0;
InPtr := Addr(InBuff);
OutPtr := Addr(OutBuff);
// process whole blocks
while InSize >= fBlockBytes do
  begin
    fBlockUpdateProc(InPtr^,OutPtr^);
    Result := Result + fBlockBytes;  
    InPtr := OffsetPtr(InPtr,fBlockBytes);
    OutPtr := OffsetPtr(OutPtr,fBlockBytes);
    InSize := InSize - fBlockBytes;
  end;
// store remaining data, if any
If InSize > 0 then
  PutToInTransfer(InPtr,InSize);
end;

//------------------------------------------------------------------------------

Function TBlockCipher.UpdateProcessingSlow(const InBuff; InSize: TMemSize; out OutBuff): TMemSize;
var
  InPtr:    Pointer;
  OutPtr:   Pointer;
  OutSize:  TMemSize;
  TempSize: TMemSize;

  procedure AdvanceIn(AdvanceBy: TMemSize);
  begin
    InPtr := OffsetPtr(InPtr,AdvanceBy);
    InSize := InSize - AdvanceBy;
  end;

  procedure AdvanceOut(AdvanceBy: TMemSize);
  begin
    Result := Result + AdvanceBy;
    OutPtr := OffsetPtr(OutPtr,AdvanceBy);
    OutSize := OutSize - AdvanceBy;
  end;

begin
{
  InSize must be non-zero, one of input or output transfer is non-empty.

  There is a risk of input data overwrite, so processed bytes are stored in
  temporary buffer and then copied into output at the end.
}
Result := 0;
// prepare buffer
If InSize > fOutBufferSize then
  begin
  {
    Do not use ReallocMem, we do not need to preserve the data and copying
    (which is part of reallocation) is therefore pointless.
  }
    If Assigned(fOutBuffer) and (fOutBufferSize <> 0) then
      FreeMem(fOutBuffer,fOutBufferSize);
    GetMem(fOutBuffer,InSize);
    fOutBufferSize := InSize;
  end;
// prepare local helper variables
InPtr := Addr(InBuff);
OutPtr := fOutBuffer;
OutSize := InSize;
// flush-complete input transfer block
If fInTransferCount > 0 then
  begin
    TempSize := Min(fBlockBytes - fInTransferCount,InSize);
    PutToInTransfer(InPtr,TempSize);
    AdvanceIn(TempSize);
  end;  
// copy transferred output (also may contain data from in-transfer completion)
If fOutTransferCount > 0 then
  begin
    TempSize := Min(fOutTransferCount,OutSize);
    TakeFromOutTransfer(OutPtr,TempSize);
    AdvanceOut(TempSize);
  end;
// process whole blocks
while (InSize >= fBlockBytes) and (OutSize >= fBlockBytes) do
  begin
    fBlockUpdateProc(InPtr^,OutPtr^);
    AdvanceIn(fBlockBytes);
    AdvanceOut(fBlockBytes);
  end;
// store remaining input
If InSize > 0 then
  begin
    PutToInTransfer(InPtr,InSize);
    AdvanceIn(InSize);
  end;
// store output
If Result > 0 then
  Move(fOutBuffer^,Addr(OutBuff)^,Result);
end;


//------------------------------------------------------------------------------

Function TBlockCipher.UpdateProcessing(const InBuff; InSize: TMemSize; out OutBuff): TMemSize;
begin
If InSize > 0 then
  begin
    If (fInTransferCount <= 0) and (fOutTransferCount <= 0) then
      Result := UpdateProcessingFast(InBuff,Insize,OutBuff)
    else
      Result := UpdateProcessingSlow(InBuff,Insize,OutBuff)
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.FinalProcessing(const InBuff; InSize: TMemSize; out OutBuff);
var
  OutPtr: Pointer;
begin
{
  First update and flush everything - note that output buffer is assumed to be
  properly allocated to accept entire rest of the output.
}
OutPtr := OffsetPtr(Addr(OutBuff),UpdateProcessing(InBuff,InSize,OutBuff));
// flush output transfer
If fOutTransferCount > 0 then
  begin
    TakeFromOutTransfer(OutPtr,fOutTransferCount,False);
    OutPtr := OffsetPtr(OutPtr,fOutTransferCount);
    fOutTransferCount := 0;
  end;
{
  Non-empty input transfer at this point indicates incomplete block and a need
  for padding and further processing.
}
If fInTransferCount > 0 then
  begin
    Move(fInTransferData^,OutPtr^,fInTransferCount);
    BlockPad(OutPtr^,fInTransferCount);
    fBlockUpdateProc(OutPtr^,OutPtr^);
    fInTransferCount := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.CipherSetup(Mode: TCipherMode; ModeOfOperation: TBlockCipherModeOfOperation; Key, InitVector: Pointer; KeyBytes, BlockBytes: TMemSize);
begin
If Mode in [cmEncrypt,cmDecrypt] then
  fMode := Mode
else
  raise ECipherInvalidValue.CreateFmt('TCipherBase.CipherSetup: Invalid cipher mode (%d).',[Ord(Mode)]);
SetModeOfOperation(ModeOfOperation);
SetBlockBytes(BlockBytes);  // also does reallocation
If Assigned(InitVector) then
  Move(InitVector^,fInitVector^,fBlockBytes)
else
  FillChar(fInitVector^,fBlockBytes,0);
SetKeyBytes(KeyBytes);
Move(Key^,fKey^,fKeyBytes);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.Initialize;
begin
inherited;
fModeOfOperation := moCBC;
fPadding := padZeroes;
fBlockBytes := 0;
fInitVector := nil;
fKey := nil;
fKeyBytes := 0;
// internals...
fInTransferData := nil;
fInTransferSize := 0;
fInTransferCount := 0;
fOutTransferData := nil;
fOutTransferSize := 0;
fOutTransferCount := 0;
fTempBlock := nil;
fOutBuffer := nil;
fOutBufferSize := 0;
fBlockUpdateProc := BlockUpdate_CBC;
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.Finalize;
begin
If fBlockBytes <> 0 then
  begin
    If Assigned(fInitVector) then
      FreeMem(fInitVector,fBlockBytes);
    If Assigned(fTempBlock) then
      FreeMem(fTempBlock,fBlockBytes);
  end;
If Assigned(fInTransferData) and (fInTransferSize <> 0) then
  FreeMem(fInTransferData,fInTransferSize);
If Assigned(fOutTransferData) and (fOutTransferSize <> 0) then
  FreeMem(fOutTransferData,fOutTransferSize);
If Assigned(fOutBuffer) and (fOutBufferSize <> 0) then
  FreeMem(fOutBuffer,fOutBufferSize);
If Assigned(fKey) and (fKeyBytes <> 0) then
  FreeMem(fKey,fKeyBytes);
inherited;
end;

//------------------------------------------------------------------------------

Function TBlockCipher.RectifyBufferSize(Value: TMemSize): TMemSize;
begin
If Value > fBlockBytes then
  Result := (Value div fBlockBytes) * fBlockBytes
else
  Result := fBlockBytes;
end;

{-------------------------------------------------------------------------------
    TBlockCipher - public methods
-------------------------------------------------------------------------------}

constructor TBlockCipher.CreateForEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize);
begin
Create;
SetupEncryption(ModeOfOperation,Key,InitVector,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBlockCipher.CreateForEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize);
begin
Create;
SetupEncryption(ModeOfOperation,Key,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBlockCipher.CreateForEncryption(const Key; KeyBytes, BlockBytes: TMemSize);
begin
Create;
SetupEncryption(Key,KeyBytes,BlockBytes);
end;

//------------------------------------------------------------------------------

constructor TBlockCipher.CreateForDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Create;
SetupDecryption(ModeOfOperation,Key,InitVector,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBlockCipher.CreateForDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Create;
SetupDecryption(ModeOfOperation,Key,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBlockCipher.CreateForDecryption(const Key; KeyBytes, BlockBytes: TMemSize{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Create;
SetupDecryption(Key,KeyBytes,BlockBytes);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetupEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmEncrypt,ModeOfOperation,@Key,@InitVector,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBlockCipher.SetupEncryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmEncrypt,ModeOfOperation,@Key,nil,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBlockCipher.SetupEncryption(const Key; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmEncrypt,fModeOfOperation,@Key,nil,KeyBytes,BlockBytes);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.SetupDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; const InitVector; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmDecrypt,ModeOfOperation,@Key,@InitVector,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBlockCipher.SetupDecryption(ModeOfOperation: TBlockCipherModeOfOperation; const Key; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmDecrypt,ModeOfOperation,@Key,nil,KeyBytes,BlockBytes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TBlockCipher.SetupDecryption(const Key; KeyBytes, BlockBytes: TMemSize);
begin
CipherSetup(cmDecrypt,fModeOfOperation,@Key,nil,KeyBytes,BlockBytes);
end;

//------------------------------------------------------------------------------

procedure TBlockCipher.Init;
begin
inherited;
fInTransferCount := 0;
fOutTransferCount := 0;
end;

//------------------------------------------------------------------------------

Function TBlockCipher.FinalOutputSize(InSize: TMemSize): TMemSize;
begin
Result := InSize + fInTransferCount;
If Result mod fBlockBytes <> 0 then
  Result := Succ(Result div fBlockBytes) * fBlockBytes;
Result := Result + fOutTransferCount;
end;

//------------------------------------------------------------------------------

Function TBlockCipher.OutputSize(InputSize: TMemSize): TMemSize;
begin
If InputSize mod fBlockBytes <> 0 then
  Result := Succ(InputSize div fBlockBytes) * fBlockBytes
else
  Result := InputSize;
end;

end.
