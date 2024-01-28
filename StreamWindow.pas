{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  StreamWindow

    Provides very simple class TStreamWindow (descendant of TStream) that is
    intended to be used as a substream within larger stream.

    It is designed for situation where you want to limit access to only small
    part of a larger stream.
    For example, if you want to read stored string list that is embedded into
    larger stream. Normally, the method LoadFromStream would read everything
    from current position to the end of the stream, producing bad results if
    there is anything stored behind the actual list data. Stream window can be
    used in this situation to limit reading to only the list data. Let's say
    the list data starts in aStream at position 1024 and are 256 bytes long,
    then you can do something like...

        StrWnd := TStreamWindow.Create(aStream,1024,256);
        try
          StringList.LoadFromStream(StrWnd);
        finally
          StrWnd.Free;
        end;

    ...this ensures that only at most 256 bytes (from position 1024) can be
    read, forcing the reader/parser to stop even when end of the underlying
    stream was not reached.

    WARNING - do not directly access (seek, read, write, size set, ...) the
              target stream while it is bound to any stream window, as there
              is no implicit position synchronization (the window keeps its own
              position that is synchronized with position of target stream only
              at creation).
              Or use method SynchonizePosition before switching operation to
              the window every time you change target stream.

  Version 1.0 (2024-01-28)

  Last change (2024-01-28)

  ©2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.StreamWindow

  Dependencies:
    none

===============================================================================}
unit StreamWindow;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESWExceptin = class(Exception);

  ESWInvalidValue = class(ESWExceptin);

{===============================================================================
--------------------------------------------------------------------------------
                                  TStreamWindow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TStreamWindow - class declaration
===============================================================================}
type
  TStreamWindow = class(TStream)
  protected
    fOwnsTarget:      Boolean;
    fTarget:          TStream;
    fStart:           Int64;
    fLength:          Int64;
    fWindowPosition:  Int64;
    procedure Initialize(Target: TStream; Start,Length: Int64); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(Target: TStream; Start,Length: Int64); overload;
    constructor Create(Target: TStream; Length: Int64); overload;
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SynchonizePosition; virtual;
    property OwnsTarget: Boolean read fOwnsTarget write fOwnsTarget;
    property Target: TStream read fTarget;
  end;

implementation

{===============================================================================
--------------------------------------------------------------------------------
                                  TStreamWindow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TStreamWindow - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TStreamWindow - protected methods
-------------------------------------------------------------------------------}

procedure TStreamWindow.Initialize(Target: TStream; Start,Length: Int64);
begin
fOwnsTarget := False;
fTarget := Target;
fStart := Start;
fLength := Length;
fWindowPosition := 0;
fTarget.Seek(fStart,soBeginning);
end;

//------------------------------------------------------------------------------

procedure TStreamWindow.Finalize;
begin
If fOwnsTarget then
  FreeAndNil(fTarget);
end;

{-------------------------------------------------------------------------------
    TStreamWindow - public methods
-------------------------------------------------------------------------------}

constructor TStreamWindow.Create(Target: TStream; Start,Length: Int64);
begin
inherited Create;
Initialize(Target,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TStreamWindow.Create(Target: TStream; Length: Int64);
begin
inherited Create;
Initialize(Target,Target.Position,Length);
end;

//------------------------------------------------------------------------------

destructor TStreamWindow.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TStreamWindow.Read(var Buffer; Count: LongInt): LongInt;
begin
If fWindowPosition < 0 then
  Seek(0,soBeginning);
If fWindowPosition >= fLength then
  begin
    Result := 0;
    Exit;
  end
else
  begin
    If fWindowPosition + Count > fLength then
      Count := LongInt(fLength - fWindowPosition);
    Result := fTarget.Read(Buffer,Count);
    Inc(fWindowPosition,Result);
  end;
end;

//------------------------------------------------------------------------------

Function TStreamWindow.Write(const Buffer; Count: LongInt): LongInt;
begin
If fWindowPosition < 0 then
  Seek(0,soBeginning);
If fWindowPosition >= fLength then
  begin
    Result := 0;
    Exit;
  end
else
  begin
    If fWindowPosition + Count > fLength then
      Count := LongInt(fLength - fWindowPosition);
    Result := fTarget.Write(Buffer,Count);
    Inc(fWindowPosition,Result);
  end;  
end;

//------------------------------------------------------------------------------

Function TStreamWindow.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
case Origin of
  soBeginning:  begin
                  If Offset < 0 then
                    fWindowPosition := fTarget.Seek(fStart,soBeginning) - fStart
                  else If Offset > fLength then
                    fWindowPosition := fTarget.Seek(fStart + fLength,soBeginning) - fStart
                  else
                    fWindowPosition := fTarget.Seek(fStart + Offset,soBeginning) - fStart;
                end;
  soCurrent:    begin
                  If Offset < -fWindowPosition then
                    fWindowPosition := fTarget.Seek(-fWindowPosition,soCurrent) - fStart
                  else If Offset > (fLength - fWindowPosition) then
                    fWindowPosition := fTarget.Seek(fLength - fWindowPosition,soCurrent) - fStart
                  else
                    fWindowPosition := fTarget.Seek(Offset,soCurrent) - fStart;
                end;
  soEnd:        begin
                  If Offset < -fLength then
                    fWindowPosition := fTarget.Seek(fStart,soBeginning) - fStart
                  else If Offset > 0 then
                    fWindowPosition := fTarget.Seek(fStart + fLength,soBeginning) - fStart
                  else
                    fWindowPosition := fTarget.Seek(fStart + fLength + Offset,soBeginning) - fStart
                end;
else
  raise ESWInvalidValue.CreateFmt('TStreamWindow.Seek: Invalid seek origin (%d).',[Ord(Origin)]);
end;
Result := fWindowPosition;
end;

//------------------------------------------------------------------------------

procedure TStreamWindow.SynchonizePosition;
begin
If fWindowPosition < 0 then
  fWindowPosition := 0
else If fWindowPosition > fLength then
  fWindowPosition := fLength;
fTarget.Seek(fStart + fWindowPosition,soBeginning);
end;

end.
