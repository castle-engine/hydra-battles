{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Hydra Battles".

  "Hydra Battles" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Hydra Battles" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Abstract state definition. }
unit GameStates;

interface

uses Classes, FGL,
  CastleConfig, CastleKeysMouse;

type
  TState = class(TComponent)
  private
    class var FCurrent: TState;
    class function GetCurrent: TState; static;
    class procedure SetCurrent(const Value: TState); static;
  public
    class property Current: TState read GetCurrent write SetCurrent;

    procedure Start; virtual;
    procedure Finish; virtual;
    procedure Resize; virtual;
    procedure Update(const SecondsPassed: Single); virtual;
    procedure Press(const Event: TInputPressRelease); virtual;
    procedure GLContextOpen; virtual;
    procedure GLContextClose; virtual;
  end;

implementation

class function TState.GetCurrent: TState;
begin
  Result := FCurrent;
end;

class procedure TState.SetCurrent(const Value: TState);
begin
  if FCurrent <> Value then
  begin
    if FCurrent <> nil then
      FCurrent.Finish;
    FCurrent := Value;
    if FCurrent <> nil then
    begin
      FCurrent.Start;
      if FCurrent = Value then
        FCurrent.GLContextOpen;
      { call FCurrent.Resize, just like OnResize always happens after OnOpen.
        However, check first that we're still within the same state,
        to safeguard from the fact that FCurrent.Start changed state
        (like the loading state, that changes to play state immediately in start). }
      if FCurrent = Value then
        FCurrent.Resize;
    end;
  end;
end;

procedure TState.Start;
begin
end;

procedure TState.Finish;
begin
  GLContextClose;
end;

procedure TState.Resize;
begin
end;

procedure TState.Update(const SecondsPassed: Single);
begin
end;

procedure TState.Press(const Event: TInputPressRelease);
begin
end;

procedure TState.GLContextOpen;
begin
end;

procedure TState.GLContextClose;
begin
end;

end.
