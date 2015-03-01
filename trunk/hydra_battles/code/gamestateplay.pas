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

{ Game state to play actual game. }
unit GameStatePlay;

interface

uses Classes,
  CastleConfig, CastleKeysMouse, CastleControls, Castle2DSceneManager,
  GameStates, GameMap;

type
  TStatePlay = class(TState)
  private
    Status: TCastleLabel;
    FirstStart: boolean;
    Props: TProps;
    Map: TMap;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Finish; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single); override;
    procedure Press(const Event: TInputPressRelease); override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils,
  CastleScene, CastleVectors, CastleFilesUtils, CastleSceneCore,
  CastleColors, CastleUIControls, CastleUtils, CastleGLUtils,
  CastleGLImages, CastleStringUtils,
  GameUtils, GameStateMainMenu;

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  { In the 1st start, controls will be created and added to Window.Controls,
    to initialize resources (non-GL and GL) only then.
    Later, controls will be kept on Window.Controls list,
    to not reinitialize GL resources without need. }
  FirstStart := true;
end;

procedure TStatePlay.Start;

  procedure CreateControls;
  begin
    Status := TCastleLabel.Create(Self);
    Status.Padding := 5;
    Status.Color := White;
    Status.Left := 10;
    Status.Bottom := 10;
    Status.Frame := false;
    Status.Alignment := prRight;
    Window.Controls.InsertFront(Status);
  end;

begin
  inherited;

  if FirstStart then
  begin
    CreateControls;
    FirstStart := false;
  end else
  begin
    Status.Exists := true;
  end;

  Props := TProps.Create;
  Map := TMap.Create(Props);
  Window.Controls.InsertFront(Map);
end;

procedure TStatePlay.Finish;
begin
  Status.Exists := false;
  FreeAndNil(Map);
  FreeAndNil(Props);
  inherited;
end;

procedure TStatePlay.Resize;
begin
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single);
var
  S: string;
begin
  inherited;

  S := Format('FPS: %f real : %f', [Window.Fps.FrameTime, Window.Fps.RealTime]);
  Status.Text.Text := S;
  Status.AlignHorizontal(prRight, prRight);
  Status.AlignVertical(prTop, prTop);

  Window.Invalidate;
end;

procedure TStatePlay.Press(const Event: TInputPressRelease);
var
  PT: TPropType;
  Prop: TProp;
  RandomMountain: char;
begin
  inherited;
  if Event.IsKey('E') then
    Map.EditMode := not Map.EditMode;
  if Event.IsKey('G') then
    Map.Grid := not Map.Grid;
  if Map.EditMode then
  begin
    if Event.IsKey(K_Up) then
      Map.EditCursor[1] := Map.EditCursor[1] + 1;
    if Event.IsKey(K_Down) then
      Map.EditCursor[1] := Map.EditCursor[1] - 1;
    if Event.IsKey(K_Right) then
      Map.EditCursor[0] := Map.EditCursor[0] + 1;
    if Event.IsKey(K_Left) then
      Map.EditCursor[0] := Map.EditCursor[0] - 1;
    Map.EditCursor[0] := Clamped(Map.EditCursor[0], 0, MapWidth - 1);
    Map.EditCursor[1] := Clamped(Map.EditCursor[1], 0, MapHeight - 1);
    for PT := Low(PT) to High(PT) do
    begin
      Prop := Props[PT];
      if Event.IsKey(Prop.EditorShortcut) then
        Map.Map[Map.EditCursor[0], Map.EditCursor[1]] := Prop;
    end;
    if Event.IsKey('0') then
    begin
      RandomMountain := Chr(Random(8) + Ord('1'));
      for PT := Low(PT) to High(PT) do
      begin
        Prop := Props[PT];
        if Prop.EditorShortcut = RandomMountain then
          Map.Map[Map.EditCursor[0], Map.EditCursor[1]] := Prop;
      end;
    end;
  end;
end;

procedure TStatePlay.GLContextOpen;
begin
  inherited;
  Props.GLContextOpen;
end;

procedure TStatePlay.GLContextClose;
begin
  if Props <> nil then
    Props.GLContextClose;
  inherited;
end;

end.
