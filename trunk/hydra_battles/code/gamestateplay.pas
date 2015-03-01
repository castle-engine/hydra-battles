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
  GameStates, GameMap, GameNpcs, GamePath;

type
  TStatePlay = class(TState)
  private
    Status: TCastleLabel;
    Props: TProps;
    Map: TMap;
    Npcs: TNpcs;
    SceneManager: T2DSceneManager;
    Paths: TPathList;
    CurrentPath: TPath;
  public
    StartMapName: string;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Finish; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single); override;
    procedure Press(const Event: TInputPressRelease); override;
    procedure Release(const Event: TInputPressRelease); override;
    procedure Motion(const Event: TInputMotion); override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils,
  CastleScene, CastleVectors, CastleFilesUtils, CastleSceneCore,
  CastleColors, CastleUIControls, CastleUtils, CastleGLUtils,
  CastleGLImages, CastleStringUtils, CastleRectangles,
  GameUtils, GameStateMainMenu;

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TStatePlay.Start;
begin
  inherited;

  GameTime := 0;

  Props := TProps.Create;
  Npcs := TNpcs.Create;
  Paths := TPathList.Create;
  Map := TMap.Create(StartMapName, Props, Npcs, Paths);
  Window.Controls.InsertFront(Map);

  SceneManager := T2DSceneManager.Create(Self);
  SceneManager.FullSize :=false;
  SceneManager.Transparent := true;
  Window.Controls.InsertFront(SceneManager);

  Status := TCastleLabel.Create(Self);
  Status.Padding := 5;
  Status.Color := White;
  Status.Left := 10;
  Status.Bottom := 10;
  Status.Frame := false;
  Status.Alignment := prRight;
  Window.Controls.InsertFront(Status);
end;

procedure TStatePlay.Finish;
begin
  FreeAndNil(Status);
  FreeAndNil(SceneManager);
  FreeAndNil(Map);
  FreeAndNil(Props);
  FreeAndNil(Npcs);
  FreeAndNil(Paths);
  inherited;
end;

procedure TStatePlay.Resize;
var
  R: TRectangle;
begin
  inherited;

  R := Map.Rect;
  SceneManager.Left := R.Left;
  SceneManager.Bottom := R.Bottom;
  SceneManager.Width := R.Width;
  SceneManager.Height := R.Height;
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

  GameTime += SecondsPassed;

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
    Map.EditCursor[0] := Clamped(Map.EditCursor[0], 0, Map.Width - 1);
    Map.EditCursor[1] := Clamped(Map.EditCursor[1], 0, Map.Height - 1);
    for PT := Low(PT) to High(PT) do
    begin
      Prop := Props[PT];
      if Event.IsKey(Prop.EditorShortcut) then
        Map.MapProps[Map.EditCursor[0], Map.EditCursor[1]] := Prop;
    end;
    if Event.IsKey('0') then
    begin
      RandomMountain := Chr(Random(8) + Ord('1'));
      for PT := Low(PT) to High(PT) do
      begin
        Prop := Props[PT];
        if Prop.EditorShortcut = RandomMountain then
          Map.MapProps[Map.EditCursor[0], Map.EditCursor[1]] := Prop;
      end;
    end;
    if Event.IsKey(' ') then
      Map.MapProps[Map.EditCursor[0], Map.EditCursor[1]] := nil;
    if Event.IsKey('S') then
      Map.SaveToFile;
    if Event.IsKey('N') then
    begin
      FreeAndNil(Map.MapNpcs[Map.EditCursor[0], Map.EditCursor[1]]);
      Map.MapNpcs[Map.EditCursor[0], Map.EditCursor[1]] := TNpcInstance.Create(
        Npcs.Npcs[RandomFaction, RandomNpcType], RandomDirection);
    end;
  end;

  if Event.IsMouseButton(mbLeft) then
  begin
    CurrentPath := TPath.Create(Map, SceneManager);
    Paths.Add(CurrentPath);
  end;
end;

procedure TStatePlay.Release(const Event: TInputPressRelease);
begin
  inherited;
  CurrentPath := nil;
end;

procedure TStatePlay.Motion(const Event: TInputMotion);

  procedure BreakPath;
  begin
    { remove (freeing) CurrentPath }
    SceneManager.Items.Remove(CurrentPath.Visualization);
    Paths.Remove(CurrentPath); // this frees CurrentPath
    CurrentPath := nil;
  end;

var
  X, Y: Integer;
  MapRect: TRectangle;
begin
  inherited;
  if CurrentPath <> nil then
  begin
    MapRect := Map.Rect;
    if not Map.PositionToTile(MapRect, Event.Position, X, Y) then
      BreakPath else
    begin
      Map.EditCursor[0] := X;
      Map.EditCursor[1] := Y;
      if not CurrentPath.Add(X, Y) then
        BreakPath;
    end;
  end;
end;

procedure TStatePlay.GLContextOpen;
begin
  inherited;
  Props.GLContextOpen;
  Npcs.GLContextOpen;
end;

procedure TStatePlay.GLContextClose;
begin
  if Props <> nil then
    Props.GLContextClose;
  if Npcs <> nil then
    Npcs.GLContextClose;
  inherited;
end;

end.
