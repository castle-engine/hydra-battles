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

{ Game map. }
unit GameMap;

interface

uses Classes, FGL,
  CastleConfig, CastleKeysMouse, CastleControls, CastleImages, CastleVectors,
  CastleGLImages, CastleUIControls, CastleRectangles,
  GameNpcs, GameAbstractMap, GamePath, GameProps;

type
  { Render map background. On a separate layer, to be place scene manager inside. }
  TMapBackground = class(TUIControl)
  strict private
    Background: TCastleImage;
    GLBackground: TGLImage;
  public
    Rect: TRectangle;
    procedure Render; override;
    constructor Create(const AName: string); reintroduce;
    destructor Destroy; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

  TMap = class(TAbstractMap)
  strict private
    FProps: TProps;
    FNpcs: TNpcs;
    { Possible props on map. }
    property Props: TProps read FProps;
  public
    { Props on map. }
    MapProps: array of array of TProp;
    { Npcs on map. }
    MapNpcs: array of array of TNpcInstance;
    NpcInstances: TNpcInstanceList;
    EditCursor: TVector2Integer;
    EditMode: boolean;
    Grid: boolean;
    constructor Create(const AName: string; const AProps: TProps; const ANpcs: TNpcs); reintroduce;
    destructor Destroy; override;
    procedure Render; override;
    procedure SaveToFile;
    function ValidTile(const X, Y: Integer; const OmitNpcInstance: TObject): boolean; override;
    procedure SetNpcInstance(const X, Y: Integer; const NewNpcInstance: TNpcInstance);
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses SysUtils, Math,
  CastleScene, CastleFilesUtils, CastleSceneCore, CastleGLUtils,
  CastleColors, CastleUtils, CastleStringUtils, CastleLog,
  GameUtils;

{ TMapBackground ------------------------------------------------------------- }

constructor TMapBackground.Create(const AName: string);
var
  ConfPath: string;
begin
  Name := AName;
  ConfPath := 'maps/' + AName;
  inherited Create(nil);
  Background := LoadImage(GameConf.GetURL(ConfPath + '/background'), []);
end;

destructor TMapBackground.Destroy;
begin
  FreeAndNil(Background);
  inherited;
end;

procedure TMapBackground.GLContextOpen;
begin
  inherited;
  if GLBackground = nil then
    GLBackground := TGLImage.Create(Background, true);
end;

procedure TMapBackground.GLContextClose;
begin
  FreeAndNil(GLBackground);
  inherited;
end;

procedure TMapBackground.Render;
begin
  inherited;
  GLBackground.Draw(Rect);
end;

{ TMap ----------------------------------------------------------------------- }

constructor TMap.Create(const AName: string; const AProps: TProps; const ANpcs: TNpcs);
var
  X, Y: Integer;
  PropName, ConfPath: string;
begin
  FProps := AProps;
  FNpcs := ANpcs;
  Name := AName;
  ConfPath := 'maps/' + Name;

  inherited Create(
    GameConf.GetValue(ConfPath + '/width', 10),
    GameConf.GetValue(ConfPath + '/height', 10)
  );

  //EditMode := true;
  EditCursor := Vector2Integer(Width div 2, Height div 2);

  SetLength(MapProps, Width, Height);
  SetLength(MapNpcs, Width, Height);

  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
    begin
      PropName := GameConf.GetValue(Format(ConfPath + '/tile_%d_%d/name', [X, Y]), '');
      if PropName = '' then
        MapProps[X, Y] := nil else
        MapProps[X, Y] := Props[PropTypeFromName(PropName)];
    end;

  NpcInstances := TNpcInstanceList.Create;
end;

destructor TMap.Destroy;
var
  X, Y: Integer;
begin
  FreeAndNil(NpcInstances);
  for Y := Height - 1 downto 0 do
    for X := 0 to Width - 1 do
      MapNpcs[X, Y] := nil;
  FreeAndNil(NpcInstances); // owns npc instances on the list, so frees them
  inherited;
end;

procedure TMap.Render;
var
  R: TRectangle;

  procedure RenderProp(const X, Y: Integer; const Prop: TProp);
  var
    TileRect: TRectangle;
    TileImage: TGLImage;
    NewTileRectHeight: Integer;
  begin
    TileRect := GetTileRect(R, X, Y);
    //UIFont.Print(TileRect.Middle[0], TileRect.Middle[1], Black, Format('%d,%d', [X, Y]));

    TileImage := Prop.GLImage;
    if Prop.GLImage = nil then
      raise Exception.CreateFmt('Prop "%s" GL resources not ready at rendering', [Prop.Name]);

    { now TileRect is calculated assuming that PropType.GLImage fills Tile size
      perfectly. But actually it may be a little taller, so account for this,
      knowing that width matches. }
    NewTileRectHeight := TileRect.Width * TileImage.Height div TileImage.Width;
    TileRect.Bottom -= (NewTileRectHeight - TileRect.Height) div 2; // keep centered
    TileRect.Height := NewTileRectHeight;
    { apply pivot }
    TileRect.Left -= Round((Prop.Pivot[0] - TileImage.Width div 2) * TileRect.Width / TileImage.Width);
    TileRect.Bottom -= Round((Prop.Pivot[1] - TileImage.Height div 2) * TileRect.Height / TileImage.Height);

    TileImage.Draw(TileRect);
  end;

  procedure RenderNpc(const X, Y: Integer; const N: TNpcInstance);
  var
    TileRect: TRectangle;
    NewTileRectHeight: Integer;
  begin
    TileRect := GetTileRect(R, X, Y);

    { for NPC, TileRect is uniform }
    NewTileRectHeight := TileRect.Width;
    TileRect.Bottom -= (NewTileRectHeight - TileRect.Height) div 2; // keep centered
    TileRect.Height := NewTileRectHeight;

    //TileRect := TileRect.Grow(TileRect.Width div 2);

    N.Draw(TileRect);
  end;

var
  X, Y: Integer;
begin
  inherited;
  { Map width, height assuming that tile width = 1.0. }
  R := Rect;

  ScissorEnable(R);

  if Grid then
    for Y := Height - 1 downto 0 do
      for X := 0 to Width - 1 do
        RenderProp(X, Y, Props[ptTileFrame]);

  for Y := Height - 1 downto 0 do
    for X := 0 to Width - 1 do
    begin
      //RenderProp(X, Y, Props[ptGrass]);
      if MapProps[X, Y] <> nil then
        RenderProp(X, Y, MapProps[X, Y]);
      if MapNpcs[X, Y] <> nil then
        RenderNpc(X, Y, MapNpcs[X, Y]);
    end;

  if EditMode then
  begin
    RenderProp(EditCursor[0], EditCursor[1], Props[ptCursor]);

    // test our neighbors logic is sensible
    // RenderProp(EditCursor[0] + 1, EditCursor[1]    , Props[ptCursor]);
    // RenderProp(EditCursor[0] - 1, EditCursor[1]    , Props[ptCursor]);
    // RenderProp(EditCursor[0]    , EditCursor[1] + 1, Props[ptCursor]);
    // RenderProp(EditCursor[0]    , EditCursor[1] - 1, Props[ptCursor]);
    // RenderProp(EditCursor[0]    , EditCursor[1] + 2, Props[ptCursor]);
    // RenderProp(EditCursor[0]    , EditCursor[1] - 2, Props[ptCursor]);
    // if Odd(EditCursor[1]) then
    // begin
    //   RenderProp(EditCursor[0] + 1, EditCursor[1] - 1, Props[ptCursor]);
    //   RenderProp(EditCursor[0] + 1, EditCursor[1] + 1, Props[ptCursor]);
    // end else
    // begin
    //   RenderProp(EditCursor[0] - 1, EditCursor[1] - 1, Props[ptCursor]);
    //   RenderProp(EditCursor[0] - 1, EditCursor[1] + 1, Props[ptCursor]);
    // end;
  end;

  ScissorDisable;
end;

procedure TMap.SaveToFile;
var
  X, Y: Integer;
  ConfPath, PropName: string;
begin
  ConfPath := 'maps/' + Name;
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
    begin
      if MapProps[X, Y] <> nil then
        PropName := MapProps[X, Y].Name else
        PropName := '';
      GameConf.SetDeleteValue(Format(ConfPath + '/tile_%d_%d/name', [X, Y]), PropName, '');
    end;
  WritelnLog('Map', 'Saved to file ' + GameConf.URL);
  GameConf.Flush;
end;

function TMap.ValidTile(const X, Y: Integer; const OmitNpcInstance: TObject): boolean;
var
  I: Integer;
begin
  { TODO: allow attacking enemies here }
  if MapProps[X, Y] <> nil then Exit(false);
  if (MapNpcs[X, Y] <> nil) and
     (MapNpcs[X, Y] <> OmitNpcInstance) then
    Exit(false);
  for I := 0 to NpcInstances.Count - 1 do
    if (NpcInstances[I] <> OmitNpcInstance) and
       (NpcInstances[I].Path <> nil) then
      if not NpcInstances[I].Path.ValidTile(X, Y) then
        Exit(false);
  Result := true;
end;

procedure TMap.SetNpcInstance(const X, Y: Integer; const NewNpcInstance: TNpcInstance);
var
  OldNpcInstance: TNpcInstance;
begin
  OldNpcInstance := MapNpcs[X, Y];
  if OldNpcInstance <> nil then
  begin
    MapNpcs[X, Y] := nil;
    NpcInstances.Remove(OldNpcInstance); // frees OldNpcInstance
    // OldNpcInstance := nil; // useless
  end;

  if NewNpcInstance <> nil then
  begin
    NpcInstances.Add(NewNpcInstance);
    MapNpcs[X, Y] := NewNpcInstance;
    NewNpcInstance.X := X;
    NewNpcInstance.Y := Y;
  end;
end;

procedure TMap.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  I: Integer;
  NI: TNpcInstance;
  OldX, OldY: Integer;
begin
  for I := 0 to NpcInstances.Count - 1 do
  begin
    NI := NpcInstances[I];
    OldX := NI.X;
    OldY := NI.Y;
    NI.Update(SecondsPassed, Self);
    if (OldX <> NI.X) or (OldY <> NI.Y) then
    begin
      MapNpcs[OldX, OldY] := nil;
      MapNpcs[NI.X, NI.Y] := NI;
    end;
  end;
end;

end.
