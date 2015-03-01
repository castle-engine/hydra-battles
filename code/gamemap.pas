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
  CastleGLImages, CastleUIControls,
  GameNpcs;

const
  { Aspect ratio of rendered tile. }
  TileWidthToHeight = 64 / 36;

type
  { All possible prop types.

    It's nice pretty to hardcode all possible prop types here.
    But it allows:

    @orderedList(
      @item(easy usage of TCastleConfig to read their properties, since
         we don't have to iterate over <prop name="xxx"> elements,
         we just read <xxx> elements.)
      @item(for some prop types, like HQ and barracks, we really need to know
       their function anyway.)
    )

    But for some props, it should be possible to define them in pure game.xml,
    which is not possible now: you have to always extent this type.
    In the future, this type should represent only generic prop types. }
  TPropType = (ptHumanHeadquarters, ptHumanBarracks, ptHumanMine,
    ptTree, ptGrass, ptWater, ptCursor, ptTileFrame,
    ptMountain1, ptMountain2, ptMountain3, ptMountain4,
    ptMountain5, ptMountain6, ptMountain7, ptMountain8);

  TProp = class
  strict private
    PropType: TPropType;
    FName: string;
    Image: TCastleImage;
    FEditorShortcut: char;
    FPivot: TVector2Integer;
    FGLImage: TGLImage;
  public
    property GLImage: TGLImage read FGLImage;
    { Pivot, in image coords (0,0 is bottom-left). }
    property Pivot: TVector2Integer read FPivot;
    property Name: string read FName;
    property EditorShortcut: char read FEditorShortcut;
    constructor Create(const APropType: TPropType);
    destructor Destroy; override;
    procedure GLContextOpen;
    procedure GLContextClose;
  end;

  TProps = class(specialize TFPGMap<TPropType,TProp>)
  public
    { Create, reading list contents from config file. }
    constructor Create;
    destructor Destroy; override;
    procedure GLContextOpen;
    procedure GLContextClose;
  end;

  TMap = class(TUIControl)
  strict private
    FProps: TProps;
    FNpcs: TNpcs;
    Background: TCastleImage;
    GLBackground: TGLImage;
    FName: string;
    FWidth, FHeight: Cardinal;
    { Possible props on map. }
    property Props: TProps read FProps;
  public
    { Props on map. }
    MapProps: array of array of TProp;
    { Npcs on map. }
    MapNpcs: array of array of TNpcInstance;
    EditCursor: TVector2Integer;
    EditMode: boolean;
    Grid: boolean;
    property Name: string read FName;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    constructor Create(const AName: string; const AProps: TProps; const ANpcs: TNpcs); reintroduce;
    destructor Destroy; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    procedure Render; override;
    procedure SaveToFile;
  end;

function PropTypeFromName(const AName: string): TPropType;

implementation

uses SysUtils, Math,
  CastleScene, CastleFilesUtils, CastleSceneCore, CastleGLUtils,
  CastleColors, CastleUtils, CastleStringUtils, CastleRectangles, CastleLog,
  GameUtils;

const
  PropName: array [TPropType] of string =
  ('humanHeadquarters', 'humanBarracks', 'humanMine',
    'tree', 'grass', 'water', 'cursor', 'tileFrame',
    'mountain1', 'mountain2', 'mountain3', 'mountain4',
    'mountain5', 'mountain6', 'mountain7', 'mountain8');

function PropTypeFromName(const AName: string): TPropType;
begin
  for Result := Low(Result) to High(Result) do
    if PropName[Result] = AName then
      Exit;
  raise Exception.CreateFmt('"%s" is not a prop name', [AName]);
end;

{ TProp ---------------------------------------------------------------------- }

constructor TProp.Create(const APropType: TPropType);
var
  EditorShortcutStr: string;
begin
  inherited Create;
  PropType := APropType;
  FName := PropName[PropType];
  Image := LoadImage(GameConf.GetURL('props/' + Name + '/url'), []);
  FPivot[0] := GameConf.GetValue('props/' + Name + '/pivot_x', Image.Width div 2);
  FPivot[1] := Image.Height - 1 - GameConf.GetValue('props/' + Name + '/pivot_y', Image.Height div 2);
  EditorShortcutStr := GameConf.GetValue('props/' + Name + '/editor_shortcut', '');
  if Length(EditorShortcutStr) > 1 then
    raise Exception.CreateFmt('Invalid prop editor shortcut (too long, this should be 1 char or nothing): %s', [EditorShortcutStr]);
  if Length(EditorShortcutStr) = 1 then
    FEditorShortcut := EditorShortcutStr[1] else
    FEditorShortcut := #0;
end;

destructor TProp.Destroy;
begin
  GLContextClose;
  FreeAndNil(Image);
  inherited;
end;

procedure TProp.GLContextOpen;
begin
  if FGLImage = nil then
    FGLImage := TGLImage.Create(Image, true);
end;

procedure TProp.GLContextClose;
begin
  FreeAndNil(FGLImage);
end;

{ TProps ------------------------------------------------------------------ }

constructor TProps.Create;
var
  PT: TPropType;
begin
  inherited;
  for PT := Low(PT) to High(PT) do
    KeyData[PT] := TProp.Create(PT);
end;

destructor TProps.Destroy;
var
  I: Integer;
begin
  GLContextClose;
  for I := 0 to Count - 1 do
  begin
    Data[I].Free;
    Data[I] := nil;
  end;
  inherited;
end;

procedure TProps.GLContextOpen;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Data[I].GLContextOpen;
end;

procedure TProps.GLContextClose;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Data[I].GLContextClose;
end;

{ TMap ----------------------------------------------------------------------- }

constructor TMap.Create(const AName: string; const AProps: TProps; const ANpcs: TNpcs);
var
  X, Y: Integer;
  PropName, ConfPath: string;
begin
  inherited Create(nil);
  FProps := AProps;
  FNpcs := ANpcs;
  FName := AName;
  ConfPath := 'maps/' + Name;
  FWidth := GameConf.GetValue(ConfPath + '/width', 10);
  FHeight := GameConf.GetValue(ConfPath + '/height', 10);

  SetLength(MapProps, Width, Height);
  SetLength(MapNpcs, Width, Height);

  Background := LoadImage(GameConf.GetURL(ConfPath + '/background'), []);
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
    begin
      PropName := GameConf.GetValue(Format(ConfPath + '/tile_%d_%d/name', [X, Y]), '');
      if PropName = '' then
        MapProps[X, Y] := nil else
        MapProps[X, Y] := Props[PropTypeFromName(PropName)];
    end;
end;

destructor TMap.Destroy;
var
  X, Y: Integer;
begin
  FreeAndNil(Background);
  for Y := Height - 1 downto 0 do
    for X := 0 to Width - 1 do
      FreeAndNil(MapNpcs[X, Y]);
  inherited;
end;

procedure TMap.GLContextOpen;
begin
  inherited;
  if GLBackground = nil then
    GLBackground := TGLImage.Create(Background, true);
end;

procedure TMap.GLContextClose;
begin
  FreeAndNil(GLBackground);
  inherited;
end;

procedure TMap.Render;
var
  R: TRectangle;
  TileW, TileH: Single;

  function GetTileRect(const X, Y: Integer): TRectangle;
  begin
    Result.Left := Round(R.Left + X * TileW);
    if not Odd(Y) then Result.Left -= Round(TileW / 2);
    Result.Bottom := Round(R.Bottom + (Y - 1) * TileH / 2);
    Result.Width := Ceil(TileW);
    Result.Height := Ceil(TileH);
  end;

  procedure RenderProp(const X, Y: Integer; const Prop: TProp);
  var
    TileRect: TRectangle;
    TileImage: TGLImage;
    NewTileRectHeight: Integer;
  begin
    TileRect := GetTileRect(X, Y);
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
    TileRect := GetTileRect(X, Y);

    { for NPC, TileRect is uniform }
    NewTileRectHeight := TileRect.Width;
    TileRect.Bottom -= (NewTileRectHeight - TileRect.Height) div 2; // keep centered
    TileRect.Height := NewTileRectHeight;

    //TileRect := TileRect.Grow(TileRect.Width div 2);

    N.Draw(TileRect);
  end;

var
  MapW, MapH: Single;
  X, Y, ContainerW, ContainerH: Integer;
begin
  inherited;
  { Map width, height assuming that tile width = 1.0. }
  MapW := Width - 1.0; { cut off 0.5 margin from left/right side }
  MapH := Height / 2 - 0.5;
  MapH /= TileWidthToHeight;
  ContainerW := ContainerWidth - 2 * SideControlWidth; // leave some space for controls on screen sides
  ContainerH := ContainerHeight;
  if MapW / MapH > ContainerW / ContainerH then
  begin
    R.Left := 0;
    R.Width := ContainerW;
    R.Height := Round(R.Width * MapH / MapW); // adjust R.Height to aspect
    R.Bottom := (ContainerH - R.Height) div 2;
  end else
  begin
    R.Bottom := 0;
    R.Height := ContainerH;
    R.Width := Round(R.Height * MapW / MapH); // adjust R.Width to aspect
    R.Left := (ContainerW - R.Width) div 2;
  end;
  R.Left += SideControlWidth;
  TileW := R.Width / (Width - 1);
  TileH := TileW / TileWidthToHeight;

  GLBackground.Draw(R);

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
    RenderProp(EditCursor[0], EditCursor[1], Props[ptCursor]);

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

end.
