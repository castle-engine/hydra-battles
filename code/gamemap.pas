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
  CastleGLImages, CastleUIControls;

const
  MapWidth = 10;
  MapHeight = 20;
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
    Grid: boolean;
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
    Background: TCastleImage;
    GLBackground: TGLImage;
    { Possible props on map. }
    property Props: TProps read FProps;
  public
    { Map contents. }
    Map: array [0 .. MapWidth - 1, 0 .. MapHeight - 1] of TProp;
    EditCursor: TVector2Integer;
    EditMode: boolean;
    Grid: boolean;
    constructor Create(const AProps: TProps); reintroduce;
    destructor Destroy; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    procedure Render; override;
  end;

function PropTypeFromName(const AName: string): TPropType;

implementation

uses SysUtils, Math,
  CastleScene, CastleFilesUtils, CastleSceneCore, CastleGLUtils,
  CastleColors, CastleUtils, CastleStringUtils, CastleRectangles,
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

constructor TMap.Create(const AProps: TProps);
var
  X, Y: Integer;
  PropName: string;
begin
  inherited Create(nil);
  FProps := AProps;
  Background := LoadImage(GameConf.GetURL('map/background'), []);
  for X := 0 to MapWidth - 1 do
    for Y := 0 to MapHeight - 1 do
    begin
      PropName := GameConf.GetValue(Format('map/tile_%d_%d/name', [X, Y]), '');
      if PropName = '' then
        Map[X, Y] := nil else
        Map[X, Y] := Props[PropTypeFromName(PropName)];
    end;
end;

destructor TMap.Destroy;
begin
  FreeAndNil(Background);
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
  inherited;
  FreeAndNil(GLBackground);
end;

procedure TMap.Render;
var
  R: TRectangle;
  TileW, TileH: Single;

  procedure RenderProp(const X, Y: Integer; const Prop: TProp);

    procedure RenderPropRect(TileRect: TRectangle; const Prop: TProp);
    var
      TileImage: TGLImage;
      NewTileRectHeight: Integer;
    begin
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

  var
    TileRect: TRectangle;
  begin
    TileRect.Left := Round(R.Left + X * TileW);
    if not Odd(Y) then TileRect.Left -= Round(TileW / 2);
    TileRect.Bottom := Round(R.Bottom + (Y - 1) * TileH / 2);
    TileRect.Width := Ceil(TileW);
    TileRect.Height := Ceil(TileH);
    //UIFont.Print(TileRect.Middle[0], TileRect.Middle[1], Black, Format('%d,%d', [X, Y]));
    RenderPropRect(TileRect, Prop);
  end;

var
  MapW, MapH: Single;
  X, Y, ContainerW, ContainerH: Integer;
begin
  inherited;
  { Map width, height assuming that tile width = 1.0. }
  MapW := MapWidth - 1.0; { cut off 0.5 margin from left/right side }
  MapH := MapHeight / 2 - 0.5;
//  if not Odd(MapHeight) then MapH -= ;
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
  TileW := R.Width / (MapWidth - 1);
  TileH := TileW / TileWidthToHeight;

  GLBackground.Draw(R);

  ScissorEnable(R);

  if Grid then
    for Y := MapHeight - 1 downto 0 do
      for X := 0 to MapWidth - 1 do
        RenderProp(X, Y, Props[ptTileFrame]);

  for Y := MapHeight - 1 downto 0 do
    for X := 0 to MapWidth - 1 do
    begin
      if Map[X, Y] <> nil then
        RenderProp(X, Y, Map[X, Y]);
      //RenderProp(X, Y, Props[ptGrass]);
    end;

  if EditMode then
    RenderProp(EditCursor[0], EditCursor[1], Props[ptCursor]);

  ScissorDisable;

end;

end.
