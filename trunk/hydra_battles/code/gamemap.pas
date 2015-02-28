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
  MapWidth = 20;
  MapHeight = 10;

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
  TPropType = (ptHumanHeadquarters, ptHumanBarracks,
    ptTree, ptGrass, ptCursor,
    ptMountain1, ptMountain2, ptMountain3, ptMountain4,
    ptMountain5, ptMountain6, ptMountain7, ptMountain8);

  TProp = class
  strict private
    PropType: TPropType;
    Name: string;
    Image: TCastleImage;
    EditorShortcut: TKey;
    { Pivot, in image coords (0,0 is bottom-left). }
    Pivot: TVector2Integer;
    GLImage: TGLImage;
  public
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
    { Map contents. }
    Map: array [0 .. MapWidth - 1, 0 .. MapHeight - 1] of TProp;
    GLBackground: TGLImage;
    { Possible props on map. }
    property Props: TProps read FProps;
  public
    constructor Create(const AProps: TProps); reintroduce;
    destructor Destroy; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

function PropTypeFromName(const AName: string): TPropType;

implementation

uses SysUtils,
  CastleScene, CastleFilesUtils, CastleSceneCore,
  CastleColors, CastleUtils, CastleGLUtils, CastleStringUtils,
  GameUtils;

const
  PropName: array [TPropType] of string =
  ('humanHeadquarters', 'humanBarracks',
    'tree', 'grass', 'cursor',
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
begin
  inherited Create;
  PropType := APropType;
  Name := PropName[PropType];
  Image := LoadImage(GameConf.GetURL('props/' + Name + '/url'), []);
  Pivot[0] := GameConf.GetValue('props/' + Name + '/pivot_x', Image.Width div 2);
  Pivot[1] := Image.Height - 1 - GameConf.GetValue('props/' + Name + '/pivot_y', Image.Height div 2);
  EditorShortcut := GameConf.GetValue('props/' + Name + '/editor_shortcut', EditorShortcut);
end;

destructor TProp.Destroy;
begin
  GLContextClose;
  FreeAndNil(Image);
  inherited;
end;

procedure TProp.GLContextOpen;
begin
  if GLImage = nil then
    GLImage := TGLImage.Create(Image, true);
end;

procedure TProp.GLContextClose;
begin
  FreeAndNil(GLImage);
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
      PropName := GameConf.GetValue(Format('map/tile%d_%d/name', [X, Y]), '');
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

end.
