{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Hydra Battles".

  "Hydra Battles" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Hydra Battles" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$modeswitch nestedprocvars}{$H+}

{ Game props: things that don't move, but lie on map, and sometimes can be destroyed
  or harvested. Buldings and trees are special kinds of props. }
unit GameProps;

interface

uses Classes, Generics.Collections,
  CastleConfig, CastleKeysMouse, CastleControls, CastleImages, CastleVectors,
  CastleGLImages, CastleUIControls, CastleRectangles,
  GameUtils, GameAbstractMap, GameNpcs;

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
  TPropType = (
    ptHumansHeadquarters, ptHumansBarracks,
    ptMonstersHeadquarters, ptMonstersBarracks,
    ptMine,
    ptBigTree01, ptBigTree02, ptBigTree03,
    ptHemp01, ptHemp02, ptHemp03,
    ptGrass, ptCursor, ptCursorLight, ptTileFrame,
    ptMountain1, ptMountain2, ptMountain3, ptMountain4,
    ptMountain5, ptMountain6, ptMountain7, ptMountain8);

  TPropInstanceList = class;

  TProp = class
  strict private
    FPropType: TPropType;
    FName: string;
    FEditorShortcut: char;
    FPivot: TVector2Integer;
    FGLImage: TGLImage;
    FCostWood, FRewardWood: Cardinal;
    FInitialLife: Single;
    FNeutral: boolean;
    FFaction: TFaction;
    FTrain: boolean;
    FTrainNpc: TNpcType;
    FTrainDuration: Single;
    FScale: Single;
    FCaption: string;
  public
    property PropType: TPropType read FPropType;
    property GLImage: TGLImage read FGLImage;
    { Pivot, in image coords (0,0 is bottom-left). }
    property Pivot: TVector2Integer read FPivot;
    property Name: string read FName;
    property EditorShortcut: char read FEditorShortcut;
    property CostWood: Cardinal read FCostWood;
    property RewardWood: Cardinal read FRewardWood;
    property InitialLife: Single read FInitialLife;
    property Neutral: boolean read FNeutral;
    property Faction: TFaction read FFaction;
    property Train: boolean read FTrain;
    property TrainNpc: TNpcType read FTrainNpc;
    property TrainDuration: Single read FTrainDuration;
    property Scale: Single read FScale;
    property Caption: string read FCaption;
    constructor Create(const APropType: TPropType);
    destructor Destroy; override;
    procedure PrepareResources;
    procedure Draw(ScreenRectangle: TRectangle);

    { Test can we build a given prop.
      Notification (and return false) if not. }
    function CanBuild(const PropInstances: TPropInstanceList): boolean;
  end;

  TProps = class(specialize TDictionary<TPropType,TProp>)
  public
    { Create, reading list contents from config file. }
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure PrepareResources;
  end;

  TPropInstance = class
  private
    FProp: TProp;
    FTrainStart: Single;
    FTraining: boolean;
    FTrainingNpc: TNpc;
  public
    { Moves along this path now. }
    Life: Single;
    { Positon on map. Synchronized with Map.MapProps. }
    X, Y: Integer;
    property Prop: TProp read FProp;
    property Training: boolean read FTraining;
    constructor Create(const AProp: TProp);
    procedure Draw(ScreenRectangle: TRectangle);
    procedure StartTraining(const Npcs: TNpcs);
    procedure Update(const Map: TAbstractMap; out SpawnNpc: TNpc; out SpawnX, SpawnY: Integer);
  end;

  TPropInstanceList = class(specialize TObjectList<TPropInstance>)
    function Contains(const PropType: TPropType): boolean;
  end;

  TDraggedProp = class(TUIControl)
  public
    Map: TAbstractMap;
    Prop: TProp;
    ScreenPosition: TVector2Single;
    X, Y: Integer;
    procedure Render; override;
  end;

function PropTypeFromName(const AName: string): TPropType;

const
  Barracks: array [TFaction] of TPropType = (ptHumansBarracks, ptMonstersBarracks);
  Headquarters: array [TFaction] of TPropType = (ptHumansHeadquarters, ptMonstersHeadquarters);
  Trees: set of TPropType = [ptBigTree01, ptBigTree02, ptBigTree03, ptHemp01, ptHemp02, ptHemp03];

implementation

uses SysUtils,
  CastleScene, CastleFilesUtils, CastleGLUtils,
  CastleColors, CastleUtils, CastleStringUtils, CastleGameNotifications, CastleRenderContext;

const
  PropName: array [TPropType] of string =
  ( 'humansHeadquarters', 'humansBarracks',
    'monstersHeadquarters', 'monstersBarracks', 'mine',
    'bigtree01', 'bigtree02', 'bigtree03',
    'hemp01', 'hemp02', 'hemp03',
    'grass', 'cursor', 'cursorLight', 'tileFrame',
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
  EditorShortcutStr, ConfPath, TrainStr: string;
begin
  inherited Create;
  FPropType := APropType;
  FName := PropName[PropType];
  ConfPath := 'props/' + Name;
  FGLImage := TGLImage.Create(GameConf.GetURL(ConfPath + '/url'));
  FPivot[0] := GameConf.GetValue(ConfPath + '/pivot_x', FGLImage.Width div 2);
  FPivot[1] := FGLImage.Height - 1 - GameConf.GetValue(ConfPath + '/pivot_y', FGLImage.Height div 2);
  EditorShortcutStr := GameConf.GetValue(ConfPath + '/editor_shortcut', '');
  FCostWood := GameConf.GetValue(ConfPath + '/cost_wood', 0);
  FInitialLife := GameConf.GetFloat(ConfPath + '/initial_life', 0.0);
  FRewardWood := GameConf.GetValue(ConfPath + '/reward_wood', 0);
  FNeutral := GameConf.GetValue(ConfPath + '/neutral', true);
  TrainStr := GameConf.GetValue(ConfPath + '/train', '');
  FTrain := TrainStr <> '';
  if FTrain then
    FTrainNpc := NpcTypeFromName(TrainStr);
  FTrainDuration := GameConf.GetFloat(ConfPath + '/train_duration', 10.0);
  if not FNeutral then
    FFaction := FactionFromName(GameConf.GetValue(ConfPath + '/faction', ''));
  if Length(EditorShortcutStr) > 1 then
    raise Exception.CreateFmt('Invalid prop editor shortcut (too long, this should be 1 char or nothing): %s', [EditorShortcutStr]);
  if Length(EditorShortcutStr) = 1 then
    FEditorShortcut := EditorShortcutStr[1] else
    FEditorShortcut := #0;
  FScale := GameConf.GetFloat(ConfPath + '/scale', 1.0);
  FCaption := GameConf.GetValue(ConfPath + '/caption', Name);
end;

destructor TProp.Destroy;
begin
  FreeAndNil(FGLImage);
  inherited;
end;

procedure TProp.PrepareResources;
begin
  { not necessary, but makes sure we're fully initialized before game begins }
  FGLImage.PrepareResources;
end;

procedure TProp.Draw(ScreenRectangle: TRectangle);
var
  NewRectHeight: Integer;
begin
  if GLImage = nil then
    raise Exception.CreateFmt('Prop "%s" GL resources not ready at rendering', [Name]);

  ScreenRectangle := ScreenRectangle.Grow(Round(ScreenRectangle.Width * (Scale - 1)));

  { now ScreenRectangle is calculated assuming that Image fills tile size (ScreenRectangle)
    perfectly. But actually it may be a little taller, so account for this,
    knowing that width of ScreenRectangle (tile) matches Image width. }
  NewRectHeight := ScreenRectangle.Width * FGLImage.Height div FGLImage.Width;
  ScreenRectangle.Bottom -= (NewRectHeight - ScreenRectangle.Height) div 2; // keep centered
  ScreenRectangle.Height := NewRectHeight;
  { apply pivot }
  ScreenRectangle.Left -= Round((Pivot[0] - FGLImage.Width div 2) * ScreenRectangle.Width / FGLImage.Width);
  ScreenRectangle.Bottom -= Round((Pivot[1] - FGLImage.Height div 2) * ScreenRectangle.Height / FGLImage.Height);

  GLImage.Draw(ScreenRectangle);
end;

function TProp.CanBuild(const PropInstances: TPropInstanceList): boolean;
var
  HQ: TPropType;
begin
  if Neutral then
    Exit(true);

  Result := Trunc(Wood[Faction]) >= CostWood;
  if not Result then
    Notifications.Show(Format('Not enough wood to build "%s"', [Caption]));

  HQ := Headquarters[Faction];
  if (PropType <> HQ) and (not PropInstances.Contains(HQ)) then
  begin
    Result := false;
    Notifications.Show(Format('Cannot build "%s", build headquarters first', [Caption]));
  end;
end;

{ TProps ------------------------------------------------------------------ }

constructor TProps.Create;
var
  PT: TPropType;
begin
  inherited;
  for PT := Low(PT) to High(PT) do
    AddOrSetValue(PT, TProp.Create(PT));
end;

destructor TProps.Destroy;
var
  V: TProp;
begin
  for V in Values do
    V.Free;
  inherited;
end;

procedure TProps.PrepareResources;
var
  V: TProp;
begin
  for V in Values do
    V.PrepareResources;
end;

{ TPropInstance -------------------------------------------------------------- }

constructor TPropInstance.Create(const AProp: TProp);
begin
  inherited Create;
  FProp := AProp;
  Life := Prop.InitialLife;
end;

procedure TPropInstance.Draw(ScreenRectangle: TRectangle);
var
  LifeBarRect, TrainBarRect: TRectangle;
begin
  LifeBarRect := BarRectFromTileRect(ScreenRectangle);
  TrainBarRect := BarRectFromTileRect(ScreenRectangle, true);
  Prop.Draw(ScreenRectangle);
  if (Prop.InitialLife <> 0) and not Prop.Neutral then
    RenderBar(LifeBarRect, Black, FactionBarColor[Prop.Faction], Life / Prop.InitialLife);
  if FTraining then
    RenderBar(TrainBarRect, Green, Black, (GameTime - FTrainStart) / Prop.TrainDuration);
end;

procedure TPropInstance.StartTraining(const Npcs: TNpcs);
begin
  if (not Training) and Prop.Train and
     { neutral props cannot spawn, as we could not determine the faction on spawned unit }
     (not Prop.Neutral) and
     FactionCanMove(Prop.Faction) then
  begin
    FTrainingNpc := Npcs.Npcs[Prop.Faction, Prop.TrainNpc];
    if Trunc(Wood[Prop.Faction]) < FTrainingNpc.CostWood then
      Notifications.Show(Format('Not enough resources to train "%s" (cost wood: %d)',
        [FTrainingNpc.Name, FTrainingNpc.CostWood])) else
    begin
      Wood[Prop.Faction] -= FTrainingNpc.CostWood;
      FTraining := true;
      FTrainStart := GameTime;
    end;
  end;
end;

procedure TPropInstance.Update(const Map: TAbstractMap; out SpawnNpc: TNpc; out SpawnX, SpawnY: Integer);

  procedure TestTrySpawning(const TrySpawnX, TrySpawnY: Integer;
    var ContinueToNeighbors: boolean);
  begin
    if Map.ValidTile(TrySpawnX, TrySpawnY, nil) then
    begin
      SpawnX := TrySpawnX;
      SpawnY := TrySpawnY;
      SpawnNpc := FTrainingNpc;
      ContinueToNeighbors := false;
    end;
  end;

begin
  SpawnNpc := nil;
  if FTraining and (GameTime > FTrainStart + Prop.TrainDuration) then
  begin
    HandleNeighbors(X, Y, @TestTrySpawning);
    FTraining := false;
  end;
end;

{ TPropInstanceList ---------------------------------------------------------- }

function TPropInstanceList.Contains(const PropType: TPropType): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Prop.PropType = PropType then
      Exit(true);
  Result := false;
end;

{ TDraggedProp --------------------------------------------------------------- }

procedure TDraggedProp.Render;
var
  R: TRectangle;
  C: TCastleColor;
begin
  inherited;

  if (X <> -1) and (Y <> -1) then
  begin
    // TODO: Should use Map.RenderRect for all rendering, not Map.Rect.

    R := Map.Rect.Round;
    RenderContext.ScissorEnable(R);

    if Map.ValidTile(X, Y, nil) then
      C := Vector4Single(0, 1, 0, 1) else
      C := Vector4Single(1, 0, 0, 1);
    C[3] := 0.5;
    Prop.GLImage.Color := C;

    Prop.Draw(Map.GetTileRect(R, X, Y));
    Prop.GLImage.Color := Vector4Single(1, 1, 1, 1);
    RenderContext.ScissorDisable;
  end;
end;

end.
