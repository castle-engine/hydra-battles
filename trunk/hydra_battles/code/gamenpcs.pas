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

{ Game NPCs. }
unit GameNpcs;

interface

uses Classes, FGL,
  CastleConfig, CastleKeysMouse, CastleControls, CastleImages, CastleVectors,
  CastleGLImages, CastleUIControls, CastleTimeUtils, CastleRectangles,
  GamePath, GameAbstractMap, GameUtils;

type
  TNpcType = (npcPeasant, npcWarrior);
  TAnimationType = (atIdle, atWalk, atWalkWood, atAttack, atDie);

  TNpcAnimation = class
    Start, Length, Fps: Cardinal;
    MoveSpeed: Single;
  end;

  TNpc = class
  strict private
    FName: string;
    FNpcType: TNpcType;
    FFaction: TFaction;
    FGLImage: TGLImage;
    Image: TCastleImage;
    TilesX, FTileWidth, FTileHeight: Cardinal;
    FInitialLife: Single;
    AttackConst, AttackRandom: Single;
    FCostWood: Single;
  private
    Animations: array [TAnimationType] of TNpcAnimation;
  public
    property Name: string read FName;
    property NpcType: TNpcType read FNpcType;
    property Faction: TFaction read FFaction;
    property InitialLife: Single read FInitialLife;
    property GLImage: TGLImage read FGLImage;
    property CostWood: Single read FCostWood;
    property TileWidth: Cardinal read FTileWidth;
    property TileHeight: Cardinal read FTileHeight;
    constructor Create(const AFaction: TFaction; const ANpcType: TNpcType);
    destructor Destroy; override;
    procedure GLContextOpen;
    procedure GLContextClose;
  end;

  TNpcs = class
  public
    Npcs: array [TFaction, TNpcType] of TNpc;
    { Create all npcs, reading contents from config file. }
    constructor Create;
    destructor Destroy; override;
    procedure GLContextOpen;
    procedure GLContextClose;
  end;

  TNpcInstance = class
  private
    FNpc: TNpc;
    FPath: TPath;
    FAnimation: TAnimationType;
    FAnimationStart: TFloatTime;
    Direction: TDirection;
    PathProgress: Single;
    procedure SetPath(const Value: TPath);
  public
    { Moves along this path now. }
    Life: Single;
    { Positon on map. Synchronized with Map.MapNpcs. }
    X, Y: Integer;
    property Npc: TNpc read FNpc;
    { Current path of this npc. Asssigning automatically frees previous path.
      Note that this instance owns (will free) the TPath instance. }
    property Path: TPath read FPath write SetPath;
    constructor Create(const ANpc: TNpc; const ADirection: TDirection);
    destructor Destroy; override;
    procedure StartAnimation(const AnimType: TAnimationType);
    procedure Draw(ScreenRectangle: TRectangle);
    { Update, called every frame.
      This is the only place where you can change position (X, Y) of this NPC,
      but inly to places allowed by TValidTileEvent. }
    procedure Update(const SecondsPassed: Single; const Map: TAbstractMap);
  end;

  TNpcInstanceList = specialize TFPGObjectList<TNpcInstance>;

function RandomFaction: TFaction;
function RandomNpcType: TNpcType;
function RandomDirection: TDirection;

implementation

uses SysUtils, Math,
  CastleScene, CastleFilesUtils, CastleSceneCore, CastleGLUtils, CastleWarnings,
  CastleColors, CastleUtils, CastleStringUtils, CastleLog;

const
  NpcName: array [TNpcType] of string =
  ('peasant', 'warrior');
  AnimationName: array [TAnimationType] of string =
  ('idle', 'walk', 'walk_wood', 'attack', 'die');
  AnimationLooping: array [TAnimationType] of boolean =
  (true, true, true, false, false);

  DefaultFps = 8;

function RandomFaction: TFaction;
begin
  Result := TFaction(Random(Ord(High(TFaction)) + 1));
end;

function RandomNpcType: TNpcType;
begin
  Result := TNpcType(Random(Ord(High(TNpcType)) + 1));
end;

function RandomDirection: TDirection;
begin
  Result := TDirection(Random(Ord(High(TDirection)) + 1));
end;

{ TNpc ---------------------------------------------------------------------- }

constructor TNpc.Create(const AFaction: TFaction; const ANpcType: TNpcType);
var
  ConfPath, AnimConfPath: string;
  AnimType: TAnimationType;
  AnimStart, AnimLength: Cardinal;
begin
  inherited Create;
  FFaction := AFaction;
  FNpcType := ANpcType;
  FName := NpcName[NpcType] + ' (' + FactionName[Faction] + ')';
  ConfPath := 'npcs/' + FactionName[Faction] + '/' + NpcName[NpcType];
  Image := LoadImage(GameConf.GetURL(ConfPath + '/url'), []);
  TilesX := GameConf.GetValue(ConfPath + '/tiles_x', 1);
  FInitialLife := GameConf.GetFloat(ConfPath + '/initial_life', 1.0);
  AttackConst := GameConf.GetFloat(ConfPath + '/attack_const', 0.0);
  AttackRandom := GameConf.GetFloat(ConfPath + '/attack_random', 0.0);
  FCostWood := GameConf.GetFloat(ConfPath + '/cost_wood', 0.0);

  FTileWidth := Image.Width div TilesX;
  if Image.Width mod TilesX <> 0 then
    OnWarning(wtMinor, 'Npc', Format('Npc "%s" image width %d does not exactly divide into tiles_x %d',
      [Name, Image.Width, TilesX]));
  FTileHeight := Image.Height div 8;
  if Image.Height mod 8 <> 0 then
    OnWarning(wtMinor, 'Npc', Format('Npc "%s" image height %d does not exactly divide into 8 directions',
      [Name, Image.Height]));
  WritelnLog('Npc', Format('Npc "%s" tile size %d x %d', [Name, TileWidth, TileHeight]));

  for AnimType := Low(AnimType) to High(AnimType) do
  begin
    AnimConfPath := ConfPath + '/' + AnimationName[AnimType];
    AnimStart := GameConf.GetValue(AnimConfPath + '/anim_start', 0);
    AnimLength := GameConf.GetValue(AnimConfPath + '/anim_length', 0);
    if (AnimStart <> 0) and (AnimLength <> 0) then
    begin
      Animations[AnimType] := TNpcAnimation.Create;
      Animations[AnimType].Start := AnimStart;
      Animations[AnimType].Length := AnimLength;
      Animations[AnimType].Fps := GameConf.GetValue(AnimConfPath + '/fps', DefaultFps);
      Animations[AnimType].MoveSpeed := GameConf.GetFloat(AnimConfPath + '/move_speed', 0.0);
      WritelnLog('Npc', Format('Npc "%s" has animation "%s" with %d, %d', [Name, AnimationName[AnimType], AnimStart, AnimLength]));
    end;
  end;
end;

destructor TNpc.Destroy;
var
  AnimType: TAnimationType;
begin
  GLContextClose;
  FreeAndNil(Image);
  for AnimType := Low(AnimType) to High(AnimType) do
    FreeAndNil(Animations[AnimType]);
  inherited;
end;

procedure TNpc.GLContextOpen;
begin
  if FGLImage = nil then
    FGLImage := TGLImage.Create(Image, true);
end;

procedure TNpc.GLContextClose;
begin
  FreeAndNil(FGLImage);
end;

{ TNpcs ------------------------------------------------------------------ }

constructor TNpcs.Create;
var
  F: TFaction;
  NT: TNpcType;
begin
  inherited;
  for F := Low(F) to High(F) do
    for NT := Low(NT) to High(NT) do
      Npcs[F, NT] := TNpc.Create(F, NT);
end;

destructor TNpcs.Destroy;
var
  F: TFaction;
  NT: TNpcType;
begin
  GLContextClose;
  for F := Low(F) to High(F) do
    for NT := Low(NT) to High(NT) do
      FreeAndNil(Npcs[F, NT]);
  inherited;
end;

procedure TNpcs.GLContextOpen;
var
  F: TFaction;
  NT: TNpcType;
begin
  for F := Low(F) to High(F) do
    for NT := Low(NT) to High(NT) do
      Npcs[F, NT].GLContextOpen;
end;

procedure TNpcs.GLContextClose;
var
  F: TFaction;
  NT: TNpcType;
begin
  for F := Low(F) to High(F) do
    for NT := Low(NT) to High(NT) do
      if Npcs[F, NT] <> nil then
        Npcs[F, NT].GLContextClose;
end;

{ TNpcInstance --------------------------------------------------------------- }

constructor TNpcInstance.Create(const ANpc: TNpc; const ADirection: TDirection);
begin
  inherited Create;
  FNpc := ANpc;
  Life := Npc.InitialLife;
  Direction := ADirection;
  StartAnimation(atIdle);
end;

destructor TNpcInstance.Destroy;
begin
  FreeAndNil(FPath);
  inherited;
end;

procedure TNpcInstance.StartAnimation(const AnimType: TAnimationType);
begin
  if Npc.Animations[AnimType] = nil then
    raise EInternalError.CreateFmt('Npc %s does not have animation %s', [Npc.Name, AnimationName[AnimType]]);
  FAnimation := AnimType;
  FAnimationStart := GameTime;
end;

procedure TNpcInstance.Draw(ScreenRectangle: TRectangle);
var
  AnimFrame: Integer;
  AnimLength: Cardinal;
  ImageX, ImageY: Integer;
  Diff, Shift: TVector2Single;
begin
  AnimLength := Npc.Animations[FAnimation].Length;
  AnimFrame := Trunc(Npc.Animations[FAnimation].Fps * (GameTime - FAnimationStart));
  if AnimationLooping[FAnimation] then
    AnimFrame := AnimFrame mod AnimLength else
    Clamp(AnimFrame, 0, AnimLength - 1);
  ImageX := (AnimFrame + (Npc.Animations[FAnimation].Start - 1)) * Npc.TileWidth;
  ImageY := Ord(Direction) * Npc.TileHeight;

  Shift := ZeroVector2Single;
  if (FPath <> nil) and (FPath.Count > 1) and (Trunc(PathProgress) < FPath.Count - 1) then
  begin
    Diff := FPath.PointsVector(Trunc(PathProgress), Trunc(PathProgress) + 1);
    Shift := Diff * Frac(PathProgress);
  end;
  ScreenRectangle.Left += Round(Shift[0]);
  ScreenRectangle.Bottom += Round(Shift[1]);

  Npc.GLImage.Draw(ScreenRectangle, ImageX, ImageY, Npc.TileWidth, Npc.TileHeight);
end;

procedure TNpcInstance.SetPath(const Value: TPath);
begin
  if FPath <> Value then
  begin
    if FPath <> nil then
      FreeAndNil(FPath);
    FPath := Value;
    PathProgress := 0.0;
    if FPath <> nil then
      StartAnimation(atWalk) else
      StartAnimation(atIdle);
  end;
end;

procedure TNpcInstance.Update(const SecondsPassed: Single; const Map: TAbstractMap);

  { update Direction now, if moving along the path }
  procedure UpdateDirection;
  var
    PreviousPointIndex, NextPointIndex: Integer;
    Dir: TDirection;
  begin
    PreviousPointIndex := Clamped(Trunc(PathProgress), 0, FPath.Count - 1);
    NextPointIndex := Clamped(Trunc(PathProgress) + 1, 0, FPath.Count - 1);
    if PreviousPointIndex < NextPointIndex then
    begin
      if Map.Neighbors(FPath[PreviousPointIndex], FPath[NextPointIndex], Dir) then
        Direction := Dir;
    end;
  end;

var
  OldPathProgress: Single;
  NextPoint: TVector2SmallInt;
  NextPointIndex: Integer;
begin
  if (FPath <> nil) and (FPath.Count > 1) then
  begin
    OldPathProgress := PathProgress;
    PathProgress += Npc.Animations[FAnimation].MoveSpeed * SecondsPassed;

    if Trunc(OldPathProgress) <> Trunc(PathProgress) then
    begin
      NextPointIndex := Clamped(Trunc(PathProgress), 0, FPath.Count - 1);
      NextPoint := FPath[NextPointIndex];
      if (NextPoint[0] <> X) or (NextPoint[1] <> Y) then
      begin
        if Map.ValidTile(NextPoint[0], NextPoint[1], Self) then
        begin
          X := NextPoint[0];
          Y := NextPoint[1];
          if NextPointIndex = FPath.Count - 1 then
            Path := nil; // end of path
        end else
          Path := nil;
      end;
    end;
  end;

  if (FPath <> nil) and (FPath.Count > 1) then
    UpdateDirection;
end;

end.
