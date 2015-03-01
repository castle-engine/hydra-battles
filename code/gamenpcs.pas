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
  GamePath;

type
  TNpcType = (npcPeasant, npcWarrior);
  TFaction = (ftHumans, ftMonsters);
  TAnimationType = (atIdle, atWalk, atWalkWood, atAttack, atDie);

  TNpcAnimation = class
    Start, Length, Fps: Cardinal;
    MoveSpeed: Single;
  end;

  TNpc = class
  private
    FName: string;
    FNpcType: TNpcType;
    FFaction: TFaction;
    FGLImage: TGLImage;
    Image: TCastleImage;
    TilesX, TileWidth, TileHeight: Cardinal;
    FInitialLife: Single;
    Animations: array [TAnimationType] of TNpcAnimation;
  public
    property Name: string read FName;
    property NpcType: TNpcType read FNpcType;
    property Faction: TFaction read FFaction;
    property InitialLife: Single read FInitialLife;
    property GLImage: TGLImage read FGLImage;
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

  { Directions, corresponding to dirs (from bottom to top) in sprte sheet file. }
  TDirection = (dirSW, dirS,  dirSE, dirE, dirNE, dirN, dirNW, dirW);

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
    property Npc: TNpc read FNpc;
    { Current path of this npc. Asssigning automatically frees previous path.
      Note that this instance owns (will free) the TPath instance. }
    property Path: TPath read FPath write SetPath;
    constructor Create(const ANpc: TNpc; const ADirection: TDirection);
    destructor Destroy; override;
    procedure StartAnimation(const AnimType: TAnimationType);
    procedure Draw(const ScreenRectangle: TRectangle);
    procedure Update(const SecondsPassed: Single);
  end;

  TNpcInstanceList = specialize TFPGObjectList<TNpcInstance>;

function RandomFaction: TFaction;
function RandomNpcType: TNpcType;
function RandomDirection: TDirection;

implementation

uses SysUtils, Math,
  CastleScene, CastleFilesUtils, CastleSceneCore, CastleGLUtils, CastleWarnings,
  CastleColors, CastleUtils, CastleStringUtils, CastleLog,
  GameUtils;

const
  NpcName: array [TNpcType] of string =
  ('peasant', 'warrior');
  FactionName: array [TFaction] of string =
  ('humans', 'monsters');
  AnimationName: array [TAnimationType] of string =
  ('idle', 'walk', 'walk_wood', 'attack', 'die');
  AnimationLooping: array [TAnimationType] of boolean =
  (true, true, true, false, false);

  DefaultFps = 4;

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

  TileWidth := Image.Width div TilesX;
  if Image.Width mod TilesX <> 0 then
    OnWarning(wtMinor, 'Npc', Format('Npc "%s" image width %d does not exactly divide into tiles_x %d',
      [Name, Image.Width, TilesX]));
  TileHeight := Image.Height div 8;
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

procedure TNpcInstance.Draw(const ScreenRectangle: TRectangle);
var
  AnimFrame: Integer;
  AnimLength: Cardinal;
  ImageX, ImageY: Integer;
begin
  AnimLength := Npc.Animations[FAnimation].Length;
  AnimFrame := Trunc(Npc.Animations[FAnimation].Fps * (GameTime - FAnimationStart));
  if AnimationLooping[FAnimation] then
    AnimFrame := AnimFrame mod AnimLength else
    Clamp(AnimFrame, 0, AnimLength - 1);
  ImageX := (AnimFrame + (Npc.Animations[FAnimation].Start - 1)) * Npc.TileWidth;
  ImageY := Ord(Direction) * Npc.TileHeight;
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
    // TODO: move along path, from TMap.Update
  end;
end;

// TODO: call this
procedure TNpcInstance.Update(const SecondsPassed: Single);

  { update Direction now, if moving along the path }
  procedure UpdateDirection;
  var
    PreviousPointIndex, NextPointIndex: Integer;
    Diff: TVector2Single;
    Angle: Float;
  begin
    PreviousPointIndex := Max(Trunc(PathProgress), 0);
    NextPointIndex := Min(FPath.Count - 1, Trunc(PathProgress) + 1);
    Diff := FPath.PointsVector(PreviousPointIndex, NextPointIndex);
    Angle := ArcTan2(Diff[1], Diff[0]);
    // TODO: update Direction from Angle
  end;

begin
  if (FPath <> nil) and (FPath.Count <> 0) then
    UpdateDirection;
end;

end.
