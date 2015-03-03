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

{$modeswitch nestedprocvars}{$H+}

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
    FAttackConst, FAttackRandom: Single;
    FCostWood: Cardinal;
    FWantsToAttack: TWantsToAttack;
  private
    Animations: array [TAnimationType] of TNpcAnimation;
  public
    property Name: string read FName;
    property NpcType: TNpcType read FNpcType;
    property Faction: TFaction read FFaction;
    property InitialLife: Single read FInitialLife;
    property GLImage: TGLImage read FGLImage;
    property CostWood: Cardinal read FCostWood;
    property TileWidth: Cardinal read FTileWidth;
    property TileHeight: Cardinal read FTileHeight;
    property WantsToAttack: TWantsToAttack read FWantsToAttack;
    property AttackConst: Single read FAttackConst;
    property AttackRandom: Single read FAttackRandom;
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
    AttackTarget: TVector2SmallInt;
    FLife: Single;
    procedure SetPath(const Value: TPath);
    function TryAttacking(const Map: TAbstractMap): boolean;
    procedure SetLife(const Value: Single);
    function CurrentAnimationFinished: boolean;
    procedure SetAnimation(const Value: TAnimationType);
  public
    { Positon on map. Synchronized with Map.MapNpcs. }
    X, Y: Integer;
    property Animation: TAnimationType read FAnimation write SetAnimation;
    property Npc: TNpc read FNpc;
    property Life: Single read FLife write SetLife;
    { Current path of this npc. Asssigning automatically frees previous path.
      Note that this instance owns (will free) the TPath instance. }
    property Path: TPath read FPath write SetPath;
    constructor Create(const ANpc: TNpc; const ADirection: TDirection);
    destructor Destroy; override;
    procedure Draw(ScreenRectangle: TRectangle);
    { Update, called every frame.
      This is the only place where you can change position (X, Y) of this NPC,
      but inly to places allowed by TValidTileEvent. }
    procedure Update(const SecondsPassed: Single; const Map: TAbstractMap);
    function RemoveFromMap: boolean;
  end;

  TNpcInstanceList = specialize TFPGObjectList<TNpcInstance>;

function RandomFaction: TFaction;
function RandomNpcType: TNpcType;
function RandomDirection: TDirection;

function NpcTypeFromName(const AName: string): TNpcType;

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

function NpcTypeFromName(const AName: string): TNpcType;
begin
  for Result := Low(Result) to High(Result) do
    if NpcName[Result] = AName then
      Exit;
  raise Exception.CreateFmt('"%s" is not an npc name', [AName]);
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
  FAttackConst := GameConf.GetFloat(ConfPath + '/attack_const', 0.0);
  FAttackRandom := GameConf.GetFloat(ConfPath + '/attack_random', 0.0);
  FCostWood := GameConf.GetValue(ConfPath + '/cost_wood', 0);

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

  FWantsToAttack := waNone;
  case NpcType of
    npcPeasant: FWantsToAttack := waTrees;
    npcWarrior:
      case Faction of
        ftHumans: FWantsToAttack := waMonsters;
        ftMonsters: FWantsToAttack := waHumans;
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
  Animation := atIdle;
end;

destructor TNpcInstance.Destroy;
begin
  FreeAndNil(FPath);
  inherited;
end;

procedure TNpcInstance.SetAnimation(const Value: TAnimationType);
begin
  if FAnimation <> Value then
  begin
    if Npc.Animations[Value] = nil then
      raise EInternalError.CreateFmt('Npc %s does not have animation %s',
        [Npc.Name, AnimationName[Value]]);
      FAnimation := Value;
    FAnimation := Value;
    FAnimationStart := GameTime;
  end;
end;

procedure TNpcInstance.Draw(ScreenRectangle: TRectangle);
var
  AnimFrame: Integer;
  AnimLength: Cardinal;
  ImageX, ImageY: Integer;
  Diff, Shift: TVector2Single;
  NewRectHeight: Integer;
  BarRect: TRectangle;
begin
  BarRect := BarRectFromTileRect(ScreenRectangle);

  { for NPC, rectangle to draw should uniform }
  NewRectHeight := ScreenRectangle.Width;
  ScreenRectangle.Bottom -= (NewRectHeight - ScreenRectangle.Height) div 2; // keep centered
  ScreenRectangle.Height := NewRectHeight;

  if Npc.Animations[FAnimation] = nil then
    raise Exception.CreateFmt('Missing animation "%s" on "%s" of faction "%s"',
      [AnimationName[FAnimation], Npc.Name, FactionName[Npc.Faction]]);

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
  BarRect.Left += Round(Shift[0]);
  BarRect.Bottom += Round(Shift[1]);

  Npc.GLImage.Draw(ScreenRectangle, ImageX, ImageY, Npc.TileWidth, Npc.TileHeight);

  RenderBar(BarRect, Black, FactionBarColor[Npc.Faction], Life / Npc.InitialLife);
end;

procedure TNpcInstance.SetPath(const Value: TPath);
begin
  if FPath <> Value then
  begin
    if FPath <> nil then
      FreeAndNil(FPath);
    FPath := Value;
    PathProgress := 0.0;
  end;
end;

function TNpcInstance.TryAttacking(const Map: TAbstractMap): boolean;

  procedure TestTryAttacking(const AttackX, AttackY: Integer;
    var ContinueToNeighbors: boolean);
  var
    Dir: TDirection;
  begin
    if Map.CanAttack(AttackX, AttackY, Npc.WantsToAttack) then
    begin
      AttackTarget := Vector2SmallInt(AttackX, AttackY);
      { update Direction to attack target }
      if Neighbors(X, Y, AttackX, AttackY, Dir) then
        Direction := Dir;
      Animation := atAttack;
      TryAttacking := true;
      ContinueToNeighbors := false;
    end;
  end;

begin
  HandleNeighbors(X, Y, @TestTryAttacking);
end;

function TNpcInstance.CurrentAnimationFinished: boolean;
var
  AnimLength, AnimFrame: Integer;
begin
  AnimLength := Npc.Animations[FAnimation].Length;
  AnimFrame := Trunc(Npc.Animations[FAnimation].Fps * (GameTime - FAnimationStart));
  Result := AnimFrame >= AnimLength;
end;

procedure TNpcInstance.Update(const SecondsPassed: Single; const Map: TAbstractMap);

  procedure TryFinishAttack;
  begin
    if CurrentAnimationFinished then
    begin
      if Map.CanAttack(AttackTarget[0], AttackTarget[1], Npc.WantsToAttack) then
        Map.Attack(Npc.Faction, AttackTarget[0], AttackTarget[1], Npc.AttackConst + Random * Npc.AttackRandom);
      Animation := atIdle;
    end;
  end;

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
      if Neighbors(FPath[PreviousPointIndex], FPath[NextPointIndex], Dir) then
        Direction := Dir;
    end;
  end;

var
  OldPathProgress: Single;
  NextPoint: TVector2SmallInt;
  NextPointIndex: Integer;
begin
  if FAnimation = atDie then Exit;

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
        end else
          Path := nil;
      end;
    end;
  end;

  if (FPath <> nil) and (FPath.Count > 1) then
    UpdateDirection;

  if (FPath <> nil) and
     (FPath.Count > 0) and
     (PathProgress > FPath.Count - 1) then
    Path := nil; { path ended }

  if FAnimation = atAttack then
    TryFinishAttack;

  if FPath = nil then
    if FAnimation <> atAttack then
      TryAttacking(Map);

  if FAnimation <> atAttack then // do not override attack
    if (FPath <> nil) and (FPath.Count > 1) then
      Animation := atWalk else
      Animation := atIdle;
end;

procedure TNpcInstance.SetLife(const Value: Single);
begin
  FLife := Value;
  if FLife <= 0 then
    Animation := atDie;
end;

function TNpcInstance.RemoveFromMap: boolean;
begin
  Result := (FAnimation = atDie) and  CurrentAnimationFinished;
end;

end.
