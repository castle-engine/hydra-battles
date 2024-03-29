{
  Copyright 2015-2022 Michalis Kamburelis.

  This file is part of "Hydra Battles".

  "Hydra Battles" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Hydra Battles" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$modeswitch nestedprocvars}{$H+}

{ Game map. }
unit GameMap;

interface

uses Classes,
  CastleConfig, CastleKeysMouse, CastleControls, CastleImages, CastleVectors,
  CastleGLImages, CastleUIControls, CastleRectangles,
  GameNpcs, GameAbstractMap, GamePath, GameProps, GameUtils;

type
  { Render map background. }
  TMapBackground = class(TCastleImageControl)
  public
    constructor Create(const AName: string); reintroduce;
  end;

  TMap = class(TAbstractMap)
  strict private
    FProps: TProps;
    FNpcs: TNpcs;
    FInitialWood: Cardinal;
    { Possible props on map. }
    property Props: TProps read FProps;
    //property Npcs: TNpcs read FNpcs;
  public
    { Props on map. }
    MapProps: array of array of TPropInstance;
    { Npcs on map. }
    MapNpcs: array of array of TNpcInstance;
    PropInstances: TPropInstanceList;
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
    procedure SetPropInstance(const X, Y: Integer; const NewPropInstance: TPropInstance);
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function CanAttack(const X, Y: Integer; const WantsToAttack: TWantsToAttack): boolean; override;
    procedure Attack(const Attacker: TFaction; const X, Y: Integer; const Damage: Single); override;
    property InitialWood: Cardinal read FInitialWood;
  end;

implementation

uses SysUtils,
  CastleScene, CastleFilesUtils, CastleGLUtils,
  CastleUtils, CastleStringUtils, CastleLog, CastleRenderContext;

{ TMapBackground ------------------------------------------------------------- }

constructor TMapBackground.Create(const AName: string);
begin
  inherited Create(nil);
  Name := AName;
  Url := GameConf.GetURL('maps/' + AName + '/background');
  Stretch := true;
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

  NpcInstances := TNpcInstanceList.Create;
  PropInstances := TPropInstanceList.Create;

  SetLength(MapProps, Width, Height);
  SetLength(MapNpcs, Width, Height);

  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
    begin
      PropName := GameConf.GetValue(Format(ConfPath + '/tile_%d_%d/name', [X, Y]), '');
      if PropName = '' then
        MapProps[X, Y] := nil else
        SetPropInstance(X, Y, TPropInstance.Create(Props[PropTypeFromName(PropName)]));
    end;

  FInitialWood := GameConf.GetValue(ConfPath + '/initial/wood', 0);
end;

destructor TMap.Destroy;
begin
  FreeAndNil(NpcInstances);
  FreeAndNil(PropInstances);
  { just ignore MapNpcs and MapProps contents, they are invalid now, everything is freed }
  inherited;
end;

procedure TMap.Render;
var
  R: TRectangle;

  procedure DrawLightCursor(const X, Y: Integer;
    var ContinueToNeighbors: boolean);
  var
    TileRect: TRectangle;
  begin
    TileRect := GetTileRect(R, X, Y);
    Props[ptCursorLight].Draw(TileRect);
  end;

var
  TileRect: TRectangle;
  X, Y: Integer;
begin
  inherited;
  { Map width, height assuming that tile width = 1.0. }

  R := RenderRect.Round;

  RenderContext.ScissorEnable(R);

  if Grid then
    for Y := Height - 1 downto 0 do
      for X := 0 to Width - 1 do
      begin
        TileRect := GetTileRect(R, X, Y);
        Props[ptTileFrame].Draw(TileRect);
      end;

  for Y := Height - 1 downto 0 do
    for X := 0 to Width - 1 do
    begin
      TileRect := GetTileRect(R, X, Y);
      if MapProps[X, Y] <> nil then
        MapProps[X, Y].Draw(TileRect);
      if MapNpcs[X, Y] <> nil then
        MapNpcs[X, Y].Draw(TileRect);
    end;

  if EditMode then
  begin
    TileRect := GetTileRect(R, EditCursor[0], EditCursor[1]);
    Props[ptCursor].Draw(TileRect);
    { Show neighbors, to test our neighbor logic is Ok. }
    HandleNeighbors(EditCursor[0], EditCursor[1], @DrawLightCursor);
  end;

  RenderContext.ScissorDisable;
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
        PropName := MapProps[X, Y].Prop.Name else
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
  { prevent moving/building outside the map, and also right on the border
    of the map (that is partially not rendered) }
  if not (
      (X > 0) and (X < Width  - 1) and
      (Y > 0) and (Y < Height - 1)) then
    Exit(false);

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

function TMap.CanAttack(const X, Y: Integer; const WantsToAttack: TWantsToAttack): boolean;
begin
  if not ValidCoord(X, Y) then Exit(false);
  case WantsToAttack of
    waHumans  :
      begin
        if (MapNpcs[X, Y] <> nil) and
           (MapNpcs[X, Y].Life > 0) and
           (MapNpcs[X, Y].Npc.Faction = ftHumans) then
          Exit(true);
        if (MapProps[X, Y] <> nil) and
           (not MapProps[X, Y].Prop.Neutral) and
           (MapProps[X, Y].Prop.Faction = ftHumans) then
          Exit(true);
      end;
    waMonsters:
      begin
        if (MapNpcs[X, Y] <> nil) and
           (MapNpcs[X, Y].Life > 0) and
           (MapNpcs[X, Y].Npc.Faction = ftMonsters) then
          Exit(true);
        if (MapProps[X, Y] <> nil) and
           (not MapProps[X, Y].Prop.Neutral) and
           (MapProps[X, Y].Prop.Faction = ftMonsters) then
          Exit(true);
      end;
    waTrees:
      if (MapProps[X, Y] <> nil) and
         (MapProps[X, Y].Prop.PropType in Trees) then
        Exit(true);
  end;
  Result := false;
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

procedure TMap.SetPropInstance(const X, Y: Integer; const NewPropInstance: TPropInstance);
var
  OldPropInstance: TPropInstance;
begin
  OldPropInstance := MapProps[X, Y];
  if OldPropInstance <> nil then
  begin
    MapProps[X, Y] := nil;
    PropInstances.Remove(OldPropInstance); // frees OldPropInstance
    // OldPropInstance := nil; // useless
  end;

  Assert(MapProps[X, Y] = nil);

  if NewPropInstance <> nil then
  begin
    PropInstances.Add(NewPropInstance);
    MapProps[X, Y] := NewPropInstance;
    NewPropInstance.X := X;
    NewPropInstance.Y := Y;
  end;
end;

procedure TMap.Update(const SecondsPassed: Single; var HandleInput: boolean);

  procedure UpdateNpcs;
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

  procedure PackNpcs;
  var
    J: Integer;
    NI: TNpcInstance;
  begin
    J := 0;
    while J < NpcInstances.Count do
    begin
      if NpcInstances[J].RemoveFromMap then
      begin
        NI := NpcInstances[J];
        MapNpcs[NI.X, NI.Y] := nil;
        NpcInstances.Delete(J);
      end else
        Inc(J);
    end;
  end;

  procedure UpdateProps;
  var
    I: Integer;
    SpawnNpc: TNpc;
    SpawnX, SpawnY: Integer;
    SpawnDir: TDirection;
  begin
    for I := 0 to PropInstances.Count - 1 do
    begin
      PropInstances[I].Update(Self, SpawnNpc, SpawnX, SpawnY);
      if SpawnNpc <> nil then
      begin
        if not Neighbors(PropInstances[I].X, PropInstances[I].Y, SpawnX, SpawnY, SpawnDir) then
          SpawnDir := RandomDirection;
        SetNpcInstance(SpawnX, SpawnY, TNpcInstance.Create(SpawnNpc, SpawnDir));
      end;
    end;
  end;

begin
  UpdateNpcs;
  PackNpcs;
  UpdateProps;
end;

procedure TMap.Attack(const Attacker: TFaction; const X, Y: Integer; const Damage: Single);
begin
  if (MapProps[X, Y] <> nil) and
     (MapProps[X, Y].Prop.InitialLife <> 0) then
  begin
    MapProps[X, Y].Life -= Damage;
    if MapProps[X, Y].Life <= 0 then
    begin
      Wood[Attacker] += MapProps[X, Y].Prop.RewardWood;
      // TODO: show reward as drop
      { just destroy immediately. Don't worry that this removes from PropInstances list,
        this can be called only while iterating over npcs. }
      SetPropInstance(X, Y, nil);
    end;
  end else

  if MapNpcs[X, Y] <> nil then
    // TODO: show damage as drop
    MapNpcs[X, Y].Life := MapNpcs[X, Y].Life - Damage;
end;

end.
