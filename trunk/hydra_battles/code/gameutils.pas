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

{ Global game utilities and variables. }
unit GameUtils;

interface

uses CastleWindow, CastleConfig,
  CastleTimeUtils, CastleVectors, CastleRectangles, Castle2DSceneManager;

type
  TFaction = (ftHumans, ftMonsters);

  { Directions, corresponding to dirs (from bottom to top) in sprte sheet file. }
  TDirection = (dirSW, dirS,  dirSE, dirE, dirNE, dirN, dirNW, dirW);

var
  GameConf: TCastleConfig;
  Window: TCastleWindowCustom;
  ContainerWidth, ContainerHeight: Integer;
  GameTime: TFloatTime;
  VisualizationSceneManager: T2DSceneManager;

function SideControlWidth: Integer;

const
  FactionName: array [TFaction] of string =
  ('humans', 'monsters');

function FactionFromName(const AName: string): TFaction;

implementation

uses SysUtils;

function SideControlWidth: Integer;
begin
  Result := Round(ContainerWidth * 0.1);
end;

function FactionFromName(const AName: string): TFaction;
begin
  for Result := Low(Result) to High(Result) do
    if FactionName[Result] = AName then
      Exit;
  raise Exception.CreateFmt('"%s" is not a faction name', [AName]);
end;

end.
