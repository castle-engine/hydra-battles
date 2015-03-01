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
  CastleTimeUtils, CastleVectors, CastleRectangles, Castle2DSceneManager,
  CastleColors;

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
  Wood: array [TFaction] of Single;

function PlayerSidebarWidth: Integer;

const
  FactionName: array [TFaction] of string =
  ('humans', 'monsters');

function FactionFromName(const AName: string): TFaction;

function BarRectFromTileRect(const R: TRectangle): TRectangle;

procedure RenderBar(R: TRectangle; const BgColor, FillColor: TCastleColor;
  const Amount: Single);

const
  FactionBarColor: array [TFaction] of TCastleColor = ((0.1, 0.1, 1, 1), (1, 0.1, 0.1, 1));

implementation

uses SysUtils,
  CastleGLUtils, CastleUtils;

function PlayerSidebarWidth: Integer;
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

procedure RenderBar(R: TRectangle; const BgColor, FillColor: TCastleColor;
  const Amount: Single);
begin
  DrawRectangle(R, BgColor);
  R := R.Grow(-2);
  R.Width := Max(0, Round(R.Width * Amount));
  DrawRectangle(R, FillColor);
end;

function BarRectFromTileRect(const R: TRectangle): TRectangle;
begin
  Result.Width := Round(R.Width * 0.6);
  Result.Left := R.Left + (R.Width - Result.Width) div 2;
  Result.Height := Round(R.Height * 0.1);
  Result.Bottom := R.Bottom - Result.Height;
end;

end.
