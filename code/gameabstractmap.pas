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

{ Abstract map, defines map and tile sizing algorithm. }
unit GameAbstractMap;

interface

uses CastleVectors, CastleUIControls, CastleRectangles,
  GameUtils;

const
  { Aspect ratio of rendered tile. }
  TileWidthToHeight = 64 / 36;

type
  TAbstractMap = class(TCastleUserInterface)
  private
    FWidth, FHeight: Cardinal;
  public
    constructor Create(const AWidth, AHeight: Cardinal); reintroduce;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    function ValidCoord(const X, Y: Integer): boolean;
    { Get rectangle of given tile, assuming that map fits given MapRect.
      MapRect must always be equal the return value of @link(Rect) method,
      it is taken here only for optimization. }
    function GetTileRect(const MapRect: TRectangle; const X, Y: Integer): TRectangle;
    { Convert screen position to tile (returns false if outside the map. }
    function PositionToTile(const MapRect: TFloatRectangle;
      ScreenPosition: TVector2; out X, Y: Integer): boolean;
    { Can you place an NPC or a prop on this tile.
      This should be used to test can you place anything on a tile during a game
      (during editing, when placing stuff in editor mode, rules are somewhat
      more liberal, and this method shouldn't be used). }
    function ValidTile(const X, Y: Integer; const OmitNpcInstance: TObject): boolean; virtual; abstract;
    function CanAttack(const X, Y: Integer; const WantsToAttack: TWantsToAttack): boolean; virtual; abstract;
    procedure Attack(const Attacker: TFaction; const X, Y: Integer; const Damage: Single); virtual; abstract;
  end;

type
  TTileHandler = procedure (const X, Y: Integer;
    var ContinueToNeighbors: boolean) is nested;

{ Call Handler for all neighbors of tile X, Y.
  Note that this doesn't look at map validity or even size,
  so be sure to check X, Y inside. }
procedure HandleNeighbors(const X, Y: Integer; const Handler: TTileHandler);

function Neighbors(const X1, Y1, X2, Y2: Cardinal): boolean;
function Neighbors(const X1, Y1, X2, Y2: Cardinal;  out Dir: TDirection): boolean;
function Neighbors(const P1, P2: TVector2SmallInt;  out Dir: TDirection): boolean;

implementation

uses Math,
  CastleUtils;

constructor TAbstractMap.Create(const AWidth, AHeight: Cardinal);
begin
  inherited Create(nil);
  FWidth := AWidth;
  FHeight := AHeight;
  FullSize := true;
  Border.Left := PlayerSidebarWidth;
  Border.Right := PlayerSidebarWidth;
end;

function TAbstractMap.GetTileRect(const MapRect: TRectangle; const X, Y: Integer): TRectangle;
var
  TileW, TileH: Single;
begin
  TileW := MapRect.Width / (Width - 1);
  TileH := TileW / TileWidthToHeight;

  Result.Left := Round(MapRect.Left + X * TileW);
  if not Odd(Y) then Result.Left -= Round(TileW / 2);
  Result.Bottom := Round(MapRect.Bottom + (Y - 1) * TileH / 2);
  Result.Width := Ceil(TileW);
  Result.Height := Ceil(TileH);
end;

function TAbstractMap.PositionToTile(const MapRect: TFloatRectangle;
  ScreenPosition: TVector2; out X, Y: Integer): boolean;
var
  TileW, TileH: Single;
  ScreenPositionFrac: TVector2;
  EvenRow: boolean;
begin
  if not MapRect.Contains(ScreenPosition) then
    Exit(false);

  TileW := MapRect.Width / (Width - 1);
  TileH := TileW / TileWidthToHeight;

  ScreenPosition.X := (ScreenPosition.X - MapRect.Left  ) / TileW;
  ScreenPosition.Y := (ScreenPosition.Y - MapRect.Bottom) / TileH;
  ScreenPositionFrac.X := Frac(ScreenPosition.X);
  ScreenPositionFrac.Y := Frac(ScreenPosition.Y);
  if ScreenPositionFrac.Y < 0.5 then
  begin
    if ScreenPositionFrac.X < 0.5 then
      EvenRow := PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0, 0)) <
                 PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0.5, 0.5)) else
      EvenRow := PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(1, 0)) <
                 PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0.5, 0.5));
    if EvenRow then
    begin
      if ScreenPositionFrac.X < 0.5 then
        X := Trunc(ScreenPosition.X) else
        X := Trunc(ScreenPosition.X) + 1;
      Y := Trunc(ScreenPosition.Y) * 2;
    end else
    begin
      X := Trunc(ScreenPosition.X);
      Y := Trunc(ScreenPosition.Y) * 2 + 1;
    end;
  end else
  begin
    if ScreenPositionFrac.X < 0.5 then
      EvenRow := PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0, 1)) <
                 PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0.5, 0.5)) else
      EvenRow := PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(1, 1)) <
                 PointsDistanceSqr(Vector2(ScreenPositionFrac.X, ScreenPositionFrac.Y), Vector2(0.5, 0.5));
    if EvenRow then
    begin
      if ScreenPositionFrac.X < 0.5 then
        X := Trunc(ScreenPosition.X) else
        X := Trunc(ScreenPosition.X) + 1;
      Y := Trunc(ScreenPosition.Y) * 2 + 2;
    end else
    begin
      X := Trunc(ScreenPosition.X);
      Y := Trunc(ScreenPosition.Y) * 2 + 1;
    end;
  end;

  Result := ValidCoord(X, Y);
end;

function TAbstractMap.ValidCoord(const X, Y: Integer): boolean;
begin
  Result :=
    (X >= 0) and (X < Width) and
    (Y >= 0) and (Y < Height);
end;

{ global routines ------------------------------------------------------------ }

function Neighbors(const X1, Y1, X2, Y2: Cardinal;
  out Dir: TDirection): boolean;
begin
  Result := false;
  if                    ((X1 + 1 = X2) and (Y1     = Y2)) then
    begin Result := true; Dir := dirE; end else
  if                    ((X1 - 1 = X2) and (Y1     = Y2)) then
    begin Result := true; Dir := dirW; end else
  if                    ((X1     = X2) and (Y1 + 1 = Y2)) then
    begin Result := true; if Odd(Y1) then Dir := dirNW else Dir := dirNE; end else
  if                    ((X1     = X2) and (Y1 - 1 = Y2)) then
    begin Result := true; if Odd(Y1) then Dir := dirSW else Dir := dirSE; end else
  if                    ((X1     = X2) and (Y1 + 2 = Y2)) then
    begin Result := true; Dir := dirN; end else
  if                    ((X1     = X2) and (Y1 - 2 = Y2)) then
    begin Result := true; Dir := dirS; end else
  if  (     Odd(Y1)  and (X1 + 1 = X2) and (Y1 - 1 = Y2)) then
    begin Result := true; Dir := dirSE; end else
  if  (     Odd(Y1)  and (X1 + 1 = X2) and (Y1 + 1 = Y2)) then
    begin Result := true; Dir := dirNE; end else
  if  ((not Odd(Y1)) and (X1 - 1 = X2) and (Y1 - 1 = Y2)) then
    begin Result := true; Dir := dirSW; end else
  if  ((not Odd(Y1)) and (X1 - 1 = X2) and (Y1 + 1 = Y2)) then
    begin Result := true; Dir := dirNW; end;
end;

function Neighbors(const P1, P2: TVector2SmallInt; out Dir: TDirection): boolean;
begin
  Result := Neighbors(P1.X, P1.Y, P2.X, P2.Y, Dir);
end;

function Neighbors(const X1, Y1, X2, Y2: Cardinal): boolean;
var
  Dir: TDirection;
begin
  Result := Neighbors(X1, Y1, X2, Y2, Dir);
end;

procedure HandleNeighbors(const X, Y: Integer; const Handler: TTileHandler);
var
  ContinueToNeighbors: boolean;
begin
  ContinueToNeighbors := true;
  Handler(X + 1, Y    , ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  Handler(X - 1, Y    , ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  Handler(X    , Y + 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  Handler(X    , Y - 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  Handler(X    , Y + 2, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  Handler(X    , Y - 2, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  if Odd(Y) then
  begin
    Handler(X + 1, Y - 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
    Handler(X + 1, Y + 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  end;
  if not Odd(Y) then
  begin
    Handler(X - 1, Y - 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
    Handler(X - 1, Y + 1, ContinueToNeighbors); if not ContinueToNeighbors then Exit;
  end;
end;

end.
