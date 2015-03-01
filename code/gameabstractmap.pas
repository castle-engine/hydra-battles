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

{ Abstract map, defines map and tile sizing algorithm. }
unit GameAbstractMap;

interface

uses CastleVectors, Castle2DSceneManager, CastleUIControls, CastleRectangles,
  GameUtils;

const
  { Aspect ratio of rendered tile. }
  TileWidthToHeight = 64 / 36;

type
  TAbstractMap = class(TUIControl)
  private
    FWidth, FHeight: Cardinal;
  public
    constructor Create(const AWidth, AHeight: Cardinal); reintroduce;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    function Rect: TRectangle;
    { Get rectangle of given tile, assuming that map fits given MapRect.
      MapRect must always be equal the return value of @link(Rect) method,
      it is taken here only for optimization. }
    function GetTileRect(const MapRect: TRectangle; const X, Y: Integer): TRectangle;
    { Convert screen position to tile (returns false if outside the map. }
    function PositionToTile(const MapRect: TRectangle;
      ScreenPosition: TVector2Single; out X, Y: Integer): boolean;
    function Neighbors(const X1, Y1, X2, Y2: Cardinal): boolean;
    function Neighbors(const X1, Y1, X2, Y2: Cardinal;  out Dir: TDirection): boolean;
    function Neighbors(const P1, P2: TVector2SmallInt;  out Dir: TDirection): boolean;
    function ValidTile(const X, Y: Integer; const OmitNpcInstance: TObject): boolean; virtual; abstract;
    function CanAttack(const X, Y: Integer; const WantsToAttack: TWantsToAttack): boolean; virtual; abstract;
    procedure Attack(const Attacker: TFaction; const X, Y: Integer; const Damage: Single); virtual; abstract;
  end;

implementation

uses Math,
  CastleUtils;

constructor TAbstractMap.Create(const AWidth, AHeight: Cardinal);
begin
  inherited Create(nil);
  FWidth := AWidth;
  FHeight := AHeight;
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

function TAbstractMap.Rect: TRectangle;
var
  MapW, MapH: Single;
  ContainerW, ContainerH: Integer;
begin
  MapW := Width - 1.0; { cut off 0.5 margin from left/right side }
  MapH := Height / 2 - 0.5;
  MapH /= TileWidthToHeight;
  ContainerW := ContainerWidth - 2 * PlayerSidebarWidth; // leave some space for controls on screen sides
  ContainerH := ContainerHeight;
  if MapW / MapH > ContainerW / ContainerH then
  begin
    Result.Left := 0;
    Result.Width := ContainerW;
    Result.Height := Round(Result.Width * MapH / MapW); // adjust Result.Height to aspect
    Result.Bottom := (ContainerH - Result.Height) div 2;
  end else
  begin
    Result.Bottom := 0;
    Result.Height := ContainerH;
    Result.Width := Round(Result.Height * MapW / MapH); // adjust Result.Width to aspect
    Result.Left := (ContainerW - Result.Width) div 2;
  end;
  Result.Left += PlayerSidebarWidth;
end;

function TAbstractMap.PositionToTile(const MapRect: TRectangle;
  ScreenPosition: TVector2Single; out X, Y: Integer): boolean;
var
  TileW, TileH: Single;
  ScreenPositionFrac: TVector2Single;
  EvenRow: boolean;
begin
  if not MapRect.Contains(ScreenPosition) then
    Exit(false);

  TileW := MapRect.Width / (Width - 1);
  TileH := TileW / TileWidthToHeight;

  ScreenPosition[0] := (ScreenPosition[0] - MapRect.Left  ) / TileW;
  ScreenPosition[1] := (ScreenPosition[1] - MapRect.Bottom) / TileH;
  ScreenPositionFrac[0] := Frac(ScreenPosition[0]);
  ScreenPositionFrac[1] := Frac(ScreenPosition[1]);
  if ScreenPositionFrac[1] < 0.5 then
  begin
    if ScreenPositionFrac[0] < 0.5 then
      EvenRow := PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0, 0)) <
                 PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0.5, 0.5)) else
      EvenRow := PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(1, 0)) <
                 PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0.5, 0.5));
    if EvenRow then
    begin
      if ScreenPositionFrac[0] < 0.5 then
        X := Trunc(ScreenPosition[0]) else
        X := Trunc(ScreenPosition[0]) + 1;
      Y := Trunc(ScreenPosition[1]) * 2;
    end else
    begin
      X := Trunc(ScreenPosition[0]);
      Y := Trunc(ScreenPosition[1]) * 2 + 1;
    end;
  end else
  begin
    if ScreenPositionFrac[0] < 0.5 then
      EvenRow := PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0, 1)) <
                 PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0.5, 0.5)) else
      EvenRow := PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(1, 1)) <
                 PointsDistanceSqr(Vector2Single(ScreenPositionFrac[0], ScreenPositionFrac[1]), Vector2Single(0.5, 0.5));
    if EvenRow then
    begin
      if ScreenPositionFrac[0] < 0.5 then
        X := Trunc(ScreenPosition[0]) else
        X := Trunc(ScreenPosition[0]) + 1;
      Y := Trunc(ScreenPosition[1]) * 2 + 2;
    end else
    begin
      X := Trunc(ScreenPosition[0]);
      Y := Trunc(ScreenPosition[1]) * 2 + 1;
    end;
  end;

  Result := (X >= 0) and (X < Width) and
            (Y >= 0) and (Y < Height);
end;

function TAbstractMap.Neighbors(const X1, Y1, X2, Y2: Cardinal;
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

function TAbstractMap.Neighbors(const P1, P2: TVector2SmallInt; out Dir: TDirection): boolean;
begin
  Result := Neighbors(P1[0], P1[1], P2[0], P2[1], Dir);
end;

function TAbstractMap.Neighbors(const X1, Y1, X2, Y2: Cardinal): boolean;
var
  Dir: TDirection;
begin
  Result := Neighbors(X1, Y1, X2, Y2, Dir);
end;

end.
