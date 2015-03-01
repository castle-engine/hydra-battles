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

uses CastleVectors, Castle2DSceneManager, CastleUIControls, CastleRectangles;

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
      const ScreenPosition: TVector2Single; out X, Y: Integer): boolean;
  end;

implementation

uses Math,
  CastleUtils,
  GameUtils;

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
  ContainerW := ContainerWidth - 2 * SideControlWidth; // leave some space for controls on screen sides
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
  Result.Left += SideControlWidth;
end;

function TAbstractMap.PositionToTile(const MapRect: TRectangle;
  const ScreenPosition: TVector2Single; out X, Y: Integer): boolean;
begin
  if not MapRect.Contains(ScreenPosition) then
    Exit(false);
  Result := true;
  { TODO: too simple }
  X := Clamped(Trunc(MapRange(ScreenPosition[0], MapRect.Left, MapRect.Right, 0, Width)), 0, Width - 1);
  Y := Clamped(Trunc(MapRange(ScreenPosition[1], MapRect.Bottom, MapRect.Top, 0, Height)), 0, Height - 1);
end;

end.
