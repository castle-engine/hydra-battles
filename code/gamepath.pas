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

{ Path to be followed by NPC. }
unit GamePath;

interface

uses FGL,
  CastleVectors, Castle2DSceneManager, X3DNodes,
  GameAbstractMap;

type
  TPath = class(TVector2SmallIntList)
  private
    FMap: TAbstractMap;
    LastX, LastY, LastLastX, LastLastY: SmallInt;
    FVisualization: T2DScene;
    LineSet: TLineSetNode;
    Coordinate: TCoordinateNode;
    property Map: TAbstractMap read FMap;
    //property Visualization: T2DScene read FVisualization;
  public
    { Start a new path. Note that position under PathStartX, PathStartY
      is assumed to be always Ok (otherwise we would collide with starting npc...). }
    constructor Create(const AMap: TAbstractMap; const PathStartX, PathStartY: Integer);
    destructor Destroy; override;
    function Add(const X, Y: SmallInt; const AssumeValid: boolean = false): boolean;
    function ValidTile(const X, Y: Integer): boolean;
    function PointsVector(const FromIndex, ToIndex: Integer): TVector2Single;
  end;

  TPathList = specialize TFPGObjectList<TPath>;

implementation

uses CastleRectangles,
  GameUtils;

constructor TPath.Create(const AMap: TAbstractMap; const PathStartX, PathStartY: Integer);

  procedure CreateVisualization;
  var
    Root: TX3DRootNode;
    Shape: TShapeNode;
    Material: TMaterialNode;
    Appearance: TAppearanceNode;
    LineProperties: TLinePropertiesNode;
  begin
    Coordinate := TCoordinateNode.Create('', '');
    Coordinate.FdPoint.Items.Clear;

    LineSet := TLineSetNode.Create('', '');
    LineSet.FdCoord.Value := Coordinate;

    Material := TMaterialNode.Create('', '');
    Material.FdEmissiveColor.Value := Vector3Single(0.1, 0.1, 0.1);

    LineProperties := TLinePropertiesNode.Create('', '');
    LineProperties.FdLineWidthScaleFactor.Value := 10;

    Appearance := TAppearanceNode.Create('', '');
    Appearance.FdMaterial.Value := Material;
    Appearance.FdLineProperties.Value := LineProperties;

    Shape := TShapeNode.Create('', '');
    Shape.FdGeometry.Value := LineSet;
    Shape.Appearance := Appearance;

    Root := TX3DRootNode.Create('', '');
    Root.FdChildren.Add(Shape);

    FVisualization := T2DScene.Create(VisualizationSceneManager);
    FVisualization.Load(Root, true);
    VisualizationSceneManager.Items.Add(FVisualization);
  end;

begin
  inherited Create;
  FMap := AMap;
  LastX := -1;
  LastY := -1;
  LastLastX := -1;
  LastLastY := -1;
  CreateVisualization;
  Add(PathStartX, PathStartY, true);
end;

destructor TPath.Destroy;
begin
  if (FVisualization <> nil) and
     (VisualizationSceneManager <> nil) then
    VisualizationSceneManager.Items.Remove(FVisualization);
  inherited;
end;

function TPath.Add(const X, Y: SmallInt; const AssumeValid: boolean): boolean;
const
  PathZ = 1;
var
  MapRect, TileRect: TRectangle;
  TileMiddle: TVector2Integer;
  Point3D: TVector3Single;
  Point2D: TVector2SmallInt;
  C: Integer;
begin
  if (X = LastX) and (Y = LastY) then
    Exit(true);

  if (LastX <> -1) and (LastY <> -1) and not Map.Neighbors(LastX, LastY, X, Y) then
    Exit(false);
  if (not AssumeValid) and (not Map.ValidTile(X, Y)) then
    Exit(false);

  MapRect := Map.Rect;
  TileRect := Map.GetTileRect(MapRect, X, Y);
  Point2D := Vector2SmallInt(X, Y);
  TileMiddle := TileRect.Middle;
  Point3D := Vector3Single(TileMiddle[0] - MapRect.Left, TileMiddle[1] - MapRect.Bottom, PathZ);

  if (LastLastX <> -1) and (LastLastY <> -1) and Map.Neighbors(LastLastX, LastLastY, X, Y) then
  begin
    { do not add new line point, only replace the last one. This smooths path. }
    Items[Count - 1] := Point2D;
    Coordinate.FdPoint.Items.Items[Coordinate.FdPoint.Items.Count - 1] := Point3D;
    Coordinate.FdPoint.Changed;

    LastX := X;
    LastY := Y;
  end else
  begin
    inherited Add(Point2D);
    Coordinate.FdPoint.Items.Add(Point3D);
    Coordinate.FdPoint.Changed;

    C := Coordinate.FdPoint.Items.Count;
    if C >= 2 then
    begin
      if LineSet.FdVertexCount.Items.Count <> 0 then
        LineSet.FdVertexCount.Items.Items[0] := C else
        LineSet.FdVertexCount.Items.Add(C);
      LineSet.FdVertexCount.Changed;
    end;

    LastLastX := LastX;
    LastLastY := LastY;
    LastX := X;
    LastY := Y;
  end;

  Result := true;
end;

function TPath.ValidTile(const X, Y: Integer): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I][0] = X) and (Items[I][1] = Y) then
     Exit(false);
  Result := true;
end;

function TPath.PointsVector(const FromIndex, ToIndex: Integer): TVector2Single;
var
  PFrom, PTo: TVector3Single;
begin
  { note that Coordinate.FdPoint contains points as 3d, and moved by MapRect.Left/Bottom.
    But it all cancels out when we just need a difference vector. }
  PFrom := Coordinate.FdPoint.Items.Items[FromIndex];
  PTo := Coordinate.FdPoint.Items.Items[ToIndex];
  Result[0] := PTo[0] - PFrom[0];
  Result[1] := PTo[1] - PFrom[1];
end;

end.
