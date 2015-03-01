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
    LastX, LastY: SmallInt;
    FVisualization: T2DScene;
    LineSet: TLineSetNode;
    Coordinate: TCoordinateNode;
    //property Visualization: T2DScene read FVisualization;
    property Map: TAbstractMap read FMap;
  public
    constructor Create(const AMap: TAbstractMap; const VisualizationSceneManager: T2DSceneManager);
    procedure Add(const X, Y: SmallInt);
  end;

  TPathList = specialize TFPGObjectList<TPath>;

implementation

uses CastleRectangles;

constructor TPath.Create(const AMap: TAbstractMap; const VisualizationSceneManager: T2DSceneManager);

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
  CreateVisualization;
end;

procedure TPath.Add(const X, Y: SmallInt);
const
  PathZ = 1;
var
  TileRect: TRectangle;
  Point: TVector2Integer;
  C: Integer;
begin
  if (X = LastX) and (Y = LastY) then
    Exit;
  LastX := X;
  LastY := Y;

  TileRect := Map.GetTileRect(Map.Rect, X, Y);
  Point := TileRect.Middle;
  Coordinate.FdPoint.Items.Add(Vector3Single(Point[0], Point[1], PathZ));
  Coordinate.FdPoint.Changed;

  C := Coordinate.FdPoint.Items.Count;
  if C >= 2 then
  begin
    if LineSet.FdVertexCount.Items.Count <> 0 then
      LineSet.FdVertexCount.Items.Items[0] := C else
      LineSet.FdVertexCount.Items.Add(C);
    LineSet.FdVertexCount.Changed;
  end;

  FVisualization.ChangedAll;

  { TODO:
    fix PositionToTile, since it breaks this path.
    smooth: smooth "teeth" when possible, removing last point if new point neighbors to last-last point.
    check is neighbor to last.
    return false if invalid (e.g. crosses prop, in future: crosses other lines). }
end;

end.
