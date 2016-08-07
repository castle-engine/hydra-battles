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

{ Sidebar of a player, to drag buildings and show info. }
unit GamePlayerSidebar;

interface

uses Classes,
  CastleImages, CastleUIControls, CastleGLImages, CastleRectangles, CastleVectors,
  GameUtils, GameProps;

type
  TPlayerSidebar = class(TUIRectangularControl)
  private
    GLFrame: TGLImage;
    FHeight: Integer;
    FFaction: TFaction;
    FProps: TProps;
  public
    constructor Create(const AOwner: TComponent;
      const AFaction: TFaction; const AProps: TProps); reintroduce;
    destructor Destroy; override;
    property Props: TProps read FProps;
    property Faction: TFaction read FFaction;
    function Rect: TRectangle; override;
    property Height: Integer read FHeight write FHeight;
    procedure Render; override;
    function StartsDragging(
      const PropInstances: TPropInstanceList;
      const ScreenPosition: TVector2Single; out Prop: TProp): boolean;
  end;

implementation

uses SysUtils,
  CastleColors, CastleFilesUtils, CastleControls, CastleGameNotifications,
  CastleGLUtils, CastleUtils;

const
  BuildingDragHeight = 0.4;
  FactionColor: array [TFaction] of TCastleColor = ((0.5, 0.5, 1, 1), (1, 0.5, 0.5, 1));

constructor TPlayerSidebar.Create(const AOwner: TComponent;
  const AFaction: TFaction; const AProps: TProps);
begin
  inherited Create(AOwner);
  FFaction := AFaction;
  GLFrame := TGLImage.Create(ApplicationData('gui/button_' + FactionName[Faction] + '.png'));
  FProps := AProps;
end;

function TPlayerSidebar.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, PlayerSidebarWidth, Height);
end;

destructor TPlayerSidebar.Destroy;
begin
  FreeAndNil(GLFrame);
  inherited;
end;

procedure TPlayerSidebar.Render;
var
  BDH: Integer;
  R: TRectangle;
  ColorBg, ColorText: TCastleColor;
  Timeout: string;
begin
  inherited;
  BDH := Round(BuildingDragHeight * Height);

  Timeout := '';
  if FactionExclusiveMoves then
    if FactionCanMove(Faction) then
      Timeout := ' [' + IntToStr(FactionExclusiveMovesDuration -
        Trunc(FloatModulo(GameTime, FactionExclusiveMovesDuration))) + ']' else
      Timeout := ' [blocked]';

  R := Rectangle(Left, Bottom, PlayerSidebarWidth, Height - BDH * 2);
  if FactionCanMove(Faction) then
  begin
    ColorBg := FactionColor[Faction];
    ColorText := Black;
  end else
  begin
    ColorBg := Black;
    ColorText := FactionColor[Faction];
  end;
  DrawRectangle(R, ColorBg);
  UIFont.PrintBrokenString(R, ColorText,
    Format('%s (%d wood)'+ Timeout, [FactionName[Faction], Trunc(Wood[Faction])]),
    0, hpMiddle, vpMiddle);

  R := Rectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Drag to build Barracks (%d wood)', [FProps[Barracks[Faction]].CostWood]),
    0, hpMiddle, vpMiddle);

  R := Rectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Drag to build HQ (%d wood)', [FProps[Headquarters[Faction]].CostWood]),
    0, hpMiddle, vpMiddle);
end;

function TPlayerSidebar.StartsDragging(
  const PropInstances: TPropInstanceList;
  const ScreenPosition: TVector2Single; out Prop: TProp): boolean;
var
  BDH: Integer;
  R: TRectangle;
begin
  BDH := Round(BuildingDragHeight * Height);
  Result := false;

  R := Rectangle(Left, Bottom + Height - 2 * BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Props[Barracks[Faction]];
    Result := Prop.CanBuild(PropInstances);
  end;

  R := Rectangle(Left, Bottom + Height - BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Props[Headquarters[Faction]];
    Result := Prop.CanBuild(PropInstances);
  end;
end;

end.
