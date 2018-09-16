{
  Copyright 2015-2017 Michalis Kamburelis.

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
    FFaction: TFaction;
    FProps: TProps;
  public
    constructor Create(const AOwner: TComponent;
      const AFaction: TFaction; const AProps: TProps); reintroduce;
    destructor Destroy; override;
    property Props: TProps read FProps;
    property Faction: TFaction read FFaction;
    function Rect: TFloatRectangle; override;
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
  FactionColor: array [TFaction] of TCastleColor = (
    (Data: (0.5, 0.5, 1, 1)),
    (Data: (1, 0.5, 0.5, 1))
  );

constructor TPlayerSidebar.Create(const AOwner: TComponent;
  const AFaction: TFaction; const AProps: TProps);
begin
  inherited Create(AOwner);
  FFaction := AFaction;
  GLFrame := TGLImage.Create(ApplicationData('gui/button_' + FactionName[Faction] + '.png'));
  FProps := AProps;
end;

function TPlayerSidebar.Rect: TFloatRectangle;
begin
  Result := FloatRectangle(Left, Bottom, PlayerSidebarWidth, Height);
end;

destructor TPlayerSidebar.Destroy;
begin
  FreeAndNil(GLFrame);
  inherited;
end;

procedure TPlayerSidebar.Render;
var
  BDH: Integer;
  R: TFloatRectangle;
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

  R := FloatRectangle(Left, Bottom, PlayerSidebarWidth, Height - BDH * 2);
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

  R := FloatRectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Drag to build Barracks (%d wood)', [FProps[Barracks[Faction]].CostWood]),
    0, hpMiddle, vpMiddle);

  R := FloatRectangle(Left, R.Top, PlayerSidebarWidth, BDH);
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
  R: TFloatRectangle;
begin
  BDH := Round(BuildingDragHeight * Height);
  Result := false;

  R := FloatRectangle(Left, Bottom + Height - 2 * BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Props[Barracks[Faction]];
    Result := Prop.CanBuild(PropInstances);
  end;

  R := FloatRectangle(Left, Bottom + Height - BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Props[Headquarters[Faction]];
    Result := Prop.CanBuild(PropInstances);
  end;
end;

end.
