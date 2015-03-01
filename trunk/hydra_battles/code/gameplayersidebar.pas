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
    Frame: TCastleImage;
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
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    function StartsDragging(const ScreenPosition: TVector2Single; out Prop: TPropType): boolean;
  end;

implementation

uses SysUtils,
  CastleColors, CastleFilesUtils, CastleControls, CastleGameNotifications;

const
  BuildingDragHeight = 0.4;
  FactionColor: array [TFaction] of TCastleColor = ((0.1, 0.1, 1, 1), (1, 0.1, 0.1, 1));

constructor TPlayerSidebar.Create(const AOwner: TComponent;
  const AFaction: TFaction; const AProps: TProps);
begin
  inherited Create(AOwner);
  FFaction := AFaction;
  Frame := LoadImage(ApplicationData('gui/button_' + FactionName[Faction] + '.png'), []);
  FProps := AProps;
end;

function TPlayerSidebar.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, PlayerSidebarWidth, Height);
end;

destructor TPlayerSidebar.Destroy;
begin
  FreeAndNil(Frame);
  inherited;
end;

procedure TPlayerSidebar.GLContextOpen;
begin
  inherited;
  if GLFrame = nil then
    GLFrame := TGLImage.Create(Frame, true);
end;

procedure TPlayerSidebar.GLContextClose;
begin
  FreeAndNil(GLFrame);
  inherited;
end;

procedure TPlayerSidebar.Render;
var
  BDH: Integer;
  R: TRectangle;
begin
  inherited;
  BDH := Round(BuildingDragHeight * Height);

  R := Rectangle(Left, Bottom, PlayerSidebarWidth, Height - BDH * 2);
  UIFont.PrintBrokenString(R, FactionColor[Faction],
    Format('%s - %d wood', [FactionName[Faction], Trunc(Wood[Faction])]),
    0, prMiddle, prMiddle);

  R := Rectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Drag to build Barracks (%d wood)', [FProps[Barracks[Faction]].CostWood]),
    0, prMiddle, prMiddle);

  R := Rectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Drag to build HQ (%d wood)', [FProps[Headquarters[Faction]].CostWood]),
    0, prMiddle, prMiddle);
end;

function TPlayerSidebar.StartsDragging(const ScreenPosition: TVector2Single; out Prop: TPropType): boolean;
var
  BDH: Integer;
  R: TRectangle;
begin
  BDH := Round(BuildingDragHeight * Height);
  Result := false;

  R := Rectangle(Left, Height - 2 * BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Barracks[Faction];
    if Trunc(Wood[Faction]) < Props[Prop].CostWood then
      Notifications.Show(Format('Not enough wood to buy Barracks by faction "%s"', [FactionName[Faction]])) else
      Result := true;
  end;

  R := Rectangle(Left, Height - BDH, PlayerSidebarWidth, BDH);
  if R.Contains(ScreenPosition) then
  begin
    Prop := Headquarters[Faction];
    if Trunc(Wood[Faction]) < Props[Prop].CostWood then
      Notifications.Show(Format('Not enough wood to buy Headquarters by faction "%s"', [FactionName[Faction]])) else
      Result := true;
  end;
end;

end.
