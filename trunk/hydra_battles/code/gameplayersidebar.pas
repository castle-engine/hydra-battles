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
  CastleImages, CastleUIControls, CastleGLImages, CastleRectangles,
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
    property Faction: TFaction read FFaction;
    function Rect: TRectangle; override;
    property Height: Integer read FHeight write FHeight;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

implementation

uses SysUtils,
  CastleColors, CastleFilesUtils, CastleControls, CastleVectors;

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
    Format('Build Barracks (%d wood)', [FProps[Barracks[Faction]].CostWood]),
    0, prMiddle, prMiddle);

  R := Rectangle(Left, R.Top, PlayerSidebarWidth, BDH);
  GLFrame.Draw3x3(R, Vector4Integer(2, 2, 2, 2));
  UIFont.PrintBrokenString(R, Black,
    Format('Build HQ (%d wood), max one', [FProps[Headquarters[Faction]].CostWood]),
    0, prMiddle, prMiddle);
end;

end.
