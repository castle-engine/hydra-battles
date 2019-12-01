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

{ Game state of main menu. }
unit GameStateMainMenu;

interface

uses Classes, CastleControls, CastleKeysMouse, CastleUIState;

type
  TStateMainMenu = class(TUIState)
  private
  type
    TPlayButton = class(TCastleButton)
    public
      MapName: string;
      procedure DoClick; override;
    end;
  var
    Background: TCastleImageControl;
    PlayButtonSmall, PlayButtonLarge: TPlayButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  CastleFilesUtils, CastleRectangles, CastleUtils, CastleGameNotifications,
  GameUtils, GameStatePlay;

const
  DefaultMapName = 'small';

procedure TStateMainMenu.TPlayButton.DoClick;
begin
  StatePlay.StartMapName := MapName;
  TUIState.Current := StatePlay;
end;

{ TStateMainMenu ----------------------------------------------------------------- }

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;

  Background := TCastleImageControl.Create(Self);
  Background.URL := 'castle-data:/at_oga/wallpaper2.png';
  Background.Stretch := true;
  Background.Proportional := true;

  PlayButtonSmall := TPlayButton.Create(Self);
  PlayButtonSmall.Caption := 'Small Map';
  PlayButtonSmall.MapName := 'small';
  PlayButtonSmall.AutoSize := false;

  PlayButtonLarge := TPlayButton.Create(Self);
  PlayButtonLarge.Caption := 'Large Map';
  PlayButtonLarge.MapName := 'large';
  PlayButtonLarge.AutoSize := false;
end;

procedure TStateMainMenu.Start;
begin
  inherited;
  InsertFront(Background);
  InsertFront(PlayButtonSmall);
  InsertFront(PlayButtonLarge);
  InsertFront(Notifications);
end;

procedure TStateMainMenu.Stop;
begin
  RemoveControl(Background);
  RemoveControl(PlayButtonSmall);
  RemoveControl(PlayButtonLarge);
  RemoveControl(Notifications);
  inherited;
end;

procedure TStateMainMenu.Resize;
var
  R: TFloatRectangle;
begin
  inherited;
  Background.Width := ContainerWidth;
  Background.Height := ContainerHeight;

  R := Background.EffectiveRect;

  UIFont.Size := 20;

  PlayButtonLarge.Left := Round(Lerp(0.69, R.Left, R.Right));
  PlayButtonSmall.Left := PlayButtonLarge.Left;

  PlayButtonLarge.Width := Round(R.Width * 0.3);
  PlayButtonSmall.Width := PlayButtonLarge.Width;

  PlayButtonLarge.Height := Round(R.Height * 0.19);
  PlayButtonSmall.Height := PlayButtonLarge.Height;

  PlayButtonSmall.Bottom := Round(Lerp(0.2, R.Bottom, R.Top));
  PlayButtonLarge.Bottom := Round(Lerp(0.5, R.Bottom, R.Top));
end;

function TStateMainMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_Enter) or Event.IsKey(K_Escape) then
  begin
    Result := true;
    StatePlay.StartMapName := DefaultMapName;
    TUIState.Current := StatePlay;
  end;
end;

end.
