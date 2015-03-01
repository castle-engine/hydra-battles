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

{ Game state of main menu. }
unit GameStateMainMenu;

interface

uses Classes, CastleControls, CastleKeysMouse,
  GameStates;

type
  TStateMainMenu = class(TState)
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
    procedure Finish; override;
    procedure Resize; override;
    procedure Press(const Event: TInputPressRelease); override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  CastleFilesUtils, CastleRectangles, CastleUtils,
  GameUtils, GameStatePlay;

const
  DefaultMapName = 'small';

procedure TStateMainMenu.TPlayButton.DoClick;
begin
  StatePlay.StartMapName := MapName;
  TState.Current := StatePlay;
end;

{ TStateMainMenu ----------------------------------------------------------------- }

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;

  Background := TCastleImageControl.Create(Self);
  Background.URL := ApplicationData('at_oga/wallpaper2.png');
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
  Window.Controls.InsertFront(Background);
  Window.Controls.InsertFront(PlayButtonSmall);
  Window.Controls.InsertFront(PlayButtonLarge);
end;

procedure TStateMainMenu.Finish;
begin
  Window.Controls.Remove(Background);
  Window.Controls.Remove(PlayButtonSmall);
  Window.Controls.Remove(PlayButtonLarge);
  inherited;
end;

procedure TStateMainMenu.Resize;
var
  R: TRectangle;
begin
  inherited;
  Background.Width := ContainerWidth;
  Background.Height := ContainerHeight;

  R := Background.Rect;

  PlayButtonLarge.Left := Round(Lerp(0.69, R.Left, R.Right));
  PlayButtonSmall.Left := PlayButtonLarge.Left;

  PlayButtonLarge.Width := Round(R.Width * 0.3);
  PlayButtonSmall.Width := PlayButtonLarge.Width;

  PlayButtonLarge.Height := Round(R.Height * 0.19);
  PlayButtonSmall.Height := PlayButtonLarge.Height;

  PlayButtonSmall.Bottom := Round(Lerp(0.2, R.Bottom, R.Top));
  PlayButtonLarge.Bottom := Round(Lerp(0.5, R.Bottom, R.Top));
end;

procedure TStateMainMenu.Press(const Event: TInputPressRelease);
begin
  inherited;
  if Event.IsKey(K_Enter) or Event.IsKey(K_Escape) then
  begin
    StatePlay.StartMapName := DefaultMapName;
    TState.Current := StatePlay;
  end;
end;

end.
