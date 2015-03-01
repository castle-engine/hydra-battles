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
    Background: TCastleImageControl;
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
  CastleFilesUtils,
  GameUtils, GameStatePlay;

{ TStateMainMenu ----------------------------------------------------------------- }

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  Background := TCastleImageControl.Create(Self);
  Background.URL := ApplicationData('at_oga/wallpaper2.png');
  Background.Stretch := true;
  Background.Proportional := true;
end;

procedure TStateMainMenu.Start;
begin
  inherited;
  Window.Controls.InsertFront(Background);
end;

procedure TStateMainMenu.Finish;
begin
  Window.Controls.Remove(Background);
  inherited;
end;

procedure TStateMainMenu.Resize;
begin
  inherited;
  Background.Width := ContainerWidth;
  Background.Height := ContainerHeight;
end;

procedure TStateMainMenu.Press(const Event: TInputPressRelease);
begin
  inherited;
  if Event.IsMouseButton(mbLeft) or Event.IsKey(K_Enter) or Event.IsKey(K_Escape) then
    TState.Current := StatePlay;
end;

end.
