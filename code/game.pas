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

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleKeysMouse, CastleConfig,
  CastleLog, CastleUtils, CastleSceneManager, CastleApplicationProperties,
  CastleUIState, CastleGameNotifications, CastleUIControls, CastleColors,
  GameUtils, GameStatePlay, GameStateMainMenu;

var
  SimpleBackground: TCastleSimpleBackground;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  { Read game.xml with game configuration. }
  procedure ReadGameConf;
  begin
    GameConf := TCastleConfig.Create(Application);
    GameConf.RootName := 'game';
    GameConf.NotModified; { otherwise changing RootName makes it modified, and saved back at freeing }
    GameConf.URL := ApplicationData('game.xml');
  end;

begin
  {$ifndef MSWINDOWS} { Under Windows, log requires stderr. }
  InitializeLog;
  {$endif}

  // disable shortcuts, in particular do not let scene manager to capture clicks
  Input_Interact.MakeClear(true);

  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Color := Yellow;

  { add window controls on all game states }
  SimpleBackground := TCastleSimpleBackground.Create(Application);
  Window.Controls.InsertFront(SimpleBackground);

  StatePlay := TStatePlay.Create(Application);
  StateMainMenu := TStateMainMenu.Create(Application);

  ReadGameConf;

  Window.Container.View := StateMainMenu;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(keyF5) then
    Container.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
end;

initialization
  ApplicationProperties.ApplicationName := 'hydra_battles';

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Window.OnPress := @WindowPress;
end.
