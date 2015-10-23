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

{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleKeysMouse, CastleConfig,
  CastleLog, CastleProgress, CastleWindowProgress, CastleUtils, CastleSceneManager,
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
  Input_Attack.MakeClear(true);
  Input_InventoryPrevious.MakeClear(true);
  Input_InventoryNext.MakeClear(true);
  Input_UseItem.MakeClear(true);

  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Color := Yellow;

  Progress.UserInterface := WindowProgressInterface;

  { add window controls on all game states }
  SimpleBackground := TCastleSimpleBackground.Create(Application);
  Window.Controls.InsertFront(SimpleBackground);

  StatePlay := TStatePlay.Create(Application);
  StateMainMenu := TStateMainMenu.Create(Application);

  ReadGameConf;

  TUIState.Current := StateMainMenu;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
end;

function MyGetApplicationName: string;
begin
  Result := 'hydra_battles';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
  Window.OnPress := @WindowPress;
end.
