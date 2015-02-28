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

uses SysUtils, CastleWindowTouch, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleConfig,
  CastleLog, CastleProgress, CastleWindowProgress, CastleUtils,
  GameUtils, GameStatePlay, GameStateMainMenu, GameStates;

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

  { initialize these in case something wants to use them before WindowResize. }
  ContainerWidth := Window.Width;
  ContainerHeight := Window.Height;

  Progress.UserInterface := WindowProgressInterface;

  { add window controls on all game states }
  SimpleBackground := TCastleSimpleBackground.Create(Application);
  Window.Controls.InsertFront(SimpleBackground);

  StatePlay := TStatePlay.Create(Application);
  StateMainMenu := TStateMainMenu.Create(Application);

  ReadGameConf;

  TState.Current := StateMainMenu;
end;

procedure WindowResize(Container: TUIContainer);
begin
  ContainerWidth := Container.Width;
  ContainerHeight := Container.Height;
  TState.Current.Resize;
end;

procedure WindowUpdate(Container: TUIContainer);
const
  { Avoid increasing WorldTime a lot (and producing a lot of dragons)
    when the game was just hanging on Android for some time. }
  MaxSensibleSecondsPassed = 1;
var
  SecondsPassed: Single;
begin
  SecondsPassed := Min(MaxSensibleSecondsPassed, Container.Fps.UpdateSecondsPassed);
  TState.Current.Update(SecondsPassed);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  TState.Current.Press(Event);
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
  Window.OnResize := @WindowResize;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
finalization
  TState.Current := nil;
end.
