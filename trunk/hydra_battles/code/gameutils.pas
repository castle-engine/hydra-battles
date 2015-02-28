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

{ Global game utilities and variables. }
unit GameUtils;

interface

uses CastleWindow, CastleConfig,
  CastleTimeUtils, CastleVectors, CastleRectangles, Castle2DSceneManager;

var
  GameConf: TCastleConfig;
  Window: TCastleWindowCustom;
  ContainerWidth, ContainerHeight: Integer;

//function GameScale: Single;

implementation

uses CastleUtils;

end.
