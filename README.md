Hydra Battles
=============

RTS (real-time strategy) game for 2 players, with some twists. Players gather resources and attack the other player, also drawing lines to hinder the other player movement.

The compiled game versions are available on

  http://michaliskambi.itch.io/hydra-battles

From here you can download the source code (compile using Castle Game Engine, http://castle-engine.sourceforge.net/ and it's build tool: https://sourceforge.net/p/castle-engine/wiki/Build%20tool/ ). Also, this document is right now the only game manual, and contains instructions and hints how to play (and win!:).

Playing
=======

Both players play on the same screen (no scrolling). Each player has 10 seconds for move (it is also possible to configure the game in "simultaneous" playing mode).

The game can be played on PC (each player then takes the mouse for 10 seconds), or Android (two players drag on a single touch screen; multi-touch works, so drag as you like).

Buildings
=========

Build buildings by dragging them from the sidebar.

1. *Headquarters (HQ)*. Cost 1000 wood. Can train a peasant, for 2 wood. You have to
build HQ before placing other buildings on the map.

2. *Barracks*. Cost 10 wood. Can train a warrior, for 6 wood.

At the beginning, you do not have any buildings or units. Your only move is to build a Headquarters (drag the "Build HQ" icon), and then train a peasant (click on HQ). Then you can instruct the peasant to gather wood, and eventually you will be able to train more peasants or build barracts (that in turn will allow to train soldiers).

Units
=====

Train units by clicking on a building (HQ train peasants, barracks train warriors).

1. *Peasants*. Send them to the forest, to cut down trees, to gather wood. Cutting down a tree increases your resources. The "hemp" trees are higher quality wood (you gain 2 wood for a tree), other trees are lower quality (you gain 1 wood for a tree).

2. *Warriors* (knights for human, minotaurs for monsters). Send them to attack other buildings or units. Kill warriors or peasants of other player.

The ultimate game goal is to destroy your enemy: kill all his units and destroy his buildings.

Commands
========

When you command a unit (a peasant or a soldier) to go somewhere, you draw a line on the screen. This line cannot be crossed by other commands. This way you may make it difficult for other player (and yourself) to make other commands (to gather resources or attack you).

By drawing correct, long lines, you not only direct your units, but also make it more difficult for other player to win. Line drawing works very nice with a mouse or a multi-touch touch screen (like on Android).

Level editor
============

Editor is included.

Editor keys:
* E - on/off editor
* S - save edited maps to file
* arrows keys - move editor cursor
* use the names of props (see data/game.xml for editor_shortcut), like "t" for tree, 1-8 for mountains
* 0 - random mountain
* space - clear
* N - random unit (npc)

To design a proper map, place only "normal" neutral props on a map. Do not place NPCs or player buildings (headquarters, barracks). The code is not ready now to handle them Ok in initial positions.

Generally useful keys:
* F5 - save screen
* G - on/off grid

Author
======

Design and programming: Michalis Kamburelis

Created during a 48h gamejam on https://www.facebook.com/tsgcompo

Using Castle Game Engine, http://castle-engine.sourceforge.net/

Graphics: isometric graphics, using assets mostly from http://opengameart.org/ . See AUTHORS.txt file for links. See sources on https://github.com/castle-engine/hydra-battles
