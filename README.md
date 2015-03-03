Hydra Battles
=============

RTS (real-time strategy) game for 2 players, with some twists. Players gather resources and attack the other player, also drawing lines to hinder the other player movement. Both players play on the same screen (no scrolling).

Playing
=======

Like in a classic RTS games, you have headquarters, you command peasants to gather resources (wood), you can build buildings (barracks) and train soldiers and command them to attack. You plan how to divide your resources, what to build/train, while the time is ticking.

Build buildings by dragging them from the sidebar.

Train units by clicking on a building (HQ train peasants, barracks train warriors).

*Then, the twist*: When you command NPC (a peasant or a soldier) to go somewhere, you draw a line on the screen. This line cannot be crossed by other commands. This way you may make it difficult for other player (and yourself) to make other commands (to gather resources or attack you).

By drawing correct, long lines, you not only direct your units, but also make it more difficult for other player to win. Line drawing works very nice with a mouse or a multi-touch touch screen (like on Android).

You can play in "free for all" mode (faction_exclusive_moves = false in game.xml) where both players can move all the time. Note that the game works on Android with multi-touch, playing on a large touch screen is very comfortable. Or you can play with faction_exclusive_moves = true, where each player has 10 seconds to performs his/her move. The latter is useful to play on PC with a single mouse.

How to start
============

At the beginning, you do not have any buildings or npcs. Your only move is to build a HeadQuarters (drag the "Build HQ" icon), and then train a peasant (click on HQ). Then you can instruct the peasant to gather wood, and eventually you will be able to train more peasants or build barracts (that in turn will allow to train soldiers).

Costs of building/training: see data/game.xml file for now.

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
* N - random npc

To design a proper map, place only "normal" neutral props on a map. Do not place NPCs or player buildings (headquarters, barracks). The code is not ready now to handle them Ok in initial positions.

Generally useful keys:
* F5 - save screen
* G - on/off grid

Author
======

Design and programming:
Michalis Kamburelis

Created during a 48h gamejam.

Using Castle Game Engine, http://castle-engine.sourceforge.net/

Graphics: isometric graphics, using assets mostly from http://opengameart.org/ . See AUTHORS.txt file for links.
