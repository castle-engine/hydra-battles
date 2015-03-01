--       _________ __                 __
--      /   _____//  |_____________ _/  |______     ____  __ __  ______
--      \_____  \\   __\_  __ \__  \\   __\__  \   / ___\|  |  \/  ___/
--      /        \|  |  |  | \// __ \|  |  / __ \_/ /_/  >  |  /\___ \
--     /_______  /|__|  |__|  (____  /__| (____  /\___  /|____//____  >
--             \/                  \/          \//_____/            \/
--  ______________________                           ______________________
--                        T H E   W A R   B E G I N S
--         Stratagus - A free fantasy real time strategy game engine
--
--      ui.lua - Define the human user interface
--
--      (c) Copyright 2001-2012 by Lutz Sammer, Kyran Jackson, and Jimmy Salmon
--
--      This program is free software; you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation; either version 2 of the License, or
--      (at your option) any later version.
--
--      This program is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with this program; if not, write to the Free Software
--      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

DefineCursor({
  Name = "cursor-point",
  Race = "human",
  File = "ui/human/cursors/human_gauntlet.png",
  HotSpot = { 3,  2},
  Size = {36, 38}})
DefineCursor({
  Name = "cursor-green-hair",
  Race = "human",
  File = "ui/human/cursors/green_eagle.png",
  HotSpot = {15, 15},
  Size = {32, 32}})
DefineCursor({
  Name = "cursor-yellow-hair",
  Race = "human",
  File = "ui/human/cursors/yellow_eagle.png",
  HotSpot = {15, 15},
  Size = {32, 32}})
DefineCursor({
  Name = "cursor-red-hair",
  Race = "human",
  File = "ui/human/cursors/red_eagle.png",
  HotSpot = {15, 15},
  Size = {32, 32}})

--;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
--	* Mythic/human race
--;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wargus.playlist = { "music/Human Battle 1.ogg", "music/Human Battle 2.ogg", "music/Human Battle 3.ogg", "music/Human Battle 4.ogg", "music/Human Battle 5.ogg", "music/Human Battle 6.ogg" }

--function HumanScreen(screen_width, screen_height)
--  local info_panel_x = 0
--  local info_panel_y = 160

--  local ui = {
--    "info-panel", {
--      "panels", {"panel-general-contents", "panel-attack-unit-contents",
--                "panel-all-unit-contents", "panel-building-contents"},
--      "completed-bar", {
--        "color", {99, 224, 211}
--      }
--    }
--  }
--end

UI.NormalFontColor = "white"
UI.ReverseFontColor = "white"

UI.Fillers:clear()

function AddFiller(file, x, y)
	if CanAccessFile(file) == true then
		filler = CFiller:new_local()
		filler.G = CGraphic:New(file)
		filler.X = x
		filler.Y = y
		UI.Fillers:push_back(filler)
	end
end

------------------Populating the HumanScreen with overlay elements------------------
minimapWidth = 256
minimapHeight = 256
minimapX = 0
minimapY = 0
mmoffsetX = 64
mmoffsetY = 64
AddFiller("ui/human/minimap2.png", minimapX, minimapY)
UI.Minimap.X = minimapX+mmoffsetX
UI.Minimap.Y = minimapY+mmoffsetY
UI.Minimap.W = 128
UI.Minimap.H = 128


--Adding Menu Button
mboffsetX = 79
mboffsetY = 0
UI.MenuButton.X = minimapX+mboffsetX
UI.MenuButton.Y = minimapY+mboffsetY
--UI.MenuButton.Text = "Menu (~<F10~>)"
--UI.MenuButton.Style = FindButtonStyle("main")
UI.MenuButton:SetCallback(
  function()
    if (Editor.Running == EditorNotRunning) then
	  RunGameMenu()
	else
	  RunInEditorMenu()
	end
  end)
---===================Info Panel and related content============================
infopanelWidth = 256
infopanelHeight = 256
infopanelX = 0
infopanelY = minimapY+minimapHeight
ipoffsetX = 48
ipoffsetY = 48
AddFiller("ui/human/infopanel2.png", infopanelX, infopanelY)

seloffset = 4 -- what is it?
select = CUIButton:new()
select.X = infopanelX+ipoffsetX+seloffset
select.Y = infopanelY+ipoffsetY+seloffset
select.Style = FindButtonStyle("icon")
UI.SingleSelectedButton = select

UI.SelectedButtons:clear()

function AddSelectedButton(x, y)
	b = CUIButton:new_local()
	b.X = x
	b.Y = y
	b.Style = FindButtonStyle("icon")
	UI.SelectedButtons:push_back(b)
end



--Positioning the icons of the selected Units
-- 3x3 = 9 units in total
separation = 4
iconsize = 48
--first row
AddSelectedButton(infopanelX+ipoffsetX+separation, infopanelY+ipoffsetY+separation)
AddSelectedButton(infopanelX+ipoffsetX+separation*2+iconsize, infopanelY+ipoffsetY+separation)
AddSelectedButton(infopanelX+ipoffsetX+separation*3+iconsize*2, infopanelY+ipoffsetY+separation)
--second row
AddSelectedButton(infopanelX+ipoffsetX+separation, infopanelY+ipoffsetY+separation*2+iconsize)
AddSelectedButton(infopanelX+ipoffsetX+separation*2+iconsize, infopanelY+ipoffsetY+separation*2+iconsize)
AddSelectedButton(infopanelX+ipoffsetX+separation*3+iconsize*2, infopanelY+ipoffsetY+separation*2+iconsize)
--third row
AddSelectedButton(infopanelX+ipoffsetX+separation, infopanelY+ipoffsetY+separation*3+iconsize*2)
AddSelectedButton(infopanelX+ipoffsetX+separation*2+iconsize, infopanelY+ipoffsetY+separation*3+iconsize*2)
AddSelectedButton(infopanelX+ipoffsetX+separation*3+iconsize*2, infopanelY+ipoffsetY+separation*3+iconsize*2)


UI.MaxSelectedFont = Fonts["game"]
UI.MaxSelectedTextX = infopanelX+ipoffsetX+10
UI.MaxSelectedTextY = infopanelY+ipoffsetY+10


---===================Commands Panel and related content============================
commandpanelWidth = 256
commandpanelHeight = 256
commandpanelX = 0
commandpanelY = infopanelY+infopanelHeight
cpoffsetX = 48
cpoffsetY = 48
AddFiller("ui/human/commandpanel.png", commandpanelX, commandpanelY)
----------Placing the command buttons on the grid----------
command = CUIButton:new()
command.X = 110
command.Y = 160 + 11 + 70
command.Style = FindButtonStyle("icon")
UI.SingleTrainingButton = command

UI.TrainingButtons:clear()

-- 3x3 grid of command buttons in the command panel
function AddTrainingButton(x, y)
	command = CUIButton:new_local()
	command.X = x
	command.Y = y
	command.Style = FindButtonStyle("icon")
	UI.TrainingButtons:push_back(command)
end

--units currently being trained------
-- 3x2 (two rows) should be in the infopanel

--first row
AddTrainingButton(infopanelX+ipoffsetX+separation, infopanelY+ipoffsetY+separation)
AddTrainingButton(infopanelX+ipoffsetX+separation*2+iconsize, infopanelY+ipoffsetY+separation)
AddTrainingButton(infopanelX+ipoffsetX+separation*3+iconsize*2, infopanelY+ipoffsetY+separation)
AddTrainingButton(infopanelX+ipoffsetX+separation, infopanelY+ipoffsetY+separation*2+iconsize)
AddTrainingButton(infopanelX+ipoffsetX+separation*2+iconsize, infopanelY+ipoffsetY+separation*2+iconsize)
AddTrainingButton(infopanelY+ipoffsetY+separation*3+iconsize*2, infopanelY+ipoffsetY+separation*2+iconsize)

-- seems like the information progress bar
b = CUIButton:new()
b.X = 110
b.Y = 160 + 11 + 70
b.Style = FindButtonStyle("icon")
UI.UpgradingButton = b

b = CUIButton:new()
b.X = 110
b.Y = 160 + 11 + 70
b.Style = FindButtonStyle("icon")
UI.ResearchingButton = b


--Fix this. No absolute positions !
UI.TransportingButtons:clear()

function AddTransportingButton(x, y)
	b = CUIButton:new_local()
	b.X = x
	b.Y = y
	b.Style = FindButtonStyle("icon")
	UI.TransportingButtons:push_back(b)
end

AddTransportingButton(9, 387)
AddTransportingButton(65, 387)
AddTransportingButton(121, 387)
AddTransportingButton(9, 434)
AddTransportingButton(65, 434)
AddTransportingButton(121, 434)

UI.CompletedBarColorRGB = CColor(48, 100, 4)
UI.CompletedBarShadow = true

UI.ButtonPanel.Buttons:clear()

function AddButtonPanelButton(x, y)
	b = CUIButton:new_local()
	b.X = x
	b.Y = y
	b.Style = FindButtonStyle("icon")
	UI.ButtonPanel.Buttons:push_back(b)
end


--Populating Command Panel
--first row
separation = 4
AddButtonPanelButton(commandpanelX+cpoffsetX, commandpanelY+cpoffsetY)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation+iconsize, commandpanelY+cpoffsetY)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation*2+iconsize*2, commandpanelY+cpoffsetY)
--second row
AddButtonPanelButton(commandpanelX+cpoffsetX, commandpanelY+cpoffsetY+separation*2+iconsize)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation+iconsize, commandpanelY+cpoffsetY+separation+iconsize)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation*2+iconsize*2, commandpanelY+cpoffsetY+separation+iconsize)
--third row
AddButtonPanelButton(commandpanelX+cpoffsetX, commandpanelY+cpoffsetY+separation*3+iconsize*2)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation+iconsize, commandpanelY+cpoffsetY+separation*2+iconsize*2)
AddButtonPanelButton(commandpanelX+cpoffsetX+separation*2+iconsize*2, commandpanelY+cpoffsetY+separation*2+iconsize*2)

UI.ButtonPanel.X = 100 --wtf?
UI.ButtonPanel.Y = 336
UI.ButtonPanel.AutoCastBorderColorRGB = CColor(0, 0, 252)

-- World Map Area
UI.MapArea.X = minimapWidth + 1
UI.MapArea.Y = 16
UI.MapArea.EndX = Video.Width - 2
UI.MapArea.EndY = Video.Height - 2

-- wtf?

UI.StatusLine.TextX = 10
UI.StatusLine.TextY = minimapHeight + infopanelHeight + commandpanelHeight + 20
UI.StatusLine.Width = Video.Width - 16 - 2 - 176
UI.StatusLine.Font = Fonts["game"]


--=========================Resources Panel =============================================-----

resPanelWidth = 840
resPanelHeight = 68
resPanelX = (Video.Width + infopanelWidth)/2 - resPanelWidth/2
resPanelY = Video.Height - resPanelHeight
resPanelSeparation = 100
iconSize = 32 -- resource icon
textOffsetX = 20
textOffsetY = 20 --iconSize/4
dx = 4
--Adding background image
AddFiller("ui/human/statsbar.png", resPanelX, resPanelY)
--Positions of the resources relative to the background image
dy = 10 
goldX = resPanelX + 25 + dx
goldY = resPanelY - dy
woodX = resPanelX + 147 + dx
woodY = resPanelY - dy
oilX = resPanelX + 269 + dx
oilY = resPanelY - dy
foodX = resPanelX + 469 + dx
foodY = resPanelY - dy
scoreX = resPanelX + 592 + dx
scoreY = resPanelY - dy

--using a single file for G(old)W(ood)O(il)M(ana)F(ood)S(core)
-- gold
UI.Resources[1].G = CGraphic:New("ui/gwomfs.png", 32, 32)
UI.Resources[1].IconFrame = 0
UI.Resources[1].IconX = goldX
UI.Resources[1].IconY = goldY
UI.Resources[1].TextX = UI.Resources[1].IconX + iconSize + textOffsetX
UI.Resources[1].TextY = UI.Resources[1].IconY + textOffsetY

-- -- wood
UI.Resources[2].G = CGraphic:New("ui/gwomfs.png", 32, 32)
UI.Resources[2].IconFrame = 1
UI.Resources[2].IconX = woodX --UI.Resources[1].IconX + resPanelSeparation
UI.Resources[2].IconY = woodY --resPanelY
UI.Resources[2].TextX = UI.Resources[2].IconX + iconSize + textOffsetX
UI.Resources[2].TextY = UI.Resources[2].IconY + textOffsetY

-- -- oil
UI.Resources[3].G = CGraphic:New("ui/gwomfs.png", 32, 32)
UI.Resources[3].IconFrame = 2
UI.Resources[3].IconX = oilX --UI.Resources[2].IconX + resPanelSeparation
UI.Resources[3].IconY = oilY --resPanelY
UI.Resources[3].TextX = UI.Resources[3].IconX + iconSize + textOffsetX
UI.Resources[3].TextY = UI.Resources[3].IconY + textOffsetY

-- -- food
UI.Resources[FoodCost].G = CGraphic:New("ui/gwomfs.png", 32, 32)
UI.Resources[FoodCost].IconFrame = 4
UI.Resources[FoodCost].IconX = foodX --UI.Resources[3].IconX + resPanelSeparation
UI.Resources[FoodCost].IconY = foodY --resPanelY
UI.Resources[FoodCost].TextX = UI.Resources[FoodCost].IconX + iconSize + textOffsetX
UI.Resources[FoodCost].TextY = UI.Resources[FoodCost].IconY + textOffsetY

-- -- score
UI.Resources[ScoreCost].G = CGraphic:New("ui/gwomfs.png", 32, 32)
UI.Resources[ScoreCost].IconFrame = 5
UI.Resources[ScoreCost].IconX = scoreX --UI.Resources[FoodCost].IconX + resPanelSeparation
UI.Resources[ScoreCost].IconY = scoreY --resPanelY
UI.Resources[ScoreCost].TextX = UI.Resources[ScoreCost].IconX + iconSize + textOffsetX
UI.Resources[ScoreCost].TextY = UI.Resources[ScoreCost].IconY + textOffsetY

--========================= End of Resources Panel======================

--shift to the left!
UI.NetworkMenuButton.X = 6 + 400
UI.NetworkMenuButton.Y = 2
UI.NetworkMenuButton.Text = "Menu"
UI.NetworkMenuButton.Style = FindButtonStyle("network")
UI.NetworkMenuButton:SetCallback(function() RunGameMenu() end)

UI.NetworkDiplomacyButton.X = 90 + 400
UI.NetworkDiplomacyButton.Y = 2
UI.NetworkDiplomacyButton.Text = "Diplomacy"
UI.NetworkDiplomacyButton.Style = FindButtonStyle("network")
UI.NetworkDiplomacyButton:SetCallback(function() RunDiplomacyMenu() end)


