
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing.Activity

import qualified Data.Text as T

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        }

-----------------------------------------------------
-- Initialization

main =
    activityOf
        [ pixelsByUnit 20
        , onKeyDown' $ const $ keyMap . T.toUpper
        , onMouseDown SetCell
        ]
        initial update draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    }

-----------------------------------------------------
-- Update state

data Action =
          NextStep
        | SetCell Point
        deriving (Eq, Show)

keyMap :: T.Text -> Maybe Action
keyMap "N" = Just NextStep
keyMap _   = Nothing

update :: Action -> UpdateM Game Action ()
update NextStep =               -- Next generation
    modify $
        \game -> game{ gmBoard = nextGeneration (gmBoard game) }
update (SetCell point) =  do    -- Set live/dead cells
    game <- get
    let pos = pointToPos point
        brd = gmBoard game
    put game{ gmBoard = setCell (not $ cellIsLive pos brd) pos brd }

pointToPos :: Point -> Pos
pointToPos (x, y) = (round x, round y)

-----------------------------------------------------
-- View state

draw game =
    drawBoard (gmBoard game)

