-- Renderer - module that maps a game state into a gloss picture

module Renderer(render) where

import State
import Playfield
import Graphics.Gloss
import Piece
import Data.Maybe
import Data.List
import Data.Ord

-- Let's start with rendering an empty well.

-- Playfield dimensions
cellSize = 35
padding = (768 - (20 * cellSize)) `quot` 2

wellWidth = 10 * cellSize
wellHeight = 20 * cellSize

wallWidth = wellWidth + 2 * padding
wallHeight = wellHeight + 2 * padding

-- Colors
wellColor = black
wallColor = dark (dark blue)

-- Convert from playfield coordinate to screen coordinate
playfieldToScreen :: (Int, Int) -> (Int, Int)
playfieldToScreen (px, py) = (sx, sy) where
  sx = (px * cellSize) `quot` 2
  sy = (11 * cellSize) + (py * cellSize) `quot` 2

-- Function that renders a single cell
renderCell :: (Int, Int) -> Color -> Picture
renderCell (px, py) col = translate (fromIntegral sx) (fromIntegral sy) (color col (rectangleSolid sz sz))
  where
    sx = fst transformed
    sy = snd transformed
    sz = 0.9 * (fromIntegral cellSize)
    transformed = playfieldToScreen (px, py)

-- Renders Well playfield to Picture
renderWell :: Well -> Picture
renderWell well =
  pictures (map cellToPicture (coordCells well))
    where
      cellToPicture (px,py,c)
        | py > (-3)  = pictures []
        | c == Empty = pictures []
        | otherwise  = renderCell (px, py) (cellColor c)

-- Coordinates for displaying each type of tetromino in next and hold
-- pos is used to determine the y-axis coordinate of where the tetromino is displayed on the screen
-- This is done so that both next and hold can use these functions even though they have different y-axis positions
coordsTetrominoI :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoI pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (324.0, 145.0 - pos), (346.0, 145.0 - pos)]

coordsTetrominoO :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoO pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (280.0, 167.0 - pos), (302.0, 167.0 - pos)]

coordsTetrominoS :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoS pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (302.0, 167.0 - pos), (324.0, 167.0 - pos)]

coordsTetrominoZ :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoZ pos = [(302.0, 145.0 - pos), (324.0, 145.0 - pos), (280.0, 167.0 - pos), (302.0, 167.0 - pos)]

coordsTetrominoT :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoT pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (324.0, 145.0 - pos), (302.0, 167.0 - pos)]

coordsTetrominoJ :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoJ pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (324.0, 145.0 - pos), (280.0, 167.0 - pos)]

coordsTetrominoL :: (Fractional a, Fractional b) => b -> [(a, b)]
coordsTetrominoL pos = [(280.0, 145.0 - pos), (302.0, 145.0 - pos), (324.0, 145.0 - pos), (324.0, 167.0 - pos)]

-- Renders the tetromino
renderTetromino :: Piece -> [(Float, Float)] -> Picture
renderTetromino piece coords = pictures ([translate x y $ color (colorOfPiece piece) $ rectangleSolid 20 20
                                          | (x,y) <- coords])

-- Renders the Piece to Picture
renderPiece :: Piece -> Float -> Picture
renderPiece piece pos
  | nameOfPiece piece == "tetrominoI" = renderTetromino piece (coordsTetrominoI pos)
  | nameOfPiece piece == "tetrominoO" = renderTetromino piece (coordsTetrominoO pos)
  | nameOfPiece piece == "tetrominoS" = renderTetromino piece (coordsTetrominoS pos)
  | nameOfPiece piece == "tetrominoZ" = renderTetromino piece (coordsTetrominoZ pos)
  | nameOfPiece piece == "tetrominoT" = renderTetromino piece (coordsTetrominoT pos)
  | nameOfPiece piece == "tetrominoJ" = renderTetromino piece (coordsTetrominoJ pos)
  | nameOfPiece piece == "tetrominoL" = renderTetromino piece (coordsTetrominoL pos)

-- Renders the hold Piece to Picture
renderHoldPiece :: Maybe a -> State -> Float -> Picture
renderHoldPiece p s pos
  | isNothing p = pictures []
  | otherwise = renderPiece (fromMaybe (piece s) (holdPiece s)) pos

-- Gets the score from the scoreboard at index i
getScore :: State -> Int -> String
getScore s i =
  let topScores = sortBy (comparing Down) (scoreboard s)
  in show (topScores !! i)

-- Game State renderer
render :: State -> Picture
render gameState
  | gameOver gameState = pictures [ displayGameOver,  displayScores1, displayScores2, displayScores3, displayScores4, displayScores5 ]
  | otherwise = pictures [ walls, playfield, activePiece, displayScore, displayLinesCleared, displayNextPieceText, displayNextPieceImage, displayHoldPieceText, displayHoldPieceImage, displayLevel ]
    where
      walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
      playfield = pictures
        [ color wellColor (rectangleSolid (fromIntegral wellWidth) (fromIntegral wellHeight))
        , renderWell (well gameState)
        ]
      activePiece = renderWell (renderActivePiece (piece gameState) (piecePos gameState) emptyWell)
      displayScore = translate (-600.0) (200.0) (scale 0.4 0.4 (pictures [playerScore]))
        where
            playerScore = color white (Text scoreText)
            scoreText = "SCORE: " ++ (show (score gameState))
      displayLinesCleared = translate (-600.0) 100.0 (scale 0.4 0.4 (pictures [linesClearedCount]))
        where
          linesClearedCount = color white (Text linesClearedText)
          linesClearedText = "LINES: " ++ show (linesCleared gameState)
      displayNextPieceText = translate 230.0 200.0 (scale 0.4 0.4 (pictures [upNextPiece]))
        where
          upNextPiece = color white (Text nextText)
          nextText = "NEXT: "
      displayNextPieceImage = scale 1.5 1.5 (pictures [nextPieceImage])
        where
          nextPieceImage = renderPiece (nextPiece gameState) 0.0
      displayHoldPieceText = translate 230.0 100.0 (scale 0.4 0.4 (pictures [currHoldPiece]))
        where
          currHoldPiece = color white (Text holdText)
          holdText = "HOLD: "
      displayHoldPieceImage = scale 1.5 1.5 (pictures [holdPieceImage])
        where
          holdPieceImage = renderHoldPiece (holdPiece gameState ) gameState 70.0
      displayGameOver = translate (-300.0) 200.0 (scale 0.8 0.8 (pictures [color white (Text "GAME OVER!")]))
      displayLevel = translate (-600.0) 0.0 (scale 0.4 0.4 (pictures [level]))
        where
          level = color white (Text countLevel)
          currentLevel gameState = if linesCleared gameState == 0 then 1 else fromIntegral (linesCleared gameState) / 3
          countLevel = "LEVEL: " ++ show (ceiling (currentLevel gameState))
      displayScores1 = translate (-100.0) 100.0 (scale 0.4 0.4 (pictures [ranking1]))
       where
        ranking1 = color white (Text rankingScores1)
        rankingScores1 = "First: " ++ getScore gameState 0
      displayScores2 = translate (-100.0) 0.0 (scale 0.4 0.4 (pictures [ranking2]))
       where
        ranking2 = color white (Text rankingScores2)
        rankingScores2 = "Second: " ++ getScore gameState 1
      displayScores3 = translate (-100.0) (-100.0) (scale 0.4 0.4 (pictures [ranking3]))
       where
        ranking3 = color white (Text rankingScores3)
        rankingScores3 = "Third: " ++ getScore gameState 2
      displayScores4 = translate (-100.0) (-200.0) (scale 0.4 0.4 (pictures [ranking4]))
       where
        ranking4 = color white (Text rankingScores4)
        rankingScores4 = "Fourth: " ++ getScore gameState 3
      displayScores5 = translate (-100.0) (-300.0) (scale 0.4 0.4 (pictures [ranking5]))
       where
        ranking5 = color white (Text rankingScores5)
        rankingScores5 = "Fifth: " ++ getScore gameState 4
