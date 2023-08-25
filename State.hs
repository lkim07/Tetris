-- Game State

module State(State(..), initialGameState, resetGameState, gameOverState) where

import Playfield
import Piece
import System.Random
import Data.List

data State = State
    { well :: Well
    , time :: Float
    , deltaTime :: Float
    , secondsToNextMove :: Float
    , piece :: Piece
    , piecePos :: (Int, Int)
    , randomSeed :: StdGen
    , score :: Int
    , accelerate :: Bool
    , linesCleared :: Int
    , nextPiece :: Piece
    , gameOver :: Bool
    , currentLevel :: Int
    , holdPiece :: Maybe Piece
    , scoreboard :: [Int]
    } deriving (Show)

initialGameState :: State
initialGameState = State
    { well = emptyWell
    , time = 0
    , deltaTime = 0
    , secondsToNextMove = 0
    , piece = tetrominoO
    , piecePos = (0, 0)
    , randomSeed = mkStdGen 0 -- found better way!
    , score = 0
    , accelerate = False
    , linesCleared = 0
    , nextPiece = tetrominoL
    , gameOver = False
    , currentLevel = 1
    , holdPiece = Nothing
    , scoreboard = [0,0,0,0,0]
    }

-- Resets a game state, maintaining the random seed and scoreboard
resetGameState :: State -> State
resetGameState s = initialGameState {randomSeed = randomSeed s, scoreboard = scoreboard s}

-- clears the state of the game and updates gameOver and scoreboard accordingly
gameOverState :: [Int] -> State
gameOverState topScores = initialGameState {gameOver = True, scoreboard = topScores}

