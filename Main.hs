-- | The main game loop.  You do not need to edit this file!  It
--   simply takes your initGameState, gamePrompt, and gameStep and
--   uses them to run a simple loop that waits for user input and uses
--   your provided functions to decide what to do next and what to
--   print.
module Main where

import Game (gamePrompt, gameStep, initGameState)
import System.Console.Haskeline
import System.Random

main :: IO ()
main = do
  g <- getStdGen
  let nums = randomRs (0, 1000000) g
  putStrLn "Input board size (integer 3-26)."
  sz <- getLine 
  runInputT defaultSettings (gameLoop gamePrompt gameStep (initGameState nums sz))

-- The gameLoop doesn't care what type your game state is.
gameLoop :: (g -> String) -> (String -> g -> (String, Maybe g)) -> g -> InputT IO ()
gameLoop prompt step = go
 where
  go curState = do
    minput <- getInputLine (prompt curState)
    case minput of
      Nothing -> return ()
      Just input -> do
        let (msg, newState) = step input curState
        outputStrLn msg
        maybe (return ()) go newState