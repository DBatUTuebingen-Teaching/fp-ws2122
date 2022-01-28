module Guess where

import Text.Read (readMaybe)
import System.Random (randomRIO)

{-

Rules:

The computer picks a random number between 1 and 2^10

1. The "player" has 13 attempts to guess the right number.

2. After every guess, the computer tells wether the guessed number was
too small or too large.

3. If the player can not guess the correct number in 13 attempts, he
loses and the computer shames him.

4. If the player manages to guess the correct number, the computer
tries to shame him by comparing his performance to that of binary
search.

-}

type Guess a = GameState -> IO (a, GameState)

data GameState = GameState
    Int -- ^ Target number
    Int -- ^ Attemps so far

-- Ask the player to give a number, try to parse it and ask again on failure
prompt :: IO Int
prompt = 

-- Given the target number n and a guess, print a hint to the player
hint :: Int -> Int -> IO ()
hint n guess =

-- Game loop: ask for guess until game is over, return outcome
gameLoop :: Guess Bool
gameLoop s =

main :: IO ()
main =
    -- Create a random number

    -- Enter game loop with initial game state

    -- Print outcome / score
