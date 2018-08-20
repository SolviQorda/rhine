{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Arrowized.Model.Gamestate where

import Arrowized.Model.Tetronimo

import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.ResamplingBuffer.Collect


data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks,
    hold              :: Maybe Tetronimo,
    seed              :: Int,
    score             :: Int,
    difficulty        :: Int,
    paused            :: Bool
} deriving (Eq)

--initial gamestate - want to use RandT
getGamestate :: Gamestate
getGamestate = undefined

printGamestate
  :: (Monad m, Show (Diff time))
  => String
  -> Behaviour m time String
printGamestate gamestate
  = timeInfoOf sinceInit >-> arr show
  >-> arr (("Gamestate: " ++ (gamestate) ++ " has ticked at: ")++)

--specialises the creation of tetronimo to a specific clock
ms500 :: ClSF IO (Millisecond 500) Gamestate String
ms500 = printGamestate $ show $
     Gamestate
        (Tetronimo
        (Pos 1 0)
        (Pos 1 2)
        (Pos 1 3)
        (Pos 1 1)
        LShape
        Zero)
        (Tetronimo
        (Pos 1 0)
        (Pos 1 2)
        (Pos 1 3)
        (Pos 1 1)
        LShape
        Zero)
        []
        (Nothing)
        (13)
        (0)
        (1)
        (False)

--initial gamestate would be provided by a BehaviourExcept where timeOf Sincestart would be 0.

ms1200 :: ClSF IO (Millisecond 1200) Play Empty
ms1200 =  playStates

data Play = Running | Gameover
  deriving (Eq, Show)

playStates
  :: (Monad m, TimeDomain time, Diff time ~ Float)
  => BehaviourF m time Play Empty
playStates = do
  try $ timer 7 >>> arr (const Running)
  try $ timer 9 >>> arr (const Gameover)
  playStates

printEverySecond :: Show a => ClSF IO (Millisecond 1000) a ()
printEverySecond = arrMCl print

main :: IO ()
main = flow $
  ms500 @@ waitClock ||@ scheduleMillisecond @|| ms1200 @@ waitClock
  >-- collect -@- concurrently -->
  printEverySecond @@ waitClock
