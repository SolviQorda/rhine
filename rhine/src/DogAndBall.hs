{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module DogAndBall where

import System.Random
import Text.Printf

import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.ClSF.Except
import qualified FRP.Rhine.ResamplingBuffer.FIFO as F

import qualified Data.VectorSpace as V
import Control.Monad
import Control.Arrow

type EventClock  = StdinClock
type SimClock    = Millisecond 10
type StatusClock = Millisecond 500

type Ball    = (Double, Double, Double)
type BallVel = (Double, Double, Double)

statusMsg :: ClSF IO StatusClock Ball ()
statusMsg = arrMCl $ \ (x, y, z) ->
  printf "%.2f %.2f %.2f\n" x y z

freeFall :: Monad m => BallVel -- the start velocity
        -> BehaviourF m UTCTime () Ball
freeFall v0 = arr (const (0, 0, -9.81)) >>> integralFrom v0 >>> integral

startVel :: ClSF IO StdinClock () BallVel
startVel = arrMCl $ const $ do
 velX <- randomRIO (-10, 10)
 velY <- randomRIO (-10, 10)
 velZ <- randomRIO (  3, 10)
 return (velX, velY, velZ)

waiting :: Monad m => ClSF (ExceptT BallVel m)
                       SimClock (Maybe BallVel) Ball
waiting = throwMaybe >>> arr (const V.zeroVector)

falling :: Monad m
  => BallVel --start velocity
  -> ClSF (ExceptT () m) SimClock
       (Maybe BallVel) Ball
falling v0 = proc _ -> do
  pos <- freeFall v0 -< ()
  let (_, _, height) = pos
  throwMaybe         -< guard $ height < 0
  returnA            -< pos

ballModes :: ClSFExcept IO SimClock (Maybe BallVel) Ball void
ballModes = do
  v0 <- try waiting
  once_ $ putStrLn "Catch!"
  try $ falling v0
  once_ $ putStrLn "Caught!"
  ballModes

ball :: ClSF IO SimClock (Maybe BallVel) Ball
ball = safely ballModes

downsampleSimToStatus :: ResBuf IO SimClock StatusClock Ball Ball
downsampleSimToStatus = downsampleMillisecond
                   >>-^ arr head

startVelRh :: Rhine IO StdinClock () BallVel
startVelRh = startVel @@ StdinClock

ballRh :: Rhine IO SimClock (Maybe BallVel) Ball
ballRh = ball @@ waitClock

statusRh :: Rhine IO StatusClock Ball ()
statusRh = statusMsg @@ waitClock

simToStatus :: ResamplingPoint IO SimClock StatusClock Ball Ball
simToStatus = downsampleSimToStatus -@- scheduleMillisecond

ballStatusRh :: Rhine IO (SeqClock IO SimClock StatusClock) (Maybe BallVel) ()
ballStatusRh = ballRh >--simToStatus--> statusRh

main :: IO ()
main = flow $ startVelRh
  >-- F.fifo -@- concurrently --> ballStatusRh
