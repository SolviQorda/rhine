{-# LANGUAGE Arrows         #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators #-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Control.Concurrent (threadDelay)
import GHC.TypeLits

-- fixed-vector
import Data.Vector.Sized (Vector, fromList)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Step
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.Util

{- |
A clock ticking every 'n' milliseconds,
in real time.
Since 'n' is in the type signature,
it is ensured that when composing two signals on a 'Millisecond' clock,
they will be driven at the same rate.

The tag of this clock is 'Bool',
where 'True' represents successful realtime,
and 'False' a lag.
-}
newtype Millisecond (n :: Nat) = Millisecond (RescaledClockS IO (Step n) UTCTime Bool)
-- TODO Consider changing the tag to Maybe Double

instance Clock IO (Millisecond n) where
  type TimeDomainOf (Millisecond n) = UTCTime
  type Tag          (Millisecond n) = Bool
  startClock (Millisecond cl) = startClock cl


-- | This clock simply sleeps 'n' milliseconds after each tick.
--   The current time is measured, but no adjustment is made.
--   Consequently, the tag is constantly 'False',
--   since the clock will accumulate the computation time as lag.
sleepClock :: KnownNat n => Millisecond n
sleepClock = sleepClock_ Step
  where
    sleepClock_ :: Step n -> Millisecond n
    sleepClock_ cl = Millisecond $ RescaledClockS cl $ const $ do
      now <- getCurrentTime
      let ticks = arrM_ $ do
            threadDelay $ fromInteger $ stepsize cl * 1000
            getCurrentTime
      return
        ( ticks *** arr (const False)
        , now
        )


-- TODO Test whether realtime detection really works here,
--  e.g. with a getLine signal
-- | A more sophisticated implementation that measures the time after each tick,
--   and waits for the remaining time until the next tick.
--   If the next tick should already have occurred,
--   the tag is set to 'False', representing a failed real time attempt.
waitClock :: KnownNat n => Millisecond n
waitClock = Millisecond $ RescaledClockS Step $ \_ -> do
  initTime <- getCurrentTime
  let
    runningClock = arrM $ \(n, ()) -> do
      beforeSleep <- getCurrentTime
      let
        diff :: Double
        diff      = realToFrac $ beforeSleep `diffUTCTime` initTime
        remaining = fromInteger $ n * 1000 - round (diff * 1000000)
      threadDelay remaining
      now         <- getCurrentTime -- TODO Test whether this is a performance penalty
      return (now, diff > 0)
  return (runningClock, initTime)


-- TODO It would be great if this could be directly implemented in terms of downsampleStep
downsampleMillisecond
  :: (KnownNat n, Monad m)
  => ResamplingBuffer m (Millisecond k) (Millisecond (n * k)) a (Vector n a)
downsampleMillisecond = collect >>-^ arr (fromList >>> assumeSize)
  where
    assumeSize = fromMaybe $ error $ unwords
      [ "You are using an incorrectly implemented schedule"
      , "for two Millisecond clocks."
      , "Use a correct schedule like downsampleMillisecond."
      ]

-- | Two 'Millisecond' clocks can always be scheduled deterministically.
scheduleMillisecond :: Schedule IO (Millisecond n1) (Millisecond n2)
scheduleMillisecond = Schedule startSchedule'
  where
    startSchedule' (Millisecond cl1) (Millisecond cl2)
      = startSchedule (rescaledScheduleS scheduleStep) cl1 cl2
