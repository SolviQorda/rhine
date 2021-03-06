{- | Internals for 'FRP.Rhine.Gloss'.
You probably won't need this module.
-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module FRP.Rhine.Gloss.Internals where

-- base
import qualified Control.Category as Category
import Data.Functor.Identity (Identity)

-- dunai
import Control.Monad.Trans.MSF.Reader (readerS, runReaderS)

-- gloss
import Graphics.Gloss.Interface.Pure.Game

-- rhine
import FRP.Rhine hiding (readerS, runReaderS)
import FRP.Rhine.Clock.Select


-- * Clocks

-- | The error message that gets thrown when you try to start a @gloss@ app with 'flow'.
errMsg :: String
errMsg =  "You cannot start gloss apps with FRP.Rhine.flow. "
       ++ "Use FRP.Rhine.Gloss.flowGloss instead."

-- | The clock that ticks whenever a @gloss@ event occurs.
data GlossEventClock = GlossEventClock

instance Clock m GlossEventClock where
  type TimeDomainOf GlossEventClock = ()
  type Tag          GlossEventClock = Event
  startClock _ = error errMsg

-- | The clock that ticks for every @gloss@ simulation step,
--   but only shows the time delta in the tag.
--   Usually, you don't need this clock, but rather 'GlossSimulationClock'.
data GlossSimulationClock_ = GlossSimulationClock_

instance Clock m GlossSimulationClock_ where
  type TimeDomainOf GlossSimulationClock_ = ()
  type Tag          GlossSimulationClock_ = Float
  startClock _ = error errMsg

-- | The clock that ticks for every @gloss@ simulation step.
--   Use 'withProperSimClock' to transform to 'GlossSimulationClock_'.
data GlossSimulationClock = GlossSimulationClock

instance Clock m GlossSimulationClock where
  type TimeDomainOf GlossSimulationClock = Float
  type Tag          GlossSimulationClock = ()
  startClock _ = error errMsg

-- | To use all features of the 'SyncSF' framework,
--   write your synchronous stream function on the 'GlossSimulationClock'
--   and then use this function to transform it.
withProperSimClock
  :: Monad m
  => SyncSF m GlossSimulationClock  a b
  -> SyncSF m GlossSimulationClock_ a b
withProperSimClock syncsf = readerS
  $ (intermingle *** Category.id) >>> runReaderS syncsf
  where
    intermingle :: Monad m => MSF m (TimeInfo GlossSimulationClock_) (TimeInfo GlossSimulationClock)
    intermingle = proc TimeInfo {tag} -> do
      let sinceTick = tag
      absolute <- sumS -< sinceTick
      let sinceStart = absolute
      returnA          -< TimeInfo { tag = (), .. }

-- | The clock that ticks for every @gloss@ graphics output.
data GlossGraphicsClock = GlossGraphicsClock

instance Clock m GlossGraphicsClock where
  type TimeDomainOf GlossGraphicsClock = ()
  type Tag          GlossGraphicsClock = ()
  startClock _ = error errMsg

-- | A schedule you can't actually use, for internal purposes.
glossSchedule :: Schedule Identity (SelectClock GlossEventClock a) GlossSimulationClock_
glossSchedule = error errMsg
