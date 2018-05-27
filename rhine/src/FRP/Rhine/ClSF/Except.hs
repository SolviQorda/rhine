{-# LANGUAGE Arrows       #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.ClSF.Except
  ( module FRP.Rhine.ClSF.Except
  , module X
  , safe, safely, Empty, exceptS, runMSFExcept, currentInput
  )
  where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except as X
import Control.Monad.Trans.Reader

-- dunai
import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.Except hiding (try, once, once_, throwOn, throwOn', throwS)
-- TODO Find out whether there is a cleverer way to handle exports
import qualified Control.Monad.Trans.MSF.Except as MSFE

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ClSF.Except.Util

-- * Types

{- | A synchronous exception-throwing signal function.
It is based on a @newtype@, 'MSFExcept',
to exhibit a monad interface /in the exception type/.
`return` then corresponds to throwing an exception,
and `(>>=)` is exception handling.
(For more information, see the documentation of 'MSFExcept'.)

* @m@:  The monad that the signal function may take side effects in
* @cl@: The clock on which the signal function ticks
* @a@:  The input type
* @b@:  The output type
* @e@:  The type of exceptions that can be thrown
-}
type ClSFExcept m cl a b e = MSFExcept (ReaderT (TimeInfo cl) m) a b e

{- | A clock polymorphic 'ClSFExcept'.
Any clock with time domain @time@ may occur.
-}
type BehaviourFExcept m time a b e
  = forall cl. time ~ Time cl => ClSFExcept m cl a b e

-- | Compatibility to U.S. american spelling.
type BehaviorFExcept m time a b e = BehaviourFExcept m time a b e



commuteExceptReader :: ExceptT e (ReaderT r m) a -> ReaderT r (ExceptT e m) a
commuteExceptReader a = ReaderT $ \r -> ExceptT $ runReaderT (runExceptT a) r

runClSFExcept :: Monad m => ClSFExcept m cl a b e -> ClSF (ExceptT e m) cl a b
runClSFExcept = liftMSFPurer commuteExceptReader . runMSFExcept

-- | Enter the monad context in the exception
--   for |ClSF|s in the |ExceptT| monad.
--   The 'ClSF' will be run until it encounters an exception.
try :: Monad m => ClSF (ExceptT e m) cl a b -> ClSFExcept m cl a b e
try = MSFE.try . liftMSFPurer commuteReaderExcept

-- | Within the same tick, perform a monadic action,
--   and immediately throw the value as an exception.
once :: Monad m => (a -> m e) -> ClSFExcept m cl a b e
once f = MSFE.once $ lift . f

-- | A variant of |once| without input.
once_ :: Monad m => m e -> ClSFExcept m cl a b e
once_ = once . const

-- | Immediately throw the exception on the input.
throwS :: Monad m => ClSF (ExceptT e m) cl e a
throwS = arrMCl throwE

-- | Throw the given exception when the 'Bool' turns true.
throwOn :: Monad m => e -> ClSF (ExceptT e m) cl Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

-- | Variant of 'throwOn', where the exception can vary every tick.
throwOn' :: Monad m => ClSF (ExceptT e m) cl (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()

-- | When the input is @Just e@, throw the exception @e@.
throwMaybe :: Monad m => ClSF (ExceptT e m) cl (Maybe e) (Maybe a)
throwMaybe = proc me -> case me of
  Nothing -> returnA -< Nothing
  Just e  -> throwS  -< e

-- | Advances a single tick with the given Kleisli arrow,
--   and then throws an exception.
step :: Monad m => (a -> m (b, e)) -> ClSFExcept m cl a b e
step f = MSFE.step $ lift . f