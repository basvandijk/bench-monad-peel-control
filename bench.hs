{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude hiding (catch)
import Control.Exception ( Exception, SomeException, throwIO )
import qualified Control.Exception as E ( mask, bracket, bracket_ )
import Data.Typeable
import Control.Monad (join)

-- from criterion:
import Criterion.Main

-- from transformers:
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

-- from monad-peel:
import qualified Control.Exception.Peel as MP
import qualified Control.Monad.IO.Peel  as MP

-- from monad-control:
import qualified Control.Exception.Control as MC
import qualified Control.Monad.IO.Control  as MC

-- from MonadCatchIO-transformers:
import qualified Control.Monad.CatchIO as MCIO


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ benchAll "bracket"   benchBracket  MCIO.bracket  MP.bracket   MC.bracket
  , benchTwo "liftIOOp"  benchLiftIOOp               MP.liftIOOp  MC.liftIOOp
  , benchAll "bracket_"  benchBracket_ MCIO.bracket_ MP.bracket_  MC.bracket_
  , benchTwo "liftIOOp_" benchLiftIOOp_              MP.liftIOOp_ MC.liftIOOp_
  , benchAll "catch"     benchCatch    MCIO.catch    MP.catch     MC.catch
  , benchAll "try"       benchTry      MCIO.try      MP.try       MC.try
  , benchTwo "mask"      benchMask                   mpMask       MC.mask
  ]

benchAll name bnch mcio peel mndCtrl = bgroup name
  [ bench "MonadCatchIO"  $ bnch mcio
  , bench "monad-peel"    $ bnch peel
  , bench "monad-control" $ bnch mndCtrl
  ]

benchTwo name bnch peel mndCtrl = bgroup name
  [ bench "monad-peel"    $ bnch peel
  , bench "monad-control" $ bnch mndCtrl
  ]

--------------------------------------------------------------------------------
-- Monad stack
--------------------------------------------------------------------------------

type M a = ReaderT Int (StateT Bool (WriterT String (MaybeT IO))) a

type R a = IO (Maybe ((a, Bool), String))

runM :: Int -> Bool -> M a -> R a
runM r s m = runMaybeT (runWriterT (runStateT (runReaderT m r) s))

exe :: M a -> R a
exe = runM 0 False


--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchBracket   bracket   = exe $              bracket  nop (\_ -> nop)  (\_ -> nop)
benchLiftIOOp  liftIOOp  = exe $ liftIOOp  (E.bracket  nop (\_ -> nop)) (\_ -> nop)
benchBracket_  bracket_  = exe $              bracket_ nop nop          nop
benchLiftIOOp_ liftIOOp_ = exe $ liftIOOp_ (E.bracket_ nop nop)         nop
benchCatch     catch     = exe $ catch throwE (\E -> nop)
benchTry       try       = exe $ try throwE :: R (Either E ())


-- benchBracketIO_ = exe $ MC.bracketIO_ nop nop nop

benchMask :: (((forall a. M a -> M a) -> M ()) -> M ()) -> R ()
benchMask mask = exe $ mask $ \restore -> nop >> restore nop >> nop


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

nop :: Monad m => m ()
nop = return ()

data E = E deriving (Show, Typeable)

instance Exception E

throwE :: MonadIO m => m ()
throwE = liftIO $ throwIO E

mpMask :: MP.MonadPeelIO m => ((forall a. m a -> m a) -> m b) -> m b
mpMask f = do
  k <- MP.peelIO
  join $ liftIO $ E.mask $ \restore -> k $ f $ MP.liftIOOp_ restore


-- The End ---------------------------------------------------------------------
