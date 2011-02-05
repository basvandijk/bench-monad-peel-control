module Main where

-- from criterion:
import Criterion.Main

-- from mtl:
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- from monad-peel:
import qualified Control.Exception.Peel as Peel

-- from monad-control:
import qualified Control.Exception.Control as MonadControl

main :: IO ()
main = defaultMain
         [ bench "Peel.bracket"         $ b Peel.bracket
         , bench "MonadControl.bracket" $ b MonadControl.bracket
         ]

type M a = ReaderT Int (StateT Bool (WriterT String IO)) a

runM :: Int -> Bool -> M a -> IO ((a, Bool), String)
runM r s m = runWriterT (runStateT (runReaderT m r) s)

b bracket =
  runM 0 False $
    bracket
      (ask >>= \r -> modify (\s -> s || even r) >> tell "Hello" >> liftIO (return 1))
      (\n -> liftIO (return n) >>= \x -> tell " " >> get >>= \s -> tell "World" >> ask >>= \r -> put (odd r && s))
      (\n -> get >>= \s -> ask >>= \r -> tell (" state=" ++ (show s)) >> tell (" env=" ++ (show r)) >> liftIO (return n))
