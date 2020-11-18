{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( brnfck
    ) where

import System.Environment (getArgs)
import qualified Control.Monad.Trans.State as S
import Control.Lens.TH
import Control.Lens
import Control.Lens.At
import Data.Function ((&))
import Control.Monad.Trans.Class
import Control.Monad
import Data.Char
import Data.Foldable

data LangState = LangState {
    _array :: [Int],
    _pointer :: Int
} deriving (Show)

makeLenses ''LangState

handleChar :: Char -> S.StateT LangState IO ()
handleChar = \case
    '<' ->
        S.modify (\s -> s & pointer %~ (-) 1)

    '>' ->
        S.modify (\s -> s & pointer %~ (+ 1))

    '+' ->
        S.modify (\s -> s & array . ix (s ^. pointer) %~ (+1))

    '-' ->
        S.modify (\s -> s & array . ix (s ^. pointer) %~ (-) 1)

    '.' ->
        S.get <&> (\s -> s ^? array . ix (s ^. pointer)) <&> fmap toEnum >>= traverse_ (lift .putChar)

    ',' ->
        lift getChar >>= \c -> S.modify (\s -> s & array . ix (s ^. pointer) .~ ord c)
    
    '[' -> pure ()

    ']' -> pure ()

    _ -> pure ()

handleArgs :: [String] -> IO ()
handleArgs [] = return ()
handleArgs (x:_) = do
    file <- readFile x
    let initState = LangState (replicate 100 0) 0
    S.evalStateT (forM_ file handleChar) initState
    return ()

brnfck :: IO ()
brnfck = do
    args <- getArgs
    handleArgs args
