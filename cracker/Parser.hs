module Parser (parseInput, readInput, readAllInput) where

import           Control.Monad
import           Control.Monad.Catch

import           Control.Lens

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Types

parseInput :: MonadThrow m => Text -> m Input
parseInput text = case parse $ T.unpack text of Left e  -> throwParseError e
                                                Right x -> return x
  where
    parse :: String -> Either Text Input
    parse = fixme

readInput :: IO Input
readInput = getLine >>= parseInput . T.pack

readAllInput :: (Input -> IO ()) -> IO ()
readAllInput f = forever $ readInput >>= f
