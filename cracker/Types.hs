{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens
import           Control.Monad.Catch

import           Data.Text           (Text)


data ParseError = ParseError Text
                deriving (Eq, Show)

instance Exception ParseError

throwParseError :: MonadThrow m => Text -> m a
throwParseError = throwM . ParseError

data Input = Input { _inputUser   :: Text
                   , _inputHash   :: Text
                   , _inputPrefix :: Text
                   , _inputLength :: Integer
                   } deriving (Eq, Read, Show)

makeLenses ''Input
