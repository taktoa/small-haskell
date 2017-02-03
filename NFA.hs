{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad.Identity  (Identity)

import           Data.Hashable           (Hashable)

import qualified Data.List               as List

import           Data.Text               (Text)
import qualified Data.Text               as T

import           Data.Set                (Set)
import qualified Data.Set                as Set

import qualified Data.HashMap.Strict     as HM
import qualified Data.Vector             as V

import           Data.Aeson              (Value (..))
import qualified Data.Aeson              as Aeson
import           Data.Aeson.BetterErrors as ABE

import           Utils

data CustomError = MkCustomError Text deriving (Show)

type JParseT m a = ABE.ParseT CustomError m a
type JParse a = JParseT Identity a
type JEither a = Either CustomError a

throwJP :: (Monad m) => Text -> JParseT m a
throwJP = MkCustomError .> throwCustomError

throwJE :: Text -> JEither a
throwJE = MkCustomError .> Left

type NFAState = Int
type NFASymbol = Char
type NFAString = Text
type a ==> b = HashMap a b

newtype CTF string st = MkCTF { runCTF :: string ==> (st ==> st) }

parseCTF :: JParse (CTF NFAString Text)
parseCTF = eachInArray parseTriple <#> processTriples .> MkCTF
  where
    parseTriple :: JParse (NFAString, (Text, Text))
    parseTriple = let kt s = key s asText
                      (sym, src, tgt) = (kt "symbol", kt "source", kt "target")
                  in (,) <$> sym <*> ((,) <$> src <*> tgt)
    processTriples :: [(NFAString, (Text, Text))]
                   -> (NFAString ==> (Text ==> Text))
    processTriples = map (second singleton)
                     .> HM.fromListWith (<>)
                     .> HM.map HM.fromList

relabelCTF :: forall str s. (Ord s) => CTF str s -> ([s], CTF str NFAState)
relabelCTF = runCTF
             .> foldMap (gather id)
             .> List.sort
             .> (\xs -> (xs, helper xs))
  where
    helper :: [s] -> CTF str NFAState
    helper = undefined
    gather :: (Container f, Monoid (f a)) => (b -> a) -> (a ==> b) -> f a
    gather conv = let comb k v x = x <> singleton k <> singleton (conv v)
                  in HM.foldrWithKey comb mempty

data ConcreteNFA' string sym st
  = MkConcreteNFA
    { _states     :: [st]
    , _alphabet   :: [sym]
    , _start      :: st
    , _accepting  :: [st]
    , _transition :: CTF string st
    }

type ConcreteNFA = ConcreteNFA' NFAString NFASymbol NFAState

parseConcreteNFA :: JParse ConcreteNFA
parseConcreteNFA = undefined

data NFA sym st
  = MkNFA
    { _start      :: st
    , _accepting  :: st -> Bool
    , _transition :: sym -> st -> st
    }

main :: IO ()
main = pure ()
