{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Arrow
import           Data.Monoid

import           Control.Monad
import           Control.Monad.Catch

import           Control.Concurrent

import           Data.Char

import           Data.Ratio
import           Numeric.Natural

import           Text.Printf

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import qualified System.Clock        as Clock

fixme :: a
fixme = error "Fix me!"

fixmeIO :: IO a
fixmeIO = return fixme

type Nat = Natural

data CResult = CrackSuccess
             | CrackCancel
             | CrackFailure
             deriving (Eq, Enum, Read, Show)

newtype Time = Time Double
             deriving (Eq, Enum, Ord, Num, Real, Fractional, Read, Show)

type Size   = Nat
type Length = Nat
type Offset = Nat

diagonal :: a -> (a, a)
diagonal x = (x, x)

(<.>) :: Functor f => (a -> b) -> (t -> f a) -> t -> f b
f <.> g = \x -> f <$> g x
infixr 8 <.>

liftText :: (String -> String) -> Text -> Text
liftText f = T.pack . f . T.unpack

liftTextM :: Functor f => (String -> f String) -> Text -> f Text
liftTextM f = T.pack <.> (f . T.unpack)

tshow :: Show s => s -> Text
tshow = T.pack . show

showTID :: ThreadId -> Text
showTID = T.pack . drop 9 . show

data Clock = ClockWall
           | ClockCPU
           | ClockThread
           deriving (Eq, Enum, Read, Show)

getTimeGeneric :: Clock -> IO Time
getTimeGeneric = fromTimeSpec <.> Clock.getTime . fromClock
  where
    fromClock ClockWall   = Clock.Monotonic
    fromClock ClockCPU    = Clock.ProcessCPUTime
    fromClock ClockThread = Clock.ThreadCPUTime
    fromTimeSpec ts = let seconds     = fromIntegral $ Clock.sec ts
                          nanoseconds = fromIntegral $ Clock.nsec ts
                      in fromRational $ seconds + nanoseconds % 1000000000

getTime, getCPUTime, getThreadCPUTime :: IO Time
getTime          = getTimeGeneric ClockWall
getCPUTime       = getTimeGeneric ClockCPU
getThreadCPUTime = getTimeGeneric ClockThread

getPrefixLength :: Text -> Length
getPrefixLength = fromIntegral . T.length . liftText go
  where
    go :: String -> String
    go []       = []
    go ('.':xs) = go xs
    go (x:xs)   = x : go xs

setStringPosition :: Integer -> Text -> Text
setStringPosition n = liftText (reverse . go n . reverse)
  where
    modulus = 26
    increaseChar :: Integer -> Char -> Char
    increaseChar x = let delta = fromIntegral $ x `mod` modulus
                     in toEnum . (+ delta) . fromEnum
    go :: Integer -> String -> String
    go _ []     = []
    go 0 xs     = xs
    go i (x:xs) = let (q, r) = quotRem i modulus
                  in increaseChar r x : go q xs

incrementString :: Text -> Maybe Text
incrementString = liftTextM $ mapReverse incrementChar <.> handleValid
  where
    mapReverse :: (a -> b) -> [a] -> [b]
    mapReverse f = reverse . map f . reverse
    incrementChar :: Char -> Char
    incrementChar = toEnum . succ . fromEnum
    handleValid :: String -> Maybe String
    handleValid str | isMax str     = Nothing
                    | isInvalid str = Nothing
                    | otherwise     = Just str
    isInvalid :: String -> Bool
    isInvalid = not . all isLower
    isMax :: String -> Bool
    isMax "y"        = True
    isMax "z"        = True
    isMax ('z':rest) = isMax rest
    isMax _          = False

getSubrange :: Size -> Size -> [(Size, Size)]
getSubrange unknown threads = fixme

printStartUser :: Text -> IO ()
printStartUser username = T.putStrLn $ "Start " <> username

printThreadStart :: ThreadId -> Text -> Offset -> Text -> IO ()
printThreadStart tid username offset initialPW = T.putStrLn out
  where
    out = mconcat [ "Thread ", showTID tid, ": "
                  , "Start ", username, " at ", tshow offset, " "
                  , "(", initialPW, ")" ]

printThreadResult :: ThreadId -> Integer -> CResult -> IO ()
printThreadResult tid hashCount result = T.putStrLn out
  where
    resultText :: Text
    resultText = case result of CrackSuccess -> "found"
                                CrackCancel  -> "cancelled"
                                CrackFailure -> "end"
    out = mconcat [ "Thread ", showTID tid, ": "
                  , "Stop after ", tshow hashCount, " iterations "
                  , "(", resultText, ")"]

printSummary :: Text -> Text -> Integer -> Time -> Time -> Bool -> IO ()
printSummary username password hashes wall cpu result = T.putStrLn out
  where
    resultText :: Text
    resultText = if result
                 then " is " <> password <> " "
                 else " not found "
    coerceDouble :: Real n => n -> Double
    coerceDouble = fromRational . toRational
    disp :: Real n => n -> Text
    disp n = T.pack $ printf "%.2f" $ coerceDouble n
    newline = "\n"
    paren xs = "(" <> mconcat xs <> ")"
    out = mconcat [ "Password for ", username, resultText
                  , paren [ tshow hashes, " hashes in ", disp wall, " seconds" ]
                  , newline
                  , "Total CPU time: ", disp cpu, " seconds."
                  , newline
                  , "CPU usage: ", disp $ cpu / wall, "x" ]

main :: IO ()
main = return ()
