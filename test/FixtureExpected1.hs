module Fixture where

example :: IO ()
example = let xxx = 1; y = xxx + (2 :: Int) in print y
