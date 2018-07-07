module Fixture where

example :: IO ()
example = let x = 1; y = x + (2 :: Int) in print y
