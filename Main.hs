{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

import Server
import Node
import MathOP

main :: IO ()
main = do
    putStrLn "hello world!"
    runServer 19900
