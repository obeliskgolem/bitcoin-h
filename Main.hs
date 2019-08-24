{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

import Server
import Node
import MathOP

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    putStrLn "hello world!"
    runServer 19900
