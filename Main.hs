{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server
import Node
import MathOP

main :: IO ()
main = do
    putStrLn "hello world!"
    runServer 19900
