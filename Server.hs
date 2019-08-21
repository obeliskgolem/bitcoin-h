{-# LANGUAGE OverloadedStrings #-}

module Server where

import MathOP
import Servant.API

type NodeAPI = "nodes" :> Get '[JSON] [Node]


runServer :: Port -> IO ()
runServer port  = do
    putStrLn $ "bitcoin-h server running at port " ++ $ show port