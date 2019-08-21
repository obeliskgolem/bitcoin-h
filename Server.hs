{-# LANGUAGE OverloadedStrings #-}

module Server where

import MathOP


runServer :: Port -> IO ()
runServer port  = do
    putStrLn $ "bitcoin-h server running at port " ++ $ show port