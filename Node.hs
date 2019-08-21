{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP

runNode :: Port -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ $ show port
