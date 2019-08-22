{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP

runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)
