module Main where

import Board

main = do
  let board = mkBoard [[1,2,3,4,5,6,7]
                      ,[1,2,3,3,3,4,6]
                      ,[1,1,1,1,2,2,2]
                      ,[1,2,3,4,5,5,7]
                      ,[2,3,5,1,1,3,7]
                      ,[2,3,4,4,1,3,7]]
  putStrLn $ show board
  putStrLn "--"

  let b1m = mark 3 board
  putStrLn $ show b1m
  putStrLn "--"

  let b1f = fill b1m
  putStrLn $ show $ b1f
  putStrLn "--"

  let b2m = mark 3 b1m
  putStrLn $ show b2m
  putStrLn "--"

  let b2f = fill b2m
  putStrLn $ show $ b2f
  putStrLn "--"
