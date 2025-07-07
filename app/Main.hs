module Main where

import ROCm.HIP

main :: IO ()
main = do
  x <- hipGetDeviceCount
  putStrLn $ show x
