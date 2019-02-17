module Main where

import           Data.Yaml          (decodeFileThrow)
import           Lib                (run)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (f,host,port) = case args of
        []        -> ("config.yml", "0.0.0.0", 3000)
        [x]       -> (x, "0.0.0.0", 3000)
        [x,h]     -> (x, h, 3000)
        (x:h:p:_) -> (x,h,read p)

  srvs <- decodeFileThrow f
  run host port srvs
