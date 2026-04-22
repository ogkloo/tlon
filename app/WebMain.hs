module Main where

import System.Environment (getArgs)
import Tlon.Web.Server

main :: IO ()
main = do
  args <- getArgs
  let port = parsePort args
  putStrLn ("Starting tlon-web on http://127.0.0.1:" ++ show port)
  runWebServer port

parsePort :: [String] -> Int
parsePort args =
  case args of
    ["--port", rawPort] -> read rawPort
    [rawPort] -> read rawPort
    _ -> 8080
