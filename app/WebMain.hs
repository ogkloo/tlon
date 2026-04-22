module Main where

import System.Environment (getArgs)
import Tlon.Web.Server

main :: IO ()
main = do
  args <- getArgs
  let options = parseOptions args
  putStrLn ("Starting tlon-web on http://127.0.0.1:" ++ show (webPort options))
  runWebServer (webPort options) (webDebug options)

data WebOptions = WebOptions
  { webPort :: Int,
    webDebug :: Bool
  }

parseOptions :: [String] -> WebOptions
parseOptions =
  go (WebOptions 8080 False)
  where
    go options args =
      case args of
        [] -> options
        "--port" : rawPort : remaining ->
          go options {webPort = read rawPort} remaining
        "--debug" : remaining ->
          go options {webDebug = True} remaining
        [rawPort] ->
          options {webPort = read rawPort}
        _ : remaining ->
          go options remaining
