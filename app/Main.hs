module Main (main) where

import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, defaultFileServerSettings, ssIndices)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Monoid

main :: IO ()
main = do
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  let prt = maybe 8080 id $ herokuPort
  putStrLn $ "Serving on http://localhost:" <> show prt
  Warp.run prt app

  where
  app = staticApp (defaultFileServerSettings ".")

