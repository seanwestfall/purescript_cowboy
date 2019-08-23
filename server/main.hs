#!/usr/bin/env cabal
{- cabal:

build-depends: base, wai-app-static, warp
default-language: Haskell2010

-}
module Main (main) where

import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, defaultFileServerSettings, ssIndices)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)


main :: IO ()
main = do
  putStrLn "Serving on http://localhost:8080"
  herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
  let prt = maybe 8080 id $ herokuPort
  Warp.run prt app

  where
  app = staticApp (defaultFileServerSettings ".")
