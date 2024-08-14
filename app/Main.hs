module Main (main) where

import Text.Show.Pretty (ppShow)

import ParsePCRE

main :: IO ()
main = do
  s <- getContents
  case parsePCRE s of
    Just e ->
      putStrLn $ ppShow e
    Nothing ->
      putStrLn "No parse"
