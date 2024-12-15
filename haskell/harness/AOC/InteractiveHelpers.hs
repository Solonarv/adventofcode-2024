module AOC.InteractiveHelpers where

import Data.List qualified as List
import Data.Char (isSpace)
import Data.Bifunctor (first)

import AOC.Solution
import ParsingPrelude qualified
import Text.Megaparsec (parse, errorBundlePretty, eof)
import qualified System.Console.ANSI as Ansi
import System.Exit (die)
import System.IO

parseNicely :: String -> ParsingPrelude.Parser a -> String -> Either String a
parseNicely loc p input = first errorBundlePretty . parse (p <* eof) loc . List.dropWhile isSpace . List.dropWhileEnd isSpace $ input

getTestInput :: Int -> Solution i a b -> i
getTestInput n sln = let
  (_, txt, _) = processTest (tests sln !! n)
  fname = "<test input " <> show n <> ">"
  in case parseNicely fname (decodeInput sln) txt of
    Left err -> error err
    Right i -> i


die' :: String -> IO void
die' s = do
  Ansi.hSetSGR stderr [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red ]
  die s