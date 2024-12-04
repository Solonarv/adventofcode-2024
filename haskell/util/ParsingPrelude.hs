module ParsingPrelude
  ( Parser
  , chainl, noeol, singleDigit, char2digitBase, scanManySkipping, nonspace
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer, onecharify
) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Void
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Set as Set
import Text.Megaparsec hiding (Stream(..), State(..))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, binary, octal, hexadecimal, scientific, float, signed, lexeme)
import Text.Megaparsec.Stream (Token)

import Util (guarding, within)

type Parser = Parsec Void String

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl el op = do
  e1 <- el
  pairs <- many ((,) <$> op <*> el)
  pure (foldl' (\a (f, e) -> f a e) e1 pairs)

noeol :: (MonadParsec e s m, Token s ~ Char) => m ()
noeol = void (takeWhileP (Just "non-newline whitespace") (\c -> c /= '\n' && c /= '\r' && isSpace c))

singleDigit :: (MonadParsec e s m, Token s ~ Char, Num a) => Int -> m a
singleDigit (min 36 -> b) = token (char2digitBase b) expect
  where
    expect = Set.singleton (Label digitMsg)
      <> if b > 10 then Set.singleton (Label letterMsg) else Set.empty
    digitMsg = 'A' :| "SCII digit between 0 and " ++ show (min 10 b - 1)
    letterMsg = 'A' :| "SCII letter between 'A' and " ++ show (chr (ord 'A' + b - 11))

char2digitBase :: Num a => Int -> Char -> Maybe a
char2digitBase (min 36 -> b) = fmap fromIntegral . guarding (< b) <=< \c -> guard (isAscii c) *> let o = ord c in asum
      [ guard (within '0' '9' c) $> (o - ord '0')
      , guard (within 'a' 'z' c) $> (o - ord 'a' + 10)
      , guard (within 'A' 'Z' c) $> (o - ord 'A' + 10)
      ]

-- | transform a parser that (might) consume multiple tokens
-- to consume exactly one token if it succeeds, using @lookAhead@
onecharify :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
onecharify p = lookAhead p <* takeP Nothing 1

scanManySkipping :: MonadParsec e s m => m a -> m void -> m [a]
scanManySkipping needle skip = go
  where
    go = optional (try needle) >>= \case
        Nothing -> (skip *> go) <|> pure []
        Just x -> (x:) <$> go

nonspace :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
nonspace = satisfy (not . isSpace)