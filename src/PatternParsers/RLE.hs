{-
Copyright 2012 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}

module PatternParsers.RLE
    ( file
    ) where

import Control.Applicative hiding (many, (<|>))
import qualified Data.Set as Set
import Text.Parsec

import qualified LifePattern as LP
import PatternParsers.Utils

data RleMetadata = Comment String
                 | Name String
                 | Offset (Int, Int)
                 | Width Int
                 | Height Int
                 | Unknown
                   deriving Show

file :: Stream s m Char => ParsecT s u m LP.LifePattern
file = do
  metadata <- header
  liveCells <- pattern (0, 0) Set.empty
  comments <- sepEndBy (many $ noneOf "\n\r") eol

  let lp = foldr setMetadata
                 (LP.LifePattern "" [] liveCells (offsetToCenter liveCells))
                 metadata
  return $ lp {LP.comments = (LP.comments lp ++ comments)}

header :: Stream s m Char => ParsecT s u m [RleMetadata]
header = (++) <$> many hashLine <*> sizeLine

hashLine :: Stream s m Char => ParsecT s u m RleMetadata
hashLine = do
  char '#'
  type_ <- letter
  spaces
  meat <- case type_ of
            'C' -> Comment <$> many (noneOf "\n\r")
            'c' -> Comment <$> many (noneOf "\n\r")
            'N' -> Name <$> many (noneOf "\n\r")
            'O' -> (Comment . ("Author: " ++)) <$> many (noneOf "\n\r")
            'P' -> Offset <$> offset
            'R' -> Offset <$> offset
            -- Error on any file that has a rules line
            'r' -> unexpected "rules not supported"
            _   -> return Unknown
  eol
  return meat

offset :: Stream s m Char => ParsecT s u m (Int, Int)
offset = do
  x <- number
  skipMany1 space
  y <- number
  return (x, y)

number :: Stream s m Char => ParsecT s u m Int
number = read <$> many1 digit

sizeLine :: Stream s m Char => ParsecT s u m [RleMetadata]
sizeLine = sepBy attribute (skipMany (oneOf " \t") >> char ',' >> spaces) <* eol
    where
      attribute = width <|> height <|> rules

      width = do
        char 'x'
        spaces
        char '='
        spaces
        fmap Width number

      height = do
        char 'y'
        spaces
        char '='
        spaces
        fmap Height number

      rules = do
        string "rule"
        spaces
        char '='
        spaces
        (string "B3/S23" <|> string "b3/s23" <?> "rules not supported")
        return Unknown

pattern :: Stream s m Char => (Int, Int) -> LP.LiveCellSet -> ParsecT s u m LP.LiveCellSet
pattern (x, y) liveCells = (char '!' >> return liveCells)
                           <|> (skipMany1 space >> pattern (x, y) liveCells)
                           <|> sequence
    where
      sequence = do
        count <- option 1 number
        tag <- oneOf "bo$"

        case tag of

          -- dead cells
          'b' -> pattern (x + count, y) liveCells

          -- live cells
          'o' -> pattern (x + count, y) (insert x y count liveCells)

          -- next line
          '$' -> pattern (0, y + count) liveCells

      insert x y 0 = id
      insert x y count = Set.insert (x, y) . insert (x + 1) y (count - 1)

setMetadata :: RleMetadata -> LP.LifePattern -> LP.LifePattern
setMetadata (Comment c) lp = lp {LP.comments = (LP.comments lp) ++ [c]}
setMetadata (Name n) lp = lp {LP.name = n}
setMetadata (Offset o) lp = lp {LP.offset = o}
setMetadata _ lp = lp
