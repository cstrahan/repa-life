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

module PatternParsers.PlainText
    ( file
    ) where

import Control.Applicative ((<*))
import Data.Array.Repa ((:.) (..))
import qualified Data.Set as Set
import Text.Parsec

import LifePattern
import PatternParsers.Utils

file :: Stream s m Char => ParsecT s u m LifePattern
file = do
  name <- header <* eol
  comments <- fmap (filter (/= "")) $ sepEndBy comment eol
  startLine <- getSourceLine
  liveSet <- fmap Set.unions $ sepEndBy (patternLine startLine) eol

  return $ LifePattern name comments liveSet (offsetToCenter liveSet)

header :: Stream s m Char => ParsecT s u m String
header = do
  char '!'
  string "Name: "
  many $ noneOf "\n\r\t"

comment :: Stream s m Char => ParsecT s u m String
comment = do
  char '!'
  many $ noneOf "\n\r\t"

patternLine :: Stream s m Char => Int -> ParsecT s u m LiveCellSet
patternLine startLine = do
  line <- fmap (\l -> l - startLine) getSourceLine
  living <- many liveCell
  return . foldr (makeSet line) Set.empty $ zip [0..] living

    where
      liveCell = (char '.' >> return False)
             <|> (char 'O' >> return True)

      makeSet y (x, alive) living =
          if alive
          then Set.insert (x, y) living
          else living

getSourceLine :: Monad m => ParsecT s u m Int
getSourceLine = fmap sourceLine getPosition

test = "!Name: Glider\n!comment followed by empty line\n!\n.O.\n..O\nOOO\n"
