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

module PatternParsers.Utils
    ( eol
    , offsetToCenter
    , PatternParser
    , patternWarning
    , parsePatternFile
    ) where

import Control.Applicative hiding ((<|>), optional)
import Control.Monad.Writer
import qualified Data.Set as Set
import Text.Parsec

import LifePattern (LifePattern, LiveCellSet)

type PatternParser s m = ParsecT s [String] m

patternWarning :: Stream s m Char => String -> PatternParser s m ()
patternWarning msg = modifyState (++ [msg])

parsePatternFile :: Stream s m Char => PatternParser s m LifePattern -> FilePath -> s
                 -> m (Either ParseError (LifePattern, [String]))
parsePatternFile parser fname contents =
    runPT parser' [] fname contents

    where
      parser' = (,) <$> parser <*> getState

eol :: Stream s m Char => ParsecT s u m ()
eol = (string "\r\n" >> return ())
      <|> (char '\n' >> optional (char '\r'))

-- | Find the center of the pattern
offsetToCenter liveSet =
    let (xmax, ymax) = Set.foldr (\(a, b) (a', b') -> (max a a', max b b')) (0, 0) liveSet
        cx = xmax `div` 2
        cy = ymax `div` 2
    in (-cx, -cy)
