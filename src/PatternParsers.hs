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

module PatternParsers
    ( parseFile
    , tryParsePlainText

    , PatternParseResult (SuccessfulParse, ParseError, UnknownFormat, FileError)
    ) where

import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (catch)
import System.FilePath (takeExtension)
import qualified Text.Parsec as P
import Text.Parsec.Text

import LifePattern
import qualified PatternParsers.PlainText as PlainText

data PatternParseResult = SuccessfulParse LifePattern
                        | ParseError String P.ParseError
                        | UnknownFormat String
                        | FileError IOException
                        | IncorrectFormat
                          deriving Show

tryParsePlainText :: FilePath -> IO PatternParseResult
tryParsePlainText fname =
    if (takeExtension fname) == ".cells"
    then do
      contents <- T.readFile fname
      return $ case P.parse PlainText.file fname contents of
                 Left err -> ParseError "plain text" err
                 Right pat -> SuccessfulParse pat
    else return IncorrectFormat

parseFile :: FilePath -> IO PatternParseResult
parseFile fname = do
  attempts <- mapM tryParser parsers
  return $ foldr selectAttempt (UnknownFormat fname) attempts

    where

      tryParser parser = parser fname `catch` (return . FileError)

      selectAttempt IncorrectFormat = id
      selectAttempt result = const result

parsers = [tryParsePlainText]
