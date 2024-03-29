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

module LifePattern where

import qualified Data.Set as Set

type LiveCellSet = Set.Set (Int, Int)

data LifePattern = LifePattern
    { name :: String
    , comments :: [String]
    , liveCells :: LiveCellSet
    , offset :: (Int, Int)
    } deriving Show

isCellLive :: LifePattern -> (Int, Int) -> Bool
isCellLive pattern (x, y) =
    let (ox, oy) = offset pattern
    in Set.member (x - ox, y - oy) . liveCells $ pattern
