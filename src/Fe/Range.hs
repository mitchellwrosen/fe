module Fe.Range
  ( Range(Range)
  , rangeStart
  , rangeEnd
  , Coord(Coord)
  , coordRow
  , coordCol
  ) where

import Control.Lens

data Range
  = Range !Coord !Coord
  deriving Show

data Coord
  = Coord !Int !Int
  deriving Show

rangeStart :: Lens' Range Coord
rangeStart = lens (\(Range x _) -> x) (\(Range _ y) x -> Range x y)

rangeEnd :: Lens' Range Coord
rangeEnd = lens (\(Range _ y) -> y) (\(Range x _) y -> Range x y)

coordRow :: Lens' Coord Int
coordRow = lens (\(Coord x _) -> x) (\(Coord _ y) x -> Coord x y)

coordCol :: Lens' Coord Int
coordCol = lens (\(Coord _ y) -> y) (\(Coord x _) y -> Coord x y)
