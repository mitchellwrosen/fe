module Fe.Cursor
  ( down
  , left
  , right
  , up
  ) where

import Fe.Range

import Yi.Rope (YiString)

import qualified Yi.Rope as Yi
import qualified Yi.Rope.Extra as Yi

down :: YiString -> Range -> Range
down s = \case
  Range (Coord r c) _
    | r == lastRow s -> Range (Coord r c) (Coord r (c+1))
    | otherwise -> Range (Coord (r+1) c') (Coord (r+1) (c'+1))
   where
    c' :: Int
    c' = min c (lastCol (Yi.line (r+1) s))

left :: YiString -> Range -> Range
left s = \case
  x@(Range (Coord 0 0) _) -> x
  Range (Coord r 0) _ -> Range (Coord (r-1) c) (Coord (r-1) (c+1))
   where
    c :: Int
    c = lastCol (Yi.line (r-1) s)
  Range (Coord r c) _ -> Range (Coord r (c-1)) (Coord r c)

right :: YiString -> Range -> Range
right s = \case
  x@(Range (Coord r c) _) ->
    if c == lastCol (Yi.line r s)
      then
        if r == lastRow s
          then x
          else Range (Coord (r+1) 0) (Coord (r+1) 1)
      else Range (Coord r (c+1)) (Coord r (c+2))

up :: YiString -> Range -> Range
up s = \case
  x@(Range (Coord r c) _)
    | r == 0 -> x
    | otherwise -> Range (Coord (r-1) c') (Coord (r-1) (c'+1))
   where
    c' :: Int
    c' = min c (lastCol (Yi.line (r-1) s))

lastCol :: YiString -> Int
lastCol line
  | Yi.head line == Just '\n' = 0
  | Yi.last line == Just '\n' = len - 2
  | otherwise                 = len - 1
 where
  len = Yi.length line

lastRow :: YiString -> Int
lastRow = Yi.countNewLines
