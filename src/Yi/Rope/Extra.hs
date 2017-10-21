module Yi.Rope.Extra
  ( line
  , insertAt
  ) where

import Fe.Range

import Data.Monoid
import Yi.Rope (YiString)

import qualified Yi.Rope as Yi

line :: Int -> YiString -> YiString
line r xs =
  case Yi.splitAtLine r xs of
    (_, ys) ->
      case Yi.splitAtLine 1 ys of
        (zs, _) -> zs

insertAt :: Coord -> YiString -> YiString -> YiString
insertAt (Coord row col) s ws =
  case Yi.splitAtLine row ws of
    (ws, xs) ->
      case Yi.splitAtLine 1 xs of
        (xs, zs) ->
          case Yi.splitAt col xs of
            (xs, ys) -> ws <> xs <> s <> ys <> zs
