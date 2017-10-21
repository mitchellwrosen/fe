module Fe.Buffer where

import Yi.Rope (YiString)

import qualified Yi.Rope as Yi

data Buffer
  = Buffer !YiString
  deriving Show

rope :: Buffer -> YiString
rope (Buffer x) = x

overRope :: (YiString -> YiString) -> Buffer -> Buffer
overRope f (Buffer x) = Buffer (f x)

length :: Buffer -> Int
length (Buffer x) = Yi.length x

line :: Int -> Buffer -> YiString
line r (Buffer xs) =
  case Yi.splitAtLine r xs of
    (_, ys) ->
      case Yi.splitAtLine 1 ys of
        (zs, _) -> zs

numLines :: Buffer -> Int
numLines (Buffer x) = Yi.countNewLines x + 1
