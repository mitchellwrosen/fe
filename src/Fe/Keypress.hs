module Fe.Keypress
  ( Keypress(Keypress)
  , keypress
  ) where

import Control.Concurrent
import Control.Monad
import Graphics.Vty (Key, Modifier, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Vty as Vty

data Keypress
  = Keypress !Key ![Modifier]
  deriving Eq

keypress :: Vty -> MomentIO (Event Keypress)
keypress vty = do
  (eKeypress, fireKeypress) <-
    newEvent

  liftIO . void . forkIO . forever $ do
    Vty.nextEvent vty >>= \case
      Vty.EvKey key mods -> fireKeypress (Keypress key mods)
      _ -> pure ()

  pure eKeypress
