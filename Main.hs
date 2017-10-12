module Main where

import Control.Concurrent
import Control.Monad
import Graphics.Vty (Image, Key(..), Modifier)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Vty as Vty

main :: IO ()
main = do
  exitingVar :: MVar () <-
    newEmptyMVar

  network :: EventNetwork <-
    compile $ do
      (eExiting, fireExiting) <- newEvent

      moment (fireExiting ()) eExiting

      reactimate (void (tryPutMVar exitingVar ()) <$ eExiting)

  actuate network

  readMVar exitingVar

--------------------------------------------------------------------------------
-- Network description

moment :: IO () -> Event () -> MomentIO ()
moment fireExiting eExiting = mdo
  eKeyPress :: Event (Key, [Modifier]) <-
    slate eExiting bImage

  let bImage :: Behavior Image
      bImage = pure (Vty.string Vty.defAttr "Hello, world!")

  -- For now, <Esc> exits.
  reactimate (fireExiting <$ filterE (== (KEsc, [])) eKeyPress)

--------------------------------------------------------------------------------
-- slate plugin

slate :: Event () -> Behavior Image -> MomentIO (Event (Key, [Modifier]))
slate eExiting bImage = do
  vty <- liftIO (Vty.mkVty =<< Vty.standardIOConfig)

  -- eKeyPress :: Event (Key, [Modifier])
  -- fireKeyPress :: (Key, [Modifier]) -> IO ()
  (eKeyPress, fireKeypress) <-
    newEvent

  liftIO . void . forkIO . forever $ do
    Vty.nextEvent vty >>= \case
      Vty.EvKey key mods -> fireKeypress (key, mods)
      _ -> pure ()

  let render :: Image -> IO ()
      render image = Vty.update vty . Vty.picForImage

  liftIO . render =<< valueB bImage
  reactimate (render <$> bImage <@ eKeyPress)

  reactimate (Vty.shutdown vty <$ eExiting)

  pure eKeyPress
