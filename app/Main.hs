module Main where

import Fe.Buffer (Buffer(Buffer))
import Fe.Exit (Exit(Exit))
import Fe.Keypress (Keypress(Keypress), keypress)
import Fe.Range (Coord(Coord), Range(Range), coordCol, rangeEnd, rangeStart)

import qualified Fe.Buffer as Buffer
import qualified Fe.Cursor as Cursor

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Exts (IsList(..))
import Graphics.Vty (Image, Key(..), Modifier, Vty)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Yi.Rope (YiString)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Graphics.Vty as Vty
import qualified Yi.Rope as Yi
import qualified Yi.Rope.Extra as Yi

import Debug.Trace

data Mode
  = NormalMode
  | InsertMode
  deriving (Eq, Show)

main :: IO ()
main = do
  vty <- Vty.mkVty =<< Vty.standardIOConfig
  fe (main' vty)
  Vty.shutdown vty

main'
  :: Vty
  -> (Exit -> IO ())
  -> Event Exit
  -> MomentIO (Behavior Image, Image -> IO ())
main' vty fireExit eExit = mdo
  eKeypress :: Event Keypress <-
    keypress vty

  eMode :: Event Mode <- do
    let eN2I :: Event Mode
        eN2I = InsertMode <$ filterE f eKeypress
         where
          f :: Keypress -> Bool
          f = \case
            Keypress (KChar c) [] -> c `elem` ("iIaAsS" :: String)
            _ -> False

    let eI2N :: Event Mode
        eI2N = NormalMode <$ filterE f eKeypress
         where
          f :: Keypress -> Bool
          f = \case
            Keypress KEsc _ -> True
            _ -> False

    let switchMode :: Mode -> Event Mode
        switchMode = \case
          NormalMode -> eN2I
          InsertMode -> eI2N

    switchE eN2I (switchMode <$> eMode)

  bMode :: Behavior Mode <-
    stepper NormalMode eMode

  eCursorF :: Event (Range -> Range) <- do
    let eN :: Event (Range -> Range)
        eN = filterJust (f <$> bBuffer <@> eKeypress)
         where
          f :: Buffer -> Keypress -> Maybe (Range -> Range)
          f buffer = \case
            Keypress KDown  _ -> Just (Cursor.down  (Buffer.rope buffer))
            Keypress KLeft  _ -> Just (Cursor.left  (Buffer.rope buffer))
            Keypress KRight _ -> Just (Cursor.right (Buffer.rope buffer))
            Keypress KUp    _ -> Just (Cursor.up    (Buffer.rope buffer))
            _                 -> Nothing

    let eI :: Event (Range -> Range)
        eI = filterJust (f <$> eKeypress)
         where
          f :: Keypress -> Maybe (Range -> Range)
          f = \case
            Keypress KEnter [] ->
              Just (\(Range (Coord r _) _) ->
                Range (Coord (r+1) 0) (Coord (r+1) 1))
            Keypress (KChar _) [] ->
              Just (\r -> r & rangeStart . coordCol +~ 1
                            & rangeEnd   . coordCol +~ 1)
            _ -> Nothing

    let switchCursorF :: Mode -> Event (Range -> Range)
        switchCursorF = \case
          NormalMode -> eN
          InsertMode -> eI

    switchE eN (switchCursorF <$> eMode)

  bCursor :: Behavior Range <-
    accumB (Range (Coord 0 0) (Coord 0 1)) eCursorF

  eRopeF :: Event (YiString -> YiString) <- do
    let eI :: Event (YiString -> YiString)
        eI = filterJust (f <$> bCursor <@> eKeypress)
         where
          f :: Range -> Keypress -> Maybe (YiString -> YiString)
          f range = \case
            Keypress KEnter [] ->
              Just (Yi.insertAt (range ^. rangeStart) (Yi.singleton '\n'))
            Keypress (KChar c) [] ->
              Just (Yi.insertAt (range ^. rangeStart) (Yi.singleton c))
            _ -> Nothing

    let eN :: Event (YiString -> YiString)
        eN = never

    let switchRopeF :: Mode -> Event (YiString -> YiString)
        switchRopeF = \case
          NormalMode -> eN
          InsertMode -> eI

    switchE eN (switchRopeF <$> eMode)

  bBuffer :: Behavior Buffer <-
    accumB (Buffer "foobar\n  bazzy\na") (Buffer.overRope <$> eRopeF)

  let render :: Buffer -> Mode -> Range -> Image
      render buffer mode cursors = Vty.vertCat
        [ Vty.string Vty.defAttr (show buffer)
        , Vty.string Vty.defAttr (show mode)
        , Vty.string Vty.defAttr (show cursors)
        ]

  let bImage :: Behavior Image
      bImage = render <$> bBuffer <*> bMode <*> bCursor

  -- For now, <Esc> exits.
  on_
    (whenE ((== NormalMode) <$> bMode)
      (filterE (== Keypress KEsc []) eKeypress))
    (fireExit Exit)

  pure (bImage, Vty.update vty . Vty.picForImage)

fe
  :: ((Exit -> IO ())
      -> Event Exit
      -> MomentIO (Behavior a, a -> IO ()))
  -> IO ()
fe moment = do
  exitingVar :: MVar () <-
    newEmptyMVar

  network :: EventNetwork <-
    compile $ do
      (eExit, fireExit) <- newEvent
      (bEditor, render) <- moment fireExit eExit

      liftIO . render =<< valueB bEditor
      on bEditor render

      on_ eExit (void (tryPutMVar exitingVar ()))

  actuate network

  readMVar exitingVar


class On f where
  on :: f a -> (a -> IO ()) -> MomentIO ()
  on_ :: f a -> IO () -> MomentIO ()

instance On Event where
  on e f = reactimate (f <$> e)
  on_ e f = reactimate (f <$ e)

instance On Behavior where
  on b f = do
    e <- changes b
    reactimate' (fmap f <$> e)

  on_ b f = do
    e <- changes b
    reactimate' ((f <$) <$> e)

leftmost :: [Event a] -> Event a
leftmost = foldr (unionWith const) never
