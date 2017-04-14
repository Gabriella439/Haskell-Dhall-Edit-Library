{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Dhall.Edit where

import Brick
import Brick.Focus (FocusRing)
import Brick.Types (BrickEvent(..))
import Brick.Widgets.Edit (Editor)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State (State)
import Data.Monoid (Monoid(..), Sum(..), (<>))
import Data.Text (Text)
import Dhall.Core (Expr(..))
import Dhall.TypeCheck (X)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Numeric.Natural (Natural)

import qualified Brick
import qualified Brick.Focus
import qualified Brick.Widgets.Edit
import qualified Control.Monad.Trans.State
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

-- TODO: Don't use 99 for Viewport

ui :: Widget ()
ui = str "Hello, world!"

data Status = Status {}

initialStatus :: Status
initialStatus = Status {}

data Fold a =
    forall s . Fold (Natural -> s -> Event -> EventM Natural s) s (FocusRing Natural-> s -> a)

instance Functor Fold where
    fmap k (Fold step begin done) = Fold step begin done'
      where
        done' b s = k (done b s)

instance Applicative Fold where
    pure r = Fold (\_ s _ -> pure s) () (\_ _ -> r)

    Fold stepL beginL doneL <*> Fold stepR beginR doneR = Fold step begin done
      where
        step n (sL, sR) e = do
            sL' <- stepL n sL e
            sR' <- stepR n sR e
            return (sL', sR')

        begin = (beginL, beginR)

        done b (sL, sR) = doneL b sL (doneR b sR)

instance Monoid a => Monoid (Fold a) where
    mempty = pure mempty

    mappend = liftA2 mappend

newtype W = W { getW :: Widget Natural }

instance Monoid W where
    mempty = W emptyWidget

    mappend (W l) (W r) = W (l <=> r)

newtype UI a = UI
    { getUI :: (Sum Natural, State Natural (Fold (W, a)))
    }

instance Functor UI where
    fmap k (UI x) = UI (fmap (fmap (fmap (fmap k))) x)

instance Applicative UI where
    pure x = UI (pure (pure (pure (pure x))))

    UI l <*> UI r = UI (liftA2 (liftA2 (liftA2 (<*>))) l r)

instance Monoid a => Monoid (UI a) where
    mempty = pure mempty

    mappend = liftA2 mappend

modifyWidget :: (Widget Natural -> Widget Natural) -> UI a -> UI a
modifyWidget f (UI x) = UI (fmap (fmap (fmap adapt)) x)
  where
    adapt (W widget, y) = (W (f widget), y)

edit :: Text -> UI Text
edit startingText = UI (Sum 1, do
    n <- Control.Monad.Trans.State.get
    Control.Monad.Trans.State.put (n + 1)

    let begin :: Editor Text Natural
        begin =
            Brick.Widgets.Edit.editorText
                    n
                    (Brick.str . Data.Text.unpack . Data.Text.intercalate "\n")
                    (Just 1)
                    startingText

        step
            :: Natural
            -> Editor Text Natural
            -> Event
            -> EventM Natural (Editor Text Natural)
        step n' editor event =
            if n == n'
            then Brick.Widgets.Edit.handleEditorEvent event editor
            else return editor

        done :: FocusRing Natural -> Editor Text Natural -> (W, Text)
        done r editor =
            ( W (Brick.Focus.withFocusRing r (Brick.Widgets.Edit.renderEditor) editor)
            , Data.Text.intercalate "\n" (Brick.Widgets.Edit.getEditContents editor)
            )

    return (Fold step begin done) )


dhallEdit :: Expr X X -> UI (Expr X X)
dhallEdit (TextLit builder) = do
    let lazyText   = Data.Text.Lazy.Builder.toLazyText builder
    let strictText = Data.Text.Lazy.toStrict lazyText
    strictText' <- edit strictText
    return (
        let lazyText' = Data.Text.Lazy.fromStrict strictText'
            builder'  = Data.Text.Lazy.Builder.fromLazyText lazyText'
        in TextLit builder' )
dhallEdit (RecordLit kvs) = do
    let process key val = do
            -- TODO: Insert label
            let adapt widget =
                        str (Data.Text.Lazy.unpack key)
                    <=> (str "↳ " <+> widget)
            modifyWidget adapt (dhallEdit val)
    kvs' <- Data.Map.traverseWithKey process kvs
    return (RecordLit kvs')
