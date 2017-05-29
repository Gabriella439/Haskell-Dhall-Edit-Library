{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Brick
import Dhall.Edit
import Data.Monoid (Sum(..))
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Control.Monad.Trans.State
import qualified Brick
import qualified Brick.Focus
import qualified Data.Text.Lazy.IO
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Graphics.Vty.Attributes
import qualified Options.Generic

-- TODO: Don't use 99 for Viewport

main :: IO ()
main = do
    file <- Options.Generic.getRecord "Dhall configuration file editor"
    lazyText <- Data.Text.Lazy.IO.readFile file
    expr <- case Dhall.Parser.exprFromText (Directed "(stdin)" 0 0 0 0) lazyText of
        Left  err  -> Control.Exception.throwIO err
        Right expr -> return expr
    expr' <- Dhall.Import.load expr
    
    let viewportScroll = Brick.viewportScroll 99
    let ui = dhallEdit (Dhall.Core.normalize expr')
    let (Sum numWidgets, state) = getUI ui
    case Control.Monad.Trans.State.evalState state 0 of
        Fold step begin done -> do
            let appDraw (s, r) =
                    let (W widget, _) = done r s
                    in  [Brick.viewport 99 Vertical widget]
            let appChooseCursor = Brick.Focus.focusRingCursor snd
            let appHandleEvent (s, r) brickEvent = case brickEvent of
                    VtyEvent event -> case event of
                        EvKey KEsc [] -> Brick.halt (s, r)
                        EvKey (KChar '\t') [] -> do
                            Brick.continue (s, Brick.Focus.focusNext r)
                        EvKey KBackTab [] -> do
                            Brick.continue (s, Brick.Focus.focusPrev r)
                        EvKey KDown [] -> do
                            Brick.vScrollBy viewportScroll 1
                            Brick.continue (s, r)
                        EvKey KUp [] -> do
                            Brick.vScrollBy viewportScroll (-1)
                            Brick.continue (s, r)
                        _ -> case Brick.Focus.focusGetCurrent r of
                            Just n -> do
                                s' <- step n s event
                                Brick.continue (s', r)
                            Nothing -> do
                                Brick.continue (s, r)
                    _ -> do
                        Brick.continue (s, r)
            let appStartEvent = return
            let appAttrMap _ = attrMap Graphics.Vty.Attributes.defAttr []

            (s', r') <- defaultMain (Brick.App {..}) (begin, Brick.Focus.focusRing [0..(numWidgets - 1)])
            let (_, mExpr'') = done r' s'
            case mExpr'' of
                Nothing     -> do
                    fail "Invalid fields"
                Just expr'' -> do
                    Data.Text.Lazy.IO.writeFile file (Dhall.Core.pretty expr'')
