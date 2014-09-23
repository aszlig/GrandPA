module Main where

import Control.Monad (void)
import Control.Wire
import Data.Word (Word8)
import Prelude hiding ((.), id, until)

import qualified Graphics.UI.SDL as SDL

import GrandPA.Enttec (Widget, withWidget, sendDMX)
import GrandPA.UI (GrandUI(..), withUI)
import GrandPA.UI.Font (withFont, blitString)

bleepWire :: SimpleWire () (Event [Word8])
bleepWire = periodic 3  . pure (take 256 $ cycle [0xff, 0x00, 0x00, 0x00])
         <& periodic 7  . pure (take 256 $ cycle [0x00, 0xff, 0x00, 0x00])
         <& periodic 11 . pure (take 256 $ cycle [0x00, 0x00, 0xff, 0x00])
         <& periodic 23 . pure (take 256 $ cycle [0x00, 0x00, 0x00, 0xff])

mainWire :: SimpleWire () [Word8]
mainWire = hold . bleepWire <|> pure (replicate 256 0x00)

mainLoop :: SimpleWire () [Word8]
         -> Session IO (Timed NominalDiffTime ())
         -> Widget
         -> IO ()
mainLoop wire session widget = do
    (stateDelta, newSession) <- stepSession session
    let Identity (result, newWire) = stepWire wire stateDelta $ Right ()
    case result of
         Left  _   -> return ()
         Right dmx -> do
             sendDMX widget 0 dmx
             mainLoop newWire newSession widget

main :: IO ()
main = withUI $ \ui -> do
    withFont (uiRenderer ui) $ \tex -> do
        void $ SDL.renderClear $ uiRenderer ui
        void $ blitString (uiRenderer ui) tex (30, 10) "Hello World!"
        void $ SDL.renderPresent $ uiRenderer ui
        withWidget $ mainLoop mainWire clockSession_
