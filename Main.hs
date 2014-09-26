module Main where

import Control.Monad (void)
import Control.Wire
import Data.Word (Word8)
import Prelude hiding ((.), id, until)

import qualified Graphics.UI.SDL as SDL

import GrandPA.Enttec (Widget, withWidget, sendDMX)
import GrandPA.UI (GrandUI(..), withUI)
import GrandPA.UI.Font (withFont, blitString)
import GrandPA.UI.Sprite (withSprite, blitSprite)

import qualified GrandPA.Sprites as Sprites

bleepWire :: SimpleWire () (Event [Word8])
bleepWire = periodic 0.2 . pure (take 256 $ cycle [0xff, 0x00, 0x00, 0x00])
         >| periodic 5.0 . pure (take 256 $ cycle [0x00, 0xff, 0x00, 0x00])
         >| periodic 1.2 . pure (take 256 $ cycle [0x00, 0x00, 0xff, 0x00])
         >| periodic 3.0 . pure (take 256 $ cycle [0x00, 0x00, 0x00, 0xff])
  where (>|) = liftA2 (merge $ zipWith max)
        infixl 5 >|

mainWire :: SimpleWire () [Word8]
mainWire = holdFor 0.1 . bleepWire <|> pure (replicate 256 0x00)

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
main = withUI $ \ui ->
    withSprite (uiRenderer ui) Sprites.revo $ \revo ->
    withFont (uiRenderer ui) $ \font -> do
        void $ SDL.renderClear $ uiRenderer ui
        blitString font (30, 10) "Hello World!"
        blitSprite revo (40, 40) 0
        void $ SDL.renderPresent $ uiRenderer ui
        withWidget $ mainLoop mainWire clockSession_
