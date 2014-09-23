module GrandPA.UI (GrandUI(..), withUI) where

import Control.Exception (bracketOnError)
import Foreign.C.String (withCAString)

import qualified Graphics.UI.SDL as SDL

data GrandUI = GrandUI
    { uiWindow   :: SDL.Window
    , uiRenderer :: SDL.Renderer
    } deriving Show

initUI :: IO GrandUI
initUI = do
    window <- withCAString "GrandPA" $ \title ->
        SDL.createWindow title posX posY 1280 1024 flags
    renderer <- SDL.createRenderer window (-1) 0
    return $ GrandUI window renderer
  where
    posX = SDL.windowPosCentered
    posY = SDL.windowPosCentered
    flags = SDL.windowFlagResizable

exitUI :: GrandUI -> IO ()
exitUI _ = SDL.quit

withUI :: (GrandUI -> IO a) -> IO a
withUI = bracketOnError initUI exitUI
