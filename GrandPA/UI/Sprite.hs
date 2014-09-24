module GrandPA.UI.Sprite
    ( AsciiSprite
    , PixelGen
    , Shape(..)
    , Sprite(..)
    , SpriteContext
    , blitSprite
    , cellSize
    , cells
    , withSprite
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8, Word32)
import Foreign.Marshal.Array (withArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))

import qualified Data.Matrix as M
import qualified Graphics.UI.SDL as SDL

type AsciiSprite = Sprite String

data Sprite a = Sprite
    { spritePixelGen :: PixelGen
    , spriteData     :: [[a]]
    }

instance Show a => Show (Sprite a) where
    show = show . spriteData

type PixelGen = Int        -- ^ The cell of the sprite sheet
             -> (Int, Int) -- ^ X and Y coordinates within the cell
             -> Char       -- ^ Character from the ASCII input
             -> Shape      -- ^ The new shape for this character

data Shape = Pixel     Word32
           | SubSprite AsciiSprite
           deriving Show

data Color = RGBA8888 Word32
             deriving Show

instance Storable Color where
    sizeOf    = const 4
    alignment = const 1
    peek      = const $ fail "No support for peeking colors!"
    poke ptr (RGBA8888 c) =
        pokeArray (castPtr ptr) $ map (getByte c) [1..4]
      where getByte :: Word32 -> Int -> Word8
            getByte v b = fromIntegral $ v `shiftR` (32 - b * 8) .&. 0xff

type TexData = [M.Matrix Color]

data SpriteContext = SpriteContext
    { contextTexData :: TexData
    , contextTarget  :: SDL.Renderer
    , contextTexture :: SDL.Texture
    }

cellSize :: SpriteContext -> (Int, Int)
cellSize = (M.ncols &&& M.nrows) . head . contextTexData

cells :: SpriteContext -> Int
cells = length . contextTexData

transformShape :: Shape -> Color
transformShape (Pixel     p) = RGBA8888 p
transformShape (SubSprite _) = RGBA8888 0xffffffff -- XXX: dummy!

mapMatrix :: ((Int, Int) -> a -> b) -> M.Matrix a -> M.Matrix b
mapMatrix f m = M.matrix (M.nrows m) (M.ncols m) $
    \(y, x) -> f (x - 1, y - 1) (M.unsafeGet y x m)

expandSprite :: AsciiSprite -> TexData
expandSprite sprite =
    map (\(cell, m) -> mapMatrix (f cell) m) $ zip [0..] converted
  where
    converted = map M.fromLists $ spriteData sprite
    pixgen = spritePixelGen sprite
    f cell pos char = transformShape $ pixgen cell pos char

createSprite :: SDL.Renderer -> TexData -> IO SpriteContext
createSprite renderer texdata = withArray flatTexdata $ \td -> do
    format <- SDL.masksToPixelFormatEnum 32 0 0 0 0
    texture <- SDL.createTexture renderer format access width height
    void $ SDL.updateTexture texture nullPtr (castPtr td) $ width * 4
    return $ SpriteContext texdata renderer texture
  where
    flatTexdata = M.toList $ foldl1 (M.<|>) texdata
    (cWidth, cHeight) = (M.ncols &&& M.nrows) $ head texdata
    width = fromIntegral $ cWidth * length texdata
    height = fromIntegral cHeight
    access = SDL.textureAccessStatic

destroySprite :: SpriteContext -> IO ()
destroySprite = SDL.destroyTexture . contextTexture

blitSprite :: SpriteContext -> (Int, Int) -> Int -> IO ()
blitSprite sc (x, y) idx =
    void . blitIdx $ fromIntegral idx
  where
    texture = contextTexture sc
    target = contextTarget sc
    (width, height) = cellSize sc
    intwidth = fromIntegral width
    intheight = fromIntegral height
    mkSrcRect n = SDL.Rect (n * intwidth) 0 intwidth intheight
    dstRect = SDL.Rect (fromIntegral x) (fromIntegral y) intwidth intheight
    doRender s d = (== 0) <$> SDL.renderCopy target texture s d
    blitIdx n = with (mkSrcRect n) (with dstRect . doRender)

withSprite :: SDL.Renderer -> AsciiSprite -> (SpriteContext -> IO a) -> IO a
withSprite r sprite = bracket (createSprite r texdata) destroySprite
                where texdata = expandSprite sprite
