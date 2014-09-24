module GrandPA.UI.Sprite
    ( PixelGen
    , Sprite(..)
    , SpriteContext
    , cellSize
    , cells
    , withSprite
    , blitSprite
    ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Bits (shiftR, (.&.))
import Data.List (transpose)
import Data.Word (Word8, Word32)
import Foreign.Marshal.Array (withArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))

import qualified Graphics.UI.SDL as SDL

type PixelGen = Int        -- ^ The cell of the sprite sheet
             -> (Int, Int) -- ^ X and Y coordinates within the cell
             -> Char       -- ^ Character from the ASCII input
             -> Word32     -- ^ The new color in RGBA format

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

data Sprite = Sprite
    { spritePixelGen :: PixelGen
    , spriteData     :: [[String]]
    }

data TexData = TexData
    { tdCells    :: Int
    , tdCellSize :: (Int, Int)
    , tdData     :: [Color]
    } deriving Show

data SpriteContext = SpriteContext
    { contextTexData :: TexData
    , contextTarget  :: SDL.Renderer
    , contextTexture :: SDL.Texture
    }

cellSize :: SpriteContext -> (Int, Int)
cellSize = tdCellSize . contextTexData

cells :: SpriteContext -> Int
cells = tdCells . contextTexData

genTexData :: Sprite -> TexData
genTexData sprite =
    TexData cellCount (cellWidth, cellHeight) pixelized
  where
    cellCount = length $ spriteData sprite
    cellHeight = length . head $ spriteData sprite
    cellWidth  = length . head . head $ spriteData sprite
    pixelized = concatMap concat . transpose . mapCells $ spriteData sprite
    mapCells = zipWith (curry mapCell) [0 ..]
    mapCell (num, cols) = map (mapColumn num) $ zip [0..] cols
    mapColumn cell (col, rows) = map (mapRow cell col) $ zip [0..] rows
    mapRow cell col (row, char) =
        RGBA8888 $ spritePixelGen sprite cell (row, col) char

createSprite :: SDL.Renderer -> TexData -> IO SpriteContext
createSprite renderer texdata = withArray (tdData texdata) $ \td -> do
    format <- SDL.masksToPixelFormatEnum 32 0 0 0 0
    texture <- SDL.createTexture renderer format access width height
    void $ SDL.updateTexture texture nullPtr (castPtr td) $ width * 4
    return $ SpriteContext texdata renderer texture
  where
    (cWidth, cHeight) = tdCellSize texdata
    width = fromIntegral $ cWidth * tdCells texdata
    height = fromIntegral cHeight
    access = SDL.textureAccessStatic

destroySprite :: SpriteContext -> IO ()
destroySprite = SDL.destroyTexture . contextTexture

blitSprite :: SpriteContext -> (Int, Int) -> Int -> IO ()
blitSprite sc (x, y) idx =
    void . blitIdx $ fromIntegral idx
  where
    texdata = contextTexData sc
    texture = contextTexture sc
    target = contextTarget sc
    (width, height) = tdCellSize texdata
    intwidth = fromIntegral width
    intheight = fromIntegral height
    mkSrcRect n = SDL.Rect (n * intwidth) 0 intwidth intheight
    dstRect = SDL.Rect (fromIntegral x) (fromIntegral y) intwidth intheight
    doRender s d = (== 0) <$> SDL.renderCopy target texture s d
    blitIdx n = with (mkSrcRect n) (with dstRect . doRender)

withSprite :: SDL.Renderer -> Sprite -> (SpriteContext -> IO a) -> IO a
withSprite r sprite = bracket (createSprite r texdata) destroySprite
                where texdata = genTexData sprite
