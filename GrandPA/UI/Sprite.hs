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
import qualified Data.Vector as V
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

mapMatrix :: ((Int, Int) -> a -> b) -> M.Matrix a -> M.Matrix b
mapMatrix f m = M.matrix (M.nrows m) (M.ncols m) $
    \(y, x) -> f (x - 1, y - 1) (M.unsafeGet y x m)

flattenMatrix :: M.Matrix (M.Matrix a) -> M.Matrix a
flattenMatrix = foldl1 (M.<->) . map (foldl1 (M.<|>)) . M.toLists

transformShapes :: M.Matrix Shape -> M.Matrix Color
transformShapes m = flattenMatrix $ mapMatrix toColors m
  where
    -------------------  SIZE TABLE  -------------------
    ----------------------------------------------------
    --       c1    c2    c3    c4    c5    c6    c7   --
    --  r1 (1,1) (1,1) (1,1) (1,1) (1,1) (1,1) (1,1)  --
    --  r2 (1,1) (4,4) (1,1) (4,4) (1,1) (4,4) (1,1)  --
    --  r3 (1,1) (1,1) (1,1) (1,1) (1,1) (1,1) (1,1)  --
    --  r4 (1,1) (1,1) (1,1) (1,1) (1,1) (4,4) (1,1)  --
    --  r5 (1,1) (1,1) (1,1) (1,1) (1,1) (1,1) (1,1)  --
    sizes = mapMatrix (const getSize) m

    getSize (Pixel     _) = (1, 1)
    getSize (SubSprite s) = (M.ncols &&& M.nrows) . head $ expandSprite s

    vectorMax f = V.foldr (max . f) 0
    maxW = map (\c -> vectorMax fst $ M.getCol c sizes) [1..M.ncols sizes]
    maxH = map (\r -> vectorMax snd $ M.getRow r sizes) [1..M.nrows sizes]

    scaleBy (w, h) = M.extendTo (RGBA8888 0xffffffff) h w
    toColors (x, y) = scaleBy (maxW !! x, maxH !! y) . reduceShape

    reduceShape (Pixel     p) = M.fromList 1 1 [RGBA8888 p]
    reduceShape (SubSprite s) = head $ expandSprite s

expandSprite :: AsciiSprite -> TexData
expandSprite sprite = map transformShapes shapes
                where shapes = zipWith (curry genShape) [0..] converted
                      genShape (cell, m) = mapMatrix (pixgen cell) m
                      converted = map M.fromLists $ spriteData sprite
                      pixgen = spritePixelGen sprite

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
