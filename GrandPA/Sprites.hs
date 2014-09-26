{-# LANGUAGE QuasiQuotes #-}
module GrandPA.Sprites (revo, ledBar) where

import Data.Bits ((.&.))
import Data.Word (Word32)

import GrandPA.AsciiQuote
import GrandPA.UI.Sprite

mkLedPixels :: Word32 -> PixelGen
mkLedPixels c _ _ '=' = Pixel $ c .&. 0x202020ff
mkLedPixels c _ _ '%' = Pixel $ c .&. 0x404040ff
mkLedPixels c _ _ '*' = Pixel $ c .&. 0x808080ff
mkLedPixels c _ _ '@' = Pixel $ c .&. 0xbbbbbbff
mkLedPixels c _ _ '#' = Pixel $ c .&. 0xffffffff
mkLedPixels c _ _ _   = Pixel $ c .&. 0x00000000

ledImage :: Word32 -> AsciiSprite
ledImage c = Sprite (mkLedPixels c) [apic|
    +-----------+
    |    ===    |
    |  ==%%%==  |
    | ==%***%== |
    | =%*@@@*%= |
    |=%*@@#@@*%=|
    |=%*@###@*%=|
    |=%*@@#@@*%=|
    | =%*@@@*%= |
    | ==%***%== |
    |  ==%%%==  |
    |    ===    |
    +-----------+
|]

mkPixels :: PixelGen
mkPixels _ _ 'c' = Pixel 0x696969ff
mkPixels _ _ 'R' = SubSprite (ledImage 0xff0000ff)
mkPixels _ _ 'G' = SubSprite (ledImage 0x00ff00ff)
mkPixels _ _ 'B' = SubSprite (ledImage 0x0000ffff)
mkPixels _ _ 'W' = SubSprite (ledImage 0xffffffff)
mkPixels _ _ _   = Pixel 0x000000ff

scale :: [[String]] -> [[String]]
scale = map (concatMap (replicate 5 . concatMap (replicate 5)))

revo :: AsciiSprite
revo = Sprite mkPixels [apic|
    +--------------------+
    |cccccccccccccccccccc|
    |c                  c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c GRGRGRGRGRGRGRGR c|
    |c WBWBWBWBWBWBWBWB c|
    |c                  c|
    |cccccccccccccccccccc|
    +--------------------+
|]

ledBar :: AsciiSprite
ledBar = Sprite mkPixels $ scale [apic|
    +-----------------------------------------------+
    |ccccccccccccccccccccccccccccccccccccccccccccccc|
    |cRRRRRRRRRRRRRRRGGGGGGGGGGGGGGGBBBBBBBBBBBBBBBc|
    |cRRRRRRRRRRRRRRRGGGGGGGGGGGGGGGBBBBBBBBBBBBBBBc|
    |ccccccccccccccccccccccccccccccccccccccccccccccc|
    +-----------------------------------------------+
|]
