module GrandPA.AsciiQuote (apic) where

import Control.Arrow ((&&&), (***), (>>>))
import Control.Monad (join)
import Data.Function (on)
import Data.List (nub, sortBy, group, transpose)
import Language.Haskell.TH (stringE, listE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

type PosLen = (Int, Int)
type HLine = [PosLen]
type VLine = [PosLen]

borderChars :: String
borderChars = "+-|,.'`<>"

accumToken :: (Int, [PosLen]) -> (Int, Char) -> (Int, [PosLen])
accumToken (pos, acc) (len, char)
    | char `elem` borderChars = (pos + len, (pos, len) : acc)
    | otherwise               = (pos + len, acc)

getBorderTokens :: String -> [PosLen]
getBorderTokens = mkPos . map (length &&& head) . group
            where mkPos = reverse . snd . foldl accumToken (0, [])

getBorders :: [String] -> ([HLine], [VLine])
getBorders = join (***) (map getBorderTokens) . (id &&& transpose) . pad
       where pad s = map (take (maximum (map length s)) . (++ repeat ' ')) s

getEdges' :: [PosLen] -> [PosLen] -> [PosLen]
getEdges' acc ((cl, 1) : (ep, el) : (cr, 1) : rest)
    | [cl..cr] == [cl] ++ [ep..ep + el - 1] ++ [cr] =
        getEdges' ((ep, el) : acc) ((ep, el) : (cr, 1) : rest)
getEdges' acc (_ : rest) = getEdges' acc rest
getEdges' acc []         = acc

getEdges :: [PosLen] -> [PosLen]
getEdges = reverse . getEdges' []

getParallels' :: Eq a => [[a]] -> [[a]] -> [[a]]
getParallels' acc []               = acc
getParallels' acc (current : rest) =
    getParallels' (filter (\c -> any (c `elem`) rest) current : acc) rest

getParallels :: Eq a => [[a]] -> [[a]]
getParallels = reverse . getParallels' []

mkBoxes :: [HLine] -> [VLine] -> [(Int, Int, Int, Int)]
mkBoxes h v = concatMap (\(n, hl) -> concatMap (zipH n) hl) $ zip [startY..] h
        where zipH _ (p, l) = map (\(p', l') -> (p, l, p', l')) $ v !! (p - 1)
              startY = 0 :: Int

getBoxes :: [String] -> [(Int, Int, Int, Int)]
getBoxes = nubsort . uncurry mkBoxes . (mkEdge *** mkEdge) . getBorders
     where mkEdge = map getEdges >>> getParallels
           nubsort = nub . sortBy (compare `on` boxCmp)
           boxCmp (xp, _, yp, _) = (yp, xp)

extractBox :: (Int, Int, Int, Int) -> [String] -> [String]
extractBox (xp, xl, yp, yl) = map (take xl . drop xp) . take yl . drop yp

extractAll :: String -> [[String]]
extractAll str = map (`extractBox` strlines) $ getBoxes strlines
           where strlines = lines str

apic :: QuasiQuoter
apic = QuasiQuoter
    { quoteExp  = mkExpr . extractAll
    , quotePat  = mkError
    , quoteType = mkError
    , quoteDec  = mkError
    }
  where
    mkExpr  = listE . map (listE . map stringE)
    mkError = const $ error "apic has to be an expression"
