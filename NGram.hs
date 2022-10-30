module NGram (NGram,Weight
             ,grams
             ,gramsWithNext
             ,combineGrams
             ,updateGram) where
import Control.Monad
import Data.List
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

-- Rename types to clarify some type signatures later
type NGram = String
type Weight = Integer

-- Produce all n-grams contained in a given string
grams :: Integer -> String -> [NGram]
grams _ "" = []
grams number str = if fromIntegral number > length str then [] else (take (fromIntegral number) str : grams number (drop 1 str))


-- Produce all n-grams contained in a given string, paired
-- with the subsequent character
gramsWithNext :: Integer -> String -> [(NGram,Char)]
gramsWithNext _ "" = []
gramsWithNext number str = zip (grams number str) (drop (fromIntegral number) str)

-- Recombine a list of n-grams to a string
combineGrams :: [NGram] -> String
combineGrams [] = []
combineGrams [a] = a
combineGrams (x : xs) = take 1 x ++ combineGrams xs


-- Update an n-gram by adding a character to the end
-- and removing the first character.
updateGram :: NGram -> Char -> NGram
updateGram g c = tail g ++ [c]


