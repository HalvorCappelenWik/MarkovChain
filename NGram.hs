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
import Distribution.Compat.Lens (_1)

-- Rename types to clarify some type signatures later
type NGram = String
type Weight = Integer


-- Produce all n-grams contained in a given string
grams :: Integer -> String -> [NGram]
grams _ "" = []
grams number str = if fromIntegral number > length str then [] else (take (fromIntegral number) str : grams number (drop 1 str))

-- Produce all n-grams contained in a given string, paired
-- with the subsequent character
gramsWithNext2 :: Integer -> String -> [(NGram,Char)]
gramsWithNext2 _ "" = []
gramsWithNext2 number str = zip (grams number str) (drop (fromIntegral number) str)

-- ("has" + "ask" + "ske" + "kel" + "ell")
-- has ke ll 
-- combineGrams (grams 3 "Haskell") == "Haskell"
-- combineGrams ["Hei"] = "Hei"
-- combineGrams [] = [] 
-- Fremgangsmåte: Bruk patternmatching og rekursjon.
-- Skriv funksjonen som rekombinerer en liste med etterfølgende n-gram tilbake til en streng:
-- Recombine a list of n-grams to a string
combineGrams :: [NGram] -> String
combineGrams [] = []
combineGrams [a] = a
combineGrams (x : xs) = take 1 x ++ combineGrams partword ++ lstpart where 
    
-- Update an n-gram by adding a character to the end
-- and removing the first character.
updateGram :: NGram -> Char -> NGram
updateGram g c = tail g ++ [c]


