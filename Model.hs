--module Model (TextModel
             --,createModel
             --,nextDistribution) where
import Control.Monad
import Data.List
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

import NGram
import Distribution.Compat.Lens (_1)

-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight , Weight)

-- The empty model with no n-grams.
emptyModel :: TextModel
emptyModel = Map.empty

--Du kan prøve å splitte det i to caser, enten er ngram i model allerede eller så er den ikke det. 
--Da trenger du bare to litt enklere kall til adjust og insert fremfor et veldig generisk kall til insertWith som dekker alt.
--Du vil fortsatt trenge insertWith i tillegg inni kallet til adjust, men det blir litt lettere 

-- Update a model with a new n-gram followed by a character.
increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next mod = case inmod of 
    True -> Map.adjust (\(m, w) -> (Map.insertWith (+) next 1 m, w + 1)) ngram mod
    False -> Map.insert ngram (Map.insert next 1 Map.empty, 1) mod
    where inmod = Map.member ngram mod

-- The distribution of next n-grams after a given one.
nextDistribution :: TextModel -> NGram -> Maybe ([(NGram, Weight)],Weight)
nextDistribution model current = case (Map.lookup current model) of
    Nothing -> Nothing
    Just x -> Just (Map.toList (fst x), snd x)



getNgram :: Maybe ([(NGram, Weight)],Weight) -> NGram
getNgram (Just (m, w)) = fst (m !! 0)

getWeight :: Maybe ([(NGram, Weight)],Weight) -> Weight
getWeight (Just (m, w)) = w



-- Probability distribution of next characters after a given n-gram.
--nextDistribution :: TextModel -> NGram -> Maybe ([(Char, Weight)],Weight)
--nextDistribution model current = case (findNgram current model) of
--    Nothing -> Nothing
--    Just (weights, total) -> Just (Map.toList weights, total)


-- Create an n-gram model from a string.
createModel :: Integer -> String -> TextModel
createModel n str = foldl (\mod (ngram, next) -> increaseWeight ngram next mod) Map.empty (gramsWithNext n str)
