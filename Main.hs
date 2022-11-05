import System.IO
import System.Environment
import Text.Read (readMaybe)

import Data.List
import NGram
import Model

import qualified Data.Map as Map
import System.Random

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Codec.Compression.GZip as GZip


-- Print the usage instructions for the program.
printUsage
 = putStrLn
 $  "Usage: ramble <COMMAND> <MODELFILE> [SAMPLEFILE]\n"
 ++ "where COMMAND is one of:\n"
 ++ "   create:  Create a new model from samplefile or stdin\n"
 ++ "   on <STARTPRASE> <LENGTH>:\n"
 ++ "                Generate text from model starting\n"
 ++ "                from STARTPHRASE until total length\n"
 ++ "                reaches LENGTH.\n\n"
 ++ "Examples:\n"
 ++ "    ramble create alice.mod alice.txt\n"
 ++ "    ramble on \"Alice went\" alice.mod"

-- A global n for our n-grams.
-- If you play around with it, remember that it
-- has to be the same when creating and using a model.
-- All example models are generated with gramLen = 7
gramLen :: (Num a) => a
gramLen = 7

-- Pick out an element from a weighted list by
-- going through the list until a certain treshold has been
-- reached.
pick :: [(a,Weight)] -> Weight -> a
pick [] _ = error "Must contain weights"
pick [(x,w)] _ = x
pick ((x,w):xs) treshold = if w > treshold then x else pick xs (treshold - w)

-- Pick a random element from a weighted list with a given
-- total weight.
pickRandom :: [(a,Weight)] -> Weight -> IO a
pickRandom wl total = do
      r <- randomRIO (0, total)
      return $ pick wl r

-- Generate a fixed amount of text from a model starting from a given
-- start string
generate :: TextModel -> String -> Integer -> IO String
generate model start amount = do
      let startGram = toNGram gramLen start
      let startDist = nextDistribution model startGram
      case startDist of
            Nothing -> return ""
            Just (dist, total) -> do
                  next <- pickRandom dist total
                  rest <- generate model (start ++ [next]) (amount - 1)
                  return (start ++ [next] ++ rest)

-- Helper function which generates n-grams from a model
generate' :: TextModel -> NGram -> Integer -> IO [NGram]
generate' model start amount = do
      case (findNgram start model) of
            Nothing -> return []
            Just (weights, total) -> do
                  next <- pickRandom (Map.toList weights) total
                  rest <- generate' model (tailNGram start ++ [next]) (amount - 1)
                  return (start : rest)


-- Serialize a text model and write a handle.
writeModel :: TextModel -> Handle -> IO ()
writeModel model h
   = ByteString.hPut h $ GZip.compress
                     $ UTF8.fromString
                     $ show model

-- Read a text model from a handle.
readModel :: Handle -> IO TextModel
readModel h = do
      contents <- ByteString.hGetContents h
      case (readMaybe $ UTF8.toString $ GZip.decompress contents) of
            Nothing -> error "Could not read model"
            Just model -> return model


main = do
   args <- getArgs
   case args of
     ["create",modelFile] -> do
        modelh <- openFile modelFile WriteMode
        sample <- hGetContents stdin
        let model = createModel gramLen sample
        writeModel model modelh
        hClose modelh
     ["create",modelFile,sampleFile] -> do
        modelh <- openFile modelFile WriteMode
        sampleh <- openFile sampleFile ReadMode
        sample <- hGetContents sampleh
        let model = createModel gramLen sample
        putStrLn $ "Created model with: " ++ show (Map.size model) ++ " n-grams"
        writeModel model modelh
        hClose modelh
        hClose sampleh
     ["on",startPhrase,sLength,modelFile] -> do
        modelh <- openFile modelFile ReadMode
        model <- readModel modelh
        case readMaybe sLength of
           (Just outlength)
                  ->  generate model startPhrase outlength >>= putStrLn
           Nothing -> printUsage
        hClose modelh
     _ -> printUsage

