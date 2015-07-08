{- | Filter.hs
Gregory W. Schwartz

Pertains to functions that help filter the fasta file for clone size
-}

{-# LANGUAGE OverloadedStrings #-}

module Filter where

-- Built-in
import Data.List
import Data.Function (on)
import qualified Data.Text.Lazy as T

-- Local
import Data.Fasta.Text.Lazy

-- | Filter those entities greater than or equal to n occurrences
filterCommonEntities :: Int -> Int -> [FastaSequence] -> [FastaSequence]
filterCommonEntities ind n = concat
                           . filter ((>= n) . length)
                           . groupBy ((==) `on` getClone ind)
                           . sortBy (compare `on` getClone ind)
  where
    getClone x = (!! (x - 1)) . T.splitOn "|" . fastaHeader

-- | Filter those entities greater than or equal to n occurrences, where
-- n is now inside the header
filterCounts :: Int -> Int -> [FastaSequence] -> [FastaSequence]
filterCounts ind n = filter ((>= n) . getCount ind)
  where
    getCount x = read . T.unpack . (!! (x - 1)) . T.splitOn "|" . fastaHeader
