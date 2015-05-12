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
                           . filter (\xs -> length xs >= n)
                           . groupBy ((==) `on` getClone ind)
                           . sortBy (compare `on` getClone ind)
  where
    getClone x = (!! (x - 1)) . T.splitOn "|" . fastaHeader

