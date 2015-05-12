{- | clone-size-filter
Gregory W. Schwartz

Takes a fasta file and returns the sequences that belong to clones of sizes
n or larger
-}

{-# LANGUAGE OverloadedStrings #-}

-- Built-in
import Data.List
import Data.Function (on)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified System.IO as IO

-- Cabal
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PTIO

-- Local
import Data.Fasta.Text.Lazy

-- Command line arguments
data Options = Options { input  :: String
                       , output :: String
                       , size   :: Int
                       , index  :: Int }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The input fasta file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The output fasta file" )
      <*> option auto
          ( long "size"
         <> short 's'
         <> metavar "INT"
         <> value 1
         <> help "The clone size to filter by. The sequences belonging to\
                 \ clones of this size or greater are kept, everything\
                 \ else is removed" )
      <*> option auto
          ( long "index"
         <> short 'c'
         <> metavar "INT"
         <> value 1
         <> help "The index of the clone field in the fasta header\
                 \ (1 indexed)" )

-- | Filter those entities greater than or equal to n occurrences
filterCommonEntities :: Int -> Int -> [FastaSequence] -> [FastaSequence]
filterCommonEntities ind n = concat
                           . filter (\xs -> length xs >= n)
                           . groupBy ((==) `on` getClone ind)
                           . sortBy (compare `on` getClone ind)
  where
    getClone x = (!! x) . T.splitOn "|" . fastaHeader

cloneSizeFilter :: Options -> IO ()
cloneSizeFilter opts = do
    hIn  <- if null . input $ opts
                then return IO.stdin
                else IO.openFile (input opts) IO.ReadMode
    fastaList <- runEffect $ P.toListM $ pipesFasta . PTIO.fromHandle $ hIn

    -- Get output string
    let filteredFastaList = filterCommonEntities
                            (index opts)
                            (size opts)
                            fastaList
        outputString      = T.unlines . map showFasta $ filteredFastaList


    -- What to do with output
    if null . output $ opts
        then TIO.putStrLn outputString
        else TIO.writeFile (output opts) outputString

main :: IO ()
main = execParser opts >>= cloneSizeFilter
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Filter fasta files by only including large clones"
     <> header "clone-size-filter, Gregory W. Schwartz" )
