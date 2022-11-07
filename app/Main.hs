module Main (main) where

import Lib
import Options.Applicative

data Args = Args
    { inputFile :: FilePath,
      outputFile :: FilePath }

argParser :: Parser Args
argParser = Args 
    <$> strOption 
        ( long "input"
        <> short 'i'
        <> metavar "INPUT"
        <> help "input file (.wav)" )
    <*> strOption 
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "output file location" )

proccessArgs :: Args -> IO ()
proccessArgs args = slowDownFile (inputFile args) (outputFile args)

main :: IO ()
main = proccessArgs =<< execParser opts
    where 
        opts = info (argParser <**> helper)
            ( fullDesc
            <> progDesc "Slow down a wave file"
            <> header "program header" )
