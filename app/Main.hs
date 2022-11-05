module Main (main) where

import Lib
import Options.Applicative

newtype Args = Args
    { filepath :: FilePath }

argParser :: Parser Args
argParser = Args 
    <$> strOption 
        ( long "input"
        <> metavar "INPUT"
        <> help "input file (.wav)" )

proccessArgs :: Args -> IO ()
proccessArgs (Args a) = slowDownFile a "out.wav"

main :: IO ()
main = proccessArgs =<< execParser opts
    where 
        opts = info (argParser <**> helper)
            ( fullDesc
            <> progDesc "description"
            <> header "program header" )
