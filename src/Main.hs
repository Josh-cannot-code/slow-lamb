import qualified Data.WAVE as W
import Data.WAVE (WAVESamples)

file :: FilePath
file = "src/piano2.wav"

out :: FilePath
out = "src/piano2slow.wav"

iw :: IO W.WAVE
iw = W.getWAVEFile file

iwSamples :: IO W.WAVESamples
iwSamples = fmap W.waveSamples iw

printSamples :: W.WAVESamples -> IO ()
printSamples = mapM_ print

printIwSamples :: IO ()
printIwSamples = printSamples =<< iwSamples

slowSamples :: WAVESamples -> WAVESamples
slowSamples samples = slowHelper samples []
    where 
        slowHelper :: WAVESamples -> WAVESamples -> WAVESamples
        slowHelper s acc = 
            case s of 
            [] -> reverse acc 
            x:xs -> slowHelper xs (x:x:acc)

processedIw :: IO W.WAVESamples
processedIw = fmap slowSamples iwSamples

printProcessedIw :: IO ()
printProcessedIw = printSamples =<< processedIw
            
writeProcessedIw :: IO ()
writeProcessedIw = do 
    s <- processedIw
    h <- fmap W.waveHeader iw
    let outWave = W.WAVE {
            W.waveHeader = h,
            W.waveSamples = s
        }
    W.putWAVEFile out outWave
