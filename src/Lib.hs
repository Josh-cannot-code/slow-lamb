module Lib
    ( halfSpeed,
      slowDownFile
    ) where

import Data.WAVE (
        WAVESamples,
        WAVESample,
        WAVE (WAVE),
        getWAVEFile,
        putWAVEFile,
        waveHeader,
        waveSamples, WAVEHeader
    )



file :: FilePath
file = "src/piano2.wav"

out :: FilePath
out = "src/piano2slow.wav"

iw :: IO WAVE
iw = getWAVEFile file

iwSamples :: IO WAVESamples
iwSamples = fmap waveSamples iw

printSamples :: WAVESamples -> IO ()
printSamples = mapM_ print

printIwSamples :: IO ()
printIwSamples = printSamples =<< iwSamples

halfSpeed :: WAVESamples -> WAVESamples
halfSpeed samples = slowHelper samples []
    where 
        slowHelper :: WAVESamples -> WAVESamples -> WAVESamples
        slowHelper s acc = 
            case s of 
            [] -> reverse acc 
            [x] -> slowHelper [] (x:acc)
            x:y:xs -> slowHelper (y:xs) (avg2 x y:x:acc)

        avg2 :: [WAVESample] -> [WAVESample] -> [WAVESample]
        avg2 = zipWith (\ x y -> (x `div` 2) + (y `div` 2)) 

processedIw :: IO WAVESamples
processedIw = fmap halfSpeed iwSamples

printProcessedIw :: IO ()
printProcessedIw = printSamples . take 5 =<< processedIw
            
writeProcessedIw :: IO ()
writeProcessedIw = do 
    s <- processedIw
    h <- fmap waveHeader iw
    let outWave = WAVE {
            waveHeader = h,
            waveSamples = s
        }
    putWAVEFile out outWave

writeSamples :: IO WAVEHeader -> IO WAVESamples -> FilePath -> IO ()
writeSamples header samples path = do 
    h <- header
    s <- samples
    let outWave = WAVE { 
            waveHeader = h,
            waveSamples = s 
        }
    putWAVEFile path outWave


slowDownFile :: FilePath -> FilePath-> IO ()
slowDownFile input = writeSamples header samples
    where
        wave = getWAVEFile input  
        header = fmap waveHeader wave
        samples = halfSpeed <$> fmap waveSamples wave
    

    

