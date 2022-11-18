module Lib
    ( halfSpeed,
      applyEffectToFile,
      delay,
      identity
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

identity :: WAVESamples -> WAVESamples
identity w = w


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
        avg2 [a, b] [c, d] = [a `div` 2 + c `div` 2, b `div` 2 + d `div` 2] -- add seperate to avoid overflow
        avg2 _ _ = []


delay :: Int -> WAVESamples -> WAVESamples 
delay duration samples = helper samples (take duration samples) zerosInit zerosInit
    where
        helper :: WAVESamples -> WAVESamples -> WAVESamples -> WAVESamples -> WAVESamples
        helper s buf accBuf acc =  
            case s of 
            [] -> acc
            x -> 
                let accBuf' = zipWith mergeS buf accBuf in
                helper (drop duration x) (take duration x) accBuf' (acc ++ accBuf')

        mergeS :: [WAVESample] -> [WAVESample] -> [WAVESample] 
        mergeS [a, b] [c, d] = [(a `div` 3)*2 + (c `div` 3), (b `div` 3)*2 + (d `div` 3)]
        mergeS _ _ = []

        zerosInit :: WAVESamples
        zerosInit = take duration [[0,0] | _ <- [0..]]


writeSamples :: IO WAVEHeader -> IO WAVESamples -> FilePath -> IO ()
writeSamples header samples path = do 
    h <- header
    s <- samples
    let outWave = WAVE { 
            waveHeader = h,
            waveSamples = s 
        }
    putWAVEFile path outWave


applyEffectToFile :: FilePath -> FilePath -> (WAVESamples -> WAVESamples) -> IO ()
applyEffectToFile input output effect = writeSamples header samples output
    where
        wave = getWAVEFile input  
        header = fmap waveHeader wave
        samples = effect <$> fmap waveSamples wave
