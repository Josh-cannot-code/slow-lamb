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
        avg2 = zipWith (\ x y -> (x `div` 2) + (y `div` 2)) -- Seperate division to avoid int32 overflow


delay :: Int -> WAVESamples -> WAVESamples 
delay duration samples = helper samples [] (take duration [[0,0] | _ <- [0..]])
    where
        helper :: WAVESamples -> WAVESamples -> WAVESamples -> WAVESamples
        helper s buf acc =  
            case s of 
            [] -> acc
            x -> helper (drop duration x) (take duration x) (acc ++ zipWith mergeS buf (reverse . take duration . reverse $ acc))

        mergeS :: [WAVESample] -> [WAVESample] -> [WAVESample] 
        mergeS [a, b] [c, d] = [a + (c `div` 2), b + (d `div` 2)]
        mergeS _ _ = []


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
