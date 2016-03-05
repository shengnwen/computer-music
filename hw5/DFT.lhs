> module DFT where
> import Data.Complex
> import Codec.Wav
> import Data.Audio
> import WavTools

This is a slight redoing of the DFT approach from HSoM.
The DFT algorithm remains the same, but the interface 
is simplified to work with the output of functions like 
readWav from WavTools.lhs. 

First, we define some type synonyms for readability.

> type NumSamples = Int -- how many samples we have
> type Bin = Int -- one entry or index in the DFT result
> type Magnitude = Double -- DFT output for a given bin
> type Frequency = Double -- frequency measured in Hz

The following function lets us determine the frequency 
represented by an arbitrary "bin" in the DFT results.

> binFrequency :: SampleRate -> NumSamples -> Bin -> Double
> binFrequency sr n b = fromIntegral (b * sr) / fromIntegral n

We now define a function to compute the DFT of a signal 
represented as a list of Samples (or Doubles). The result 
is a list of pairs: the frequency of the entry and its 
magnitude.

> dft :: SampleRate -> [Sample] -> [(Frequency, Magnitude)]
> dft sr samples = 
>     let n = length samples
>         cSamples = map (:+ 0) samples -- convert to the Complex type
>         dftResult = dftBackend cSamples -- run DFT
>         f (a :+ b) = sqrt (a * a + b * b) -- convert a Complex to a magnitude
>         mags = normalize $ map f dftResult -- find all magnitudes
>         binFreqs = map (binFrequency sr n) [0..n-1] -- find bin frequencies
>     in  zip binFreqs mags where 
>     -- dft backend, which is HSoM's "dft" function
>     dftBackend :: RealFloat a => [Complex a] -> [Complex a]
>     dftBackend xs = 
>         let  lenI = length xs
>              lenR = fromIntegral lenI
>              lenC = lenR :+ 0
>         in [  let i = -2 * pi * fromIntegral k / lenR
>             in (1/lenC) * sum [  (xs!!n) * exp (0 :+ i * fromIntegral n)
>                              | n <- [0,1..lenI-1] ]
>             | k <- [0,1..lenI-1] ]
>     -- normalization function for results
>     normalize :: (Ord a, Fractional a) => [a] -> [a]
>     normalize [] = []
>     normalize xs = 
>         let xmax = maximum xs
>         in  map (/xmax) xs

Finally, the printResults function allows us to see the results of the
DFT in a readable format.

> printResults :: [(Frequency, Magnitude)] -> IO ()
> printResults = putStrLn . concatMap (\(f,m) -> show f ++ "...." ++ show m ++ "\n")
