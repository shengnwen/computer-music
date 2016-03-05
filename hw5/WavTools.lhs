WAV File reading/writing functions
Donya Quick
Last modified: 09-Feb-2016

> module WavTools where
> import GHC.Int
> import Codec.Wav
> import Data.Audio
> import Data.Array.Base
> import Data.List

Two important features of WAV format:

1. WAV files contain 3 things: a sample rate, a number of channels, 
  and a single list of interleaved samples (the first samples from 
  all channels, then the second samples from all channels, and so on).
  
2. Samples must range from -1.0 to 1.0. 

======================================================================


First, we define some type synonyms for readability. 

> type SampleRate = Int
> type Channels = Int

The Data.Audio library also provides another important synonym that 
we will use:

type Sample = Double

=============================
Reading and Writing WAV Files
=============================

The following function, writeWav', writes a list of interleaved 
samples to a WAV file. Note that there is already a writeWav' in 
Euterpea, but it is for use with Euterpea's signals framework (which 
does not appear in this file).

> writeWav' :: FilePath -> SampleRate -> Channels -> [Sample] -> IO ()
> writeWav' filepath sr cn xs = 
>     let dat = map (fromSample.(*0.999)) xs :: [Int32]
>         array = listArray (0, length dat-1) dat
>         aud = Audio { sampleRate = sr,
>                       channelNumber = cn, 
>                       sampleData = array }
>     in exportFile filepath aud

We can also read a WAV file. This returns three items: the sample 
rate of the file, the number of channels in the file, and the 
interleaved list of samples. To split these appart, you can use 
uninterleave with the number of channels.


> readWav :: FilePath -> IO (SampleRate, Channels, [Sample])
> readWav path = do
>   maybeAudio <- importFile path
>   case maybeAudio :: Either String (Audio Int32) of
>     Left s -> error ("(readWav) file format error: " ++ s)
>     Right (Audio rate channels samples) -> do
>       return(rate, channels, map toSample $ elems samples)

===============================
Sample Formatting (Interleving)
===============================

The following can be used to interleave samples for use with writeWav'.
For example: interleave [[1,1,1], [2,2,2]] ==> [1,2,1,2,1,2]

> interleave :: [[a]] -> [a]
> interleave = concat . transpose

For mono files, this is unnecessary. However, for stereo files, we 
must do the following:

writeWav myFile myRate 2 (interleave [leftChanSamples, rightChanSamples])

Given a bunch of interleaved samples, if we know the channel count we 
can split them apart into their respective channels again.
For example: uninterleave [1,2,1,2,1,2] ==> [[1,1,1], [2,2,2]]

> uninterleave :: Channels -> [a] -> [[a]]
> uninterleave num = transpose . segs where
>     segs [] = []
>     segs v = take num v : segs (drop num v)

We have the following general property:

uninterleave (length xs) (interleave xs) == xs
when all sublists of xs have the same length

Because readWAV is within the IO monad, usage of uninterleave will 
typically look like this:

myFun ... = do
  ...
  (sr, ch, samples) <- readWav fileName
  let splitSamples = uninterleave ch samples
  ...




