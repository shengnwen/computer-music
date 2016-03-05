CPSC 432/532 Homework #4 
Wen Sheng

> module HW4 where
> import WavTools

> type Frequency = Double -- in Hz
> type Amplitude = Double -- from 0.0 to 1.0
> type Seconds = Double
> type Sample = Double

========================================================================

Problem 1
Suppose we record an hour of stereo audio in WAV format with a sample 
rate of 48000Hz and a sample resolution of 24bits per sample. How much 
disk space in megabytes (mb) will the samples require (ignoring storage 
overhead from the WAV header)?

Note: use the standard of 1024bytes/kb and 1024kb/mb.

Answer:
60min * 60s/min * 48000Hz * 24bits / (8 bits/bytes * 1024 bytes/kb * 1024kb/mb)
= 494.4 mb
  
========================================================================

Problem 2

Suppose we have a sinewave with a frequency of 20000Hz - which is above 
the range of hearing for many people (although not all). Suppose we sample
this signal at a rate of 25000Hz. The result will be a form of aliasing, 
and we will capture a sinewave at a different frequency from that of the 
original. What frequency will show up due to aliasing in the recording?

Hint: try diagraming the situation by hand on paper.

Answer: 
Using the diagram, sampling on every 4 times of original sinewave will 
generate one complete sampe sinewave.
So the sampled sinewave frequency will be
20000Hz * 1 / 4 = 5000 Hz

========================================================================

Problem 3
Fill in the definition of sineWav below so that it generates a sinewave
with freq frequency and amp amplitude for sec seconds and writes the 
result to outFile (you can assume it ends in ".wav"). You should write 
a mono WAV file with only a single channel.

> repeatList :: Double -> [Amplitude] -> [Amplitude]
> repeatList 0 xs = []
> repeatList n xs = xs ++ (repeatList (n - 1) xs)

> genStandardSample :: Frequency -> SampleRate -> [Sample]
> genStandardSample freq 0 = []
> genStandardSample freq sr = (genStandardSample freq (sr - 1)) ++ [sin ((2*pi*freq) / (fromIntegral sr))]


> genSample :: Frequency -> SampleRate -> Amplitude -> Seconds -> [Sample]
> genSample freq sr amp sec = 
>   let perSample= map (*amp) (genStandardSample freq sr) 
>   in repeatList sec perSample

> sineWav :: SampleRate -> Frequency -> Amplitude -> Seconds -> FilePath -> IO ()
> sineWav sr freq amp sec outFile = do
>   let samples = genSample freq sr amp sec
>	in writeWav' outFile sr 1 samples



========================================================================

Problem 4
Fill in the definition of reverseWav below. It should read in a WAV file,
reverse the sound within, and then write the result to a new file. 
Importantly, each channel must be reversed without altering the order of 
the channels! You are essentially trying to reproduce the "Reverse" effect 
in Audacity. 

> reverseWav:: FilePath -> FilePath -> IO ()
> reverseWav inFile outFile = do
>  (sr, ch, samples) <- readWav inFile
>  let splitSamples = uninterleave ch samples
>  writeWav' outFile sr ch (interleave (map reverse splitSamples))

