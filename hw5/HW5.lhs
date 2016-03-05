CPSC 432/532 Homework Assignment #5
YOUR NAME HERE

> module HW5 where
> import WavTools
> import DFT
> import Data.List

Problem 1
Fill in the following function definition such that it 
reads in a WAV file and then writes a TXT file that contains
the resulting DFT using the dft function in DFT.lhs. The DFT 
results should look like this in the output file:

frequency0   magnitude0
frequency1   magnitude1
frequency2   magnitude2
...
frequencyN   magnitudeN

where N is the number of samples (and thus DFT entries). The 
frequencies and magnitudes should be separated by a tab 
character, which is "\t" in Haskell. Your function should 
throw an error if the supplied WAV file has >1 channel.

> concatFM :: (Frequency, Magnitude) -> String
> concatFM (f, m) = (show f) ++"\t" ++(show m)

> writeDFT :: FilePath -> FilePath -> IO ()
> writeDFT inFile outFile = do
>  (sr, ch, samples) <- readWav inFile
>  if ch > 1
>    then error "More then one Channel!\n"
>    else do
>      let fm = dft sr samples
>      let strs = map concatFM fm
>      writeFile outFile (intercalate "\n" strs)

===================================================================

Problem 2
Fill in the function definition below such that it reads 
in a WAV file, performs the DFT, and prints the entry with 
the highest magnitude. As with problem 1, your function 
should throw an error when there is >1 channel.

> findPeak1 :: FilePath -> IO ()
> findPeak1 inFile = do
>  (sr, ch, samples) <- readWav inFile
>  if ch > 1
>    then error "More then one Channel!\n"
>    else do
>      let fm = dft sr samples
>      let (fs, ms) = unzip fm
>      let maxIdx = snd . maximum $ zip ms [0 .. ]
>      print (fm!!maxIdx)


===================================================================

Problem 3
Fill in the function definition below such that it does the 
same thing as for findPeak1, but only checks magnitudes 
within the audible range (so, a peak falling outside that 
range would be ignored). Assume an audible range of 20Hz to
22000Hz. As with problems 1 and 2, your function should throw 
an error when there is >1 channel.

> findPeak2 :: FilePath -> IO ()
> findPeak2 inFile = do
>  (sr, ch, samples) <- readWav inFile
>  if ch > 1
>    then error "More then one Channel!\n"
>    else do
>      let fm = filter (\(a,b) -> a>=20 && a<=22000)(dft sr samples)
>      let (fs, ms) = unzip fm
>      let maxIdx = snd . maximum $ zip ms [0 .. ]
>      print (fm!!maxIdx)      


===================================================================

Problem 4

Test your three functions on the supplied WAV files. If your 
functions are working corretly, you will see some discrepencies
between what findPeak1 and findPeak2 print, and that the 
correct frequency is only found for one file. Explain your 
observations.


### 4.1 Observation
Test 1 writeDFT

writeDFT "sine1500_48000.wav" "1.txt"
writeDFT "sine440_44100.wav" "2.txt"
writeDFT "sine500_44100.wav" "3.txt"

Test 2 findPeak1

findPeak1 "sine1500_48000.wav"
-> (46500.0,1.0)
findPeak1 "sine440_44100.wav"
-> (43700.0,1.0)
findPeak1 "sine500_44100.wav"
-> (500.0,1.0)

Test 3 findPeak2

findPeak2 "sine1500_48000.wav"
-> (1500.0,0.9999999999999974)
findPeak2 "sine440_44100.wav"
-> (400.0,0.9999999999999799)
findPeak2 "sine500_44100.wav"
-> (500.0,1.0)

# 4.2 Explanation
Because the implementation of findPeak1 and findPeak2 is different with one condition: the frequency should
between 20HZ to 22000HZ. Only the peak entry in "sine500_44100.wav" is inside this range, so this entry is 
the only one that both return in findPeak1 and findPeak2.


