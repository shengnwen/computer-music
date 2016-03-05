CPSC 432/532 Homework Assignment #3 
Wen Sheng

> {-#  LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
> module HW3 where
> import Euterpea
> import Control.DeepSeq -- for problem 3

Problem 1

A. Write the melody of "Frere Jacques" as concisely as you can
(within reason!) as a Music Pitch. Call this value "frere."

define music pitch 

> p1 = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
> p2 = line [e 4 qn, f 4 qn, g 4 hn]
> p3 = line [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
> p4 = line [c 4 qn, g 3 qn, c 4 hn]

> pieces = times 2 p1 :+: times 2 (p2:=:p1) :+: times 2 (p3:=:p2:=:p1) :+: times 2 (p4:=:p3:=:p2)
> frere :: Music Pitch
> frere = let t = (dhn/qn) * (69/120)
>         in instrument RhodesPiano (tempo t pieces)

B. Create a 4-part round where each part has a different 
instrument. Call this value "frereRound." 

> frereRound :: Music Pitch
> frereRound = let t = (dhn/qn) * (69/120)
>    in instrument Violin (tempo t pieces) 
>        :=: instrument HonkyTonkPiano (tempo t (offset bn pieces))
>        :=: instrument Clavinet (tempo t (offset (2 * bn) pieces))
>        :=: instrument Celesta (tempo t (offset (3*bn) pieces))


Note: the parts in a round made from a melody with the form
of "Frere Jacques" are typically offset 2 measures each time. 
So, the first instrument has no offset, the second is offset
by 2 measure, the third by 4 measures, and so on.




C. Generalize your solution from part B to create frereRoundN, a
function that takes a list of InstrumentNames and creates a 
round of that many voices, one for each InstrumentName.

> frereRoundN :: [InstrumentName]->Dur -> Music Pitch
> frereRoundN [] _  = rest 0
> frereRoundN (i:is) l = instrument i (tempo ((dhn/qn) * (69/120)) (offset l pieces))
>     :=: frereRoundN is (l + bn)

===============================================================
Problem 2
Exercise 2.2 from the textbook

1. Define a new algebraic data type called BluesPitchClass that captures this scale (for example, you may wish to use the constructor names Ro, MT, Fo, Fi, and MS).

> data BluesPitchClass = Ro | MT | Fo | Fi | MS

2. Define a type synonym BluesPitch, akin to Pitch.

> type BluesPitch = (BluesPitchClass, Octave)

3. Define auxiliary functions ro, mt, fo, fi, and ms, akin to those in Figure 2.2, that make it easy to construct notes of type Music BluesPitch.

> ro o d = note d (Ro, o)
> mt o d = note d (MT, o)
> fo o d = note d (Fo, o)
> fi o d = note d (Fi, o)
> ms o d = note d (MS, o) 

4. In order to play a value of type Music BluesPitch using MIDI, it will have to be converted into a Music Pitch value. Define a function fromBlues :: Music BluesPitch ! Music Pitch to do this, using the “approximate” translation described at the beginning of this exercise.
Hint: To do this properly, you will have to pattern match against the Music value, something like this:

> fbs (Ro, o) = (C, o)
> fbs (MT, o) = (Ef, o)
> fbs (Fi, o) = (G, o)
> fbs (MS, o) = (Bf, o)
> fbs (Fo, o) = (F, o)
> fromBlues :: Music BluesPitch ->  Music Pitch
> fromBlues (Prim (Note d p)) = Prim (Note d (fbs p))
> fromBlues (Prim (Rest d)) = Prim (Rest d)
> fromBlues (m1:+:m2) = fromBlues m1:+: fromBlues m2
> fromBlues (m1:=:m2) = fromBlues m1:=: fromBlues m2
> fromBlues (Modify c  m) = Modify c (fromBlues m) 


5. Write out a few melodies of type Music BluesPitch, and play them using fromBlues and play.

> p_1 = fromBlues (times 2 (note qn (Ro, 4):+:note qn (MS, 4))) :=: note qn (Ef, 4)

===============================================================
Problem 3
Define an instance of ToMusic1 for BluesPitch and an NFData 
instance for BluesPitchClass. This will allow the use of "play"
directly on Music BluesPitch.

Hint: the ToMusic1 instance can be done in 2 lines of code, 
and the NFData instance can just return () for each one of 
BluesPitch's constructors.

> instance ToMusic1 BluesPitch where
>   toMusic1 = toMusic1.fromBlues

> instance NFData BluesPitchClass where
>   rnf Ro = ()
>   rnf MT = ()
>   rnf MS = ()
>   rnf Fi = ()
>   rnf Fo = ()


