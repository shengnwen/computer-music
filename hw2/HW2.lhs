> module HW2 where
> import Euterpea

Exercise 3.1 
- part 1

> f1 :: Int -> [Pitch] -> [Pitch]
> f1 i xs = map (trans i) xs

- part 2

> f2 :: [Dur] -> [Music a]
> f2 xs = map rest xs
     
Exercise 3.4
    --simple x y z = x * (y + z)

> applyEach :: [a -> b] -> a  -> [b]
> applyEach fs a = let ae f = f a
>                  in map ae fs                  

applyEach [(+5), (+3)] 5
or another way:
applyEach [] a = []
applyEach (f: fx) i = f i : applyEach fx i

Exercise 3.5

> applyAll :: [(a -> a)] -> a -> a
> applyAll fs a = let aa f a = f a
>                 in foldr aa a fs

or another way:
applyAll (f:[]) x = f x
applyAll (f: fx) x = f (applyAll fx x)

Exercise 3.11
    
> chrom :: Pitch -> Pitch -> Music Pitch
> chrom p1 p2 
>      | (absPitch p1) == (absPitch p2) = note qn p1 
>      | (absPitch p1) > (absPitch p2) = (note qn p1) :+: chrom (trans (-1) p1) p2
>      | (absPitch p1) < (absPitch p1) = (note qn p1) :+: chrom (trans 1 p1) p2

