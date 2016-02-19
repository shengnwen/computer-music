> module HW1 where
> import Euterpea

Exercise 1.2:
    According to definition：
       x = (a - b), y = a, z = b
    so, simple (a - b) a b
       => (a - b) * (a + b)
       => a * a - b * a + b * a - b * b
       => a * a - b * b

Exercise 1.3:
    a)Well-typed:
	[A, B, C] :: [PitchClass]
        (-42, Ef) :: (Integer, PitchClass)
	[('a', 3),('b', 5)] :: [(Char, Integer)]
    	(simple 1 2 3, simple):: (Integer, Integer->Integer->Integer->Integer) 
	["I", "love", "Euterpea"]::[String]
    b)Ill-typed:
	[D, 42] :: [PitchClass, Interger] => There have different classes in one array
	simple 'a' 'b' 'c'  => The parameter should be Integer, not Char and 'a' * ('b' + 'c') is ill typed

Exercise 1.4:
    hNote :: Dur -> Pitch -> Integer -> Music Pitch
    hNote d p i = note d p :=: note d (trans(i) p)
    
    hList :: Dur -> Integer -> [Pitch] -> Music Pitch
    hList d[] = rest 0
    hList d i (p:ps) = hNote d p i :+: hList d i ps    

    mel :: Integer ->  hList -> Music Pitch
    mel i list = i hList qn [p1, p2, p3]
	=> hNote qn p1 i :+: hNote qn p2 i :+: hNote qn p3 i

Exercise 2.5:	
    transM :: AbsPitch -> MusicPitch -> Music Pitch
    transM ap (Prim (Note d p)) = Prim (Note d (trans (ap) p))
    transM ap (Prim (Rest d)) = Prim (Rest d)
    transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
    transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
    transM ap (Modify c m) = Modify (TransM ap) (Modify c m)



