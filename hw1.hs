{- Melike Selin Aydın 1678747 -}
module Hw1 where

data Path = Nil | Node Char Integer Path deriving  (Read, Show, Eq) 
data Tree = Empty | Leaf Char Integer | Branch Char Integer Tree Tree deriving  (Read, Show, Eq)


{-aşağıdaki fonksyon tree elemanını empty midir kontrol ediyor-}	
emptyMi :: Tree -> Bool
emptyMi a = 
	if a == Empty
		then True
		else False
	
--aradığımız bir karakterin branchin hangi kolunda olduğuna bakmak için

burdaMisin :: Tree -> Char -> Integer
burdaMisin Empty c = 0
burdaMisin (Leaf a b) c =
	if a == c
		then 1
		else 0
burdaMisin (Branch a b s t) c =
	if a == c
		then 1
		else max (burdaMisin s c) (burdaMisin t c)

--bir pathden bir tree oluşturuyor

atama :: Path -> Tree -> Tree
atama Nil x = x
atama (Node c i p) Empty = 
	atama p (Leaf c i)
atama (Node c i p) (Leaf a b) =
	if c == a
		then (atama p (Leaf a (i + b)))
		else (atama (Node c 0 p) (Branch a b (Leaf c i) Empty))
atama (Node c i p) (Branch a b s t) =
	if c == a
		then (atama p (Branch a (b + i) s t))
		else if (burdaMisin s c) == 1 --branchin s kolundaysa
			then (Branch a b (atama (Node c i p) s) t)
			else if (burdaMisin t c) == 1 --branchin t kolundaysa
				then (Branch a b s (atama (Node c i p) t))
				else if (emptyMi s) --s boşsa s koluna degilse t koluna eklemeliyiz
					then (Branch a b (atama (Node c i p) s) t)
					else (Branch a b s (atama (Node c i p) t))
	
--bütün path treelerinden bir tree oluşturuyor, arguman sayısını bir e düşürmek için kullandım				
		
kolayiVar :: [Path] -> Tree -> Tree
kolayiVar [] a = a
kolayiVar (x:xs) a = 
	kolayiVar xs (atama x a)

			
hw1 :: [Path] -> Tree
hw1 x = kolayiVar x Empty
	
