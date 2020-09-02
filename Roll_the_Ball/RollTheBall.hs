{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module RollTheBall where
import Pipes
import ProblemState

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = CellConstructor {
	getChara::Char,
	getPos::Position
} deriving (Eq, Ord)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {
	getList::[[Cell]]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}


instance Show Level 
    where 
    	show level = [endl] ++ foldr (\ l1 acc -> acc ++ foldr (\ l2 acc1 -> [(getChara l2)] ++ acc1) [endl] l1) "" (reverse (getList level)) 

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (yd, xd) = Level [[CellConstructor emptySpace (y, x) | x <- [0..xd]] | y <- [0..yd]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (c, (x,y)) level = 
	if (x < (length (getList level))) && y < length ((getList level)!!0 ) && x >= 0 && y >= 0 && emptySpace == getChara((getList level)!!x!!y) then (Level $ before ++ [before1 ++ [CellConstructor c (x,y)] ++ after1] ++ after)
	else level 
		where
			(before1, _:after1) = splitAt y ((getList level)!!x) 
			(before, _:after) = splitAt x (getList level) 
		

addCellAux :: (Char, Position) -> Level -> Level
addCellAux (c, (x,y)) level = 
	if (x < (length (getList level))) && y < length ((getList level)!!0 ) && x >= 0 && y >= 0 then (Level $ before ++ [before1 ++ [CellConstructor c (x,y)] ++ after1] ++ after)
	else level 
		where
			(before1, _:after1) = splitAt y ((getList level)!!x) 
			(before, _:after) = splitAt x (getList level) 

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe  
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel (x, y) list = foldr (\ l acc -> (addCell l acc)) (emptyLevel (x, y) ) list


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) d level = 
	if (elem (getChara((getList level)!!x!!y)) winningCells) || (elem (getChara((getList level)!!x!!y)) startCells)
		then
			level
	else if d == North && x - 1 >= 0 && emptySpace == getChara((getList level)!!(x - 1)!!y) 
		then 
			(addCellAux (emptySpace, (x,y)) (addCell (getChara ((getList level)!!x!!y), (x - 1, y) ) level))  
	else if d == South && x + 1 < (length (getList level)) && emptySpace == getChara((getList level)!!(x + 1)!!y)
		then
			(addCellAux (emptySpace, (x,y)) (addCell (getChara ((getList level)!!x!!y), (x + 1, y) ) level))
	else if d == East && y + 1 < (length ((getList level)!!0)) && emptySpace == getChara((getList level)!!x!!(y + 1))
		then
			(addCellAux (emptySpace, (x,y)) (addCell (getChara ((getList level)!!x!!y), (x, y + 1) ) level))
	else if d == West && y - 1 >= 0 && emptySpace == getChara((getList level)!!x!!(y - 1))
		then
			(addCellAux (emptySpace, (x,y)) (addCell (getChara ((getList level)!!x!!y), (x, y - 1) ) level))
	else
		level

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection = undefined

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel level = undefined

instance ProblemState Level (Position, Directions) where
    successors (Level celule) = [(((i,j),direction),(moveCell (i,j) direction (Level celule))) | direction<-[North,West,South,East], j<-[0..((length (celule!!0))-1)], i<-[0..((length celule)-1)] , (Level celule)/=(moveCell (i,j) direction (Level celule))  ]
    isGoal = undefined
    reverseAction = undefined