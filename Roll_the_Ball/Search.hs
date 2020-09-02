{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a =  MyConst {
    getSt::s,
    getAc:: Maybe a,
    getPr::Maybe(Node s a),
    getDe:: Int,
    getCh:: [Node s a]
} deriving (Eq)





{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState = getSt

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = getPr

nodeDepth :: Node s a -> Int
nodeDepth = getDe

nodeAction :: Node s a -> Maybe a
nodeAction = getAc

nodeChildren :: Node s a -> [Node s a]
nodeChildren = getCh



createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace  =  facemNodNou  Nothing 0 Nothing 

facemNodNou parent depth action level = nod
    where
        nod = (MyConst level action parent depth   (map (\(abcd, xyz) -> (facemNodNou (Just nod) (depth +1) (Just abcd) xyz )) (successors level)))

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera


-}



bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs  node =  iterate helperIterate ([node],[node])


helperIterate (first,second) =
    if (length second) == 0 then  (first,second)
    else ( (getCh (head second)),(drop 1 second) ++(getCh (head second)))



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
