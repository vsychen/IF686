-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 7, Questão 1-}
    {-compose-}
compose :: (u -> v) -> [(t -> u)] -> [(t -> v)]
compose f fl = [f . (head fl)] ++ (compose f (tail fl))

	{-map and fold graph-}
type Node t = t
type Edge t = (Node t, Node t, Int)
data Graph t = NilG
               | Graph [Node t] [Edge t] deriving (Eq, Show)

mapNode :: [Node t] -> (t -> u) -> [Node u]
mapNode [] _ = []
mapNode (x:xs) f = (f x) : mapNode xs f

mapEdge :: [Edge t] -> (t -> u) -> [Edge u]
mapEdge [] _ = []
mapEdge ((x1, x2, d):xs) f = ((f x1), (f x2), d) : (mapEdge xs f)

mapGraph :: Graph t -> (t -> u) -> Graph u
mapGraph (NilG) _ = (NilG)
mapGraph (Graph n e) f = Graph (mapNode n f) (mapEdge e f)

foldrNode :: (Eq t) => [Node t] -> u -> (t -> u -> u) -> u
foldrNode x b f = if x == [] then b
                  else f (head x) (foldrNode (tail x) b f)

foldrGraph :: (Eq t) => Graph t -> u -> (t -> u -> u) -> u
foldrGraph (NilG) b _ = b
foldrGraph (Graph n e) b f = foldrNode n b f
{-
	{-filterTree-}
data Tree t = NilT
              | Node t (Tree t) (Tree t) deriving (Eq, Show)

--


filterTree :: (Ord t) => Tree t -> (t -> Bool) -> [Tree t]
filterTree (NilT) _ = []
filterTree (Node x t1 t2) = 
-}