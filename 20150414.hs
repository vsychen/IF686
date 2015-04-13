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
data Graph t = NilG
               | Node t [(t, Int)] (Graph t)

mapList :: [(t, Int)] -> (t -> u) -> [(u, Int)]
mapList [] _ = []
mapList (x:xs) f = (f (fst x), (snd x)) : mapList xs f

mapGraph :: Graph t -> (t -> u) -> Graph u
mapGraph (NilG) _ = (NilG)
mapGraph (Node x l g) f = Node (f x) (mapList l f) (mapGraph g f)

auxF :: (Num t) => Graph t -> [t]
auxF (NilG) = []
auxF (Node x l g) = [x] ++ (auxF g)

foldGraph :: (Num t) => Graph t -> (t -> t -> t) -> t -> t
foldGraph (NilG) _ i = i
foldGraph x f i = foldr f i (auxF x)

-- graph's checking function
showList :: (Show t) => [(t, Int)] -> String
showList [] = ""
showList ((n, w):xs) = (" - " ++ (show n) ++ " " ++ (show w)) ++ Main.showList xs

showGraph :: (Show t) => Graph t -> String
showGraph (NilG) = ""
showGraph (Node id list graph) = ((show id) ++ (Main.showList list)) ++ ". " ++ showGraph graph

	{-binaryTreeFilter-}