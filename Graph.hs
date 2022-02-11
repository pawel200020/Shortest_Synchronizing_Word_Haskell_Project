module Graph
  ( removeDubs,
    hasElement,
    generateRestGraph,
    generateSubsets,
  )
where

type NodeId = Int
type Node = (NodeId, [NodeNeighbor])
type Graph = [Node]
type NodeNeighbor = (NodeId,String)

type Graph2 = [Node2]
type Node2 = ([NodeId2], [NodeNeighbor2])
type NodeId2 = Int
type NodeNeighbor2 = ([NodeId2], String)

removeDubs:: [Int]->[Int]
removeDubs []=[]
removeDubs (l:list) = l:(removeDubs (filter (/=l) list))

hasElement :: Eq a => [a] -> a -> Bool
hasElement lst a =
  case filtered_list of
    [] -> False
    _ -> True
    where
        filtered_list = filter ((==) a) lst

generateRestGraph:: [[Int]]->Graph->[String]->Graph2
generateRestGraph [] _ _ =[]
generateRestGraph (s:subsets) graph alphabet = (s,generateNeighbor s graph (sAlphabet graph alphabet s)):(generateRestGraph subsets graph alphabet)
          where
              sAlphabet :: Graph -> [String] -> [Int] -> [String]
              sAlphabet graph alphabet s = filter (checkLetter graph s) alphabet              

              checkLetter :: Graph -> [Int] -> String -> Bool
              checkLetter graph s letter =
                case graph of
                  [] -> True    
                  (node:rest) ->
                    let (node_state, nbrs) = node
                    in
                      if (hasElement s node_state) 
                        then
                          if (hasLetter nbrs letter)       
                            then checkLetter rest s letter  
                            else False                      
                        else checkLetter rest s letter 
              
              hasLetter :: [NodeNeighbor] -> String -> Bool
              hasLetter nbrs letter =
                case nbrs of
                  [] -> False       
                  (n:rest) ->
                    let the_letter = snd n     
                    in
                      if the_letter == letter
                        then True
                        else hasLetter rest letter



generateNeighbor:: [Int]->Graph->[String]->[NodeNeighbor2]
generateNeighbor _ _ [] = []
generateNeighbor x graph (a:alphabet) = (getSortedNeighbours x graph a,a):generateNeighbor x graph alphabet 
        where
              
                getSortedNeighbours:: [Int]->Graph->String->[Int]
                getSortedNeighbours x graph letter = removeDubs (qsort (getNeighborbyLetter x graph letter))

                getNeighborbyLetter:: [Int]->Graph->String->[Int]
                getNeighborbyLetter [] _ _=[]
                getNeighborbyLetter (x:xs) graph letter= (getIdNeighbour x graph letter): (getNeighborbyLetter xs graph letter)

                getIdNeighbour:: Int -> Graph-> String -> NodeId
                getIdNeighbour number graph letter = if( length (getNodeNeighborSelectedLetter number graph letter)>0) then fst (head(getNodeNeighborSelectedLetter number graph letter)) else -1

                getNodeNeighborSelectedLetter:: Int -> Graph-> String -> [(NodeId, String)]
                getNodeNeighborSelectedLetter number graph letter =  filter (\(neighbor,sign)-> letter==sign) (getNodeNeighbor number graph)

                getNodeNeighbor:: Int -> Graph -> [NodeNeighbor]
                getNodeNeighbor _ [] = []
                getNodeNeighbor number ((nodeId,neighbors):rest) = if nodeId == number then neighbors else getNodeNeighbor number rest

generateSubsets:: Graph->[[Int]]
generateSubsets graph= filter (\x-> length x >1)(subsets (listOfvertex graph))
        where
                listOfvertex :: Graph -> [Int]
                listOfvertex []=[]
                listOfvertex ((nodeId,neighbors):rest) = nodeId:listOfvertex rest

                subsets :: [Int] -> [[Int]]
                subsets [] = [[]]
                subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort left) ++ [x]++(qsort right)
        where
                left = [y|y<-xs,y<=x]
                right = [y|y<-xs,y>x]