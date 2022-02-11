module SyncWord(
    mbSyncWord,
) where

import Graph

type Graph2 = [Node2]
type Node2 = ([NodeId2], [NodeNeighbor2])
type NodeId2 = Int
type NodeNeighbor2 = ([NodeId2], String)

type BFSNode = (Node2, String)

mbSyncWord :: Graph2 -> Maybe String
mbSyncWord graph =
  if anySingleStateinGraph2 graph 
    then
      let start = longestState graph
      in  bfsSearch graph [] $ map (\n -> (n, "")) start
    
    else 
      Nothing

anySingleStateinGraph2:: Graph2 -> Bool
anySingleStateinGraph2 [] = False 
anySingleStateinGraph2 ((nodeId,neighbors):rest) =
  if  length (filter(\(x,y)->length x ==1) neighbors) > 0 then True else anySingleStateinGraph2 rest

longestState :: Graph2 -> [Node2]
longestState graph =
  let nodes = map fst graph
      lens = map length nodes
      max_len = foldl max 0 lens
  in  filter (\node -> length (fst node) == max_len) graph


bfsSearch :: Graph2 -> [[NodeId2]] -> [BFSNode] -> Maybe String
bfsSearch graph tabu_node_states lst =
  case lst of
    [] -> Nothing
    (bfs_node : rest) ->   
      let
        (node, current_word) = bfs_node 
        (node_states, moves) = node  
      in
        if hasElement tabu_node_states node_states
          then
            bfsSearch graph tabu_node_states rest
          else
            case singletonWord moves of 
              Just w -> Just $ current_word ++ w 
              Nothing -> 
                let new_nodes = map (nodeToCheck graph current_word) moves
                    new_tabu = node_states : tabu_node_states
                in  bfsSearch graph new_tabu $ rest ++ new_nodes

nodeToCheck :: Graph2 -> String -> NodeNeighbor2 -> BFSNode
nodeToCheck graph word (pth_label, new_letter) = (graph_node, new_word)
  where
    new_word = word ++ new_letter
    graph_node = head $ filter (\n -> (fst n) == pth_label) graph

singletonWord :: [NodeNeighbor2] -> Maybe String
singletonWord lst =
  case lst of
    [] -> Nothing 
    (currentNode:rest) ->
      let (nodeID, letter) = currentNode
      in
        case nodeID of
          [_] -> Just letter
          _ -> singletonWord rest 