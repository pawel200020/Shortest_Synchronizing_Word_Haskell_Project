{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parse
  ( convertData,
    parseAlphabet,
  )
where

type NodeId = Int
type Node = (NodeId, [NodeNeighbor])
type Graph = [Node]
type NodeNeighbor = (NodeId,String)

convertData:: [String]->Graph
convertData [] = []
convertData (a:b:rest) = (read a ::Int, (parseTuple.parseNumber) b):convertData rest
        where
                parseNumber:: String-> [String]
                parseNumber (w:word) = foldl(\acc x -> if x==' ' then acc++[""] else init acc++[last acc++[x]]) [[w]] word
                
                parseTuple:: [String]->[NodeNeighbor]
                parseTuple [] = []
                parseTuple (string:int:rest) = (read int :: Int,string): parseTuple rest 

parseAlphabet:: String->[String]
parseAlphabet []= []
parseAlphabet (w:word)= if w==' ' then parseAlphabet word else [w]:parseAlphabet word