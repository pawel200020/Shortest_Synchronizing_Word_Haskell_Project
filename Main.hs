module Main
  ( main,
  )
where

import System.IO
import System.Environment
import Parse
import Graph
import SyncWord

--Najkrótsze słowo synchronizujące.
--pogram pobiera z pliku opis deterministycznego automatu następnie stwierdza czy ma on słowo synchronizujące czy nie
--słowo synchronizujące to takie słowo które niezależnie od stanu w którym się znajdujemy to zawsze skończymy w tym samym stanie
--źródło algorytmu: http://www.math.uni.wroc.pl/~kisiel/auto/volkov-surv.pdf

readFileToList :: Handle -> IO [String]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  return (line:rest)

main :: IO ()
main = do
        (inFileName:_) <-getArgs                        
        inFileHandle <-openFile inFileName ReadMode 
        list<-readFileToList inFileHandle                
        let alphabet = (parseAlphabet.head) list
        let graph = (convertData.tail) list
        let graph2 = generateRestGraph (generateSubsets graph) graph alphabet
        let sync_word = mbSyncWord graph2            
        case sync_word of
          Nothing ->
            print "Automaton is not synchronizing. Sorry."

          Just word ->
            print $ "Automaton is synchronizing: " ++ word