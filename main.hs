
import System.IO ()
import Data.Char ()
import Data.String ()
import Data.Foldable ()
import Data.List (nub)


array = [["A","B","C"]
        ,["B","C","E"]
        ,["C","G"]
        ,["D","A","F"]
        ,["E","F"]
        ,["F","H"]
        ,["G","Y"]]

array1 = [["A","B"]
        ,["B","C"]
        ,["C","A"]]
--nub se usa para eliminar los repetidos de una lista


buscarDependencias :: [[String]] -> [String] -> Int -> [String]
buscarDependencias all (x:xs) pos 
    | pos == 0 =  x : buscarDependencias all xs (pos+1)
    | otherwise = x : revisarDep all x ++ buscarDependencias all xs pos
buscarDependencias all _ pos = []

revisarDep :: [[String]] -> String -> [String]
revisarDep all@(x:xs) actual
    | head x == actual = tail x ++ revisarDep all (head (tail x)) ++ buscarDependencias all (tail x) 0
    | otherwise = revisarDep xs actual
revisarDep _ actual = [] 

ejecutar ::[[String]] -> [[String]] -> [[String]]
ejecutar all (x:xs) = nub (buscarDependencias all x 0) : ejecutar all xs
ejecutar all _ = [[]]


main::IO()
main = do
   print $ ejecutar array array