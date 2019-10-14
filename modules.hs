-- import Data.List
-- import Data.List (nub, length)
-- import Data.List hiding (nub)
-- import qualified Data.List 
import qualified Data.List as L (nub, length, tails)

import Data.Char (chr, ord)    
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)

-- import qualified Geometry.Sphere as Sphere  
-- import qualified Geometry.Cuboid as Cuboid  
-- import qualified Geometry.Cube as Cube  

numUniques :: (Eq a) => [a] -> Int  
numUniques = L.length . L.nub  

search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack = 
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack)

cypher :: Int -> String -> String
cypher n = map $ chr . (+n) . ord

encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted

decode :: Int -> String -> String
decode n = encode (-n)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key dictionary = snd <$> find (\(k,v) -> k == key) dictionary

fromList :: Ord k => [(k, a)] -> Map k a
-- fromList [] = Map.empty
-- fromList ((k,v):xs) = Map.insert k v $ fromList xs
fromList = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
-- phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs 
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 
