import System.IO     
import Data.Char

main = do
    contents <- readFile "io/girlfriend.txt"
    let caps = map toUpper contents
    writeFile "io/GIRLFRIEND.txt" caps