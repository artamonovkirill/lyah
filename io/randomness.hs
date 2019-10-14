import Data.List
import System.Random

threeCoins :: StdGen -> [Bool]
threeCoins = take 3 . randoms

randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = 
    let (value, newGen) = random gen 
    in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 g = ([], g)
finiteRandoms n g = 
    let (v, ng) = random g
        (r, fg) = finiteRandoms (n-1) ng
    in (v:r, fg)

main = do  
    gen <- getStdGen
    putStrLn . take 20 . randomRs ('a','z') $ gen
    gen' <- newStdGen  
    putStr . take 20 . randomRs ('a','z') $ gen'