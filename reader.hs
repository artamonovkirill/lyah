import System.Random (StdGen, random)
import Control.Monad.State (state, State, runState)

-- addStuff :: Int -> Int  
-- addStuff = do  
--     a <- (*2)  
--     b <- (+10)  
--     return (a+b)

addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, _) = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  

type Stack = [Int]  
  
-- pop :: Stack -> (Int,Stack)
-- pop (x:xs) = (x,xs)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs) 

-- push :: Int -> Stack -> ((),Stack)
-- push x s = ((), x:s)

push :: Int -> State Stack ()  
push x = state $ \xs -> ((),x:xs) 

stackManip :: State Stack Int
stackManip = do  
    push 3
    pop
    pop