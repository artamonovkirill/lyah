import System.Random (StdGen, random, Random, RandomGen, mkStdGen)
import Control.Monad.State (state, State, runState, get, put)

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

-- threeCoin    s :: StdGen -> (Bool, Bool, Bool)  
-- threeCoins gen =   
--     let (firstCoin, newGen) = random gen  
--         (secondCoin, newGen') = random newGen  
--         (thirdCoin, _) = random newGen'  
--     in  (firstCoin, secondCoin, thirdCoin)  

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random 

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c) 

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

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()             

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]