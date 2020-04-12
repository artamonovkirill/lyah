import Control.Monad (guard, filterM)

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b 
applyMaybe Nothing _  = Nothing
applyMaybe (Just a) f = f a

type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft = land (\b (l, r) -> (l+b, r))

landRight :: Birds -> Pole -> Maybe Pole
landRight = land (\b (l, r) -> (l, r+b))

land :: (Birds -> Pole -> Pole) -> Birds -> Pole -> Maybe Pole
land f b old
        | abs (l - r) < 4 = Just new
        | otherwise       = Nothing
        where new@(l, r) = f b old

(-:) :: a -> (a->b) -> b
x -: f = f x  

banana :: Pole -> Maybe Pole  
banana _ = Nothing

foo :: (Num a, Show a) => Maybe a -> Maybe String -> Maybe String  
foo a b = do
        x <- a
        y <- b
        Just (show x ++ y)

marySue :: (Num a, Ord a) => Maybe a -> Maybe Bool
marySue a = do
        x <- a
        Just (x > 8)

routine :: Maybe Pole  
routine = 
        let start = (0,0)
        in do
                first <- landRight 2 start
                Nothing
                second <- landLeft 2 first
                landLeft 1 second

justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x 

wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..100]  
    guard ('7' `elem` show x)  
    return x

type KnightPos = (Int,Int)  

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
        (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
        guard (c' `elem` [1..8] && r' `elem` [1..8])
        return (c',r')

in3 :: KnightPos -> [KnightPos]
-- in3 start = do
--         first <- moveKnight start
--         second <- moveKnight first
--         moveKnight second
in3 start = moveKnight start >>= moveKnight >>= moveKnight 

inMany :: Int -> KnightPos -> [KnightPos]
inMany n start =  return start >>= foldr (<=<) return (replicate n moveKnight)

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  

canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start

powerset :: [a] -> [[a]]
powerset xs = filterM (\_ -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)