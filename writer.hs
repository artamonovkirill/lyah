import Data.Monoid
import Control.Monad.Writer

-- isBigGang :: Int -> Bool  
-- isBigGang x = x > 9  

isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String  
type Price = Sum Int 

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int  
logNumber x = do
    tell ["Got number: " ++ show x]
    return x

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5 
    tell ["Gonna multiply these two"]
    return (a*b)  

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b   
    | b == 0    = do
        tell . toDiffList $ ["Finished with " ++ show a]  
        return a 
    | otherwise = do
        let m = a `mod` b
        tell . toDiffList $ [show a ++ " mod " ++ show b ++ " = " ++ show m]  
        gcd' b m  

gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        let m = a `mod` b
        result <- gcdReverse b m
        tell [show a ++ " mod " ++ show b ++ " = " ++ show m]  
        return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList x) <> (DiffList y) = DiffList $ x . y

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)

finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = tell . toDiffList . pure $ "0"
finalCountDown x = do  
    finalCountDown (x-1)  
    tell . toDiffList . pure .show $ x

finalCountDown' :: Int -> Writer [String] ()  
finalCountDown' 0 = tell . pure $ "0"
finalCountDown' x = do  
    finalCountDown (x-1)  
    tell . pure . show $ x