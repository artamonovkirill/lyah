{-# LANGUAGE TupleSections #-}

import Data.Ratio((%))
import Data.Bifunctor (first, second)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs)= Prob . map (first f) $ xs

instance Applicative Prob where
    pure x = Prob [(x, 1)]
    Prob fs <*> Prob xs = Prob . concatMap (\(v, p1) -> map (\(f, p2) -> (f v, p1*p2)) fs) $ xs

instance Monad Prob where
    return x = Prob [(x, 1)]
    m >>= f = flatten (fmap f m)

instance MonadFail Prob where
    fail _ = Prob []

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob . concatMap multAll $ xs
    where multAll (Prob innerxs,p) = map (second (p *)) innerxs

data Coin = Heads | Tails deriving (Show, Eq) 

coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  

loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  

flipThree :: Prob [Coin]  
flipThree = do
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return [a,b,c]

probability :: ([a] -> Bool) -> Prob [a] -> Rational
probability f = sum . map snd . filter (f . fst) . getProb

dice :: Prob Int
dice = Prob . map (, 1%6) $ [1..6]
