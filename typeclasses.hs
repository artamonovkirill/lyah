import Data.Map (Map)
import qualified Data.Map as Map

-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--                      } deriving (Show)

data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)  

tellCar :: (Show a) => Car String String a -> String 
tellCar Car {company = c, model = m, year = y} = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  

data Vector a = Vector a a a deriving (Show)  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n 

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Name = String  
type PhoneNumber = String  
type PhoneBook = [(Name,PhoneNumber)] 
phoneBook :: PhoneBook
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]
-- type IntMap v = Map Int v  
type IntMap = Map Int

data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = Map Int (LockerState, Code)  

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup n m =
    case Map.lookup n m of
        Nothing         -> Left $ "Locker number " ++ show n ++ " doesn't exist!"
        Just (Taken, _) -> Left $ "Locker " ++ show n ++ " is already taken!"  
        Just (Free, c)  -> Right c

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)  
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

-- infixr 5  ++ 
-- (++) :: [a] -> [a] -> [a]  
-- []     ++ ys = ys  
-- (x:xs) ++ ys = x : (xs ++ ys) 

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys  = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys) 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance Functor Tree where 
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x l r)   = Node (f x) (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`  
                             f x         `mappend`  
                             foldMap f r

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x t@(Node a l r) 
    | x == a = t
    | x < a  = Node a (treeInsert x l) r
    | x > a  = Node a l (treeInsert x r)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a l r)
    | x == a = True
    | x < a  = treeElem x l
    | x > a  = treeElem x r

-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)  

data TrafficLight = Red | Yellow | Green  
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 

instance Read TrafficLight where  
    readsPrec _ input 
        | input == "Red Light" = [(Red, "Red Light")]
        | input == "Green Light" = [(Green, "Green Light")]
        | input == "Yellow Light" = [(Yellow, "Yellow Light")]
        | otherwise = []

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where 
    yesno [] = False
    yesno _  = True

instance YesNo Bool where 
    yesno = id

instance YesNo (Maybe a) where 
    yesno Nothing = False
    yesno _       = True

instance YesNo (Tree a) where 
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- instance Functor (Map k) where  
--     fmap f = Map.fromList $ fmap $ Map.toList

-- instance Functor (Map k) where  
--     fmap f = Map.fromList . map (\(k, v)-> (k, f v)) . Map.toList

class Tofu t where  
    tofu :: j a -> t a j  

data Frank a b  = Frank {frankField :: b a} deriving (Show)
instance Tofu Frank where  
    tofu x = Frank x  

data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)
instance Functor (Barry a b) where
    --fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}  
    fmap f (Barry y d) = Barry (f y) d