data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

-- changeToP :: Tree Char -> Tree Char  
-- changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)  
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
changeToP _ Empty = Empty

elemAt :: Directions -> Tree a -> Maybe a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _ ) = Just x
elemAt _ Empty = Nothing

-- type Breadcrumbs = [Direction]  

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node value left right, breadcrumbs) = Just (left, LeftCrumb value right:breadcrumbs)  
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node value left right, breadcrumbs) = Just (right, RightCrumb value left:breadcrumbs) 
goRight (Empty, _) = Nothing

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]  

goUp :: Zipper a -> Maybe (Zipper a)
goUp (tree, LeftCrumb value right:breadcrumbs) = Just (Node value tree right, breadcrumbs)
goUp (tree, RightCrumb value left:breadcrumbs) = Just (Node value left tree, breadcrumbs)
goUp (_, []) = Nothing

type Zipper a = (Tree a, Breadcrumbs a)  

modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)  
modify _ (Empty, _) = Nothing

attach :: Tree a -> Zipper a -> Zipper a 
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a 
topMost z = maybe z topMost $ goUp z

type ListZipper a = ([a],[a])  

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward ([], _) = Nothing
goForward (x:xs, bs) = Just (xs, x:bs) 

goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (_, []) = Nothing
goBack (xs, b:bs) = Just (b:xs, bs) 

type Name = String  
type Data = String  
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show) 

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]  

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb]) 

fsUp :: FSZipper -> Maybe FSZipper 
fsUp (item, FSCrumb name bs as:cs) = Just (Folder name (bs ++ [item] ++ as), cs) 
fsUp (_, []) = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo _ (File _ _, _) = Nothing
fsTo name (Folder fn items, cs) = 
    let (ls, item:rs) = break (nameIs name) items
    in Just (item, FSCrumb fn ls rs:cs)

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Folder _ items, breadcrumbs) = (Folder n items, breadcrumbs) 
fsRename n (File _ content, breadcrumbs) = (File n content, breadcrumbs) 

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile i (Folder name items, breadcrumbs) = Just (Folder name (i:items), breadcrumbs)
fsNewFile _ (File _ _, _) = Nothing