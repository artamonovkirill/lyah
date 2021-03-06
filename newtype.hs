newtype Pair b a = Pair { getPair :: (a,b) } 

instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  

    newtype CoolBool = CoolBool { getCoolBool :: Bool } 

helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"  