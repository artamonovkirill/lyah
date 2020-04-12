type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft = land (\b (l, r) -> (l+b, r))

landRight :: Birds -> Pole -> Either String Pole
landRight = land (\b (l, r) -> (l, r+b))

land :: (Birds -> Pole -> Pole) -> Birds -> Pole -> Either String Pole
land f b old
        | abs (l - r) < 4 = Right new
        | otherwise       = Left "out of balance"
        where new@(l, r) = f b old

banana :: Pole -> Either String Pole  
banana _ = Left "banana"

routine :: Either String Pole  
routine = 
        let start = (0,0)
        in do
                first <- landRight 2 start
                -- banana
                second <- landLeft 2 first
                -- second <- la:ndLeft 5 first
                landLeft 1 second