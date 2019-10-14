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