import Control.Monad (foldM, liftM)  
  
solveRPN :: String -> Maybe Double  
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)  
    return result

foldingFunction :: [Double] -> String -> Maybe [Double] 
foldingFunction (x:y:ys) "+" = return $ x+y:ys
foldingFunction (x:y:ys) "-" = return $ y-x:ys
foldingFunction (x:y:ys) "*" = return $ x*y:ys
foldingFunction (x:y:ys) "/" = return $ y/x:ys
foldingFunction xs s         = liftM (:xs) . readMaybe $ s
-- foldingFunction (x:y:ys) "^" = Just $ y**x:ys
-- foldingFunction (x:xs) "ln"  = Just $ log x:xs  
-- foldingFunction xs "sum"     = Just $ [sum xs] 

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  