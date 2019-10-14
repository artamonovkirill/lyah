main = do putS "hello"

putS :: String -> IO ()
putS []     = return ()
putS (x:xs) = do
    putChar x
    putS xs