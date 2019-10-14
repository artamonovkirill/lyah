main = do
    task <- getLine
    appendFile "io/todo.txt" $ task ++ "\n"