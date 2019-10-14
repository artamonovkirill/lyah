-- main = do  
--     contents <- getContents  
--     putStr $ shortLinesOnly contents

-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly input = 
--     let allLines = lines input
--         shortLines = filter (\l -> length l < 10) allLines
--     in unlines shortLines

main = interact $ unlines . filter (\l -> length l < 10) . lines