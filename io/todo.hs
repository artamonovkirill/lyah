import System.Environment (getArgs)
import System.IO (readFile, hClose, hPutStr, hGetContents, openTempFile, IOMode(ReadMode), openFile)
import System.Directory (renameFile, removeFile)
import System.Exit (exitFailure)
import Data.List (delete)

dispatch :: [(String, String -> [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ] 

main = do  
    args <- getArgs
    case length args of 
        0 -> exit "Please provide action as first argument"
        1 -> exit "Please provide file name as second argument"
        _ -> do
            let (command:filename:rest) = args
                action = lookup command dispatch
            case action of 
                Just a -> a filename rest
                Nothing -> exit $ command ++ " is not a valid action"

exit :: String -> IO ()
exit message = do
    putStrLn message
    exitFailure

add :: String -> [String] -> IO ()
add _ [] = exit "Please provide a TODO"
add fileName todoItems = appendFile fileName . concatMap (++ "\n") $ todoItems

view :: String -> [String] -> IO ()
view fileName _ = do 
    contents <- readFile fileName
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

remove :: String -> [String] -> IO ()
remove _ [] = exit "Please provide a TODO # to delete"
remove fileName [numberString] = do
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "/tmp" "temp"  
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

