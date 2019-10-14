import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catchIOError` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           let n = show . length . lines $ contents
           putStrLn $ "The file has " ++ n ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e
    | isDoesNotExistError e = 
        let fileName = ioeGetFileName e
        in case fileName of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                            Nothing -> ioError e
    | otherwise = ioError e