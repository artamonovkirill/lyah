import System.Random  
import Control.Monad (when)

main = do  
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do   
    let (n, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of?" 
    answer <- getLine
    when (not . null $ answer) $ do
        let guess = read answer
        if guess == n
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show n  
        askForNumber newGen
