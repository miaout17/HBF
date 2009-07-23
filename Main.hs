import HBF.VM
import System.Environment


main = do
    argv <- getArgs
    case argv of
        [filename] -> execFile filename
        _ -> putStr "Usage: Main [filename]"

execFile filename = do
    code <- readFile filename
    runBF code