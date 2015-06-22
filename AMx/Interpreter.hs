module AMx.Interpreter where
import Prelude hiding (EQ,LT,GT)
import Numeric(readDec)
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import qualified AMx.AM0 as AM0
import AMx.Parser(runParser)

main = getArgs >>= handleArgs
usage = putStrLn "Usage: am0 [--am0 | --am1] FILE [input..]" >> exit
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

handleArgs []         = usage
handleArgs ["-h"]     = usage
handleArgs ["--help"] = usage
handleArgs (f:inp)    = runFile f inp


runFile :: FilePath -> [String] -> IO ()
runFile fp inp = do
    s <- readFile fp
    inps <- readInputs inp
    case runParser s AM0.instructionSpecifications of
        Left  e  -> putStrLn $ show e
        Right is -> AM0.run is inps

readInputs = mapM readInput

readInput :: String -> IO Int
readInput s = case readDec s of
    [(n, "")] -> return n
    _         -> putStrLn ("Illegal input number: " ++ s) >> die
