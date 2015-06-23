module AMx.AM0 where
import Prelude hiding (EQ,LT,GT)
import Data.List(intercalate)
import Data.Map.Strict(Map)
import qualified Data.IntMap.Strict as IntMap
import AMx.Language(InstructionSpecification(..), Type(..), Program, getInstruction, fromList, programBounds)

type Address = Int
type Value = Int
type Stack = [Value]
type Memory = IntMap.IntMap Value
type Input = [Value]
type Output = [Value]

-- Possible runtime errors
data Exception = EmptyStack | EmptyInput

data Result = SuccessResult Configuration | ErrorResult Exception

-- AM0 Instructions
data Instruction = READ Address
                 | WRITE Address
                 | LIT Value
                 | LOAD Address
                 | STORE Address
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | MOD
                 | EQ
                 | LE
                 | LT
                 | GE
                 | GT
                 | NE
                 | JMP Address
                 | JMC Address
                 deriving (Show, Eq)

-- Specifications for AM0 instructions
instructionSpecifications :: Map String (InstructionSpecification Instruction)
instructionSpecifications = fromList [ Nullary "ADD" ADD
                                     , Nullary "SUB" SUB
                                     , Nullary "MUL" MUL
                                     , Nullary "DIV" DIV
                                     , Nullary "MOD" MOD
                                     , Nullary "EQ" EQ
                                     , Nullary "GE" GE
                                     , Nullary "GT" GT
                                     , Nullary "LE" LE
                                     , Nullary "LT" LT
                                     , Nullary "NE" NE
                                     , Unary "READ" READ IntType
                                     , Unary "WRITE" WRITE IntType
                                     , Unary "LOAD" LOAD IntType
                                     , Unary "STORE" STORE IntType
                                     , Unary "LIT" LIT IntType
                                     , Unary "JMP" JMP IntType
                                     , Unary "JMC" JMC IntType
                                     ]

-- Configuration state of an AM0 machine
data Configuration = Configuration { instructionPointer :: Address
                                   , stack :: Stack
                                   , memory :: Memory
                                   , input :: Input
                                   , output :: Output
                                   } deriving (Eq)

instance Show Configuration where
    show (Configuration ip stack mem inp out)
        = "(" ++ intercalate ", " [show (ip + 1), showSequence stack, showMemory mem, showSequence inp, showSequence $ reverse out] ++ ")"

showMemory :: Memory -> String
showMemory mem = "[" ++ (intercalate ", " $ map showMemoryCell $ IntMap.assocs mem) ++ "]"

showMemoryCell :: (Address, Value) -> String
showMemoryCell (a, v) = show a ++ "/" ++ show v

showSequence :: Show a => [a] -> String
showSequence [] = "ε"
showSequence as = intercalate ":" $ map show as

instance Show Exception where
    show EmptyStack = "AM0.Interpreter.Error: Empty stack"
    show EmptyInput = "AM0.Interpreter.Error: Empty input"

memoryLookup :: Address -> Memory -> Value
memoryLookup = IntMap.findWithDefault 0

memoryUpdate :: Address -> Value -> Memory -> Memory
memoryUpdate = IntMap.insert

run :: Program Instruction -> Input -> IO ()
run prog inp = interpret prog inp

interpret :: Program Instruction -> Input -> IO ()
interpret prog inp = interpretStepwise prog (Configuration 0 [] IntMap.empty inp []);

interpretStepwise :: Program Instruction -> Configuration -> IO ()
interpretStepwise prog conf = do
        putStrLn $ show conf
        if ip >= l && ip <= h
            then
                case execute (getInstruction prog ip) conf of
                    Left  next -> interpretStepwise prog next
                    Right exc  -> putStrLn $ show exc
            else return ()
    where
        (l, h) = programBounds prog
        ip     = instructionPointer conf

execute :: Instruction -> Configuration -> Either Configuration Exception
-- Load/Store instructions
execute (READ  n) (Configuration ip stack mem inp out)
    = case inp of
        []   -> Right EmptyInput
        i:is -> Left $ Configuration (ip + 1) stack (memoryUpdate n i mem) is out
execute (WRITE n) (Configuration ip stack mem inp out)
    = Left $ Configuration (ip + 1) stack mem inp (memoryLookup n mem : out)
execute (LIT   n) (Configuration ip stack mem inp out)
    = Left $ Configuration (ip + 1) (n : stack) mem inp out
execute (LOAD  n) (Configuration ip stack mem inp out)
    = Left $ Configuration (ip + 1) (memoryLookup n mem : stack) mem inp out
execute (STORE n) (Configuration ip stack mem inp out)
    = case stack of
        []   -> Right EmptyStack
        a:as -> Left $ Configuration (ip + 1) as (memoryUpdate n a mem) inp out
-- Arithmetic and logical instructions
execute ADD conf = executeStackOperation (+) conf
execute SUB conf = executeStackOperation (-) conf
execute MUL conf = executeStackOperation (*) conf
execute DIV conf = executeStackOperation div conf
execute MOD conf = executeStackOperation mod conf
execute EQ  conf = executeStackOperation ((fromEnum .) . (==)) conf
execute LE  conf = executeStackOperation ((fromEnum .) . (<=)) conf
execute LT  conf = executeStackOperation ((fromEnum .) . (<))  conf
execute GE  conf = executeStackOperation ((fromEnum .) . (>=)) conf
execute GT  conf = executeStackOperation ((fromEnum .) . (>))  conf
execute NE  conf = executeStackOperation ((fromEnum .) . (/=)) conf
-- Control flow instructions
execute (JMP n) (Configuration ip stack mem inp out)
    = Left $ Configuration (n - 1) stack mem inp out
execute (JMC n) (Configuration ip stack mem inp out)
    = case stack of
        []   -> Right EmptyStack
        0:as -> Left $ Configuration (n - 1)  as mem inp out
        _:as -> Left $ Configuration (ip + 1) as mem inp out

executeStackOperation :: (Value -> Value -> Value) -> Configuration -> Either Configuration Exception
executeStackOperation f (Configuration ip stack mem inp out)
    = case stack of
        []     -> Right EmptyStack
        _:[]   -> Right EmptyStack
        a:b:cs -> Left $ Configuration (ip + 1) (f b a : cs) mem inp out
