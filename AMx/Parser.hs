{-# OPTIONS_GHC -w #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module AMx.Parser(runParser) where
import Prelude hiding (LT,GT)
import Control.Monad.Trans.Error(runErrorT)
import Data.Char(toUpper)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import AMx.Language
import AMx.Lexer (Token(..), runLexer)
import AMx.ParserMonad(ParserError(..), ParserMonad, Reason(..), getNextToken, throwParserError)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (16) = happyShift action_5
action_2 (17) = happyShift action_6
action_2 (18) = happyAccept
action_2 (5) = happyGoto action_3
action_2 (6) = happyGoto action_4
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (14) = happyShift action_15
action_4 (10) = happyGoto action_14
action_4 _ = happyFail

action_5 (13) = happyShift action_13
action_5 _ = happyFail

action_6 (11) = happyShift action_10
action_6 (16) = happyShift action_11
action_6 (17) = happyShift action_12
action_6 (7) = happyGoto action_7
action_6 (8) = happyGoto action_8
action_6 (9) = happyGoto action_9
action_6 _ = happyReduce_8

action_7 _ = happyReduce_5

action_8 _ = happyReduce_7

action_9 (15) = happyShift action_19
action_9 _ = happyReduce_9

action_10 (16) = happyShift action_11
action_10 (17) = happyShift action_12
action_10 (8) = happyGoto action_18
action_10 (9) = happyGoto action_9
action_10 _ = happyReduce_8

action_11 _ = happyReduce_11

action_12 _ = happyReduce_12

action_13 (17) = happyShift action_6
action_13 (6) = happyGoto action_17
action_13 _ = happyFail

action_14 (14) = happyShift action_16
action_14 _ = happyReduce_4

action_15 _ = happyReduce_13

action_16 _ = happyReduce_14

action_17 (14) = happyShift action_15
action_17 (10) = happyGoto action_22
action_17 _ = happyFail

action_18 (12) = happyShift action_21
action_18 _ = happyFail

action_19 (16) = happyShift action_11
action_19 (17) = happyShift action_12
action_19 (9) = happyGoto action_20
action_19 _ = happyFail

action_20 _ = happyReduce_10

action_21 _ = happyReduce_6

action_22 (14) = happyShift action_16
action_22 _ = happyReduce_3

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyMonadReduce 2 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( getSpecification happy_var_1 >>= happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 (checkNullaryInstruction
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (checkUnaryInstruction happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (checkBinaryInstruction happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn9
		 (IntArgument happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 (StringArgument happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 (()
	)

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 _
	_
	 =  HappyAbsSyn10
		 (()
	)

happyNewToken action sts stk
	= getNextToken(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 18 18 tk (HappyState action) sts stk;
	TokenOpenParenthesis -> cont 11;
	TokenCloseParenthesis -> cont 12;
	TokenColon -> cont 13;
	TokenSeparator -> cont 14;
	TokenComma -> cont 15;
	TokenInt happy_dollar_dollar -> cont 16;
	TokenName happy_dollar_dollar -> cont 17;
	_ -> happyError' tk
	})

happyError_ 18 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParserMonad a -> (a -> ParserMonad b) -> ParserMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParserMonad a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParserMonad a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> ParserMonad a
happyError' tk = parseError tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError tokens = throwParserError $ OtherError ("Parsing failed: " ++ show tokens)

runParser :: String -> Either ParserError [Instruction]
runParser s = case runLexer s (runErrorT parse) of
    Left  msg -> Left $ ParserError Nothing (LexerError msg)
    Right a   -> a

data InstructionSpecification where
    Nullary :: String -> Instruction -> InstructionSpecification
    Unary :: forall a . String -> (a -> Instruction) -> (Type a) -> InstructionSpecification
    Binary :: forall a b . String -> (a -> b -> Instruction) -> (Type a) -> (Type b) -> InstructionSpecification

data Argument = IntArgument Int | StringArgument String

data Type a where
    IntType :: Type Int
    StringType :: Type String

fromList :: [InstructionSpecification] -> Map String InstructionSpecification
fromList = Map.fromList . map toTuple
    where toTuple spec@(Nullary name _    ) = (name, spec)
          toTuple spec@(Unary   name _ _  ) = (name, spec)
          toTuple spec@(Binary  name _ _ _) = (name, spec)

instructions :: Map String InstructionSpecification
instructions = Map.fromList [ ("ADD", Nullary "ADD" ADD)
                            , ("SUB", Nullary "SUB" SUB)
                            , ("MUL", Nullary "MUL" MUL)
                            , ("DIV", Nullary "DIV" DIV)
                            , ("MOD", Nullary "MOD" MOD)
                            , ("GE", Nullary "GE" GE)
                            , ("GT", Nullary "GT" GT)
                            , ("LE", Nullary "LE" LE)
                            , ("LT", Nullary "LT" LT)
                            , ("READ", Unary "READ" READ IntType)
                            , ("WRITE", Unary "WRITE" WRITE IntType)
                            , ("LOAD", Unary "LOAD" LOAD IntType)
                            , ("STORE", Unary "STORE" STORE IntType)
                            , ("LIT", Unary "LIT" LIT IntType)
                            , ("JMP", Unary "JMP" JMP IntType)
                            , ("JMC", Unary "JMC" JMC IntType)
                            ]

getSpecification :: String -> ParserMonad InstructionSpecification
getSpecification name = case Map.lookup upper_name instructions of
        Nothing   -> throwParserError $ UnknownInstruction upper_name
        Just spec -> return spec
    where upper_name = map toUpper name

checkNullaryInstruction :: InstructionSpecification -> ParserMonad Instruction
checkNullaryInstruction (Nullary _name f    ) = return f
checkNullaryInstruction (Unary    name _ _  ) = throwParserError $ WrongOperandCount name 1 0
checkNullaryInstruction (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 0

checkUnaryInstruction :: Argument -> InstructionSpecification -> ParserMonad Instruction
checkUnaryInstruction _   (Nullary  name _    ) = throwParserError $ WrongOperandCount name 0 1
checkUnaryInstruction arg (Unary   _name f t  ) = checkType arg t >>= \v -> return $ f v
checkUnaryInstruction _   (Binary   name _ _ _) = throwParserError $ WrongOperandCount name 2 1

checkBinaryInstruction :: Argument -> Argument -> InstructionSpecification -> ParserMonad Instruction
checkBinaryInstruction _    _    (Nullary  name _            ) = throwParserError $ WrongOperandCount name 0 2
checkBinaryInstruction _    _    (Unary    name _ _          ) = throwParserError $ WrongOperandCount name 1 2
checkBinaryInstruction arg1 arg2 (Binary  _name f type1 type2) = do
    value1 <- checkType arg1 type1
    value2 <- checkType arg2 type2
    return $ f value1 value2

checkType :: Argument -> Type a -> ParserMonad a
checkType (IntArgument i)    IntType    = return i
checkType (StringArgument s) StringType = return s
checkType _                  _          = throwParserError $ OtherError "Wrong type."
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-7.10.1/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
