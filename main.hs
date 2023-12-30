import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Data.Char
import Data.List
-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Data structures -- 
-- Stack --
data Stack = Stk [String]
    deriving Show

push :: String -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs

top :: Stack -> String
top (Stk (x:_)) = x

emptyStk :: Stack
emptyStk = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

-- State --
data State = State [(String,String)]
    deriving Show

emptyState :: Main.State
emptyState = Main.State []

addState :: Main.State -> (String, String) -> Main.State
addState (Main.State state) (key, value) =
  Main.State $ sort $ (key, value) : filter (\(k, _) -> k /= key) state

isEmptyState :: Main.State -> Bool
isEmptyState (Main.State [])= True
isEmptyState (Main.State _) = False

-- TO DO functions --
createEmptyStack :: Stack
createEmptyStack = emptyStk

checkStr :: String -> String
checkStr s = if s == "ff" then "False" else if s == "tt" then "True" else s

stack2Str :: Stack -> String 
stack2Str stk = if isEmpty stk then "" else if isEmpty (pop stk) then checkStr (top stk) else checkStr (top stk) ++ "," ++ stack2Str (pop stk)

createEmptyState :: Main.State
createEmptyState = emptyState

state2Str :: Main.State -> String
state2Str (Main.State pairs) =
  if null pairs
    then ""
    else init (concatMap pairToStr pairs)
  where
    pairToStr (key, value) = key ++ "=" ++ checkStr value ++ "," 

add :: Stack -> Stack
add stack =
    case stack of 
      Stk (x:y:xs) -> push (show (read x + read y)) (pop(pop stack))
      _            -> error "Not enough elements on the stack for this operation."

mul :: Stack -> Stack
mul stack =
    case stack of
      Stk (x:y:xs) -> push (show (read x * read y)) (pop(pop stack))
      _            -> error "Not enough elements on the stack for this operation."

equ :: Code
equ = [Equ]

le :: Code
le = [Le]

getStatePairs :: Main.State -> [(String, String)]
getStatePairs (Main.State pairs) = pairs

removeQuotes :: String -> String
removeQuotes str = filter (/= '\"') str

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, Main.State) -> (Code, Stack, Main.State)
run ([], stack, state) = ([], stack, state)  -- If the code is empty, return the current state
run (inst:code, stack, state) =
  case inst of
    Push n -> run (code, push (show n) stack, state)
    Tru -> run (code, push "tt" stack, state)
    Fals -> run (code, push "ff" stack, state)
    Add ->
      case stack of
        Stk (x:y:xs) ->
          case (reads x, reads y) of
            ([(xVal, "")], [(yVal, "")]) ->
              run (code, push (show (xVal + yVal)) (pop (pop stack)), state)
            _ -> error "Invalid operands for addition: at least one of the values isn't an integer."
        _ -> error "Not enough elements on the stack for this operation."
    Mult ->
      case stack of
        Stk (x:y:xs) ->
          case (reads x, reads y) of
            ([(xVal, "")], [(yVal, "")]) ->
              run (code, push (show (xVal * yVal)) (pop (pop stack)), state)
            _ -> error "Invalid operands for multiplication: at least one of the values isn't an integer."
        _ -> error "Not enough elements on the stack for this operation."
    Sub ->
      case stack of
        Stk (x:y:xs) ->
          case (reads x, reads y) of
            ([(xVal, "")], [(yVal, "")]) ->
              run (code, push (show (xVal - yVal)) (pop (pop stack)), state)
            _ -> error "Invalid operands for subtraction: at least one of the values isn't an integer."
        _ -> error "Not enough elements on the stack for this operation."
    Equ ->
      case stack of
        Stk (x:y:xs) -> run (code, push (if x == y then "tt" else "ff") (pop (pop stack)), state)
        _ -> error "Not enough elements on the stack for this operation."
    Le ->
      case stack of
        Stk (x:y:xs) ->
          case (reads x, reads y) of
            ([(xVal, "")], [(yVal, "")]) ->
              run (code, push (if (xVal :: Integer) <= (yVal :: Integer) then "tt" else "ff") (pop (pop stack)), state)
            _ -> error "Invalid operands for less than or equal to comparison: at least one of the values isn't an integer."
        _ -> error "Not enough elements on the stack for this operation."
    Fetch x ->
      case lookup x (getStatePairs state) of
        Just value -> run (code, push value stack, state)
        Nothing    -> error $ "Run-time error"
    Store x ->
      case stack of
        Stk (value:rest) -> run (code, pop stack, addState state (x, removeQuotes value))
        _                -> error "Not enough elements on the stack for store operation."
    Branch c1 c2 ->
      case stack of
        Stk ("tt":xs) -> run (c1 ++ code, pop stack, state)
        Stk ("ff":xs) -> run (c2 ++ code, pop stack, state)
        _             -> error "Invalid operand for branch: top of stack must be tt or ff."
    Neg ->
      case stack of
        Stk(x:xs) ->
          case x of
            "tt" -> run(code, push "ff" (pop stack), state)
            "ff" -> run(code, push "tt" (pop stack), state)
            _    -> error "Run-time error"
        _ -> error "Not enough elements on the stack for this operation."
    And ->
      case stack of
        Stk (x:y:xs) ->
          if isBoolean x && isBoolean y
            then run(code, push(logicalAnd x y) (pop(pop stack)), state)
            else error $ "Run-time error"
        _ -> error $ "Run-time error"
    Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state) ---- Loop c1 c2 -> run (c1 ++ [Branch c2 (Noop : Loop c1 c2 : [])] ++ code, stack, state)
    Noop -> run (code, stack, state)
    -- Add other cases for the remaining instructions


checkTopTwo :: Stack -> Maybe (Integer, Integer)
checkTopTwo (Stk (x:y:xs)) =
  case (reads x, reads y) of
    ([(x', "")], [(y', "")]) -> Just (x', y')
    _ -> Nothing
checkTopTwo _ = Nothing

isBoolean :: String -> Bool
isBoolean "tt" = True
isBoolean "ff" = True
isBoolean _    = False

logicalAnd :: String -> String -> String
logicalAnd "tt" "tt" = "tt"
logicalAnd _    _    = "ff"

--isNestedList :: (Eq a) => [a] -> Bool
--isNestedList [] = True
--isNestedList (x:xs) = isList x && isNestedList xs
--  where
--    isList :: (Eq a) => a -> Bool
--    isList [] = True
--    isList (_:_) = True
--    isList _ = False

--result2 = isNestedList [1, 2, [3, "hello"]]

-- Example Code: Push 2, Push 3, Add, Mult
exampleCode :: Code
exampleCode = [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"]

-- Initial empty Stack and State
initialStack :: Stack
initialStack = createEmptyStack

initialState :: Main.State
initialState = Main.State []

-- Test the run function
result :: (Code, Stack, Main.State)
result = run (exampleCode, initialStack, initialState)

-- Print the result
main :: IO ()
main = print result

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
test = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
test2 = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
test3 = testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
test4 = testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
test5 = testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
test6 = testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
test7 = testAssembler [Push (-20),Push (-21), Le] == ("True","")
test8 = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
test9 = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
--If you test:
test11 = testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
test12 = testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp
  = ALit Integer
  | AVar String 
  | AAdd Aexp Aexp 
  | ASub Aexp Aexp 
  | AMul Aexp Aexp
  deriving Show

data Bexp
  = BLit Bool  
  | BNot Bexp 
  | BAnd Bexp Bexp
  | BEq Aexp Aexp
  | BBEq Bexp Bexp
  | BLe Aexp Aexp 
  deriving Show

data Stm
  = Assign String Aexp
  | If Bexp Program Program 
  | While Bexp Program
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (ALit s) = [Push s]
compA (AVar var) = [Fetch var]
compA (AAdd e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (ASub e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (AMul e1 e2) = compA e2 ++ compA e1 ++ [Mult]

compB :: Bexp -> Code
compB (BLit True) = [Tru]
compB (BLit False) = [Fals]
compB (BNot e) = compB e ++ [Neg]
compB (BAnd e1 e2) = compB e1 ++ compB e2 ++ [And]
compB (BBEq e1 e2) = compB e1 ++ compB e2 ++ [Equ]
compB (BEq e1 e2) = compA e1 ++ compA e2 ++ [Equ]
compB (BLe e1 e2) = compA e1 ++ compA e2 ++ [Le]

comp :: Stm -> Code
comp (Assign s e) = compA e ++ [Store s]
comp (If e p1 p2) = compB e ++ [Branch (compile p1) (compile p2)]
comp (While e p1) = [Loop (compB e)  (compile p1)]

compile :: Program -> Code
compile [] = []
compile (x:xs) = comp x ++ compile xs

identifier :: Parser String
identifier = many1 letter

integer :: Parser Integer
integer = read <$> many1 digit

aexpParser :: Parser Aexp
aexpParser = try aexpParenParser <|> aexpOpParser <|> aexpTermParser

aexpTermParser :: Parser Aexp
aexpTermParser = ALit <$> integer <|> AVar <$> identifier

aexpParenParser :: Parser Aexp
aexpParenParser = char '(' *> aexpParser <* char ')'

aexpOpParser :: Parser Aexp
aexpOpParser = buildExpressionParser aexptable aexpOpTermParser

aexpOpTermParser :: Parser Aexp
aexpOpTermParser = try aexpParenParser <|> aexpTermParser

aexptable =
  [ [binary "*" AMul AssocLeft]
  , [binary "+" AAdd AssocLeft, binary "-" ASub AssocLeft]
  ]

binary name fun assoc = Infix (reservedOp name >> return fun) assoc

reservedOp :: String -> Parser String
reservedOp op = do
  try (spaces >> string op)
  spaces
  return op

bexpParser :: Parser Bexp
bexpParser = try bexpLitParser

bexpLitParser :: Parser Bexp
bexpLitParser = BLit <$> (trueLit <|> falseLit)
  where
    trueLit = string "true" >> return True
    falseLit = string "false" >> return False

stmParser :: Parser Stm
stmParser = try assignParser <|> ifParser <|> whileParser 

assignParser :: Parser Stm
assignParser = do
  spaces
  var <- identifier
  spaces
  string ":="
  spaces
  expr <- aexpParser
  spaces
  char ';'
  return (Assign var expr)

ifParser :: Parser Stm
ifParser = do
  spaces
  string "if"
  spaces
  condition <- bexpParser
  spaces
  string "then"
  spaces
  trueBranch <- try programParserInParens <|> try programParser
  spaces
  string "else"
  spaces
  falseBranch <- try programParserInParens <|> programParser
  spaces
  optional (char ';')
  return (If condition trueBranch falseBranch)

whileParser :: Parser Stm
whileParser = do
  spaces
  string "while"
  spaces
  condition <- bexpParser
  spaces
  string "do"
  spaces
  body <- programParserInParens <|> programParser
  spaces
  char ';'
  return (While condition body)

programParserInParens :: Parser Program
programParserInParens = char '(' *> programParser <* char ')'

programParser :: Parser Program
programParser = many stmParser

parseProgram :: String -> Program
parseProgram codeString = case Text.Parsec.parse programParser "" codeString of
    Left err -> error $ "Parse error: " ++ show err
    Right program -> program

parse :: String -> Program
parse codeString = parseProgram codeString

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (Main.parse programCode), createEmptyStack, createEmptyState)

-- Examples:
tp1 = testParser "x := 5; x := x - 1;" == ("","x=4")
tp2 = testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")