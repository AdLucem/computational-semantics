type Name     = String
type Index    = [Int]

data Variable = Variable Name Index deriving (Eq,Ord)

instance Show Variable where
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
    where showInts []     = ""
          showInts [i]    = show i
          showInts (i:is) = show i ++ "_" ++ showInts is

data Formula a =  Atom String [a]
                | Eq a a
                | Neg  (Formula a)
                | Impl (Formula a) (Formula a)
                | Equi (Formula a) (Formula a)
                | Conj [Formula a]
                | Disj [Formula a]
                | Forall Variable (Formula a)
                | Exists Variable (Formula a)
                deriving Eq

instance Show a => Show (Formula a) where
  show (Atom str []) = str
  show (Atom str ls) = str ++ show ls
  show (Eq t1 t2) = " ( " ++ (show t1) ++ " == " ++ (show t2) ++ " ) "
  show (Neg form) = " ( ~ " ++ (show form) ++ " ) "
  show (Impl form1 form2) = " ( " ++ (show form1) ++ " ==> " ++ (show form2) ++ " ) "
  show (Conj []) = "true"
  show (Conj ls) = " conj " ++ (show ls)
  show (Disj []) = "false"
  show (Disj ls) = " disj " ++ (show ls)
  show (Forall v form) = " A " ++ (show v) ++ (show form) 
  show (Exists v form) = " E " ++ (show v) ++ (show form)

data Term = Var Variable | Struct String [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Var v)       = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

--toAST :: Ord a => [[a]] -> Formula a 
toAST ls = case (head ls) of
  "=="   -> Eq (toAST $ (head . tail) ls)
              (toAST $ last ls)
  "~"    -> Neg (toAST $ last ls)
  "==>"  -> Impl (toAST $ (head . tail) ls)
                (toAST $ last ls)
  "<=>"  -> Equi (toAST $ (head . tail) ls)
                (toAST $ last ls)
  "conj" -> Conj (map toAST $ tail ls)
  "disj" -> Disj (map toAST $ tail ls)
  "A"    -> Forall (Variable ((head . tail) ls) [])
                  (toAST $ last ls)
  "E"    -> Exists (Variable ((head . tail) ls) [])
                  (toAST $ last ls)
  otherwise -> Atom ((head . tail) ls) (tail ls)

-- list of operators in predicate logic
opList :: [String]
opList = ["conj","disj","~","==","==>","<=>","A","E"]


isOperator, isOperand, isLeftParenthesis, isRightParenthesis :: String -> Bool

-- | is element an operator
isOperator s = s `elem` opList

-- | is element an operand
isOperand s = s `notElem` (opList ++ ["(",")"])

-- | is element left bracket
isLeftParenthesis s = s `elem` ["("]

-- | is element right bracket
isRightParenthesis s = s `elem` [")"]


-- function to determine operator precedence
operatorPrecedence :: String -> Int
operatorPrecedence "~" = 4
operatorPrecedence "conj" = 3
operatorPrecedence "disj" = 2
operatorPrecedence "==>" = 1
operatorPrecedence "<=>" = 1
operatorPrecedence "==" = 1


-- | function that using the shunting yard algorithm to convert from infix to postfix notation
-- | input, operator list, output list, return list 
infToPost :: [String] -> [String] -> [String] -> [String]

-- when all lists but output empty, reverse output
infToPost [] [] outQueue = reverse outQueue

-- when no more input tokens but still has operator tokens, add them to the output queue
infToPost [] (op:ops) outQueue = infToPost [] ops (op:outQueue)

-- | parsing input tokens
infToPost (token:tokens) opStack outQueue

    -- if the token is an operand then put it on the output queue
    | isOperand token = infToPost tokens opStack (token:outQueue)

    -- if the token is an operator
    | isOperator token = case opStack of

        -- if operator stack is empty then add the operator to the stack
        [] -> infToPost tokens (token:opStack) outQueue

        -- | if the operator stack isn't empty compare precedence with operator on top of opStack
        -- note: all operators are right-associative
        (op2:ops) -> if ((operatorPrecedence token) < (operatorPrecedence op2))
                     then infToPost (token:tokens) ops (op2:outQueue)
                     else infToPost tokens (token:opStack) outQueue

    -- if token is left bracket  then push onto operator stack
    | isLeftParenthesis token = infToPost tokens (token:opStack) outQueue

    -- if token is right bracket then check operator stack 
    | isRightParenthesis token = case opStack of
        -- if top of operator stack is left bracket then just pop
        ("(":ops) ->  infToPost tokens ops outQueue
        -- pop from operator stack and push into output queue till reach left bracket
        (op:ops) -> infToPost (token:tokens) ops (op:outQueue)

-- function to convert a string
-- to a predicate logical formula
predPreprocess :: String -> [String]
predPreprocess exp = infToPost (words exp) [] []
