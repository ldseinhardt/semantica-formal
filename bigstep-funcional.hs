{- Definição da linguagem -}
data Exp = Num Int
          | TRUE
          | FALSE
          | Var String
          | Soma Exp Exp
          | Mult Exp Exp
          | And Exp Exp
          | Or Exp Exp
          | Not Exp
          | IF Exp Exp Exp
          | Ap Exp Exp
          | Fun String Tipo Exp
          | Let String Tipo Exp Exp

  deriving (Eq, Show)

data Tipo = INT
          | BOOL
          | F Tipo Tipo

  deriving (Eq, Show)

{- Big Step -}
bigStep :: Exp -> Exp
bigStep (Soma e1 e2) =
  let
    (Num n1) = bigStep e1
    (Num n2) = bigStep e2
  in Num (n1 + n2)

bigStep (Mult e1 e2) =
  let
    (Num n1) = bigStep e1
    (Num n2) = bigStep e2
  in Num (n1 * n2)

bigStep (And e1 e2) =
  let
    b = bigStep e1
  in case b of
    TRUE  -> bigStep e2
    FALSE -> FALSE

bigStep (Or e1 e2) =
  let
    b = bigStep e1
  in case b of
    TRUE  -> TRUE
    FALSE -> bigStep e2

bigStep (Not e) =
  let
    b = bigStep e
  in case b of
    TRUE  -> FALSE
    FALSE -> TRUE

bigStep (IF e e1 e2) =
  let
    b = bigStep e
  in case b of
    TRUE  -> bigStep e1
    FALSE -> bigStep e2

bigStep (Ap e1 e2) =
  let
    (Fun x t e) = bigStep e1
  in bigStep (subs x e2 e)

bigStep (Let x t e1 e2) = bigStep (subs x e1 e2)

-- final case
bigStep v = v

{- Função de substituição -}
subs :: String -> Exp -> Exp -> Exp
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs var val (Mult exp1 exp2) = Mult (subs var val exp1) (subs var val exp2)
subs var val (And exp1 exp2) = And (subs var val exp1) (subs var val exp2)
subs var val (Or exp1 exp2) = Or (subs var val exp1) (subs var val exp2)
subs var val (Not exp) = Not (subs var val exp)
subs var val (IF exp exp1 exp2) = IF (subs var val exp) (subs var val exp1) (subs var val exp2)
subs var val (Ap exp1 exp2) = Ap (subs var val exp1) (subs var val exp2)
subs var val (Fun s t exp) = Fun s t (subs var val exp)
subs var val (Let s t exp1 exp2) = Let s t (subs var val exp1) (subs var val exp2)
subs var val (Var x)
  | x == var = val
  | otherwise = Var x
-- final case
subs _ _ v = v
-- {v/x}e1 = subs x v e1

{- Casos de Teste -}
prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3

prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11

prog3 :: Exp
prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))))

-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))

prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x") 

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x 

erroTipo1 :: Exp
erroTipo1 = IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Num 3)

erroTipo2 :: Exp
erroTipo2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") TRUE))

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x True