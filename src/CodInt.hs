module CodInt where

import           Parser
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

type Table = Map Id String
type Id    = String
type Temp  = String
type Label = String

data Instr = MOVE Temp Temp            --temp1 := temp2
           | MOVEI Temp Int            --temp1 := num
           | OP BinOp Temp Temp Temp   --temp1 := temp2 op temp3
           | OPI BinOp Temp Temp Int   --temp1 := temp2 op num
           | OPC RelOp Temp Temp Temp
           | OPCI RelOp Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp RelOp Temp Label Label
           | CALL Temp Label [Temp]
           | PRINTI Temp
           | PRINTSTR Temp
           | SCANI
           | RETURN Temp
           deriving Show

transExpr :: Table -> Exp -> Id -> State Count [Instr]
transExpr table (Num n) dest = return [MOVEI dest n]

transExpr table (Id x) dest
    = case Map.lookup x table of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExpr table (Str str) dest
  = case Map.lookup str table of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "variable not found"

transExpr table (Op op e1 e2) dest
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExpr table e1 t1
         code2 <- transExpr table e2 t2
         popTemp 2
         return (code1 ++ code2 ++ [OP op dest t1 t2])

transExpr table (OpC op e1 e2) dest
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExpr table e1 t1
         code2 <- transExpr table e2 t2
         popTemp 2
         return (code1 ++ code2 ++ [OPC op dest t1 t2])

transExpr table (Attribution (Id x) e) dest
    = case Map.lookup x table of
        Nothing -> error "undefined variable"
        Just varTemp -> do t <- newTemp
                           code <- transExpr table e t
                           popTemp 1
                           return (code ++ [MOVE varTemp t])

transExpr table (CallFunc (Id x) args) dest
    = do (code, temps) <- transArgs table args
         popTemp (length args)
         return (code ++ [CALL dest x temps])

transExpr table (SeqExp es) dest = transExps table es dest

transExpr table (Ifthen cond e) dest
    = do ltrue <- newLabel
         lfalse <- newLabel
         code1 <- transCond table cond ltrue lfalse
         code2 <- transExpr table e dest
         return (code1 ++ [LABEL ltrue] ++ code2 ++ [LABEL lfalse])

transExpr table (Ifthenelse cond exp1 exp2) dest
    = do ltrue <- newLabel
         lfalse <- newLabel
         lend <- newLabel
         code1 <- transCond table cond ltrue lfalse
         code2 <- transExpr table exp1 dest
         code3 <- transExpr table exp2 dest
         return (code1 ++ [LABEL ltrue] ++ code2 ++ [JUMP lend, LABEL lfalse] ++ code3 ++ [LABEL lend])

transExpr table (While cond e) dest
    = do lbody <- newLabel
         lend <- newLabel
         lcond <- newLabel
         code1 <- transExpr table e dest
         code2 <- transCond table cond lbody lend
         return ([JUMP lcond, LABEL lbody] ++ code1 ++ [LABEL lcond] ++ code2 ++ [LABEL lend])

transExpr table (Printi e) dest
    = do temp <- newTemp
         code <- transExpr table e temp
         return (code ++ [PRINTI temp])

transExpr table (PrintStr (Str str)) dest
  = do temp <- newTemp
       popTemp(1)
       let code = [MOVE temp str]
       return (code ++ [PRINTSTR temp])

transExpr table (Scani) dest = return [SCANI]

transExpr table (Letinend decls es) dest
    = do (codeDecls,table') <- transDecls table decls
         codeExps <- transExps table' es "None"
         return (codeDecls ++ codeExps)

transExps :: Table -> [Exp] -> Id -> State Count [Instr]
transExps tabl [] dest = return []
transExps tabl (e:remainder) dest = do code1 <- transExpr tabl e dest
                                       code2 <- transExps tabl remainder dest
                                       return (code1 ++ code2)

transArgs :: Table -> [Exp] -> State Count ([Instr],[Temp])
transArgs table args  = worker args 
  where
    worker []  = return ([], [])
    worker (e:exps) 
      = do temp <- newTemp 
           code <- transExpr table e temp 
           (code', temps') <- worker exps 
           return (code++code', temp:temps')

transCond :: Table -> Exp -> Label -> Label -> State Count [Instr]
transCond table (OpC op e1 e2) ltrue lfalse
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExpr table e1 t1
         code2 <- transExpr table e2 t2
         return (code1 ++ code2 ++ [COND t1 op t2 ltrue lfalse])

transCond table e ltrue lfalse
    = do t1 <- newTemp
         t2 <- newTemp
         code1 <- transExpr table e t1
         code2 <- transExpr table (Num 0) t2
         popTemp 2
         return (code1 ++ code2 ++ [COND t1 Noteq t2 ltrue lfalse])

----------------------------------------------------------------------------------
transDecl :: Table -> Decl -> State Count ([Instr],Table)
transDecl table (VarValue (Id x) e)
    = do t <- newTemp
         code <- transExpr table e t
         table' <- newTable' table (t,x)
         return (code,table')

transDecl table (DecFun (Id x) t e)
    = do nt <- newTable Map.empty t
         code <- transExpr nt e "-"
         popTemp (length nt)
         return ([LABEL x] ++ code,table)

transDecl table (DecFunType (Id x) t _ e)
    = do nt <- newTable Map.empty t
         temp <- newTemp
         code <- transExpr nt e temp
         popTemp (length nt + 1)
         return ([LABEL x] ++ code,table)

transDecls :: Table -> [Decl] -> State Count ([Instr],Table)
transDecls table [] = return ([],table)
transDecls table (decl:remainder)
    = do (instr,table) <- transDecl table decl
         (instr'',table'') <- transDecls table remainder
         return (instr++instr'',table'')

----------------------------------------------------------------------------------
--obter codigo intermedio
execCodInt :: Program -> State Count [Instr]
execCodInt (LetIn decls e)
    = do  (codeDecls,tabl) <- transDecls Map.empty decls
          codeExprs <- transExps tabl e "None"
          return (codeDecls ++ [LABEL "main"] ++ codeExprs)

printCodInt :: Program -> [Instr]
printCodInt p = evalState (execCodInt p) (0,0)
----------------------------------------------------------------------------------
type Count = (Int,Int)  -- contadores para temporários e etiquetas

newTable :: Table -> [TypeDecl] -> State Count Table
newTable table []  = return table
newTable table ((TypeDecl (Id x) t):remainder) 
    = do t <- newTemp
         table' <- newTable' table (t,x)
         nt <- newTable table' remainder
         return nt

newTable' :: Table -> (Temp,String) -> State Count Table
newTable' table (temp,x) = return (Map.insert x temp table)

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label 
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)

---------------------------------------------------------------------------

--basic example
test1 = Op Mult (Num 3) (Op Add (Num 4) (Num 5))
test10 = LetIn [VarValue (Id "n") (Num 0)] [OpC Equal (Id "n") (Num 1)]
--sumsquares
test2 = LetIn [VarValue (Id "s") (Num 0),VarValue (Id "n") (Num 1)] [While (OpC Lessereq (Id "n") (Num 10)) (SeqExp [Attribution (Id "s") (Op Add (Id "s") (Op Mult (Id "n") (Id "n"))),Attribution (Id "n") (Op Add (Id "n") (Num 1))]),Printi (Id "n")]
--factorial
test3 = LetIn [DecFunType (Id "fact") [TypeDecl (Id "n") Typeint] Typeint (Ifthenelse (OpC Greaterthan (Id "n") (Num 0)) (Op Mult (Id "n") (CallFunc (Id "fact") [Op Minus (Id "n") (Num 1)])) (Num 1))] [Printi (CallFunc (Id "fact") [Num 10])]
--runState (transExpr Map.empty test1 "x") (0,0)
--printCodInt test2