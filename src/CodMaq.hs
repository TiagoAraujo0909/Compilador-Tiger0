module CodMaq where

import Lexer
import Parser
import CodInt

readInstList :: [Instr] -> String
readInstList [] = ""
readInstList (h:t) = let code1 = transCodMaq h
                         code2 = readInstList t
                     in (code1 ++ code2) 

transCodMaq :: Instr -> String
transCodMaq (MOVE s1 s2) = "\tmove $"++s1++", $" ++ s2 ++ "\n"

transCodMaq (MOVEI s1 n) = "\tli $"++s1++", "++ show n++"\n"

transCodMaq (OP op s0 s1 s2) 
  = case op of
         Add -> "\tadd $" ++ s0 ++ ", $" ++ s1 ++ ", $" ++ s2 ++ "\n"
         Minus -> "\tsub $" ++ s0 ++ ", $" ++ s1 ++ ", $" ++ s2 ++ "\n"
         Mult -> "\tmult $" ++ s0 ++ ", $" ++ s1 ++ ", $" ++ s2 ++ "\n"
         Div -> "\tdiv $" ++ s1 ++ ", $" ++ s2 ++ "\n" ++ "\tmflo " ++ "$" ++ s0 ++ "\n"
         Mod ->"\tdiv $" ++ s1 ++ ", $" ++ s2 ++ "\n" ++ "\tmfhi " ++ "$" ++ s0 ++ "\n"

transCodMaq (OPI op s0 s1 x) 
  = case op of
         Add -> "\taddi $" ++ s0 ++ ", $" ++ s1 ++ "," ++ show x ++ "\n"
         Minus -> "\tsubi $" ++ s0 ++ ", $" ++ s1 ++ "," ++ show x ++ "\n"
         Mult -> "\tmulti $" ++ s0 ++ ", $" ++ s1 ++ "," ++ show x ++ "\n"
         Div -> "\tdivi $" ++ s1 ++ ", $" ++ show x ++ "\n" ++ "\tmflo " ++ "$" ++ s0 ++ "\n"
         Mod ->"\tdivi $" ++ s1 ++ ", $" ++ show x ++ "\n" ++ "\tmfhi " ++ "$" ++ s0 ++ "\n"

transCodMaq (OPC op s0 s1 c) 
  = case op of
         Equal -> "\tseqi $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"
         Lesserthan -> "\tslti $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"
         Lessereq -> "\tslei $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"
         Greaterthan -> "\tsgti $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"
         Greatereq -> "\tsgei $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"
         Noteq -> "\tsnei $" ++ s0 ++ " $" ++ s1 ++ " $" ++ show c ++ "\n"

transCodMaq (COND c1 op c2 labelt labelf) 
  = case op of
         Equal -> "\tbne $" ++ c1 ++ ", $" ++ c2 ++ " ," ++ labelf ++ "\n\tj " ++ labelt ++ "\n"
         Lesserthan -> "\tblt $" ++ c1 ++ ", $" ++ c2 ++" ," ++ labelt ++"\n\tj " ++ labelf ++"\n"
         Lessereq -> "\tslt $s0" ++ ", $" ++ c1 ++ ", $" ++ c2 ++ "\n" ++ "\tbeq $s0" ++ ", $0, " ++ labelt ++"\n" ++ "\tj " ++ labelf ++ "\n"
         Greaterthan -> "\tblt $" ++ c1 ++ ", $" ++ c2 ++ " ," ++ labelf ++ "\n\tj " ++ labelt ++ "\n"
         Greatereq -> "\tslt $s0" ++ ", $" ++ c2 ++ ", $" ++ c1 ++ "\n" ++ "\tbeq $0" ++ ", $0, " ++ labelt ++"\n" ++ "\tj "++ labelf  ++ "\n"
         Noteq -> "\tbeq $" ++ c1 ++ ", $" ++ c2 ++ " ," ++ labelf ++ "\n\tj " ++ labelt ++ "\n"

transCodMaq (LABEL l) = l ++ ":\n"

transCodMaq (JUMP j) = "\tj " ++ j ++ "\n"

transCodMaq (PRINTI t) = "\tmove $v0" ++ ", $" ++ t ++ "\n\tlw $a0, 0($sp)\n\tsyscall\n\tjr $ra\n"

transCodMaq (SCANI) = "\tscani\n"

transCodMaq (RETURN r) = "\tmove $v0, $" ++ r ++ "\n"
--transCodMaq (PRINT p)
 -- case p of
   --     Int -> "\tli $v0, 1\n\tla $a0, " ++ show p ++ "\n\tsyscall\n"
        --String -> "\tli $v0, 4\n\tla $a0, " ++ p ++ "\n\tsyscall\n"