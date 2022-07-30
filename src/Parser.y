{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

num      { TOKEN_NUM $$ }
id       { TOKEN_ID $$ }
string   { TOKEN_STRING $$ }

'('      { TOKEN_LPAREN }
')'      { TOKEN_RPAREN }
'['      { TOKEN_LBRACK }
']'      { TOKEN_RBRACK }

'+'      { TOKEN_PLUS }
'-'      { TOKEN_MINUS }
'*'      { TOKEN_MULT }
'/'      { TOKEN_DIV }
'%'      { TOKEN_MOD }

'='      { TOKEN_EQUAL }
'>'      { TOKEN_GREATERTHAN }
'<'      { TOKEN_LESSERTHAN }
'>='     { TOKEN_GREATEREQ }
'<='     { TOKEN_LESSEREQ }
'<>'     { TOKEN_NOTEQ }
'&'      { TOKEN_CONJ }
'|'      { TOKEN_DISJ }
':='     { TOKEN_ATTRIBUTION }
','      { TOKEN_COMMA }
';'      { TOKEN_SEMICOLON }
':'      { TOKEN_COLON }

break    { TOKEN_BREAK }
do       { TOKEN_DO }
else     { TOKEN_ELSE }
end      { TOKEN_END }
for      { TOKEN_FOR }
function { TOKEN_FUNCTION }
if       { TOKEN_IF }
in       { TOKEN_IN }
let      { TOKEN_LET }
of       { TOKEN_OF }
then     { TOKEN_THEN }
var      { TOKEN_VAR }
while    { TOKEN_WHILE }
to       { TOKEN_TO }

scani    { TOKEN_SCANI }
printi   { TOKEN_PRINTI }
print { TOKEN_PRINTSTR }

typeint    { TOKEN_TYPEINT }
typestring { TOKEN_TYPESTRING }

%right else do then
%nonassoc  '<>' '<' '<=' '>' '>=' '='
%left ':='
%left '-' '+'
%left '%' '/' '*'

%%

Program : let DecList in ExpSeq {LetIn $2 $4}

Exp : num              { Num $1 }
     | string          { Str $1 }
     --operacoes
     | Exp '+' Exp     { Op Add $1 $3 }
     | Exp '-' Exp     { Op Minus $1 $3 }
     | Exp '*' Exp     { Op Mult $1 $3 }
     | Exp '/' Exp     { Op Div $1 $3 }
     | Exp '%' Exp     { Op Mod $1 $3 }
     | '-' Exp         { Negative $2 }
     --comparacoes
     | Exp '=' Exp     { OpC Equal $1 $3 }
     | Exp '<>' Exp    { OpC Noteq $1 $3 }
     | Exp '<' Exp     { OpC Lesserthan $1 $3 }
     | Exp '<=' Exp    { OpC Lessereq $1 $3 }
     | Exp '>' Exp     { OpC Greaterthan $1 $3 }
     | Exp '>=' Exp    { OpC Greatereq $1 $3 }
     --atribuicao
     | Lvalue          { $1 }
     | Lvalue ':=' Exp { Attribution $1 $3}
     --funcoes
     | id '(' ExpList ')'             { CallFunc (Id $1) $3 } 
     | '(' ExpSeq ')'                 { SeqExp $2}
     | if Exp then Exp                { Ifthen $2 $4 }
     | if Exp then Exp else Exp       { Ifthenelse $2 $4 $6 }
     | while Exp do Exp               { While $2 $4 }
     | let VarDeclList in ExpSeq end  { Letinend $2 $4}
     | printi'('Exp')'                { Printi $3 }
     | print'(' Exp ')' ';'           { PrintStr $3 }
     | scani'('')'                    { Scani }

Lvalue : id {Id $1}

ExpSeq : {- empty -}     { [] }
        | Exp            { [$1] }
        | ExpSeq ';' Exp { $1 ++ [$3] }

ExpList : {- empty -}      { [] }
         | Exp             { [$1] }
         | ExpList ',' Exp { $1 ++ [$3]}

VarDeclList : VarDecl             { [$1] }
            | VarDeclList VarDecl {$1 ++ [$2]}
VarDecl : var id ':=' Exp { VarValue (Id $2) $4 }

DecList : Decl         { $1 }
        | DecList Decl { $1 ++ $2 }

Decl : VarDecl { [$1] }
     | FunDecl { [$1] }

FunDecl : function id '(' TypeFields ')' '=' Exp                { DecFun (Id $2) $4 $7 }
        | function id '(' TypeFields ')' ':' TypeId '=' Exp     { DecFunType (Id $2) $4 $7 $9 }

TypeFields : TypeField                { [$1] }
           | TypeFields ',' TypeField { $1 ++ [$3] }

TypeField : id ':' TypeId {TypeDecl (Id $1) $3}

TypeId : typeint    {Typeint}
       | typestring {Typestring}

{
data Program = LetIn [Decl] [Exp]
             deriving Show

data Exp = Num Int
         | Str String
         | Id String
         | Negative Exp
         --operacoes
         | Op BinOp Exp Exp
         | OpC RelOp Exp Exp
         --funcoes
         | Attribution Exp Exp
         | CallFunc Exp [Exp]
         | SeqExp [Exp]
         | Ifthen Exp Exp
         | Ifthenelse Exp Exp Exp
         | While Exp Exp
         | Letinend [Decl] [Exp]
         | Printi Exp
         | PrintStr Exp
         | Scani
         deriving Show

data Decl = VarValue Exp Exp 
         | DecFun Exp [TypeDecl] Exp
         | DecFunType Exp [TypeDecl] Type Exp
		 deriving Show

data BinOp = Add
           | Minus
           | Mult
           | Div
           | Mod
           deriving Show

data RelOp = Equal
           | Lesserthan
           | Lessereq
           | Greaterthan
           | Greatereq
           | Noteq
           deriving Show

data Type = Typeint
          | Typestring
          deriving Show

data TypeDecl = TypeDecl Exp Type
              deriving Show
parseError :: [Token] -> a
parseError toks = error "parse error"
} 