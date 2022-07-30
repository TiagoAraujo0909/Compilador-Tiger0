{
module Lexer where
}

%wrapper "basic"

$white  =  [\ \t\n\r]
$digit  =  [0-9]
$letter = [_a-zA-Z]

tokens :-

$white+   ;
"/*"([^\*] | "*"[^\/] | [\n])* "*/" ;

","       { \_ -> TOKEN_COMMA }
";"       { \_ -> TOKEN_SEMICOLON }
":"       { \_ -> TOKEN_COLON }
"["       { \_ -> TOKEN_LBRACK }
"]"       { \_ -> TOKEN_RBRACK }

--operadores
"+"       { \_ -> TOKEN_PLUS }
"-"       { \_ -> TOKEN_MINUS }
"*"       { \_ -> TOKEN_MULT }
"/"       { \_ -> TOKEN_DIV }
"%"       { \_ -> TOKEN_MOD }
"("       { \_ -> TOKEN_LPAREN }
")"       { \_ -> TOKEN_RPAREN }
"="       { \_ -> TOKEN_EQUAL }
">"       { \_ -> TOKEN_GREATERTHAN }
"<"       { \_ -> TOKEN_LESSERTHAN }
">="      { \_ -> TOKEN_GREATEREQ }
"<="      { \_ -> TOKEN_LESSEREQ }
"<>"      { \_ -> TOKEN_NOTEQ }
"&"       { \_ -> TOKEN_CONJ }
"|"       { \_ -> TOKEN_DISJ }
":="      { \_ -> TOKEN_ATTRIBUTION }

--palavras reservadas
break     { \_ -> TOKEN_BREAK }
do        { \_ -> TOKEN_DO }
end       { \_ -> TOKEN_END }
to        { \_ -> TOKEN_TO }
else      { \_ -> TOKEN_ELSE }
for       { \_ -> TOKEN_FOR }
function  { \_ -> TOKEN_FUNCTION }
if        { \_ -> TOKEN_IF }
in        { \_ -> TOKEN_IN }
let       { \_ -> TOKEN_LET }
of        { \_ -> TOKEN_OF }
then      { \_ -> TOKEN_THEN }
var       { \_ -> TOKEN_VAR }
while     { \_ -> TOKEN_WHILE }

scani     { \_ -> TOKEN_SCANI }
printi    { \_ -> TOKEN_PRINTI }
print     { \_ -> TOKEN_PRINTSTR }

int       { \_ -> TOKEN_TYPEINT }
string    { \_ -> TOKEN_TYPESTRING }

\"[^\"]*\"               { \s -> TOKEN_STRING (read s) }
$digit+                  { \s -> TOKEN_NUM (read s) }
$letter($letter|$digit)* { \s -> TOKEN_ID s }

{
data Token
  = TOKEN_ID String
  | TOKEN_STRING String  
  | TOKEN_NUM Int
  | TOKEN_TYPEINT
  | TOKEN_TYPESTRING
  | TOKEN_PLUS
  | TOKEN_TO
  | TOKEN_MINUS
  | TOKEN_MULT
  | TOKEN_DIV
  | TOKEN_MOD
  | TOKEN_LPAREN
  | TOKEN_RPAREN
  | TOKEN_EQUAL
  | TOKEN_GREATERTHAN
  | TOKEN_LESSERTHAN
  | TOKEN_GREATEREQ
  | TOKEN_LESSEREQ
  | TOKEN_NOTEQ
  | TOKEN_CONJ
  | TOKEN_DISJ
  | TOKEN_ATTRIBUTION
  | TOKEN_BREAK
  | TOKEN_DO
  | TOKEN_ELSE
  | TOKEN_END
  | TOKEN_FOR
  | TOKEN_FUNCTION
  | TOKEN_IF
  | TOKEN_IN
  | TOKEN_LET
  | TOKEN_OF
  | TOKEN_THEN
  | TOKEN_VAR
  | TOKEN_WHILE
  | TOKEN_COMMA
  | TOKEN_SEMICOLON
  | TOKEN_COLON
  | TOKEN_LBRACK
  | TOKEN_RBRACK
  | TOKEN_SCANI
  | TOKEN_PRINTI
  | TOKEN_PRINTSTR
  deriving (Show, Eq)
}