(* L1 Compiler
 * Lexer
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

fun eof () = Tokens.EOF (0,0)

%%
%header (functor BVLexFn(structure Tokens : BV_TOKENS));
%full

id = [A-Za-z_][A-Za-z0-9_]*;

ws = [\ \t\012];

%%

<INITIAL> {ws}+       => (lex ());
<INITIAL> \n          => (ParseState.newline(yypos); lex());

<INITIAL> "("         => (Tokens.LPAREN (yypos, yypos + size yytext));
<INITIAL> ")"         => (Tokens.RPAREN (yypos, yypos + size yytext));

<INITIAL> "lambda"    => (Tokens.LAMBDA (yypos, yypos + size yytext));

<INITIAL> "0"         => (Tokens.ZERO (yypos, yypos + size yytext));
<INITIAL> "1"         => (Tokens.ONE (yypos, yypos + size yytext));

<INITIAL> "if0"       => (Tokens.IF0 (yypos, yypos + size yytext));
<INITIAL> "fold"      => (Tokens.FOLD (yypos, yypos + size yytext));

<INITIAL> "not"       => (Tokens.NOT (yypos, yypos + size yytext));
<INITIAL> "shl1"      => (Tokens.SHL1 (yypos, yypos + size yytext));
<INITIAL> "shr1"      => (Tokens.SHR1 (yypos, yypos + size yytext));
<INITIAL> "shr4"      => (Tokens.SHR4 (yypos, yypos + size yytext));
<INITIAL> "shr16"     => (Tokens.SHR16 (yypos, yypos + size yytext));

<INITIAL> "and"       => (Tokens.AND (yypos, yypos + size yytext));
<INITIAL> "or"        => (Tokens.OR (yypos, yypos + size yytext));
<INITIAL> "xor"       => (Tokens.XOR (yypos, yypos + size yytext));
<INITIAL> "plus"      => (Tokens.PLUS (yypos, yypos + size yytext));

<INITIAL> {id}        => (Tokens.IDENT (yytext, yypos, yypos + size yytext));

<INITIAL> .           => (ErrorMsg.error (ParseState.ext (yypos,yypos))
                              ("illegal character: \"" ^ yytext ^ "\"");
                          lex ());
