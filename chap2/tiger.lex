(* User declarations *)

type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* global variables *)
val comment_nesting_level : int ref = ref 0
val curr_str : string option ref = ref NONE

fun eof() =
  if !comment_nesting_level > 0
  then raise Fail "Detected unclosed comment"
  else if (Option.isSome (!curr_str))
  then raise Fail "Detected unclosed string literal"
  else let val pos = hd(!linePos)
       in Tokens.EOF(pos,pos)
       end

fun nest_comment() =
  comment_nesting_level := !comment_nesting_level + 1

fun unnest_comment() =
  comment_nesting_level := !comment_nesting_level - 1

fun append c =
  case !curr_str of
      NONE => curr_str := SOME(c)
    | SOME(str) => curr_str := SOME(str ^ c)

%%
%s COMMENT, STRING;
id = [a-zA-Z][a-zA-Z0-9_]*;
intlit = [0-9]+;
%%

<COMMENT>.    => (continue());
<COMMENT>"*/" => (if (!comment_nesting_level = 0)
                  then (YYBEGIN INITIAL; continue())
                  else (unnest_comment(); continue()));
<COMMENT>"/*" => (nest_comment(); continue());

<STRING>"\"" => (YYBEGIN INITIAL;
		 let val str = Option.valOf (!curr_str) (* blows up if no str *)
		 in (curr_str := NONE;
		    Tokens.STRING(str,yypos-(String.size str),yypos))
		 end);
<STRING>.    => (append(yytext); continue());

<INITIAL>[\t\ ]+  => (continue());
<INITIAL>"/*"     => (YYBEGIN COMMENT; continue());
<INITIAL>type     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>var      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>break    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>of       => (Tokens.OF(yypos,yypos+2));
<INITIAL>end      => (Tokens.END(yypos,yypos+3));
<INITIAL>in       => (Tokens.IN(yypos,yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>let      => (Tokens.LET(yypos,yypos+3));
<INITIAL>do       => (Tokens.DO(yypos,yypos+2));
<INITIAL>to       => (Tokens.TO(yypos,yypos+2));
<INITIAL>for      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>while    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>else     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>then     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>if       => (Tokens.IF(yypos,yypos+2));
<INITIAL>array    => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>":="     => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>or       => (Tokens.OR(yypos,yypos+2));
<INITIAL>and      => (Tokens.AND(yypos,yypos+3));
<INITIAL>">="     => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"      => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="     => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"      => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"     => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"      => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"      => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"      => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"      => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."      => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"      => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"      => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"      => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["      => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"      => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("      => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"      => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"\""     => (YYBEGIN STRING; continue());
<INITIAL>{intlit} => (Tokens.INT(Option.valOf (Int.fromString yytext),yypos,yypos + String.size yytext));
<INITIAL>{id}     => (Tokens.ID(yytext,yypos,yypos + String.size yytext));
<INITIAL>\n       => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
.                 => (ErrorMsg.error yypos ("illegal character [" ^ yytext ^ "]"); continue());
