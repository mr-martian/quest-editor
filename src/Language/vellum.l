/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/
// clang-format off
%option unicode dotall nodefault

%option token-type="asyncpp::generator<Token>"
%option exception="std::invalid_argument(\"Scanner jammed\")"

%option ctorarg="std::string filename, bool newline_mode"
//%option ctorinit="_filename(std::move(filename))"
%option freespace

%top{
#include TOKEN_HEADER
//#include <coroutine>
#include <asyncpp/generator.h>

// clang-format off
%}

%class{

      public:
      std::string _filename;
      bool _newline_mode;

      source_location sl() const {
			return {_filename.c_str(), lineno(), columno(), size()};
      }

      Token token(Token::Type t) const {
			return Token{t, str(), sl()};
      }
      Token token(Token::Type t, std::string str) const {
			return Token{t, std::move(str), sl()};
      }
%}

%init{
	_filename = std::move(filename);
	_newline_mode = newline_mode;
%}

WHITESPACE	[[:space:]]+
ILCOMMENT	"//"([^\n]|\\\n)*\n
MLCOMMENT	"/*"(.|\n)*?"*/"
NAME			[[:alpha:]_][[:alnum:]_]*
UFLT			[0-9]*([0-9]|\.[0-9]|[0-9]\.)[0-9]*([Ee][-+]?[0-9]+)?
UOCT			0[Oo][0-7]+
UHEX			0[Xx][[:xdigit:]]+
UBIN			0[Bb][01]+
UDEC			0|[1-9][0-9]*
UINT			({UOCT}|{UHEX}|{UDEC})([Uu]?|[Uu][Ll]{0,2})
CHAR			'(\\.|[^\\\n'])*'
STRING		\"(\\.|\\\r?\n|[^\\\n"])*\"

literal_int [+-]?(0[ob])?[0-9]+|0x[[:xdigit:]]+
digit	[0-9]
hex_quad	[[:xdigit:]]{4}
decimal_int {digit}+
hexfloat		{digit}
literal_float	[+-]?({digit}+(\.{digit}*)?|\.{digit}+)([eE][-+]?{digit}+)?|{hexfloat}
named_UCN	"\\N{"[^"}\n"]+"}"
UCN	(?x)"\\u"{hex_quad} \
	| "\\U"{hex_quad}{hex_quad} \
	| "\\u{"[[:xdigit:]]+"}" \
	| {named_UCN}
simple_escape	\\[?0abfnrtv]|\\\"|\\'
escape_sequence	{simple_escape}|"\\x"([[:xdigit:]][[:xdigit:]]?|"{"[[:xdigit:]]+"}")
literal_string \"([^"\"\\\n"]|{escape_sequence}|{UCN})*\"

identifier	\p{Letter}[\p{Letter}\d_]*
operator_seq	[\+\-\*\/\%\=\<\>\&\|\^\?\@\#]+

%x COMMENT
%s ATTRIBUTE

%%

\n		{ if (_newline_mode) { co_yield token(Token::punct_newline); } }
\p{Space}+	{ }

"`" {identifier} "`"		{
 co_yield token(Token::identifier, str().substr(1, str().size() - 2));
}
"`" {operator_seq} "`"		{
	co_yield token(Token::identifier, str().substr(1, str().size() - 2));
}

"/*"	{ start(COMMENT); std::cout << "comment\n"; }
<COMMENT>{
"*/"	{ start(INITIAL); }
[^*]+	{ } // ignore everything in comments
"*"+[^*/]*	{ } // ignore *s not followed by /
<<EOF>>	{ lexer_error("Block comment not ended before end of file"); co_return; }
}
"//"[^\n]* { std::cout << "comment\n"; }

<ATTRIBUTE>{
"]"            { co_yield token(Token::punct_rbrck); start(INITIAL); }
{identifier}	{ co_yield token(Token::identifier); }
}

{literal_int}   	{ co_yield token(Token::literal_int, str()); }
{literal_float} 	{ co_yield token(Token::literal_float, str()); }
{literal_string}	{ co_yield token(Token::literal_string, str()); }

"Bool"   	{ co_yield token(Token::kw_Bool); }
"Byte"   	{ co_yield token(Token::kw_Byte); }
"Fail"   	{ co_yield token(Token::kw_Fail); }
"Float32"	{ co_yield token(Token::kw_Float32); }
"Float64"	{ co_yield token(Token::kw_Float64); }
"Int"    	{ co_yield token(Token::kw_Int); }
"None"   	{ co_yield token(Token::kw_None); }
"Noreturn"	{ co_yield token(Token::kw_Noreturn); }
"This"   	{ co_yield token(Token::kw_This); }
"Type"   	{ co_yield token(Token::kw_Type); }
"Unsigned"	{ co_yield token(Token::kw_Unsigned); }

"i"([0-9]+)	{ co_yield token(Token::id_int, str()); }
"u"([0-9]+)	{ co_yield token(Token::id_unsigned, str()); }

"__"\w*  	{ co_yield token(Token::reserved_id, str()); }
"_"[A-Z]\w*	{ co_yield token(Token::reserved_id, str()); }
"_"[0-9]+	{ co_yield token(Token::placeholder, str()); }
"_"      	{ co_yield token(Token::kw_underscore); }

"false"  	{ co_yield token(Token::kw_false); }
"true"   	{ co_yield token(Token::kw_true); }

"alias"  	{ co_yield token(Token::kw_alias); }
"as"     	{ co_yield token(Token::kw_as); }
"asm"    	{ co_yield token(Token::kw_asm); }
"await"		{ co_yield token(Token::kw_await); }
"break"  	{ co_yield token(Token::kw_break); }
"const"     { co_yield token(Token::kw_const); }
"consume"	{ co_yield token(Token::kw_consume); }
"continue"	{ co_yield token(Token::kw_continue); }
"defer"  	{ co_yield token(Token::kw_defer); }
"delete" 	{ co_yield token(Token::kw_delete); }
"do"     	{ co_yield token(Token::kw_do); }
"drop"   	{ co_yield token(Token::kw_drop); }
"else"   	{ co_yield token(Token::kw_else); }
"end"    	{ co_yield token(Token::kw_end); }
"enum"    	{ co_yield token(Token::kw_enum); }
"export"		{ co_yield token(Token::kw_export); }
"extern"		{ co_yield token(Token::kw_extern); }
"fn"     	{ co_yield token(Token::kw_fn); }
"for"    	{ co_yield token(Token::kw_for); }
"if"     	{ co_yield token(Token::kw_if); }
"implements"	{ co_yield token(Token::kw_implements); }
"import" 	{ co_yield token(Token::kw_import); }
"in"     	{ co_yield token(Token::kw_in); }
"is"     	{ co_yield token(Token::kw_is); }
"let"			{ co_yield token(Token::kw_let); }
"llvm"   	{ co_yield token(Token::kw_llvm); }
"loop"   	{ co_yield token(Token::kw_loop); }
"match"  	{ co_yield token(Token::kw_match); }
"module" 	{ co_yield token(Token::kw_module); }
"mut"    	{ co_yield token(Token::kw_mut); }
"namespace"	{ co_yield token(Token::kw_namespace); }
"private"	{ co_yield token(Token::kw_private); }
"proc"   	{ co_yield token(Token::kw_proc); }
"public" 	{ co_yield token(Token::kw_public); }
"result" 	{ co_yield token(Token::kw_result); }
"return" 	{ co_yield token(Token::kw_return); }
"struct" 	{ co_yield token(Token::kw_struct); }
"substrate"	{ co_yield token(Token::kw_substrate); }
"trait"  	{ co_yield token(Token::kw_trait); }
"typeof" 	{ co_yield token(Token::kw_typeof); }
"unless" 	{ co_yield token(Token::kw_unless); }
"until"  	{ co_yield token(Token::kw_until); }
"while"  	{ co_yield token(Token::kw_while); }
"yield"  	{ co_yield token(Token::kw_yield); }

":="	{ co_yield token(Token::op_assign); }
"+="	{ co_yield token(Token::op_plus_assign); }
"+?="	{ co_yield token(Token::op_qplus_assign); }
"+%="	{ co_yield token(Token::op_wplus_assign); }
"-="	{ co_yield token(Token::op_minus_assign); }
"-?="	{ co_yield token(Token::op_qminus_assign); }
"-%="	{ co_yield token(Token::op_wminus_assign); }
"*="	{ co_yield token(Token::op_times_assign); }
"/="	{ co_yield token(Token::op_div_assign); }
"%="	{ co_yield token(Token::op_rem_assign); }
"&="	{ co_yield token(Token::op_bitand_assign); }
"|="	{ co_yield token(Token::op_bitor_assign); }
"<<="	{ co_yield token(Token::op_lshift_assign); }
">>="	{ co_yield token(Token::op_rshift_assign); }
"xor=" { co_yield token(Token::op_xor_assign); }
"⊻="  { co_yield token(Token::op_xor_assign); }

"{"	{ co_yield token(Token::punct_lbrace); }
"}"	{ co_yield token(Token::punct_rbrace); }
"["	{ co_yield token(Token::punct_lbrck); }
"]"	{ co_yield token(Token::punct_rbrck); }
"("	{ co_yield token(Token::punct_lparen); }
")"	{ co_yield token(Token::punct_rparen); }
","	{ co_yield token(Token::punct_comma); }
";"	{ co_yield token(Token::punct_semi); }
"="	{ co_yield token(Token::punct_equal); }
"->"	{ co_yield token(Token::punct_arrow); }
":"	{ co_yield token(Token::punct_colon); }
"::"	{ co_yield token(Token::punct_scope); }
"!"	{ co_yield token(Token::punct_bang); }
"$"	{ co_yield token(Token::punct_dollar); }
"#["	{ co_yield token(Token::punct_attr); start(ATTRIBUTE); }
"(:"	{ co_yield token(Token::punct_substr_b); }
":)"	{ co_yield token(Token::punct_substr_e); }

"..."	{ co_yield token(Token::op_dots); }
"."	{ co_yield token(Token::op_dot); }
"^"	{ co_yield token(Token::op_carat); }
"@"	{ co_yield token(Token::op_at); }
"#"	{ co_yield token(Token::op_hash); }
"|>"	{ co_yield token(Token::op_pipe); }

"and"	{ co_yield token(Token::op_and); }
"or"	{ co_yield token(Token::op_or); }
"not"	{ co_yield token(Token::op_not); }
"¬"	{ co_yield token(Token::op_not); }
"mod"	{ co_yield token(Token::op_mod); }
"xor"	{ co_yield token(Token::op_xor); }
"⊻"	{ co_yield token(Token::op_xor); }
"+"	{ co_yield token(Token::op_plus); }
"-"	{ co_yield token(Token::op_minus); }
"*"	{ co_yield token(Token::op_times); }
"/"	{ co_yield token(Token::op_div); }
"%"	{ co_yield token(Token::op_rem); }
"+?"	{ co_yield token(Token::op_qplus); }
"-?"	{ co_yield token(Token::op_qminus); }
"+%"	{ co_yield token(Token::op_wplus); }
"-%"	{ co_yield token(Token::op_wminus); }
"&"	{ co_yield token(Token::op_bitand); }
"|"	{ co_yield token(Token::op_bitor); }
"<<"	{ co_yield token(Token::op_lshift); }
">>"	{ co_yield token(Token::op_rshift); }
"~"	{ co_yield token(Token::op_compl); }
"=="	{ co_yield token(Token::op_equal); }
"!="	{ co_yield token(Token::op_unequal); }
"<>"	{ co_yield token(Token::op_unequal); }
"<=>"	{ co_yield token(Token::op_cmp); }
"<="	{ co_yield token(Token::op_lte); }
">="	{ co_yield token(Token::op_gte); }
"<"	{ co_yield token(Token::op_less); }
">"	{ co_yield token(Token::op_greater); }
"?:"	{ co_yield token(Token::op_orelse); }
"??"	{ co_yield token(Token::op_qmqm); }
"?"	{ co_yield token(Token::op_qm); }

{identifier}	{ co_yield token(Token::identifier, str()); }

\p{Unicode}		{ co_yield token(Token::unknown, str()); }
.					{ co_yield token(Token::unknown, str()); }

<<EOF>>			{ co_yield token(Token::eof); co_return; }

%%
