// clang-format off
%option unicode dotall nodefault

%option token-type="asyncpp::generator<Token>"
%option exception="std::invalid_argument(\"Scanner jammed\")"

%option ctorarg="std::string filename, bool newline_mode"
//%option ctorinit="_filename(std::move(filename))"

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
%}

%init{
	_filename = std::move(filename);
	_newline_mode = newline_mode;
%}

literal_int (0[xob])?[0-9]+
decimal_int [0-9]+
literal_float	[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][-+]?[0-9]+)?

identifier	\p{Letter}[\p{Letter}\p{Digit}_]*

%x COMMENT

%%


\n		if (_newline_mode) { co_yield {Token::punct_newline}; }
\p{Space}+	

"/*"	start(COMMENT);
<COMMENT>{
"*/"	start(INITIAL);
[^*]+	// ignore everything in comments
"*"+[^*/]*	// ignore *s not followed by /
<<EOF>>	lexer_error("Block comment not ended before end of file"); co_return;
}
"//"[^\n]*

{literal_int}	co_yield {Token::literal_int, str()};
{literal_float}	co_yield {Token::literal_float, str()};

"Bool"	co_yield {Token::kw_Bool};
"Byte"	co_yield {Token::kw_Byte};
"Fail"	co_yield {Token::kw_Fail};
"Float32"	co_yield {Token::kw_Float32};
"Float64"	co_yield {Token::kw_Float64};
"Int"	co_yield {Token::kw_Int};
"None"	co_yield {Token::kw_None};
"Noreturn"	co_yield {Token::kw_Noreturn};
"This"	co_yield {Token::kw_This};
"Type"	co_yield {Token::kw_Type};
"Unsigned"	co_yield {Token::kw_Unsigned};

"i"([0-9]+)	co_yield {Token::id_int, str()};
"u"([0-9]+)	co_yield {Token::id_unsigned, str()};

"__"\w*	co_yield {Token::reserved_id, str()};
"_"[A-Z]\w*	co_yield {Token::reserved_id, str()};
"_"[0-9]+	co_yield {Token::placeholder, str()};
"_"	co_yield {Token::kw_underscore};

"alias"	co_yield {Token::kw_alias};
"asm"	co_yield {Token::kw_asm};
"as"	co_yield {Token::kw_as};
"await"	co_yield {Token::kw_await};
"break"	co_yield {Token::kw_break};
"consume"	co_yield {Token::kw_consume};
"continue"	co_yield {Token::kw_continue};
"defer"	co_yield {Token::kw_defer};
"delete"	co_yield {Token::kw_delete};
"do"	co_yield {Token::kw_do};
"drop"	co_yield {Token::kw_drop};
"else"	co_yield {Token::kw_else};
"end"	co_yield {Token::kw_end};
"enum"	co_yield {Token::kw_enum};
"export"	co_yield {Token::kw_export};
"false"	co_yield {Token::kw_false};
"fn"	co_yield {Token::kw_fn};
"for"	co_yield {Token::kw_for};
"if"	co_yield {Token::kw_if};
"implements"	co_yield {Token::kw_implements};
"import"	co_yield {Token::kw_import};
"is"	co_yield {Token::kw_is};
"let"	co_yield {Token::kw_let};
"loop"	co_yield {Token::kw_loop};
"match"	co_yield {Token::kw_match};
"module"	co_yield {Token::kw_module};
"mut"	co_yield {Token::kw_mut};
"private"	co_yield {Token::kw_private};
"proc"	co_yield {Token::kw_proc};
"public"	co_yield {Token::kw_public};
"result"	co_yield {Token::kw_result};
"return"	co_yield {Token::kw_return};
"struct"	co_yield {Token::kw_struct};
"substrate"	co_yield {Token::kw_substrate};
"trait"	co_yield {Token::kw_trait};
"true"	co_yield {Token::kw_true};
"typeof"	co_yield {Token::kw_typeof};
"unless"	co_yield {Token::kw_unless};
"until"	co_yield {Token::kw_until};
"while"	co_yield {Token::kw_while};
"yield"	co_yield {Token::kw_yield};

":="	co_yield {Token::op_assign};
"+="	co_yield {Token::op_plus_assign};
"+?="	co_yield {Token::op_qplus_assign};
"+%="	co_yield {Token::op_wplus_assign};
"-="	co_yield {Token::op_minus_assign};
"-?="	co_yield {Token::op_qminus_assign};
"-%="	co_yield {Token::op_wminus_assign};
"*="	co_yield {Token::op_times_assign};
"/="	co_yield {Token::op_div_assign};
"%="	co_yield {Token::op_rem_assign};
"&="	co_yield {Token::op_bitand_assign};
"|="	co_yield {Token::op_bitor_assign};
"<<="	co_yield {Token::op_lshift_assign};
">>="	co_yield {Token::op_rshift_assign};

"{"	co_yield {Token::punct_lbrace};
"}"	co_yield {Token::punct_rbrace};
"["	co_yield {Token::punct_lbrck};
"]"	co_yield {Token::punct_rbrck};
"("	co_yield {Token::punct_lparen};
")"	co_yield {Token::punct_rparen};
","	co_yield {Token::punct_comma};
";"	co_yield {Token::punct_semi};
"="	co_yield {Token::punct_equal};
"->"	co_yield {Token::punct_arrow};
":"	co_yield {Token::punct_colon};
"::"	co_yield {Token::punct_scope};
"!"	co_yield {Token::punct_bang};
"$"	co_yield {Token::punct_dollar};
"#["	co_yield {Token::punct_attr};
"(:"	co_yield {Token::punct_substr_b};
":)"	co_yield {Token::punct_substr_e};

"."	co_yield {Token::op_dot};
"^"	co_yield {Token::op_carat};
"@"	co_yield {Token::op_at};
"#"	co_yield {Token::op_hash};
"|>"	co_yield {Token::op_pipe};

"and"	co_yield {Token::op_and};
"or"	co_yield {Token::op_or};
"not"	co_yield {Token::op_not};
"mod"	co_yield {Token::op_mod};
"xor"	co_yield {Token::op_xor};
"+"	co_yield {Token::op_plus};
"-"	co_yield {Token::op_minus};
"*"	co_yield {Token::op_times};
"/"	co_yield {Token::op_div};
"%"	co_yield {Token::op_rem};
"+?"	co_yield {Token::op_qplus};
"-?"	co_yield {Token::op_qminus};
"+%"	co_yield {Token::op_wplus};
"-%"	co_yield {Token::op_wminus};
"&"	co_yield {Token::op_bitand};
"|"	co_yield {Token::op_bitor};
"<<"	co_yield {Token::op_lshift};
">>"	co_yield {Token::op_rshift};
"~"	co_yield {Token::op_compl};
"=="	co_yield {Token::op_equal};
"!="	co_yield {Token::op_unequal};
"<>"	co_yield {Token::op_unequal};
"<=>"	co_yield {Token::op_cmp};
"<="	co_yield {Token::op_lte};
">="	co_yield {Token::op_gte};
"<"	co_yield {Token::op_less};
">"	co_yield {Token::op_greater};
"??"	co_yield {Token::op_qmqm};
"?"	co_yield {Token::op_qm};

{identifier}	co_yield {Token::identifier, str()};

\p{Unicode}	co_yield {Token::unknown, str()};
.	co_yield {Token::unknown, str()};

<<EOF>>	co_return;

%%
