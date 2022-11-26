%option unicode dotall nodefault

%top{
#include AST_HEADER

// clang-format off
%}

%class{

	Token _type;

%}

%init{
	
%}

literal_int (0[xob])?[0-9]+
decimal_int [0-9]+
literal_float	[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][-+]?[0-9]+)?

%%

\p{Space}+	

{literal_int}	return (_type = {Token::literal_int, str()}).good();
{literal_float}	return (_type = {Token::literal_float, str()}).good();

"Bool"    	return (_type = {Token::kw_Bool}).good();
"Fail"	return (_type = {Token::kw_Fail}).good();
"Float32"	return (_type = {Token::kw_Float32}).good();
"Float64"	return (_type = {Token::kw_Float64}).good();
"Int"	return (_type = {Token::kw_Int}).good();
"None"	return (_type = {Token::kw_None}).good();
"Noreturn"	return (_type = {Token::kw_Noreturn}).good();
"This"	return (_type = {Token::kw_This}).good();
"Type"	return (_type = {Token::kw_Type}).good();
"Unsigned"	return (_type = {Token::kw_Unsigned}).good();

i([0-9]+)	return (_type = {Token::id_int, str()}).good();
u([0-9]+)	return (_type = {Token::id_unsigned, str()}).good();

"alias"	return (_type = {Token::kw_alias}).good();
"as"	return (_type = {Token::kw_as}).good();
"break"	return (_type = {Token::kw_break}).good();
"consume"	return (_type = {Token::kw_consume}).good();
"continue"	return (_type = {Token::kw_continue}).good();
"defer"	return (_type = {Token::kw_defer}).good();
"delete"	return (_type = {Token::kw_delete}).good();
"do"	return (_type = {Token::kw_do}).good();
"else"	return (_type = {Token::kw_else}).good();
"end"	return (_type = {Token::kw_end}).good();
"enum"	return (_type = {Token::kw_enum}).good();
"export"	return (_type = {Token::kw_export}).good();
"false"	return (_type = {Token::kw_false}).good();
"fn"	return (_type = {Token::kw_fn}).good();
"for"	return (_type = {Token::kw_for}).good();
"if"	return (_type = {Token::kw_if}).good();
"implements"	return (_type = {Token::kw_implements}).good();
"import"	return (_type = {Token::kw_import}).good();
"is"	return (_type = {Token::kw_is}).good();
"let"	return (_type = {Token::kw_let}).good();
"loop"	return (_type = {Token::kw_loop}).good();
"match"	return (_type = {Token::kw_match}).good();
"module"	return (_type = {Token::kw_module}).good();
"mut"	return (_type = {Token::kw_mut}).good();
"private"	return (_type = {Token::kw_private}).good();
"proc"	return (_type = {Token::kw_proc}).good();
"public"	return (_type = {Token::kw_public}).good();
"result"	return (_type = {Token::kw_result}).good();
"return"	return (_type = {Token::kw_return}).good();
"struct"	return (_type = {Token::kw_struct}).good();
"trait"	return (_type = {Token::kw_trait}).good();
"true"	return (_type = {Token::kw_true}).good();
"typeof"	return (_type = {Token::kw_typeof}).good();
"unless"	return (_type = {Token::kw_unless}).good();
"until"	return (_type = {Token::kw_until}).good();
"while"	return (_type = {Token::kw_while}).good();
"yield"	return (_type = {Token::kw_yield}).good();

"__\w*"	return (_type = {Token::reserved_id, str()}).good();
"_[A-Z]\w*"	return (_type = {Token::reserved_id, str()}).good();
"_[0-9]+"	return (_type = {Token::placeholder}).good();
"_"	return (_type = {Token::kw_underscore}).good();

"{"	return (_type = {Token::punct_lbrace}).good();
"}"	return (_type = {Token::punct_rbrace}).good();
"\["	return (_type = {Token::punct_lbrck}).good();
"\]"	return (_type = {Token::punct_rbrck}).good();
"\("	return (_type = {Token::punct_lparen}).good();
"\)"	return (_type = {Token::punct_rparen}).good();
","	return (_type = {Token::punct_comma}).good();
";"	return (_type = {Token::punct_semi}).good();
"="	return (_type = {Token::punct_equal}).good();
"->"	return (_type = {Token::punct_arrow}).good();
":"	return (_type = {Token::punct_colon}).good();
"::"	return (_type = {Token::punct_scope}).good();
"!"	return (_type = {Token::punct_bang}).good();
"\$"	return (_type = {Token::punct_dollar}).good();
"#\["	return (_type = {Token::punct_attr}).good();
"\n"	return (_type = {Token::punct_newline}).good();

"and"	return (_type = {Token::op_and}).good();
"or"	return (_type = {Token::op_or}).good();
"not"	return (_type = {Token::op_not}).good();
"xor"	return (_type = {Token::op_xor}).good();
"\."	return (_type = {Token::op_dot}).good();
"\+"	return (_type = {Token::op_plus}).good();
"-"	return (_type = {Token::op_minus}).good();
"\*"	return (_type = {Token::op_times}).good();
"/"	return (_type = {Token::op_div}).good();
"%"	return (_type = {Token::op_mod}).good();
"\+\?"	return (_type = {Token::op_qplus}).good();
"-\?"	return (_type = {Token::op_qminus}).good();
"\+%"	return (_type = {Token::op_wplus}).good();
"-%"	return (_type = {Token::op_wminus}).good();
"&"	return (_type = {Token::op_bitand}).good();
"\|"	return (_type = {Token::op_bitor}).good();
"^"	return (_type = {Token::op_carat}).good();
"~"	return (_type = {Token::op_compl}).good();
"@"	return (_type = {Token::op_at}).good();
"#"	return (_type = {Token::op_hash}).good();
"=="	return (_type = {Token::op_equal}).good();
"!="	return (_type = {Token::op_unequal}).good();
"<=>"	return (_type = {Token::op_cmp}).good();
"<"	return (_type = {Token::op_less}).good();
">"	return (_type = {Token::op_greater}).good();
"<="	return (_type = {Token::op_lte}).good();
">="	return (_type = {Token::op_gte}).good();
"\?"	return (_type = {Token::op_qm}).good();
"\?\?"	return (_type = {Token::op_qmqm}).good();
":="	return (_type = {Token::op_assign}).good();


\w+

\p{Unicode}
.

%%
