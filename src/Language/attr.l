/* *****************************************************************************
 * Vellum
 * Copyright (c) 2026 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/
 // clang-format off
%option unicode dotall nodefault

%option token-type="Token"
%option exception="std::invalid_argument(\"Scanner jammed\")"

%option freespace

%top{
#include "token.hpp"
// clang-format off
%}

%class{

	public:
	source_location sl() const {
		return {"", lineno(), columno(), size()};
	}
	Token token(Token::Type t) const {
		return Token{t, str(), sl()};
	}
	Token token(Token::Type t, std::string str) const {
		return Token{t, std::move(str), sl()};
	}
%}

UFLT			[0-9]*([0-9]|\.[0-9]|[0-9]\.)[0-9]*([Ee][-+]?[0-9]+)?
UDEC			0|[1-9][0-9]*
UOCT			0[Oo][0-7]+
UHEX			0[Xx][[:xdigit:]]+
UBIN			0[Bb][01]+
UINT			({UOCT}|{UHEX}|{UDEC})([Uu]?|[Uu][Ll]{0,2})
literal_int [+-]?({UDEC}|{UHEX}|{UOCT}|{UBIN})[Uu]?

hexfloat		0[[:xdigit:]]p
literal_float	[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][-+]?[0-9]+)?|{hexfloat}

hex_quad	[[:xdigit:]]{4}
named_UCN	"\\N{"[^"}\n"]+"}"
UCN	(?x)"\\u"{hex_quad} \
	| "\\U"{hex_quad}{hex_quad} \
	| "\\u{"[[:xdigit:]]+"}" \
	| {named_UCN}
simple_escape	\\[?0abfnrtv\"']
escape_sequence	{simple_escape}|"\\x"([[:xdigit:]][[:xdigit:]]?|"{"[[:xdigit:]]+"}")
literal_string (?m)\"([^"\"\\\n"]|{escape_sequence}|{UCN}|(?:\\\n))*\"
illegal_escape	\\[^?0abfnrtv\"']
illegal_string (?m)\"([^"\"\\"]|{escape_sequence}|{UCN}|(?:\\\n)|{illegal_escape})*\"
illegal_int [0-9]\w*

identifier	\p{Letter}[\p{Letter}\d_]*
operator	[\+\-\*\/\%\=\<\>\&\|\^\?\@\#]
stropped_identifier [\p{Letter}\d_\+\-\*\/\%\=\<\>\&\|\^\?\@\#]+

%%

\n		{ return token(Token::punct_newline); }
\p{Space}+	{ }

<*>"`" {stropped_identifier} "`"		{
 return token(Token::identifier, str().substr(1, str().size() - 2));
}

"/*"(.|\n)*?"*/"	{ std::cout << "comment " << std::quoted(text()) << "\n"; }
"/*"(.|\n)*?<<EOF>>	{ lexer_error("Block comment not ended before end of file"); }
"//"[^\n]* { std::cout << "comment " << std::quoted(text()) << "\n"; }

{literal_int}   	{ return token(Token::literal_int, str()); }
{literal_float} 	{ return token(Token::literal_float, str()); }
{literal_string}	{ return token(Token::literal_string, str()); }
{illegal_int}		{ return token(Token::illegal_literal, str()); }
{illegal_string}	{ return token(Token::illegal_literal, str()); }

":="	{ return token(Token::op_assign); }
"+="	{ return token(Token::op_plus_assign); }
"+?="	{ return token(Token::op_qplus_assign); }
"+%="	{ return token(Token::op_wplus_assign); }
"-="	{ return token(Token::op_minus_assign); }
"-?="	{ return token(Token::op_qminus_assign); }
"-%="	{ return token(Token::op_wminus_assign); }
"*="	{ return token(Token::op_times_assign); }
"/="	{ return token(Token::op_div_assign); }
"%="	{ return token(Token::op_rem_assign); }
"&="	{ return token(Token::op_bitand_assign); }
"|="	{ return token(Token::op_bitor_assign); }
"<<="	{ return token(Token::op_lshift_assign); }
">>="	{ return token(Token::op_rshift_assign); }
"xor=" { return token(Token::op_xor_assign); }
"⊻="  { return token(Token::op_xor_assign); }

"{"	{ return token(Token::punct_lbrace); }
"}"	{ return token(Token::punct_rbrace); }
"["	{ return token(Token::punct_lbrck); }
"]"	{ return token(Token::punct_rbrck); }
"("	{ return token(Token::punct_lparen); }
")"	{ return token(Token::punct_rparen); }
","	{ return token(Token::punct_comma); }
";"	{ return token(Token::punct_semi); }
"="	{ return token(Token::punct_equal); }
"->"	{ return token(Token::punct_arrow); }
":"	{ return token(Token::punct_colon); }
"::"	{ return token(Token::punct_scope); }
"!"	{ return token(Token::punct_bang); }
"$"	{ return token(Token::punct_dollar); }
"#["	{ return token(Token::punct_attr); }
"(:"	{ return token(Token::punct_substr_b); }
":)"	{ return token(Token::punct_substr_e); }

"..."	{ return token(Token::op_dots); }
"."	{ return token(Token::op_dot); }
"^"	{ return token(Token::op_carat); }
"@"	{ return token(Token::op_at); }
"#"	{ return token(Token::op_hash); }
"|>"	{ return token(Token::op_pipe); }

<*>{
{identifier}	{ return token(Token::identifier, str()); }
}

\p{Unicode}		{ return token(Token::unknown, str()); }
.					{ return token(Token::unknown, str()); }

<<EOF>>			{ return token(Token::eof); }

%%
