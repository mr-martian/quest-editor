%option unicode dotall nodefault

%top{
	#include <cstdio>
	#include "ast.hpp"
	
%}

%class{
	
%}

%init{
	
%}



%%

\p{Space}+	

(0[xob])?[0-9]+	return {Token::literal_int};
[+-]?([0-9]+(\.[0-9]*)?|\.[0-9]+)([eE][-+]?[0-9]+)?	return {Token::literal_float};

"Bool"    	return {Token::Bool};
"Float32"	return {Token::};
"Float64"
"Int"
"None"
"Noreturn"
"This"
"Type"
"Unsigned"
"Vector"

"alias"
"and"
"break"
"consume"
"continue"
"defer"
"delete"
"do"
"else"
"end"
"enum"
"export"
"fail"
"fn"
"for"
"if"
"implements"
"import"
"is"
"let"
"loop"
"module"
"mut"
"or"
"private"
"proc"
"public"
"result"
"return"
"struct"
"trait"
"typeof"
"until"
"while"
"yield"
"__\w*"
"_[A-Z]\w*"
"_"

\w+

\p{Unicode}
.

%%
