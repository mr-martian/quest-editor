%option unicode dotall nodefault

%top{
	#include <cstdio>
	
%}

%class{
	
%}

%init{
	
%}




%%

\s+
"Bool"
"Float32"
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
"_"
"_[A-Z]\w*"
"__\w*"

\w+

\p{Unicode}
.

%%
