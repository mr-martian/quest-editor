#include <ast.hpp>

#include <fstream>
#include <iostream>

int main(int argc, char** argv) {
	if (argc > 1) {
		std::ifstream ifile(argv[1]);
		tokenizer tk(ifile);
		auto AST = AST::parse_module(tk);
		AST->pretty_print(std::cout);
	}
}
