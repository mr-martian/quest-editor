/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

#include <fstream>
#include <iostream>
#include <tclap/CmdLine.h>

int main(int argc, char** argv) try {

	TCLAP::CmdLine cmd("Vellum compiler/interpreter");

	const TCLAP::UnlabeledValueArg<std::string> file("file", "source file", true,
	                                                 "", "filename", cmd);
	const TCLAP::SwitchArg tokenize_only("T", "tokenize",
	                                     "run only the tokenizer", cmd);
	const TCLAP::SwitchArg verbose("v", "verbose",
	                               "Print extra debug information", cmd);

	cmd.parse(argc, argv);

	if (tokenize_only.getValue()) {
		std::ifstream ifile(file.getValue());
		Lexer lex(file.getValue(), false, ifile);
		int tokens{};
		for (auto tok : lex.lex()) {
			++tokens;
			const auto name = tok_name(tok.type);
			/*constexpr auto& flag = "\x1B";
			auto formatted = std::string{};
			kblib::search_replace_copy(name,     //
			                           flag,     //
			                           last.str, //
			                           std::back_inserter(formatted));*/
			std::cout << "TOKEN: " << tok.type << ' '; // << formatted
			for (auto c : name) {
				if (c == '\x1B') {
					std::cout << tok.str;
				} else {
					std::cout << c;
				}
			}
			std::cout << '\n' << std::flush;
		}
		std::cout << "Read " << tokens << " tokens.\n";
	} else {
		std::ifstream ifile(file.getValue());
		tokenizer tk(ifile, file.getValue(), false, verbose.getValue());
		auto scope = AST::Scope{};
		try {
			auto ast = AST::parse_module(tk, scope);
			ast->pretty_print(std::cout);
		} catch (int e) {
			std::clog.flush();
			std::cout.flush();

			std::cerr << "Error at/before: " << tk.peek().loc << ": ";
			std::cerr << tok_name(tk.peek()) << '\n';
			throw;
		}
	}
} catch (int e) {
	std::clog.flush();
	std::cerr << "Not yet implemented: " << e << ' ';
	if (e) {
		std::cerr << "[Error]\n";
	} else {
		std::cerr << "[Todo]\n";
	}
} catch (const unexpected& e) {
	std::clog.flush();
	std::cout.flush();
	std::cerr << e.what() << '\n';
} catch (const std::exception& e) {
	std::clog.flush();
	std::cout.flush();
	std::cerr << e.what() << '\n';
} catch (...) {
	std::clog.flush();
	std::cout.flush();
	std::cerr << "Unknown exception.\n";
}
