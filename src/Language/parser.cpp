/* *****************************************************************************
 * Vellum
 * Copyright (c) 2021 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * ****************************************************************************/

#include "ast.hpp"

tokenizer::tokenizer(std::istream& in, bool l) : source{&in}, line_mode{l} {
	advance();
}

void tokenizer::advance() {
	last = std::exchange(next, read());
	return;
}
Token tokenizer::gettok() noexcept {
	advance();
	return last;
}
std::optional<Token> tokenizer::gettok_if(const Token::Type t) noexcept {
	if (next.type == t) {
		advance();
		return last;
	} else {
		return std::nullopt;
	}
}
Token tokenizer::expect(const Token::Type t) {
	if (auto n = gettok_if(t)) {
		return *std::move(n);
	} else {
		throw unexpected(last, t);
	}
}

tokenizer& tokenizer::ignore(const Token::Type t) noexcept {
	if (t != Token::eof and next.type == t) {
		advance();
	}
	return *this;
}
tokenizer& tokenizer::ignore_consecutive(const Token::Type t) noexcept {
	ignore(t);
	while (t != Token::eof and last.type == t) {
		advance();
	}
	return *this;
}

Token tokenizer::read() {
	// run tokenizer here
	return {};
}

namespace AST {

auto parse_module_def(tokenizer& tk) -> unique_ptr<ModuleAST>;

auto parse_module(tokenizer& tk) -> unique_ptr<ModuleAST> {
	unique_ptr<ModuleAST> root;
	if (tk.peek().type == Token::kw_export or
	    tk.peek().type == Token::kw_module) {
		root = parse_module_def(tk);
	} else {
		throw unexpected(tk.gettok(), Token::kw_module);
	}

	while (tk.peek()) {
		root->_declarations.push_back(parse_decl(tk));
	}
	return root;
}

auto parse_module_def(tokenizer& tk) -> unique_ptr<ModuleAST> {
	auto root = std::make_unique<ModuleAST>();
	{
		const auto tok = tk.gettok();
		switch (tok.type) {
		case Token::kw_export:
			root->is_exported = true;
			tk.expect(Token::kw_module);
			[[fallthrough]];
		case Token::kw_module: {
			const auto name = tk.expect(Token::identifier);
			root->name = name.str;
			tk.expect(Token::punct_semi);
		} break;
		default:
			throw unexpected(tok, Token::kw_module);
		}
	}
	while (tk.gettok_if(Token::kw_import)) {
		const auto tok = tk.expect(Token::identifier);
		root->imports.push_back(tok.str);
		tk.expect(Token::punct_semi);
	}
	return root;
}

auto parse_var_decl(tokenizer& tk) -> unique_ptr<VarDeclAST>;
auto parse_fn_decl(tokenizer& tk) -> unique_ptr<PrototypeAST>;
auto parse_enum_decl(tokenizer& tk) -> unique_ptr<EnumDeclAST>;
auto parse_struct_decl(tokenizer& tk) -> unique_ptr<StructProtoAST>;
auto parse_trait_decl(tokenizer& tk) -> unique_ptr<TraitDeclAST>;

auto parse_type_id(tokenizer& tk) -> unique_ptr<TypeID>;

auto parse_decl(tokenizer& tk) -> unique_ptr<DeclarationAST> {
	unique_ptr<DeclarationAST> decl;
	bool is_export = false;
	if (tk.gettok_if(Token::kw_export)) {
		is_export = true;
	}
	switch (auto tok = tk.gettok(); tok.type) {
	case Token::kw_let: {
		decl = parse_var_decl(tk);
	} break;
	case Token::kw_alias: {
		// avoid a dynamic_cast without opening up possibility of memory leak
		auto alias = new AliasDeclAST();
		decl.reset(alias);
		// parse alias declaration
		alias->_name = tk.expect(Token::identifier).str;
		tk.expect(Token::punct_equal);
		alias->_type = parse_type_id(tk);
	} break;
	case Token::kw_fn: {
		decl = parse_fn_decl(tk);
	} break;
	case Token::kw_enum: {
		decl = parse_enum_decl(tk);
	} break;
	case Token::kw_struct: {
		decl = parse_struct_decl(tk);
	} break;
	case Token::kw_trait: {
		decl = parse_trait_decl(tk);
	} break;
	default:
		throw unexpected(tok, Token::unknown);
	}
	decl->_is_export = is_export;
	return decl;
}

} // namespace AST
