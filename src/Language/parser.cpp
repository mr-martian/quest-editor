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

namespace AST {

auto parse_qualified_name(tokenizer& tk) -> unique_ptr<NameAST> {
	unique_ptr<NameAST> name;
	if (tk.gettok_if(Token::kw_Bool)) {
		if (tk.gettok_if(Token::punct_scope)) {
			auto val = tk.expect({Token::kw_true, Token::kw_false});
			name->_type = std::make_unique<BuiltinTypeID>("Bool");
			name->_basename = (val.type == Token::kw_true) ? "true" : "false";
		} else {
			name->_type = std::make_unique<BuiltinTypeID>("Type");
			name->_basename = "Bool";
		}
		return name;
	} else {
		if (tk.gettok_if(Token::punct_scope)) {
			name->_discrim.scope.emplace_back();
		}
		auto el = tk.expect(Token::identifier);
		while (tk.gettok_if(Token::punct_scope)) {
			name->_discrim.scope.push_back(std::move(el.str));
			el = tk.expect(Token::identifier);
		}
		name->_basename = el.str;
		if (tk.gettok_if(Token::punct_bang)) {
			auto args = tk.expect(Token::literal_int);
			name->_discrim.args = stoi(args.str);
		}
		return name;
	}
}

auto parse_var_decl(tokenizer& tk) -> unique_ptr<VarDeclAST>;
auto parse_fn_decl(tokenizer& tk) -> unique_ptr<PrototypeAST>;
auto parse_enum_decl(tokenizer& tk) -> unique_ptr<EnumDeclAST>;
auto parse_struct_decl(tokenizer& tk) -> unique_ptr<StructProtoAST>;
auto parse_trait_decl(tokenizer& tk) -> unique_ptr<TraitDeclAST>;

auto parse_expr(tokenizer& tk) -> unique_ptr<ExprAST>;

auto parse_module_def(tokenizer& tk) -> unique_ptr<ModuleAST>;

auto parse_module(tokenizer& tk) -> unique_ptr<ModuleAST> {
	unique_ptr<ModuleAST> root;
	if (tk.peek().type == Token::kw_export or
	    tk.peek().type == Token::kw_module) {
		root = parse_module_def(tk);
	} else {
		throw unexpected(tk.gettok(), Token::kw_module);
	}

	while (tk) {
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
		alias->_type = parse_expr(tk);
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
