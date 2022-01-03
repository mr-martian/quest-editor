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

// avoid a dynamic_cast without opening up possibility of memory leak
template <typename Class, typename Base, typename... Args>
auto make_unique_for_modify(std::unique_ptr<Base>& owner, Args&&... args) {
	owner.reset();
	auto object = new Class(std::forward<Args>(args)...);
	owner.reset(object);
	return object;
}

namespace AST {

auto parse_qualified_name(tokenizer& tk) -> unique_ptr<NameAST> {
	auto name = std::make_unique<NameAST>();
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

auto parse_decl(tokenizer& tk, const DeclarationAST& context)
    -> unique_ptr<DeclarationAST>;
auto parse_var_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<VarDeclAST>;
auto parse_fn_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<PrototypeAST>;
auto parse_enum_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<EnumDeclAST>;
auto parse_struct_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<StructProtoAST>;
auto parse_trait_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<TraitDeclAST>;

auto parse_expr(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ExprAST>;

auto parse_module_def(tokenizer& tk) -> unique_ptr<ModuleAST>;
auto parse_ns_decl_seq(tokenizer& tk, const DeclarationAST& context)
    -> vector<unique_ptr<DeclarationAST>> {
	vector<unique_ptr<DeclarationAST>> decls;

	while (tk and not tk.check(Token::punct_rbrace)) {
		decls.push_back(parse_decl(tk, context));
	}
	return decls;
}
auto parse_namespace_block(tokenizer& tk, const DeclarationAST& outer)
    -> unique_ptr<NamespaceAST> {
	const auto& context = outer._synthesized_discriminators;
	tk.expect(Token::kw_namespace);
	auto decl = std::make_unique<NamespaceAST>();
	decl->language = outer.language;
	decl->_name = tk.expect(Token::identifier).str;
	auto& scope = decl->_synthesized_discriminators.scope;
	scope.reserve(context.scope.size() + 1);
	scope = context.scope;
	tk.expect(Token::punct_lbrace);

	// the scope of the declarations in the namespace has to include the
	// namespace name. Doing it in-place saves memory
	scope.push_back(decl->_name);
	decl->_declarations = parse_ns_decl_seq(tk, *decl);
	// remove the redundant name from the scope
	scope.pop_back();

	tk.expect(Token::punct_rbrace);
	return decl;
}

auto parse_module(tokenizer& tk) -> unique_ptr<ModuleAST> {
	unique_ptr<ModuleAST> root;
	if (tk.peek().type == Token::kw_export or
	    tk.peek().type == Token::kw_module) {
		root = parse_module_def(tk);
		root->language = "Vellum";
	} else {
		throw unexpected(tk.gettok(), Token::kw_module);
	}
	root->_declarations = parse_ns_decl_seq(tk, *root);
	return root;
}

auto parse_module_def(tokenizer& tk) -> unique_ptr<ModuleAST> {
	auto root = std::make_unique<ModuleAST>();
	{
		const auto tok = tk.gettok();
		switch (tok.type) {
		case Token::kw_export:
			root->_is_export = true;
			tk.expect(Token::kw_module);
			[[fallthrough]];
		case Token::kw_module: {
			const auto name = tk.expect(Token::identifier);
			root->_name = name.str;
			tk.expect(Token::punct_semi);
		} break;
		default:
			throw unexpected(tok, Token::kw_module);
		}
	}
	while (tk.gettok_if(Token::kw_import)) {
		const auto tok = tk.expect(Token::identifier);
		root->_imports.push_back(tok.str);
		tk.expect(Token::punct_semi);
	}
	return root;
}

auto parse_extern_decl(tokenizer& tk, const DeclarationAST& outer)
    -> unique_ptr<NamespaceAST> {
	const auto& context = outer._synthesized_discriminators;
	unique_ptr<DeclarationAST> decl;
	string language =
	    tk.gettok_if(Token::literal_string)
	        .value_or(Token{Token::literal_string, outer.language, {}})
	        .str;
	if (tk.check({Token::kw_fn, Token::kw_proc})) {
		decl = parse_fn_decl(tk, context);
		decl->language = language;
	} else if (tk.gettok_if(Token::punct_lbrace)) {
		auto alias = make_unique_for_modify<NamespaceAST>(decl);
		alias->_synthesized_discriminators.scope = context.scope;
		alias->language = language;
		alias->_declarations = parse_ns_decl_seq(tk, *alias);
		tk.expect(Token::punct_rbrace);
	}
}

auto parse_decl(tokenizer& tk, const DeclarationAST& outer)
    -> unique_ptr<DeclarationAST> {
	const auto& context = outer._synthesized_discriminators;
	unique_ptr<DeclarationAST> decl;
	bool is_export = tk.gettok_if(Token::kw_export).has_value();
	switch (tk.peek().type) {
	case Token::kw_let: {
		decl = parse_var_decl(tk, context);
	} break;
	case Token::kw_alias: {
		auto alias = make_unique_for_modify<AliasDeclAST>(decl);
		alias->_synthesized_discriminators.scope = context.scope;
		// parse alias declaration
		alias->_name = tk.expect(Token::identifier).str;
		tk.expect(Token::punct_equal);
		alias->_type = parse_expr(tk, context);
	} break;
	case Token::kw_fn:
	case Token::kw_proc: {
		decl = parse_fn_decl(tk, context);
		decl->language = outer.language;
	} break;
	case Token::kw_enum: {
		decl = parse_enum_decl(tk, context);
	} break;
	case Token::kw_struct: {
		decl = parse_struct_decl(tk, context);
	} break;
	case Token::kw_trait: {
		decl = parse_trait_decl(tk, context);
	} break;
	case Token::kw_extern: {
		decl = parse_extern_decl(tk, outer);
	} break;
	case Token::kw_namespace: {
		decl = parse_namespace_block(tk, outer);
	} break;
	default:
		throw unexpected(tk.peek(), Token::unknown);
	}
	decl->_is_export = is_export;
	return decl;
}

} // namespace AST
