/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"
#include LEX_HEADER

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
	if (tk.peek().type == Token::kw_export
	    or tk.peek().type == Token::kw_module) {
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
    -> unique_ptr<DeclarationAST> {
	const auto& context = outer._synthesized_discriminators;
	unique_ptr<DeclarationAST> decl;
	string language = tk.gettok_if(Token::literal_string)
	                      .value_or(Token{Token::literal_string, outer.language})
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
	return decl;
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

auto parse_initializer(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ASTNode> {
	if (tk.gettok_if(Token::punct_equal)) {
		return parse_expr(tk, context);
	} else {
		tk.expect(Token::punct_lbrace);
		auto list = std::make_unique<ExprListAST>();
		while (not tk.gettok_if(Token::punct_rbrace)) {
			list->_elems.push_back(parse_expr(tk, context));
		}
		return list;
	}
}

auto parse_var_decl(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<VarDeclAST> {
	tk.expect(Token::kw_let);
	auto decl = std::make_unique<VarDeclAST>();

	if (tk.gettok_if(Token::kw_mut)) {
		decl->_is_mut = true;
	} else if (tk.gettok_if(Token::kw_const)) {
		decl->_is_const = true;
	}
	decl->_name = tk.expect(Token::identifier).str;
	if (tk.gettok_if(Token::punct_colon)) {
		decl->_type = parse_expr(tk, context);
	}
	if (not tk.gettok_if(Token::punct_semi)) {
		decl->_initializer = parse_initializer(tk, context);
		tk.expect(Token::punct_semi);
	}
	return decl;
}

auto parse_control_expr(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ExprAST>;
auto parse_assignment_expr(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ExprAST>;
auto parse_block(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ExprAST>;

auto parse_expr(tokenizer& tk, const Discriminators& context)
    -> unique_ptr<ExprAST> {
	switch (tok_classify(tk.cur().type)) {
	case token_class::eof:
	case token_class::unknown: {
		throw unexpected(tk.cur(), "expression");
	} break;
	case token_class::literal:
	case token_class::identifier: {

	} break;
	case token_class::punct: {
		switch (tk.cur().type) {
		case Token::punct_lbrace: {
			parse_block(tk, context);
		} break;
		case Token::punct_lbrck: {

		} break;
		case Token::punct_lparen: {

		} break;
		case Token::punct_scope: {

		} break;
		case Token::punct_substr_b: {

		} break;
		case Token::punct_attr: {

		} break;
		default:
			throw unexpected(tk.cur(), "expression");
		}
	} break;
	case token_class::op: {

	} break;
	case token_class::keyword: {
		// left corners of control_expr, excepting if-expressions and short-form
		// substrate-expressions
		switch (tk.cur().type) {
		case Token::kw_await:
		case Token::kw_break:
		case Token::kw_continue:
		case Token::kw_do:
		case Token::kw_for:
		case Token::kw_if:
		case Token::kw_llvm:
		case Token::kw_loop:
		case Token::kw_match:
		case Token::kw_result:
		case Token::kw_return:
		case Token::kw_substrate:
		case Token::kw_unless:
		case Token::kw_until:
		case Token::kw_while:
		case Token::kw_yield: {
			return parse_control_expr(tk, context);
		} break;
		default:
			parse_assignment_expr(tk, context);
		}
	} break;
	case token_class::special: {

	} break;
	}
}

} // namespace AST
