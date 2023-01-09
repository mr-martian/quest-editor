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

/* qualified-name ::=
 *		[ '::' ] { namespace-id '::' } identifier [ '!' integer-literal ]
 *******************************************************************************
 * Returns the token corresponding to the basename of the qualified-name
 * The out-param is because of both Name and NameExpr existing
 */
auto parse_qualified_name(tokenizer& tk, const Scope& scope, Name& name)
    -> Token {
	Token ret;
	if (not tk.gettok_if(Token::punct_scope)
	    and tok_classify(tk.peek().type) != token_class::keyword) {
		name._discrim.scope = scope.name._discrim.scope;
	}
	try {
		if (auto tok = tk.gettok_if(Token::kw_Bool)) {
			// Bool is always ::Bool
			name._discrim.scope.clear();
			if (tk.gettok_if(Token::punct_scope)) {
				ret = tk.expect({Token::kw_true, Token::kw_false});
				name._basename = (ret.type == Token::kw_true) ? "true" : "false";
				return ret;
			} else {
				name._basename = "Bool";
				ret = *tok;
				return ret;
			}
		} else {
			if (tk.gettok_if(Token::punct_scope)) {
				name._discrim.scope.emplace_back();
			}
			ret = tk.expect(Token::identifier);
			while (tk.gettok_if(Token::punct_scope)) {
				name._discrim.scope.push_back(std::move(ret).str);
				ret = tk.expect(Token::identifier);
			}
			name._basename = ret.str;
			if (tk.gettok_if(Token::punct_bang)) {
				auto args = tk.expect(Token::literal_int);
				name._discrim.args = kblib::parse_integer<Integer>(args.str);
			}
			return ret;
		}
	} catch (unexpected& e) {
		return std::move(e).found;
	}
}

auto parse_decl(tokenizer& tk, Scope& scope, const Declaration& context)
    -> unique_ptr<Declaration>;
auto parse_var_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<VarDecl>;
auto parse_fn_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<Prototype>;
auto parse_enum_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<EnumDecl> {
	throw 0;
}
auto parse_struct_decl(tokenizer& tk, Scope& scope,
                       const Discriminators& context)
    -> unique_ptr<StructProto> {
	throw 0;
}
auto parse_trait_decl(tokenizer& tk, Scope& scope,
                      const Discriminators& context) -> unique_ptr<TraitDecl> {
	throw 0;
}

auto parse_expr(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<Expr>;

auto parse_module_def(tokenizer& tk) -> unique_ptr<Module>;
/* ns-decl-seq ::=
 * 	{ declaration }
 */
auto parse_ns_decl_seq(tokenizer& tk, Scope& scope, const Declaration& context)
    -> vector<unique_ptr<Declaration>> {
	vector<unique_ptr<Declaration>> decls;

	while (tk and not tk.check(Token::punct_rbrace) and not tk.eof()) {
		bool is_export = tk.gettok_if(Token::kw_export).has_value();
		auto& decl = decls.emplace_back(parse_decl(tk, scope, context));
		decl->_is_export = is_export;
	}
	return decls;
}
/* namespace-block ::=
 *		'namespace' qualified-name '{' ns-decl-seq '}'
 */
auto parse_namespace_block(tokenizer& tk, Scope& parent,
                           const Declaration& outer) -> unique_ptr<Namespace> {
	const auto& context = outer._name._discrim;
	tk.expect(Token::kw_namespace);
	auto decl = std::make_unique<Namespace>();
	decl->language = outer.language;
	auto name = parse_qualified_name(tk, parent, decl->_name);
	if (name.type != Token::identifier) {
		throw unexpected(name, "qualified-name");
	}
	if (auto old_name = parent.symbols.find(decl->_name);
	    old_name != parent.symbols.end()) {
		if (not dynamic_cast<Namespace*>(old_name->second)) {
			// redefinition of name as different type of symbol
			throw 1;
		}
	} else {
		parent.symbols.try_emplace(decl->_name, decl.get());
	}
	auto& scope = decl->_name._discrim.scope;
	// scope.reserve(context.scope.size() + 1);
	scope = context.scope;
	tk.expect(Token::punct_lbrace);

	// the scope of the declarations in the namespace has to include the
	// namespace name. Doing it in-place saves memory
	scope.push_back(decl->_name._basename);
	decl->_declarations = parse_ns_decl_seq(tk, parent, *decl);
	// remove the redundant name from the scope
	scope.pop_back();

	tk.expect(Token::punct_rbrace);
	return decl;
}

/* module ::=
 *		module-definition [ module-imports ] module-decl-seq
 * module-imports ::=
 *		{ [ 'export' ] 'import' module-name ';' }
 * module-decl-seq ::=
 *		{ [ 'export' ] declaration }
 */
auto parse_module(tokenizer& tk) -> unique_ptr<Module> {
	unique_ptr<Module> root;
	auto scope = Scope{};
	if (tk.peek().type == Token::kw_export
	    or tk.peek().type == Token::kw_module) {
		root = parse_module_def(tk);
		root->language = "Vellum";
	} else {
		throw unexpected(tk.gettok(), "module");
	}

	auto is_export = bool{};
	while (tk and not tk.eof()) {
		is_export = tk.gettok_if(Token::kw_export).has_value();
		if (tk.gettok_if(Token::kw_import)) {
			do {
				auto name = tk.expect(Token::identifier);
				root->_imports.push_back({name.str, is_export});
			} while (tk.gettok_if(Token::punct_comma));
			tk.expect(Token::punct_semi);
		} else {
			break;
		}
	}
	root->pretty_print(std::cout) << '\n';
	while (tk and not tk.eof()) {
		auto& last
		    = root->_declarations.emplace_back(parse_decl(tk, scope, *root));
		last->_is_export = is_export;
		last->pretty_print(std::cout) << '\n';

		// this has to be done last so that it doesn't interfere with the previous
		// loop already setting it and then breaking
		is_export = tk.gettok_if(Token::kw_export).has_value();
	}
	return root;
}

/* module-definition ::=
 *		[ 'export' ] 'module' module-name ';'
 */
auto parse_module_def(tokenizer& tk) -> unique_ptr<Module> {
	auto root = std::make_unique<Module>();
	const auto tok = tk.gettok();
	switch (tok.type) {
	case Token::kw_export:
		root->_is_export = true;
		tk.expect(Token::kw_module);
		break;
	case Token::kw_module:
		break;
	default:
		throw unexpected(tok, "module");
	}
	const auto name = tk.expect(Token::identifier);
	root->_name._basename = name.str;
	tk.expect(Token::punct_semi);
	return root;
}

auto parse_extern_decl(tokenizer& tk, Scope& scope, const Declaration& outer)
    -> unique_ptr<Declaration> {
	const auto& context = outer._name._discrim;
	unique_ptr<Declaration> decl;
	string language = tk.gettok_if(Token::literal_string)
	                      .value_or(Token{Token::literal_string, outer.language})
	                      .str;
	if (tk.check({Token::kw_fn, Token::kw_proc})) {
		decl = parse_fn_decl(tk, scope, context);
		decl->language = language;
	} else if (tk.gettok_if(Token::punct_lbrace)) {
		auto alias = make_unique_for_modify<Namespace>(decl);
		alias->_name._discrim.scope = context.scope;
		alias->language = language;
		alias->_declarations = parse_ns_decl_seq(tk, scope, *alias);
		tk.expect(Token::punct_rbrace);
	}
	return decl;
}

auto parse_decl(tokenizer& tk, Scope& scope, const Declaration& outer)
    -> unique_ptr<Declaration> {
	const auto& context = outer._name._discrim;
	unique_ptr<Declaration> decl;
	switch (tk.peek().type) {
	case Token::kw_let: {
		decl = parse_var_decl(tk, scope, context);
	} break;
	case Token::kw_alias: {
		auto alias = make_unique_for_modify<AliasDecl>(decl);
		alias->_name._discrim.scope = context.scope;
		tk.ignore();
		// parse alias declaration
		alias->_name._basename = tk.expect(Token::identifier).str;
		tk.expect(Token::punct_equal);
		alias->_type = parse_expr(tk, scope, context);
	} break;
	case Token::kw_fn:
	case Token::kw_proc: {
		decl = parse_fn_decl(tk, scope, context);
		if (decl->language.empty()) {
			decl->language = outer.language;
		}
	} break;
	case Token::kw_enum: {
		decl = parse_enum_decl(tk, scope, context);
	} break;
	case Token::kw_struct: {
		decl = parse_struct_decl(tk, scope, context);
	} break;
	case Token::kw_trait: {
		decl = parse_trait_decl(tk, scope, context);
	} break;
	case Token::kw_extern: {
		decl = parse_extern_decl(tk, scope, outer);
	} break;
	case Token::kw_namespace: {
		decl = parse_namespace_block(tk, scope, outer);
	} break;
	case Token::kw_import: {
		throw unexpected(tk.peek(), "declaration");
	} break;
	default:
		throw unexpected(tk.peek(), "declaration");
	}
	return decl;
}

auto parse_initializer(tokenizer& tk, Scope& scope,
                       const Discriminators& context) -> unique_ptr<Node> {
	if (tk.gettok_if(Token::punct_equal)) {
		return parse_expr(tk, scope, context);
	} else {
		tk.expect(Token::punct_lbrace);
		auto list = std::make_unique<ExprList>();
		while (not tk.gettok_if(Token::punct_rbrace)) {
			list->_elems.push_back(parse_expr(tk, scope, context));
		}
		return list;
	}
}

auto parse_var_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<VarDecl> {
	tk.expect(Token::kw_let);
	auto decl = std::make_unique<VarDecl>();

	if (tk.gettok_if(Token::kw_mut)) {
		decl->_is_mut = true;
	} else if (tk.gettok_if(Token::kw_const)) {
		decl->_is_const = true;
	}
	decl->_name._basename = tk.expect(Token::identifier).str;
	if (tk.gettok_if(Token::punct_colon)) {
		decl->_type = parse_expr(tk, scope, context);
	}
	if (not tk.gettok_if(Token::punct_semi)) {
		decl->_initializer = parse_initializer(tk, scope, context);
		tk.expect(Token::punct_semi);
	}
	return decl;
}

auto parse_arg_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<ArgDecl> {
	auto decl = std::make_unique<ArgDecl>();
	bool is_reference{};
	if (tk.gettok_if(Token::op_bitand)) {
		is_reference = true;
	}
	if (tk.gettok_if(Token::kw_mut)) {
		decl->_is_mut = true;
	}
	if (tk.gettok_if(Token::kw_const)) {
		decl->_is_const = true;
	}
	if (decl->_is_mut and decl->_is_const) {
		throw 1;
	}
	if (tk.gettok_if(Token::punct_dollar)) {
		decl->_is_implicit = true;
		if (not decl->_is_const) {
			throw 1;
		}
	}
	decl->_name._basename = tk.expect(Token::identifier).str;
	if (tk.gettok_if(Token::punct_colon)) {
		decl->_type = parse_expr(tk, scope, context);
	}
	return decl;
}

auto parse_fn_args(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> vector<unique_ptr<ArgDecl>> {
	tk.expect(Token::punct_lparen);
	auto args = vector<unique_ptr<ArgDecl>>{};
	bool tail{};
	while (not tk.gettok_if(Token::punct_rparen)) {
		args.emplace_back(parse_arg_decl(tk, scope, context));
		// all implicit arguments must be at end
		if (tail and not args.back()->_is_implicit) {
			throw 1;
		}
		tail = args.back()->_is_implicit;
	}
	return args;
}

auto parse_fn_decl(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<Prototype> {
	auto decl = std::make_unique<Prototype>();
	auto local_scope = scope;
	if (tk.gettok_if(Token::kw_fn)) {
		decl->_is_proc = false;
	} else if (tk.expect(Token::kw_proc)) {
		decl->_is_proc = true;
	}
	if (tk.gettok_if(Token::op_bitand)) {
		decl->_signature->_captures.emplace<Signature::ref_tag_t>();
	} else if (tk.gettok_if(Token::punct_lbrace)) {
		throw 0;
	} else {
		decl->_name._basename = tk.expect(Token::identifier).str;
	}
	decl->_signature->_args = parse_fn_args(tk, scope, decl->_name._discrim);
	tk.expect(Token::punct_arrow);

	throw 0;
}

auto parse_control_expr(tokenizer& tk, Scope& scope,
                        const Discriminators& context) -> unique_ptr<Expr> {
	throw 0;
}
auto parse_assignment_expr(tokenizer& tk, Scope& scope,
                           const Discriminators& context) -> unique_ptr<Expr> {
	throw 0;
}
auto parse_block(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<Expr> {
	throw 0;
}

template <Token::Type begin, Token::Type end>
auto read_bracketed_expr(tokenizer& tk, Scope& scope,
                         const Discriminators& context,
                         std::vector<Token>& expr) -> void {
	tk.expect(begin);
	while (auto& tok = expr.emplace_back(tk.peek())) {
		if (tok.type == end) {
			return;
		}
		switch (tok.type) {
		case Token::punct_rparen:
		case Token::punct_rbrck:
		case Token::punct_rbrace:
		case Token::punct_substr_e:
			throw 1;
		case Token::punct_lparen:
			read_bracketed_expr<Token::punct_lparen, Token::punct_rparen>(
			    tk, scope, context, expr);
			break;
		case Token::punct_lbrck:
			read_bracketed_expr<Token::punct_lbrck, Token::punct_rbrck>(
			    tk, scope, context, expr);
			break;
		case Token::punct_attr:
			read_bracketed_expr<Token::punct_attr, Token::punct_rbrck>(
			    tk, scope, context, expr);
			break;
		case Token::punct_lbrace:
			read_bracketed_expr<Token::punct_lbrace, Token::punct_rbrace>(
			    tk, scope, context, expr);
			break;
		case Token::punct_substr_b:
			read_bracketed_expr<Token::punct_substr_b, Token::punct_substr_e>(
			    tk, scope, context, expr);
			break;
		default:
			tk.ignore();
		}
	}
}

auto parse_expr(tokenizer& tk, Scope& scope, const Discriminators& context)
    -> unique_ptr<Expr> {
	std::vector<Token> expr;

	while (auto& tok = expr.emplace_back(tk.peek())) {
		switch (tok.type) {
		case Token::punct_lparen:
			read_bracketed_expr<Token::punct_lparen, Token::punct_rparen>(
			    tk, scope, context, expr);
			break;
		case Token::punct_lbrck:
			read_bracketed_expr<Token::punct_lbrck, Token::punct_rbrck>(
			    tk, scope, context, expr);
			break;
		case Token::punct_attr:
			read_bracketed_expr<Token::punct_attr, Token::punct_rbrck>(
			    tk, scope, context, expr);
			break;
		case Token::punct_lbrace:
			read_bracketed_expr<Token::punct_lbrace, Token::punct_rbrace>(
			    tk, scope, context, expr);
			break;
		case Token::punct_substr_b:
			read_bracketed_expr<Token::punct_substr_b, Token::punct_substr_e>(
			    tk, scope, context, expr);
			break;
		default:
			break;
			// these tokens cannot be part of the expression:
		case Token::punct_semi:
		case Token::punct_comma:
		case Token::punct_rbrace:
		case Token::punct_rbrck:
		case Token::punct_rparen:
		case Token::punct_substr_e: {
			expr.pop_back();
			goto end_expr;
		}
		}
		tk.ignore();
	}
end_expr:
	if (expr.size() == 1) {
		switch (expr.front().type) {
		case Token::identifier: {
			auto name = std::make_unique<NameExpr>();
			name->_tok = expr.front();
			name->_basename = expr.front().str;
			return name;
		} break;
		case Token::literal_char: {
			auto name = std::make_unique<CharLiteral>();
			name->_tok = expr.front();
			name->_text = expr.front().str;
			return name;
		} break;
		case Token::literal_float: {
			auto name = std::make_unique<FloatLiteral>();
			name->_tok = expr.front();
			name->_text = expr.front().str;
			return name;
		} break;
		case Token::literal_int: {
			auto name = std::make_unique<IntegerLiteral>();
			name->_tok = expr.front();
			name->_text = expr.front().str;
			return name;
		} break;
		case Token::literal_string: {
			auto name = std::make_unique<StringLiteral>();
			name->_tok = expr.front();
			name->_text = expr.front().str;
			return name;
		} break;
		default:
			std::cout << "expression parser NYI\n";
			throw 0;
		}
	} else {
		std::cout << "expression parser NYI\n";
		throw 0;
	}
}

} // namespace AST
