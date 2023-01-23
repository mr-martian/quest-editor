/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"
#include <iomanip>
#include <ostream>
#include LEX_HEADER

namespace AST {

auto parse_attr(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	Name name;
	auto ret = make_unique<Attribute>(parse_qualified_name(tk, scope, name));
	ret->_attr = std::move(name);

	if (tk.gettok_if(Token::punct_lparen)) {
		ret->_args.emplace();
		while (auto arg = parse_expr(tk, scope,
		                             {Token::punct_comma, Token::punct_rparen})) {
			ret->_args->push_back(std::move(arg));
			if (tk.gettok_if(Token::punct_rparen)) {
				break;
			} else {
				tk.expect(Token::punct_comma);
			}
		}
	}

	return ret;
}

/* attribute-seq ::=
 *		  '#[' attribute	{ ',' attribute } ']'
 * attribute ::=
 *		qualified-name [ '(' attribute-argument-list ')' ]
 * attribute-argument-list ::=
 *		  expression
 *		| expression ',' attribute-argument-list
 */
auto parse_attr_seq(tokenizer& tk, Scope& scope,
                    std::initializer_list<Token::Type>)
    -> vector<unique_ptr<Node>> {
	if (tk.gettok_if(Token::punct_attr)) {
		vector<unique_ptr<Node>> ret;
		auto attr = parse_attr(tk, scope);
		do {
			ret.push_back(std::move(attr));
		} while (tk.gettok_if(Token::punct_comma)
		         and (attr = parse_attr(tk, scope)));
		tk.expect(Token::punct_rbrck);
		return ret;
	} else {
		return {};
	}
}

/* qualified-name ::=
 *		  [ '::' ] { namespace-id '::' } identifier [ '!' integer-literal ]
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

auto parse_tl_decl(tokenizer& tk, Scope& scope, const Declaration& context,
                   vector<unique_ptr<Node>>&& attrs) -> unique_ptr<Declaration>;
auto parse_var_decl(tokenizer& tk, Scope& scope,
                    vector<unique_ptr<Node>>&& attrs) -> unique_ptr<VarDecl>;
auto parse_fn_decl(tokenizer& tk, Scope& scope,
                   vector<unique_ptr<Node>>&& attrs) -> unique_ptr<Prototype>;
auto parse_enum_decl(tokenizer& tk, Scope& scope,
                     vector<unique_ptr<Node>>&& attrs) -> unique_ptr<EnumDecl> {
	throw 0;
}
auto parse_struct_decl(tokenizer& tk, Scope& scope,
                       vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<StructProto> {
	throw 0;
}
auto parse_trait_decl(tokenizer& tk, Scope& scope,
                      vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<TraitDecl> {
	throw 0;
}

auto parse_module_def(tokenizer& tk, vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Module>;

/* ns-decl-seq ::=
 * 	  { [ 'export' ] declaration }
 */
auto parse_ns_decl_seq(tokenizer& tk, Scope& scope, const Declaration& context)
    -> vector<unique_ptr<Declaration>> {
	vector<unique_ptr<Declaration>> decls;

	while (tk and not tk.check(Token::punct_rbrace) and not tk.eof()) {
		auto attrs = parse_attr_seq(tk, scope);
		bool is_export = tk.gettok_if(Token::kw_export).has_value();
		auto& decl = decls.emplace_back(
		    parse_tl_decl(tk, scope, context, std::move(attrs)));
		decl->_is_export = is_export;
	}
	return decls;
}

/* namespace-block ::=
 *		  'namespace' qualified-name '{' ns-decl-seq '}'
 */
auto parse_namespace_block(tokenizer& tk, Scope& parent,
                           const Declaration& outer,
                           vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Namespace> {
	const auto& context = outer._name._discrim;
	auto decl = std::make_unique<Namespace>(tk.expect(Token::kw_namespace));
	decl->_attributes = std::move(attrs);
	auto name = parse_qualified_name(tk, parent, decl->_name);
	decl->language = outer.language;
	if (name.type != Token::identifier) {
		throw unexpected(name, "qualified-name");
	}
	if (auto old_name = parent.find_name(decl->_name)) {
		if (not std::get_if<Namespace*>(&old_name->second.decl)) {
			// redefinition of name as different type of symbol
			throw 1;
		}
	} else {
		parent.add_name(decl.get());
	}
	auto& scope_name = decl->_name._discrim.scope;
	// scope.reserve(context.scope.size() + 1);
	scope_name = context.scope;
	tk.expect(Token::punct_lbrace);

	// the scope of the declarations in the namespace has to include the
	// namespace name. Doing it in-place saves memory
	scope_name.push_back(decl->_name._basename);
	decl->_declarations = parse_ns_decl_seq(tk, parent, *decl);
	// remove the redundant name from the scope
	scope_name.pop_back();

	tk.expect(Token::punct_rbrace);
	return decl;
}

/* module ::=
 *		  module-definition [ module-imports ] module-decl-seq
 * module-imports ::=
 *		  { [ 'export' ] 'import' module-name ';' }
 * module-decl-seq ::=
 *		  { [ 'export' ] declaration }
 */
auto parse_module(tokenizer& tk, Scope& scope) -> unique_ptr<Module> {
	unique_ptr<Module> root;
	auto attrs = parse_attr_seq(tk, scope);
	if (tk.peek().type == Token::kw_export
	    or tk.peek().type == Token::kw_module) {
		root = parse_module_def(tk, std::move(attrs));

		root->language = "Vellum";
	} else {
		throw unexpected(tk.gettok(), "module");
	}

	auto is_export = bool{};
	while (tk and not tk.eof()) {
		attrs = parse_attr_seq(tk, scope);
		is_export = tk.gettok_if(Token::kw_export).has_value();
		if (tk.gettok_if(Token::kw_import)) {
			do {
				auto name = tk.expect(Token::identifier);
				root->_imports.push_back({name.str, is_export, std::move(attrs)});
			} while (false and tk.gettok_if(Token::punct_comma));
			tk.expect(Token::punct_semi);
		} else {
			break;
		}
	}
	root->pretty_print(std::cout);
	try {
		while (tk and not tk.eof()) {
			auto& last = root->_declarations.emplace_back(
			    parse_tl_decl(tk, scope, *root, std::move(attrs)));
			last->_is_export = is_export;
			last->pretty_print(std::cout) << '\n';

			// this has to be done last so that it doesn't interfere with the
			// previous loop already setting it and then breaking
			attrs = parse_attr_seq(tk, scope);
			is_export = tk.gettok_if(Token::kw_export).has_value();
		}
	} catch (const unexpected& e) {
		std::cout << "Bailing:\n";
		std::cout << "Current scope: \n";
		scope.pretty_print(std::cout);
		std::cout << "AST:\n";
		root->pretty_print(std::cout) << std::flush;
		throw;
	} catch (int e) {
		if (e) {
			std::cout << "Bailing:\n";
		} else {
			std::cout << "Todo!:\n";
		}
		std::cout << "Current scope: \n";
		scope.pretty_print(std::cout);
		std::cout << "AST:\n";
		root->pretty_print(std::cout) << std::flush;
		throw;
	}

	return root;
}

/* module-definition ::=
 *		  [ 'export' ] 'module' module-name ';'
 */
auto parse_module_def(tokenizer& tk, vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Module> {
	auto root = unique_ptr<Module>();
	const auto tok = tk.gettok();
	switch (tok.type) {
	case Token::kw_export:
		// This uses raw new because it needs left-to-right evaluation of the
		// braced-init-list
		root.reset(new Module{tk.expect(Token::kw_module),
		                      tk.expect(Token::identifier).str, true});
		break;
	case Token::kw_module:
		root = std::make_unique<Module>(tk.cur(),
		                                tk.expect(Token::identifier).str, false);
		break;
	default:
		throw unexpected(tok, "module");
	}
	tk.expect(Token::punct_semi);
	root->_attributes = std::move(attrs);
	return root;
}

/*
 *
 */
auto parse_n_decl(tokenizer& tk, Scope& scope, vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Declaration> {
	switch (tk.peek().type) {
		unique_ptr<Declaration> decl;
		switch (tk.peek().type) {
		case Token::kw_let: {
			decl = parse_var_decl(tk, scope, std::move(attrs));
		} break;
		case Token::kw_alias: {
			auto alias_tok = tk.gettok();
			auto name = Name();
			parse_qualified_name(tk, scope, name);
			auto alias = make_unique_for_modify<AliasDecl>(
			    decl, std::move(alias_tok), std::move(name));
			alias->_attributes = std::move(attrs);
			// parse alias declaration
			tk.expect(Token::punct_equal);
			alias->_type = parse_expr(tk, scope);
			scope.add_name(alias);
		} break;
		case Token::kw_fn:
		case Token::kw_proc: {
			decl = parse_fn_decl(tk, scope, std::move(attrs));
		} break;
		case Token::kw_enum: {
			decl = parse_enum_decl(tk, scope, std::move(attrs));
		} break;
		case Token::kw_struct: {
			decl = parse_struct_decl(tk, scope, std::move(attrs));
		} break;
		case Token::kw_trait: {
			decl = parse_trait_decl(tk, scope, std::move(attrs));
		} break;
		case Token::kw_extern:
		case Token::kw_namespace: {
			throw 1;
		} break;
		case Token::kw_import: {
			throw unexpected(tk.peek(), "declaration");
		} break;
		default:
			throw unexpected(tk.peek(), "declaration");
		}
		return decl;
	}
}

/* extern-declaration ::=
 *		  'extern' [ string-literal ] fn-declaration
 *		| 'extern' [ string-literal ] '{' ns-decl-seq '}'
 */
auto parse_extern_decl(tokenizer& tk, Scope& scope, const Declaration& outer,
                       vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Declaration> {
	const auto& context = outer._name._discrim;
	auto extern_tok = tk.expect(Token::kw_extern);
	unique_ptr<Declaration> decl;
	string language
	    = tk.gettok_if(Token::literal_string)
	          .value_or(Token{Token::literal_string, outer.language, {}})
	          .str;
	if (tk.check({Token::kw_fn, Token::kw_proc})) {
		decl = parse_fn_decl(tk, scope, std::move(attrs));
		decl->language = language;
	} else if (tk.gettok_if(Token::punct_lbrace)) {
		auto alias = make_unique_for_modify<Namespace>(
		    decl, std::move(extern_tok), Name("", {context.scope}));
		decl->_attributes = std::move(attrs);
		alias->language = language;
		alias->_declarations = parse_ns_decl_seq(tk, scope, *alias);
		tk.expect(Token::punct_rbrace);
	}
	return decl;
}

/*
 *
 */
auto parse_tl_decl(tokenizer& tk, Scope& scope, const Declaration& outer,
                   vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<Declaration> {
	const auto& context = outer._name._discrim;
	unique_ptr<Declaration> decl;
	switch (tk.peek().type) {
	case Token::kw_let: {
		decl = parse_var_decl(tk, scope, std::move(attrs));
	} break;
	case Token::kw_alias: {
		auto alias = make_unique_for_modify<AliasDecl>(decl, tk.gettok(),
		                                               Name("", {context.scope}));
		alias->_attributes = std::move(attrs);
		// parse alias declaration
		alias->_name._basename = tk.expect(Token::identifier).str;
		tk.expect(Token::punct_equal);
		alias->_type = parse_expr(tk, scope);
		scope.add_name(alias);
	} break;
	case Token::kw_fn:
	case Token::kw_proc: {
		decl = parse_fn_decl(tk, scope, std::move(attrs));
		if (decl->language.empty()) {
			decl->language = outer.language;
		}
	} break;
	case Token::kw_enum: {
		decl = parse_enum_decl(tk, scope, std::move(attrs));
	} break;
	case Token::kw_struct: {
		decl = parse_struct_decl(tk, scope, std::move(attrs));
	} break;
	case Token::kw_trait: {
		decl = parse_trait_decl(tk, scope, std::move(attrs));
	} break;
	case Token::kw_extern: {
		decl = parse_extern_decl(tk, scope, outer, std::move(attrs));
	} break;
	case Token::kw_namespace: {
		decl = parse_namespace_block(tk, scope, outer, std::move(attrs));
	} break;
	case Token::kw_import: {
		throw unexpected(tk.peek(), "declaration");
	} break;
	default:
		throw unexpected(tk.peek(), "declaration");
	}
	return decl;
}

/*
 *
 */
auto parse_initializer(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	if (tk.gettok_if(Token::punct_equal)) {
		return parse_expr(tk, scope);
	} else {
		auto list = std::make_unique<ExprList>(tk.expect(Token::punct_lbrace));
		while (not tk.gettok_if(Token::punct_rbrace)) {
			list->_elems.push_back(parse_expr(tk, scope));
		}
		return list;
	}
}

/* var-definition ::=
 * 	  ['&'] ['mut' | 'const'] unqual-var-def
 * unqual-var-def ::=
 *		  var-name [':' type-constraint]
 */
auto parse_var_def(tokenizer& tk, Scope& scope,
                   vector<unique_ptr<Node>>&& attrs) -> unique_ptr<VarDecl> {
	bool is_reference{};
	bool is_mut{};
	bool is_const{};
	if (tk.gettok_if(Token::op_bitand)) {
		is_reference = true;
	}
	if (tk.gettok_if(Token::kw_mut)) {
		is_mut = true;
	}
	if (tk.gettok_if(Token::kw_const)) {
		is_const = true;
	}
	// catch 'mut const' or 'const mut'
	if (is_const and (is_mut or tk.check(Token::kw_mut))) {
		throw 1;
	}
	tk.expect(Token::identifier);
	auto decl = std::make_unique<VarDecl>(tk.cur(),
	                                      Name{tk.cur().str, Discriminators{}},
	                                      is_mut, is_const, is_reference, false);
	decl->_attributes = std::move(attrs);
	if (tk.gettok_if(Token::punct_colon)) {
		decl->_type
		    = parse_expr(tk, scope, {Token::punct_comma, Token::punct_equal});
	}
	return decl;
}

/* var-declaration ::=
 *		  'let' [ 'const' ] unqual-var-def var-initializer ';'
 *		| 'let' 'mut' unqual-var-def [ var-initializer ] ';'
 */
auto parse_var_decl(tokenizer& tk, Scope& scope,
                    vector<unique_ptr<Node>>&& attrs) -> unique_ptr<VarDecl> {
	tk.expect(Token::kw_let);
	auto decl = parse_var_def(tk, scope, std::move(attrs));

	if (auto tok = tk.gettok_if(Token::punct_semi); not tok) {
		decl->_initializer = parse_initializer(tk, scope);
		tk.expect(Token::punct_semi);
	} else if (not decl->_is_mut) {
		throw unexpected(*tok, "initializer for non-mut variable");
	}
	scope.add_name(decl.get());
	return decl;
}

/* arg-definition	::=
 *		  ['&'] ['mut' | 'const'] arg-name [':' arg-type-constraint] [default-arg]
 * default-arg ::=
 *		  equal-initializer
 * arg-name ::=
 *		  identifier
 *		| '$' identifier
 */
auto parse_arg_decl(tokenizer& tk, Scope& scope) -> unique_ptr<ArgDecl> {
	// auto decl = std::make_unique<ArgDecl>();
	auto attrs = parse_attr_seq(tk, scope);
	bool is_reference{};
	bool is_mut{};
	bool is_const{};
	bool is_implicit{};
	if (tk.gettok_if(Token::op_bitand)) {
		is_reference = true;
	}
	if (tk.gettok_if(Token::kw_mut)) {
		is_mut = true;
	}
	if (tk.gettok_if(Token::kw_const)) {
		is_const = true;
	}
	if (is_mut and is_const) {
		throw 1;
	}
	if (tk.gettok_if(Token::punct_dollar)) {
		is_implicit = true;
		if (not is_const) {
			throw 1;
		}
	}
	tk.expect(Token::identifier);
	auto decl = std::make_unique<ArgDecl>(
	    tk.cur(), Name{tk.cur().str, Discriminators{}}, is_mut, is_const,
	    is_reference, is_implicit, false, false);
	decl->_attributes = std::move(attrs);
	if (tk.gettok_if(Token::punct_colon)) {
		decl->_type = parse_expr(tk, scope, {Token::punct_comma});
	}
	if (tk.check(Token::punct_equal)) {
		decl->_initializer = parse_initializer(tk, scope);
	}
	scope.add_name(decl.get());
	return decl;
}

/* fn-signature ::=
 *		  '(' argument-list ')' [ '->' type-constraint ]
 *		| '(' argument-list '->' type-constraint { ',' type-constraint } ')'
 * argument-list ::=
 *		  '(' {arg-definition} ')'
 * return-spec    ::= '->' type-constraint
 */
auto parse_argument_list(tokenizer& tk, Scope& scope)
    -> vector<unique_ptr<ArgDecl>> {
	auto args = vector<unique_ptr<ArgDecl>>{};
	bool tail{};
	bool first{true};
	while (not tk.check({Token::punct_rparen, Token::punct_arrow})) {
		if (not std::exchange(first, false)) {
			tk.expect(Token::punct_comma);
		}
		args.emplace_back(parse_arg_decl(tk, scope));
		// all implicit arguments must be at end
		if (tail and not args.back()->_is_implicit) {
			throw 1;
		}
		tail = args.back()->_is_implicit;
	}
	return args;
}
/* fn-signature ::=
 *		  '(' argument-list ')' [ '->' type-constraint ]
 *		| '(' argument-list '->' type-constraint { ',' type-constraint } ')'
 * capture-desc ::=
 *		  ε
 *		| '&'
 *		| '{' ? '}'
 */
auto parse_signature(tokenizer& tk, Scope& scope) -> unique_ptr<Signature> {
	auto decl = std::make_unique<Signature>(
	    tk.expect({Token::punct_lparen, Token::op_bitand, Token::punct_lbrace}));
	auto new_scope = scope;
	if (tk.was(Token::op_bitand)) {
		decl->_captures.emplace<Signature::ref_tag_t>();
	} else if (tk.was(Token::punct_lbrace)) {
		throw 0;
	}
	decl->_args = parse_argument_list(tk, new_scope);
	if (tk.gettok_if(Token::punct_rparen)) {
		if (tk.gettok_if(Token::punct_arrow)) {
			decl->_returns.emplace_back(parse_expr(tk, new_scope));
		}
	} else {
		tk.expect(Token::punct_arrow);
		while (not tk.gettok_if(Token::punct_rparen)) {
			decl->_returns.push_back(parse_expr(tk, new_scope));
		}
	}
	return decl;
}

auto parse_substrate_block(tokenizer& tk, Scope& scope,
                           vector<unique_ptr<Node>>&& attrs)
    -> unique_ptr<SubstrateNode>;

/* fn-declaration ::=
 * 	  prototype fn-body
 * 	| prototype ';'
 * prototype ::=
 *		  fn-prologue fn-name fn-signature
 * anon-prototype ::=
 *		  fn-prologue fn-signature
 * fn-prologue ::=
 *		  ('fn' | 'proc')
 * fn-body ::=
 *		  block
 *		| substrate-block
 *		| '=' expression ';'
 *		| '=' 'delete' [reason-expr] ';'
 */
auto parse_fn_decl(tokenizer& tk, Scope& scope,
                   vector<unique_ptr<Node>>&& attrs) -> unique_ptr<Prototype> {
	auto decl = std::make_unique<FunctionDef>(
	    tk.expect({Token::kw_fn, Token::kw_proc}));
	decl->_attributes = std::move(attrs);
	if (tk.cur().type == Token::kw_fn) {
		decl->_is_proc = false;
	} else if (tk.cur().type == Token::kw_proc) {
		decl->_is_proc = true;
	}
	parse_qualified_name(tk, scope, decl->_name);
	// reparent name to scope
	{}
	if (auto old_decl = scope.find_name(decl->_name)) {
		if (auto p_old_func = std::get_if<FunctionDef*>(&old_decl->second.decl);
		    p_old_func and *p_old_func) {
			if ((*p_old_func)->_body) {
				throw 1;
			}
		} else if (auto p_old_func
		           = std::get_if<OverloadSet>(&old_decl->second.decl)) {
			;
		} else {
			throw 0;
		}
	}

	auto local_scope = scope;
	local_scope.name._basename = "__fn_local";
	auto t_name = local_scope.add_name(decl.get());
	decl->_signature = parse_signature(tk, local_scope);

	if (auto& a = decl->_name._discrim.args;
	    a && *a != decl->_signature->_args.size()) {
		throw 1;
	} else if (not a) {
		a = decl->_signature->_args.size();
		local_scope.refine_name(t_name, decl.get());
	} else {
		assert(*a == decl->_signature->_args.size());
	}

	scope.add_name(decl.get());

	if (not tk.gettok_if(Token::punct_semi)) {
		switch (tk.peek().type) {
		case Token::punct_lbrace: {
			decl->_body = parse_block(tk, scope);
		} break;
		case Token::kw_substrate:
		case Token::punct_substr_b: {
			decl->_body = parse_expr(tk, local_scope);
		} break;
		case Token::punct_equal: {
			if (tk.check(Token::kw_delete)) {
				throw 0;
			} else {
				tk.ignore();
				decl->_body = parse_expr(tk, local_scope);
				tk.expect(Token::punct_semi);
			}
		} break;
		default:
			throw unexpected(tk.gettok(), "function body or ';'");
		}
	}
	return decl;
}

} // namespace AST
