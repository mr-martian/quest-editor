/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {
// TODO: replace with extensible runtime system
enum precedences {
	Expr_End = -1,
	Assignment_Expr_R = 1,
	Assignment_Expr_L,
	Exit_expr,
	Loop_Expr,
	If_Expr_R,
	If_Expr_L,
	Match_Expr,
	If_Expr_Inverted_R,
	If_Expr_Inverted_L,
	Expr_List,
	Cond_Expr_R,
	Cond_Expr_L,
	Rewrite_Expr_R,
	Rewrite_Expr_L,
	Null_Coalescing_Expr_R,
	Null_Coalescing_Expr_L,
	Or_Expr,
	And_Expr,
	Comparison_Expr,
	Not_Expr,
	Bit_Or_Expr,
	Bit_And_Expr,
	Bitshift_Expr,
	Add_Expr,
	Mul_Expr,
	Prefix_Expr,
	Postfix_Expr,
	Primary_Expr,
	Dot_Expr,
	Atom_Expr,
};

std::unordered_map<Token::Type, std::pair<Direction, Direction>>
    Operator::op_traits{
        {},
    };

Operator::Operator(Token t)
    : Name(tok_name(t), Discriminators{}) {
	std::tie(_assoc, _eval_order) = op_traits[t.type];
}

auto parse_literal(tokenizer& tk, Scope&, std::initializer_list<Token::Type>)
    -> unique_ptr<Node> {
	switch (tk.gettok().type) {
	case Token::literal_char:
		return std::make_unique<CharLiteral>(tk.cur());
	case Token::literal_float:
		return std::make_unique<FloatLiteral>(tk.cur());
	case Token::literal_int:
		return std::make_unique<IntegerLiteral>(tk.cur());
	case Token::literal_string:
		return std::make_unique<StringLiteral>(tk.cur());
	default:
		throw 1;
	}
}

auto parse_name_expr(tokenizer& tk, Scope& scope,
                     std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto name = Name();
	return make_unique<NameExpr>(parse_qualified_name(tk, scope, name),
	                             std::move(name));
}

auto parse_typeid(tokenizer& tk, Scope& scope,
                  std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	switch (tk.gettok().type) {
	case Token::kw_Bool:
		return std::make_unique<BoolTypeID>(tk.cur());
	case Token::kw_Byte:
		return std::make_unique<ByteTypeID>(tk.cur());
	case Token::kw_Fail:
		return std::make_unique<FailTypeID>(tk.cur());
	case Token::kw_Float32:
		return std::make_unique<FloatTypeID>(tk.cur(), FloatTypeID::Float32);
	case Token::kw_Float64:
		return std::make_unique<FloatTypeID>(tk.cur(), FloatTypeID::Float64);
	case Token::kw_None:
		return std::make_unique<NoneExpr>(tk.cur());
	case Token::kw_Noreturn:
		return std::make_unique<NoreturnTypeID>(tk.cur());
	case Token::kw_This:
		return std::make_unique<ThisTypeID>(tk.cur());
	case Token::kw_Type:
		return std::make_unique<TypeTypeID>(tk.cur());
	case Token::id_int:
		return std::make_unique<IntegerTypeID>(tk.cur());
	case Token::id_unsigned:
		return std::make_unique<UnsignedTypeID>(tk.cur());
	default:
		throw 1;
	}
}

template <int bp>
auto parse_prefix_expr(tokenizer& tk, Scope& scope,
                       std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto ret = std::make_unique<PrefixExpr>(tk.gettok());
	ret->_operand = parse_expr(tk, scope, {}, bp);
	return ret;
}

template <int bp_r>
auto parse_postfix_expr(tokenizer& tk, Scope& scope,
                        std::initializer_list<Token::Type> end_tok_types,
                        unique_ptr<Node> left) -> unique_ptr<Node> {
	auto ret = make_unique<PostfixExpr>(tk.gettok());
	ret->_operand = std::move(left);
	return ret;
}

template <int bp_r>
auto parse_binary_expr(tokenizer& tk, Scope& scope,
                       std::initializer_list<Token::Type> end_tok_types,
                       unique_ptr<Node> left) -> unique_ptr<Node> {
	auto ret = make_unique<BinaryExpr>(tk.gettok());
	ret->_left = std::move(left);
	ret->_right = parse_expr(tk, scope, {}, bp_r);
	return ret;
}

auto parse_ref_expr(tokenizer& tk, Scope& scope,
                    std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto ret = std::make_unique<RefExpr>(tk.gettok());
	if (tk.gettok_if(Token::kw_mut)) {
		ret->_is_mut = true;
	}
	ret->_operand = parse_expr(tk, scope, {}, Prefix_Expr);
	return ret;
}

auto parse_label_use(tokenizer& tk, Scope& scope) -> optional<Token> {
	if (tk.gettok_if(Token::punct_colon)) {
		return tk.expect(
		    {
		        Token::identifier,
		        Token::kw_for,
		        Token::kw_while,
		        Token::kw_do,
		        Token::kw_loop,
		        Token::kw_fn,
		        Token::kw_proc,
		        Token::kw_match,
		    },
		    "label");
	} else {
		return {};
	}
}
auto parse_label_decl(tokenizer& tk, Scope& scope) -> optional<string> {
	if (tk.gettok_if(Token::punct_colon)) {
		return tk.expect(Token::identifier).str;
	} else {
		return {};
	}
}

/* condition ::=
 *		   expression
 * 	| 'let' var-definition equal-initializer
 */
auto parse_condition(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	if (tk.check(Token::kw_let)) {
		// this probably expects a ';' and won't work
		return parse_var_def(tk, scope, {});
	} else {
		return parse_expr(tk, scope, {Token::punct_lbrace});
	}
}

auto parse_loop_constraint(tokenizer& tk, Scope& scope)
    -> unique_ptr<LoopConstraint> {
	auto constraint = make_unique<LoopConstraint>(
	    tk.expect({Token::kw_while, Token::kw_until}));
	constraint->_condition = parse_expr(tk, scope);
	return constraint;
}
auto parse_loop_constraint(tokenizer& tk, Scope& scope, Token&& t)
    -> unique_ptr<LoopConstraint> {
	assert(t.type == Token::kw_while or t.type == Token::kw_until);
	auto constraint = make_unique<LoopConstraint>(std::move(t));
	constraint->_condition = parse_expr(tk, scope);
	return constraint;
}
template <precedences P = Loop_Expr>
auto parse_else_expr(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	if (tk.gettok_if(Token::kw_else)) {
		return parse_expr(tk, scope, {}, P);
	} else {
		return nullptr;
	}
}
auto parse_else_block(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	if (tk.gettok_if(Token::kw_else)) {
		return parse_block(tk, scope);
	} else {
		return nullptr;
	}
}

auto parse_for_loop(tokenizer& tk, Scope& scope) -> unique_ptr<ForLoopExpr> {
	auto for_tk = tk.gettok();
	auto label = parse_label_decl(tk, scope);
	auto induction_var = unique_ptr<VarDecl>();
	if (not tk.gettok_if(Token::kw_underscore)) {
		induction_var = parse_var_def(tk, scope, parse_attr_seq(tk, scope));
	}

	if (tk.gettok_if(Token::kw_in)) {
		auto expr = make_unique<ForInExpr>(std::move(for_tk));
		expr->_label = std::move(label);
		expr->_induction_variable = std::move(induction_var);

		expr->_range_expr = parse_expr(tk, scope, {Token::punct_lbrace});
		expr->_body = parse_block(tk, scope);
		expr->_else = parse_else_expr(tk, scope);
		return expr;
	}

	auto init = unique_ptr<Node>();
	if (tk.check(Token::punct_equal)) {
		assert(induction_var);
		init = parse_initializer(tk, scope);
	}

	if (tk.check({Token::kw_while, Token::kw_until})) {
		auto expr = make_unique<GForExpr>(std::move(for_tk));
		expr->_label = std::move(label);
		if ((expr->_induction_variable = std::move(induction_var))) {
			expr->_induction_variable->_initializer = std::move(init);
		}
		expr->_constraint = parse_loop_constraint(tk, scope);
		if (tk.gettok_if(Token::punct_semi)) {
			expr->_update = parse_expr(tk, scope, {Token::punct_lbrace});
		}
		expr->_body = parse_block(tk, scope);
		expr->_else = parse_else_expr(tk, scope);
		return expr;
	}

	auto expr = make_unique<ForRangeExpr>(std::move(for_tk));
	expr->_label = std::move(label);
	expr->_induction_variable = std::move(induction_var);
	expr->_induction_variable->_initializer = std::move(init);
	if (tk.gettok_if(Token::punct_arrow)) {
		expr->_r_end = parse_expr(tk, scope, {Token::punct_lbrace});
	}
	if (tk.gettok_if(Token::punct_semi)) {
		expr->_update = parse_expr(tk, scope, {Token::punct_lbrace});
	}

	expr->_body = parse_block(tk, scope);
	expr->_else = parse_else_expr(tk, scope);
	return expr;
}

auto parse_loop_expr(tokenizer& tk, Scope& scope) -> unique_ptr<Node> {
	switch (tk.peek().type) {
	case Token::kw_loop: {
		auto expr = make_unique<LoopExpr>(tk.gettok());
		expr->_label = parse_label_decl(tk, scope);
		expr->_body = parse_block(tk, scope);
		return expr;
	} break;
	case Token::kw_for: {
		return parse_for_loop(tk, scope);
	} break;
	case Token::kw_while:
	case Token::kw_until: {
		auto expr = make_unique<WhileExpr>(tk.gettok());
		expr->_label = parse_label_decl(tk, scope);
		expr->_constraint = parse_loop_constraint(tk, scope, tk.cur());
		expr->_body = parse_block(tk, scope);
		expr->_else = parse_else_expr(tk, scope);
		return expr;
	} break;
	case Token::kw_do: {
		auto expr = make_unique<DoWhileExpr>(tk.gettok());
		expr->_label = parse_label_decl(tk, scope);
		expr->_body = parse_block(tk, scope);
		expr->_constraint = parse_loop_constraint(tk, scope);
		expr->_else = parse_else_expr(tk, scope);
		return expr;
	} break;
	default:
		throw 1;
	}

	throw 0;
}
/* if-expression ::=
 *		  ('if' | 'unless') condition block ['else' (block | if-expression)]
 */
auto parse_if_expr1(tokenizer& tk, Scope& scope,
                    std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto kw = tk.expect({Token::kw_if, Token::kw_unless});
	auto expected = kw.type == Token::kw_if;
	auto node = make_unique<IfExpression>(std::move(kw));
	node->_target = expected;
	node->_condition = parse_condition(tk, scope);
	node->_body = parse_block(tk, scope);
	node->_else_body = parse_else_block(tk, scope);
	return node;
}
/* inverted-if-expression ::=
 *		  expression 'if' expression ['else' expression]
 */
auto parse_if_expr2(tokenizer& tk, Scope& scope,
                    std::initializer_list<Token::Type> end_tok_types,
                    unique_ptr<Node> left) -> unique_ptr<Node> {
	auto kw = tk.expect({Token::kw_if, Token::kw_unless});
	auto expected = kw.type == Token::kw_if;
	auto node = make_unique<InvertedIfExpr>(std::move(kw));
	node->_target = expected;
	node->_condition = parse_condition(tk, scope);
	node->_expr = parse_block(tk, scope);
	node->_else_expr = parse_else_expr<If_Expr_Inverted_R>(tk, scope);
	return node;
}
/* conditional-expression ::=
 *		  expression '?' expression ':' expression
 */
auto parse_cond_expr(tokenizer& tk, Scope& scope,
                     std::initializer_list<Token::Type> end_tok_types,
                     unique_ptr<Node> left) -> unique_ptr<Node> {
	throw 0;
}
auto parse_block(tokenizer& tk, Scope& scope) -> unique_ptr<Block> {
	auto list = std::make_unique<Block>(tk.expect(Token::punct_lbrace));
	while (not tk.gettok_if(Token::punct_rbrace)) {
		if (auto rb = tk.gettok_if(Token::punct_rbrace)) {
			list->_expressions.push_back(make_unique<NoneExpr>(std::move(*rb)));
			break;
		}
		auto attrs = parse_attr_seq(tk, scope);
		if (tk.peek().type == Token::kw_let) {
			list->_expressions.push_back(
			    parse_var_decl(tk, scope, std::move(attrs)));
		} else if (tk.peek().type == Token::kw_fn
		           or tk.peek().type == Token::kw_proc) {
			list->_expressions.push_back(
			    parse_fn_decl(tk, scope, std::move(attrs)));
		} else {
			list->_expressions.push_back(
			    parse_expr(tk, scope, {Token::punct_rbrace}));
			list->_expressions.back()->_attributes = std::move(attrs);
			if (tk.gettok_if(Token::punct_rbrace)) {
				break;
			} else {
				tk.expect(Token::punct_semi);
			}
		}
	}
	return list;
}

auto parse_parenthesized_expr(tokenizer& tk, Scope& scope,
                              std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto l = tk.expect(Token::punct_lparen);
	if (not tk.gettok_if(Token::punct_rparen)) {
		auto expr = parse_expr(tk, scope, {Token::punct_rparen});
		tk.expect(Token::punct_rparen);
		return expr;
	} else {
		return make_unique<NullExpr>(std::move(l));
	}
}
template <Token::Type end>
auto parse_call_expr(tokenizer& tk, Scope& scope,
                     std::initializer_list<Token::Type> end_tok_types,
                     unique_ptr<Node> left) -> unique_ptr<Node> {
	auto ret = std::make_unique<CallExpr>(tk.gettok());
	ret->_fun = std::move(left);
	if (not tk.gettok_if(end)) {
		ret->_args.push_back(parse_expr(tk, scope, {Token::punct_comma}));
		while (tk.gettok_if(Token::punct_comma)) {
			ret->_args.push_back(parse_expr(tk, scope, {Token::punct_comma}));
		}
		tk.expect(end);
	}
	return ret;
}

auto parse_array_expr(tokenizer& tk, Scope& scope,
                      std::initializer_list<Token::Type> end_tok_types)
    -> unique_ptr<Node> {
	auto ret = std::make_unique<ArrayPrefixExpr>(tk.gettok());

	while (not tk.gettok_if(Token::punct_rbrck)) {
		auto attr = parse_attr_seq(tk, scope);
		if (auto comma = tk.gettok_if(Token::punct_comma)) {
			if (not attr.empty()) {
				ret->_args.push_back(
				    make_unique<NullExpr>(std::move(*comma), std::move(attr)));
			}
		} else {
			auto expr = parse_expr(tk, scope, {Token::punct_comma}, Atom_Expr);
			expr->_attributes = std::move(attr);
			assert(expr);
			ret->_args.push_back(std::move(expr));
		}
	}

	ret->_rhs = parse_expr(tk, scope, end_tok_types);

	return ret;
}

template <Token::Type begin, Token::Type end>
auto read_bracketed_expr(tokenizer& tk, Scope& scope,
                         std::initializer_list<Token::Type> end_tok_types,
                         std::vector<Token>& expr) -> void {
	tk.expect(begin);
	if (tk.gettok_if(end)) {
		return;
	}
	while (auto& tok = expr.emplace_back(tk.peek())) {
		switch (tok.type) {
		case Token::punct_rparen:
		case Token::punct_rbrck:
		case Token::punct_rbrace:
		case Token::punct_substr_e:
			throw 1;
		case Token::punct_lparen:
			read_bracketed_expr<Token::punct_lparen, Token::punct_rparen>(
			    tk, scope, end_tok_types, expr);
			break;
		case Token::punct_lbrck:
			read_bracketed_expr<Token::punct_lbrck, Token::punct_rbrck>(
			    tk, scope, end_tok_types, expr);
			break;
		case Token::punct_attr:
			read_bracketed_expr<Token::punct_attr, Token::punct_rbrck>(
			    tk, scope, end_tok_types, expr);
			break;
		case Token::punct_lbrace:
			read_bracketed_expr<Token::punct_lbrace, Token::punct_rbrace>(
			    tk, scope, end_tok_types, expr);
			break;
		case Token::punct_substr_b:
			read_bracketed_expr<Token::punct_substr_b, Token::punct_substr_e>(
			    tk, scope, end_tok_types, expr);
			break;
		default:
			tk.ignore();
		}
	}
}

/* Parses an unparenthesized expression (which may consist of a single
 * parenthesized expression)
 *
 *
auto parse_expr0(tokenizer& tk, Scope& scope) -> unique_ptr<Expr> {
   std::vector<Token> expr;

   while (auto& tok = expr.emplace_back(tk.peek())) {
      switch (tok.type) {
      case Token::punct_lparen:
         read_bracketed_expr<Token::punct_lparen, Token::punct_rparen>(
             tk, scope, expr);
         break;
      case Token::punct_lbrck:
         read_bracketed_expr<Token::punct_lbrck, Token::punct_rbrck>(tk, scope,
                                                                     expr);
         break;
      case Token::punct_attr:
         read_bracketed_expr<Token::punct_attr, Token::punct_rbrck>(tk, scope,
                                                                    expr);
         break;
      case Token::punct_lbrace:
         read_bracketed_expr<Token::punct_lbrace, Token::punct_rbrace>(
             tk, scope, expr);
         break;
      case Token::punct_substr_b:
         read_bracketed_expr<Token::punct_substr_b, Token::punct_substr_e>(
             tk, scope, expr);
         break;
      default:
         break;
         // these tokens cannot be part of the expression:
      case Token::punct_semi:
      case Token::punct_comma:
      case Token::punct_colon:
      case Token::punct_arrow:
      case Token::punct_equal:
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
   if (expr.empty()) {
      throw unexpected(tk.gettok(), "expression");
   } else if (expr.size() == 1) {
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
      case Token::kw_Bool: {
         auto name = std::make_unique<BoolTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_Byte: {
         auto name = std::make_unique<ByteTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_Fail: {
         auto name = std::make_unique<FailTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_Float32: {
         auto name = std::make_unique<FloatTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         name->_width = FloatTypeID::Float32;
         return name;
      } break;
      case Token::kw_Float64: {
         auto name = std::make_unique<FloatTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         name->_width = FloatTypeID::Float64;
         return name;
      } break;
      case Token::kw_None: {
         auto name = std::make_unique<NoneTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_Noreturn: {
         auto name = std::make_unique<NoreturnTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_This: {
         auto name = std::make_unique<ThisTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::kw_Type: {
         auto name = std::make_unique<TypeTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         return name;
      } break;
      case Token::id_int: {
         auto name = std::make_unique<IntegerTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         name->_width = kblib::fromStr<Integer>(
             std::string_view{expr.front().str}.substr(1), "Integer");
         return name;
      } break;
      case Token::id_unsigned: {
         auto name = std::make_unique<UnsignedTypeID>();
         name->_tok = expr.front();
         name->_name = expr.front().str;
         name->_width = kblib::fromStr<Integer>(
             std::string_view{expr.front().str}.substr(1), "Integer");
         return name;
      } break;
      default:
         std::cout << "expression parser NYI\n";
         std::cout << "Unknown expression category: " << tok_name(expr.front())
                   << '\n';
         throw 0;
      }
   } else {
      std::cout << "expression parser NYI\n";
      std::cout << "Expression tokens: [";
      for (auto& tok : expr) {
         std::cout << tok_name(tok) << ", ";
      }
      std::cout << "]\n";
      throw 0;
   }
}//*/

template <auto F>
auto drop_stop_tokens(tokenizer& tk, Scope& scope,
                      std::initializer_list<Token::Type>) -> unique_ptr<Node> {
	if constexpr (requires { F(tk, scope); }) {
		return F(tk, scope);
	} else {
		static_assert(requires { F(tk, scope, {}); });
		return F(tk, scope, {});
	}
}
template <auto F>
auto drop_stop_tokens(tokenizer& tk, Scope& scope,
                      std::initializer_list<Token::Type>, unique_ptr<Node> left)
    -> unique_ptr<Node> {
	if constexpr (requires { F(tk, scope, std::move(left)); }) {
		return F(tk, scope, std::move(left));
	} else {
		static_assert(requires { F(tk, scope, std::move(left), {}); });
		return F(tk, scope, std::move(left), {});
	}
}

void Scope::default_ops() {
	// Atoms:
	reg1({Token::identifier, Token::punct_scope}, {&parse_name_expr, Atom_Expr});
	reg1({Token::kw_Bool, Token::kw_Byte, Token::kw_Fail, Token::kw_Float32,
	      Token::kw_Float64, Token::kw_None, Token::kw_Noreturn, Token::kw_This,
	      Token::id_int, Token::id_unsigned},
	     {&parse_typeid, Atom_Expr});
	reg1({Token::literal_char, Token::literal_float, Token::literal_int,
	      Token::literal_string},
	     {&parse_literal, Atom_Expr});

	// Prefix operators (tight binding)
	reg1({Token::op_plus, Token::op_minus, Token::op_compl, Token::op_not,
	      Token::op_at, Token::op_hash, Token::op_times, Token::op_qm},
	     {&parse_prefix_expr<Prefix_Expr>, Prefix_Expr});

	reg1(Token::op_bitand, {&parse_ref_expr, Prefix_Expr});

	reg1(Token::op_not, {&parse_prefix_expr<Not_Expr>, Not_Expr});

	// Prefix operators (technical)
	// Labels
	reg1(Token::punct_colon, {&parse_name_expr, Atom_Expr});

	// Bracketing operators
	reg1(Token::punct_lparen, {&parse_parenthesized_expr, Atom_Expr});
	reg1(Token::punct_lbrace, {&drop_stop_tokens<parse_block>, Atom_Expr});

	// Prefix operators (loose binding)
	reg1({Token::kw_await, Token::kw_break, Token::kw_continue, Token::kw_result,
	      Token::kw_return, Token::kw_yield},
	     {&parse_prefix_expr<0>, Exit_expr});

	reg1({Token::kw_if, Token::kw_unless}, {&parse_if_expr1, If_Expr_L});
	reg2({Token::kw_if, Token::kw_unless}, {&parse_if_expr2, If_Expr_R});
	reg2({Token::op_orelse},
	     {&parse_binary_expr<Cond_Expr_R>, Cond_Expr_L, Cond_Expr_R});
	reg2({Token::op_qm}, {&parse_cond_expr, Cond_Expr_L, Cond_Expr_R});

	reg1({Token::kw_loop, Token::kw_while, Token::kw_for, Token::kw_do,
	      Token::kw_until},
	     {&drop_stop_tokens<parse_loop_expr>, Loop_Expr});

	// postfix operators
	reg2({Token::op_carat, Token::op_dots},
	     {&parse_postfix_expr<Postfix_Expr>, Postfix_Expr});

	// infix operators
	// {Tokens}, {&parse_binary_expr<bp_r>, bp_l}
	reg2(Token::op_dot, {&parse_binary_expr<Dot_Expr>, Dot_Expr});
	reg2({Token::op_times, Token::op_div, Token::op_mod, Token::op_rem},
	     {&parse_binary_expr<Mul_Expr>, Mul_Expr});
	reg2({Token::op_plus, Token::op_minus, Token::op_qplus, Token::op_qminus,
	      Token::op_wplus, Token::op_wminus},
	     {&parse_binary_expr<Add_Expr>, Add_Expr});
	reg2({Token::op_lshift, Token::op_rshift},
	     {&parse_binary_expr<Bitshift_Expr>, Bitshift_Expr});
	reg2(Token::op_bitand, {&parse_binary_expr<Bit_And_Expr>, Bit_And_Expr});
	reg2({Token::op_bitor, Token::op_xor},
	     {&parse_binary_expr<Bit_Or_Expr>, Bit_Or_Expr});
	reg2({Token::op_equal, Token::op_unequal, Token::op_cmp, Token::op_less,
	      Token::op_lte, Token::op_greater, Token::op_gte},
	     {&parse_binary_expr<Comparison_Expr>, Comparison_Expr});
	reg2({Token::op_and}, {&parse_binary_expr<And_Expr>, And_Expr});
	reg2({Token::op_or}, {&parse_binary_expr<Or_Expr>, Or_Expr});
	reg2({Token::op_qmqm}, {&parse_binary_expr<Null_Coalescing_Expr_R>,
	                        Null_Coalescing_Expr_L, Null_Coalescing_Expr_R});
	reg2({Token::op_pipe},
	     {&parse_binary_expr<Rewrite_Expr_R>, Rewrite_Expr_L, Rewrite_Expr_R});

	// infix match?
	reg2(Token::kw_match, {nullptr, Match_Expr});

	reg2(Token::punct_comma, {nullptr, Expr_List});

	//
	reg2(Token::punct_lparen,
	     {&parse_call_expr<Token::punct_rparen>, Postfix_Expr});
	reg2(Token::punct_lbrck,
	     {&parse_call_expr<Token::punct_rbrck>, Postfix_Expr});
	reg1(Token::punct_lbrck, {&parse_array_expr, Postfix_Expr});

	// end of expression:
	reg1({Token::punct_semi, Token::punct_rbrace, Token::punct_rbrck,
	      Token::punct_rparen, Token::punct_equal, Token::punct_colon,
	      Token::punct_arrow},
	     {nullptr, Expr_End});
	reg2({Token::punct_semi, Token::punct_rbrace, Token::punct_rbrck,
	      Token::punct_rparen, Token::punct_equal, Token::punct_colon,
	      Token::punct_arrow},
	     {nullptr, Expr_End});

	// Declarations/statement expressions
	reg1(Token::kw_let, {&drop_stop_tokens<parse_var_decl>, Atom_Expr});
}

unique_ptr<Node> parse_expr(tokenizer& tk, Scope& scope,
                            std::initializer_list<Token::Type> end_tok_types,
                            int bp) {
	if (std::find(end_tok_types.begin(), end_tok_types.end(), tk.peek().type)
	    != end_tok_types.end()) {
		throw 1;
	}
	auto prefix = scope._prefix_parselets.at(tk.peek().type);
	if (prefix.bp < bp) {
		throw 1;
	}
	assert(prefix);
	auto expr = prefix(tk, scope, end_tok_types);

	while (true) {
		if (std::find(end_tok_types.begin(), end_tok_types.end(), tk.peek().type)
		    != end_tok_types.end()) {
			break;
		}
		if (auto infix_i = scope._infix_parselets.find(tk.peek().type);
		    infix_i != scope._infix_parselets.end()) {
			if (infix_i->second.bp_l < bp) {
				break;
			}
			expr = (infix_i->second)(tk, scope, end_tok_types, std::move(expr));
		} else {
			// next token not valid for expression, but not one of the stop tokens
			break;
		}
	}
	return expr;
}
} // namespace AST
