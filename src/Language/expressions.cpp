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

auto parse_literal(Scope&, tokenizer& tk) -> unique_ptr<Expr> {
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

auto parse_name_expr(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
	auto name = Name();
	return make_unique<NameExpr>(parse_qualified_name(tk, scope, name),
	                             std::move(name));
}

auto parse_typeid(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
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
		return std::make_unique<NoneTypeID>(tk.cur());
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
auto parse_prefix_expr(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
	auto ret = std::make_unique<PrefixExpr>(tk.gettok());
	ret->_operand = parse_expr(tk, scope, {}, bp);
	return ret;
}

template <int bp_r>
auto parse_postfix_expr(Scope& scope, tokenizer& tk, unique_ptr<Expr> left)
    -> unique_ptr<Expr> {
	auto ret = make_unique<PostfixExpr>(tk.gettok());
	ret->_operand = std::move(left);
	return ret;
}

template <int bp_r>
auto parse_binary_expr(Scope& scope, tokenizer& tk, unique_ptr<Expr> left)
    -> unique_ptr<Expr> {
	auto ret = make_unique<BinaryExpr>(tk.gettok());
	ret->_left = std::move(left);
	ret->_right = parse_expr(tk, scope, {}, bp_r);
	return ret;
}

auto parse_ref_expr(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
	auto ret = std::make_unique<RefExpr>(tk.gettok());
	if (tk.gettok_if(Token::kw_mut)) {
		ret->_is_mut = true;
	}
	ret->_operand = parse_expr(tk, scope, {}, Prefix_Expr);
	return ret;
}

template <Token::Type>
auto parse_if_expr1(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
	throw 0;
}
template <Token::Type>
auto parse_if_expr2(Scope& scope, tokenizer& tk, unique_ptr<Expr> left)
    -> unique_ptr<Expr> {
	throw 0;
}
auto parse_block(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> { throw 0; }
auto parse_parenthesized_expr(Scope& scope, tokenizer& tk) -> unique_ptr<Expr> {
	throw 0;
}
template <Token::Type end>
auto parse_call_expr(Scope& scope, tokenizer& tk, unique_ptr<Expr> left)
    -> unique_ptr<Expr> {
	auto ret = std::make_unique<CallExpr>(tk.gettok());
	ret->_fun = std::move(left);
	ret->_args.push_back(parse_expr(tk, scope, {Token::punct_comma}));
	while (tk.gettok_if(Token::punct_comma)) {
		ret->_args.push_back(parse_expr(tk, scope, {Token::punct_comma}));
	}
	tk.expect(Token::punct_rparen);
	return ret;
}

template <Token::Type begin, Token::Type end>
auto read_bracketed_expr(tokenizer& tk, Scope& scope, std::vector<Token>& expr)
    -> void {
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
	reg1(Token::punct_lbrace, {&parse_block, Atom_Expr});

	// Prefix operators (loose binding)
	reg1({Token::kw_await, Token::kw_break, Token::kw_continue, Token::kw_result,
	      Token::kw_return, Token::kw_yield},
	     {&parse_prefix_expr<0>, Exit_expr});

	reg1(Token::kw_if, {&parse_if_expr1<Token::kw_else>, If_Expr_L});

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
	reg2({Token::op_qmqm},
	     {&parse_binary_expr<Null_Coalescing_Expr_R>, Null_Coalescing_Expr_L});
	reg2({Token::op_pipe}, {&parse_binary_expr<Rewrite_Expr_R>, Rewrite_Expr_L});
	reg2({Token::op_orelse}, {&parse_binary_expr<Cond_Expr_R>, Cond_Expr_L});
	reg2({Token::op_qm}, {&parse_if_expr2<Token::punct_colon>, Cond_Expr_L});
	reg2(Token::kw_match, {nullptr, Match_Expr});
	reg2(Token::kw_if, {nullptr, If_Expr_L});

	reg2(Token::punct_comma, {nullptr, Expr_List});

	//
	reg2(Token::punct_lparen,
	     {&parse_call_expr<Token::punct_rparen>, Postfix_Expr});
	reg2(Token::punct_lbrck,
	     {&parse_call_expr<Token::punct_rbrck>, Postfix_Expr});

	// end of expression:
	reg2({Token::punct_semi, Token::punct_rbrace, Token::punct_rbrck,
	      Token::punct_rparen, Token::punct_equal, Token::punct_colon},
	     {nullptr, Expr_End});
}

unique_ptr<Expr> parse_expr(tokenizer& tk, Scope& scope,
                            std::initializer_list<Token::Type> end_tok_types,
                            int bp) {
	auto prefix = scope._prefix_parselets.at(tk.peek().type);
	assert(prefix);
	auto expr = prefix(scope, tk);

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
			expr = (infix_i->second)(scope, tk, std::move(expr));
		} else {
			throw 1;
		}
	}
	return expr;
}
} // namespace AST
