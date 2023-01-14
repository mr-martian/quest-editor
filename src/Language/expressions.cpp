/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {

auto parse_control_expr(tokenizer& tk, Scope& scope) -> unique_ptr<Expr> {
	throw 0;
}
auto parse_assignment_expr(tokenizer& tk, Scope& scope) -> unique_ptr<Expr> {
	throw 0;
}
auto parse_block(tokenizer& tk, Scope& scope) -> unique_ptr<Block> { throw 0; }

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
 */
auto parse_expr(tokenizer& tk, Scope& scope) -> unique_ptr<Expr> {
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
}
} // namespace AST
