/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

#include <kblib/convert.h>
#include <kblib/simple.h>

#include <istream>

token_class tok_classify(Token::Type t) {
	using kblib::in_range, kblib::in_range_i;
	auto v = kblib::etoi(t);
	if (v == Token::eof) {
		return token_class::eof;
	} else if (v == Token::unknown) {
		return token_class::unknown;
	} else if (in_range_i(v, {Token::literal_int, Token::literal_string})) {
		return token_class::literal;
	} else if (in_range_i(v, {Token::punct_lbrace, Token::punct_substr_e})) {
		return token_class::punct;
	} else if (in_range_i(v, {Token::op_dot, Token::op_xor})) {
		return token_class::op;
	} else if (in_range_i(v, {Token::kw_Bool, Token::kw_underscore})) {
		return token_class::keyword;
	} else if (v == Token::id_int or v == Token::id_unsigned) {
		return token_class::special;
	} else if (v == Token::reserved_id or v == Token::placeholder) {
		return token_class::special;
	} else if (v == Token::identifier) {
		return token_class::identifier;
	} else {
		return token_class::unknown;
	}
	// no return here to ensure warning if not all cases covered
}

tokenizer::tokenizer(std::istream& in, std::string filename, bool l)
    : line_mode{l}
    , _lex{std::move(filename), in}
    , _c(_lex.lex())
    , _cur(_c.begin())
    , _end(_c.end())
    , next(*_cur) {
	++_cur;
	advance();
}

std::optional<Token> tokenizer::gettok_if(const Token::Type t) {
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

Token tokenizer::expect(std::initializer_list<Token::Type> ts) {
	if (std::any_of(ts.begin(), ts.end(),
	                [&](const auto& t) { return t == next.type; })) {
		return gettok();
	} else {
		throw unexpected(last, Token::unknown);
	}
}

tokenizer& tokenizer::ignore(const Token::Type t) {
	if (t != Token::eof and next.type == t) {
		advance();
	}
	return *this;
}
tokenizer& tokenizer::ignore_consecutive(const Token::Type t) {
	ignore(t);
	while (t != Token::eof and last.type == t) {
		advance();
	}
	return *this;
}

void tokenizer::advance() {
	if (not line_mode) {
		while (next.type != Token::eof and next.type == Token::punct_newline) {
			next = *_cur;
		}
	}
	last = std::exchange(next, *_cur);
	++_cur;
	return;
}
