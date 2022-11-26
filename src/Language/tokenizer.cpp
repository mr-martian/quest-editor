/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"
#include <istream>

tokenizer::tokenizer(std::istream& in, const char* filename, bool l)
    : source{&in}
    , buffer_pos{filename}
    , line_mode{l} {
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

Token tokenizer::expect(std::initializer_list<Token::Type> ts) {
	if (std::any_of(ts.begin(), ts.end(),
	                [&](const auto& t) { return t == next.type; })) {
		return gettok();
	} else {
		throw unexpected(last, Token::unknown);
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
	if (buffer.empty()) {
		if (not *source) {
			return Token{Token::eof, "end of file"};
		} else {
			auto old_pos = buffer_pos;
			std::getline(*source, buffer, '\n');
			++buffer_pos.line;
			buffer_pos.col = 0;
			if (line_mode) {
				return {Token::punct_newline, ""};
			} else {
				return read();
			}
		}
	} else {
		// run tokenizer here
		return {};
	}
}
