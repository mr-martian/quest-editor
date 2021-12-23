/* *****************************************************************************
 * %{QMAKE_PROJECT_NAME}
 * Copyright (c) %YEAR% killerbee
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
#include <istream>

tokenizer::tokenizer(std::istream& in, const char* filename, bool l)
    : source{&in}, buffer_pos{filename}, line_mode{l} {
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
			return Token{Token::eof, "end of file", buffer_pos};
		} else {
			auto old_pos = buffer_pos;
			std::getline(*source, buffer, '\n');
			++buffer_pos.line;
			buffer_pos.col = 0;
			if (line_mode) {
				return {Token::punct_newline, "", old_pos};
			} else {
				return read();
			}
		}
	} else {
		// run tokenizer here
		return {};
	}
}
