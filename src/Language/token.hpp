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
#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <stdexcept>
#include <utility>

struct source_location {
	const char* filename{};

	int line{};
	int col{};

	int length{1};
};

struct Token {
	enum Type {
		eof = -1,
		unknown = 0,

		literal_int,
		literal_float,
		literal_char,
		literal_string,

		punct_lbrace,   // {
		punct_rbrace,   // }
		punct_lbrck,    // [
		punct_rbrck,    // ]
		punct_lparen,   // (
		punct_rparen,   // )
		punct_comma,    // ,
		punct_semi,     // ;
		punct_equal,    // =
		punct_arrow,    // ->
		punct_colon,    // :
		punct_scope,    // ::
		punct_bang,     // !
		punct_dollar,   // $
		punct_attr,     // #[
		punct_newline,  // \n
		punct_substr_b, // (:
		punct_substr_e, // :)

		op_dot,     // .
		op_plus,    // +
		op_minus,   // -
		op_times,   // *
		op_div,     // /
		op_rem,     // %
		op_mod,     // mod
		op_qplus,   // +?
		op_qminus,  // -?
		op_wplus,   // +%
		op_wminus,  // -%
		op_bitand,  // &
		op_bitor,   // |
		op_carat,   // ^
		op_compl,   // ~
		op_lshift,  // <<
		op_rshift,  // >>
		op_at,      // @
		op_hash,    // #
		op_pipe,    // |>
		op_equal,   // ==
		op_unequal, // !=
		op_cmp,     // <=>
		op_less,    // <
		op_greater, // >
		op_lte,     // <=
		op_gte,     // >=
		op_qm,      // ?
		op_qmqm,    // ??

		op_assign,        // :=
		op_plus_assign,   // +=
		op_minus_assign,  // -=
		op_times_assign,  // *=
		op_div_assign,    // /=
		op_rem_assign,    // %=
		op_qplus_assign,  // +?=
		op_qminus_assign, // -?=
		op_wplus_assign,  // +%=
		op_wminus_assign, // -%=
		op_bitand_assign, // &=
		op_bitor_assign,  // |=
		op_lshift_assign, // <<=
		op_rshift_assign, // >>=
		op_mod_assign,    // mod=
		op_xor_assign,    // xor=

		op_and,
		op_or,
		op_not,
		op_xor,

		kw_Bool,
		kw_Byte,
		kw_Fail,
		kw_Float,
		kw_Float32,
		kw_Float64,
		kw_Int,
		kw_Unsigned,
		kw_None,
		kw_Noreturn,
		kw_This,
		kw_Type,

		kw_true,
		kw_false,

		kw_module,
		kw_export,
		kw_import,
		kw_alias,
		kw_asm,
		kw_enum,
		kw_fn,
		kw_proc,
		kw_let,
		kw_const,
		kw_mut,
		kw_struct,
		kw_trait,
		kw_extern,
		kw_namespace,
		kw_substrate,
		kw_llvm,

		kw_await,
		kw_break,
		kw_consume,
		kw_continue,
		kw_drop,
		kw_match,
		kw_result,
		kw_return,
		kw_yield,

		kw_as,
		kw_if,
		kw_is,
		kw_else,
		kw_end,
		kw_for,
		kw_do,
		kw_unless,
		kw_until,
		kw_while,
		kw_loop,
		kw_in,
		kw_typeof,

		kw_defer,
		kw_implements,
		kw_delete,
		kw_private,
		kw_public,

		kw_underscore, // _

		id_int,      // i##
		id_unsigned, // u##

		reserved_id, // __.*
		placeholder, // _[0-9]+

		identifier,

	} type;

	std::string str;

	[[nodiscard]] bool good() const noexcept {
		return type != eof and type != unknown;
	}
	[[nodiscard]] explicit operator bool() const noexcept { return good(); }
};

enum class token_class {
	unknown,
	literal,
	punct,
	op,
	keyword,
	identifier,
	special,
	eof,
};
constexpr token_class tok_classify(Token::Type t) {
	auto in_range_i = [](auto v, std::pair<Token::Type, Token::Type> r) {
		return (r.first <= v) and (v <= r.second);
	};
	auto v = static_cast<std::underlying_type_t<Token::Type>>(t);
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
std::string tok_name(Token::Type t);

class unexpected : std::invalid_argument {
 public:
	Token found;
	std::string expected;

	static std::string str(Token found, std::string expected) {
		using namespace std::literals;
		return "expected "s + expected + "before " + found.str;
	}

	unexpected(const Token& found, Token::Type expected)
	    : std::invalid_argument(str(found, tok_name(expected)))
	    , found(found)
	    , expected(tok_name(expected)) {}
	unexpected(const Token& found, std::string expected)
	    : std::invalid_argument(str(found, expected))
	    , found(found)
	    , expected(std::move(expected)) {}
};

#endif // TOKEN_HPP
