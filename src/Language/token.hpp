/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/
#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <iomanip>
#include <stdexcept>
#include <utility>

#include <kblib/stringops.h>

struct source_location {
	const char* filename{};

	std::size_t line{};
	std::size_t col{};

	std::size_t length{1};

	auto operator<=>(const source_location&) const = default;
};

struct Token;
std::string tok_name(Token t);
struct Token {
	enum Type {
		eof = -1,
		unknown = 0,
		illegal_literal,

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
		punct_substr_b, // (:
		punct_substr_e, // :)
		punct_attr,     // #[
		punct_comma,    // ,
		punct_semi,     // ;
		punct_equal,    // =
		punct_arrow,    // ->
		punct_colon,    // :
		punct_scope,    // ::
		punct_bang,     // !
		punct_dollar,   // $
		punct_newline,  // \n

		op_dot,     // .
		op_dots,    // ...
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
		op_orelse,  // ?:

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

		kw_alias,
		kw_as,
		kw_asm,
		kw_await,
		kw_break,
		kw_const,
		kw_consume,
		kw_continue,
		kw_defer,
		kw_delete,
		kw_do,
		kw_drop,
		kw_else,
		kw_end,
		kw_enum,
		kw_export,
		kw_extern,
		kw_fn,
		kw_for,
		kw_if,
		kw_implements,
		kw_import,
		kw_in,
		kw_is,
		kw_let,
		kw_llvm,
		kw_loop,
		kw_match,
		kw_module,
		kw_mut,
		kw_namespace,
		kw_private,
		kw_proc,
		kw_public,
		kw_result,
		kw_return,
		kw_struct,
		kw_substrate,
		kw_trait,
		kw_typeof,
		kw_unless,
		kw_until,
		kw_while,
		kw_yield,

		kw_underscore, // _

		id_int,      // i##
		id_unsigned, // u##

		reserved_id, // __.*
		placeholder, // _[0-9]+

		identifier,

	} type{unknown};

	std::string str;
	source_location loc;

	[[nodiscard]] bool good() const noexcept {
		return type != eof and type != unknown;
	}
	[[nodiscard]] explicit operator bool() const noexcept { return good(); }
	auto operator<=>(const Token&) const = default;
	friend std::ostream& operator<<(std::ostream& os, const Token& tok) {
		os << "TOKEN[" << std::setw(3) << tok.type << "] " << tok.loc.line << ':'
		   << tok.loc.col;
		if (auto l = tok.loc.length; l > 1) {
			os << '-' << tok.loc.col + l;
		}
		os << ' ' << tok_name(tok);
		return os;
	}
};
std::string tok_name(Token::Type t);

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
	} else if (in_range_i(v, {Token::literal_int, Token::literal_string})
	           or v == Token::illegal_literal) {
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

class unexpected : public std::invalid_argument {
 public:
	Token found;
	std::string expected;

	static std::string format_str(Token found, std::string expected);

	unexpected(const Token& found, std::string expected)
	    : std::invalid_argument(format_str(found, expected))
	    , found(found)
	    , expected(std::move(expected)) {}
};

#endif // TOKEN_HPP
