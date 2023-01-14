/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

#include "reflex/matcher.h"
#include <kblib/convert.h>
#include <kblib/simple.h>

#include <istream>

std::string tok_name(Token::Type t) {
	switch (t) {
	//@eof:empty
	case Token::eof:
		return "end of file";
	//@unknown:string
	case Token::unknown:
		return "unkown token \"\x1B\"";

	//@literal:string
	case Token::literal_int:
		return "integer '\x1B'";
	case Token::literal_float:
		return "floating-point number '\x1B'";
	case Token::literal_char:
		return "character literal \"\x1B\"";
	case Token::literal_string:
		return "string literal \x1B";

	//@punct:empty
	case Token::punct_lbrace:
		return "'{'";
	case Token::punct_rbrace:
		return "'}'";
	case Token::punct_lbrck:
		return "'['";
	case Token::punct_rbrck:
		return "']'";
	case Token::punct_lparen:
		return "'('";
	case Token::punct_rparen:
		return "')'";
	case Token::punct_comma:
		return "','";
	case Token::punct_semi:
		return "';'";
	case Token::punct_equal:
		return "'='";
	case Token::punct_arrow:
		return "'->'";
	case Token::punct_colon:
		return "':'";
	case Token::punct_scope:
		return "'::'";
	case Token::punct_bang:
		return "'!'";
	case Token::punct_dollar:
		return "'$'";
	case Token::punct_attr:
		return "'#['";
	case Token::punct_newline:
		return "newline";
	case Token::punct_substr_b:
		return "'(:'";
	case Token::punct_substr_e:
		return "':)'";

	//@op:empty
	case Token::op_dot:
		return "'.'";
	case Token::op_plus:
		return "'+'";
	case Token::op_minus:
		return "'-'";
	case Token::op_times:
		return "'*'";
	case Token::op_div:
		return "'/'";
	case Token::op_rem:
		return "'%'";
	case Token::op_mod:
		return "'mod'";
	case Token::op_qplus:
		return "'+?'";
	case Token::op_qminus:
		return "'-?'";
	case Token::op_wplus:
		return "'+%'";
	case Token::op_wminus:
		return "'-%'";
	case Token::op_bitand:
		return "'&'";
	case Token::op_bitor:
		return "'|'";
	case Token::op_carat:
		return "'^'";
	case Token::op_compl:
		return "'~'";
	case Token::op_lshift:
		return "'<<'";
	case Token::op_rshift:
		return "'>>'";
	case Token::op_at:
		return "'@'";
	case Token::op_hash:
		return "'#'";
	case Token::op_pipe:
		return "'|>'";
	case Token::op_equal:
		return "'=='";
	case Token::op_unequal:
		return "'!='";
	case Token::op_cmp:
		return "'<=>'";
	case Token::op_less:
		return "'<'";
	case Token::op_greater:
		return "'>'";
	case Token::op_lte:
		return "'<='";
	case Token::op_gte:
		return "'>='";
	case Token::op_qm:
		return "'?'";
	case Token::op_qmqm:
		return "'?\?'";

	case Token::op_assign:
		return "':='";
	case Token::op_plus_assign:
		return "'+='";
	case Token::op_minus_assign:
		return "'-='";
	case Token::op_times_assign:
		return "'*='";
	case Token::op_div_assign:
		return "'/='";
	case Token::op_rem_assign:
		return "'%='";
	case Token::op_qplus_assign:
		return "'+?='";
	case Token::op_qminus_assign:
		return "'-?='";
	case Token::op_wplus_assign:
		return "'+%='";
	case Token::op_wminus_assign:
		return "'-%='";
	case Token::op_bitand_assign:
		return "'&='";
	case Token::op_bitor_assign:
		return "'|='";
	case Token::op_lshift_assign:
		return "'<<='";
	case Token::op_rshift_assign:
		return "'>>='";
	case Token::op_mod_assign:
		return "'mod='";
	case Token::op_xor_assign:
		return "'xor='";

	case Token::op_and:
		return "'and'";
	case Token::op_or:
		return "'or'";
	case Token::op_not:
		return "'not'";
	case Token::op_xor:
		return "'xor'";

	//@keyword:empty
	case Token::kw_Bool:
		return "'Bool'";
	case Token::kw_Byte:
		return "'Byte'";
	case Token::kw_Fail:
		return "'Fail'";
	case Token::kw_Float:
		return "'Float'";
	case Token::kw_Float32:
		return "'Float32'";
	case Token::kw_Float64:
		return "'Float64'";
	case Token::kw_Int:
		return "'Int'";
	case Token::kw_Unsigned:
		return "'Unsigned'";
	case Token::kw_None:
		return "'None'";
	case Token::kw_Noreturn:
		return "'Noreturn'";
	case Token::kw_This:
		return "'This'";
	case Token::kw_Type:
		return "'Type'";

	case Token::kw_true:
		return "'true'";
	case Token::kw_false:
		return "'false'";

	case Token::kw_module:
		return "'module'";
	case Token::kw_export:
		return "'export'";
	case Token::kw_import:
		return "'import'";
	case Token::kw_alias:
		return "'alias'";
	case Token::kw_asm:
		return "'asm'";
	case Token::kw_enum:
		return "'enum'";
	case Token::kw_fn:
		return "'fn'";
	case Token::kw_proc:
		return "'proc'";
	case Token::kw_let:
		return "'let'";
	case Token::kw_const:
		return "'const'";
	case Token::kw_mut:
		return "'mut'";
	case Token::kw_struct:
		return "'struct'";
	case Token::kw_trait:
		return "'trait'";
	case Token::kw_extern:
		return "'extern'";
	case Token::kw_namespace:
		return "'namespace'";
	case Token::kw_substrate:
		return "'substrate'";
	case Token::kw_llvm:
		return "'llvm'";

	case Token::kw_await:
		return "'await'";
	case Token::kw_break:
		return "'break'";
	case Token::kw_consume:
		return "'consume'";
	case Token::kw_continue:
		return "'continue'";
	case Token::kw_drop:
		return "'drop'";
	case Token::kw_match:
		return "'match'";
	case Token::kw_result:
		return "'result'";
	case Token::kw_return:
		return "'return'";
	case Token::kw_yield:
		return "'yield'";

	case Token::kw_as:
		return "'as'";
	case Token::kw_if:
		return "'if'";
	case Token::kw_is:
		return "'is'";
	case Token::kw_else:
		return "'else'";
	case Token::kw_end:
		return "'end'";
	case Token::kw_for:
		return "'for'";
	case Token::kw_do:
		return "'do'";
	case Token::kw_unless:
		return "'unless'";
	case Token::kw_until:
		return "'until'";
	case Token::kw_while:
		return "'while'";
	case Token::kw_loop:
		return "'loop'";
	case Token::kw_in:
		return "'in'";
	case Token::kw_typeof:
		return "'typeof'";

	case Token::kw_defer:
		return "'defer'";
	case Token::kw_implements:
		return "'implements'";
	case Token::kw_delete:
		return "'delete'";
	case Token::kw_private:
		return "'private'";
	case Token::kw_public:
		return "'public'";

	case Token::kw_underscore:
		return "'_'";

	//@special:uint32_t
	case Token::id_int:
		return "'\x1B'";
	case Token::id_unsigned:
		return "'\x1B'";

	//@identifier:string
	case Token::reserved_id:
		return "reserved identifier \"\x1B\"";
	case Token::placeholder:
		return "placeholder \"\x1B\"";

	case Token::identifier:
		return "identifier \"\x1B\"";
	}
}

std::string tok_name(Token t) {
	std::string ret;
	const auto name = tok_name(t.type);
	for (auto c : name) {
		if (c == '\x1B') {
			ret += t.str;
		} else {
			ret += c;
		}
	}
	return ret;
}

std::string unexpected::format_str(Token found, std::string expected) {
	using namespace std::literals;
	return kblib::concat("expected "sv, expected, " before "sv, tok_name(found));
}

tokenizer::tokenizer(std::istream& in, std::string filename, bool l)
    : _lex{std::move(filename), l, in}
    , _c(_lex.lex())
    , _cur(_c.begin())
    , _end(_c.end())
    , next(*_cur) {
	++_cur;
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
		throw unexpected(last, tok_name(Token{t, "", {}}));
	}
}

Token tokenizer::expect(std::initializer_list<Token::Type> ts) {
	if (std::any_of(ts.begin(), ts.end(),
	                [&](const auto& t) { return t == next.type; })) {
		return gettok();
	} else {
		std::vector<std::string> expected_names(ts.size());
		std::transform(begin(ts), end(ts), begin(expected_names),
		               [](Token::Type t) {
			               return tok_name(Token{t, "", {}});
		               });
		auto expected = kblib::join(expected_names, ", or ");
		throw unexpected(last, expected);
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
	last = std::exchange(next, *_cur);
	++_cur;

	{
		/*constexpr auto& flag = "\x1B";
		auto formatted = std::string{};
		kblib::search_replace_copy(name,     //
		                           flag,     //
		                           last.str, //
		                           std::back_inserter(formatted));*/
		if (_verbose) {
			std::cout << "TOKEN: " << std::setw(3) << last.type << ' '
			          << tok_name(last) << '\n'
			          << std::flush;
		}
	}
	return;
}
