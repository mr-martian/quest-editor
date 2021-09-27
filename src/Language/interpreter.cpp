#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <kblib/variant.h>

struct source_location {
	const char* filename{};

	int line{};
	int col{};

	int length{1};
};

class Token {
	enum type {
		unknown,
		eof,

		op_scope, // ::
		op_dot,   // .
		op_plus,  // +
		op_minus, // -
		op_times, // *
		op_div,   // /
		op_mod,   // %

		kw_None,
		kw_Noreturn,
		kw_This,
		kw_Type,
		kw_alias,
		kw_and,
		kw_bool,
		kw_break,
		kw_continue,
		kw_defer,
		kw_delete,
		kw_do,
		kw_double,
		kw_else,
		kw_end,
		kw_enum,
		kw_export,
		kw_fail,
		kw_float,
		kw_fn,
		kw_for,
		kw_if,
		kw_implements,
		kw_import,
		kw_int,
		kw_let,
		kw_loop,
		kw_module,
		kw_mut,
		kw_or,
		kw_private,
		kw_proc,
		kw_public,
		kw_result,
		kw_return,
		kw_struct,
		kw_trait,
		kw_until,
		kw_while,
		kw_yield,

		reserved_id,

		identifier,

	};

	std::string str;
	source_location loc;
};

class ASTNode {
 public:
	ASTNode(const ASTNode&) = delete;
	ASTNode(ASTNode&&) = delete;
	ASTNode& operator=(const ASTNode&) = delete;
	ASTNode& operator=(ASTNode&&) = delete;

	virtual ~ASTNode() = default;
};

int main(int argc, char** argv) {}
