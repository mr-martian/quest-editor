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
#ifndef AST_HPP
#define AST_HPP

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

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

		punct_lbrace,  // {
		punct_rbrace,  // }
		punct_lbrck,   // [
		punct_rbrck,   // ]
		punct_lparen,  // (
		punct_rparen,  // )
		punct_comma,   // ,
		punct_semi,    // ;
		punct_equal,   // =
		punct_arrow,   // ->
		punct_colon,   // :
		punct_scope,   // ::
		punct_bang,    // !
		punct_dollar,  // $
		punct_attr,    // #[
		punct_newline, // \n

		op_dot,     // .
		op_plus,    // +
		op_minus,   // -
		op_times,   // *
		op_div,     // /
		op_mod,     // %
		op_qplus,   // +?
		op_qminus,  // -?
		op_wplus,   // +%
		op_wminus,  // -%
		op_bitand,  // &
		op_bitor,   // |
		op_bitxor,  // ^
		op_compl,   // ~
		op_at,      // @
		op_hash,    // #
		op_equal,   // ==
		op_unequal, // !=
		op_cmp,     // <=>
		op_less,    // <
		op_greater, // >
		op_lte,     // <=
		op_gte,     // >=
		op_qm,      // ?
		op_qmqm,    // ??
		op_assign,  // :=

		kw_and,
		kw_or,
		kw_not,

		kw_Bool,
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

		id_int,      // i##
		id_unsigned, // u##

		kw_module,
		kw_export,
		kw_import,
		kw_alias,
		kw_enum,
		kw_fn,
		kw_proc,
		kw_let,
		kw_mut,
		kw_struct,
		kw_trait,

		kw_result,
		kw_return,
		kw_break,
		kw_continue,
		kw_yield,

		kw_if,
		kw_else,
		kw_end,
		kw_for,
		kw_do,
		kw_until,
		kw_while,
		kw_loop,
		kw_in,

		kw_defer,
		kw_implements,
		kw_delete,
		kw_private,
		kw_public,

		kw_underscore, // _

		reserved_id, // __.*

		identifier,

	} type;

	std::string str;
	source_location loc;

	[[nodiscard]] operator bool() const noexcept {
		return type != eof and type != unknown;
	}
};

std::string tok_name(Token::Type t);
class unexpected : std::invalid_argument {
 public:
	Token found;
	Token::Type expected;

	static std::string str(Token found, Token::Type expected) {
		using namespace std::literals;
		return "expected "s + tok_name(expected) + "before " + found.str;
	}

	unexpected(const Token& found, Token::Type expected)
	    : std::invalid_argument(str(found, expected)), found(found),
	      expected(expected) {}
};

class tokenizer {
 public:
	tokenizer(std::istream& is, bool l = false);

	[[nodiscard]] Token gettok() noexcept;
	[[nodiscard]] std::optional<Token> gettok_if(Token::Type t) noexcept;
	Token expect(Token::Type t);

	Token peek() noexcept { return next; }

	tokenizer& ignore(Token::Type) noexcept;
	tokenizer& ignore_consecutive(Token::Type t) noexcept;

 private:
	std::istream* source{};
	Token last{};
	Token next{};

	Token read();
	void advance();

 public:
	bool line_mode{};
};

namespace AST {

using Integer = std::int64_t;

using std::optional;
using std::string;
using std::unique_ptr;
using std::vector;

template <typename K, typename... Ts>
using set = std::unordered_set<K, Ts...>;

class SubstrateNode;

class ASTNode {
 public:
	ASTNode() = default;
	ASTNode(const ASTNode&) = delete;
	ASTNode(ASTNode&&) = delete;
	ASTNode& operator=(const ASTNode&) = delete;
	ASTNode& operator=(ASTNode&&) = delete;

	virtual unique_ptr<SubstrateNode> substrate() const;
	virtual std::ostream& pretty_print(std::ostream& os) const;

	virtual ~ASTNode() = default;

	vector<unique_ptr<ASTNode>> _attributes;
};

class ExprAST : virtual public ASTNode {};

/*
 * Type Expressions
 */

class TypeID : public ExprAST {};

class BuiltinTypeID : public TypeID {
	string _name;
};

class IntegerTypeID : public BuiltinTypeID {
 public:
	Integer _width;
};
class UnsignedTypeID : public BuiltinTypeID {
 public:
	Integer _width;
};

class OwnerTypeID : public BuiltinTypeID {
 public:
	unique_ptr<TypeID> _owned_type;
};
class SharedTypeID : public BuiltinTypeID {
 public:
	unique_ptr<TypeID> _owned_type;
};
class ArrayTypeID : public BuiltinTypeID {
 public:
	unique_ptr<TypeID> _element_type;
	vector<optional<Integer>> _bounds;
};

class TupleTypeID : public TypeID {
 public:
	vector<unique_ptr<TypeID>> _member_types;
};
class UnionTypeID : public TypeID {
 public:
	set<unique_ptr<TypeID>> _member_types;
};

class NoneTypeID : public TupleTypeID {};
class NoreturnTypeID : public UnionTypeID {};
class ThisTypeID : public BuiltinTypeID {};

class TemplatedTypeID : public TypeID {
 public:
	vector<unique_ptr<ExprAST>> _args;
};

/*
 * Simple Declarations
 */

class SignatureAST : virtual public ASTNode {
	vector<unique_ptr<TypeID>> _args, _returns;
};

struct Discriminators {
	vector<string> scope;
	optional<Integer> args;
	unique_ptr<SignatureAST> signature;
};

class DeclarationAST : virtual public ASTNode {
 public:
	string _name;
	bool _is_export;
	Discriminators _synthesized_discriminators;
};

class AliasDeclAST : public DeclarationAST {
 public:
	unique_ptr<TypeID> _type;
};

class VarDeclAST : public DeclarationAST {
 public:
	unique_ptr<TypeID> _type;
	bool _is_mut;
	unique_ptr<ASTNode> _initializer;
};

/*
 * Primitives
 */

enum class Direction : unsigned short {
	None = 0,
	Left = 1,
	Right = 2,
	Either = Left | Right,
};

class NameAST : public ExprAST {
 public:
	string _basename;
	Discriminators _discrim;
};

class OperatorAST : public NameAST {
 public:
	Direction _assoc;
	Direction _eval_order;
};

class LiteralAST : public ExprAST {
 public:
	string _text;
};

class IntegerLiteralAST : public LiteralAST {};

class FloatLiteralAST : public LiteralAST {};

class StringLiteralAST : public LiteralAST {};

class EnumeratorAST : public LiteralAST {
 public:
	string _name;
	unique_ptr<ExprAST> _value;
};

class BoolKeywordAST : public EnumeratorAST {};

/*
 * Expressions / Statements
 */

class UnaryExprAST : public ExprAST {
 public:
	unique_ptr<OperatorAST> _op;
	unique_ptr<ExprAST> _operand;
};

class BinaryExprAST : public ExprAST {
 public:
	unique_ptr<OperatorAST> _op;
	unique_ptr<ExprAST> _left, _right;
};

class CallExprAST : public ExprAST {
 public:
	unique_ptr<ExprAST> _fun;
	vector<unique_ptr<ExprAST>> _args;
};

class RewriteExprAST : public ExprAST {
 public:
	unique_ptr<ExprAST> _val;
	unique_ptr<ExprAST> _pattern;
};

class BlockAST : public ExprAST {
	vector<unique_ptr<ExprAST>> _expressions;
};

class AssignmentAST : public ExprAST {};

class ControlExprAST : public ExprAST {};

class IfExpression : public ControlExprAST {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target;
	unique_ptr<BlockAST> _true_body;
	unique_ptr<ExprAST> _false_body;
};

class InvertedIfExprAST : public ControlExprAST {
 public:
	unique_ptr<ExprAST> _body;
	unique_ptr<ExprAST> _condition;
	bool _target;
};

class LoopExprAST : public ControlExprAST {
 public:
	optional<string> _label;
	unique_ptr<BlockAST> _body;
};

class WhileExprAST : public LoopExprAST {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target;
	unique_ptr<ExprAST> _else;
};

class DoWhileExprAST : public LoopExprAST {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target;
	unique_ptr<ExprAST> _else;
};

class ForLoopExpr : public LoopExprAST {
 public:
	unique_ptr<VarDeclAST> _induction_variable;
	bool _is_mut;
};

class ForInExprAST : public ForLoopExpr {
 public:
	unique_ptr<ExprAST> _range_expr;
};

class ForRangeExprAST : public ForLoopExpr {
	unique_ptr<ExprAST> _r_begin, _r_end;
	unique_ptr<ExprAST> _update;
};

class GForExprAST : public ForLoopExpr {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target;
	unique_ptr<ExprAST> _update;
};

class MatchExprAST : public ControlExprAST {
 public:
};

class ResultExprAST : public ControlExprAST, public UnaryExprAST {
 public:
	string _result_keyword;
	optional<string> _target_label;
};

/*
 * Complex Declarations
 */

class ArgDeclAST : public VarDeclAST {};

class PrototypeAST : public DeclarationAST, public TypeID {
 public:
	bool _is_proc;
	optional<string> _linkage;
	unique_ptr<SignatureAST> _signature;
};

enum class Protection {
	None,
	Private,
	Module,
	Public,
};

class StructProtoAST : public DeclarationAST, public TypeID {
 public:
	vector<unique_ptr<ArgDeclAST>> _args;
};

class StructMemberAST : public DeclarationAST {};

class DataMemberDeclAST : public StructMemberAST, public VarDeclAST {
 public:
	Protection _read;
	Protection _mut;
};

class FunctionMemberDeclAST : public StructMemberAST, public PrototypeAST {
 public:
	Protection _access;
	bool _is_default;
};

class StructTraitImpl : public StructMemberAST {};

class StructDefAST : public StructProtoAST {
 public:
	vector<unique_ptr<DeclarationAST>> _members;
};

class EnumDeclAST : public DeclarationAST, public TypeID {
 public:
	vector<unique_ptr<EnumeratorAST>> _enumerators;
};

class TraitDeclAST : public DeclarationAST {
 public:
	vector<unique_ptr<ArgDeclAST>> _args;
	vector<unique_ptr<FunctionMemberDeclAST>> _members;
};

class FunctionDefAST : public PrototypeAST {
 public:
	bool _is_simple;
	optional<string> _delete_expr;
	unique_ptr<ASTNode> _body;
};

class ModuleAST : virtual public ASTNode {
 public:
	string name;
	bool is_exported;
	vector<string> imports;
	vector<unique_ptr<DeclarationAST>> _declarations;
};

/*
 * Substrate
 */

class SubstrateAST : virtual public ASTNode {
 public:
	vector<unique_ptr<ExprAST>> _terms;
};

auto parse_module(tokenizer& tk) -> unique_ptr<ModuleAST>;
auto parse_decl(tokenizer& tk) -> unique_ptr<DeclarationAST>;

} // namespace AST

#endif // AST_HPP
