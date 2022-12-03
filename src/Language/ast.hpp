/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#ifndef AST_HPP
#define AST_HPP

#include LEX_HEADER

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iosfwd>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

class tokenizer {
 public:
	tokenizer(std::istream& is, const char* filename, bool l = false);

	[[nodiscard]] Token gettok() noexcept;
	[[nodiscard]] std::optional<Token> gettok_if(Token::Type t) noexcept;
	[[nodiscard]] std::optional<Token> gettok_if(
	    std::initializer_list<Token::Type> ts) noexcept;

	Token expect(Token::Type t);
	Token expect(std::initializer_list<Token::Type> ts);

	[[nodiscard]] Token peek() noexcept { return next; }
	[[nodiscard]] Token cur() noexcept { return last; }
	[[nodiscard]] bool check(Token::Type t) noexcept { return next.type == t; }
	[[nodiscard]] bool check(std::initializer_list<Token::Type> ts) {
		return std::find(begin(ts), end(ts), next.type) != end(ts);
	}

	tokenizer& ignore() noexcept;
	tokenizer& ignore(Token::Type) noexcept;
	tokenizer& ignore_consecutive(Token::Type t) noexcept;

	[[nodiscard]] bool good() const noexcept { return last.good(); }
	[[nodiscard]] explicit operator bool() const noexcept { return good(); }

 private:
	Lexer _lex;
	source_location _buffer_pos;

	Token last{};
	Token next{};

	Token read();
	void advance();

 public:
	// If true, the Token::punct_newline token will be returned for the end of
	// each line, as well as immediately preceeding EOF. Otherwise, newlines are
	// considered whitespace and discarded.
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

class ExprAST : virtual public ASTNode {
 public:
	unique_ptr<ExprAST> _type;
};

/*
 * Type Expressions
 */

class BuiltinTypeID : public ExprAST {
 public:
	BuiltinTypeID() = default;
	BuiltinTypeID(std::string name)
	    : _name(std::move(name)) {}

	string _name;
};

class IntegerTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
};
class UnsignedTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
};

class OwnerTypeID : public BuiltinTypeID {
 public:
	unique_ptr<ExprAST> _owned_type;
};
class SharedTypeID : public BuiltinTypeID {
 public:
	unique_ptr<ExprAST> _owned_type;
};
class ArrayTypeID : public BuiltinTypeID {
 public:
	unique_ptr<ExprAST> _element_type;
	vector<optional<Integer>> _bounds;
};

class TupleTypeID : public ExprAST {
 public:
	vector<unique_ptr<ExprAST>> _member_types;
};
class UnionTypeID : public ExprAST {
 public:
	set<unique_ptr<ExprAST>> _member_types;
};

class NoneTypeID : public TupleTypeID {};
class NoreturnTypeID : public UnionTypeID {};
class ThisTypeID : public BuiltinTypeID {};

class TemplatedTypeID : public ExprAST {
 public:
	vector<unique_ptr<ExprAST>> _args;
};

/*
 * Simple Declarations
 */

class SignatureAST : virtual public ASTNode {
	vector<unique_ptr<ExprAST>> _args, _returns;
};

struct Discriminators {
	vector<string> scope;
	optional<Integer> args;
	unique_ptr<SignatureAST> signature;
};

class DeclarationAST : virtual public ASTNode {
 public:
	string _name;
	bool _is_export{};
	Discriminators _synthesized_discriminators{};
	string language;
	virtual bool is_static_scope() const noexcept { return true; }
};

class AliasDeclAST : public DeclarationAST {
 public:
	unique_ptr<ExprAST> _type;
};

class VarDeclAST : public DeclarationAST {
 public:
	unique_ptr<ExprAST> _type;
	bool _is_mut{};
	bool _is_const{};
	unique_ptr<ASTNode> _initializer;
};

class ExprListAST : public ASTNode {
 public:
	vector<unique_ptr<ExprAST>> _elems;
};

class NamespaceAST : public DeclarationAST {
 public:
	vector<unique_ptr<DeclarationAST>> _declarations;
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
	Direction _assoc{};
	Direction _eval_order{};
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
	bool _target{};
	unique_ptr<BlockAST> _true_body;
	unique_ptr<ExprAST> _false_body;
};

class InvertedIfExprAST : public ControlExprAST {
 public:
	unique_ptr<ExprAST> _body;
	unique_ptr<ExprAST> _condition;
	bool _target{};
};

class LoopExprAST : public ControlExprAST {
 public:
	optional<string> _label;
	unique_ptr<BlockAST> _body;
};

class WhileExprAST : public LoopExprAST {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target{};
	unique_ptr<ExprAST> _else;
};

class DoWhileExprAST : public LoopExprAST {
 public:
	unique_ptr<ExprAST> _condition;
	bool _target{};
	unique_ptr<ExprAST> _else;
};

class ForLoopExpr : public LoopExprAST {
 public:
	unique_ptr<VarDeclAST> _induction_variable;
	bool _is_mut{};
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
	bool _target{};
	unique_ptr<ExprAST> _update;
};

class MatchExprAST : public ControlExprAST {
 public:
};

class ResultExprAST
    : public ControlExprAST
    , public UnaryExprAST {
 public:
	string _result_keyword;
	optional<string> _target_label;
};

/*
 * Complex Declarations
 */

class ArgDeclAST : public VarDeclAST {};

class PrototypeAST
    : public DeclarationAST
    , public ExprAST {
 public:
	bool _is_proc{};
	optional<string> _linkage;
	unique_ptr<SignatureAST> _signature;

	bool is_static_scope() const noexcept override { return true; }
};

enum class Protection {
	None,
	Private,
	Module,
	Public,
};

class StructProtoAST
    : public DeclarationAST
    , public ExprAST {
 public:
	vector<unique_ptr<ArgDeclAST>> _args;
};

class StructMemberAST : public DeclarationAST {};

class DataMemberDeclAST
    : public StructMemberAST
    , public VarDeclAST {
 public:
	Protection _read{};
	Protection _mut{};
};

class FunctionMemberDeclAST
    : public StructMemberAST
    , public PrototypeAST {
 public:
	Protection _access{};
	bool _is_default{};
	using PrototypeAST::is_static_scope;
};

class StructTraitImpl : public StructMemberAST {};

class StructDefAST : public StructProtoAST {
 public:
	vector<unique_ptr<DeclarationAST>> _members;
};

class EnumDeclAST
    : public DeclarationAST
    , public ExprAST {
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
	bool _is_simple{};
	optional<string> _delete_expr;
	unique_ptr<ASTNode> _body;
};

class ModuleAST
    : virtual public ASTNode
    , public NamespaceAST {
 public:
	vector<string> _imports;
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
