/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#ifndef AST_HPP
#define AST_HPP

#include "lex.yy.h"
#include "token.hpp"

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
	using generator_type = asyncpp::generator<Token>;

 public:
	tokenizer(std::istream& is, std::string filename, bool l = false);

	[[nodiscard]] Token gettok() {
		advance();
		return last;
	}
	[[nodiscard]] std::optional<Token> gettok_if(Token::Type t);
	[[nodiscard]] std::optional<Token> gettok_if(
	    std::initializer_list<Token::Type> ts);

	Token expect(Token::Type t);
	Token expect(std::initializer_list<Token::Type> ts);

	[[nodiscard]] Token peek() { return next; }
	[[nodiscard]] Token cur() { return last; }

	[[nodiscard]] bool check(Token::Type t) noexcept { return next.type == t; }
	[[nodiscard]] bool check(std::initializer_list<Token::Type> ts) noexcept {
		return std::find(begin(ts), end(ts), next.type) != end(ts);
	}

	tokenizer& ignore() {
		advance();
		return *this;
	}
	tokenizer& ignore(Token::Type);
	tokenizer& ignore_consecutive(Token::Type t);

	[[nodiscard]] bool good() const noexcept { return last.good(); }
	[[nodiscard]] explicit operator bool() const noexcept { return good(); }

 public:
	// If true, the Token::punct_newline token will be returned for the end of
	// each line, as well as immediately preceeding EOF. Otherwise, newlines are
	// considered whitespace and discarded.
	// bool line_mode{};
	bool newline_mode() { return _lex._newline_mode; }
	void newline_mode(bool mode) { _lex._newline_mode = mode; }

 private:
	Lexer _lex;
	generator_type _c;
	generator_type::iterator _cur;
	decltype(_c.end()) _end;

	Token last{};
	Token next{};

	void advance();
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

class Node {
 public:
	Node() = default;
	Node(const Node&) = delete;
	Node(Node&&) = delete;
	Node& operator=(const Node&) = delete;
	Node& operator=(Node&&) = delete;

	virtual unique_ptr<SubstrateNode> substrate() const;
	virtual std::ostream& pretty_print(std::ostream& os) const;

	virtual ~Node() = default;

	vector<unique_ptr<Node>> _attributes;
};

class Expr : virtual public Node {
 public:
	unique_ptr<Expr> _type;
};

/*
 * Type Expressions
 */

class BuiltinTypeID : public Expr {
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
	unique_ptr<Expr> _owned_type;
};
class SharedTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _owned_type;
};
class ArrayTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _element_type;
	vector<optional<Integer>> _bounds;
};

class TupleTypeID : public Expr {
 public:
	vector<unique_ptr<Expr>> _member_types;
};
class UnionTypeID : public Expr {
 public:
	set<unique_ptr<Expr>> _member_types;
};

class NoneTypeID : public TupleTypeID {};
class NoreturnTypeID : public UnionTypeID {};
class ThisTypeID : public BuiltinTypeID {};

class TemplatedTypeID : public Expr {
 public:
	vector<unique_ptr<Expr>> _args;
};

/*
 * Simple Declarations
 */

class Signature : virtual public Node {
 public:
	vector<unique_ptr<Expr>> _args, _returns;
	struct ref_tag_t {};
	std::variant<std::monostate, ref_tag_t, vector<unique_ptr<Expr>>> _captures;
};

struct Discriminators {
	vector<string> scope;
	optional<Integer> args;
	unique_ptr<Signature> signature;
};

class Declaration : virtual public Node {
 public:
	string _name;
	bool _is_export{};
	Discriminators _synthesized_discriminators{};
	string language;
	virtual bool is_static_scope() const noexcept { return true; }
};

class AliasDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
};

class VarDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
	bool _is_mut{};
	bool _is_const{};
	unique_ptr<Node> _initializer;
};

class ExprList : public Node {
 public:
	vector<unique_ptr<Expr>> _elems;
};

class Namespace : public Declaration {
 public:
	vector<unique_ptr<Declaration>> _declarations;
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

class Name : public Expr {
 public:
	string _basename;
	Discriminators _discrim;
};

class Operator : public Name {
 public:
	Direction _assoc{};
	Direction _eval_order{};
};

class Literal : public Expr {
 public:
	string _text;
};

class IntegerLiteral : public Literal {};

class FloatLiteral : public Literal {};

class StringLiteral : public Literal {};

class Enumerator : public Literal {
 public:
	string _name;
	unique_ptr<Expr> _value;
};

class BoolKeyword : public Enumerator {};

/*
 * Expressions / Statements
 */

class UnaryExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _operand;
};

class BinaryExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _left, _right;
};

class CallExpr : public Expr {
 public:
	unique_ptr<Expr> _fun;
	vector<unique_ptr<Expr>> _args;
};

class RewriteExpr : public Expr {
 public:
	unique_ptr<Expr> _val;
	unique_ptr<Expr> _pattern;
};

class Block : public Expr {
	vector<unique_ptr<Expr>> _expressions;
};

class Assignment : public Expr {};

class ControlExpr : public Expr {};

class IfExpression : public ControlExpr {
 public:
	unique_ptr<Expr> _condition;
	bool _target{};
	unique_ptr<Block> _true_body;
	unique_ptr<Expr> _false_body;
};

class InvertedIfExpr : public ControlExpr {
 public:
	unique_ptr<Expr> _body;
	unique_ptr<Expr> _condition;
	bool _target{};
};

class LoopExpr : public ControlExpr {
 public:
	optional<string> _label;
	unique_ptr<Block> _body;
};

class WhileExpr : public LoopExpr {
 public:
	unique_ptr<Expr> _condition;
	bool _target{};
	unique_ptr<Expr> _else;
};

class DoWhileExpr : public LoopExpr {
 public:
	unique_ptr<Expr> _condition;
	bool _target{};
	unique_ptr<Expr> _else;
};

class ForLoopExpr : public LoopExpr {
 public:
	unique_ptr<VarDecl> _induction_variable;
	bool _is_mut{};
};

class ForInExpr : public ForLoopExpr {
 public:
	unique_ptr<Expr> _range_expr;
};

class ForRangeExpr : public ForLoopExpr {
	unique_ptr<Expr> _r_begin, _r_end;
	unique_ptr<Expr> _update;
};

class GForExpr : public ForLoopExpr {
 public:
	unique_ptr<Expr> _condition;
	bool _target{};
	unique_ptr<Expr> _update;
};

class MatchExpr : public ControlExpr {
 public:
};

class ResultExpr
    : public ControlExpr
    , public UnaryExpr {
 public:
	string _result_keyword;
	optional<string> _target_label;
};

/*
 * Complex Declarations
 */

class ArgDecl : public VarDecl {};

class Prototype
    : public Declaration
    , public Expr {
 public:
	bool _is_proc{};
	optional<string> _linkage;
	unique_ptr<Signature> _signature;

	bool is_static_scope() const noexcept override { return true; }
};

enum class Protection {
	None,
	Private,
	Module,
	Public,
};

class StructProto
    : public Declaration
    , public Expr {
 public:
	vector<unique_ptr<ArgDecl>> _args;
};

class StructMember : public Declaration {};

class DataMemberDecl
    : public StructMember
    , public VarDecl {
 public:
	Protection _read{};
	Protection _mut{};
};

class FunctionMemberDecl
    : public StructMember
    , public Prototype {
 public:
	Protection _access{};
	bool _is_default{};
	using Prototype::is_static_scope;
};

class StructTraitImpl : public StructMember {};

class StructDef : public StructProto {
 public:
	vector<unique_ptr<Declaration>> _members;
};

class EnumDecl
    : public Declaration
    , public Expr {
 public:
	vector<unique_ptr<Enumerator>> _enumerators;
};

class TraitDecl : public Declaration {
 public:
	vector<unique_ptr<ArgDecl>> _args;
	vector<unique_ptr<FunctionMemberDecl>> _members;
};

class FunctionDef : public Prototype {
 public:
	bool _is_simple{};
	optional<string> _delete_expr;
	unique_ptr<Node> _body;
};

class Module
    : virtual public Node
    , public Namespace {
 public:
	vector<string> _imports;
};

/*
 * Substrate
 */

class SubstrateNode : virtual public Node {
 public:
	vector<unique_ptr<Expr>> _terms;
};

auto parse_module(tokenizer& tk) -> unique_ptr<Module>;
auto parse_decl(tokenizer& tk) -> unique_ptr<Declaration>;

} // namespace AST

#endif // AST_HPP
