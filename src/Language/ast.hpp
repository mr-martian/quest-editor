/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#ifndef AST_HPP
#define AST_HPP

#include "lex.yy.h"
#include "token.hpp"

#include <kblib/hash.h>
#include <kblib/poly_obj.h>

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iosfwd>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

inline auto operator<<(std::ostream& os, source_location loc) -> std::ostream& {
	os << loc.filename << ':' << loc.line << ':' << loc.col;
	if (loc.length > 1) {
		os << '-' << loc.col + loc.length;
	}
	return os;
}

class tokenizer {
	using generator_type = asyncpp::generator<Token>;

 public:
	tokenizer(std::istream& is, std::string filename, bool l = false,
	          bool verbose = false);

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
	[[nodiscard]] bool was(Token::Type t) noexcept { return last.type == t; }
	[[nodiscard]] bool was(std::initializer_list<Token::Type> ts) noexcept {
		return std::find(begin(ts), end(ts), last.type) != end(ts);
	}

	tokenizer& ignore() {
		advance();
		return *this;
	}
	tokenizer& ignore(Token::Type);
	tokenizer& ignore_consecutive(Token::Type t);

	[[nodiscard]] bool good() const noexcept { return last.good(); }
	[[nodiscard]] explicit operator bool() const noexcept { return good(); }
	[[nodiscard]] bool eof() const noexcept { return _cur == _end; }

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

 public:
	bool _verbose{};
};

class Type;

namespace AST {
using namespace std::literals;

using Integer = std::int64_t;

using std::make_shared;
using std::make_unique;
using std::nullptr_t;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

template <typename K, typename... Ts>
using set = std::unordered_set<K, Ts...>;
template <typename K, typename T, typename... Ts>
using map = std::unordered_map<K, T, Ts...>;

class SubstrateNode;

class Node {
 public:
	Node() = delete;
	Node(const Node&) = delete;
	Node(Node&&) = delete;
	Node& operator=(const Node&) = delete;
	Node& operator=(Node&&) = delete;

	explicit Node(Token&& t)
	    : _tok(std::move(t)) {}
	explicit Node(const Token& t)
	    : _tok(t) {}
	Node(Token t, vector<unique_ptr<Node>> attr)
	    : _tok(std::move(t))
	    , _attributes(std::move(attr)) {}

	virtual unique_ptr<SubstrateNode> substrate() const;
	virtual std::ostream& pretty_print(std::ostream& os) const = 0;

	virtual ~Node() = default;

	Token _tok{};
	vector<unique_ptr<Node>> _attributes;

	inline static const auto indent_idx = std::ios_base::xalloc();

 protected:
	Node(nullptr_t) { throw nullptr; }
};

class Expr : virtual public Node {
 public:
	using Node::Node;
	Expr(Token&& t, unique_ptr<Expr> type)
	    : Node(std::move(t))
	    , _type(std::move(type)) {}
	unique_ptr<Expr> _type;

 protected:
	Expr(nullptr_t, unique_ptr<Expr> type)
	    : Node(nullptr)
	    , _type(std::move(type)) {}
};

/*
 * Type Expressions
 */

class BuiltinTypeID : public Expr {
 public:
	explicit BuiltinTypeID(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _name(_tok.str) {}
	BuiltinTypeID(Token&& t, string name)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _name(name) {}

	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type " << _name << ")";
	}

	string _name;

 protected:
	BuiltinTypeID()
	    : Node(nullptr)
	    , Expr(nullptr) {}
	explicit BuiltinTypeID(string name)
	    : Node(nullptr)
	    , Expr(nullptr)
	    , _name(name) {}
};

class IntegerTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type " << 'i' << _width << ")";
	}
	explicit IntegerTypeID(Token&& t);
	explicit IntegerTypeID(Token&& t, Integer width)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str)
	    , _width(width) {}
	explicit IntegerTypeID(Token&& t, string str, Integer width)
	    : Node(std::move(t))
	    , BuiltinTypeID(std::move(str))
	    , _width(width) {}

 protected:
	explicit IntegerTypeID(nullptr_t, string id);
	explicit IntegerTypeID(nullptr_t, string name, Integer width);
};
class UnsignedTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type " << 'u' << _width << ")";
	}
	explicit UnsignedTypeID(Token&& t);
	explicit UnsignedTypeID(Token&& t, Integer width)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str)
	    , _width(width) {}
	explicit UnsignedTypeID(Token&& t, string str, Integer width)
	    : Node(std::move(t))
	    , BuiltinTypeID(std::move(str))
	    , _width(width) {}

 protected:
	explicit UnsignedTypeID(nullptr_t, string id);
	explicit UnsignedTypeID(nullptr_t, string name, Integer width);
};
class FloatTypeID : public BuiltinTypeID {
 public:
	enum width {
		null,
		Float32 = 32,
		Float64 = 64,
	} _width{};
	explicit FloatTypeID(Token&& t);
	explicit FloatTypeID(string id);
	explicit FloatTypeID(Token&& t, width w)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str)
	    , _width(w) {}
	explicit FloatTypeID(Token&& t, string str, width w)
	    : Node(std::move(t))
	    , BuiltinTypeID(std::move(str))
	    , _width(w) {}

 protected:
	explicit FloatTypeID(nullptr_t, string name, Integer width);
};

class OwnerTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _owned_type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "(owner ";
		if (_owned_type) {
			return _owned_type->pretty_print(os) << ")";
		} else {
			throw 1;
		}
	}
};
class SharedTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _owned_type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "(shared ";
		if (_owned_type) {
			return _owned_type->pretty_print(os) << ")";
		} else {
			throw 1;
		}
	}
};
class ArrayTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _element_type;
	vector<optional<Integer>> _bounds;
	std::ostream& pretty_print(std::ostream& os) const override;
	// Should be the '[' token that starts the array spec
	explicit ArrayTypeID(Token&& t)
	    : Node(std::move(t)) {}

 protected:
	ArrayTypeID()
	    : Node(nullptr)
	    , BuiltinTypeID() {}
};

class TupleTypeID : public BuiltinTypeID {
 public:
	vector<unique_ptr<Expr>> _member_types;

	// Node interface
 public:
	std::ostream& pretty_print(std::ostream& os) const override;
	// Should be the 'Tuple' or '(' token that starts the tuple-id
	explicit TupleTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}

 protected:
	TupleTypeID()
	    : Node(nullptr)
	    , BuiltinTypeID() {}
	explicit TupleTypeID(string str)
	    : Node(nullptr)
	    , BuiltinTypeID(std::move(str)) {}
};
class UnionTypeID : public BuiltinTypeID {
 public:
	set<unique_ptr<Expr>> _member_types;

	// Node interface
 public:
	std::ostream& pretty_print(std::ostream& os) const override;
	// Should be the 'Union' token that starts the tuple-id, or else the first
	// token of the first member type
	explicit UnionTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID() {}

 protected:
	UnionTypeID()
	    : Node(nullptr)
	    , BuiltinTypeID() {}
	explicit UnionTypeID(string str)
	    : Node(nullptr)
	    , BuiltinTypeID(std::move(str)) {}
};

struct BoolTypeID : public BuiltinTypeID {
	explicit BoolTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}
};
struct ByteTypeID : public BuiltinTypeID {
	explicit ByteTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}
};
struct FailTypeID : public BuiltinTypeID {
	explicit FailTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}
};

struct NoneTypeID : public TupleTypeID {
	explicit NoneTypeID(Token&& t)
	    : Node(std::move(t))
	    , TupleTypeID(_tok.str) {}
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type None)";
	}
};
struct NoreturnTypeID : public UnionTypeID {
	explicit NoreturnTypeID(Token&& t)
	    : Node(std::move(t))
	    , UnionTypeID(_tok.str) {}
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type Noreturn)";
	}
};
struct ThisTypeID : public BuiltinTypeID {
	explicit ThisTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type This)";
	}
};
struct TypeTypeID : public BuiltinTypeID {
	explicit TypeTypeID(Token&& t)
	    : Node(std::move(t))
	    , BuiltinTypeID(_tok.str) {}
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(Type Type)";
	}
};

class TemplatedTypeID : public Expr {
 public:
	vector<unique_ptr<Expr>> _args;
};

/*
 * Simple Declarations
 */

class ArgDecl;

class Signature : virtual public Node {
 public:
	vector<unique_ptr<ArgDecl>> _args;
	vector<unique_ptr<Expr>> _returns;
	struct ref_tag_t {};
	std::variant<std::monostate, ref_tag_t, vector<unique_ptr<Expr>>> _captures;
	std::ostream& pretty_print(std::ostream& os) const override;

	// Should be the '(' token that begins the argument list
	explicit Signature(Token&& t)
	    : Node(std::move(t)) {}
};

struct Discriminators {
	vector<string> scope;
	optional<Integer> args{};

	friend auto operator<=>(const Discriminators&, const Discriminators&)
	    = default;
};

class Scope;

class Name {
 public:
	Name() = default;
	Name(string basename, Discriminators discrim)
	    : _basename(std::move(basename))
	    , _discrim(std::move(discrim)) {}
	inline Name(string basename, const Scope& scope,
	            optional<Integer> args = std::nullopt);

	Name& assign(string basename, Discriminators scope);
	Name& assign(string basename, const Scope& scope,
	             optional<Integer> args = std::nullopt);

	friend auto operator<=>(const Name&, const Name&) = default;
	virtual std::ostream& pretty_print(std::ostream& os) const;
	friend std::ostream& operator<<(std::ostream& os, const Name& name) {
		return name.pretty_print(os);
	}
	static bool must_strop(string id) noexcept;
	Name without_args() const { return Name{_basename, {_discrim.scope, {}}}; }

	string _basename;
	Discriminators _discrim;

	virtual ~Name() = default;

	Name(const Name&) = default;
	Name(Name&&) = default;
	Name& operator=(const Name&) = default;
	Name& operator=(Name&&) = default;
};

class NameExpr
    : virtual public Expr
    , public Name {
 public:
	std::ostream& pretty_print(std::ostream& os) const override {
		return Name::pretty_print(os);
	}
	explicit NameExpr(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr) {}
	NameExpr(Token&& t, Name n)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , Name(std::move(n)) {}
};

class Declaration : virtual public Node {
 public:
	Name _name;
	bool _is_export{};
	string language;
	virtual bool is_static_scope() const noexcept { return true; }

	explicit Declaration(Token&& t)
	    : Node(std::move(t)) {}
	explicit Declaration(nullptr_t)
	    : Node(nullptr) {}
	Declaration(Token&& t, Name n, bool exp = {}, string lang = {})
	    : Node(std::move(t))
	    , _name(std::move(n))
	    , _is_export(exp)
	    , language(std::move(lang)) {}
	Declaration(nullptr_t, Name n, bool exp = {}, string lang = {})
	    : Node(nullptr)
	    , _name(std::move(n))
	    , _is_export(exp)
	    , language(std::move(lang)) {}
};

} // namespace AST

namespace std {

template <>
struct hash<AST::Discriminators> {
	auto operator()(const AST::Discriminators& discrim) const -> std::size_t;
};

template <>
struct hash<AST::Name> {
	auto operator()(const AST::Name& name) const -> std::size_t;
};
} // namespace std

namespace AST {

class AliasDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "(alias " << _name << " = ";
		assert(_type);
		return _type->pretty_print(os) << ")\n";
	}

	AliasDecl(Token&& t, Name n)
	    : Node(std::move(t))
	    , Declaration(nullptr, std::move(n)) {}

 protected:
	explicit AliasDecl(Token&& t)
	    : Node(std::move(t))
	    , Declaration(nullptr) {}
};

class VarDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
	bool _is_mut{};
	bool _is_const{};
	bool _is_reference{};
	unique_ptr<Node> _initializer;
	std::ostream& pretty_print(std::ostream& os) const override;

	VarDecl(Token&& t, Name n, bool exp = false)
	    : Node(std::move(t))
	    , Declaration(nullptr, std::move(n), exp) {}
	VarDecl(Token&& t, Name n, bool mut, bool con, bool ref, bool exp = false)
	    : Node(std::move(t))
	    , Declaration(nullptr, std::move(n), exp)
	    , _is_mut(mut)
	    , _is_const(con)
	    , _is_reference(ref) {
		if (_is_mut and _is_const) {
			throw 1;
		}
	}
	explicit VarDecl(Token&& t)
	    : Node(std::move(t))
	    , Declaration(nullptr) {}

 protected:
	explicit VarDecl(nullptr_t)
	    : Node(nullptr)
	    , Declaration(nullptr) {}
	explicit VarDecl(nullptr_t, Name n, bool exp = false)
	    : Node(nullptr)
	    , Declaration(nullptr, std::move(n), exp) {}
	VarDecl(nullptr_t, Name n, bool mut, bool con, bool ref, bool exp = false)
	    : Node(nullptr)
	    , Declaration(nullptr, std::move(n), exp)
	    , _is_mut(mut)
	    , _is_const(con)
	    , _is_reference(ref) {
		if (_is_mut and _is_const) {
			throw 1;
		}
	}
};

class ArgDecl : public VarDecl {
 public:
	explicit ArgDecl(Token&& t)
	    : Node(std::move(t))
	    , VarDecl(nullptr) {
		// arguments have no linkage and can't be exported
		_is_export = false;
	}
	bool _is_implicit{};
	bool _is_anonymous{};
	bool _is_generic{};
	std::ostream& pretty_print(std::ostream& os) const override;

	ArgDecl(Token&& t, Name n)
	    : Node(std::move(t))
	    , VarDecl(nullptr, std::move(n), false) {}

	ArgDecl(Token&& t, Name n, bool mut, bool con, bool ref, bool imp, bool anon,
	        bool gen)
	    : Node(std::move(t))
	    , VarDecl(nullptr, std::move(n), mut, con, ref, false)
	    , _is_implicit(imp)
	    , _is_anonymous(anon)
	    , _is_generic(gen) {}

 protected:
	explicit ArgDecl(nullptr_t)
	    : Node(nullptr)
	    , VarDecl(nullptr) {}
	ArgDecl(nullptr_t, Name n)
	    : Node(nullptr)
	    , VarDecl(nullptr, std::move(n), false) {}
};

class ExprList : public Node {
 public:
	using Node::Node;
	vector<unique_ptr<Expr>> _elems;
	std::ostream& pretty_print(std::ostream& os) const override;
};

class Namespace : public Declaration {
 public:
	vector<unique_ptr<Declaration>> _declarations;
	std::ostream& pretty_print(std::ostream& os) const override;

	Namespace(Token&& t, Name n, bool exp = false)
	    : Node(std::move(t))
	    , Declaration(nullptr, std::move(n), exp) {}

	explicit Namespace(Token&& t)
	    : Node(std::move(t))
	    , Declaration(nullptr) {}

 protected:
	explicit Namespace(nullptr_t)
	    : Node(nullptr)
	    , Declaration(nullptr) {}
	explicit Namespace(nullptr_t, Name n, bool exp = false)
	    : Node(nullptr)
	    , Declaration(nullptr, std::move(n), exp) {}
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

class Operator : public Name {
 public:
	Direction _assoc{};
	Direction _eval_order{};
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "(operator ";
		// Name::pretty_print(os);
		os << _basename;
		return os << ')';
	}
	Operator() = default;
	Operator(Token t);
	static std::unordered_map<Token::Type, std::pair<Direction, Direction>>
	    op_traits;
};

class Literal : public Expr {
 public:
	string _text;
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(literal " << _text << ")";
	}
	Literal(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _text(_tok.str) {}

 protected:
	Literal(string t)
	    : Node(nullptr)
	    , Expr(nullptr)
	    , _text(std::move(t)) {}
};

class IntegerLiteral : public Literal {
 public:
	Integer _value;

	IntegerLiteral(Token&& t)
	    : Node(std::move(t))
	    , Literal(_tok.str)
	    , _value(kblib::parse_integer<Integer>(_tok.str)) {}
};

class FloatLiteral : public Literal {
 public:
	long double _value;

	FloatLiteral(Token&& t)
	    : Node(std::move(t))
	    , Literal(_tok.str)
	    , _value(kblib::lexical_cast<long double>(_tok.str)) {}
};

class StringLiteral : public Literal {
 public:
	string _value;

	StringLiteral(Token&& t)
	    : Node(std::move(t))
	    , Literal(_tok.str)
	    , _value(_tok.str) {}
};

class CharLiteral : public Literal {
 public:
	string _value;

	CharLiteral(Token&& t)
	    : Node(std::move(t))
	    , Literal(_tok.str)
	    , _value(_tok.str) {}
};

class Enumerator : public Literal {
 public:
	string _name;
	unique_ptr<Expr> _value;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "(enumerator " << _name;
		if (_value) {
			os << " = ";
			_value->pretty_print(os);
		}
		return os << ')';
	}
};

class BoolKeyword : public Enumerator {};

/*
 * Expressions / Statements
 */

class PrefixExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _operand;
	std::ostream& pretty_print(std::ostream& os) const override;

	// t should refer to the operator of this prefix expr
	PrefixExpr(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}

 protected:
	PrefixExpr(nullptr_t)
	    : Node(nullptr)
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}
};
class RefExpr : public PrefixExpr {
 public:
	bool _is_mut{};
	RefExpr(Token&& t)
	    : Node(std::move(t))
	    , PrefixExpr(nullptr) {}
};

class PostfixExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _operand;
	std::ostream& pretty_print(std::ostream& os) const override;

	// t should refer to the operator of this postfix expr
	PostfixExpr(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}

 protected:
	PostfixExpr(nullptr_t)
	    : Node(nullptr)
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}
};

class BinaryExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _left, _right;
	std::ostream& pretty_print(std::ostream& os) const override;

	// t should refer to the operator of this infix expr
	BinaryExpr(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}

 protected:
	BinaryExpr(nullptr_t)
	    : Node(nullptr)
	    , Expr(nullptr)
	    , _op(make_unique<Operator>(_tok)) {}
};

class CallExpr : public Expr {
 public:
	unique_ptr<Expr> _fun;
	vector<unique_ptr<Expr>> _args;
	std::ostream& pretty_print(std::ostream& os) const override;

	// t should refer to the opening bracket of this call expr's argument list
	CallExpr(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr) {}

 protected:
	CallExpr(nullptr_t)
	    : Node(nullptr)
	    , Expr(nullptr) {}
};

class RewriteExpr : public Expr {
 public:
	unique_ptr<Expr> _val;
	unique_ptr<Expr> _pattern;
};

class Block : public Expr {
	vector<unique_ptr<Expr>> _expressions;
	std::ostream& pretty_print(std::ostream& os) const override;
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

class JumpExpr : public ControlExpr {
 public:
	string _result_keyword;
	optional<string> _target_label;
};

class ResultExpr
    : public JumpExpr
    , public PrefixExpr {
 public:
};

/*
 * Complex Declarations
 */

class Prototype
    : public Declaration
    , public Expr {
 public:
	bool _is_proc{};
	optional<string> _linkage;
	unique_ptr<Signature> _signature;

	bool is_static_scope() const noexcept override { return true; }
	std::ostream& pretty_print(std::ostream& os) const override;
	// t should be the fn or proc keyword that starts the prototype
	Prototype(Token&& t)
	    : Node(std::move(t))
	    , Declaration(nullptr)
	    , Expr(nullptr)
	    , _is_proc(_tok.type == Token::kw_proc ? true
	               : _tok.type == Token::kw_fn ? false
	                                           : throw 1) {}

 protected:
	Prototype(nullptr_t)
	    : Node(nullptr)
	    , Declaration(nullptr)
	    , Expr(nullptr)
	    , _is_proc(_tok.type == Token::kw_proc ? true
	               : _tok.type == Token::kw_fn ? false
	                                           : throw 1) {}
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

class StructMember {};

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
	vector<unique_ptr<StructMember>> _members;
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
	optional<string> _delete_expr;
	unique_ptr<Node> _body;
	FunctionDef(Token&& t)
	    : Node(std::move(t))
	    , Prototype(nullptr) {}
	std::ostream& pretty_print(std::ostream& os) const override;
};

class Attribute : public Expr {
 public:
	Attribute(Token&& t)
	    : Node(std::move(t))
	    , Expr(nullptr) {}

 protected:
	Attribute(nullptr_t)
	    : Node(nullptr)
	    , Expr(nullptr) {}

 public:
	Name _attr;
	optional<vector<unique_ptr<Expr>>> _args;
	std::ostream& pretty_print(std::ostream& os) const override;
};

/*
 * Substrate
 */

class SubstrateNode : virtual public Expr {
 public:
	vector<unique_ptr<Expr>> _terms;
};

struct prefix_parselet_t {
	auto (*func)(Scope& scope, tokenizer& tk) -> unique_ptr<Expr>;
	int bp{};
	operator decltype(func)() const { return func; }
};
struct infix_parselet_t {
	auto (*func)(Scope& scope, tokenizer& tk, unique_ptr<Expr> left)
	    -> unique_ptr<Expr>;
	int bp_l{};
	int bp_r{bp_l};
	operator decltype(func)() const { return func; }
};

class OverloadSet {
 public:
	auto members() const -> const std::vector<FunctionDef*>& { return _members; }

	auto insert(FunctionDef* f) -> std::size_t;
	auto refine(FunctionDef* f) -> std::size_t;
	auto get(const Name& name) const -> FunctionDef*;
	auto get(std::optional<std::size_t>) const -> FunctionDef*;
	auto lookup(const Name& name) const
	    -> std::variant<std::monostate, std::nullopt_t, std::size_t>;

	OverloadSet() = default;
	OverloadSet(FunctionDef* singular)
	    : _members{}
	    , _fn_unknown{} {
		assert(singular);
		if (auto args = singular->_name._discrim.args) {
			_members.push_back(singular);
		} else {
			_fn_unknown = singular;
		}
		return;
	}

 private:
	std::vector<FunctionDef*> _members;
	FunctionDef* _fn_unknown;
	friend class Scope;

	auto find(const Name& name) -> decltype(_members)::iterator {
		return std::find_if(_members.begin(), _members.end(),
		                    [&](FunctionDef* f) { return f->_name == name; });
	}
	auto find(const Name& name) const -> decltype(_members)::const_iterator {
		return std::find_if(_members.begin(), _members.end(),
		                    [&](FunctionDef* f) { return f->_name == name; });
	}
};

class Scope {
 public:
	struct entity {
		using type
		    = std::variant<AliasDecl*, EnumDecl*, Namespace*, VarDecl*,
		                   FunctionDef*, StructProto*, TraitDecl*,
		                   DataMemberDecl*, FunctionMemberDecl*, OverloadSet>;
		constexpr static std::array names = {
		    "AliasDecl"sv,   "EnumDecl"sv,       "Namespace"sv,
		    "VarDecl"sv,     "FunctionDef"sv,    "StructProto"sv,
		    "TraitDecl"sv,   "DataMemberDecl"sv, "FunctionMemberDecl"sv,
		    "OverloadSet"sv,
		};
		type decl;
		entity(type t)
		    : decl(t) {}
		entity() = delete;
		entity& operator=(type t) {
			decl = t;
			return *this;
		}

		friend std::ostream& operator<<(std::ostream& os, const entity& ent) {
			auto& d = ent.decl;
			os << "[" << d.index() << "] ";
			if (d.valueless_by_exception()) {
				os << "invalid";
			} else {
				assert(d.index() <= entity::names.size());
				os << entity::names[d.index()];
			}
			return os;
		}
	};

	std::unordered_map<Name, entity> symbols;
	Name name;
	using value_type = decltype(symbols)::value_type;
	struct iterator {
		decltype(symbols)::iterator _val;
		// Represents either nothing, the _fn_unknown, or the index in _members
		std::variant<std::monostate, std::nullopt_t, std::size_t> _subindex;
	};

	std::ostream& pretty_print(std::ostream& os) const;

	template <std::derived_from<Declaration> T>
	iterator add_name(T* ent) requires requires {
		std::get<T*>(std::declval<entity&>().decl);
	}
	{
		auto [i, n] = do_add_name(ent);
		assert(n);
		return i;
	}

	iterator add_name(FunctionDef* ent);

 private:
	template <typename T>
	auto do_add_name(T* ent) -> std::pair<iterator, bool> {
		assert(ent);
		auto& e_name = ent->Declaration::_name;
		if (auto existing = symbols.find(e_name); existing != symbols.end()) {
			if (std::holds_alternative<FunctionDef*>(existing->second.decl)) {
				return {{existing, std::monostate{}}, false};
			} else if (auto ov
			           = std::get_if<OverloadSet>(&existing->second.decl)) {
				auto idx = ov->lookup(e_name);
				assert(std::holds_alternative<std::nullopt_t>(idx));
				return {{existing, idx}, false};
			} else {
				throw 1;
				return {{existing, std::monostate{}}, false};
			}
		} else {
			auto [s, _] = symbols.insert_or_assign(e_name, ent);
			assert(_);
			assert(s != symbols.end());
			std::clog << "Entity: " << e_name << " is " << s->second << "\n";
			return {{s, std::monostate{}}, true};
		}
	}

	iterator add_name(Declaration* ent);

 public:
	template <std::derived_from<Declaration> T>
	iterator refine_name(iterator old, T* ent) {
		symbols.erase(old._val);
		return add_name(ent);
	}
	iterator refine_name(iterator old, FunctionDef* ent);

	value_type* find_name(const Name& name_);
	std::unordered_map<Token::Type, prefix_parselet_t> _prefix_parselets;
	std::unordered_map<Token::Type, infix_parselet_t> _infix_parselets;
	std::vector<std::vector<Token::Type>> _prefix_precedence_classes;
	std::vector<std::vector<Token::Type>> _infix_precedence_classes;

	void reg1(Token::Type token, prefix_parselet_t parselet) {
		_prefix_parselets[token] = parselet;
	}
	void reg2(Token::Type token, infix_parselet_t parselet) {
		_infix_parselets[token] = parselet;
	}

	void reg1(std::initializer_list<Token::Type> tokens,
	          prefix_parselet_t parselet) {
		for (auto t : tokens) {
			reg1(t, parselet);
		}
	}
	void reg2(std::initializer_list<Token::Type> tokens,
	          infix_parselet_t parselet) {
		for (auto t : tokens) {
			reg2(t, parselet);
		}
	}

	void default_ops();
	Scope() { default_ops(); }
	~Scope();
};

struct ModuleImportDesc {
	std::string _name;
	bool _is_export{};
	vector<unique_ptr<Node>> _attributes;
};

class Module
    : virtual public Node
    , public Namespace {
 public:
	vector<ModuleImportDesc> _imports;
	std::ostream& pretty_print(std::ostream& os) const override;

	Module(Token&& t, string str, bool is_export)
	    : Node(std::move(t))
	    , Namespace(nullptr, Name(str, Scope()), is_export) {}
};

auto parse_module(tokenizer& tk, Scope& scope) -> unique_ptr<Module>;
auto parse_expr(tokenizer& tk, Scope& scope,
                std::initializer_list<Token::Type> end_tok_types = {},
                int bp = 0) -> unique_ptr<Expr>;
auto parse_qualified_name(tokenizer& tk, const Scope& scope, Name& name)
    -> Token;
auto parse_attr_seq(tokenizer& tk, Scope& scope) -> vector<unique_ptr<Node>>;

// avoid a dynamic_cast without opening up possibility of memory leak
template <typename Class, typename Base, typename... Args>
auto make_unique_for_modify(std::unique_ptr<Base>& owner, Args&&... args) {
	owner.reset();
	auto object = new Class(std::forward<Args>(args)...);
	owner.reset(object);
	return object;
}

inline Name::Name(std::string basename, const Scope& scope,
                  optional<Integer> args)
    : _basename(basename)
    , _discrim{scope.name._discrim.scope, args} {}

} // namespace AST

#endif // AST_HPP
