/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
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
	[[nodiscard]] bool eof() const noexcept { return _cur != _end; }

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
	virtual std::ostream& pretty_print(std::ostream& os) const = 0;

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

	std::ostream& pretty_print(std::ostream& os) const override {
		return os << _name;
	}

	string _name;
};

class IntegerTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << 'i' << _width;
	}
};
class UnsignedTypeID : public BuiltinTypeID {
 public:
	Integer _width{};
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << 'u' << _width;
	}
};

class OwnerTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _owned_type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "owner ";
		if (_owned_type) {
			return _owned_type->pretty_print(os);
		} else {
			throw 1;
		}
	}
};
class SharedTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _owned_type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "shared ";
		if (_owned_type) {
			return _owned_type->pretty_print(os);
		} else {
			throw 1;
		}
	}
};
class ArrayTypeID : public BuiltinTypeID {
 public:
	unique_ptr<Expr> _element_type;
	vector<optional<Integer>> _bounds;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << '[';
		auto first = true;
		for (auto dim : _bounds) {
			if (not std::exchange(first, false)) {
				os << ',';
				if (dim) {
					os << ' ' << *dim;
				}
			} else if (dim) {
				os << *dim;
			}
		}
		os << ']';
		if (_element_type) {
			return _element_type->pretty_print(os);
		} else {
			throw 1;
		}
	}
};

class TupleTypeID : public Expr {
 public:
	vector<unique_ptr<Expr>> _member_types;

	// Node interface
 public:
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "tuple(";
		auto first = true;
		for (auto& type : _member_types) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			assert(type);
			type->pretty_print(os);
		}
		return os << ')';
	}
};
class UnionTypeID : public Expr {
 public:
	set<unique_ptr<Expr>> _member_types;

	// Node interface
 public:
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "union(";
		auto first = true;
		for (auto& type : _member_types) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			assert(type);
			type->pretty_print(os);
		}
		return os << ')';
	}
};

class NoneTypeID : public TupleTypeID {
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "None";
	}
};
class NoreturnTypeID : public UnionTypeID {
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "Noreturn";
	}
};
class ThisTypeID : public BuiltinTypeID {
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "This";
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
};

struct Discriminators {
	vector<string> scope;
	optional<Integer> args;
	unique_ptr<Signature> signature;

	Discriminators() = default;
	Discriminators(Discriminators&&) = default;
	Discriminators& operator=(Discriminators&&) = default;
	~Discriminators();
	friend auto operator<=>(const Discriminators&, const Discriminators&)
	    = default;
};

class Scope;

class Name {
 public:
	Name() = default;
	Name(const Name&);
	Name(Name&&) = default;
	Name(string basename, Discriminators discrim)
	    : _basename(std::move(basename))
	    , _discrim(std::move(discrim)) {}
	Name(string basename, const Scope& scope);

	Name& assign(string basename, Discriminators scope);
	Name& assign(string basename, const Scope& scope);

	friend auto operator<=>(const Name&, const Name&) = default;
	virtual std::ostream& pretty_print(std::ostream& os) const;
	friend std::ostream& operator<<(std::ostream& os, const Name& name) {
		return name.pretty_print(os);
	}

	string _basename;
	Discriminators _discrim;
};

class NameExpr
    : virtual public Expr
    , public Name {
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << static_cast<const Name&>(*this);
	}
};

class Declaration : virtual public Node {
 public:
	Name _name;
	bool _is_export{};
	string language;
	virtual bool is_static_scope() const noexcept { return true; }
};

} // namespace AST

namespace std {
template <>
struct hash<AST::Name> {
	auto operator()(const AST::Name& name) const -> std::size_t { return 0; }
};
} // namespace std

namespace AST {

class Scope {
 public:
	std::unordered_map<Name, Declaration*> symbols;
	Name name;
};

class AliasDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "alias " << _name << " = ";
		assert(_type);
		return _type->pretty_print(os) << ";";
	}
};

class VarDecl : public Declaration {
 public:
	unique_ptr<Expr> _type;
	bool _is_mut{};
	bool _is_const{};
	unique_ptr<Node> _initializer;
	std::ostream& pretty_print(std::ostream& os) const override;
};

class ArgDecl : public VarDecl {
 public:
	ArgDecl() { _is_export = false; }
	bool _is_implicit;
	bool _is_anonymous;
	bool _is_generic;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "let ";
		if (_is_mut) {
			os << "mut ";
		}
		if (_is_const) {
			os << "const ";
		}
		if (_is_implicit) {
			os << "$";
		}
		if (not _is_anonymous) {
			os << _name;
		}
		if (_type) {
			os << " : ";
			_type->pretty_print(os);
		}
		if (_initializer) {
			os << " = ";
			_initializer->pretty_print(os);
		}
		return os << ";";
	}
};

class ExprList : public Node {
 public:
	vector<unique_ptr<Expr>> _elems;
	std::ostream& pretty_print(std::ostream& os) const override {
		auto first = true;
		for (auto& ex : _elems) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			assert(ex);
			ex->pretty_print(os);
		}
		return os;
	}
};

class Namespace : public Declaration {
 public:
	vector<unique_ptr<Declaration>> _declarations;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "namespace " << _name << "{";
		for (auto& decl : _declarations) {
			assert(decl);
			decl->pretty_print(os);
		}
		return os << "}";
	}
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
		os << "(operator " << static_cast<const Name&>(*this) << ')';
	}
};

class Literal : public Expr {
 public:
	string _text;
	std::ostream& pretty_print(std::ostream& os) const override {
		return os << "(literal " << _text << ")";
	}
};

class IntegerLiteral : public Literal {};

class FloatLiteral : public Literal {};

class StringLiteral : public Literal {};

class Enumerator : public Literal {
 public:
	string _name;
	unique_ptr<Expr> _value;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << _name;
		if (_value) {
			os << " = ";
			_value->pretty_print(os);
		}
		return os;
	}
};

class BoolKeyword : public Enumerator {};

/*
 * Expressions / Statements
 */

class UnaryExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _operand;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << '(';
		assert(_op);
		_op->pretty_print(os);
		assert(_operand);
		os << ' ';
		_operand->pretty_print(os);
		return os << ')';
	}
};

class BinaryExpr : public Expr {
 public:
	unique_ptr<Operator> _op;
	unique_ptr<Expr> _left, _right;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << '(';
		assert(_op);
		_op->pretty_print(os);
		assert(_left);
		os << ' ';
		_left->pretty_print(os);
		assert(_right);
		os << ", ";
		_right->pretty_print(os);
		return os << ')';
	}
};

class CallExpr : public Expr {
 public:
	unique_ptr<NameExpr> _fun;
	vector<unique_ptr<Expr>> _args;
	std::ostream& pretty_print(std::ostream& os) const override {
		assert(_fun);
		os << "((" << *_fun << ") (";
		auto first = true;
		for (auto& arg : _args) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			assert(arg);
			arg->pretty_print(os);
		}
		os << "))";
	}
};

class RewriteExpr : public Expr {
 public:
	unique_ptr<Expr> _val;
	unique_ptr<Expr> _pattern;
};

class Block : public Expr {
	vector<unique_ptr<Expr>> _expressions;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "{";
		auto first = true;
		for (auto& ex : _expressions) {
			if (not std::exchange(first, false)) {
				os << "; ";
			}
			assert(ex);
			ex->pretty_print(os);
		}
		os << "}";
	}
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

class Prototype
    : public Declaration
    , public Expr {
 public:
	bool _is_proc{};
	optional<string> _linkage;
	unique_ptr<Signature> _signature;

	bool is_static_scope() const noexcept override { return true; }
	std::ostream& pretty_print(std::ostream& os) const override;
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

struct ModuleImport {
	std::string _name;
	bool _is_export;
};

class Module
    : virtual public Node
    , public Namespace {
 public:
	vector<ModuleImport> _imports;
	std::ostream& pretty_print(std::ostream& os) const override {
		os << "module " << _name << ';';
		for (auto& import : _imports) {
			if (import._is_export) {
				os << " export";
			}
			os << " import " << import._name << ';';
		}
		(void)this->_declarations;
	}
};

/*
 * Substrate
 */

class SubstrateNode : virtual public Node {
 public:
	vector<unique_ptr<Expr>> _terms;
};

auto parse_module(tokenizer& tk) -> unique_ptr<Module>;

} // namespace AST

#endif // AST_HPP
