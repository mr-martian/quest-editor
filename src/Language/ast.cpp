/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {

std::ostream& indent(std::ostream& os) {
	auto depth = os.iword(Node::indent_idx);
	return os << kblib::repeat(' ', kblib::to_unsigned(depth * 2));
}
std::ostream& nest(std::ostream& os) {
	++os.iword(Node::indent_idx);

	return os;
}
std::ostream& unnest(std::ostream& os) {
	auto& depth = os.iword(Node::indent_idx);
	if (--depth <= 0) {
		depth = 0;
	}
	return os;
}

unique_ptr<AST::SubstrateNode> AST::Node::substrate() const { throw 0; }

std::ostream& Node::print_attrs(std::ostream& os) const {
	if (not _attributes.empty()) {
		os << "#[";
		auto first = true;
		for (auto& a : _attributes) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			a->pretty_print(os);
		}
		os << "]";
	}
	return os;
}

auto NullExpr::pretty_print(std::ostream& os) const -> std::ostream& {
	os << "(null ";
	print_attrs(os);
	return os << ")";
}

std::ostream& Name::pretty_print(std::ostream& os) const {
	os << "(Name ";
	for (auto& s : _discrim.scope) {
		os << s << "::";
	}
	os << '(';
	if (must_strop(_basename)) {
		os << '`' << _basename << '`';
	} else {
		os << _basename;
	}
	os << ')';
	if (_discrim.args) {
		os << '!' << *_discrim.args;
	}
	return os << ")";
}

static const auto reserved_ident_pattern = reflex::Pattern(
    reflex::Matcher::convert(
        // R"re( ([^_\p{L}\d]) | (^(\d|_([_A-Z]|[0-9]+$))) )re",
        R"re( ([^_a-zA-Z0-9]) | (^(\d|_([_A-Z]|[0-9]+$))) )re",
        reflex::convert_flag::unicode | reflex::convert_flag::lex
            | reflex::convert_flag::freespace),
    "rw");
bool Name::must_strop(std::string id) noexcept {
	return id.empty() or reflex::Matcher(reserved_ident_pattern, id).scan();
}

IntegerTypeID::IntegerTypeID(Token&& t)
    : Node(std::move(t)) {
	auto id = std::string_view(_tok.str);
	assert(id.size() >= 2);
	assert(id[0] == 'i');
	id.remove_prefix(1);
	_width = kblib::fromStr<Integer>(id, "Integer");
	assert(_width >= 1);
}
UnsignedTypeID::UnsignedTypeID(Token&& t)
    : Node(std::move(t)) {
	auto id = std::string_view(_tok.str);
	assert(id.size() >= 2);
	assert(id[0] == 'u');
	id.remove_prefix(1);
	_width = kblib::fromStr<Integer>(id, "Integer");
	assert(_width >= 1);
}

std::ostream& Prototype::pretty_print(std::ostream& os) const {
	os << "(Prototype ";
	if (_is_export) {
		os << "export ";
	}
	if (_linkage) {
		os << "extern " << kblib::quoted(*_linkage) << ' ';
	}
	os << (_is_proc ? "proc " : "fn ");
	_name.pretty_print(os) << "\n" << nest << indent;
	_signature->pretty_print(os);
	return os << unnest << ")";
}

std::ostream& FunctionDef::pretty_print(std::ostream& os) const {
	os << "(FunctionDef\n" << nest << indent;
	Prototype::pretty_print(os);
	if (_body) {
		os << '\n' << indent << "(body ";
		_body->pretty_print(os);
		os << ")";
	}
	os << ")" << unnest;
	return os;
}

std::ostream& VarDecl::pretty_print(std::ostream& os) const {
	os << "(let (";
	if (_is_reference) {
		os << "&";
	}
	if (_is_mut) {
		os << "mut ";
	}
	if (_is_const) {
		os << "const ";
	}
	os << _name << ')';
	if (_type) {
		os << " : ";
		_type->pretty_print(os);
	}
	if (_initializer) {
		if (auto braced_initializer
		    = dynamic_cast<const ExprList*>(_initializer.get())) {
			os << '{';
			braced_initializer->pretty_print(os);
			os << '}';
		} else {
			os << " = ";
			_initializer->pretty_print(os);
		}
	}
	return os << ")";
}

std::ostream& ArgDecl::pretty_print(std::ostream& os) const {
	os << "(arg (";
	if (_is_reference) {
		os << "&";
	}
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
	os << ')';
	if (_type) {
		os << " : ";
		_type->pretty_print(os);
	}
	if (_initializer) {
		os << " = ";
		_initializer->pretty_print(os);
	}
	return os << ")";
}

std::ostream& Signature::pretty_print(std::ostream& os) const {
	os << "(signature (";
	auto first = true;
	for (auto& arg : _args) {
		if (not std::exchange(first, false)) {
			os << ", ";
		}
		arg->pretty_print(os);
	}
	if (_returns.empty()) {

	} else if (_returns.size() == 1) {
		os << ") -> (";
		_returns.front()->pretty_print(os);
	} else if (_returns.size() > 1) {
		os << " -> ";
		first = true;
		for (auto& rt : _returns) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			rt->pretty_print(os) << " ";
		}
	}
	return os << "))";
}

std::ostream& ArrayPrefixExpr::pretty_print(std::ostream& os) const {
	os << "(Array [";
	auto first = true;
	for (auto& dim : _args) {
		if (not std::exchange(first, false)) {
			os << ',';
			if (dim) {
				os << ' ' << *dim;
			}
		} else if (dim) {
			os << *dim;
		}
	}
	os << "] ";
	if (_rhs) {
		return _rhs->pretty_print(os) << ")";
	} else {
		throw 1;
	}
}

std::ostream& TupleTypeID::pretty_print(std::ostream& os) const {
	os << "(Tuple ";
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

std::ostream& UnionTypeID::pretty_print(std::ostream& os) const {
	os << "(Union ";
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

std::ostream& ExprList::pretty_print(std::ostream& os) const {
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

std::ostream& PrefixExpr::pretty_print(std::ostream& os) const {
	os << "(prefix ";
	assert(_op);
	_op->pretty_print(os);
	assert(_operand);
	os << ' ';
	_operand->pretty_print(os);
	return os << ')';
}

std::ostream& PostfixExpr::pretty_print(std::ostream& os) const {
	os << "(postfix ";
	assert(_op);
	_op->pretty_print(os);
	assert(_operand);
	os << ' ';
	_operand->pretty_print(os);
	return os << ')';
}

std::ostream& BinaryExpr::pretty_print(std::ostream& os) const {
	os << "(infix ";
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

std::ostream& CallExpr::pretty_print(std::ostream& os) const {
	assert(_fun);
	os << "(call\n" << nest << indent;
	_fun->pretty_print(os) << '\n';
	os << indent << "(";
	auto first = true;
	for (auto& arg : _args) {
		if (not std::exchange(first, false)) {
			os << ", ";
		}
		assert(arg);
		arg->pretty_print(os);
	}
	return os << unnest << "))";
}

std::ostream& Attribute::pretty_print(std::ostream& os) const {
	os << "(attribute " << _attr << " ";
	if (_args) {
		os << '(';
		auto first = true;
		for (auto& arg : *_args) {
			if (not std::exchange(first, false)) {
				os << ", ";
			}
			assert(arg);
			arg->pretty_print(os);
		}
		os << ')';
	}
	return os << ")";
}

std::ostream& Block::pretty_print(std::ostream& os) const {
	os << "(block {" << nest;
	for (auto& ex : _expressions) {
		assert(ex);
		ex->pretty_print(os << "\n" << indent);
	}
	return os << unnest << "})";
}

std::ostream& Namespace::pretty_print(std::ostream& os) const {
	os << "(namespace " << _name << "\n" << nest;
	for (auto& decl : _declarations) {
		assert(decl);
		os << indent;
		decl->pretty_print(os) << '\n';
	}
	return os << unnest << indent << ")";
}

std::ostream& Module::pretty_print(std::ostream& os) const {

	/*
	   using namespace std::literals;
	   os << "(import "sv << _name;
	   if (_is_export) {
	      os << "export "sv;
	   }
	   if (not language.empty() and language != "Vellum"sv) {
	      os << '[' << kblib::quoted(language) << ']';
	   }
	   return os << ")\n"sv;
	   */
	os << "(module " << _name << '\n' << nest;
	for (auto& import : _imports) {
		os << indent << "(import " << import._name;
		if (import._is_export) {
			os << " export";
		}
		os << ")\n";
	}
	for (auto& decl : _declarations) {
		decl->pretty_print(os << indent) << '\n';
	}
	return os << unnest << indent << ")\n";
}

std::ostream& LoopExpr::pretty_print(std::ostream& os) const {
	os << "(loop ";
	write_label(os);
	os << nest;

	_body->pretty_print(os << '\n' << indent);

	if (_else) {
		os << '\n' << indent << "(else ";
		_else->pretty_print(os) << ")";
	}
	return os << ")" << unnest;
}

std::ostream& LoopConstraint::pretty_print(std::ostream& os) const {
	os << "(" << (_target ? "while " : "until ");
	_condition->pretty_print(os) << ")";
	return os;
}

std::ostream& WhileExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'while' ";
	write_label(os);
	os << nest;

	_constraint->pretty_print(os << '\n' << indent);

	_body->pretty_print(os << '\n' << indent);

	write_else(os << '\n' << indent);
	return os << ")" << unnest;
}

std::ostream& DoWhileExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'do' ";
	write_label(os);
	os << '\n' << nest << indent;

	_body->pretty_print(os) << '\n';

	_constraint->pretty_print(os);

	write_else(os);
	return os << ")" << unnest;
}

std::ostream& ForLoopExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'for' ";
	write_label(os);
	os << '\n' << nest << indent;

	_induction_variable->pretty_print(os);

	_body->pretty_print(os) << '\n';

	write_else(os);
	return os << ")" << unnest;
}

std::ostream& ForInExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'for' ";
	write_label(os);
	os << '\n' << nest << indent;

	os << "(in ";
	_induction_variable->pretty_print(os);
	_range_expr->pretty_print(os);
	os << ")\n";

	_body->pretty_print(os) << '\n';

	write_else(os);
	return os << ")" << unnest;
}

std::ostream& ForRangeExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'for' ";
	write_label(os);
	os << '\n' << nest << indent;

	os << "(range\n" << nest << indent;
	_induction_variable->pretty_print(os);
	if (_r_end) {
		os << '\n' << indent << "(-> ";
		_r_end->pretty_print(os);
		os << ")";
	}
	if (_update) {
		_update->pretty_print(os << '\n' << indent);
	}
	os << ")" << unnest;

	_induction_variable->pretty_print(os << '\n' << indent);

	_body->pretty_print(os << '\n' << indent);

	write_else(os);
	return os << ")" << unnest;
}

std::ostream& GForExpr::pretty_print(std::ostream& os) const {
	os << "(loop 'for' " << nest;
	write_label(os);
	os << '\n' << nest << indent;

	os << "(range ";
	_induction_variable->pretty_print(os);
	os << '\n' << nest << indent;
	_constraint->pretty_print(os);
	if (_update) {
		_update->pretty_print(os << '\n' << indent);
	}
	os << ")\n" << unnest << indent;

	_induction_variable->pretty_print(os);

	_body->pretty_print(os) << '\n';

	write_else(os);
	return os << ")" << unnest;
}

std::ostream& IfExpression::pretty_print(std::ostream& os) const {
	os << "(if " << (_target ? "'if'" : "'unless'");
	os << '\n' << nest << indent << '(';
	_condition->pretty_print(os);
	os << ") (";
	_body->pretty_print(os);
	os << ")";
	if (_else_body) {
		os << " (";
		_else_body->pretty_print(os);
		os << ')';
	}
	os << ')';
	return os << ")" << unnest;
}

std::ostream& InvertedIfExpr::pretty_print(std::ostream& os) const {
	os << "(trailing-if " << (_target ? "'if'" : "'unless'");
	os << '\n' << nest << indent << '(';
	_expr->pretty_print(os);
	os << ") (";
	_condition->pretty_print(os);
	os << ")";
	if (_else_expr) {
		os << " (";
		_else_expr->pretty_print(os);
		os << ')';
	}
	os << ')';
	return os << ")" << unnest;
}

} // namespace AST
std::size_t std::hash<AST::Discriminators>::operator()(
    const AST::Discriminators& discrim) const {
	const auto scope_hash = kblib::FNV_hash<>{}(discrim.scope);
	const auto args_hash = kblib::FNV_hash<>{}(discrim.args, scope_hash);
	return args_hash;
}
std::size_t std::hash<AST::Name>::operator()(const AST::Name& name) const {
	return kblib::FNV_hash<std::string_view>{}(
	    name._basename, std::hash<AST::Discriminators>{}(name._discrim));
}
