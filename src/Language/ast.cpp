/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {

unique_ptr<AST::SubstrateNode> AST::Node::substrate() const { throw 0; }

Name::Name(const Name& other)
    : _basename(other._basename)
    , _discrim() {
	// throw 0;
}

std::ostream& Name::pretty_print(std::ostream& os) const {
	os << "(Name ";
	if (_discrim.signature) {
		os << '(';
		_discrim.signature->pretty_print(os);
		os << ')';
	}
	for (auto& s : _discrim.scope) {
		os << s << "::";
	}
	os << _basename;
	if (_discrim.args) {
		os << '!' << *_discrim.args;
	}
	return os << ")";
}

Discriminators::~Discriminators() = default;

std::ostream& Prototype::pretty_print(std::ostream& os) const {
	os << "(Prototype ";
	if (_is_export) {
		os << "export ";
	}
	if (_linkage) {
		os << "extern " << kblib::quoted(*_linkage) << ' ';
	}
	os << (_is_proc ? "proc " : "fn ");
	_name.pretty_print(os) << "\n\t";
	_signature->pretty_print(os);
	return os << ")";
}

std::ostream& VarDecl::pretty_print(std::ostream& os) const {
	os << "(let ";
	if (_is_reference) {
		os << "&";
	}
	if (_is_mut) {
		os << "mut ";
	}
	if (_is_const) {
		os << "const ";
	}
	os << _name;
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

std::ostream& ArrayTypeID::pretty_print(std::ostream& os) const {
	os << "(Array [";
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
	os << "] ";
	if (_element_type) {
		return _element_type->pretty_print(os) << ")";
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

std::ostream& ImportDecl::pretty_print(std::ostream& os) const {
	using namespace std::literals;
	os << "(import "sv << _name;
	if (_is_export) {
		os << "export "sv;
	}
	if (not language.empty() and language != "Vellum"sv) {
		os << '[' << kblib::quoted(language) << ']';
	}
	return os << ")\n"sv;
}

std::ostream& ArgDecl::pretty_print(std::ostream& os) const {
	os << "(arg ";
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

std::ostream& Namespace::pretty_print(std::ostream& os) const {
	os << "(namespace " << _name << "{";
	for (auto& decl : _declarations) {
		assert(decl);
		decl->pretty_print(os);
	}
	return os << "})";
}

std::ostream& UnaryExpr::pretty_print(std::ostream& os) const {
	os << '(';
	assert(_op);
	_op->pretty_print(os);
	assert(_operand);
	os << ' ';
	_operand->pretty_print(os);
	return os << ')';
}

std::ostream& BinaryExpr::pretty_print(std::ostream& os) const {
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

std::ostream& CallExpr::pretty_print(std::ostream& os) const {
	assert(_fun);
	os << "(call " << *_fun << " (";
	auto first = true;
	for (auto& arg : _args) {
		if (not std::exchange(first, false)) {
			os << ", ";
		}
		assert(arg);
		arg->pretty_print(os);
	}
	return os << "))";
}

std::ostream& Block::pretty_print(std::ostream& os) const {
	os << "(block {";
	auto first = true;
	for (auto& ex : _expressions) {
		if (not std::exchange(first, false)) {
			os << "; ";
		}
		assert(ex);
		ex->pretty_print(os);
	}
	return os << "})";
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
	os << "(module " << _name << '\n';
	for (auto& import : _imports) {
		os << "\t(import " << import._name;
		if (import._is_export) {
			os << " export";
		}
		os << ")\n";
	}
	for (auto& decl : _declarations) {
		decl->pretty_print(os) << ";\n";
	}
	return os << ")\n";
}

} // namespace AST
std::size_t std::hash<AST::Discriminators>::operator()(
    const AST::Discriminators& discrim) const {
	const auto scope_hash = kblib::FNV_hash<>{}(discrim.scope);
	const auto args_hash = kblib::FNV_hash<>{}(discrim.args, scope_hash);
	// TODO(killerbee13): hash AST nodes
	const auto sig_hash = args_hash;
	return sig_hash;
}
std::size_t std::hash<AST::Name>::operator()(const AST::Name& name) const {
	return kblib::FNV_hash<std::string_view>{}(
	    name._basename, std::hash<AST::Discriminators>{}(name._discrim));
}
