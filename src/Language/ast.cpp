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
	throw 0;
}

std::ostream& Name::pretty_print(std::ostream& os) const {
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
	return os;
}

Discriminators::~Discriminators() = default;

std::ostream& Prototype::pretty_print(std::ostream& os) const {
	if (_linkage) {
		os << "extern " << kblib::quoted(*_linkage) << ' ';
		os << (_is_proc ? "proc" : "fn");
	}
	throw 0;
}

std::ostream& VarDecl::pretty_print(std::ostream& os) const {
	os << "let ";
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
	return os << ";";
}

} // namespace AST
