/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {

unique_ptr<AST::SubstrateNode> AST::Node::substrate() const { throw 0; }

std::ostream& AST::Node::pretty_print(std::ostream& os) const { throw 0; }

} // namespace AST
