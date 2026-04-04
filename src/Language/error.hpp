/* *****************************************************************************
 * Vellum
 * Copyright (c) 2023 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/
#ifndef ERROR_HPP
#define ERROR_HPP

#include "token.hpp"
#include <format>
#include <source_location>
#include <string>

class syntax_error : public std::invalid_argument {
 public:
	using std::invalid_argument::invalid_argument;
};

class unexpected : public syntax_error {
 public:
	Token found;
	std::string expected;

	static std::string format_str(Token found, std::string expected);

	unexpected(const Token& found, std::string expected)
	    : syntax_error(format_str(found, expected))
	    , found(found)
	    , expected(std::move(expected)) {}
};

class constraint_error : public syntax_error {
 public:
	constraint_error(const Token& loc, std::string desc)
	    : syntax_error(desc)
	    , _loc(loc) {}

	Token _loc;
};

class not_implemented_exception {
 public:
	not_implemented_exception(std::string desc,
	                          std::source_location sl
	                          = std::source_location::current())
	    : _desc(std::format("Not yet implemented: {}. In {} at {}:{}", desc,
	                        sl.function_name(), sl.file_name(), sl.line()))
	    , _sl(sl) {}

	const std::string& what() const { return _desc; }

 private:
	std::string _desc;
	std::source_location _sl;
};

#endif // ERROR_HPP
