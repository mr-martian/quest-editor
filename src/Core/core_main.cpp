/* *****************************************************************************
 * Quest Text Editor
 * Copyright (c) 2021 Daniel Swanson (@mr-martian), d-us-vb,
 *   killerbee (@killerbee13), Richard (@CodeTriangle)
 *
 * <Insert License Here>
 *
 * ****************************************************************************/

#include "utf8_rope.hpp"
#include <iostream>
#include <kblib/io.h>

int main(int argc, char** argv) {
	if (argc == 0) {
		return 0;
	}
	auto file = kblib::get_file_contents(argv[1]).value_or("");
	quest::utf8_rope r{file};
	std::cout << "read " << file.size() << " bytes\n"
	          << "rope size_all(): " << r.size_all() << "\nwc: " << std::flush;
	std::system((std::string("wc ") + argv[1]).c_str());
	std::cout << '\n';
	r.debug_print_tree();
	assert(r.size_chars() == file.size());
}
