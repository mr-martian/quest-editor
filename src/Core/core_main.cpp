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
	auto file = kblib::get_file_contents<std::u8string>(argv[1]).value_or(u8"");
	quest::utf8_rope r{file};
	std::cout << "read " << file.size() << " bytes\n"
	          << "rope size_all(): " << r.size_all() << "\nwc: " << std::flush;
	std::system((std::string("wc ") + argv[1]).c_str());
	std::cout << '\n';
	r.debug_print_tree();
	assert(r.size_chars() == file.size());

	const int limit_arg = [&] {
		if (argc > 2) {
			return std::atoi(argv[2]);
		} else {
			return 16;
		}
	}();
	int limit = limit_arg;
	int discrete_count{};
	auto it = r.begin();
	for (std::size_t i = 0; i < file.size(); ++i) {
		bool error{};
		if (auto c = r[i]; file[i] != c) {
			error = true;
			if (--limit >= 0) {
				std::cout << kblib::escapify(
				    kblib::concat("index mismatch: file[", i, "] (", file[i],
				                  ") != r[", i, "] (", c, ")"))
				          << '\n';
			}
		}
		if (auto c = *it++; file[i] != c) {
			error = true;
			if (--limit >= 0) {
				std::cout << kblib::escapify(kblib::concat(
				    "index mismatch: file[", i, "] (", file[i], ") != *it (", c,
				    ")")) << '\n';
			}
		}
		if (auto c = *r.nth(i); file[i] != c) {
			error = true;
			if (--limit >= 0) {
				std::cout << kblib::escapify(
				    kblib::concat("index mismatch: file[", i, "] (", file[i],
				                  ") != *r.nth[", i, "] (", c, ")"))
				          << '\n';
			}
		}
		if (auto c = *(r.begin() + static_cast<std::ptrdiff_t>(i));
		    file[i] != c) {
			error = true;
			if (--limit >= 0) {
				std::cout << kblib::escapify(
				    kblib::concat("index mismatch: file[", i, "] (", file[i],
				                  ") != (*r.begin() + ", i, ") (", c, ")"))
				          << '\n';
			}
		}
		if (error)
			++discrete_count;
	}
	if (limit == limit_arg) {
		std::cout << "indexing verified\n";
	} else {
		if (limit < 0) {
			std::cout << -limit << " more...\n";
		}
		std::cout << "verification failed " << (limit_arg - limit) << " times at "
		          << discrete_count << " locations\n";
	}
}
