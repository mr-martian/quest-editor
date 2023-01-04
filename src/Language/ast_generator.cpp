/* *****************************************************************************
 * Vellum
 * Copyright (c) 2022 Bee (@killerbee13), Daniel (@mr-martian), Dusty
 * (@d-us-vb), Richard (@CodeTriangle)
 * ****************************************************************************/

#include "kblib/io.h"
#include "kblib/stringops.h"
#include <cassert>

int main(int argc, char** argv) {
	assert(argc >= 1);
	std::ifstream list(argv[1]);
	if (not list) {
		std::cerr << "file not found\n";
		return 1;
	}
	std::string line;
	std::string category;
	std::string type;
	while (list >> kblib::get_line(line)) {
		if (line.empty()) {
			continue;
		}
		if (line[0] == '@') {
		}
	}
}
