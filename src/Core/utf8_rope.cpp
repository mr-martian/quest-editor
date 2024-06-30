/* *****************************************************************************
 * This file is part of the Quest Text Editor project.
 * Copyright (c) 2021 Daniel Swanson (@mr-martian), Dusty Miller (@d-us-vb),
 *   killerbee (@killerbee13), Richard (@CodeTriangle)
 *
 * This file contains the implementation and tests for utf8_rope
 *
 * ****************************************************************************/
#include "utf8_rope.hpp"

#include <deque>

namespace quest {
using namespace std::literals;

utf8_rope::utf8_rope(std::string_view str)
    : tree(node::allocate_subtree(str, 0)){};

utf8_rope::utf8_rope(std::u8string_view str)
    : utf8_rope(std::string_view(reinterpret_cast<const char*>(str.data()),
                                 str.size())) {}

utf8_rope::utf8_rope(std::initializer_list<char8_t> il) {}

utf8_rope::utf8_rope(std::initializer_list<char> il) {}

void utf8_rope::assign(std::initializer_list<char8_t> il) {}

void utf8_rope::assign(std::initializer_list<char> il) {}

void utf8_rope::assign(std::string_view str) {}

void utf8_rope::assign(std::u8string_view str) {}

void utf8_rope::assign(utf8_rope::size_type num, char8_t val) {}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos,
                                      char8_t val) {}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos, char val) {

}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos,
                                      utf8_rope::size_type num, char8_t val) {}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos,
                                      utf8_rope::size_type num, char val) {}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos,
                                      std::initializer_list<char8_t> il) {}

utf8_rope::iterator utf8_rope::insert(utf8_rope::const_iterator pos,
                                      std::initializer_list<char> il) {}

utf8_rope::iterator utf8_rope::erase(utf8_rope::const_iterator pos) {}

utf8_rope::iterator utf8_rope::erase(utf8_rope::const_iterator begin,
                                     utf8_rope::const_iterator end) {}

// avoids calculating size (and descending the tree twice)
utf8_rope::reference utf8_rope::back() {
	node* p{tree.get()};
	assert(p);

	while (true) {
		assert(p);
		if (std::u8string* s = std::get_if<std::u8string>(&p->data)) {
			assert(not s->empty());
			return s->back();
		} else if (node::children_t* ch
		           = std::get_if<node::children_t>(&p->data)) {
			p = ch->right.get();
		}
	}
}

utf8_rope::const_reference utf8_rope::back() const {
	node* p{tree.get()};
	assert(p);

	while (true) {
		assert(p);
		if (std::u8string* s = std::get_if<std::u8string>(&p->data)) {
			assert(not s->empty());
			return s->back();
		} else if (node::children_t* ch
		           = std::get_if<node::children_t>(&p->data)) {
			p = ch->right.get();
		}
	}
}

void utf8_rope::push_front(char8_t val) {}

void utf8_rope::push_back(char8_t val) {}

void utf8_rope::pop_front() {}

void utf8_rope::pop_back() {}

utf8_rope::reference utf8_rope::operator[](utf8_rope::size_type idx) noexcept {
	node* p{tree.get()};

	while (true) {
		assert(p);
		if (std::u8string* s = std::get_if<std::u8string>(&p->data)) {
			// std::cout << "found character " << static_cast<char>((*s)[idx])
			// << '\n';
			return (*s)[idx];
		} else if (node::children_t* ch
		           = std::get_if<node::children_t>(&p->data)) {
			if (idx >= ch->l_chars) {
				// std::cout << "(" << idx << " >= " << ch->l_chars << ") R, ";
				p = ch->right.get();
				idx -= ch->l_chars;
			} else {
				// std::cout << "(" << idx << " < " << ch->l_chars << ") L, ";
				p = ch->left.get();
			}
		}
	}
}

utf8_rope::const_reference utf8_rope::operator[](
    utf8_rope::size_type idx) const noexcept {
	node* p{tree.get()};

	while (true) {
		assert(p);
		if (std::u8string* s = std::get_if<std::u8string>(&p->data)) {
			return (*s)[idx];
		} else if (node::children_t* ch
		           = std::get_if<node::children_t>(&p->data)) {
			if (idx >= ch->l_chars) {
				p = ch->right.get();
				idx -= ch->l_chars;
			} else {
				p = ch->left.get();
			}
		}
	}
}

// utf8_rope::iterator utf8_rope::nth(utf8_rope::size_type idx) noexcept {}

// utf8_rope::const_iterator utf8_rope::nth(
//    utf8_rope::size_type idx) const noexcept {}

// utf8_rope::size_type utf8_rope::index_of(
//    utf8_rope::const_iterator it) const noexcept {}

char32_t pop_code_point(std::string_view& str) {
	assert(not str.empty());
	if (not (str[0] & 0x80)) {
		auto c = static_cast<char32_t>(str[0]);
		str.remove_prefix(1);
		return c;
	} else {
		return 0;
	}
}

// returns true if a mandatory line break occurs after the first code point in
// str. str must not be empty
bool is_newline_after(std::string_view str) {
	char32_t first = pop_code_point(str);

	// LF, VT, FF, NEL, LINE SEPARATOR, PARAGRAPH SEPARATOR, or CR not followed
	// by LF
	return U"\n\v\f\u0085\u2028\u2029"sv.find(first) != std::u32string_view::npos
	       or (first == U'\r' and (str.empty() or pop_code_point(str) != U'\n'));
}

utf8_rope::sizes utf8_rope::find_last_cluster_within(
    const std::string_view str) {
	utf8_rope::sizes sz{};
	auto last = str.begin();
	for (auto it = str.begin(); it != str.end(); ++it) {
		if (not (*it & 0x80)) {
			++sz.codepoints;
			++sz.clusters;
			if (is_newline_after(std::string_view{it, str.end()})) {
				++sz.lines;
			}
			last = it;
		}
	}
	sz.chars = static_cast<std::size_t>(last - str.begin() + 1);
	// std::cout << "f_l_c_w: " << sz << '\n';
	assert(sz.validate());
	return sz;
}

utf8_rope::sizes count_sizes(const std::string_view str) {
	utf8_rope::sizes sz{};
	sz.chars = str.size();
	for (auto it = str.begin(); it != str.end(); ++it) {
		if (not (*it & 0x80)) {
			++sz.codepoints;
			++sz.clusters;
			if (is_newline_after(std::string_view{it, str.end()})) {
				++sz.lines;
			}
		}
	}
	// std::cout << "count_sizes: " << sz << '\n';
	return sz;
}

std::shared_ptr<utf8_rope::node> utf8_rope::node::allocate_subtree(
    const std::string_view contents, std::size_t capacity) {
	capacity = std::max(capacity, contents.size());
	if (capacity > max_size_) {
		throw std::length_error("rope: requested size greater than max_size()");
	} else {
		// divide string into approximately equal segments
		auto remaining_length = capacity;
		auto remaining_contents = contents;
		struct fragment_t {
			std::shared_ptr<node> tree{};
			sizes cumulative{};
		};
		std::deque<fragment_t> queue;
		sizes cumulative{};

		while (remaining_length > target_fragment_length) {
			auto partition = find_last_cluster_within(
			    remaining_contents.substr(0, target_fragment_length));
			auto& fragment = queue.emplace_back(
			    fragment_t{std::make_shared<node>(node::leaf)});

			auto& node_ = *fragment.tree;
			auto& s = std::get<std::u8string>(node_.data);
			// s.assign(remaining_contents.substr(0, partition.chars));
			s.resize(partition.chars);
			std::memcpy(s.data(), remaining_contents.data(), partition.chars);
			node_.assign_from(partition);

			fragment.cumulative = cumulative;
			cumulative += partition;
			// std::cout << "a_s1: " << cumulative << '\n';

			remaining_contents.remove_prefix(partition.chars);
			remaining_length -= partition.chars;
		}
		{
			auto partition = count_sizes(remaining_contents);
			auto& fragment = queue.emplace_back(
			    fragment_t{std::make_shared<node>(node::leaf)});

			auto& node_ = *fragment.tree;
			auto& s = std::get<std::u8string>(node_.data);
			// s.assign(remaining_contents);
			s.resize(remaining_contents.size());
			std::memcpy(s.data(), remaining_contents.data(),
			            remaining_contents.size());
			node_.assign_from(partition);

			fragment.cumulative = cumulative;
			cumulative += partition;
			// std::cout << "a_s2: " << cumulative << '\n';
		}

		//		for (auto& n : queue) {
		//			std::cout << '[';
		//			if (n.tree) {
		//				std::cout << n.tree->size_all() << ',';
		//			} else {
		//				std::cout << "nullptr,";
		//			}
		//			std::cout << n.cumulative << "]\n";
		//		}

		// build tree out of segments
		using q_it = decltype(queue)::iterator;
		auto tree_helper = [](q_it begin, q_it end, sizes offset,
		                      auto self) -> std::shared_ptr<node> {
			auto l = end - begin;
			if (l == 0) {
				return {};
			} else if (l == 1) {
				return std::move(begin->tree);
			} else {
				auto midpoint = begin + l / 2;
				auto sz = midpoint->cumulative - offset;
				// std::cout << "t_h(" << offset << "): " << sz << '\n';
				auto n = std::make_shared<node>(node::internal);
				auto& c = std::get<node::children_t>(n->data);
				// std::cout << "L:";
				c.left = self(begin, midpoint, offset, self);
				// std::cout << "R:";
				c.right = self(midpoint, end, midpoint->cumulative, self);
				// n->assign_from(c.left->size_all());
				n->assign_from(sz);
				// std::cout << "E\n";
				return n;
			}
		};
		return tree_helper(queue.begin(), queue.end(), {}, tree_helper);
	}
}

std::ostream& operator<<(std::ostream& os, utf8_rope::sizes sz) {
	return os << '{' << sz.chars << ", " << sz.codepoints << ", " << sz.clusters
	          << ", " << sz.lines << '}';
}
void debug_print_tree_i(const utf8_rope::node* n) {
	if (not n) {
		std::cout << "nullptr\n";
		return;
	} else {
		std::cout << '{' << n->l_sizes() << "; ";
		kblib::visit2(
		    n->data,
		    [](const quest::utf8_rope::node::children_t& ch) {
			    debug_print_tree_i(ch.left.get());
			    std::cout << "; ";
			    debug_print_tree_i(ch.right.get());
		    },
		    [](const std::u8string& s) {
			    auto sv = std::string_view(reinterpret_cast<const char*>(s.data()),
			                               std::min(s.size(), std::size_t{16}));
			    std::cout << s.size() << std::quoted(sv) << "...";
		    });
		std::cout << "}\n";
	}
}

} // namespace quest
