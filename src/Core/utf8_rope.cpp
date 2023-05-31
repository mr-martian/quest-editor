/* *****************************************************************************
 * This file is part of the Quest Text Editor project.
 * Copyright (c) 2021 Daniel Swanson (@mr-martian), Dusty Miller (@d-us-vb),
 *   killerbee (@killerbee13), Richard (@CodeTriangle)
 *
 * This file contains the implementation and tests for utf8_rope
 *
 * ****************************************************************************/
#include "utf8_rope.hpp"

namespace quest {

utf8_rope::utf8_rope(utf8_rope::size_type num, char8_t val) {}

utf8_rope::utf8_rope(std::string_view str) {}

utf8_rope::utf8_rope(std::u8string_view str) {}

utf8_rope::utf8_rope(std::initializer_list<char8_t> il) {}

utf8_rope::utf8_rope(std::initializer_list<char> il) {}

void utf8_rope::assign(std::initializer_list<char8_t> il) {}

void utf8_rope::assign(std::initializer_list<char> il) {}

void utf8_rope::assign(std::string_view str) {}

void utf8_rope::assign(std::u8string_view str) {}

void utf8_rope::assign(utf8_rope::size_type num, char8_t val) {}

utf8_rope::iterator utf8_rope::begin() noexcept {}

utf8_rope::const_iterator utf8_rope::begin() const noexcept {}

utf8_rope::const_iterator utf8_rope::cbegin() const noexcept {}

utf8_rope::iterator utf8_rope::end() noexcept {}

utf8_rope::const_iterator utf8_rope::end() const noexcept {}

utf8_rope::const_iterator utf8_rope::cend() const noexcept {}

utf8_rope::reverse_iterator utf8_rope::rbegin() noexcept {}

utf8_rope::const_reverse_iterator utf8_rope::rbegin() const noexcept {}

utf8_rope::const_reverse_iterator utf8_rope::crbegin() const noexcept {}

utf8_rope::reverse_iterator utf8_rope::rend() noexcept {}

utf8_rope::const_reverse_iterator utf8_rope::rend() const noexcept {}

utf8_rope::const_reverse_iterator utf8_rope::crend() const noexcept {}

void utf8_rope::swap(utf8_rope&) noexcept {}

utf8_rope::size_type utf8_rope::size() const noexcept {}

bool utf8_rope::empty() const noexcept {}

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

void utf8_rope::clear() noexcept {}

utf8_rope::reference utf8_rope::front() {}

utf8_rope::const_reference utf8_rope::front() const {}

utf8_rope::reference utf8_rope::back() {}

utf8_rope::const_reference utf8_rope::back() const {}

void utf8_rope::push_front(char8_t val) {}

void utf8_rope::push_back(char8_t val) {}

void utf8_rope::pop_front() noexcept {}

void utf8_rope::pop_back() noexcept {}

utf8_rope::reference utf8_rope::at(utf8_rope::size_type idx) {}

utf8_rope::const_reference utf8_rope::at(utf8_rope::size_type idx) const {}

utf8_rope::iterator utf8_rope::nth(utf8_rope::size_type idx) noexcept {}

utf8_rope::const_iterator utf8_rope::nth(
    utf8_rope::size_type idx) const noexcept {}

utf8_rope::size_type utf8_rope::index_of(
    utf8_rope::const_iterator it) const noexcept {}

} // namespace quest
