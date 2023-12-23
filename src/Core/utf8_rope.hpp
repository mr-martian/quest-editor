/* *****************************************************************************
 * This file is part of the Quest Text Editor project.
 * Copyright (c) 2021 Daniel Swanson (@mr-martian), Dusty Miller (@d-us-vb),
 *   killerbee (@killerbee13), Richard (@CodeTriangle)
 *
 * This file contains all types that are used globally in the Quest System
 *
 * ****************************************************************************/
#ifndef UTF8_ROPE_H
#define UTF8_ROPE_H

#include <compare>
#include <iterator>
#include <memory>
#include <string>

#include <kblib/traits.h>
#include <kblib/variant.h>

namespace quest {

template <typename CharT>
class utf8_rope_iterator;

#if __cpp_char8_t < 201811L
#	error "need char8_t"
#endif

template <typename T>
concept character = kblib::is_character_v<T>;

template <character T>
struct grapheme : std::basic_string<T> {};
template <character T>
struct grapheme_cluster : std::basic_string<T> {};

// ill-formed:
// grapheme_cluster<int> t;
// grapheme_cluster<char> c;

class utf8_rope {
 public:
	using value_type = char8_t;
	using reference = char8_t&;
	using const_reference = const char8_t&;
	using iterator = utf8_rope_iterator<char8_t>;
	using const_iterator = utf8_rope_iterator<const char8_t>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;
	using difference_type = std::ptrdiff_t;
	using size_type = std::size_t;

	utf8_rope() noexcept = default;
	utf8_rope(const utf8_rope&) = default;
	utf8_rope(utf8_rope&&) noexcept = default;
	utf8_rope& operator=(const utf8_rope& other) = default;
	utf8_rope& operator=(utf8_rope&& other) noexcept = default;

	template <character T>
	utf8_rope(size_type num, T val);
	template <character T>
	utf8_rope(size_type num, grapheme<T> val);
	template <character T>
	utf8_rope(size_type num, grapheme_cluster<T> val);
	utf8_rope(char8_t, size_type) = delete;
	utf8_rope(char, size_type) = delete;

	explicit utf8_rope(std::string_view str);
	explicit utf8_rope(std::u8string_view str);

	template <typename InputIt, typename Sentinel>
	utf8_rope(InputIt begin, Sentinel end);

	explicit utf8_rope(std::initializer_list<char8_t> il);
	explicit utf8_rope(std::initializer_list<char> il);
	utf8_rope& operator=(std::initializer_list<char8_t> il);
	utf8_rope& operator=(std::initializer_list<char> il);

	template <typename InputIt, typename Sentinel>
	void assign(InputIt begin, Sentinel end);
	void assign(std::initializer_list<char8_t> il);
	void assign(std::initializer_list<char> il);

	void assign(std::string_view str);
	void assign(std::u8string_view str);

	void assign(size_type num, char8_t val);
	void assign(char8_t, size_type) = delete;
	void assign(char, size_type) = delete;

	~utf8_rope() = default;

	iterator begin() noexcept;
	const_iterator begin() const noexcept;
	const_iterator cbegin() const noexcept;
	iterator end() noexcept;
	const_iterator end() const noexcept;
	const_iterator cend() const noexcept;
	iterator before_begin() noexcept;
	const_iterator before_begin() const noexcept;
	const_iterator cbefore_begin() const noexcept;

	reverse_iterator rbegin() noexcept;
	const_reverse_iterator rbegin() const noexcept;
	const_reverse_iterator crbegin() const noexcept;
	reverse_iterator rend() noexcept;
	const_reverse_iterator rend() const noexcept;
	const_reverse_iterator crend() const noexcept;
	iterator rbefore_begin() noexcept;
	const_iterator rbefore_begin() const noexcept;
	const_iterator crbefore_begin() const noexcept;

	friend bool operator==(const utf8_rope&, const utf8_rope&);
	friend std::strong_ordering operator<=>(const utf8_rope&, const utf8_rope&);

	void swap(utf8_rope&) noexcept;

	bool empty() const noexcept;

	static constexpr size_type max_size() { return max_size_; }

	// char8_t will always be a character type, so emplace could only ever have
	// one argument
	template <typename... Args>
	iterator emplace(
	    const_iterator pos,
	    Args&&... args) requires std::is_constructible_v<char8_t, Args&&...>;
	iterator insert(const_iterator pos, char8_t val);
	iterator insert(const_iterator pos, char val);
	iterator insert(const_iterator pos, size_type num, char8_t val);
	iterator insert(const_iterator pos, size_type num, char val);
	template <typename InputIt, typename Sentinel>
	iterator insert(const_iterator pos, InputIt begin, Sentinel end);
	iterator insert(const_iterator pos, std::initializer_list<char8_t> il);
	iterator insert(const_iterator pos, std::initializer_list<char> il);

	iterator erase(const_iterator pos);
	iterator erase(const_iterator begin, const_iterator end);

	void clear() noexcept;

	reference front();
	const_reference front() const;

	reference back();
	const_reference back() const;

	template <typename... Args>
	void emplace_front(
	    Args&&... args) requires std::is_constructible_v<char8_t, Args&&...>;
	template <typename... Args>
	void emplace_back(
	    Args&&... args) requires std::is_constructible_v<char8_t, Args&&...>;

	void push_front(char8_t val);
	void push_back(char8_t val);

	void pop_front() noexcept;
	void pop_back() noexcept;

	//	reference operator[](size_type idx) noexcept;
	//	const_reference operator[](size_type idx) const noexcept;
	//	reference at(size_type idx);
	//	const_reference at(size_type idx) const;

	//	iterator nth(size_type idx) noexcept;
	//	const_iterator nth(size_type idx) const noexcept;

	//	size_type index_of(const_iterator it) const noexcept;

	friend std::ostream& operator<<(std::ostream&, const utf8_rope&);

	struct sizes {
		size_type chars{}, codepoints{}, graphemes{}, clusters{}, lines{};
		sizes operator+=(const sizes& r) noexcept {
			chars += r.chars;
			codepoints += r.codepoints;
			graphemes += r.graphemes;
			clusters += r.clusters;
			lines += r.lines;
			return *this;
		}
		friend sizes operator+(sizes l, const sizes& r) noexcept {
			return l += r;
		}
		bool validate() const noexcept {
			return chars <= codepoints and codepoints <= graphemes
			       and graphemes <= clusters and clusters <= lines;
		}
	};

 private:
	static constexpr size_type target_fragment_length = 4096;
	static constexpr size_type max_size_ = size_type(-1) >> 1;

	static sizes find_last_cluster_within(std::string_view str);

	template <typename CharT>
	friend class utf8_rope_iterator;

	class node {
	 public:
		enum type { leaf, internal };

		node(type t)
		    : data(t == leaf
		               ? std::variant<children_t, std::string>{std::string{}}
		               : std::variant<children_t, std::string>{children_t{}}) {}
		friend class utf8_rope;

		template <typename CharT>
		friend class utf8_rope_iterator;

		struct children_t {
			std::shared_ptr<node> left{}, right{};
			size_type l_chars{};
		};

		static std::shared_ptr<node> allocate_subtree(std::string_view contents,
		                                              std::size_t capacity = 0);

	 private:
		std::variant<children_t, std::string> data;
		size_type l_codepoints{}, l_graphemes{}, l_clusters{}, l_lines{};
		size_type l_chars() const {
			return kblib::visit2(
			    data, [](const std::string& str) { return str.size(); },
			    [](const node::children_t& ch) { return ch.l_chars; });
		}

	 public:
		sizes size_all() const { return size_all(this); }
		size_type size_chars() const { return size_chars(this); }
		size_type size_codepoints() const { return size_codepoints(this); }
		size_type size_graphemes() const { return size_graphemes(this); }
		size_type size_clusters() const { return size_clusters(this); }
		size_type size_lines() const { return size_lines(this); }

		static sizes size_all(const node* n) {
			if (n) {
				auto l_sizes = sizes{0, n->l_codepoints, n->l_graphemes,
				                     n->l_clusters, n->l_lines};
				return kblib::visit2(
				    n->data,
				    [&l_sizes](const std::string& str) {
					    l_sizes.chars = str.size();
					    return l_sizes;
				    },
				    [&l_sizes](const node::children_t& ch) {
					    l_sizes.chars = ch.l_chars;
					    return l_sizes + size_all(ch.right.get());
				    });
			} else {
				return {};
			}
		}
		static size_type size_chars(const node* n) {
			if (n) {
				return kblib::visit2(
				    n->data, [](const std::string& str) { return str.size(); },
				    [](const node::children_t& ch) {
					    return ch.l_chars + size_chars(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_codepoints(const node* n) {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::string&) { return n->l_codepoints; },
				    [n](const children_t& ch) {
					    return n->l_codepoints + size_codepoints(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_graphemes(const node* n) {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::string&) { return n->l_graphemes; },
				    [n](const children_t& ch) {
					    return n->l_graphemes + size_graphemes(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_clusters(const node* n) {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::string&) { return n->l_clusters; },
				    [n](const children_t& ch) {
					    return n->l_clusters + size_clusters(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_lines(const node* n) {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::string&) { return n->l_lines; },
				    [n](const children_t& ch) {
					    return n->l_lines + size_lines(ch.right.get());
				    });
			} else {
				return 0;
			}
		}

		[[nodiscard]] bool empty() const {
			return kblib::visit2(
			    data,
			    [&](const std::string& str) { //
				    return str.empty();
			    },
			    [&](const children_t& ch) {
				    return (ch.l_chars == 0)
				           and (not ch.right or ch.right->empty());
			    });
		}

	 private:
		void assign_from(sizes sz) {
			if (not sz.validate()) {
				std::abort();
			}

			kblib::visit2(
			    data,
			    [&](std::string& str) {
				    if (str.size() != sz.chars) {
					    std::abort();
				    }
			    },
			    [&](children_t& ch) {
				    if (sz.chars == 0) {
					    if (ch.left and not ch.left->empty()) {
						    std::abort();
					    }
				    }
				    if (not ch.left or ch.left->size_chars() != sz.chars) {
					    std::abort();
				    }
			    });

			l_codepoints = sz.codepoints;
			l_graphemes = sz.graphemes;
			l_clusters = sz.clusters;
		}
	};

	std::shared_ptr<node> tree;
};

template <typename CharT>
class utf8_rope_iterator {};

} // namespace quest

namespace std {

template <typename CharT>
class reverse_iterator<quest::utf8_rope_iterator<CharT>> {};

} // namespace std

#endif // UTF8_ROPE_H
