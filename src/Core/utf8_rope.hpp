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
#include <iosfwd>
#include <memory>
#include <string>

#include <kblib/traits.h>
#include <kblib/variant.h>

namespace quest {

#if __cpp_char8_t < 201811L
#	error "need char8_t"
#endif

template <typename T>
concept character = kblib::is_character_v<T>;

template <character T>
struct grapheme_cluster : std::basic_string<T> {};

template <typename CharT>
class utf8_rope_iterator;
class utf8_rope_code_point_iterator;
class utf8_rope_grapheme_cluster_iterator;
class utf8_rope_line_iterator;

// ill-formed:
// grapheme_cluster<int> t;
// grapheme_cluster<char> c;

class utf8_rope {
 public:
	using value_type = char8_t;
	using reference = char8_t&;
	using const_reference = const char8_t&;
	using difference_type = std::ptrdiff_t;
	using size_type = std::size_t;
	using iterator = utf8_rope_iterator<char8_t>;
	using const_iterator = utf8_rope_iterator<const char8_t>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	// iterators that work within one fragment (used for for_each_subrange)
	using local_iterator = std::u8string::iterator;
	using const_local_iterator = std::u8string::const_iterator;
	// only iterator and char_iterator can be non-constant
	using char_iterator = utf8_rope_iterator<char>;
	using const_char_iterator = utf8_rope_iterator<const char>;

	// transcoding/segment iterators are constant, so no const_ versions
	using code_point_iterator = utf8_rope_code_point_iterator;
	using grapheme_cluster_iterator = utf8_rope_grapheme_cluster_iterator;
	using line_iterator = utf8_rope_line_iterator;

	utf8_rope() noexcept = default;
	utf8_rope(const utf8_rope&) = default;
	utf8_rope(utf8_rope&&) noexcept = default;
	utf8_rope& operator=(const utf8_rope& other) = default;
	utf8_rope& operator=(utf8_rope&& other) noexcept = default;

	template <character T>
	utf8_rope(size_type num, T val);
	template <character T>
	utf8_rope(size_type num, grapheme_cluster<T> val);
	utf8_rope(char8_t, size_type) = delete;
	utf8_rope(char, size_type) = delete;

	explicit utf8_rope(std::string_view str);
	explicit utf8_rope(std::u8string_view str)
	    : utf8_rope(std::string_view(reinterpret_cast<const char*>(str.data()),
	                                 str.size())) {}

	template <typename InputIt, typename Sentinel>
	utf8_rope(InputIt begin, Sentinel end);

	explicit utf8_rope(std::initializer_list<char8_t> il)
	    : utf8_rope(std::string_view(reinterpret_cast<const char*>(il.begin()),
	                                 il.size())) {}
	explicit utf8_rope(std::initializer_list<char> il)
	    : utf8_rope(std::string_view(il.begin(), il.size())) {}
	utf8_rope& operator=(std::initializer_list<char8_t> il) {
		assign(il);
		return *this;
	}
	utf8_rope& operator=(std::initializer_list<char> il) {
		assign(il);
		return *this;
	}

	template <typename InputIt, typename Sentinel>
	void assign(InputIt begin, Sentinel end);

	void assign(std::string_view str);
	void assign(std::u8string_view str) {
		assign(std::string_view(reinterpret_cast<const char*>(str.data()),
		                        str.size()));
	}
	void assign(std::initializer_list<char8_t> il) {
		assign(std::string_view(reinterpret_cast<const char*>(il.begin()),
		                        il.size()));
	}
	void assign(std::initializer_list<char> il) {
		assign(std::string_view(il.begin(), il.size()));
	}

	void assign(size_type num, char8_t val);
	void assign(size_type num, char val) {
		assign(num, static_cast<char8_t>(val));
	}
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
	reverse_iterator rbefore_begin() noexcept;
	const_reverse_iterator rbefore_begin() const noexcept;
	const_reverse_iterator crbefore_begin() const noexcept;

	// internal iteration avoids some iterator overhead
	template <typename F>
	void for_each_subrange(F f) const;
	template <typename F>
	void for_each_subrange(F f);

	friend bool operator==(const utf8_rope&, const utf8_rope&);
	friend std::strong_ordering operator<=>(const utf8_rope&, const utf8_rope&);

	void swap(utf8_rope& other) noexcept { std::swap(tree, other.tree); }

	bool empty() const noexcept { return not tree or tree->empty(); }

	static constexpr size_type max_size() { return max_size_; }

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

	void clear() noexcept { tree.reset(); }

	reference front() { return (*this)[0]; }
	const_reference front() const { return (*this)[0]; }

	reference back() { return back_helper<false>(); }
	const_reference back() const { return back_helper<false>(); }

	template <typename... Args>
	void emplace_front(Args&&... args)
	   requires std::is_constructible_v<char8_t, Args&&...>;
	template <typename... Args>
	void emplace_back(Args&&... args)
	   requires std::is_constructible_v<char8_t, Args&&...>;

	void push_front(char8_t val);
	void push_back(char8_t val);

	void pop_front();
	void pop_back();

	reference operator[](size_type idx) noexcept;
	const_reference operator[](size_type idx) const noexcept;
	reference at(size_type idx) { return at_helper<true>(tree.get(), idx); }
	const_reference at(size_type idx) const {
		return at_helper<true>(tree.get(), idx);
	}

	// slightly less work than begin() + idx
	iterator nth(size_type idx) noexcept;
	const_iterator nth(size_type idx) const noexcept;

	code_point_iterator nth_code_point(size_type idx) const noexcept;
	grapheme_cluster_iterator nth_grapheme_cluster(size_type idx) const noexcept;
	line_iterator nth_line(size_type idx) const noexcept;

	size_type index_of(iterator it) const noexcept;
	size_type index_of(const_iterator it) const noexcept;

	friend std::ostream& operator<<(std::ostream&, const utf8_rope&);

	struct sizes {
		size_type chars{}, codepoints{}, clusters{}, lines{};
		sizes operator+=(const sizes& r) noexcept {
			chars += r.chars;
			codepoints += r.codepoints;
			clusters += r.clusters;
			lines += r.lines;
			return *this;
		}
		friend sizes operator+(sizes l, const sizes& r) noexcept {
			return l += r;
		}
		sizes operator-=(const sizes& r) noexcept {
			chars -= r.chars;
			codepoints -= r.codepoints;
			clusters -= r.clusters;
			lines -= r.lines;
			return *this;
		}
		friend sizes operator-(sizes l, const sizes& r) noexcept {
			return l -= r;
		}
		bool validate() const noexcept {
			return chars >= codepoints and codepoints >= clusters
			       and clusters >= lines;
		}
		friend std::ostream& operator<<(std::ostream& os, utf8_rope::sizes sz);
	};

	size_type size_chars() const noexcept {
		if (tree) {
			return tree->size_chars();
		} else {
			return {};
		}
	}
	size_type size_codepoints() const noexcept {
		if (tree) {
			return tree->size_codepoints();
		} else {
			return {};
		}
	}
	size_type size_clusters() const noexcept {
		if (tree) {
			return tree->size_clusters();
		} else {
			return {};
		}
	}
	size_type size_lines() const noexcept {
		if (tree) {
			return tree->size_lines();
		} else {
			return {};
		}
	}
	sizes size_all() const noexcept {
		if (tree) {
			return tree->size_all();
		} else {
			return {};
		}
	}
	// this is not technically conforming as it is not O(1). It can be changed,
	// but would add overhead to certain other operations.
	size_type size() const noexcept { return size_chars(); }

 private:
	// +1 for null terminator
	static constexpr size_type target_fragment_length = 4095;
	static constexpr size_type max_size_ = size_type(-1) >> 1;

	static sizes find_last_cluster_within(std::string_view str);

	template <typename CharT>
	friend class utf8_rope_iterator;

	class node {
	 public:
		enum type { leaf, internal };

		node(type t)
		    : data(t == leaf
		               ? std::variant<children_t, std::u8string>{std::u8string{}}
		               : std::variant<children_t, std::u8string>{children_t{}}) {}
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
		std::variant<children_t, std::u8string> data;
		size_type l_codepoints{}, l_clusters{}, l_lines{};
		size_type l_chars() const noexcept {
			return kblib::visit2(
			    data, [](const std::u8string& str) { return str.size(); },
			    [](const node::children_t& ch) { return ch.l_chars; });
		}
		sizes l_sizes() const noexcept { return l_sizes(this); }

	 public:
		sizes size_all() const noexcept { return size_all(this); }
		size_type size_chars() const noexcept { return size_chars(this); }
		size_type size_codepoints() const noexcept {
			return size_codepoints(this);
		}
		size_type size_clusters() const noexcept { return size_clusters(this); }
		size_type size_lines() const noexcept { return size_lines(this); }
		static sizes l_sizes(const node* n) noexcept {
			if (n) {
				return sizes{n->l_chars(), n->l_codepoints, n->l_clusters,
				             n->l_lines};
			} else {
				return {};
			}
		}

		static sizes size_all(const node* n) noexcept {
			if (n) {
				auto l_sizes = sizes{0, n->l_codepoints, n->l_clusters, n->l_lines};
				return kblib::visit2(
				    n->data,
				    [&l_sizes](const std::u8string& str) {
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
		static size_type size_chars(const node* n) noexcept {
			if (n) {
				return kblib::visit2(
				    n->data, [](const std::u8string& str) { return str.size(); },
				    [](const node::children_t& ch) {
					    return ch.l_chars + size_chars(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_codepoints(const node* n) noexcept {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::u8string&) { return n->l_codepoints; },
				    [n](const children_t& ch) {
					    return n->l_codepoints + size_codepoints(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_clusters(const node* n) noexcept {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::u8string&) { return n->l_clusters; },
				    [n](const children_t& ch) {
					    return n->l_clusters + size_clusters(ch.right.get());
				    });
			} else {
				return 0;
			}
		}
		static size_type size_lines(const node* n) noexcept {
			if (n) {
				return kblib::visit2(
				    n->data, [n](const std::u8string&) { return n->l_lines; },
				    [n](const children_t& ch) {
					    return n->l_lines + size_lines(ch.right.get());
				    });
			} else {
				return 0;
			}
		}

		[[nodiscard]] bool empty() const noexcept {
			return kblib::visit2(
			    data,
			    [&](const std::u8string& str) { //
				    return str.empty();
			    },
			    [&](const children_t& ch) {
				    return (ch.l_chars == 0)
				           and (not ch.right or ch.right->empty());
			    });
		}

	 private:
		void assign_from(sizes sz) {
			// std::cout << "assign_from: " << sz << '\n';
			assert(sz.validate());

			kblib::visit2(
			    data, [&](std::u8string& str) { assert(str.size() == sz.chars); },
			    [&](children_t& ch) {
				    if (sz.chars == 0) {
					    assert(not ch.left or ch.left->empty());
				    }
				    assert(ch.left);
				    assert(ch.left->size_chars() == sz.chars);
				    ch.l_chars = sz.chars;
			    });

			l_codepoints = sz.codepoints;
			l_clusters = sz.clusters;
			l_lines = sz.lines;
		}
		friend void debug_print_tree_i(const utf8_rope::node* n);
	};

	std::shared_ptr<node> tree;

	friend void debug_print_tree_i(const utf8_rope::node* n);

 public:
	void debug_print_tree() const { debug_print_tree_i(tree.get()); }

 private:
	template <typename CharT>
	friend class utf8_rope_iterator;
	struct iterator_base {
		utf8_rope::node* root;
		char8_t* fragment;
		utf8_rope::size_type index;
		std::uint16_t fragment_size;
		std::uint16_t fragment_index;

		friend std::strong_ordering operator<=>(const iterator_base& lhs,
		                                        const iterator_base& rhs) {
			if (lhs.root == rhs.root) {
				return lhs.index <=> rhs.index;
			} else {
				return std::compare_three_way{}(

				    lhs.root, rhs.root);
			}
		}
		friend bool operator==(const iterator_base& lhs, const iterator_base& rhs)
		    = default;

		void recalculate_from_index() {
			// set up the before_begin iterator
			if (index == static_cast<std::size_t>(-1)) {
				fragment = nullptr;
				fragment_size = 0;
				fragment_index = 0;
				return;
			}

			node* p{root};
			size_type idx{index};
			while (true) {
				assert(p);
				if (std::u8string* s = std::get_if<std::u8string>(&p->data)) {
					fragment = s->data();
					fragment_size = static_cast<std::uint16_t>(s->size());
					fragment_index = static_cast<std::uint16_t>(idx);
					return;
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
			assert(false);
		}
	};

	// non-const-correct implementation functions to deduplicate back() and end()
	template <bool check>
	reference back_helper() const noexcept(not check);
	template <bool check>
	iterator end_helper() const noexcept(not check);
	template <bool check, typename T>
	friend auto at_helper(T* tree,
	                      size_type idx) -> kblib::copy_const_t<T, value_type>&;
};
// handles direct (non-transcoded) character iteration
template <typename CharT>
class utf8_rope_iterator : private utf8_rope::iterator_base {
 public:
	using value_type = CharT;
	using reference = CharT&;
	using pointer = CharT*;
	using difference_type = std::ptrdiff_t;
	// technically, this is a lie, because RA is O(log n) instead of O(1) time
	using iterator_category = std::random_access_iterator_tag;
	using iterator_concept = std::random_access_iterator_tag;

	utf8_rope_iterator() = default;
	utf8_rope_iterator(utf8_rope::node* n, utf8_rope::size_type idx)
	    : utf8_rope::iterator_base(n, {}, idx, {}, {}) {
		recalculate_from_index();
	}
	utf8_rope_iterator(const utf8_rope_iterator&) = default;
	utf8_rope_iterator(utf8_rope_iterator&&) = default;
	utf8_rope_iterator(
	    const utf8_rope_iterator<std::remove_const_t<CharT>>& o) //
	   requires std::is_const_v<CharT>
	    : utf8_rope::iterator_base{
	          static_cast<const utf8_rope::iterator_base&>(o)} {}

 private:
	utf8_rope_iterator(utf8_rope::node* n, utf8_rope::size_type idx, char8_t* f,
	                   std::uint16_t f_size, std::uint16_t f_idx)
	    : utf8_rope::iterator_base(n, f, idx, f_size, f_idx) {}

 public:
	utf8_rope_iterator& operator=(const utf8_rope_iterator&) = default;
	utf8_rope_iterator& operator=(utf8_rope_iterator&&) = default;
	utf8_rope_iterator& operator=(
	    const utf8_rope_iterator<std::remove_const_t<CharT>>& o)
	   requires std::is_const_v<CharT>
	{
		*this = static_cast<const utf8_rope::iterator_base&>(o);
	}

	reference operator*() const { return fragment[fragment_index]; }
	// note that CharT should never be a class type, so this can't be used as an
	// actual operator, but it is still useful to provide a means to extract a
	// pointer from the iterator
	pointer operator->() const { return &fragment[fragment_index]; }
	reference operator[](difference_type d) const { return *(*this + d); }

	friend utf8_rope_iterator operator+(utf8_rope_iterator it,
	                                    difference_type d) {
		const auto s = static_cast<difference_type>(it.fragment_size);
		const auto i = static_cast<difference_type>(it.fragment_index);
		it.index = static_cast<std::size_t>(static_cast<difference_type>(it.index)
		                                    + d);
		if (d < s and d > -s and (0 <= i + d and i + d < s)) {
			it.fragment_index = static_cast<std::uint16_t>(i + d);
		} else {
			it.recalculate_from_index();
		}
		return it;
	}
	friend difference_type operator-(const utf8_rope_iterator& lhs,
	                                 const utf8_rope_iterator& rhs) {
		assert(lhs.root == rhs.root);
		return lhs.index - rhs.index;
	}

	friend utf8_rope_iterator operator+(difference_type d,
	                                    const utf8_rope_iterator& it) {
		return it + d;
	}
	friend utf8_rope_iterator operator-(const utf8_rope_iterator& it,
	                                    difference_type d) {
		return it + -d;
	}
	friend utf8_rope_iterator operator-(difference_type d,
	                                    const utf8_rope_iterator& it) {
		return it + -d;
	}

	utf8_rope_iterator& operator+=(difference_type d) {
		return (*this = *this + d);
	}
	utf8_rope_iterator& operator-=(difference_type d) { return (*this) += -d; }

	utf8_rope_iterator& operator++() { return *this += 1; }
	utf8_rope_iterator& operator--() { return *this -= 1; }
	utf8_rope_iterator operator++(int) {
		auto t = *this;
		++*this;
		return t;
	}
	utf8_rope_iterator operator--(int) {
		auto t = *this;
		--*this;
		return t;
	}

	std::strong_ordering operator<=>(const utf8_rope_iterator&) const = default;

 private:
	friend class utf8_rope;
	template <typename>
	friend utf8_rope::size_type index_of(utf8_rope_iterator it) noexcept;
	template <typename>
	friend class utf8_rope_iterator;
};

template class quest::utf8_rope_iterator<char8_t>;

static_assert(std::random_access_iterator<utf8_rope_iterator<char>> //
              and std::output_iterator<utf8_rope_iterator<char>, char>);
static_assert(std::random_access_iterator<utf8_rope_iterator<const char>>);
static_assert(std::random_access_iterator<utf8_rope_iterator<char8_t>> //
              and std::output_iterator<utf8_rope_iterator<char8_t>, char8_t>);
static_assert(std::random_access_iterator<utf8_rope_iterator<const char8_t>>);

inline utf8_rope::iterator utf8_rope::begin() noexcept {
	return iterator{tree.get(), 0};
}
inline utf8_rope::const_iterator utf8_rope::begin() const noexcept {
	return const_iterator{tree.get(), 0};
}
inline utf8_rope::const_iterator utf8_rope::cbegin() const noexcept {
	return const_iterator{tree.get(), 0};
}
inline utf8_rope::iterator utf8_rope::before_begin() noexcept {
	return iterator{tree.get(), static_cast<size_type>(-1)};
}
inline utf8_rope::const_iterator utf8_rope::before_begin() const noexcept {
	return const_iterator{tree.get(), static_cast<size_type>(-1)};
}
inline utf8_rope::const_iterator utf8_rope::cbefore_begin() const noexcept {
	return const_iterator{tree.get(), static_cast<size_type>(-1)};
}
inline utf8_rope::iterator utf8_rope::nth(size_type idx) noexcept {
	return iterator{tree.get(), idx};
}
inline utf8_rope::const_iterator utf8_rope::nth(size_type idx) const noexcept {
	return const_iterator{tree.get(), idx};
}
inline utf8_rope::iterator utf8_rope::end() noexcept {
	return end_helper<false>();
}
inline utf8_rope::const_iterator utf8_rope::end() const noexcept {
	return end_helper<false>();
}
inline utf8_rope::const_iterator utf8_rope::cend() const noexcept {
	return end_helper<false>();
}

template <typename CharT>
utf8_rope::size_type index_of(utf8_rope_iterator<CharT> it) noexcept {
	return it.index;
}
inline utf8_rope::size_type utf8_rope::index_of(
    utf8_rope::iterator it) const noexcept {
	assert(it.root == tree.get());
	return it.index;
}
inline utf8_rope::size_type utf8_rope::index_of(
    utf8_rope::const_iterator it) const noexcept {
	assert(it.root == tree.get());
	return it.index;
}

} // namespace quest

namespace std {
// the only purpose of this specialization is to avoid frequent unnecessary
// decrements, because utf8_rope_iterator has a usable before_begin
template <class CharT>
class reverse_iterator<quest::utf8_rope_iterator<CharT>> {
 public:
	using iterator_type = quest::utf8_rope_iterator<CharT>;
	using iterator_concept = random_access_iterator_tag;
	using iterator_category = random_access_iterator_tag;
	using value_type = iter_value_t<iterator_type>;
	using difference_type = iter_difference_t<iterator_type>;
	using pointer = typename iterator_traits<iterator_type>::pointer;
	using reference = iter_reference_t<iterator_type>;

	constexpr static inline struct raw_construct_tag_t {
	} raw_construct_tag{};

	constexpr reverse_iterator() = default;
	constexpr explicit reverse_iterator(iterator_type x)
	    : current(prev(x)) {}
	constexpr explicit reverse_iterator(iterator_type x, raw_construct_tag_t)
	    : current(x) {}
	template <class U>
	   requires(not is_same_v<U, iterator_type>)
	           and convertible_to<const U&, iterator_type>
	constexpr reverse_iterator(const reverse_iterator<U>& u)
	    : current(u.current) {}
	template <class U>
	   requires(not is_same_v<U, iterator_type>)
	           and convertible_to<const U&, iterator_type>
	           and assignable_from<iterator_type&, const U&>
	constexpr reverse_iterator& operator=(const reverse_iterator<U>& u) {
		current = u.current;
	}

	constexpr iterator_type base() const { return next(current); }
	constexpr iterator_type raw_base() const { return current; }
	constexpr reference operator*() const { return *current; }
	constexpr pointer operator->() const { return current.operator->(); }
	constexpr iterator_type::reference operator[](difference_type n) const {
		return current[-n];
	}

	constexpr reverse_iterator& operator++() {
		--current;
		return *this;
	}
	constexpr reverse_iterator operator++(int) {
		auto tmp = *this;
		--current;
		return tmp;
	}
	constexpr reverse_iterator& operator--() {
		++current;
		return *this;
	}
	constexpr reverse_iterator operator--(int) {
		auto tmp = *this;
		++current;
		return tmp;
	}

	constexpr reverse_iterator operator+(difference_type n) const {
		auto tmp = *this;
		tmp.current -= n;
		return tmp;
	}
	constexpr reverse_iterator& operator+=(difference_type n) {
		current -= n;
		return *this;
	}
	constexpr reverse_iterator operator-(difference_type n) const {
		auto tmp = *this;
		tmp.current += n;
		return tmp;
	}
	constexpr reverse_iterator& operator-=(difference_type n) {
		current += n;
		return *this;
	}

	friend constexpr iter_rvalue_reference_t<iterator_type> iter_move(
	    const reverse_iterator&
	        i) noexcept(is_nothrow_copy_constructible_v<iterator_type>
	                    && noexcept(
	                        ranges::iter_move(--declval<iterator_type&>()))) {
		auto tmp = i.current;
		return ranges::iter_move(tmp);
	}
	template <indirectly_swappable<iterator_type> Iterator2>
	friend constexpr void iter_swap(
	    const reverse_iterator& x,
	    const reverse_iterator<Iterator2>&
	        y) noexcept(is_nothrow_copy_constructible_v<iterator_type>
	                    && is_nothrow_copy_constructible_v<Iterator2>
	                    && noexcept(
	                        ranges::iter_swap(--declval<iterator_type&>(),
	                                          --declval<Iterator2&>()))) {
		auto xtmp = x.current;
		auto ytmp = y.current;
		ranges::iter_swap(xtmp, ytmp);
	}

 protected:
	iterator_type current;
};
} // namespace std

namespace quest {

inline utf8_rope::reverse_iterator utf8_rope::rbegin() noexcept {
	return reverse_iterator(end());
}
inline utf8_rope::const_reverse_iterator utf8_rope::rbegin() const noexcept {
	return const_reverse_iterator(end());
}
inline utf8_rope::const_reverse_iterator utf8_rope::crbegin() const noexcept {
	return const_reverse_iterator(end());
}
inline utf8_rope::reverse_iterator utf8_rope::rend() noexcept {
	return reverse_iterator(before_begin(), reverse_iterator::raw_construct_tag);
}
inline utf8_rope::const_reverse_iterator utf8_rope::rend() const noexcept {
	return const_reverse_iterator(before_begin(),
	                              const_reverse_iterator::raw_construct_tag);
}
inline utf8_rope::const_reverse_iterator utf8_rope::crend() const noexcept {
	return const_reverse_iterator(before_begin(),
	                              const_reverse_iterator::raw_construct_tag);
}
inline utf8_rope::reverse_iterator utf8_rope::rbefore_begin() noexcept {
	return reverse_iterator(end(), reverse_iterator::raw_construct_tag);
}
inline utf8_rope::const_reverse_iterator utf8_rope::rbefore_begin()
    const noexcept {
	return const_reverse_iterator(end(),
	                              const_reverse_iterator::raw_construct_tag);
}
inline utf8_rope::const_reverse_iterator utf8_rope::crbefore_begin()
    const noexcept {
	return const_reverse_iterator(end(),
	                              const_reverse_iterator::raw_construct_tag);
}

} // namespace quest

#endif // UTF8_ROPE_H
