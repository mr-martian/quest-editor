/* *****************************************************************************
 *
 * ****************************************************************************/

#include "ast.hpp"

namespace AST {

auto OverloadSet::insert(FunctionDef* f) -> std::size_t {
	assert(f);
	if (not f->_name._discrim.args) {
		assert(not _fn_unknown);
		_fn_unknown = f;
	}
	auto l = std::lower_bound(_members.begin(), _members.end(), f,
	                          [](FunctionDef* f1, FunctionDef* f2) {
		                          return f1->_name._discrim.args
		                                 < f2->_name._discrim.args;
	                          });
	if (l != _members.end()
	    and (*l)->_name._discrim.args == f->_name._discrim.args) {
		if ((*l)->_body) {
			throw 1;
		} else
			return l - members().begin();
	}
	return _members.insert(l, f) - _members.begin();
}
auto OverloadSet::refine(FunctionDef* f) -> std::size_t {
	assert(f);
	assert(_fn_unknown);
	assert(_fn_unknown == f);

	if (f->_name._discrim.args) {
		_fn_unknown = nullptr;
		return insert(f);
	} else {
		throw 1;
		return find(f->_name) - _members.begin();
	}
}
auto OverloadSet::get(std::optional<std::size_t> idx) const -> FunctionDef* {
	if (idx) {
		return _members.at(*idx);
	} else {
		return _fn_unknown;
	}
}
auto OverloadSet::get(const Name& name) const -> FunctionDef* {
	if (not name._discrim.args) {
		if (_members.size() == 1) {
			return _members.front();
		} else {
			return _fn_unknown;
		}
	} else {
		auto r = find(name);
		if (r != _members.end()) {
			return *r;
		} else {
			return nullptr;
		}
	}
}
auto OverloadSet::lookup(const Name& name) const
    -> std::variant<std::monostate, std::nullopt_t, std::size_t> {
	if (not name._discrim.args) {
		if (_members.size() == 1) {
			return std::size_t{0};
		} else {
			return std::nullopt;
		}
	} else {
		auto r = find(name);
		if (r != _members.end()) {
			return std::size_t(r - _members.begin());
		} else {
			return std::monostate{};
		}
	}
}

auto Scope::find_name(const Name& name_) -> decltype(symbols)::value_type* {
	auto it = symbols.find(name_);
	if (it != symbols.end()) {
		return &*it;
	} else {
		return nullptr;
	}
}
auto Scope::refine_name(iterator old, FunctionDef* ent) -> iterator {
	if (std::holds_alternative<std::nullopt_t>(old._subindex)) {
		auto& ov = std::get<OverloadSet>(old._val->second.decl);
		return {old._val, ov.refine(ent)};
	} else {
		throw 0;
	}
}

std::ostream& Scope::pretty_print(std::ostream& os) const {
	os << "(Scope " << name << " {\n" << nest;
	for (auto& ent : symbols) {
		auto& e_name = ent.first;
		os << indent << "Entity: " << e_name << " is " << ent.second << "\n";
	}
	os << unnest << indent << "})\n";
	return os;
}

auto Scope::add_name(FunctionDef* ent) -> iterator {
	assert(ent);
	if (ent->_name._discrim.args) {
		auto [ret, new_name] = do_add_name(ent);
		if (not new_name) {
			if (auto p_old = std::get_if<OverloadSet>(&ret._val->second.decl)) {
				if (auto idx = std::get_if<std::size_t>(&ret._subindex)) {
					assert(not p_old->get(*idx)->_body);
				} else if (std::holds_alternative<std::nullopt_t>(ret._subindex)) {
					assert(not p_old->get(std::nullopt)->_body);
				}
			} else if (auto p_old
			           = std::get_if<FunctionDef*>(&ret._val->second.decl)) {
				assert(not (*p_old)->_body);
			}
		} else {
			assert(std::holds_alternative<std::monostate>(ret._subindex));
		}
		auto base = ent->_name.without_args();
		if (auto it = symbols.find(base); it != symbols.end()) {
			auto& ov = std::get<OverloadSet>(it->second.decl);
			ret._subindex = ov.insert(ent);
		} else {
			symbols.insert({base, entity{OverloadSet{ent}}});
			ret._subindex = std::size_t{0};
		}
		assert(not std::holds_alternative<std::nullopt_t>(ret._subindex));
		return ret;
	} else {
		// unknown number of args
		if (auto it = symbols.find(ent->_name); it != symbols.end()) {
			auto& ov = std::get<OverloadSet>(it->second.decl);
			ov.insert(ent);
			return {it, std::nullopt};
		} else {
			return {symbols.insert({ent->_name, entity{OverloadSet{ent}}}).first,
			        std::nullopt};
		}
	}
}

template <typename T>
auto do_try_type = [](auto arg, auto f) {
	if constexpr (std::is_pointer_v<T>) {
		static_assert(std::derived_from<std::remove_pointer_t<T>, Declaration>);
		if (auto p = dynamic_cast<T>(arg)) {
			f(p);
			return true;
		} else {
			return false;
		}
	} else {
		return false;
	}
};

template <typename... Ts, typename P, typename F>
auto try_each_type(P p, F f) {
	static_assert(
	    std::conjunction_v<std::is_same<
	        bool, std::invoke_result_t<decltype(do_try_type<Ts>), P, F>>...>);
	return (do_try_type<Ts>(p, f) or ...);
}

template <typename... Ts, typename P, typename F>
auto try_each_type(P p, F f, std::variant<Ts...>*) {
	return try_each_type<Ts...>(p, f);
}

auto Scope::add_name(Declaration* ent) -> iterator {
	iterator ret;
	auto success = try_each_type(
	    ent,
	    [&](auto* ent) {
		    if constexpr (not std::is_same_v<Declaration*, decltype(ent)>) {
			    ret = add_name(ent);
		    }
	    },
	    static_cast<entity::type*>(nullptr));
	if (success) {
		return ret;
	} else {
		throw 1;
	}
}

Scope::~Scope() {
	// std::clog << "scope destroyed: " << name << '\n';
	// pretty_print(std::clog);
}
} // namespace AST
