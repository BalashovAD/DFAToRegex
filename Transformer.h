#pragma once

#include <memory>
#include <vector>

#include "utils.h"

template <typename Ptr, typename NoneType, typename Maker, typename CoreType>
class TransformerTemplate {
public:
    template <typename T>
    using Transformer = TransformerTemplate<Ptr, NoneType, Maker, T>;
    using Vec = std::vector<Ptr>;

    explicit TransformerTemplate(Vec const& values)
        : m_values(values) {

    }

    explicit TransformerTemplate(Ptr const& value)
        : m_values({value}) {

    }

    TransformerTemplate() = default;

    Vec const& get() const {
        return m_values;
    }

    template <std::invocable<Ptr> Fn>
    Transformer<CoreType> transformOr(Fn f) const {
        if (m_spReplace) {
            return *this;
        }
        Transformer<CoreType> t;
        for (auto const& tr : m_values) {
            if (auto newValue = f(tr); newValue) {
                t.emplace(newValue);
            } else {
                t.emplace(tr);
            }
        }
        return t;
    }

    template <std::invocable<Ptr> Fn>
    Transformer<CoreType> transformMulti(Fn f) const {
        if (m_spReplace) {
            return *this;
        }
        Transformer<CoreType> t;
        for (auto const& tr : m_values) {
            if (auto newValue = f(tr); !newValue.empty()) {
                t.insert(newValue);
            } else {
                t.emplace(tr);
            }
        }
        return t;
    }

    template <std::invocable<Ptr> Fn>
    Transformer<CoreType> filter(Fn f) const {
        if (m_spReplace) {
            return *this;
        }
        Transformer<CoreType> t;
        for (auto const& tr : m_values) {
            if (f(tr)) {
                t.emplace(tr);
            }
        }
        return t;
    }

    template <typename Fn>
    Transformer<CoreType> replace(Fn f) {
        if (m_spReplace) {
            return *this;
        }
        for (auto const& tr : m_values) {
            m_spReplace = invoke(f, tr);
            if (m_spReplace) {
                return *this;
            }
        }
        return *this;
    }

    Transformer<CoreType> extractIfOne() {
        if (!m_spReplace && m_values.size() == 1) {
//            std::cout << "extract one" << m_values[0] << std::endl;
            m_spReplace = m_values[0];
        }
        return *this;
    }

    Transformer<CoreType> removeDuplicates() {
        if (m_spReplace || m_values.size() == 1) {
            return *this;
        }

        Transformer<CoreType> t;
        for (auto const& tr : m_values) {
            bool found = false;
            for (auto const& movedTr : t.m_values) {
                if (tr == movedTr) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                t.emplace(tr);
            }
        }

        if (t.m_values.size() != m_values.size()) {
            std::cout << "OPT alt: " << m_values.size() - t.m_values.size() << std::endl;
        }

        return t;
    }


    template <std::invocable<Ptr> Fn = Id>
    Ptr result(Fn f = Id{}) const {
        if (m_spReplace) {
//            std::cout << "replace" << m_spReplace << std::endl;
            return f(m_spReplace);
        }

        if (m_values.empty()) {
//            std::cout << "None" << std::endl;
            return f(Maker::make(NoneType{}));
        }

        if (m_hasChanges) {
            if constexpr (std::is_constructible_v<CoreType, Vec>) {
                return f(Maker::make(CoreType{m_values}));
            } else if constexpr (std::is_constructible_v<CoreType, Ptr>) {
                return f(Maker::make(CoreType{m_values[0]}));
            } else {
//            static_assert(always_false<CoreType>);
                return Ptr{};
            }
        } else {
            return nullptr;
        }
    }
private:
    template <typename Fn>
    auto invoke(Fn const& f, Ptr tr) const {
        if constexpr (std::is_invocable_v<Fn, Ptr>) {
            return f(tr);
        } else if constexpr (std::is_invocable_v<Fn, Ptr, Transformer<CoreType> const&>) {
            return f(tr, *this);
        } else {
            //            static_assert(always_false<CoreType>);
            return Ptr{};
        }
    }

    void emplace(Ptr ptr) {
        m_values.push_back(std::move(ptr));
    }


    template <typename Iterable>
    void insert(Iterable const& list) {
        m_values.insert(m_values.end(), list.begin(), list.end());
    }

    Vec m_values;
    bool m_hasChanges = true; // TODO
    Ptr m_spReplace;
};