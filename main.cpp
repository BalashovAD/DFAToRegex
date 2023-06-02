#include <iostream>

#include <array>
#include <string>
#include <sstream>
#include <fstream>
#include <memory>
#include <utility>
#include <variant>
#include <vector>
#include <ranges>
#include <cassert>
#include <numeric>
#include <algorithm>
#include <functional>
#include <bitset>
#include <chrono>

#include "utils.h"
#include "Transformer.h"


using namespace std::ranges;

template <typename Ptr>
struct BrzozowskiTemplate {
    std::function<Ptr(size_t)> get;
    size_t index;
};

class Transition {
public:
    using ptr = std::shared_ptr<Transition>;
    using Brzozowski = BrzozowskiTemplate<ptr>;

    struct None {
        friend std::ostream& operator<<(std::ostream& os, None const& tr) {
            return os << "<NONE>";
        }

        friend bool operator==(None const& lhs, None const& rhs) {
            return true;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            return os << "<ERROR>";
        }

        ptr optimize() const {
            return nullptr;
        }

        ptr brzozowski(Brzozowski const& br) const {
            return nullptr;
        }

        size_t size() const {
            return 0;
        }
    };

    template <typename T>
    using Transformer = TransformerTemplate<ptr, None, Transition, T>;

    static auto optimizeFn() {
        return [](ptr const& tr) {
            return tr->optimize();
        };
    }


    static auto noneFilter() {
        return [](ptr const& tr) {
            return !tr->extract<None>();
        };
    }

    static auto brzFn(Brzozowski const& br) {
        return [&br](ptr const& tr) {
            return tr->brzozowski(br);
        };
    }

    struct ListOfTransitions {
        ListOfTransitions(ptr lhs, ptr rhs)
            : values({std::move(lhs), std::move(rhs)}) {}

        ListOfTransitions(std::vector<ptr> list, ptr tail)
            : values(append(std::move(list), std::move(tail))) {}


        template <typename Range>
        explicit ListOfTransitions(Range list)
            : values(list.begin(), list.end()) {}


        ptr tail() const {
            if (values.empty()) {
                return nullptr;
            } else {
                return *values.rbegin();
            }
        }

        std::vector<ptr> const values;
    };

    struct Alt : ListOfTransitions {
        using ListOfTransitions::ListOfTransitions;

        friend std::ostream& operator<<(std::ostream& os, Alt const& alt) {
            os << "(";
            bool isFirst = true;
            for (auto const& tr : alt.values) {
                if (!std::exchange(isFirst, false)) {
                    os << "|";
                }
                auto size = tr->size();
                if (size > 1) {
                    os << "(";
                }
                os << tr << "";
                if (size > 1) {
                    os << ")";
                }
            }
            os << ")";
            return os;
        }

        friend bool operator==(Alt const& lhs, Alt const& rhs) {
            if (lhs.values.size() != rhs.values.size()) {
                return false;
            }
            for (auto const& lhsTr : lhs.values) {
                bool found = false;
                for (auto const& rhsTr : rhs.values) {
                    if (lhsTr == rhsTr) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    return false;
                }
            }
            return true;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            os << "(";
            bool isFirst = true;
            for (auto const& tr : values) {
                if (!std::exchange(isFirst, false)) {
                    os << "|";
                }
                auto size = tr->size();
                if (size > 1) {
                    os << "(";
                }
                tr->regexFormat(os);
                if (size > 1) {
                    os << ")";
                }
            }
            os << ")";
            return os;
        }

        ptr optimize() const {
            return Transformer<Alt>{values}
            .transformOr(optimizeFn())
            .transformMulti([](ptr const& tr) {
                return tr->extractVec<Alt>([&](Alt const& concat) {
                    return concat.values;
                });
            }).filter(noneFilter())
            .extractIfOne()
            .removeDuplicates()
            .result();
        }

        ptr brzozowski(Brzozowski const& br) const {
            return Transformer<Alt>{values}
            .transformOr(optimizeFn())
            .transformOr(brzFn(br))
            .replace([&](ptr const& tr, Transformer<Alt> const& t) {
                return tr->extractPtr<Concat>([&](Concat const& concat) -> ptr {
                    std::cout << "BRZ ALT->CONCAT(" << concat << "): " << br.index << std::endl;
                    if (concat.tail()) {
                        return concat.tail()->extractPtr<Unknown>([&](Unknown const& unknown) -> ptr {
                            if (unknown.id == br.index) {
                                auto withoutUnknown = make(Concat{eraseElement(concat.values, concat.values.end() - 1)});
                                auto withoutIt = make(Alt{eraseElement(t.get(), tr)});
                                auto result = **withoutUnknown + withoutIt;
                                std::cout << "BRZ Replace to (" << result << "): " << br.index << std::endl;
                                if (auto updatedResult = result->brzozowski(br); updatedResult) {
                                    std::cout << "BRZ Replace UPDATED to (" << updatedResult << "): " << br.index << std::endl;
                                    return updatedResult;
                                } else {
                                    return result;
                                }
                            } else {
                                return nullptr;
                            }
                        });
                    } else {
                        return nullptr;
                    }
                });
            }).result();
        }

        size_t size() const {
            return std::accumulate(values.begin(), values.end(), 0, [](size_t p, ptr const& tr) {
                return p + tr->size();
            });
        }
    };
    struct Concat : ListOfTransitions {
        using ListOfTransitions::ListOfTransitions;

        friend std::ostream& operator<<(std::ostream& os, Concat const& tr) {
            join(os, tr.values, ".");
            return os;
        }

        friend bool operator==(Concat const& lhs, Concat const& rhs) {
            if (lhs.values.size() != rhs.values.size()) {
                return false;
            }
            for (auto i = 0; i != lhs.values.size(); ++i) {
                if (lhs.values[i] != rhs.values[i]) {
                    return false;
                }
            }
            return true;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            for (auto const& el : values) {
                el->regexFormat(os);
            }

            return os;
        }

        ptr optimize() const {
            return Transformer<Concat>{values}
            .transformOr(optimizeFn())
            .transformMulti([](ptr const& tr) {
                return tr->extractVec<Concat>([&](Concat const& concat) {
                    return concat.values;
                });
            }).replace([](ptr const& tr) -> ptr {
                if (tr->extract<None>()) {
                    return tr;
                } else {
                    return nullptr;
                }
            }).filter([](ptr const& tr) {
                return tr->size() > 0;
            }).extractIfOne()
            .result();
        }

        ptr brzozowski(Brzozowski const& br) const {
            return Transformer<Concat>{values}
            .transformOr(optimizeFn())
            .transformOr(brzFn(br))
            .result();
        }

        size_t size() const {
            return std::accumulate(values.begin(), values.end(), 0, [](size_t p, ptr const& tr) {
                return p + tr->size();
            });
        }
    };
    struct Star {
        ptr const value;

        friend std::ostream& operator<<(std::ostream& os, Star const& tr) {
            if (tr.value->size() == 1) {
                return os << tr.value << "*";
            } else {
                return os << '(' << tr.value << ")*";
            }
        }

        friend bool operator==(Star const& lhs, Star const& rhs) {
            return lhs.value == rhs.value;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            if (value->size() == 1 || value->extract<Alt>()) {
                return value->regexFormat(os) << "*";
            } else {
                os << "(";
                return value->regexFormat(os) << ")*";
            }
        }

        ptr optimize() const {
            return Transformer<Star>{value}
            .transformOr(optimizeFn())
            .transformOr([](ptr const& tr) -> ptr {
                return tr->extractPtr<Star>([](Star const& star) {
                    return star.value;
                });
            }).replace([](ptr const& tr) -> ptr {
                if (tr->extract<None>()) {
                    return make(Value{""});
                } else {
                    return nullptr;
                }
            }).replace([](ptr const& tr) -> ptr {
                if (tr->size() == 0) {
                    return tr;
                } else {
                    return nullptr;
                }
            }).result();
        }

        ptr brzozowski(Brzozowski const& br) const {
            return Transformer<Star>{value}
            .transformOr(brzFn(br))
            .result();
        }

        size_t size() const {
            return value->size();
        }
    };
    struct Value {
        std::string const value;

        friend std::ostream& operator<<(std::ostream& os, Value const& tr) {
            if (tr.value.empty()) {
                return os << "<eps>";
            } else {
                return os << tr.value;
            }
        }

        friend bool operator==(Value const& lhs, Value const& rhs) {
            return lhs.value == rhs.value;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            return os << value;
        }

        ptr optimize() const {
            return nullptr;
        }

        ptr brzozowski(Brzozowski const& br) const {
            return nullptr;
        }

        size_t size() const {
            return value.size();
        }
    };
    struct Unknown {
        size_t const id;

        friend std::ostream& operator<<(std::ostream& os, Unknown const& tr) {
            return os << "<S_" << tr.id << ">";
        }

        friend bool operator==(Unknown const& lhs, Unknown const& rhs) {
            return lhs.id == rhs.id;
        }

        std::ostream& regexFormat(std::ostream& os) const {
            return os << "<ERROR>";
        }

        ptr optimize() const {
            return nullptr;
        }

        ptr brzozowski(Brzozowski const& br) const {
            if (auto replacedNode = br.get(id); replacedNode) {
                if (auto updatedNode = replacedNode->brzozowski(br); updatedNode) {
                    return updatedNode;
                } else {
                    return replacedNode;
                }
            } else {
                return nullptr;
            }
        }

        size_t size() const {
            return 1;
        }
    };

    using Variant = std::variant<Alt, Star, Concat, Value, Unknown, None>;

    template<std::convertible_to<Variant> T>
    explicit Transition(T &&t)
        : m_transaction(std::forward<T>(t)) {

    }

    friend std::ostream& operator<<(std::ostream& os, Transition::ptr const& spTransaction) {
        return std::visit([&os](auto value) -> std::ostream& {
           return os << value;
        }, spTransaction->m_transaction);
    }

    friend bool operator==(Transition::ptr const& lhs, Transition::ptr const& rhs) {
        return std::visit([&](auto const& valueLhs) -> bool {
            using Type = std::decay_t<decltype(valueLhs)>;
            return std::visit(overloaded{
                [&](Type const& valueRhs) {
                    return valueLhs == valueRhs;
                }, [](auto const& t) {
                return false;
            }}, rhs->m_transaction);
        }, lhs->m_transaction);
    }

    std::ostream& regexFormat(std::ostream& os) {
        return std::visit([&os](auto value) -> std::ostream& {
           return value.regexFormat(os);
        }, m_transaction);
    }

    template<class T>
    static ptr make(T &&t) {
        return std::make_shared<Transition>(std::forward<T>(t));
    }

    static ptr orNone(ptr const& t) {
        if (t) {
            return t;
        } else {
            return make(None{});
        }
    }

    ptr alt(ptr const& transition) const {
        return std::visit(overloaded{[&](Alt const& altTr) {
            return make(Alt{altTr.values, transition});
        }, [&](auto const& notAlt) {
            return make(Alt{make(notAlt), transition});
        }}, m_transaction);
    }

    ptr star() const {
        return std::visit(overloaded{[](Star const& starTr) {
            return make(starTr);
        }, [](auto const& notStar) {
            return make(Star{make(notStar)});
        }}, m_transaction);
    }

    ptr concat(ptr const& transition) const {
        return std::visit(overloaded{[&](Concat const& concatTr) {
            return make(Concat{concatTr.values, transition});
        }, [&](auto const& notConcat) {
            return make(Concat{make(notConcat), transition});
        }}, m_transaction);
    }

    ptr clone() const {
        return std::visit([](auto const& t) {
            return make(t);
        }, m_transaction);
    }

    template <typename T, typename Fn = Id>
    bool extract(Fn f = Id{}) const {
        return std::visit(overloaded{[&](T const& t) {
            f(t);
            return true;
        }, [](auto _) {
            return false;
        }}, m_transaction);
    }

    template <typename T, typename Fn>
    ptr extractPtr(Fn f) const {
        return std::visit(overloaded{[&](T const& t) -> ptr {
            return f(t);
        }, [](auto _) -> ptr {
            return nullptr;
        }}, m_transaction);
    }

    template <typename T, typename Fn>
    std::vector<ptr> extractVec(Fn f) const {
        return std::visit(overloaded{[&](T const& t) -> std::vector<ptr> {
            return f(t);
        }, [](auto _) -> std::vector<ptr> {
            return {};
        }}, m_transaction);
    }

    size_t size() const {
        return std::visit([&](auto const& t) {
            return t.size();
        }, m_transaction);
    }

    ptr optimize() const {
        return std::visit([&](auto const& t) {
            return t.optimize();
        }, m_transaction);
    }

    ptr brzozowski(Brzozowski const& br) const {
        return std::visit([&](auto const& t) {
            return t.brzozowski(br);
        }, m_transaction);
    }

    static ptr optimizePtr(ptr const& tr) {
        if (auto newTr = tr->optimize(); newTr) {
            return newTr;
        } else {
            return tr;
        }
    }

    static ptr value(std::string const& value) {
        return make(Value{value});
    }

    static ptr unknown(size_t const& value) {
        return make(Unknown{value});
    }

    friend ptr operator|(ptr const& lhs, ptr const& rhs) {
        if (!lhs) {
            return rhs;
        }
        return lhs->alt(rhs);
    }

    friend ptr operator+(ptr const& lhs, ptr const& rhs) {
        return lhs->concat(rhs);
    }

    friend ptr operator*(Transition const& lhs) {
        return lhs.star();
    }
private:
    Variant const m_transaction;
};


auto operator""_tr (const char* str, std::size_t size) {
    return Transition::make(Transition::Value{std::string{str, size}});
}

auto operator""_st (std::size_t size) {
    return Transition::make(Transition::Unknown{size});
}


static inline const Transition::ptr EMPTY = ""_tr;

using StateName = size_t;

template <size_t N>
struct State {
    // explicit
    State(size_t t_id = 0) noexcept
        : name(t_id) {}

    StateName name;
    std::array<Transition::ptr, N> links;
    bool isFinal = false;
};


template <size_t N>
struct SystemOfEq {
    static_assert(N > 0);

    std::array<Transition::ptr, N> vars;

    void optimize() {
        for (auto& var : vars) {
            std::cout << "Before: " << var << std::endl;
            var = Transition::optimizePtr(var);
            std::cout << "After: " << var << std::endl;
        }
    }

    std::ostream& print(std::ostream& os) const {
        for (size_t i = 0; i != N; ++i) {
            os << Transition::Unknown{i} << " = " << vars[i] << "\n";
        }
        return os;
    }

    std::ostream& regexFormat(std::ostream& os) const {
        vars[0]->regexFormat(os);
        return os;
    }

    void brzozowski() {
        for (size_t i = N - 1; i != -1; --i) {
            const auto get = [&](size_t index) -> Transition::ptr {
                if (index > i) {
                    return vars[index];
                } else {
                    return nullptr;
                }
            };
            Transition::Brzozowski br{get, i};
            std::cout << "Start BRZ: " << i << " - " << vars[i] << std::endl;
            if (auto newVar = vars[i]->brzozowski(br); newVar) {
                std::cout << "Update " << i << ": " << newVar << std::endl;
                vars[i] = Transition::optimizePtr(newVar);
            } else {
                std::cout << "No update " << i << std::endl;
            }
            std::cout << "Step" << i << ":" << vars[i]->size() << "\n";
        }
    }
};


template <size_t N>
class States {
public:
    States() noexcept {
        std::iota(m_states.begin(), m_states.end(), 0);
    }

    template <size_t pos>
        requires(pos < N)
    struct Maker {
        States& states;

        template <size_t to>
        Maker<pos> add(Transition::ptr const& spTransition) {
            static_assert(to < N);
            return add(to, spTransition);
        }

        Maker<pos> add(size_t to, Transition::ptr const& spTransition) {
            assert(to < N);
            auto& spStateTr = states.m_states[pos].links[to];
            if (spStateTr) {
                spStateTr = spStateTr->alt(spTransition);
            } else {
                spStateTr = spTransition;
            }
            return *this;
        }

        Maker<pos> finalState() {
            states.m_states[pos].isFinal = true;
            return *this;
        }

        auto next() {
            if constexpr (pos + 1 == N) {
                return Checker{states};
            } else {
                return Maker<pos + 1>{states};
            }
        }
    };

    struct Checker {
        States& states;

        explicit Checker(States& t_states)
            : states(t_states) {
        };

        bool finish() const {
            return any_of(states.m_states, &State<N>::isFinal);
        }
    };

    template <size_t pos = 0>
    Maker<pos> maker() {
        return Maker<pos>{*this};
    }

    void add(size_t state, size_t to, Transition::ptr tr) {
        if (state >= N || to >= N) {
            throw std::runtime_error("Bad index");
        }

        auto& spTr = m_states[state].links[to];
        spTr = spTr | tr;
    }

    std::ostream& printScheme(std::ostream& os) const {
        {
            os << "digraph finite_state_machine {\n\trankdir=RL;\n\tsize=\"8,5\";\n\n";
        }
        {
            std::vector<Transition::Unknown> finalPos;
            os << "\t" << "node [shape = circle]; ";
            for (auto const& state : m_states) {
                if (state.isFinal) {
                    finalPos.push_back(Transition::Unknown{state.name});
                } else {
                    os << Transition::Unknown{state.name} << " ";
                }
            }
            os << ";\n";

            os << "\t" << "node [shape = doublecircle]; ";
            join(os, finalPos, " ") << ";\n\n";
        }
        {
            for (const auto& state : m_states) {
                size_t pos = 0;
                for (const auto& spTr : state.links) {
                    if (spTr) {
                        os << "\t" << Transition::Unknown{state.name} << " -> " << Transition::Unknown{pos} << " [label = \"" << spTr << "\"];\n";
                    }

                    ++pos;
                }
            }
        }
        {
            os << "}\n";
        }
        return os.flush();
    }

    std::ostream& printSchemeV2(std::ostream& os) const {
        os << "#states\n";
        std::vector<Transition::Unknown> finalPos;
        for (auto const& state : m_states) {
            os << "s" << state.name << "\n";
        }
        os << "#initial\ns0\n#accepting\n";
        for (auto const& state : m_states) {
            if (state.isFinal) {
                os << "s" << state.name << "\n";
            }
        }
        os << "#alphabet\n0\n1\n#transitions\n";
        for (const auto& state : m_states) {
            size_t pos = 0;
            for (const auto& spTr : state.links) {
                if (spTr) {
                    os << "s" << state.name << ":" << spTr << ">" << "s" << pos << "\n";
                }

                ++pos;
            }
        }

        return os.flush();
    }

    SystemOfEq<N> makeSystem() const {
        SystemOfEq<N> system;
        for (auto const& state : m_states) {
            std::vector<Transition::ptr> rightSide;

            for (size_t i = 0; i != N; ++i) {
                auto spTr = state.links[i];
                if (spTr) {
                    auto spTrWithUnknown = spTr + Transition::unknown(i);
                    rightSide.emplace_back(spTrWithUnknown);
                }
            }
            if (state.isFinal) {
                rightSide.emplace_back(EMPTY);
            }

            system.vars[state.name] = Transition::make(Transition::Alt{rightSide});
        }
        return system;
    }

    Transition::ptr stateRemoval() {
        size_t lastSize = 0;
        for (auto i = 1; i != N; ++i) {
            auto& state = m_states[i];
            if (state.isFinal) {
                continue;
            }
            auto loopLink = state.links[i] ? **state.links[i] : ""_tr;
            for (auto& fromState : m_states) {
                if (fromState.name != i) {
                    if (auto linkTo = fromState.links[i]; linkTo) {
                         for (auto j = 0; j != N; ++j) {
                            if (j == i) {
                                continue;
                            }
                            if (state.links[j]) {
                                fromState.links[j] = Transition::optimizePtr(
                                        fromState.links[j] | (linkTo + loopLink + state.links[j]));
                            }
                        }
                        fromState.links[i] = nullptr;
                    }
                }
            }
        }

        auto startState = m_states[0];
        if (startState.isFinal) {
            return Transition::optimizePtr(startState.links[0] ? **startState.links[0] : ""_tr);
        }
        for (auto i = 0; i != N; ++i) {
            if (m_states[i].isFinal) {
                auto endState = m_states[i];
                auto loopStart = startState.links[0] ? **startState.links[0] : ""_tr;
                auto loopEnd = endState.links[i] ? endState.links[i] : ""_tr;
                auto toLink = Transition::orNone(startState.links[i]);
                auto fromLink = Transition::orNone(endState.links[0]);
                auto secondPart = **(loopEnd | fromLink + loopStart + toLink);
                auto firstPart = loopStart + toLink;
                auto spAnswer = firstPart + secondPart;
                return Transition::optimizePtr(spAnswer);
            }
        }

        return Transition::make(Transition::None{});
    }
private:
    std::array<State<N>, N> m_states;
};

void test() {
    States<2> fsm;
    auto hasFinalState = fsm.maker()
            .add<0>("a"_tr).add<1>("b"_tr).next()
            .add<1>("b"_tr).finalState().next()
            .finish();

    if (!hasFinalState) {
        std::cout << "No final state" << std::endl;
        throw std::runtime_error("Simple solve failed");
    }
    auto system = fsm.makeSystem();
    system.optimize();
    system.brzozowski();
    system.optimize();
    std::stringstream os;
    system.regexFormat(os);

    if (os.str() != "a*bb*") {
//        throw std::runtime_error("Simple solve failed");
    }

    std::stringstream osStateRemoval;
    auto spAnswer = fsm.stateRemoval();
    spAnswer->regexFormat(osStateRemoval);
    if (osStateRemoval.str() != "a*bb*") {
//        throw std::runtime_error("Simple solve failed");
    }
}

void test2() {
    States<2> fsm;
    auto hasFinalState = fsm.maker()
            .add<0>("b"_tr).add<1>("a"_tr).finalState().next()
            .add<0>("a"_tr).add<1>("b"_tr).next()
            .finish();

    if (!hasFinalState) {
        std::cout << "No final state" << std::endl;
//        throw std::runtime_error("Simple solve failed");
    }
    auto system = fsm.makeSystem();
    system.optimize();
    system.brzozowski();
    system.optimize();
    std::stringstream os;
    system.regexFormat(os);

    if (os.str() != "a*bb*") {
        std::cout << "F: " << os.str() << std::endl;
//        throw std::runtime_error("Simple solve failed: " + os.str());
    }

    std::stringstream osStateRemoval;
    auto spAnswer = fsm.stateRemoval();
    spAnswer->regexFormat(osStateRemoval);
    if (osStateRemoval.str() != "(b|(ab*a))*") {
        std::cout << "F: " << osStateRemoval.str() << std::endl;
//        throw std::runtime_error("Simple solve failed" + os.str());
    }
}

void test3() {
    States<3> fsm;
    auto hasFinalState = fsm.maker()
            .add<0>("b"_tr).add<1>("a"_tr).next()
            .add<1>("a"_tr).add<2>("b"_tr).next()
            .add<2>("a"_tr).add<2>("b"_tr).finalState().next()
            .finish();

    if (!hasFinalState) {
        std::cout << "No final state" << std::endl;
        throw std::runtime_error("Simple solve failed");
    }

    auto system = fsm.makeSystem();
    system.optimize();
    system.brzozowski();
    system.optimize();
    std::stringstream os;
    system.regexFormat(os);

    if (os.str() != "b*aa*b(a|b)*") {
        std::cout << "F3: " << os.str() << std::endl;
//        throw std::runtime_error("Simple solve failed: " + os.str());
    }

    std::stringstream osStateRemoval;
    auto spAnswer = fsm.stateRemoval();
    spAnswer->regexFormat(osStateRemoval);
    if (osStateRemoval.str() != "b*aa*b(a|b)*") {
        std::cout << "F: " << osStateRemoval.str() << std::endl;
//        throw std::runtime_error("Simple solve failed" + os.str());
    }
}

void test4() {
    States<3> fsm;
    auto hasFinalState = fsm.maker()
            .add<1>("a"_tr).finalState().next()
            .add<2>("a"_tr).next()
            .add<0>("a"_tr).next()
            .finish();

    if (!hasFinalState) {
        std::cout << "No final state" << std::endl;
        throw std::runtime_error("Simple solve failed");
    }

    std::ofstream schemeFile("scheme2.txt");
    std::ofstream scheme2File("scheme3.txt");
    fsm.printScheme(schemeFile);
    fsm.printSchemeV2(scheme2File);

    auto system = fsm.makeSystem();
    system.optimize();
    system.brzozowski();
    system.optimize();
    std::stringstream os;
    system.regexFormat(os);

    if (os.str() != "(aaa)*") {
        std::cout << "F: " << os.str() << std::endl;
//        throw std::runtime_error("Simple solve failed: " + os.str());
    }

    std::stringstream osStateRemoval;
    auto spAnswer = fsm.stateRemoval();
    spAnswer->regexFormat(osStateRemoval);
    if (osStateRemoval.str() != "(aaa)*") {
        std::cout << "F: " << osStateRemoval.str() << std::endl;
//        throw std::runtime_error("Simple solve failed" + os.str());
    } else {
        std::cout << "PASS: " << osStateRemoval.str() << std::endl;
    }
}

#define EXPECT(x, y) \
{                    \
    std::stringstream s; \
    (x)->optimize()->regexFormat(s);                     \
if ((s.str()) != (y)) throw std::runtime_error("Error: "  __FILE__  ":" + std::to_string(__LINE__) + ": " + s.str() + "?" + y); \
}

void testOpt() {
    std::vector<Transition::ptr> t{"a"_tr};
    EXPECT(Transition::make(Transition::Alt{t}), "a");
    EXPECT(("a"_tr | "b"_tr | "c"_tr) + Transition::make(Transition::None{}), "<ERROR>");
    EXPECT(("a"_tr | "b"_tr | "c"_tr) + **Transition::make(Transition::None{}), "(a|b|c)");
    EXPECT(**("b"_tr | Transition::make(Transition::None{})), "b*");
}


void f1() {
    auto _0 = "000"_tr;
    auto _1 = "001"_tr;
    auto _2 = "010"_tr;
    auto _3 = "011"_tr;
    auto _4 = "100"_tr;
    auto _5 = "101"_tr;
    auto _6 = "110"_tr;
    auto _7 = "111"_tr;

    std::array<Transition::ptr, 8> tr{_0, _1, _2, _3, _4, _5, _6, _7};

    auto loop = _0 | _7;
    States<7 + 1> fsm;
    fsm.maker().add<3 + 1>("11"_tr).add<2 + 1>("10"_tr).add<1 + 1>("1"_tr).next().finalState();
    for (auto i = 0; i != 7; ++i) {
        fsm.add(i + 1, i + 1, loop);
        for (auto j = 1; j != 7; ++j) {
            fsm.add(i + 1, (i + j) % 7 + 1, tr[j]);
        }
    }

    States<7> fsmDiv3;
    fsmDiv3.maker().finalState();
    for (auto i = 0; i != 7; ++i) {
        fsmDiv3.add(i, i, loop);
        for (auto j = 1; j != 7; ++j) {
            fsmDiv3.add(i, (i + j) % 7, tr[j]);
        }
    }

    std::ofstream schemeFile("s.txt");
    if (!schemeFile) {
        throw std::runtime_error("cannot open file");
    }

    fsm.printScheme(schemeFile);

    std::ofstream eqFile("e.txt");
    if (!eqFile) {
        throw std::runtime_error("cannot open file");
    }

    auto trResult = fsm.stateRemoval();
    auto trResultDiv3 = fsmDiv3.stateRemoval();

    Transition::optimizePtr(trResult | trResultDiv3)->regexFormat(eqFile);
}


template <size_t N>
Transition::ptr solve() {
    auto const base = 2;
    auto const _0 = "0"_tr;
    auto const _1 = "1"_tr;

    States<N> dfa;
    dfa.maker().finalState();

    for (auto i = 0; i != N; ++i) {
        dfa.add(i, (i * base) % N, _0);
        dfa.add(i, (i * base + 1) % N, _1);
    }

    return dfa.stateRemoval();
}


template <size_t N>
std::string generateSolutions() {
    if constexpr (N > 1) {
        std::stringstream os;
        os << "case " << N << ": return \"";
        solve<8>()->regexFormat(os);
        os << "\";\n";
        return os.str() + generateSolutions<N - 1>();
    } else {
        return "default: return \"\";";
    }
}

int main() {
//    testOpt();
//    test();
    test2();
    test3();
    test4();
    f1();

    using namespace std::chrono;
    auto start = system_clock::now();
    auto str = generateSolutions<18>();
    std::cout << "Time: " << duration_cast<milliseconds>(system_clock::now() - start) << std::endl;

    std::ofstream resultFile("result.txt");
    if (!resultFile) {
        throw std::runtime_error("cannot open file");
    }
    resultFile << str;

    return 0;
}
