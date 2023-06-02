#pragma once

#include <array>
#include <vector>
#include <ostream>

template <typename T, std::size_t N>
constexpr std::array<T, N> make_array() noexcept {
    std::array<T, N> tempArray{};
    int count = 0;
    for(int &elem : tempArray) {
        elem = ++count;
    }
    return tempArray;
}


template <typename List>
std::ostream& join(std::ostream& os, List const& list, std::string_view delim) {
    bool isFirst = true;
    for (auto const& el : list) {
        if (!std::exchange(isFirst, false)) {
            os << delim;
        }
        os << el;
    }

    return os;
}

template <typename T>
std::vector<T> append(std::vector<T> arr, T t) {
    arr.push_back(std::move(t));
    return arr;
}


template <typename T>
std::vector<T> eraseElement(std::vector<T> const& arr, typename std::vector<T>::const_iterator erase) {
    std::vector<T> out; out.reserve(arr.size() - 1);
    for (auto it = arr.begin(); it != arr.end(); ++it) {
        if (it != erase) {
            out.emplace_back(*it);
        }
    }
    return out;
}

template <typename T>
std::vector<T> eraseElement(std::vector<T> arr, T const& eraseElem) {
    auto it = std::find(arr.begin(), arr.end(), eraseElem);
    if (it != arr.end()) {
        arr.erase(it);
    }
    return arr;
}

struct Id {
    template <typename T>
    decltype(auto) operator()(T &&t) const {
        return std::forward<T>(t);
    }
};

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template<typename T>
constexpr bool always_false = false;
