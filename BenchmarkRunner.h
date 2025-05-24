#pragma once

#include "Xsum.h"
#include <chrono>
#include <format>

namespace Benchmark {

namespace {

template <size_t Size> constexpr std::array<double, Size> generateArray(const double value) {
    std::array<double, Size> arr;
    for (size_t i = 0; i < Size; ++i) {
        arr[i] = value;
    }
    return arr;
}

constexpr double ELEMENT = 1.1;
constexpr auto arr10 = generateArray<10>(ELEMENT);
constexpr auto arr100 = generateArray<100>(ELEMENT);
constexpr auto arr1000 = generateArray<1'000>(ELEMENT);
constexpr auto arr10000 = generateArray<10'000>(ELEMENT);
constexpr auto arr100000 = generateArray<100'000>(ELEMENT);

std::string getXsumKindName(XSUM::XsumKind kind) {
    return kind == XSUM::XsumKind::XsumSmall ? "XsumSmall" : "XsumLarge";
}

} // namespace

constexpr std::array<const std::span<const double>, 5> arrList{arr10, arr100, arr1000, arr10000, arr100000};

void runBenchmarkWithAddV(XSUM::XsumKind kind, size_t eachTestCaseIter = 10'000) {
    std::cout << std::format("# XsumAuto({}) addv() benchmark with {} iteration for each test case",
                             getXsumKindName(kind), eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumAuto xauto{kind};
            xauto.addv(arr);
            double res = xauto.computeRound();
            if (res != ELEMENT * arr.size()) {
                std::cout << std::format("wrong calculation with arr{}", arr.size()) << std::endl;
                std::exit(EXIT_FAILURE);
            }
        }
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
        std::cout << std::format("arr with {:10} elements: {:10}", arr.size(), duration) << std::endl;
    }
    std::cout << std::endl;
}

void runBenchmarkWithAdd1(XSUM::XsumKind kind, size_t eachTestCaseIter = 10'000) {
    std::cout << std::format("# XsumAuto({}) add1() benchmark with {} iteration for each test case",
                             getXsumKindName(kind), eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumAuto xauto{kind};
            for (const auto &ele : arr) {
                xauto.add1(ele);
            }
            double res = xauto.computeRound();
            if (res != ELEMENT * arr.size()) {
                std::cout << std::format("wrong calculation with arr{}", arr.size()) << std::endl;
                std::exit(EXIT_FAILURE);
            }
        }
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
        std::cout << std::format("arr with {:10d} elements: {:10}", arr.size(), duration) << std::endl;
    }
    std::cout << std::endl;
}

} // namespace Benchmark
