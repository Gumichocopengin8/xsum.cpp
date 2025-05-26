#pragma once

#include "Xsum.h"
#include "XsumOriginal.h"
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
constexpr std::array<const std::span<const double>, 5> arrList{arr10, arr100, arr1000, arr10000, arr100000};

constexpr std::string getXsumKindName(XSUM::XsumKind kind) {
    return kind == XSUM::XsumKind::XsumSmall ? "XsumSmall" : "XsumLarge";
}

constexpr size_t DEFAULT_EACT_TEST_CASE_ITERATION = 10'000;

} // namespace

void runXsumSmallBenchmarkWithAddV(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumSmall addv() benchmark with {} iteration for each test case", eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumSmall xsmall;
            xsmall.addv(arr);
            double res = xsmall.computeRound();
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

void runXsumSmallBenchmarkWithAdd1(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumSmall add1() benchmark with {} iteration for each test case", eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumSmall xsmall;
            for (const auto &ele : arr) {
                xsmall.add1(ele);
            }
            double res = xsmall.computeRound();
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

void runXsumLargeBenchmarkWithAddV(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumLarge addv() benchmark with {} iteration for each test case", eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumLarge xlarge;
            xlarge.addv(arr);
            double res = xlarge.computeRound();
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

void runXsumLargeBenchmarkWithAdd1(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumLarge add1() benchmark with {} iteration for each test case", eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            XSUM::XsumLarge xlarge;
            for (const auto &ele : arr) {
                xlarge.add1(ele);
            }
            double res = xlarge.computeRound();
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

void runXsumAutoBenchmarkWithAddV(XSUM::XsumKind kind, size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumAuto({}) addv() benchmark with {} iteration for each test case",
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

void runXsumAutoBenchmarkWithAdd1(XSUM::XsumKind kind, size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### XsumAuto({}) add1() benchmark with {} iteration for each test case",
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

void runXsumOriginalSmallBenchmarkWithAddv(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### Xsum Original Small add1() benchmark with {} iteration for each test case",
                             eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            xsum_small_accumulator exact_sum;
            xsum_small_init(&exact_sum);
            xsum_small_addv(&exact_sum, arr.data(), arr.size());
            double res = xsum_small_round(&exact_sum);
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

void runXsumOriginalSmallBenchmarkWithAdd1(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### Xsum Original Small add1() benchmark with {} iteration for each test case",
                             eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            xsum_small_accumulator exact_sum;
            xsum_small_init(&exact_sum);
            for (const auto &ele : arr) {
                xsum_small_add1(&exact_sum, ele);
            }
            double res = xsum_small_round(&exact_sum);
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

void runXsumOriginalLargeBenchmarkWithAddv(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### Xsum Original Large add1() benchmark with {} iteration for each test case",
                             eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            xsum_large_accumulator exact_sum;
            xsum_large_init(&exact_sum);
            xsum_large_addv(&exact_sum, arr.data(), arr.size());
            double res = xsum_large_round(&exact_sum);
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

void runXsumOriginalLargeBenchmarkWithAdd1(size_t eachTestCaseIter = DEFAULT_EACT_TEST_CASE_ITERATION) {
    std::cout << std::format("### Xsum Original Large add1() benchmark with {} iteration for each test case",
                             eachTestCaseIter)
              << std::endl;
    for (auto &arr : arrList) {
        auto start = std::chrono::high_resolution_clock::now();
        for (size_t i = 0; i < eachTestCaseIter; ++i) {
            xsum_large_accumulator exact_sum;
            xsum_large_init(&exact_sum);
            for (const auto &ele : arr) {
                xsum_large_add1(&exact_sum, ele);
            }
            double res = xsum_large_round(&exact_sum);
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
