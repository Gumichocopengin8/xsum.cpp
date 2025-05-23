#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <span>
#include <variant>
#include <vector>

namespace XSUM {

namespace {

struct XsumSmallAccumulator final {
    std::vector<int64_t> m_chunk; // Chunks making up small accumulator
    int m_addsUntilPropagate;     // Number of remaining adds before carry
    int64_t m_Inf;                // If non-zero, +Inf, -Inf, or NaN
    int64_t m_NaN;                // If non-zero, a NaN value with payload
    size_t m_sizeCount;           // number of added values
    bool m_hasPosNumber;          // check if added values have at least one positive number

    explicit XsumSmallAccumulator();
    explicit XsumSmallAccumulator(std::vector<int64_t> &&chunk, const int addsUntilPropagate, const int64_t inf,
                                  const int64_t nan, const size_t sizeCount, const bool hasPosNumber);

    int carryPropagate();
    void addInfNan(int64_t ivalue);
    inline void add1NoCarry(double value);
    inline void incrementWhenValueAdded(double value);
};

struct XsumLargeAccumulator final {
    std::vector<uint64_t> m_chunk;      // Chunks making up large accumulator
    std::vector<int_least16_t> m_count; // Counts of # adds remaining for chunks, or -1 if not used yet or special
    std::vector<uint64_t> m_chunksUsed; // Bits indicate chunks in use
    uint64_t m_usedUsed;                // Bits indicate chunk_used entries not 0
    XsumSmallAccumulator m_sacc;        // The small accumulator to condense into

    explicit XsumLargeAccumulator();

    void addLchunkToSmall(int_fast16_t ix);
    void largeAddValueInfNan(int_fast16_t ix, uint64_t uintv);
    void transferToSmall();
};

} // namespace

class XsumSmall final {
  public:
    explicit XsumSmall();
    explicit XsumSmall(XsumSmallAccumulator sacc);
    ~XsumSmall() = default;

    void addv(const std::span<const double> vec);
    void add1(double value);
    double computeRound();

  private:
    XsumSmallAccumulator m_sacc;
};

class XsumLarge final {
  public:
    explicit XsumLarge();
    ~XsumLarge() = default;

    void addv(const std::span<const double> vec);
    void add1(double value);
    double computeRound();

  private:
    XsumLargeAccumulator m_lacc;
};

class XsumAuto final {
  public:
    explicit XsumAuto();
    explicit XsumAuto(size_t expectedInputSize);
    ~XsumAuto() = default;

    void addv(const std::span<const double> vec);
    void add1(double value);
    double computeRound();

  private:
    using XsumVariant = std::variant<XsumSmall, XsumLarge>;
    XsumVariant m_acc;
};

namespace {

template <typename T>
concept XsumInterface = requires(T x, const std::span<const double> Vec, double Value) {
    { x.addv(Vec) } -> std::same_as<void>;
    { x.add1(Value) } -> std::same_as<void>;
    { x.computeRound() } -> std::same_as<double>;
};

static_assert(XsumInterface<XsumSmall>, "XsumSmall does not satisfy XsumInterface");
static_assert(XsumInterface<XsumLarge>, "XsumLarge does not satisfy XsumInterface");
static_assert(XsumInterface<XsumAuto>, "XsumAuto does not satisfy XsumInterface");

} // namespace

} // namespace XSUM
