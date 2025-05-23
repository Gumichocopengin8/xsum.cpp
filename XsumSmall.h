#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <span>
#include <vector>

namespace XSUM {

struct XsumSmallAccumulator final {
    std::vector<int64_t> m_chunk; // Chunks making up small accumulator
    int m_addsUntilPropagate;     // Number of remaining adds before carry
    int64_t m_Inf;                // If non-zero, +Inf, -Inf, or NaN
    int64_t m_NaN;                // If non-zero, a NaN value with payload
    size_t m_sizeCount;           // number of added values
    bool m_hasPosNumber;          // check if added values have at least one positive number

    explicit constexpr XsumSmallAccumulator(const int addsUntilPropagate, const int64_t inf, const int64_t nan);
    explicit constexpr XsumSmallAccumulator(const std::span<const int64_t> chunk, const int addsUntilPropagate,
                                            const int64_t inf, const int64_t nan, const size_t sizeCount,
                                            const bool hasPosNumber);

    int xsumCarryPropagate();
    void xsumSmallAddInfNan(int64_t ivalue);
    inline void incrementWhenValueAdded(double value);
};

struct XsumLargeAccumulator final {
    std::vector<uint64_t> m_chunk;      // Chunks making up large accumulator
    std::vector<int_least16_t> m_count; // Counts of # adds remaining for chunks, or -1 if not used yet or special
    std::vector<uint64_t> m_chunksUsed; // Bits indicate chunks in use
    uint64_t m_usedUsed;                // Bits indicate chunk_used entries not 0
    XsumSmallAccumulator m_sacc;        // The small accumulator to condense into

    explicit constexpr XsumLargeAccumulator();

    void addLchunkToSmall(int_fast16_t ix);
    void largeAddValueInfNan(int_fast16_t ix, uint64_t uintv);
};

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

    inline void xsumAdd1NoCarry(double value);
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

    void transferToSmall();
};

} // namespace XSUM
