#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <span>
#include <vector>

namespace XSUM {

constexpr int64_t XSUM_MANTISSA_BITS = 52; // Bits in fp mantissa, excludes implict 1
constexpr int64_t XSUM_EXP_BITS = 11;      // Bits in fp exponent
constexpr int64_t XSUM_MANTISSA_MASK = (static_cast<int64_t>(1) << XSUM_MANTISSA_BITS) - 1; // Mask for mantissa bits
constexpr int64_t XSUM_EXP_MASK = (1 << XSUM_EXP_BITS) - 1;                                 // Mask for exponent
constexpr int64_t XSUM_EXP_BIAS = (1 << (XSUM_EXP_BITS - 1)) - 1;              // Bias added to signed exponent
constexpr int64_t XSUM_SIGN_BIT = XSUM_MANTISSA_BITS + XSUM_EXP_BITS;          // Position of sign bit
constexpr uint64_t XSUM_SIGN_MASK = static_cast<uint64_t>(1) << XSUM_SIGN_BIT; // Mask for sign bit
constexpr int64_t XSUM_SCHUNK_BITS = 64;                                       // Bits in chunk of the small accumulator
constexpr int64_t XSUM_LOW_EXP_BITS = 5;                                  // # of low bits of exponent, in one chunk
constexpr int64_t XSUM_LOW_EXP_MASK = (1 << XSUM_LOW_EXP_BITS) - 1;       // Mask for low-order exponent bits
constexpr int64_t XSUM_HIGH_EXP_BITS = XSUM_EXP_BITS - XSUM_LOW_EXP_BITS; // # of high exponent bits for index
constexpr int64_t XSUM_SCHUNKS = (1 << XSUM_HIGH_EXP_BITS) + 3;           // # of chunks in small accumulator
constexpr int64_t XSUM_LOW_MANTISSA_BITS = 1 << XSUM_LOW_EXP_BITS;        // Bits in low part of mantissa
constexpr int64_t XSUM_LOW_MANTISSA_MASK = (static_cast<int64_t>(1) << XSUM_LOW_MANTISSA_BITS) - 1; // Mask for low bits
constexpr int64_t XSUM_SMALL_CARRY_BITS = (XSUM_SCHUNK_BITS - 1) - XSUM_MANTISSA_BITS; // Bits sums can carry into
constexpr int64_t XSUM_SMALL_CARRY_TERMS = (1 << XSUM_SMALL_CARRY_BITS) - 1; // # terms can add before need prop.

class XsumSmall final {
  public:
    explicit XsumSmall();
    ~XsumSmall() = default;

    void addv(const std::span<const double> vec);
    void add1(double value);
    double computeRound() const;

  private:
    struct XsumSmallAccumulator {
        std::array<int64_t, XSUM_SCHUNKS> chunk; // Chunks making up small accumulator
        int addsUntilPropagate;                  // Number of remaining adds before carry
        int64_t Inf;                             // If non-zero, +Inf, -Inf, or NaN
        int64_t NaN;                             // If non-zero, a NaN value with payload

        explicit constexpr XsumSmallAccumulator(const int addsUntilPropagate, const int64_t inf, const int64_t nan);
    };
    std::unique_ptr<XsumSmallAccumulator> m_sacc;
    size_t m_sizeCount;
    bool m_hasPosNumber;

    void xsumSmallAddInfNan(int64_t ivalue) const;
    inline void xsumAdd1NoCarry(double value) const;
    int xsumCarryPropagate() const;
    void incrementWhenValueAdded(double value);
};

} // namespace XSUM
