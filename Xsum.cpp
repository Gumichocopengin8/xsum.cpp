#include "Xsum.h"

namespace XSUM {

namespace {

// CONSTANTS DEFINING THE FLOATING POINT FORMAT
constexpr int64_t XSUM_MANTISSA_BITS = 52; // Bits in fp mantissa, excludes implict 1
constexpr int64_t XSUM_EXP_BITS = 11;      // Bits in fp exponent
constexpr int64_t XSUM_MANTISSA_MASK = (static_cast<int64_t>(1) << XSUM_MANTISSA_BITS) - 1; // Mask for mantissa bits
constexpr int64_t XSUM_EXP_MASK = (1 << XSUM_EXP_BITS) - 1;                                 // Mask for exponent
constexpr int64_t XSUM_EXP_BIAS = (1 << (XSUM_EXP_BITS - 1)) - 1;              // Bias added to signed exponent
constexpr int64_t XSUM_SIGN_BIT = XSUM_MANTISSA_BITS + XSUM_EXP_BITS;          // Position of sign bit
constexpr uint64_t XSUM_SIGN_MASK = static_cast<uint64_t>(1) << XSUM_SIGN_BIT; // Mask for sign bit

// CONSTANTS DEFINING THE SMALL ACCUMULATOR FORMAT
constexpr int64_t XSUM_SCHUNK_BITS = 64;                                  // Bits in chunk of the small accumulator
constexpr int64_t XSUM_LOW_EXP_BITS = 5;                                  // # of low bits of exponent, in one chunk
constexpr int64_t XSUM_LOW_EXP_MASK = (1 << XSUM_LOW_EXP_BITS) - 1;       // Mask for low-order exponent bits
constexpr int64_t XSUM_HIGH_EXP_BITS = XSUM_EXP_BITS - XSUM_LOW_EXP_BITS; // # of high exponent bits for index
constexpr int64_t XSUM_SCHUNKS = (1 << XSUM_HIGH_EXP_BITS) + 3;           // # of chunks in small accumulator
constexpr int64_t XSUM_LOW_MANTISSA_BITS = 1 << XSUM_LOW_EXP_BITS;        // Bits in low part of mantissa
constexpr int64_t XSUM_LOW_MANTISSA_MASK = (static_cast<int64_t>(1) << XSUM_LOW_MANTISSA_BITS) - 1; // Mask for low bits
constexpr int64_t XSUM_SMALL_CARRY_BITS = (XSUM_SCHUNK_BITS - 1) - XSUM_MANTISSA_BITS; // Bits sums can carry into
constexpr int64_t XSUM_SMALL_CARRY_TERMS = (1 << XSUM_SMALL_CARRY_BITS) - 1; // # terms can add before need prop.

// CONSTANTS DEFINING THE LARGE ACCUMULATOR FORMAT
constexpr int64_t XSUM_LCOUNT_BITS = 64 - XSUM_MANTISSA_BITS; // # of bits in count
constexpr int64_t XSUM_LCHUNKS = 1 << (XSUM_EXP_BITS + 1);    // # of chunks in large accumulator

// CONSTANTS DEFINING THE XsumAuto
constexpr size_t XSUM_THRESHOLD = 1000;

// XsumSmallAccumulator
XsumSmallAccumulator::XsumSmallAccumulator()
    : m_chunk(XSUM_SCHUNKS, 0LL), m_addsUntilPropagate{XSUM_SMALL_CARRY_TERMS}, m_Inf{0}, m_NaN{0}, m_sizeCount{0},
      m_hasPosNumber{false} {}

XsumSmallAccumulator::XsumSmallAccumulator(std::vector<int64_t> &&chunk, const int addsUntilPropagate,
                                           const int64_t inf, const int64_t nan, const size_t sizeCount,
                                           const bool hasPosNumber)
    : m_chunk(std::move(chunk)), m_addsUntilPropagate{addsUntilPropagate}, m_Inf{inf}, m_NaN{nan},
      m_sizeCount{sizeCount}, m_hasPosNumber{hasPosNumber} {}

/*
    ADD AN INF OR NAN TO A SMALL ACCUMULATOR.  This only changes the flags,
    not the chunks in the accumulator, which retains the sum of the finite
    terms (which is perhaps sometimes useful to access, though no function
    to do so is defined at present).  A NaN with larger payload (seen as a
    52-bit unsigned integer) takes precedence, with the sign of the NaN always
    being positive.  This ensures that the order of summing NaN values doesn't
    matter.
*/
void XsumSmallAccumulator::addInfNan(const int64_t ivalue) {
    const int64_t mantissa = ivalue & XSUM_MANTISSA_MASK;

    if (mantissa == 0) {  // Inf
        if (m_Inf == 0) { // no previous Inf
            m_Inf = ivalue;
        } else if (m_Inf != ivalue) { // previous Inf was opposite sign
            double fltv = std::bit_cast<double>(ivalue);
            fltv = fltv - fltv; // result will be a NaN
            m_Inf = std::bit_cast<int64_t>(fltv);
        }
    } else { // NaN
        // Choose the NaN with the bigger payload and clear its sign.
        // Using <= ensures that we will choose the first NaN over the previous zero.
        if ((m_NaN & XSUM_MANTISSA_MASK) <= mantissa) {
            m_NaN = ivalue & ~XSUM_SIGN_MASK;
        }
    }
}

/*
    PROPAGATE CARRIES TO NEXT CHUNK IN A SMALL ACCUMULATOR.  Needs to
    be called often enough that accumulated carries don't overflow out
    the top, as indicated by sacc.addsUntilPropagate.  Returns the
    index of the uppermost non-zero chunk (0 if number is zero).

    After carry propagation, the uppermost non-zero chunk will indicate
    the sign of the number, and will not be -1 (all 1s).  It will be in
    the range -2^XSUM_LOW_MANTISSA_BITS to 2^XSUM_LOW_MANTISSA_BITS - 1.
    Lower chunks will be non-negative, and in the range from 0 up to
    2^XSUM_LOW_MANTISSA_BITS - 1.
*/
int XsumSmallAccumulator::carryPropagate() {
    // Set u to the index of the uppermost non-zero (for now) chunk, or
    // return with value 0 if there is none.
    int u = XSUM_SCHUNKS - 1;
    while (0 <= u && m_chunk[u] == 0) {
        if (u == 0) {
            m_addsUntilPropagate = XSUM_SMALL_CARRY_TERMS - 1;
            return 0;
        }
        --u;
    }

    // At this point, m_chunk[u] must be non-zero
    assert(m_chunk[u] != 0);

    // Carry propagate, starting at the low-order chunks.  Note that the
    // loop limit of u may be increased inside the loop.
    int i = 0;    // set to the index of the next non-zero chunck, from bottom
    int uix = -1; // indicates that a non-zero chunk has not been found yet

    do {
        int64_t c;     // Set to the chunk at index i (next non-zero one)
        int64_t clow;  // Low-order bits of c
        int64_t chigh; // High-order bits of c

        // Find the next non-zero chunk, setting i to its index, or break out
        // of loop if there is none.  Note that the chunk at index u is not
        // necessarily non-zero - it was initially, but u or the chunk at u
        // may have changed.
        do {
            c = m_chunk[i];
            if (c != 0) {
                break;
            }
            i += 1;
        } while (i <= u);

        if (i > u) {
            break;
        }

        chigh = c >> XSUM_LOW_MANTISSA_BITS;
        if (chigh == 0) {
            uix = i;
            i += 1;
            continue; // no need to change this chunk
        }

        if (u == i) {
            if (chigh == -1) {
                uix = i;
                break; // don't propagate -1 into the region of all zeros above
            }
            u = i + 1; // we will change chunk[u+1], so we'll need to look at it
        }

        clow = c & XSUM_LOW_MANTISSA_MASK;
        if (clow != 0) {
            uix = i;
        }

        // We now change chunk[i] and add to chunk[i+1]. Note that i+1 should be
        // in range (no bigger than XSUM_CHUNKS-1) if summing memory, since
        // the number of chunks is big enough to hold any sum, and we do not
        // store redundant chunks with values 0 or -1 above previously non-zero
        // chunks.  But other add operations might cause overflow, in which
        // case we produce a NaN with all 1s as payload.  (We can't reliably produce
        // an Inf of the right sign.)

        m_chunk[i] = clow;
        if (i + 1 >= XSUM_SCHUNKS) {
            this->addInfNan((static_cast<int64_t>(XSUM_EXP_MASK) << XSUM_MANTISSA_BITS) | XSUM_MANTISSA_MASK);
            u = i;
        } else {
            m_chunk[i + 1] += chigh; // note: this could make this chunk be zero
        }

        i += 1;

    } while (i <= u);

    // Check again for the number being zero, since carry propagation might
    // have created zero from something that initially looked non-zero.
    if (uix < 0) {
        uix = 0;
        m_addsUntilPropagate = XSUM_SMALL_CARRY_TERMS - 1;
        return uix;
    }

    // While the uppermost chunk is negative, with value -1, combine it with
    // the chunk below (if there is one) to produce the same number but with
    // one fewer non-zero chunks.
    while (m_chunk[uix] == -1 && uix > 0) {
        // Left shift of a negative number is undefined according to the standard,
        // so do a multiply - it's all presumably constant-folded by the compiler.
        m_chunk[uix - 1] += static_cast<int64_t>(-1) * (static_cast<int64_t>(1) << XSUM_LOW_MANTISSA_BITS);
        m_chunk[uix] = 0;
        uix -= 1;
    }

    m_addsUntilPropagate = XSUM_SMALL_CARRY_TERMS - 1;
    return uix; // Return index of uppermost non-zero chunk
}

/*
    ADD ONE NUMBER TO A SMALL ACCUMULATOR ASSUMING NO CARRY PROPAGATION REQ'D.
    This function is declared INLINE regardless of the setting of INLINE_SMALL
    and for good performance it must be inlined by the compiler (otherwise the
    procedure call overhead will result in substantial inefficiency).
*/
inline void XsumSmallAccumulator::add1NoCarry(const double value) {
    const int64_t ivalue = std::bit_cast<int64_t>(value);

    // Extract exponent and mantissa.  Split exponent into high and low parts.
    int_fast16_t exp = (ivalue >> XSUM_MANTISSA_BITS) & XSUM_EXP_MASK;
    int64_t mantissa = ivalue & XSUM_MANTISSA_MASK;
    const int_fast16_t highExp = exp >> XSUM_LOW_EXP_BITS;
    int_fast16_t lowExp = exp & XSUM_LOW_EXP_MASK;

    // Categorize number as normal, denormalized, or Inf/NaN according to
    // the value of the exponent field.
    if (exp == 0) { // zero or denormalized
        // If it's a zero (positive or negative), we do nothing.
        if (mantissa == 0) {
            return;
        }
        // Denormalized mantissa has no implicit 1, but exponent is 1 not 0.
        exp = lowExp = 1;
    } else if (exp == XSUM_EXP_MASK) { // Inf or NaN
        // Just update flags in accumulator structure.
        this->addInfNan(ivalue);
        return;
    } else { // normalized
        // OR in implicit 1 bit at top of mantissa
        mantissa |= static_cast<int64_t>(1) << XSUM_MANTISSA_BITS;
    }

    // Separate mantissa into two parts, after shifting, and add to (or
    // subtract from) this chunk and the next higher chunk (which always
    // exists since there are three extra ones at the top).

    // Note that low_mantissa will have at most XSUM_LOW_MANTISSA_BITS bits,
    // while high_mantissa will have at most XSUM_MANTISSA_BITS bits, since
    // even though the high mantissa includes the extra implicit 1 bit, it will
    // also be shifted right by at least one bit.
    const std::array<int64_t, 2> splitMantissa{
        static_cast<int64_t>((static_cast<uint64_t>(mantissa) << lowExp) & XSUM_LOW_MANTISSA_MASK),
        mantissa >> (XSUM_LOW_MANTISSA_BITS - lowExp)};

    // Add to, or subtract from, the two affected chunks.
    if (ivalue < 0) {
        m_chunk[highExp] -= splitMantissa[0];
        m_chunk[highExp + 1] -= splitMantissa[1];
    } else {
        m_chunk[highExp] += splitMantissa[0];
        m_chunk[highExp + 1] += splitMantissa[1];
    }
}

/*
Increment m_sizeCount and check positive value every time when value is added.
This is needed to return -0 (negative zero) if applicable.
*/
inline void XsumSmallAccumulator::incrementWhenValueAdded(const double value) {
    m_sizeCount++;
    m_hasPosNumber = m_hasPosNumber || !std::signbit(value);
}

// XsumLargeAccumulator

XsumLargeAccumulator::XsumLargeAccumulator()
    : m_chunk(XSUM_LCHUNKS), m_count(XSUM_LCHUNKS, -1), m_chunksUsed(XSUM_LCHUNKS / 64, 0), m_usedUsed{0}, m_sacc{} {}

/*
    ADD CHUNK FROM A LARGE ACCUMULATOR TO THE SMALL ACCUMULATOR WITHIN IT.
    The large accumulator chunk to add is indexed by ix.  This chunk will
    be cleared to zero and its count reset after it has been added to the
    small accumulator (except no add is done for a new chunk being initialized).
    This procedure should not be called for the special chunks correspnding to
    Inf or NaN, whose counts should always remain at -1.
*/
void XsumLargeAccumulator::addLchunkToSmall(int_fast16_t ix) {
    const int_fast16_t count = m_count[ix];

    // Add to the small accumulator only if the count is not -1, which
    // indicates a chunk that contains nothing yet.
    if (count >= 0) {
        // Propagate carries in the small accumulator if necessary.
        if (m_sacc.m_addsUntilPropagate == 0) {
            m_sacc.carryPropagate();
        }

        // Get the chunk we will add.  Note that this chunk is the integer sum
        // of entire 64-bit floating-point representations, with sign, exponent,
        // and mantissa, but we want only the sum of the mantissas.
        uint64_t chunk = m_chunk[ix];

        // If we added the maximum number of values to 'chunk', the sum of
        // the sign and exponent parts (all the same, equal to the index) will
        // have overflowed out the top, leaving only the sum of the mantissas.
        // If the count of how many more terms we could have summed is greater
        // than zero, we therefore add this count times the index (shifted to
        // the position of the sign and exponent) to get the unwanted bits to
        // overflow out the top.
        if (count > 0) {
            chunk += static_cast<uint64_t>(count * ix) << XSUM_MANTISSA_BITS;
        }

        // Find the exponent for this chunk from the low bits of the index,
        // and split it into low and high parts, for accessing the small
        // accumulator.  Noting that for denormalized numbers where the
        // exponent part is zero, the actual exponent is 1 (before subtracting
        // the bias), not zero.
        int_fast16_t exp = ix & XSUM_EXP_MASK;
        int_fast16_t lowExp = exp & XSUM_LOW_EXP_MASK;
        int_fast16_t highExp = exp >> XSUM_LOW_EXP_BITS;
        if (exp == 0) {
            lowExp = 1;
            highExp = 0;
        }

        // Split the mantissa into three parts, for three consecutive chunks in
        // the small accumulator.  Except for denormalized numbers, add in the sum
        // of all the implicit 1 bits that are above the actual mantissa bits.
        uint64_t lowChunk = (chunk << lowExp) & XSUM_LOW_MANTISSA_MASK;
        uint64_t midChunk = chunk >> (XSUM_LOW_MANTISSA_BITS - lowExp);
        if (exp != 0) { // normalized
            midChunk += static_cast<uint64_t>((1 << XSUM_LCOUNT_BITS) - count)
                        << (XSUM_MANTISSA_BITS - XSUM_LOW_MANTISSA_BITS + lowExp);
        }
        uint64_t highChunk = midChunk >> XSUM_LOW_MANTISSA_BITS;
        midChunk &= XSUM_LOW_MANTISSA_MASK;

        // Add or subtract the three parts of the mantissa from three small
        // accumulator chunks, according to the sign that is part of the index.
        if (ix & (1 << XSUM_EXP_BITS)) {
            m_sacc.m_chunk[highExp] -= lowChunk;
            m_sacc.m_chunk[highExp + 1] -= midChunk;
            m_sacc.m_chunk[highExp + 2] -= highChunk;
        } else {
            m_sacc.m_chunk[highExp] += lowChunk;
            m_sacc.m_chunk[highExp + 1] += midChunk;
            m_sacc.m_chunk[highExp + 2] += highChunk;
        }

        // The above additions/subtractions reduce by one the number we can
        // do before we need to do carry propagation again.
        m_sacc.m_addsUntilPropagate -= 1;
    }

    // We now clear the chunk to zero, and set the count to the number
    // of adds we can do before the mantissa would overflow.  We also
    // set the bit in chunks_used to indicate that this chunk is in use
    // (if that is enabled).
    m_chunk[ix] = 0;
    m_count[ix] = 1 << XSUM_LCOUNT_BITS;
    m_chunksUsed[ix >> 6] |= static_cast<uint64_t>(1) << (ix & 0x3f);
    m_usedUsed |= static_cast<uint64_t>(1) << (ix >> 6);
}

/*
    ADD A CHUNK TO THE LARGE ACCUMULATOR OR PROCESS NAN OR INF.  This routine
    is called when the count for a chunk is negative after decrementing, which
    indicates either inf/nan, or that the chunk has not been initialized, or
    that the chunk needs to be transferred to the small accumulator.
*/
void XsumLargeAccumulator::largeAddValueInfNan(int_fast16_t ix, uint64_t uintv) {
    if ((ix & XSUM_EXP_MASK) == XSUM_EXP_MASK) {
        m_sacc.addInfNan(uintv);
    } else {
        this->addLchunkToSmall(ix);
        m_count[ix] -= 1;
        m_chunk[ix] += uintv;
    }
}

/*
    TRANSFER ALL CHUNKS IN LARGE ACCUMULATOR TO ITS SMALL ACCUMULATOR.
*/
void XsumLargeAccumulator::transferToSmall() {
    size_t p = 0;
    size_t chunksUsedSize = m_chunksUsed.size();

    // Very quickly skip some unused low-order blocks of chunks by looking
    // at the m_usedUsed flags.
    uint64_t uu = m_usedUsed;
    if ((uu & 0xffffffff) == 0) {
        uu >>= 32;
        p += 32;
    }
    if ((uu & 0xffff) == 0) {
        uu >>= 16;
        p += 16;
    }
    if ((uu & 0xff) == 0) {
        p += 8;
    }

    // Loop over remaining blocks of chunks.
    uint64_t u = m_chunksUsed[p];
    do {
        // Loop to quickly find the next non-zero block of used flags,
        // or finish up if we've added all the used blocks to the small accumulator.
        for (;;) {
            u = m_chunksUsed[p];
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == chunksUsedSize) {
                return;
            }
            u = m_chunksUsed[p];
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == chunksUsedSize) {
                return;
            }
            u = m_chunksUsed[p];
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == chunksUsedSize) {
                return;
            }
            u = m_chunksUsed[p];
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == chunksUsedSize) {
                return;
            }
        }

        // Find and process the chunks in this block that are used.  We skip
        // forward based on the m_chunksUsed flags until we're within eight
        // bits of a chunk that is in use.
        int ix = p << 6;
        if ((u & 0xffffffff) == 0) {
            u >>= 32;
            ix += 32;
        }
        if ((u & 0xffff) == 0) {
            u >>= 16;
            ix += 16;
        }
        if ((u & 0xff) == 0) {
            u >>= 8;
            ix += 8;
        }

        do {
            if (m_count[ix] >= 0) {
                this->addLchunkToSmall(ix);
            }
            ix += 1;
            u >>= 1;
        } while (u != 0);
        p += 1;
    } while (p < chunksUsedSize);
}

} // namespace

// XsumSmall

XsumSmall::XsumSmall() : m_sacc{} {}

XsumSmall::XsumSmall(XsumSmallAccumulator sacc)
    : m_sacc{std::move(sacc.m_chunk), sacc.m_addsUntilPropagate, sacc.m_Inf, sacc.m_NaN,
             sacc.m_sizeCount,        sacc.m_hasPosNumber} {}

/*
    ADD A VECTOR OF FLOATING-POINT NUMBERS TO A SMALL ACCUMULATOR.  Mixes
    calls of carryPropagate with calls of add1NoCarry.
*/
void XsumSmall::addv(const std::span<const double> vec) {
    size_t offset = 0;
    size_t n = vec.size();

    while (0 < n) {
        if (m_sacc.m_addsUntilPropagate == 0) {
            m_sacc.carryPropagate();
        }
        size_t m = std::min(static_cast<int>(n), m_sacc.m_addsUntilPropagate);
        for (size_t i = 0; i < m; i++) {
            const double value = vec[offset + i];
            m_sacc.incrementWhenValueAdded(value);
            m_sacc.add1NoCarry(value);
        }
        m_sacc.m_addsUntilPropagate -= m;
        offset += m;
        n -= m;
    }
}

/*
    ADD ONE DOUBLE TO A SMALL ACCUMULATOR.  This is equivalent to, but
    somewhat faster than, calling xsum_small_addv with a vector of one
    value.
*/
void XsumSmall::add1(const double value) {
    m_sacc.incrementWhenValueAdded(value);
    if (m_sacc.m_addsUntilPropagate == 0) {
        m_sacc.carryPropagate();
    }
    m_sacc.add1NoCarry(value);
    m_sacc.m_addsUntilPropagate -= 1;
}

/*
    RETURN THE RESULT OF ROUNDING A SMALL ACCUMULATOR.  The rounding mode
    is to nearest, with ties to even.  The small accumulator may be modified
    by this operation (by carry propagation being done), but the value it
    represents should not change.
*/
double XsumSmall::computeRound() {
    // See if we have a NaN from one of the numbers being a NaN, in
    // which case we return the NaN with largest payload, or an infinite
    // result (+Inf, -Inf, or a NaN if both +Inf and -Inf occurred).
    // Note that we do NOT return NaN if we have both an infinite number
    // and a sum of other numbers that overflows with opposite sign,
    // since there is no real ambiguity regarding the sign in such a case.

    if (m_sacc.m_NaN != 0) {
        return std::bit_cast<double>(m_sacc.m_NaN);
    }

    if (m_sacc.m_Inf != 0) {
        return std::bit_cast<double>(m_sacc.m_Inf);
    }

    if (m_sacc.m_sizeCount == 0) {
        return -0.0;
    }

    // If none of the numbers summed were infinite or NaN, we proceed to
    // propagate carries, as a preliminary to finding the magnitude of
    // the sum.  This also ensures that the sign of the result can be
    // determined from the uppermost non-zero chunk.

    // We also find the index, i, of this uppermost non-zero chunk, as
    // the value returned by carryPropagate, and set ivalue to
    // m_sacc.chunk[i].  Note that ivalue will not be 0 or -1, unless
    // i is 0 (the lowest chunk), in which case it will be handled by
    // the code for denormalized numbers.
    const int i = m_sacc.carryPropagate();
    int64_t ivalue = m_sacc.m_chunk[i];
    int64_t intv = 0;

    // Handle a possible denormalized number, including zero.
    if (i <= 1) {
        // Check for zero value, in which case we can return immediately.
        if (ivalue == 0) {
            return !m_sacc.m_hasPosNumber ? -0.0 : 0.0;
        }

        // Check if it is actually a denormalized number.  It always is if only
        // the lowest chunk is non-zero.  If the highest non-zero chunk is the
        // next-to-lowest, we check the magnitude of the absolute value.
        // Note that the real exponent is 1 (not 0), so we need to shift right
        // by 1 here.
        if (i == 0) {
            intv = 0 <= ivalue ? ivalue : -ivalue;
            intv >>= 1;
            if (ivalue < 0) {
                intv |= XSUM_SIGN_MASK;
            }
            return std::bit_cast<double>(intv);
        } else {
            // Note: Left shift of -ve number is undefined, so do a multiply instead,
            // which is probably optimized to a shift.
            int64_t intv =
                ivalue * (static_cast<int64_t>(1) << (XSUM_LOW_MANTISSA_BITS - 1)) + (m_sacc.m_chunk[0] >> 1);
            if (intv < 0) {
                if (intv > -(static_cast<int64_t>(1) << XSUM_MANTISSA_BITS)) {
                    intv = (-intv) | XSUM_SIGN_MASK;
                    return std::bit_cast<double>(intv);
                }
            } else { // non-negative
                if (static_cast<uint64_t>(intv) < static_cast<uint64_t>(1) << XSUM_MANTISSA_BITS) {
                    return std::bit_cast<double>(intv);
                }
            }
            // otherwise, it's not actually denormalized, so fall through to below
        }
    }

    // Find the location of the uppermost 1 bit in the absolute value of
    // the upper chunk by converting it (as a signed integer) to a
    // floating point value, and looking at the exponent.  Then set
    // 'more' to the number of bits from the lower chunk (and maybe the
    // next lower) that are needed to fill out the mantissa of the
    // result (including the top implicit 1 bit), plus two extra bits to
    // help decide on rounding.  For negative numbers, it may turn out
    // later that we need another bit, because negating a negative value
    // may carry out of the top here, but not carry out of the top once
    // more bits are shifted into the bottom later on.

    const double fltv = static_cast<double>(ivalue); // finds position of topmost 1 bit of |ivalue|
    intv = std::bit_cast<int64_t>(fltv);
    int e = (intv >> XSUM_MANTISSA_BITS) & XSUM_EXP_MASK; // e-bias is in 0..32
    int more = 2 + XSUM_MANTISSA_BITS + XSUM_EXP_BIAS - e;

    // Change 'ivalue' to put in 'more' bits from lower chunks into the bottom.
    // Also set 'j' to the index of the lowest chunk from which these bits came,
    // and 'lower' to the remaining bits of that chunk not now in 'ivalue'.
    // Note that 'lower' initially has at least one bit in it, which we can
    // later move into 'ivalue' if it turns out that one more bit is needed.

    ivalue *= static_cast<int64_t>(1) << more; // multiply, since << of negative undefined
    int j = i - 1;
    int64_t lower = m_sacc.m_chunk[j]; // must exist, since denormalized if i==0
    if (more >= XSUM_LOW_MANTISSA_BITS) {
        more -= XSUM_LOW_MANTISSA_BITS;
        ivalue += lower << more;
        j -= 1;
        lower = j < 0 ? 0 : m_sacc.m_chunk[j];
    }
    ivalue += lower >> (XSUM_LOW_MANTISSA_BITS - more);
    lower &= (static_cast<int64_t>(1) << (XSUM_LOW_MANTISSA_BITS - more)) - 1;

    // Decide on rounding, with separate code for positive and negative values.
    // At this point, 'ivalue' has the signed mantissa bits, plus two extra
    // bits, with 'e' recording the exponent position for these within their
    // top chunk.  For positive 'ivalue', the bits in 'lower' and chunks
    // below 'j' add to the absolute value; for negative 'ivalue' they
    // subtract.
    // After setting 'ivalue' to the tentative unsigned mantissa
    // (shifted left 2), and 'intv' to have the correct sign, this
    // code goes to done_rounding if it finds that just discarding lower
    // order bits is correct, and to round_away_from_zero if instead the
    // magnitude should be increased by one in the lowest mantissa bit.
    bool shouldRoundAwayFromZero = false;
    if (0 <= ivalue) { // number is positive, lower bits are added to magnitude
        intv = 0;      // positive sign

        if ((ivalue & 2) == 0) { // extra bits are 0x
            // TODO: this is not required,
            // but removing the branch would change the logic
            shouldRoundAwayFromZero = false;
        } else if ((ivalue & 1) != 0) { // extra bits are 11
            shouldRoundAwayFromZero = true;
        } else if ((ivalue & 4) != 0) { // low bit is 1 (odd), extra bits are 10
            shouldRoundAwayFromZero = true;
        } else {
            if (lower == 0) { // see if any lower bits are non-zero
                while (j > 0) {
                    j -= 1;
                    if (m_sacc.m_chunk[j] != 0) {
                        lower = 1;
                        break;
                    }
                }
            }
            if (lower != 0) { // low bit 0 (even), extra bits 10, non-zero lower bits
                shouldRoundAwayFromZero = true;
            }
        }
    } else { // number is negative, lower bits are subtracted from magnitude
        // Check for a negative 'ivalue' that when negated doesn't contain a full
        // mantissa's worth of bits, plus one to help rounding.  If so, move one
        // more bit into 'ivalue' from 'lower' (and remove it from 'lower').
        // This happens when the negation of the upper part of 'ivalue' has the
        // form 10000... but the negation of the full 'ivalue' is not 10000...

        if (((-ivalue) & (static_cast<int64_t>(1) << (XSUM_MANTISSA_BITS + 2))) == 0) {
            int pos = (int64_t)1 << (XSUM_LOW_MANTISSA_BITS - 1 - more);
            ivalue *= 2; // note that left shift undefined if ivalue is negative
            if (lower & pos) {
                ivalue += 1;
                lower &= ~pos;
            }
            e -= 1;
        }

        intv = XSUM_SIGN_MASK; // negative sign
        ivalue = -ivalue;      // ivalue now contains the absolute value

        if ((ivalue & 3) == 3) { // extra bits are 11
            shouldRoundAwayFromZero = true;
        }

        if (lower == 0) { // see if any lower bits are non-zero
            while (j > 0) {
                j -= 1;
                if (m_sacc.m_chunk[j] != 0) {
                    lower = 1;
                    break;
                }
            }
        }

        if (lower == 0) { // low bit 1 (odd), extra bits are 10, lower bits are all 0
            shouldRoundAwayFromZero = true;
        }
    }

    if (shouldRoundAwayFromZero) {
        // Round away from zero, then check for carry having propagated out the
        // top, and shift if so.
        ivalue += 4; // add 1 to low-order mantissa bit
        if (ivalue & (static_cast<int64_t>(1) << (XSUM_MANTISSA_BITS + 3))) {
            ivalue >>= 1;
            e += 1;
        }
    }

    // Get rid of the bottom 2 bits that were used to decide on rounding.
    ivalue >>= 2;

    // Adjust to the true exponent, accounting for where this chunk is.
    e += (i << XSUM_LOW_EXP_BITS) - XSUM_EXP_BIAS - XSUM_MANTISSA_BITS;

    // If exponent has overflowed, change to plus or minus Inf and return.
    if (e >= XSUM_EXP_MASK) {
        intv |= static_cast<int64_t>(XSUM_EXP_MASK) << XSUM_MANTISSA_BITS;
        return std::bit_cast<double>(intv);
    }

    // Put exponent and mantissa into intv, which already has the sign,
    // then copy into fltv.

    intv += static_cast<int64_t>(e) << XSUM_MANTISSA_BITS;
    intv += ivalue & XSUM_MANTISSA_MASK; // mask out the implicit 1 bit
    return std::bit_cast<double>(intv);
}

// XsumLarge

XsumLarge::XsumLarge() : m_lacc{} {}

/*
    ADD A VECTOR OF FLOATING-POINT NUMBERS TO A LARGE ACCUMULATOR.
*/
void XsumLarge::addv(const std::span<const double> vec) {
    for (const auto value : vec) {
        // increment
        m_lacc.m_sacc.incrementWhenValueAdded(value);

        // Convert to integer form in uintv
        uint64_t uintv = std::bit_cast<uint64_t>(value);

        // Isolate the upper sign+exponent bits that index the chunk.
        int_fast16_t ix = uintv >> XSUM_MANTISSA_BITS;

        // Find the count for this chunk, and subtract one.
        int_least16_t count = m_lacc.m_count[ix] - 1;

        if (count < 0) {
            // If the decremented count is negative, it's either a special
            // Inf/NaN chunk (in which case count will stay at -1), or one that
            // needs to be transferred to the small accumulator, or one that
            // has never been used before and needs to be initialized.
            m_lacc.largeAddValueInfNan(ix, uintv);
        } else {
            // Store the decremented count of additions allowed before transfer,
            // and add this value to the chunk.
            m_lacc.m_count[ix] = count;
            m_lacc.m_chunk[ix] += uintv;
        }
    }
}

/*
    ADD ONE DOUBLE TO A LARGE ACCUMULATOR.  Just calls xsum_large_addv.
*/
void XsumLarge::add1(double value) {
    addv(std::array<double, 1>{value});
}

/*
    RETURN RESULT OF ROUNDING A LARGE ACCUMULATOR.  Rounding mode is to nearest,
    with ties to even.
    This is done by adding all the chunks in the large accumulator to the
    small accumulator, and then calling its rounding procedure.
*/
double XsumLarge::computeRound() {
    m_lacc.transferToSmall();
    XsumSmall xsumSmall{m_lacc.m_sacc};
    return xsumSmall.computeRound();
}

// XsumAuto

XsumAuto::XsumAuto() : m_acc{XsumSmall{}} {}

XsumAuto::XsumAuto(size_t expectedInputSize)
    : m_acc{(expectedInputSize < XSUM_THRESHOLD) ? XsumVariant{XsumSmall{}} : XsumVariant{XsumLarge{}}} {}

XsumAuto::XsumAuto(XsumKind kind)
    : m_acc{(kind == XsumKind::XsumSmall) ? XsumVariant{XsumSmall{}} : XsumVariant{XsumLarge{}}} {}

void XsumAuto::addv(const std::span<const double> vec) {
    // std::get_if is faster than std::visit and std::get_if
    switch (this->m_acc.index()) {
        case 0:
            std::get<0>(this->m_acc).addv(vec);
            break;
        case 1:
            std::get<1>(this->m_acc).addv(vec);
            break;
        default:
            break;
    }
}

void XsumAuto::add1(double value) {
    // std::get_if is faster than std::visit and std::get_if
    switch (this->m_acc.index()) {
        case 0:
            std::get<0>(this->m_acc).add1(value);
            break;
        case 1:
            std::get<1>(this->m_acc).add1(value);
            break;
        default:
            break;
    }
}

double XsumAuto::computeRound() {
    // std::get is faster than std::visit and std::get_if
    switch (this->m_acc.index()) {
        case 0:
            return std::get<0>(this->m_acc).computeRound();
        case 1:
            return std::get<1>(this->m_acc).computeRound();
        default:
            return -0;
    }
}

} // namespace XSUM
