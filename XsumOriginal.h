// The code is originally from https://gitlab.com/radfordneal/xsum/-/blob/master/xsum.c?ref_type=heads
// to compare xsum benchmakrs with C based code and C++ based code
// Only enabled USE_USED_LARGE flag (see the original code on the above URL), and removed unused code

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

// To be conpatible with C++
#ifdef __cplusplus
#define RESTRICT
#elif defined(_MSC_VER)
#define RESTRICT __restrict
#else
#define RESTRICT __restrict__
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* CONSTANTS DEFINING THE FLOATING POINT FORMAT. */

typedef double xsum_flt; /* C floating point type sums are done for */

typedef int64_t xsum_int;         /* Signed integer type for a fp value */
typedef uint64_t xsum_uint;       /* Unsigned integer type for a fp value */
typedef int_fast16_t xsum_expint; /* Integer type for holding an exponent */

#define XSUM_MANTISSA_BITS 52 /* Bits in fp mantissa, excludes implict 1 */
#define XSUM_EXP_BITS 11      /* Bits in fp exponent */

#define XSUM_MANTISSA_MASK (((xsum_int)1 << XSUM_MANTISSA_BITS) - 1) /* Mask for mantissa bits */

#define XSUM_EXP_MASK ((1 << XSUM_EXP_BITS) - 1) /* Mask for exponent */

#define XSUM_EXP_BIAS ((1 << (XSUM_EXP_BITS - 1)) - 1) /* Bias added to signed exponent */

#define XSUM_SIGN_BIT (XSUM_MANTISSA_BITS + XSUM_EXP_BITS) /* Position of sign bit */

#define XSUM_SIGN_MASK ((xsum_uint)1 << XSUM_SIGN_BIT) /* Mask for sign bit */

/* CONSTANTS DEFINING THE SMALL ACCUMULATOR FORMAT. */

#define XSUM_SCHUNK_BITS 64  /* Bits in chunk of the small accumulator */
typedef int64_t xsum_schunk; /* Integer type of small accumulator chunk */

#define XSUM_LOW_EXP_BITS 5 /* # of low bits of exponent, in one chunk */

#define XSUM_LOW_EXP_MASK ((1 << XSUM_LOW_EXP_BITS) - 1) /* Mask for low-order exponent bits */

#define XSUM_HIGH_EXP_BITS (XSUM_EXP_BITS - XSUM_LOW_EXP_BITS) /* # of high exponent bits for index */

#define XSUM_SCHUNKS ((1 << XSUM_HIGH_EXP_BITS) + 3) /* # of chunks in small accumulator */

#define XSUM_LOW_MANTISSA_BITS (1 << XSUM_LOW_EXP_BITS) /* Bits in low part of mantissa */

#define XSUM_LOW_MANTISSA_MASK (((xsum_int)1 << XSUM_LOW_MANTISSA_BITS) - 1) /* Mask for low bits */

#define XSUM_SMALL_CARRY_BITS ((XSUM_SCHUNK_BITS - 1) - XSUM_MANTISSA_BITS) /* Bits sums can carry into */

#define XSUM_SMALL_CARRY_TERMS ((1 << XSUM_SMALL_CARRY_BITS) - 1) /* # terms can add before need prop. */

typedef struct {
    xsum_schunk chunk[XSUM_SCHUNKS]; /* Chunks making up small accumulator */
    xsum_int Inf;                    /* If non-zero, +Inf, -Inf, or NaN */
    xsum_int NaN;                    /* If non-zero, a NaN value with payload */
    int adds_until_propagate;        /* Number of remaining adds before carry */
} xsum_small_accumulator;            /*     propagation must be done again    */

/* CONSTANTS DEFINING THE LARGE ACCUMULATOR FORMAT. */

typedef uint64_t xsum_lchunk; /* Integer type of large accumulator chunk,
                                 must be EXACTLY 64 bits in size */

#define XSUM_LCOUNT_BITS (64 - XSUM_MANTISSA_BITS) /* # of bits in count */
typedef int_least16_t xsum_lcount;                 /* Signed int type of counts for large acc.*/

#define XSUM_LCHUNKS (1 << (XSUM_EXP_BITS + 1)) /* # of chunks in large accumulator */

typedef uint64_t xsum_used; /* Unsigned type for holding used flags */

typedef struct {
    xsum_lchunk chunk[XSUM_LCHUNKS];          /* Chunks making up large accumulator */
    xsum_lcount count[XSUM_LCHUNKS];          /* Counts of # adds remaining for chunks,
                                                   or -1 if not used yet or special. */
    xsum_used chunks_used[XSUM_LCHUNKS / 64]; /* Bits indicate chunks in use */
    xsum_used used_used;                      /* Bits indicate chunk_used entries not 0 */
    xsum_small_accumulator sacc;              /* The small accumulator to condense into */
} xsum_large_accumulator;

/* TYPE FOR LENGTHS OF ARRAYS.  Must be a signed integer type.  Set to
   ptrdiff_t here on the assumption that this will be big enough, but
   not unnecessarily big, which seems to be true. */

typedef ptrdiff_t xsum_length;

/* FUNCTIONS FOR EXACT SUMMATION, WITH POSSIBLE DIVISION BY AN INTEGER. */

void xsum_small_init(xsum_small_accumulator *RESTRICT);
void xsum_small_add1(xsum_small_accumulator *RESTRICT, xsum_flt);
void xsum_small_addv(xsum_small_accumulator *RESTRICT, const xsum_flt *RESTRICT, xsum_length);
xsum_flt xsum_small_round(xsum_small_accumulator *RESTRICT);

void xsum_large_init(xsum_large_accumulator *RESTRICT);
void xsum_large_add1(xsum_large_accumulator *RESTRICT, xsum_flt);
void xsum_large_addv(xsum_large_accumulator *RESTRICT, const xsum_flt *RESTRICT, xsum_length);
xsum_flt xsum_large_round(xsum_large_accumulator *RESTRICT);

void xsum_large_to_small_accumulator(xsum_small_accumulator *RESTRICT, xsum_large_accumulator *RESTRICT);
void xsum_small_to_large_accumulator(xsum_large_accumulator *RESTRICT, xsum_small_accumulator *RESTRICT);

#ifdef __cplusplus
}
#endif
