// The code is originally from https://gitlab.com/radfordneal/xsum/-/blob/master/xsum.c?ref_type=heads
// to compare xsum benchmakrs with C based code and C++ based code
// Only enabled USE_USED_LARGE flag (see the original code on the above URL), and removed unused code

#include "XsumOriginal.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

/* COPY A 64-BIT QUANTITY - DOUBLE TO 64-BIT INT OR VICE VERSA.  The
   arguments are destination and source variables (not values). */

#define COPY64(dst, src) memcpy(&(dst), &(src), sizeof(double))

/* SET UP INLINE / NOINLINE MACROS. */

#if __GNUC__
#define INLINE inline __attribute__((always_inline))
#define NOINLINE __attribute__((noinline))
#else
#define INLINE inline
#define NOINLINE
#endif

static NOINLINE void xsum_small_add_inf_nan(xsum_small_accumulator *restrict sacc, xsum_int ivalue) {
    xsum_int mantissa;
    double fltv;

    mantissa = ivalue & XSUM_MANTISSA_MASK;

    if (mantissa == 0) /* Inf */
    {
        if (sacc->Inf == 0) { /* no previous Inf */
            sacc->Inf = ivalue;
        } else if (sacc->Inf != ivalue) { /* previous Inf was opposite sign */
            COPY64(fltv, ivalue);
            fltv = fltv - fltv; /* result will be a NaN */
            COPY64(sacc->Inf, fltv);
        }
    } else /* NaN */
    {      /* Choose the NaN with the bigger payload and clear its sign.  Using <=
              ensures that we will choose the first NaN over the previous zero. */
        if ((sacc->NaN & XSUM_MANTISSA_MASK) <= mantissa) {
            sacc->NaN = ivalue & ~XSUM_SIGN_MASK;
        }
    }
}

/* PROPAGATE CARRIES TO NEXT CHUNK IN A SMALL ACCUMULATOR.  Needs to
   be called often enough that accumulated carries don't overflow out
   the top, as indicated by sacc->adds_until_propagate.  Returns the
   index of the uppermost non-zero chunk (0 if number is zero).

   After carry propagation, the uppermost non-zero chunk will indicate
   the sign of the number, and will not be -1 (all 1s).  It will be in
   the range -2^XSUM_LOW_MANTISSA_BITS to 2^XSUM_LOW_MANTISSA_BITS - 1.
   Lower chunks will be non-negative, and in the range from 0 up to
   2^XSUM_LOW_MANTISSA_BITS - 1. */

static NOINLINE int xsum_carry_propagate(xsum_small_accumulator *restrict sacc) {
    int i, u, uix;

    /* Set u to the index of the uppermost non-zero (for now) chunk, or
       return with value 0 if there is none. */

    for (u = XSUM_SCHUNKS - 1; sacc->chunk[u] == 0; u--) {
        if (u == 0) {
            uix = 0;
            goto done;
        }
    }

    /* At this point, sacc->chunk[u] must be non-zero */

    /* Carry propagate, starting at the low-order chunks.  Note that the
       loop limit of u may be increased inside the loop. */

    i = 0;    /* set to the index of the next non-zero chunck, from bottom */
    uix = -1; /* indicates that a non-zero chunk has not been found yet */

    do {
        xsum_schunk c;     /* Set to the chunk at index i (next non-zero one) */
        xsum_schunk clow;  /* Low-order bits of c */
        xsum_schunk chigh; /* High-order bits of c */

        /* Find the next non-zero chunk, setting i to its index, or break out
           of loop if there is none.  Note that the chunk at index u is not
           necessarily non-zero - it was initially, but u or the chunk at u
           may have changed. */

        do {
            c = sacc->chunk[i];
            if (c != 0) {
                goto nonzero;
            }
            i += 1;
        } while (i <= u);

        break;

        /* Propagate possible carry from this chunk to next chunk up. */

    nonzero:
        chigh = c >> XSUM_LOW_MANTISSA_BITS;
        if (chigh == 0) {
            uix = i;
            i += 1;
            continue; /* no need to change this chunk */
        }

        if (u == i) {
            if (chigh == -1) {
                uix = i;
                break; /* don't propagate -1 into the region of all zeros above */
            }
            u = i + 1; /* we will change chunk[u+1], so we'll need to look at it */
        }

        clow = c & XSUM_LOW_MANTISSA_MASK;
        if (clow != 0) {
            uix = i;
        }

        /* We now change chunk[i] and add to chunk[i+1]. Note that i+1 should be
           in range (no bigger than XSUM_CHUNKS-1) if summing memory, since
           the number of chunks is big enough to hold any sum, and we do not
           store redundant chunks with values 0 or -1 above previously non-zero
           chunks.  But other add operations might cause overflow, in which
           case we produce a NaN with all 1s as payload.  (We can't reliably produce
           an Inf of the right sign.) */

        sacc->chunk[i] = clow;
        if (i + 1 >= XSUM_SCHUNKS) {
            xsum_small_add_inf_nan(sacc, ((xsum_int)XSUM_EXP_MASK << XSUM_MANTISSA_BITS) | XSUM_MANTISSA_MASK);
            u = i;
        } else {
            sacc->chunk[i + 1] += chigh; /* note: this could make this chunk be zero */
        }

        i += 1;

    } while (i <= u);

    /* Check again for the number being zero, since carry propagation might
       have created zero from something that initially looked non-zero. */

    if (uix < 0) {
        uix = 0;
        goto done;
    }

    /* While the uppermost chunk is negative, with value -1, combine it with
       the chunk below (if there is one) to produce the same number but with
       one fewer non-zero chunks. */

    while (sacc->chunk[uix] == -1 &&
           uix > 0) { /* Left shift of a negative number is undefined according to the standard,
                         so do a multiply - it's all presumably constant-folded by the compiler.*/
        sacc->chunk[uix - 1] += ((xsum_schunk)-1) * (((xsum_schunk)1) << XSUM_LOW_MANTISSA_BITS);
        sacc->chunk[uix] = 0;
        uix -= 1;
    }

    /* We can now add one less than the total allowed terms before the
       next carry propagate. */

done:
    sacc->adds_until_propagate = XSUM_SMALL_CARRY_TERMS - 1;

    /* Return index of uppermost non-zero chunk. */

    return uix;
}

/* INITIALIZE LARGE ACCUMULATOR CHUNKS.  Sets all counts to -1. */

static void xsum_large_init_chunks(xsum_large_accumulator *restrict lacc) {
    xsum_lcount *p;
    int n;
    p = lacc->count;
    n = XSUM_LCHUNKS;
    do {
        *p++ = -1;
        n -= 1;
    } while (n > 0);

    xsum_lchunk *pp;
    pp = lacc->chunks_used;
    n = XSUM_LCHUNKS / 64;
    do {
        *pp++ = 0;
        n -= 1;
    } while (n > 0);
    lacc->used_used = 0;
}

/* ADD CHUNK FROM A LARGE ACCUMULATOR TO THE SMALL ACCUMULATOR WITHIN IT.
   The large accumulator chunk to add is indexed by ix.  This chunk will
   be cleared to zero and its count reset after it has been added to the
   small accumulator (except no add is done for a new chunk being initialized).
   This procedure should not be called for the special chunks correspnding to
   Inf or NaN, whose counts should always remain at -1. */

static void xsum_add_lchunk_to_small(xsum_large_accumulator *restrict lacc, xsum_expint ix) {
    xsum_expint exp, low_exp, high_exp;
    xsum_uint low_chunk, mid_chunk, high_chunk;
    xsum_lchunk chunk;

    const xsum_expint count = lacc->count[ix];

    /* Add to the small accumulator only if the count is not -1, which
       indicates a chunk that contains nothing yet. */

    if (count >= 0) {
        /* Propagate carries in the small accumulator if necessary. */

        if (lacc->sacc.adds_until_propagate == 0) {
            (void)xsum_carry_propagate(&lacc->sacc);
        }

        /* Get the chunk we will add.  Note that this chunk is the integer sum
           of entire 64-bit floating-point representations, with sign, exponent,
           and mantissa, but we want only the sum of the mantissas. */

        chunk = lacc->chunk[ix];

        /* If we added the maximum number of values to 'chunk', the sum of
           the sign and exponent parts (all the same, equal to the index) will
           have overflowed out the top, leaving only the sum of the mantissas.
           If the count of how many more terms we could have summed is greater
           than zero, we therefore add this count times the index (shifted to
           the position of the sign and exponent) to get the unwanted bits to
           overflow out the top. */

        if (count > 0) {
            chunk += (xsum_lchunk)(count * ix) << XSUM_MANTISSA_BITS;
        }

        /* Find the exponent for this chunk from the low bits of the index,
           and split it into low and high parts, for accessing the small
           accumulator.  Noting that for denormalized numbers where the
           exponent part is zero, the actual exponent is 1 (before subtracting
           the bias), not zero. */

        exp = ix & XSUM_EXP_MASK;
        if (exp == 0) {
            low_exp = 1;
            high_exp = 0;
        } else {
            low_exp = exp & XSUM_LOW_EXP_MASK;
            high_exp = exp >> XSUM_LOW_EXP_BITS;
        }

        /* Split the mantissa into three parts, for three consecutive chunks in
           the small accumulator.  Except for denormalized numbers, add in the sum
           of all the implicit 1 bits that are above the actual mantissa bits. */

        low_chunk = (chunk << low_exp) & XSUM_LOW_MANTISSA_MASK;
        mid_chunk = chunk >> (XSUM_LOW_MANTISSA_BITS - low_exp);
        if (exp != 0) /* normalized */
        {
            mid_chunk += (xsum_lchunk)((1 << XSUM_LCOUNT_BITS) - count)
                         << (XSUM_MANTISSA_BITS - XSUM_LOW_MANTISSA_BITS + low_exp);
        }
        high_chunk = mid_chunk >> XSUM_LOW_MANTISSA_BITS;
        mid_chunk &= XSUM_LOW_MANTISSA_MASK;

        /* Add or subtract the three parts of the mantissa from three small
           accumulator chunks, according to the sign that is part of the index. */

        if (ix & (1 << XSUM_EXP_BITS)) {
            lacc->sacc.chunk[high_exp] -= low_chunk;
            lacc->sacc.chunk[high_exp + 1] -= mid_chunk;
            lacc->sacc.chunk[high_exp + 2] -= high_chunk;
        } else {
            lacc->sacc.chunk[high_exp] += low_chunk;
            lacc->sacc.chunk[high_exp + 1] += mid_chunk;
            lacc->sacc.chunk[high_exp + 2] += high_chunk;
        }

        /* The above additions/subtractions reduce by one the number we can
           do before we need to do carry propagation again. */

        lacc->sacc.adds_until_propagate -= 1;
    }

    /* We now clear the chunk to zero, and set the count to the number
       of adds we can do before the mantissa would overflow.  We also
       set the bit in chunks_used to indicate that this chunk is in use
       (if that is enabled). */

    lacc->chunk[ix] = 0;
    lacc->count[ix] = 1 << XSUM_LCOUNT_BITS;

    lacc->chunks_used[ix >> 6] |= (xsum_used)1 << (ix & 0x3f);
    lacc->used_used |= (xsum_used)1 << (ix >> 6);
}

/* ADD A CHUNK TO THE LARGE ACCUMULATOR OR PROCESS NAN OR INF.  This routine
   is called when the count for a chunk is negative after decrementing, which
   indicates either inf/nan, or that the chunk has not been initialized, or
   that the chunk needs to be transferred to the small accumulator. */

#if INLINE_LARGE
INLINE
#endif
static void xsum_large_add_value_inf_nan(xsum_large_accumulator *restrict lacc, xsum_expint ix, xsum_lchunk uintv) {
    if ((ix & XSUM_EXP_MASK) == XSUM_EXP_MASK) {
        xsum_small_add_inf_nan(&lacc->sacc, uintv);
    } else {
        xsum_add_lchunk_to_small(lacc, ix);
        lacc->count[ix] -= 1;
        lacc->chunk[ix] += uintv;
    }
}

/* TRANSFER ALL CHUNKS IN LARGE ACCUMULATOR TO ITS SMALL ACCUMULATOR. */

static void xsum_large_transfer_to_small(xsum_large_accumulator *restrict lacc) {
    xsum_used *p, *e;
    xsum_used u, uu;
    int ix;

    p = lacc->chunks_used;
    e = p + XSUM_LCHUNKS / 64;

    /* Very quickly skip some unused low-order blocks of chunks by looking
        at the used_used flags. */

    uu = lacc->used_used;
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

    /* Loop over remaining blocks of chunks. */

    do {
        /* Loop to quickly find the next non-zero block of used flags, or finish
            up if we've added all the used blocks to the small accumulator. */

        for (;;) {
            u = *p;
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == e) {
                return;
            }
            u = *p;
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == e) {
                return;
            }
            u = *p;
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == e) {
                return;
            }
            u = *p;
            if (u != 0) {
                break;
            }
            p += 1;
            if (p == e) {
                return;
            }
        }

        /* Find and process the chunks in this block that are used.  We skip
            forward based on the chunks_used flags until we're within eight
            bits of a chunk that is in use. */

        ix = (p - lacc->chunks_used) << 6;
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
            if (lacc->count[ix] >= 0) {
                xsum_add_lchunk_to_small(lacc, ix);
            }
            ix += 1;
            u >>= 1;
        } while (u != 0);

        p += 1;

    } while (p != e);
}

/* ------------------------ EXTERNAL ROUTINES ------------------------------- */

/* INITIALIZE A SMALL ACCUMULATOR TO ZERO. */

void xsum_small_init(xsum_small_accumulator *restrict sacc) {
    sacc->adds_until_propagate = XSUM_SMALL_CARRY_TERMS;
    sacc->Inf = sacc->NaN = 0;
    xsum_schunk *p;
    int n;
    p = sacc->chunk;
    n = XSUM_SCHUNKS;
    do {
        *p++ = 0;
        n -= 1;
    } while (n > 0);
}

/* ADD ONE NUMBER TO A SMALL ACCUMULATOR ASSUMING NO CARRY PROPAGATION REQ'D.
   This function is declared INLINE regardless of the setting of INLINE_SMALL
   and for good performance it must be inlined by the compiler (otherwise the
   procedure call overhead will result in substantial inefficiency). */

static INLINE void xsum_add1_no_carry(xsum_small_accumulator *restrict sacc, xsum_flt value) {
    xsum_int ivalue;
    xsum_int mantissa;
    xsum_expint exp, low_exp, high_exp;
    xsum_schunk *chunk_ptr;

    /* Extract exponent and mantissa.  Split exponent into high and low parts. */

    COPY64(ivalue, value);

    exp = (ivalue >> XSUM_MANTISSA_BITS) & XSUM_EXP_MASK;
    mantissa = ivalue & XSUM_MANTISSA_MASK;
    high_exp = exp >> XSUM_LOW_EXP_BITS;
    low_exp = exp & XSUM_LOW_EXP_MASK;

    /* Categorize number as normal, denormalized, or Inf/NaN according to
       the value of the exponent field. */

    if (exp == 0) /* zero or denormalized */
    {             /* If it's a zero (positive or negative), we do nothing. */
        if (mantissa == 0) {
            return;
        }
        /* Denormalized mantissa has no implicit 1, but exponent is 1 not 0. */
        exp = low_exp = 1;
    } else if (exp == XSUM_EXP_MASK) /* Inf or NaN */
    {                                /* Just update flags in accumulator structure. */
        xsum_small_add_inf_nan(sacc, ivalue);
        return;
    } else /* normalized */
    {      /* OR in implicit 1 bit at top of mantissa */
        mantissa |= (xsum_int)1 << XSUM_MANTISSA_BITS;
    }

    /* Use high part of exponent as index of chunk, and low part of
       exponent to give position within chunk.  Fetch the two chunks
       that will be modified. */

    chunk_ptr = sacc->chunk + high_exp;

    /* Separate mantissa into two parts, after shifting, and add to (or
       subtract from) this chunk and the next higher chunk (which always
       exists since there are three extra ones at the top).

       Note that low_mantissa will have at most XSUM_LOW_MANTISSA_BITS bits,
       while high_mantissa will have at most XSUM_MANTISSA_BITS bits, since
       even though the high mantissa includes the extra implicit 1 bit, it will
       also be shifted right by at least one bit. */

    xsum_int split_mantissa[2];
    split_mantissa[0] = ((xsum_uint)mantissa << low_exp) & XSUM_LOW_MANTISSA_MASK;
    split_mantissa[1] = mantissa >> (XSUM_LOW_MANTISSA_BITS - low_exp);

    /* Add to, or subtract from, the two affected chunks. */

    if (ivalue < 0) {
        chunk_ptr[0] -= split_mantissa[0];
        chunk_ptr[1] -= split_mantissa[1];
    } else {
        chunk_ptr[0] += split_mantissa[0];
        chunk_ptr[1] += split_mantissa[1];
    }
}

/* ADD ONE DOUBLE TO A SMALL ACCUMULATOR.  This is equivalent to, but
   somewhat faster than, calling xsum_small_addv with a vector of one
   value. */

void xsum_small_add1(xsum_small_accumulator *restrict sacc, xsum_flt value) {
    if (sacc->adds_until_propagate == 0) {
        (void)xsum_carry_propagate(sacc);
    }

    xsum_add1_no_carry(sacc, value);

    sacc->adds_until_propagate -= 1;
}

/* ADD A VECTOR OF FLOATING-POINT NUMBERS TO A SMALL ACCUMULATOR.  Mixes
   calls of xsum_carry_propagate with calls of xsum_add1_no_carry. */

void xsum_small_addv(xsum_small_accumulator *restrict sacc, const xsum_flt *restrict vec, xsum_length n) {
    xsum_length m, i;

    while (n > 0) {
        if (sacc->adds_until_propagate == 0) {
            (void)xsum_carry_propagate(sacc);
        }
        m = n <= sacc->adds_until_propagate ? n : sacc->adds_until_propagate;
        for (i = 0; i < m; i++) {
            xsum_add1_no_carry(sacc, vec[i]);
        }
        sacc->adds_until_propagate -= m;
        vec += m;
        n -= m;
    }
}

/* RETURN THE RESULT OF ROUNDING A SMALL ACCUMULATOR.  The rounding mode
   is to nearest, with ties to even.  The small accumulator may be modified
   by this operation (by carry propagation being done), but the value it
   represents should not change. */

xsum_flt xsum_small_round(xsum_small_accumulator *restrict sacc) {
    xsum_int ivalue;
    xsum_schunk lower;
    int i, j, e, more;
    xsum_int intv;
    double fltv;

    /* See if we have a NaN from one of the numbers being a NaN, in
       which case we return the NaN with largest payload, or an infinite
       result (+Inf, -Inf, or a NaN if both +Inf and -Inf occurred).
       Note that we do NOT return NaN if we have both an infinite number
       and a sum of other numbers that overflows with opposite sign,
       since there is no real ambiguity regarding the sign in such a case. */

    if (sacc->NaN != 0) {
        COPY64(fltv, sacc->NaN);
        return fltv;
    }

    if (sacc->Inf != 0) {
        COPY64(fltv, sacc->Inf);
        return fltv;
    }

    /* If none of the numbers summed were infinite or NaN, we proceed to
       propagate carries, as a preliminary to finding the magnitude of
       the sum.  This also ensures that the sign of the result can be
       determined from the uppermost non-zero chunk.

       We also find the index, i, of this uppermost non-zero chunk, as
       the value returned by xsum_carry_propagate, and set ivalue to
       sacc->chunk[i].  Note that ivalue will not be 0 or -1, unless
       i is 0 (the lowest chunk), in which case it will be handled by
       the code for denormalized numbers. */

    i = xsum_carry_propagate(sacc);

    ivalue = sacc->chunk[i];

    /* Handle a possible denormalized number, including zero. */

    if (i <= 1) {
        /* Check for zero value, in which case we can return immediately. */

        if (ivalue == 0) {
            return 0.0;
        }

        /* Check if it is actually a denormalized number.  It always is if only
           the lowest chunk is non-zero.  If the highest non-zero chunk is the
           next-to-lowest, we check the magnitude of the absolute value.
           Note that the real exponent is 1 (not 0), so we need to shift right
           by 1 here. */

        if (i == 0) {
            intv = ivalue >= 0 ? ivalue : -ivalue;
            intv >>= 1;
            if (ivalue < 0) {
                intv |= XSUM_SIGN_MASK;
            }

            COPY64(fltv, intv);
            return fltv;
        } else { /* Note: Left shift of -ve number is undefined, so do a multiply instead,
                          which is probably optimized to a shift. */
            intv = ivalue * ((xsum_int)1 << (XSUM_LOW_MANTISSA_BITS - 1)) + (sacc->chunk[0] >> 1);
            if (intv < 0) {
                if (intv > -((xsum_int)1 << XSUM_MANTISSA_BITS)) {
                    intv = (-intv) | XSUM_SIGN_MASK;

                    COPY64(fltv, intv);
                    return fltv;
                }
            } else /* non-negative */
            {
                if ((xsum_uint)intv < (xsum_uint)1 << XSUM_MANTISSA_BITS) {
                    COPY64(fltv, intv);
                    return fltv;
                }
            }
            /* otherwise, it's not actually denormalized, so fall through to below */
        }
    }

    /* Find the location of the uppermost 1 bit in the absolute value of
       the upper chunk by converting it (as a signed integer) to a
       floating point value, and looking at the exponent.  Then set
       'more' to the number of bits from the lower chunk (and maybe the
       next lower) that are needed to fill out the mantissa of the
       result (including the top implicit 1 bit), plus two extra bits to
       help decide on rounding.  For negative numbers, it may turn out
       later that we need another bit, because negating a negative value
       may carry out of the top here, but not carry out of the top once
       more bits are shifted into the bottom later on. */

    fltv = (xsum_flt)ivalue; /* finds position of topmost 1 bit of |ivalue| */
    COPY64(intv, fltv);
    e = (intv >> XSUM_MANTISSA_BITS) & XSUM_EXP_MASK; /* e-bias is in 0..32 */
    more = 2 + XSUM_MANTISSA_BITS + XSUM_EXP_BIAS - e;

    /* Change 'ivalue' to put in 'more' bits from lower chunks into the bottom.
       Also set 'j' to the index of the lowest chunk from which these bits came,
       and 'lower' to the remaining bits of that chunk not now in 'ivalue'.
       Note that 'lower' initially has at least one bit in it, which we can
       later move into 'ivalue' if it turns out that one more bit is needed. */

    ivalue *= (xsum_int)1 << more; /* multiply, since << of negative undefined */

    j = i - 1;
    lower = sacc->chunk[j]; /* must exist, since denormalized if i==0 */
    if (more >= XSUM_LOW_MANTISSA_BITS) {
        more -= XSUM_LOW_MANTISSA_BITS;
        ivalue += lower << more;

        j -= 1;
        lower = j < 0 ? 0 : sacc->chunk[j];
    }
    ivalue += lower >> (XSUM_LOW_MANTISSA_BITS - more);
    lower &= ((xsum_schunk)1 << (XSUM_LOW_MANTISSA_BITS - more)) - 1;

    /* Decide on rounding, with separate code for positive and negative values.

       At this point, 'ivalue' has the signed mantissa bits, plus two extra
       bits, with 'e' recording the exponent position for these within their
       top chunk.  For positive 'ivalue', the bits in 'lower' and chunks
       below 'j' add to the absolute value; for negative 'ivalue' they
       subtract.

       After setting 'ivalue' to the tentative unsigned mantissa
       (shifted left 2), and 'intv' to have the correct sign, this
       code goes to done_rounding if it finds that just discarding lower
       order bits is correct, and to round_away_from_zero if instead the
       magnitude should be increased by one in the lowest mantissa bit. */

    if (ivalue >= 0) /* number is positive, lower bits are added to magnitude */
    {
        intv = 0; /* positive sign */

        if ((ivalue & 2) == 0) /* extra bits are 0x */
        {
            goto done_rounding;
        }

        if ((ivalue & 1) != 0) /* extra bits are 11 */
        {
            goto round_away_from_zero;
        }

        if ((ivalue & 4) != 0) /* low bit is 1 (odd), extra bits are 10 */
        {
            goto round_away_from_zero;
        }

        if (lower == 0) /* see if any lower bits are non-zero */
        {
            while (j > 0) {
                j -= 1;
                if (sacc->chunk[j] != 0) {
                    lower = 1;
                    break;
                }
            }
        }

        if (lower != 0) /* low bit 0 (even), extra bits 10, non-zero lower bits */
        {
            goto round_away_from_zero;
        } else /* low bit 0 (even), extra bits 10, all lower bits 0 */
        {
            goto done_rounding;
        }
    }

    else /* number is negative, lower bits are subtracted from magnitude */
    {
        /* Check for a negative 'ivalue' that when negated doesn't contain a full
           mantissa's worth of bits, plus one to help rounding.  If so, move one
           more bit into 'ivalue' from 'lower' (and remove it from 'lower').
           This happens when the negation of the upper part of 'ivalue' has the
           form 10000... but the negation of the full 'ivalue' is not 10000... */

        if (((-ivalue) & ((xsum_int)1 << (XSUM_MANTISSA_BITS + 2))) == 0) {
            int pos = (xsum_schunk)1 << (XSUM_LOW_MANTISSA_BITS - 1 - more);
            ivalue *= 2; /* note that left shift undefined if ivalue is negative */
            if (lower & pos) {
                ivalue += 1;
                lower &= ~pos;
            }
            e -= 1;
        }

        intv = XSUM_SIGN_MASK; /* negative sign */
        ivalue = -ivalue;      /* ivalue now contains the absolute value */

        if ((ivalue & 3) == 3) /* extra bits are 11 */
        {
            goto round_away_from_zero;
        }

        if ((ivalue & 3) <= 1) /* extra bits are 00 or 01 */
        {
            goto done_rounding;
        }

        if ((ivalue & 4) == 0) /* low bit is 0 (even), extra bits are 10 */
        {
            goto done_rounding;
        }

        if (lower == 0) /* see if any lower bits are non-zero */
        {
            while (j > 0) {
                j -= 1;
                if (sacc->chunk[j] != 0) {
                    lower = 1;
                    break;
                }
            }
        }

        if (lower != 0) /* low bit 1 (odd), extra bits 10, non-zero lower bits */
        {
            goto done_rounding;
        } else /* low bit 1 (odd), extra bits are 10, lower bits are all 0 */
        {
            goto round_away_from_zero;
        }
    }

round_away_from_zero:

    /* Round away from zero, then check for carry having propagated out the
       top, and shift if so. */

    ivalue += 4; /* add 1 to low-order mantissa bit */
    if (ivalue & ((xsum_int)1 << (XSUM_MANTISSA_BITS + 3))) {
        ivalue >>= 1;
        e += 1;
    }

done_rounding:;

    /* Get rid of the bottom 2 bits that were used to decide on rounding. */

    ivalue >>= 2;

    /* Adjust to the true exponent, accounting for where this chunk is. */

    e += (i << XSUM_LOW_EXP_BITS) - XSUM_EXP_BIAS - XSUM_MANTISSA_BITS;

    /* If exponent has overflowed, change to plus or minus Inf and return. */

    if (e >= XSUM_EXP_MASK) {
        intv |= (xsum_int)XSUM_EXP_MASK << XSUM_MANTISSA_BITS;
        COPY64(fltv, intv);
        return fltv;
    }

    /* Put exponent and mantissa into intv, which already has the sign,
       then copy into fltv. */

    intv += (xsum_int)e << XSUM_MANTISSA_BITS;
    intv += ivalue & XSUM_MANTISSA_MASK; /* mask out the implicit 1 bit */
    COPY64(fltv, intv);

    return fltv;
}

/* INITIALIZE A LARGE ACCUMULATOR TO ZERO. */

void xsum_large_init(xsum_large_accumulator *restrict lacc) {
    xsum_large_init_chunks(lacc);
    xsum_small_init(&lacc->sacc);
}

/* ADD A VECTOR OF FLOATING-POINT NUMBERS TO A LARGE ACCUMULATOR. */

void xsum_large_addv(xsum_large_accumulator *restrict lacc, const xsum_flt *restrict vec, xsum_length n) {
    if (n == 0) {
        return;
    }

    xsum_lcount count;
    xsum_expint ix;
    xsum_uint uintv;

    do {
        /* Fetch the next number, and convert to integer form in uintv. */

        COPY64(uintv, *vec);
        vec += 1;

        /* Isolate the upper sign+exponent bits that index the chunk. */

        ix = uintv >> XSUM_MANTISSA_BITS;

        /* Find the count for this chunk, and subtract one. */

        count = lacc->count[ix] - 1;

        if (count < 0) {
            /* If the decremented count is negative, it's either a special
                Inf/NaN chunk (in which case count will stay at -1), or one that
                needs to be transferred to the small accumulator, or one that
                has never been used before and needs to be initialized. */

            xsum_large_add_value_inf_nan(lacc, ix, uintv);
        } else {
            /* Store the decremented count of additions allowed before transfer,
                and add this value to the chunk. */

            lacc->count[ix] = count;
            lacc->chunk[ix] += uintv;
        }

        n -= 1;

    } while (n > 0);
}

/* ADD ONE DOUBLE TO A LARGE ACCUMULATOR.  Just calls xsum_large_addv. */

void xsum_large_add1(xsum_large_accumulator *restrict lacc, xsum_flt value) {
    xsum_large_addv(lacc, &value, 1);
}

/* RETURN RESULT OF ROUNDING A LARGE ACCUMULATOR.  Rounding mode is to nearest,
   with ties to even.

   This is done by adding all the chunks in the large accumulator to the
   small accumulator, and then calling its rounding procedure. */

xsum_flt xsum_large_round(xsum_large_accumulator *restrict lacc) {
    xsum_large_transfer_to_small(lacc);

    return xsum_small_round(&lacc->sacc);
}

/* TRANSFER NUMBER FROM A LARGE ACCUMULATOR TO A SMALL ACCUMULATOR. */

void xsum_large_to_small_accumulator(xsum_small_accumulator *restrict sacc, xsum_large_accumulator *restrict lacc) {
    xsum_large_transfer_to_small(lacc);
    *sacc = lacc->sacc;
}

/* TRANSFER NUMBER FROM A SMALL ACCUMULATOR TO A LARGE ACCUMULATOR. */

void xsum_small_to_large_accumulator(xsum_large_accumulator *restrict lacc, xsum_small_accumulator *restrict sacc) {
    xsum_large_init_chunks(lacc);
    lacc->sacc = *sacc;
}
