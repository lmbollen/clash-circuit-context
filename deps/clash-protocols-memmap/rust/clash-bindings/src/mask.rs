// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! A `Mask<M, N>` type with bool-array semantics.
//!
//! `Mask<M, N>` is a transparent newtype around [`BitVector<M, N>`]: it has
//! identical storage and identical bit ordering (bit `i` of the mask is bit
//! `i` of the underlying byte array — LSB at byte 0, bit 0). The wrapper
//! exists to provide a bool-indexed API (`get`, `set`, `from_iter`, `iter`)
//! without re-implementing storage or size-checking. Conversion to/from
//! `BitVector` is zero-cost.
//!
//! This is the Rust counterpart of the Haskell `Mask n` type from
//! `Protocols.MemoryMap.Mask`. The Haskell side enforces the
//! "index `i` of a `Vec n Bool` corresponds to bit `i`" convention, which is
//! the same convention this type uses on the Rust side.
#![deny(missing_docs)]

use core::ops::Index;

use crate::bitvector::{BitVector, BitVectorSizeCheck};

/// Bit-mask of `M` bits, backed by a [`BitVector<M, N>`].
///
/// Bit `i` (for `i in 0..M`) is bit `i` of the underlying byte array (byte
/// `i / 8`, bit `i % 8`). This matches the storage convention of
/// [`BitVector<M, N>`], so converting between `Mask<M, N>` and
/// `BitVector<M, N>` is zero-cost.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Mask<const M: usize, const N: usize>(pub(crate) BitVector<M, N>);

impl<const M: usize, const N: usize> Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    /// Create a `Mask` with all bits cleared.
    #[inline]
    pub fn zero() -> Self {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // SAFETY: the all-zero byte array represents the value `0`, which is
        // in range for any `BitVector<M, N>`.
        Mask(unsafe { BitVector::<M, N>::new_unchecked([0u8; N]) })
    }

    /// Read bit `i`. Returns `None` if `i >= M`.
    #[inline]
    pub fn get(&self, i: usize) -> Option<bool> {
        if i >= M {
            None
        } else {
            // SAFETY: just bounds-checked.
            Some(unsafe { self.get_unchecked(i) })
        }
    }

    /// Read bit `i` without bounds-checking.
    ///
    /// # Safety
    /// `i` must be `< M`.
    #[inline]
    pub unsafe fn get_unchecked(&self, i: usize) -> bool {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // PERFORMANCE: The `/8` and `%8` compile to `i >> 3` and `i & 7` respectively
        let byte = self.0.0[i / 8];
        ((byte >> (i % 8)) & 1) == 1
    }

    /// Set bit `i` to `b`. Returns `None` if `i >= M`.
    #[inline]
    pub fn set(&mut self, i: usize, b: bool) -> Option<()> {
        if i >= M {
            return None;
        }
        // SAFETY: just bounds-checked.
        unsafe { self.set_unchecked(i, b) };
        Some(())
    }

    /// Set bit `i` to `b` without bounds-checking.
    ///
    /// # Safety
    /// `i` must be `< M`.
    #[inline]
    pub unsafe fn set_unchecked(&mut self, i: usize, b: bool) {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // PERFORMANCE: The `/8` and `%8` compile to `i >> 3` and `i & 7` respectively
        let bit = 1u8 << (i % 8);
        let byte = &mut self.0.0[i / 8];
        if b {
            *byte |= bit;
        } else {
            *byte &= !bit;
        }
    }

    /// Iterate over the bits in order `0..M`.
    #[inline]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
        (0..M).map(|i| unsafe { self.get_unchecked(i) })
    }

    /// Unwrap to the underlying [`BitVector<M, N>`].
    #[inline]
    pub fn into_bitvector(self) -> BitVector<M, N> {
        self.0
    }

    /// Construct a `Mask` from a [`BitVector<M, N>`]
    #[inline]
    pub fn from_bitvector(bv: BitVector<M, N>) -> Self {
        Mask(bv)
    }
}

impl<const M: usize, const N: usize> FromIterator<bool> for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    /// Build a `Mask` from a bool iterator. Bit `i` of the result is the
    /// `i`-th item of the iterator. Items at index `>= M` are silently
    /// ignored; if the iterator yields fewer than `M` items, the missing
    /// bits are taken as `false`.
    #[inline]
    fn from_iter<I: IntoIterator<Item = bool>>(bools: I) -> Self {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        let mut out = Self::zero();
        for (i, b) in bools.into_iter().enumerate().take(M) {
            // SAFETY: `take(M)` keeps `i < M`.
            unsafe { out.set_unchecked(i, b) };
        }
        out
    }
}

impl<const M: usize, const N: usize> Index<usize> for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    type Output = bool;

    /// Read bit `i`. Panics if `i >= M`.
    #[inline]
    fn index(&self, i: usize) -> &bool {
        match self.get(i) {
            Some(true) => &true,
            Some(false) => &false,
            None => panic!("Mask index out of bounds: {i} >= {M}"),
        }
    }
}

impl<const M: usize, const N: usize, const O: usize, const P: usize> From<BitVector<M, N>>
    for Mask<O, P>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
    BitVector<O, P>: BitVectorSizeCheck<Inner = [u8; P]>,
{
    #[inline]
    fn from(bv: BitVector<M, N>) -> Mask<O, P> {
        let _ = BitVector::<M, N>::SIZE_CHECK;
        let _ = BitVector::<O, P>::SIZE_CHECK;
        let _ = const {
            if M > O {
                const_panic::concat_panic!(
                    const_panic::fmt::FmtArg::DISPLAY;
                    "Cannot infallibly convert from a `BitVector<",
                    M,
                    ", ",
                    N,
                    ">` into a `Mask<",
                    O,
                    ", ",
                    P,
                    ">` because `",
                    M,
                    " > ",
                    O,
                    "`",
                );
            }
        };
        if const { N == P } {
            Mask(BitVector(unsafe { core::mem::transmute_copy(&bv.0) }))
        } else {
            let mut mask = [0; P];
            mask[0..N].copy_from_slice(&bv.0[..]);
            Mask(BitVector(mask))
        }
    }
}

impl<const M: usize, const N: usize, const O: usize, const P: usize> From<Mask<M, N>>
    for BitVector<O, P>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
    BitVector<O, P>: BitVectorSizeCheck<Inner = [u8; P]>,
{
    #[inline]
    fn from(mask: Mask<M, N>) -> BitVector<O, P> {
        let _ = BitVector::<M, N>::SIZE_CHECK;
        let _ = BitVector::<O, P>::SIZE_CHECK;
        let _ = const {
            if M > O {
                const_panic::concat_panic!(
                    const_panic::fmt::FmtArg::DISPLAY;
                    "Cannot infallibly convert from a `Mask<",
                    M,
                    ", ",
                    N,
                    ">` into a `BitVector<",
                    O,
                    ", ",
                    P,
                    ">` because `",
                    M,
                    " > ",
                    O,
                    "`",
                );
            }
        };
        if const { N == P } {
            BitVector(unsafe { core::mem::transmute_copy(&mask.0.0) })
        } else {
            let mut bv = [0; P];
            bv[0..N].copy_from_slice(&mask.0.0[..]);
            BitVector(bv)
        }
    }
}

impl<const M: usize, const N: usize> core::fmt::Debug for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck,
{
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(fmt, "Mask<{M}>(")?;
        <BitVector<M, N> as core::fmt::Debug>::fmt(&self.0, fmt)?;
        write!(fmt, ")")
    }
}

impl<const M: usize, const N: usize> ufmt::uDebug for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck,
{
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "Mask<{}>(", M)?;
        <BitVector<M, N> as ufmt::uDebug>::fmt(&self.0, fmt)?;
        ufmt::uwrite!(fmt, ")")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_iter_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let collected: [bool; 8] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn from_iter_round_trip_non_byte_multiple() {
        let bs = [
            true, false, true, true, false, false, false, true, true, true, false, false, true,
        ];
        let m: Mask<13, 2> = Mask::from_iter(bs);
        let collected: [bool; 13] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn get_and_set() {
        let mut m: Mask<10, 2> = Mask::zero();
        assert_eq!(m.get(0), Some(false));
        assert_eq!(m.get(9), Some(false));
        assert_eq!(m.get(10), None);

        m.set(0, true).unwrap();
        m.set(9, true).unwrap();
        assert_eq!(m.get(0), Some(true));
        assert_eq!(m.get(9), Some(true));
        assert_eq!(m.set(10, true), None);

        m.set(0, false).unwrap();
        assert_eq!(m.get(0), Some(false));
    }

    #[test]
    fn lsb_first_layout() {
        // Bit 0 must be the LSB of the underlying byte array.
        let mut m: Mask<16, 2> = Mask::zero();
        m.set(0, true).unwrap();
        let bv: BitVector<16, 2> = m.into();
        assert_eq!(bv.into_inner(), [0x01, 0x00]);

        // Bit 8 must be bit 0 of byte 1.
        let mut m: Mask<16, 2> = Mask::zero();
        m.set(8, true).unwrap();
        let bv: BitVector<16, 2> = m.into();
        assert_eq!(bv.into_inner(), [0x00, 0x01]);
    }

    #[test]
    fn index_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        for i in 0..8 {
            assert_eq!(m[i], bs[i]);
        }
    }

    #[test]
    #[should_panic]
    fn index_out_of_bounds_panics() {
        let m: Mask<8, 1> = Mask::zero();
        let _ = m[8];
    }

    #[test]
    fn iter_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let mut collected = [false; 8];
        for (i, b) in m.iter().enumerate() {
            collected[i] = b;
        }
        assert_eq!(collected, bs);
    }

    #[test]
    fn bitvector_round_trip() {
        // Round-trip Mask -> BitVector -> Mask preserves bits.
        let bs = [true, false, true, true, false, true, false, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let bv: BitVector<8, 1> = m.into();
        let m2: Mask<8, 1> = bv.into();
        let collected: [bool; 8] = core::array::from_fn(|i| m2[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn mask_macro_binary_literal() {
        let m: Mask<8, 1> = clash_macros::mask!(0b1010_0001, n = 8);
        let bs = [true, false, false, false, false, true, false, true];
        let collected: [bool; 8] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn mask_macro_hex_literal() {
        let m: Mask<32, 4> = clash_macros::mask!(0xab_cd_ef_01, n = 32);
        let bv: BitVector<32, 4> = m.into();
        let expected = clash_macros::bitvector!(0xab_cd_ef_01, n = 32);
        assert_eq!(bv, expected);
    }

    #[test]
    fn mask_macro_non_byte_multiple() {
        let m: Mask<13, 2> = clash_macros::mask!(0b1_0110_0011_0101, n = 13);
        let bv: BitVector<13, 2> = m.into();
        let expected = clash_macros::bitvector!(0b1_0110_0011_0101, n = 13);
        assert_eq!(bv, expected);
    }

    #[test]
    fn from_into_sanity() {
        use rand::{RngExt, SeedableRng, rngs::SmallRng};

        fn panic_helper<const A: usize, const B: usize, const C: usize, const D: usize>(
            lhs: &'static str,
            rhs: &'static str,
            expect: bool,
            actual: bool,
            idx: usize,
        ) {
            const_panic::concat_panic!(
                const_panic::fmt::FmtArg::DISPLAY;
                lhs, "<", C, ", ", D, ">",
                " from ",
                rhs, "<", A, ", ", B, ">",
                " failed at bit ",
                idx,
                ". Expected ",
                expect as u8,
                ", got ",
                actual as u8,
                ".",
            )
        }

        fn run_sanity_check<const A: usize, const B: usize, const C: usize, const D: usize>(
            rng: &mut SmallRng,
        ) {
            let mut small_arr = rng.random::<[u8; B]>();
            if !A.is_multiple_of(8) {
                small_arr[const { B - 1 }] &= const { !(!0 << (A % 8)) };
            }

            // Set up test values
            let mask_small: Mask<A, B> = Mask(BitVector(small_arr));
            let bv_small: BitVector<A, B> = BitVector(small_arr);

            // Extend
            let mask_from_bv_small: Mask<C, D> = bv_small.into();
            let bv_from_mask_small: BitVector<C, D> = mask_small.into();

            // Check that extension preserves lower bits (whole bytes)
            for byte in 0..const { B - 1 } {
                let byte_ext = byte * 8;
                for bit in 0..8 {
                    let idx = byte_ext + bit;

                    // Fetch bv bits
                    let bvs_bit = (bv_small.0[byte] >> bit) & 1 != 0;
                    let bvfms_bit = (bv_from_mask_small.0[byte] >> bit) & 1 != 0;

                    // Fetch mask bits
                    let ms_bit = mask_small.get(idx).unwrap_or_else(|| {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Failed to fetch index ", idx, " from Mask<", A, ", ", B, ">",
                        )
                    });
                    let mfbvs_bit = mask_from_bv_small.get(idx).unwrap_or_else(|| {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Failed to fetch index ", idx, " from Mask<", C, ", ", D, ">",
                        )
                    });

                    // Compare
                    if mfbvs_bit != ms_bit {
                        panic_helper::<A, B, C, D>("Mask", "BitVector", ms_bit, mfbvs_bit, idx);
                    }
                    if bvfms_bit != bvs_bit {
                        panic_helper::<A, B, C, D>("BitVector", "Mask", bvs_bit, bvfms_bit, idx);
                    }
                }
            }
            // Check that extension preserves lower bits (last byte, may be incomplete)
            for bit in 0..const { A % 8 } {
                let idx = const { B * 8 - 8 } + bit;

                // Fetch bv bits
                let bvs_bit = (bv_small.0[const { B - 1 }] >> bit) & 1 != 0;
                let bvfms_bit = (bv_from_mask_small.0[const { B - 1 }] >> bit) & 1 != 0;

                // Fetch mask bits
                let ms_bit = mask_small.get(idx).unwrap_or_else(|| {
                    const_panic::concat_panic!(
                        const_panic::fmt::FmtArg::DISPLAY;
                        "Failed to fetch index ", idx, " from Mask<", A, ", ", B, ">",
                    )
                });
                let mfbvs_bit = mask_from_bv_small.get(idx).unwrap_or_else(|| {
                    const_panic::concat_panic!(
                        const_panic::fmt::FmtArg::DISPLAY;
                        "Failed to fetch index ", idx, " from Mask<", C, ", ", D, ">",
                    )
                });

                // Compare
                if mfbvs_bit != ms_bit {
                    panic_helper::<A, B, C, D>("Mask", "BitVector", ms_bit, mfbvs_bit, idx);
                }
                if bvfms_bit != bvs_bit {
                    panic_helper::<A, B, C, D>("BitVector", "Mask", bvs_bit, bvfms_bit, idx);
                }
            }

            // Ensure extension is all zeros
            for idx in A..C {
                if let Some(true) = mask_from_bv_small.get(idx) {
                    panic_helper::<A, B, C, D>("Mask", "BitVector", false, true, idx);
                }
                if (bv_from_mask_small[idx / 8] >> (idx % 8)) & 1 != 0 {
                    panic_helper::<A, B, C, D>("BitVector", "Mask", false, true, idx);
                }
            }
        }

        let mut rng = rand::rngs::SmallRng::seed_from_u64(0x0DDB1A5E5BAD5EED);
        subst_macros::repeat_parallel_subst! {
            groups: [
                // Exhaustively test smaller cases
                [group [sub [SMALL] = [1]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [2]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [3]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [4]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [5]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [6]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [7]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [8]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [9]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [10]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [11]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [12]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [13]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [14]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [15]] [sub [LONG] = [16]]]
                [group [sub [SMALL] = [16]] [sub [LONG] = [16]]]
                // Test some larger cases
                [group [sub [SMALL] = [16]] [sub [LONG] = [128]]]
                [group [sub [SMALL] = [32]] [sub [LONG] = [128]]]
                [group [sub [SMALL] = [64]] [sub [LONG] = [128]]]
                [group [sub [SMALL] = [128]] [sub [LONG] = [128]]]
            ],
            callback: NONE,
            in: {
                {
                    const SMALL_BITS: usize = SMALL;
                    const LONG_BITS: usize = LONG;
                    const SMALL_BYTES: usize = SMALL_BITS.div_ceil(8);
                    const LONG_BYTES: usize = LONG_BITS.div_ceil(8);

                    run_sanity_check::<SMALL_BITS, SMALL_BYTES, LONG_BITS, LONG_BYTES>(&mut rng);
                }
            }
        }
    }
}
