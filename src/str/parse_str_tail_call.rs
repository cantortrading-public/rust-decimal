use crate::{
    constants::{BYTES_TO_OVERFLOW_U64, OVERFLOW_U96, WILL_OVERFLOW_U64},
    error::Error,
    Decimal,
};

#[inline]
// dedicated implementation for the most common case.
pub(crate) fn parse_str_radix_10(str: &str) -> Result<Decimal, crate::Error> {
    let bytes = str.as_bytes();

    if bytes.len() < BYTES_TO_OVERFLOW_U64 {
        parse_str_radix_10_dispatch::<false>(bytes)
    } else {
        parse_str_radix_10_dispatch::<true>(bytes)
    }
}

#[inline]
fn parse_str_radix_10_dispatch<const BIG: bool>(bytes: &[u8]) -> Result<Decimal, crate::Error> {
    match bytes {
        [b, rest @ ..] => byte_dispatch_u64::<false, false, false, BIG, true>(rest, 0, 0, *b),
        [] => tail_error("Invalid decimal: empty"),
    }
}

#[inline]
fn overflow_64(val: u64) -> bool {
    val >= WILL_OVERFLOW_U64
}

#[inline]
pub fn overflow_128(val: u128) -> bool {
    val >= OVERFLOW_U96
}

#[inline]
fn dispatch_next<const POINT: bool, const NEG: bool, const HAS: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<Decimal, crate::Error> {
    if let Some((next, bytes)) = bytes.split_first() {
        byte_dispatch_u64::<POINT, NEG, HAS, BIG, false>(bytes, data64, scale, *next)
    } else {
        handle_data::<NEG, HAS>(data64 as u128, scale)
    }
}

#[inline(never)]
fn non_digit_dispatch_u64<const POINT: bool, const NEG: bool, const HAS: bool, const BIG: bool, const FIRST: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    b: u8,
) -> Result<Decimal, crate::Error> {
    match b {
        b'-' if FIRST && !HAS => dispatch_next::<false, true, false, BIG>(bytes, data64, scale),
        b'+' if FIRST && !HAS => dispatch_next::<false, false, false, BIG>(bytes, data64, scale),
        b'_' if HAS => handle_separator::<POINT, NEG, BIG>(bytes, data64, scale),
        b => tail_invalid_digit(b),
    }
}

#[inline]
fn byte_dispatch_u64<
    const POINT: bool, // a decimal point has been seen
    const NEG: bool,   // we've encountered a '-' and the number is negative
    const HAS: bool,   // a digit has been encountered (when HAS is false it's invalid)
    const BIG: bool,   // a number that uses 96 bits instead of only 64 bits
    const FIRST: bool, // true if it is the first byte in the string
>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    b: u8,
) -> Result<Decimal, crate::Error> {
    match b {
        b'0'..=b'9' => handle_digit_64::<POINT, NEG, BIG>(bytes, data64, scale, b - b'0'),
        b'.' if !POINT => handle_point::<NEG, HAS, BIG>(bytes, data64, scale),
        b => non_digit_dispatch_u64::<POINT, NEG, HAS, BIG, FIRST>(bytes, data64, scale, b),
    }
}

#[inline(never)]
fn handle_digit_64<const POINT: bool, const NEG: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    digit: u8,
) -> Result<Decimal, crate::Error> {
    // we have already validated that we cannot overflow
    let data64 = data64 * 10 + digit as u64;
    let scale = if POINT { scale + 1 } else { 0 };

    if let Some((next, bytes)) = bytes.split_first() {
        let next = *next;
        if POINT && BIG && scale >= 28 {
            maybe_round(data64 as u128, next, scale, POINT, NEG)
        } else if BIG && overflow_64(data64) {
            handle_full_128::<POINT, NEG>(data64 as u128, bytes, scale, next)
        } else {
            byte_dispatch_u64::<POINT, NEG, true, BIG, false>(bytes, data64, scale, next)
        }
    } else {
        let data: u128 = data64 as u128;

        handle_data::<NEG, true>(data, scale)
    }
}

#[inline(never)]
fn handle_point<const NEG: bool, const HAS: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<Decimal, crate::Error> {
    dispatch_next::<true, NEG, HAS, BIG>(bytes, data64, scale)
}

#[inline(never)]
fn handle_separator<const POINT: bool, const NEG: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<Decimal, crate::Error> {
    dispatch_next::<POINT, NEG, true, BIG>(bytes, data64, scale)
}

#[inline(never)]
#[cold]
fn tail_invalid_digit(digit: u8) -> Result<Decimal, crate::Error> {
    match digit {
        b'.' => tail_error("Invalid decimal: two decimal points"),
        b'_' => tail_error("Invalid decimal: must start lead with a number"),
        _ => tail_error("Invalid decimal: unknown character"),
    }
}

#[inline(never)]
#[cold]
fn handle_full_128<const POINT: bool, const NEG: bool>(
    mut data: u128,
    bytes: &[u8],
    scale: u8,
    next_byte: u8,
) -> Result<Decimal, crate::Error> {
    let b = next_byte;
    match b {
        b'0'..=b'9' => {
            let digit = u32::from(b - b'0');

            // If the data is going to overflow then we should go into recovery mode
            let next = (data * 10) + digit as u128;
            if overflow_128(next) {
                if !POINT {
                    return tail_error("Invalid decimal: overflow from too many digits");
                }

                if digit >= 5 {
                    data += 1;
                }
                handle_data::<NEG, true>(data, scale)
            } else {
                data = next;
                let scale = scale + POINT as u8;
                if let Some((next, bytes)) = bytes.split_first() {
                    let next = *next;
                    if POINT && scale >= 28 {
                        maybe_round(data, next, scale, POINT, NEG)
                    } else {
                        handle_full_128::<POINT, NEG>(data, bytes, scale, next)
                    }
                } else {
                    handle_data::<NEG, true>(data, scale)
                }
            }
        }
        b'.' if !POINT => {
            // This call won't tail?
            if let Some((next, bytes)) = bytes.split_first() {
                handle_full_128::<true, NEG>(data, bytes, scale, *next)
            } else {
                handle_data::<NEG, true>(data, scale)
            }
        }
        b'_' => {
            if let Some((next, bytes)) = bytes.split_first() {
                handle_full_128::<POINT, NEG>(data, bytes, scale, *next)
            } else {
                handle_data::<NEG, true>(data, scale)
            }
        }
        b => tail_invalid_digit(b),
    }
}

#[inline(never)]
#[cold]
fn maybe_round(mut data: u128, next_byte: u8, scale: u8, point: bool, negative: bool) -> Result<Decimal, crate::Error> {
    let digit = match next_byte {
        b'0'..=b'9' => u32::from(next_byte - b'0'),
        b'_' => 0, // this should be an invalid string?
        b'.' if point => 0,
        b => return tail_invalid_digit(b),
    };

    // Round at midpoint
    if digit >= 5 {
        data += 1;
        if overflow_128(data) {
            // Highly unlikely scenario which is more indicative of a bug
            return tail_error("Invalid decimal: overflow when rounding");
        }
    }

    if negative {
        handle_data::<true, true>(data, scale)
    } else {
        handle_data::<false, true>(data, scale)
    }
}

#[inline(never)]
fn tail_no_has() -> Result<Decimal, crate::Error> {
    tail_error("Invalid decimal: no digits found")
}

#[inline]
fn handle_data<const NEG: bool, const HAS: bool>(data: u128, scale: u8) -> Result<Decimal, crate::Error> {
    debug_assert_eq!(data >> 96, 0);
    if !HAS {
        tail_no_has()
    } else {
        Ok(Decimal::from_parts(
            data as u32,
            (data >> 32) as u32,
            (data >> 64) as u32,
            NEG,
            scale as u32,
        ))
    }
}

#[cold]
pub(crate) fn tail_error(from: &'static str) -> Result<Decimal, Error> {
    Err(from.into())
}
