use crate::{
    constants::{BYTES_TO_OVERFLOW_U64, MAX_PRECISION, MAX_STR_BUFFER_SIZE, OVERFLOW_U96, WILL_OVERFLOW_U64},
    error::{tail_error, Error},
    ops::array::{div_by_u32, is_all_zero},
    Decimal,
};

use arrayvec::{ArrayString, ArrayVec};

use alloc::{string::String, vec::Vec};
use core::fmt;

// impl that doesn't allocate for serialization purposes.
pub(crate) fn to_str_internal(
    value: &Decimal,
    append_sign: bool,
    precision: Option<usize>,
) -> (ArrayString<MAX_STR_BUFFER_SIZE>, Option<usize>) {
    // Get the scale - where we need to put the decimal point
    let scale = value.scale() as usize;

    // Convert to a string and manipulate that (neg at front, inject decimal)
    let mut chars = ArrayVec::<_, MAX_STR_BUFFER_SIZE>::new();
    let mut working = value.mantissa_array3();
    while !is_all_zero(&working) {
        let remainder = div_by_u32(&mut working, 10u32);
        chars.push(char::from(b'0' + remainder as u8));
    }
    while scale > chars.len() {
        chars.push('0');
    }

    let (prec, additional) = match precision {
        Some(prec) => {
            let max: usize = MAX_PRECISION.into();
            if prec > max {
                (max, Some(prec - max))
            } else {
                (prec, None)
            }
        }
        None => (scale, None),
    };

    let len = chars.len();
    let whole_len = len - scale;
    let mut rep = ArrayString::new();
    // Append the negative sign if necessary while also keeping track of the length of an "empty" string representation
    let empty_len = if append_sign && value.is_sign_negative() {
        rep.push('-');
        1
    } else {
        0
    };
    for i in 0..whole_len + prec {
        if i == len - scale {
            if i == 0 {
                rep.push('0');
            }
            rep.push('.');
        }

        if i >= len {
            rep.push('0');
        } else {
            let c = chars[len - i - 1];
            rep.push(c);
        }
    }

    // corner case for when we truncated everything in a low fractional
    if rep.len() == empty_len {
        rep.push('0');
    }

    (rep, additional)
}

pub(crate) fn fmt_scientific_notation(
    value: &Decimal,
    exponent_symbol: &str,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    #[cfg(not(feature = "std"))]
    use alloc::string::ToString;

    // Get the scale - this is the e value. With multiples of 10 this may get bigger.
    let mut exponent = -(value.scale() as isize);

    // Convert the integral to a string
    let mut chars = Vec::new();
    let mut working = value.mantissa_array3();
    while !is_all_zero(&working) {
        let remainder = div_by_u32(&mut working, 10u32);
        chars.push(char::from(b'0' + remainder as u8));
    }

    // First of all, apply scientific notation rules. That is:
    //  1. If non-zero digit comes first, move decimal point left so that e is a positive integer
    //  2. If decimal point comes first, move decimal point right until after the first non-zero digit
    // Since decimal notation naturally lends itself this way, we just need to inject the decimal
    // point in the right place and adjust the exponent accordingly.

    let len = chars.len();
    let mut rep;
    if len > 1 {
        if chars.iter().take(len - 1).all(|c| *c == '0') {
            // Chomp off the zero's.
            rep = chars.iter().skip(len - 1).collect::<String>();
        } else {
            chars.insert(len - 1, '.');
            rep = chars.iter().rev().collect::<String>();
        }
        exponent += (len - 1) as isize;
    } else {
        rep = chars.iter().collect::<String>();
    }

    rep.push_str(exponent_symbol);
    rep.push_str(&exponent.to_string());
    f.pad_integral(value.is_sign_positive(), "", &rep)
}

// dedicated implementation for the most common case.
#[inline]
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
#[inline]
pub(crate) fn parse_str_radix_n(str: &str, radix: u32) -> Result<Decimal, crate::Error> {
    if radix < 2 {
        return Err(Error::from("Unsupported radix < 2"));
    }
    if radix > 36 {
        // As per trait documentation
        return Err(Error::from("Unsupported radix > 36"));
    }

    match radix {
        2 => parse_str_radix_const::<2>(str),
        3 => parse_str_radix_const::<3>(str),
        4 => parse_str_radix_const::<4>(str),
        5 => parse_str_radix_const::<5>(str),
        6 => parse_str_radix_const::<6>(str),
        7 => parse_str_radix_const::<7>(str),
        8 => parse_str_radix_const::<8>(str),
        9 => parse_str_radix_const::<9>(str),
        10 => parse_str_radix_const::<10>(str),
        11 => parse_str_radix_const::<11>(str),
        12 => parse_str_radix_const::<12>(str),
        13 => parse_str_radix_const::<13>(str),
        14 => parse_str_radix_const::<14>(str),
        15 => parse_str_radix_const::<15>(str),
        16 => parse_str_radix_const::<16>(str),
        17 => parse_str_radix_const::<17>(str),
        18 => parse_str_radix_const::<18>(str),
        19 => parse_str_radix_const::<19>(str),
        20 => parse_str_radix_const::<20>(str),
        21 => parse_str_radix_const::<21>(str),
        22 => parse_str_radix_const::<22>(str),
        23 => parse_str_radix_const::<23>(str),
        24 => parse_str_radix_const::<24>(str),
        25 => parse_str_radix_const::<25>(str),
        26 => parse_str_radix_const::<26>(str),
        27 => parse_str_radix_const::<27>(str),
        28 => parse_str_radix_const::<28>(str),
        29 => parse_str_radix_const::<29>(str),
        30 => parse_str_radix_const::<30>(str),
        31 => parse_str_radix_const::<31>(str),
        32 => parse_str_radix_const::<32>(str),
        33 => parse_str_radix_const::<33>(str),
        34 => parse_str_radix_const::<34>(str),
        35 => parse_str_radix_const::<35>(str),
        36 => parse_str_radix_const::<36>(str),
        _ => unreachable!(),
    }
}

const fn estimated_max_precision(radix: u8) -> u8 {
    match radix {
        2 => 96,
        3 => 61,
        4 => 48,
        5 => 42,
        6 => 38,
        7 => 35,
        8 => 32,
        9 => 31,
        10 => 28,
        11 => 28,
        12 => 27,
        13 => 26,
        14 => 26,
        15 => 25,
        16 => 24,
        17 => 24,
        18 => 24,
        19 => 23,
        20 => 23,
        21 => 22,
        22 => 22,
        23 => 22,
        24 => 21,
        25 => 21,
        26 => 21,
        27 => 21,
        28 => 20,
        29 => 20,
        30 => 20,
        31 => 20,
        32 => 20,
        33 => 20,
        34 => 19,
        35 => 19,
        36 => 19,
        _ => 0xFF,
    }
}

#[inline]
pub(crate) fn parse_str_radix_const<const RADIX: u8>(str: &str) -> Result<Decimal, crate::Error> {
    if str.is_empty() {
        return Err(Error::from("Invalid decimal: empty"));
    }
    debug_assert!(RADIX >= 2);
    debug_assert!(RADIX <= 36);

    let mut bytes = str.as_bytes();
    let mut negative = false; // assume positive

    // handle the sign

    bytes = match bytes[0] {
        b'-' => {
            negative = true; // leading minus means negative
            &bytes[1..]
        }
        b'+' => {
            // leading + allowed
            &bytes[1..]
        }
        _ => bytes,
    };

    // should now be at numeric part of the significand
    let mut passed_decimal_point = false;
    let mut has_digit = false;
    let mut scale: u8 = 0;
    let mut data64: u64 = 0;

    while data64 < (u64::MAX / RADIX as u64 - u8::MAX as u64) {
        // we have already validated that we cannot overflow
        if !bytes.is_empty() {
            let b = bytes[0];
            match b {
                b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' => {
                    let digit = match b {
                        b'0'..=b'9' => u32::from(b - b'0'),
                        b'a'..=b'z' if RADIX > 10 => u32::from(b - b'a' + 10),
                        b'A'..=b'Z' if RADIX > 10 => u32::from(b - b'A' + 10),
                        _ => return Err(Error::from("Invalid decimal: invalid character")),
                    };
                    if digit >= RADIX as u32 {
                        return Err(Error::from("Invalid decimal: invalid character"));
                    }
                    has_digit = true;

                    data64 = data64 * RADIX as u64 + digit as u64;
                    scale += passed_decimal_point as u8;

                    if scale >= estimated_max_precision(RADIX) {
                        if !bytes.is_empty() {
                            return maybe_round_radix::<RADIX>(
                                data64 as u128,
                                bytes[0],
                                scale,
                                passed_decimal_point,
                                negative,
                            );
                        }
                        break;
                    }
                }
                b'.' => {
                    if passed_decimal_point {
                        return Err(Error::from("Invalid decimal: two decimal points"));
                    }
                    passed_decimal_point = true;
                }
                b'_' => {
                    // Must start with a number...
                    if !has_digit {
                        return Err(Error::from("Invalid decimal: must start lead with a number"));
                    }
                }
                _ => return Err(Error::from("Invalid decimal: unknown character")),
            }
            bytes = &bytes[1..];
        } else {
            break;
        }
    }

    let data: u128 = data64 as u128;

    if !bytes.is_empty() {
        return handle_full_128_radix::<RADIX>(data, bytes, scale, passed_decimal_point, negative);
    } else if !has_digit {
        return Err(Error::from("Invalid decimal: no digits found"));
    }

    handle_data_radix(data, scale, negative)
}

const fn midpoint_for_radix(radix: u8) -> u8 {
    if radix & 0x1 == 1 {
        radix / 2
    } else {
        (radix + 1) / 2
    }
}

#[inline(never)]
#[cold]
fn handle_full_128_radix<const RADIX: u8>(
    mut data: u128,
    mut bytes: &[u8],
    mut scale: u8,
    mut passed_decimal_point: bool,
    negative: bool,
) -> Result<Decimal, crate::Error> {
    while !bytes.is_empty() {
        let b = bytes[0];
        match b {
            b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' => {
                let digit = match b {
                    b'0'..=b'9' => u32::from(b - b'0'),
                    b'a'..=b'z' if RADIX > 10 => u32::from(b - b'a' + 10),
                    b'A'..=b'Z' if RADIX > 10 => u32::from(b - b'A' + 10),
                    _ => return Err(Error::from("Invalid decimal: invalid character")),
                };
                if digit >= RADIX as u32 {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }

                // If the data is going to overflow then we should go into recovery mode
                let next = data * RADIX as u128 + digit as u128;
                if overflow_128(next) {
                    // This means that we have more data to process, that we're not sure what to do with.
                    // This may or may not be an issue - depending on whether we're past a decimal point
                    // or not.
                    if !passed_decimal_point {
                        return Err(Error::from("Invalid decimal: overflow from too many digits"));
                    }

                    if digit >= midpoint_for_radix(RADIX) as u32 {
                        data += 1;
                    }
                    break;
                } else {
                    data = next;
                    scale += passed_decimal_point as u8;

                    if scale >= estimated_max_precision(RADIX) {
                        if !bytes.is_empty() {
                            return maybe_round_radix::<RADIX>(
                                data as u128,
                                bytes[0],
                                scale,
                                passed_decimal_point,
                                negative,
                            );
                        }
                        break;
                    }
                }
            }
            b'.' => {
                if passed_decimal_point {
                    return Err(Error::from("Invalid decimal: two decimal points"));
                }
                passed_decimal_point = true;
            }
            b'_' => {}
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        }
        bytes = &bytes[1..];
    }

    handle_data_radix(data, scale, negative)
}

#[inline(never)]
#[cold]
fn maybe_round_radix<const RADIX: u8>(
    mut data: u128,
    next_byte: u8,
    scale: u8,
    passed_decimal_point: bool,
    negative: bool,
) -> Result<Decimal, crate::Error> {
    let digit = match next_byte {
        b'0'..=b'9' => u32::from(next_byte - b'0'),
        b'a'..=b'z' if RADIX > 10 => u32::from(next_byte - b'a' + 10),
        b'A'..=b'Z' if RADIX > 10 => u32::from(next_byte - b'A' + 10),
        b'_' => 0, // this is a bug?
        b'.' => {
            // Still an error if we have a second dp
            if passed_decimal_point {
                return Err(Error::from("Invalid decimal: two decimal points"));
            }
            0
        }
        _ => return Err(Error::from("Invalid decimal: unknown character")),
    };

    if digit >= RADIX as u32 {
        return Err(Error::from("Invalid decimal: invalid character"));
    }

    // Round at midpoint
    if digit >= midpoint_for_radix(RADIX) as u32 {
        data += 1;
        if overflow_128(data) {
            // Highly unlikely scenario which is more indicative of a bug
            return Err(Error::from("Invalid decimal: overflow when rounding"));
        }
    }

    handle_data_radix(data, scale, negative)
}

fn handle_data_radix(data: u128, scale: u8, negative: bool) -> Result<Decimal, crate::Error> {
    debug_assert_eq!(data >> 96, 0);

    let data = [data as u32, (data >> 32) as u32, (data >> 64) as u32];

    Ok(Decimal::from_parts(data[0], data[1], data[2], negative, scale as u32))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Decimal;
    use arrayvec::ArrayString;
    use core::{fmt::Write, str::FromStr};

    #[test]
    fn display_does_not_overflow_max_capacity() {
        let num = Decimal::from_str("1.2").unwrap();
        let mut buffer = ArrayString::<64>::new();
        let _ = buffer.write_fmt(format_args!("{:.31}", num)).unwrap();
        assert_eq!("1.2000000000000000000000000000000", buffer.as_str());
    }

    #[test]
    fn from_str_rounding_0() {
        let str = "1.234";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::new(1234, 3).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_rounding_1() {
        let str = "11111_11111_11111.11111_11111_11111";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_111, 14).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_rounding_2() {
        let str = "11111_11111_11111.11111_11111_11115";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_112, 14).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_rounding_3() {
        let str = "11111_11111_11111.11111_11111_11195";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(1_111_111_111_111_111_111_111_111_1120, 14).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_rounding_4() {
        let str = "99999_99999_99999.99999_99999_99995";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(10_000_000_000_000_000_000_000_000_000, 13).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_many_pointless_chars() {
        let str = "00________________________________________________________________001.1";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(11, 1).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_1() {
        let str = "00001.1";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(11, 1).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_2() {
        let str = "00000_00000_00000_00000_00001.00001";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(100001, 5).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_3() {
        let str = "0.00000_00000_00000_00000_00000_00100";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(1, 28).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_trailing_0s_1() {
        let str = "0.00001_00000_00000";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(10_000_000_000, 15).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_trailing_0s_2() {
        let str = "0.00001_00000_00000_00000_00000_00000";
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(100_000_000_000_000_000_000_000, 28).unpack()
        );
        assert_eq!(
            parse_str_radix_10(str).unwrap().unpack(),
            parse_str_radix_n(str, 10).unwrap().unpack()
        );
    }

    #[test]
    fn from_str_overflow_1() {
        let str = "99999_99999_99999_99999_99999_99999.99999";
        assert_eq!(
            parse_str_radix_10(str),
            Err(Error::from("Invalid decimal: overflow from too many digits"))
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_overflow_2() {
        let str = "99999_99999_99999_99999_99999_11111.11111";
        assert!(parse_str_radix_10(str).is_err());
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_overflow_3() {
        let str = "99999_99999_99999_99999_99999_99994";
        assert!(parse_str_radix_10(str).is_err());
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_overflow_4() {
        let str = "99999_99999_99999_99999_99999_999.99";
        assert_eq!(
            // This does not overflow, moving the decimal point 1 more step would result in
            // 'overflow from too many digits'
            parse_str_radix_10(str).unwrap().unpack(),
            Decimal::from_i128_with_scale(10_000_000_000_000_000_000_000_000_000, 0).unpack()
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_edge_cases_1() {
        let str = "";
        assert_eq!(parse_str_radix_10(str), Err(Error::from("Invalid decimal: empty")));
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_edge_cases_2() {
        let str = "0.1.";
        assert_eq!(
            parse_str_radix_10(str),
            Err(Error::from("Invalid decimal: two decimal points"))
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_edge_cases_3() {
        let str = "_";
        assert_eq!(
            parse_str_radix_10(str),
            Err(Error::from("Invalid decimal: must start lead with a number"))
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_edge_cases_4() {
        let str = "1?2";
        assert_eq!(
            parse_str_radix_10(str),
            Err(Error::from("Invalid decimal: unknown character"))
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }

    #[test]
    fn from_str_edge_cases_5() {
        let str = ".";
        assert_eq!(
            parse_str_radix_10(str),
            Err(Error::from("Invalid decimal: no digits found"))
        );
        assert_eq!(parse_str_radix_10(str), parse_str_radix_n(str, 10));
    }
}
