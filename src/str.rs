use crate::{
    constants::{MAX_PRECISION, MAX_STR_BUFFER_SIZE},
    error::Error,
    ops::array::{add_by_internal_flattened, add_one_internal, div_by_u32, is_all_zero, mul_by_10, mul_by_u32},
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
pub(crate) fn parse_str_radix_10(str: &str) -> Result<Decimal, crate::Error> {
    if str.is_empty() {
        return Err(Error::from("Invalid decimal: empty"));
    }

    let mut offset = 0;
    let len = str.len();
    let bytes = str.as_bytes();
    let mut negative = false; // assume positive

    // handle the sign
    match bytes[offset] {
        b'-' => {
            negative = true; // leading minus means negative
            offset += 1;
        }
        b'+' => {
            // leading + allowed
            offset += 1;
        }
        _ => {}
    }

    // should now be at numeric part of the significand
    let mut passed_decimal_point = false;
    let mut has_digit = false;
    let mut coeff: u32 = 0; // number of digits
    let mut scale = 0;
    let mut maybe_round = false;

    let mut data = [0u32, 0u32, 0u32];
    let mut tmp = [0u32, 0u32, 0u32];
    while offset < len {
        let b = bytes[offset];
        match b {
            b'0'..=b'9' => {
                let digit = u32::from(b - b'0');
                has_digit = true;
                coeff += if coeff == 0 && digit == 0 { 0 } else { 1 }; // got one more digit

                // If the data is going to overflow then we should go into recovery mode
                tmp[0] = data[0];
                tmp[1] = data[1];
                tmp[2] = data[2];
                let overflow = mul_by_10(&mut tmp);
                if overflow > 0 {
                    // This means that we have more data to process, that we're not sure what to do with.
                    // This may or may not be an issue - depending on whether we're past a decimal point
                    // or not.
                    if !passed_decimal_point {
                        return Err(Error::from("Invalid decimal: overflow from too many digits"));
                    }

                    if digit >= 5 {
                        let carry = add_one_internal(&mut data);
                        if carry > 0 {
                            // Highly unlikely scenario which is more indicative of a bug
                            return Err(Error::from("Invalid decimal: overflow when rounding"));
                        }
                    }
                    break;
                } else {
                    data[0] = tmp[0];
                    data[1] = tmp[1];
                    data[2] = tmp[2];
                    let carry = add_by_internal_flattened(&mut data, digit);
                    if passed_decimal_point {
                        scale += 1;
                    }
                    if carry > 0 {
                        // Highly unlikely scenario which is more indicative of a bug
                        return Err(Error::from("Invalid decimal: overflow from carry"));
                    }
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
                if offset == 0 {
                    return Err(Error::from("Invalid decimal: must start lead with a number"));
                }
            }
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        }
        offset += 1;

        if coeff > MAX_PRECISION as u32 || scale >= 28 {
            maybe_round = true;
            break;
        }
    }

    if !has_digit {
        return Err(Error::from("Invalid decimal: no digits found"));
    }

    // If we exited before the end of the string then do some rounding if necessary
    if maybe_round && offset < bytes.len() {
        let next_byte = bytes[offset];
        let digit = match next_byte {
            b'0'..=b'9' => u32::from(next_byte - b'0'),
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

        // Round at midpoint
        if digit >= 5 {
            let carry = add_one_internal(&mut data);
            if carry > 0 {
                // Highly unlikely scenario which is more indicative of a bug
                return Err(Error::from("Invalid decimal: overflow when rounding"));
            }
        }
    }

    Ok(Decimal::from_parts(data[0], data[1], data[2], negative, scale))
}

pub(crate) fn parse_str_radix_n(str: &str, radix: u32) -> Result<Decimal, crate::Error> {
    if str.is_empty() {
        return Err(Error::from("Invalid decimal: empty"));
    }
    if radix < 2 {
        return Err(Error::from("Unsupported radix < 2"));
    }
    if radix > 36 {
        // As per trait documentation
        return Err(Error::from("Unsupported radix > 36"));
    }

    let mut offset = 0;
    let mut len = str.len();
    let bytes = str.as_bytes();
    let mut negative = false; // assume positive

    // handle the sign
    if bytes[offset] == b'-' {
        negative = true; // leading minus means negative
        offset += 1;
        len -= 1;
    } else if bytes[offset] == b'+' {
        // leading + allowed
        offset += 1;
        len -= 1;
    }

    // should now be at numeric part of the significand
    let mut digits_before_dot: i32 = -1; // digits before '.', -1 if no '.'
    let mut coeff = ArrayVec::<_, 96>::new(); // integer significand array

    // Supporting different radix
    let (max_n, max_alpha_lower, max_alpha_upper) = if radix <= 10 {
        (b'0' + (radix - 1) as u8, 0, 0)
    } else {
        let adj = (radix - 11) as u8;
        (b'9', adj + b'a', adj + b'A')
    };

    // Estimate the max precision. All in all, it needs to fit into 96 bits.
    // Rather than try to estimate, I've included the constants directly in here. We could,
    // perhaps, replace this with a formula if it's faster - though it does appear to be log2.
    let estimated_max_precision = match radix {
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
        _ => return Err(Error::from("Unsupported radix")),
    };

    let mut maybe_round = false;
    while len > 0 {
        let b = bytes[offset];
        match b {
            b'0'..=b'9' => {
                if b > max_n {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'0'));
                offset += 1;
                len -= 1;

                // If the coefficient is longer than the max, exit early
                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'a'..=b'z' => {
                if b > max_alpha_lower {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'a') + 10);
                offset += 1;
                len -= 1;

                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'A'..=b'Z' => {
                if b > max_alpha_upper {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'A') + 10);
                offset += 1;
                len -= 1;

                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'.' => {
                if digits_before_dot >= 0 {
                    return Err(Error::from("Invalid decimal: two decimal points"));
                }
                digits_before_dot = coeff.len() as i32;
                offset += 1;
                len -= 1;
            }
            b'_' => {
                // Must start with a number...
                if coeff.is_empty() {
                    return Err(Error::from("Invalid decimal: must start lead with a number"));
                }
                offset += 1;
                len -= 1;
            }
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        }
    }

    // If we exited before the end of the string then do some rounding if necessary
    if maybe_round && offset < bytes.len() {
        let next_byte = bytes[offset];
        let digit = match next_byte {
            b'0'..=b'9' => {
                if next_byte > max_n {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'0')
            }
            b'a'..=b'z' => {
                if next_byte > max_alpha_lower {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'a') + 10
            }
            b'A'..=b'Z' => {
                if next_byte > max_alpha_upper {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'A') + 10
            }
            b'_' => 0,
            b'.' => {
                // Still an error if we have a second dp
                if digits_before_dot >= 0 {
                    return Err(Error::from("Invalid decimal: two decimal points"));
                }
                0
            }
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        };

        // Round at midpoint
        let midpoint = if radix & 0x1 == 1 { radix / 2 } else { (radix + 1) / 2 };
        if digit >= midpoint {
            let mut index = coeff.len() - 1;
            loop {
                let new_digit = coeff[index] + 1;
                if new_digit <= 9 {
                    coeff[index] = new_digit;
                    break;
                } else {
                    coeff[index] = 0;
                    if index == 0 {
                        coeff.insert(0, 1u32);
                        digits_before_dot += 1;
                        coeff.pop();
                        break;
                    }
                }
                index -= 1;
            }
        }
    }

    // here when no characters left
    if coeff.is_empty() {
        return Err(Error::from("Invalid decimal: no digits found"));
    }

    let mut scale = if digits_before_dot >= 0 {
        // we had a decimal place so set the scale
        (coeff.len() as u32) - (digits_before_dot as u32)
    } else {
        0
    };

    // Parse this using specified radix
    let mut data = [0u32, 0u32, 0u32];
    let mut tmp = [0u32, 0u32, 0u32];
    let len = coeff.len();
    for (i, digit) in coeff.iter().enumerate() {
        // If the data is going to overflow then we should go into recovery mode
        tmp[0] = data[0];
        tmp[1] = data[1];
        tmp[2] = data[2];
        let overflow = mul_by_u32(&mut tmp, radix);
        if overflow > 0 {
            // This means that we have more data to process, that we're not sure what to do with.
            // This may or may not be an issue - depending on whether we're past a decimal point
            // or not.
            if (i as i32) < digits_before_dot && i + 1 < len {
                return Err(Error::from("Invalid decimal: overflow from too many digits"));
            }

            if *digit >= 5 {
                let carry = add_one_internal(&mut data);
                if carry > 0 {
                    // Highly unlikely scenario which is more indicative of a bug
                    return Err(Error::from("Invalid decimal: overflow when rounding"));
                }
            }
            // We're also one less digit so reduce the scale
            let diff = (len - i) as u32;
            if diff > scale {
                return Err(Error::from("Invalid decimal: overflow from scale mismatch"));
            }
            scale -= diff;
            break;
        } else {
            data[0] = tmp[0];
            data[1] = tmp[1];
            data[2] = tmp[2];
            let carry = add_by_internal_flattened(&mut data, *digit);
            if carry > 0 {
                // Highly unlikely scenario which is more indicative of a bug
                return Err(Error::from("Invalid decimal: overflow from carry"));
            }
        }
    }

    Ok(Decimal::from_parts(data[0], data[1], data[2], negative, scale))
}

#[cfg(test)]
mod test {
    use super::Error;
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
        assert_eq!(Decimal::from_str("1.234").unwrap(), Decimal::new(1234, 3));
    }

    #[test]
    fn from_str_rounding_1() {
        assert_eq!(
            Decimal::from_str("11111_11111_11111.11111_11111_11111").unwrap(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_111, 14)
        );
    }

    #[test]
    fn from_str_rounding_2() {
        assert_eq!(
            Decimal::from_str("11111_11111_11111.11111_11111_11115").unwrap(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_112, 14)
        );
    }

    #[test]
    fn from_str_rounding_3() {
        assert_eq!(
            Decimal::from_str("11111_11111_11111.11111_11111_11195").unwrap(),
            Decimal::from_i128_with_scale(1_111_111_111_111_111_111_111_111_112, 13)
        );
    }

    #[test]
    fn from_str_rounding_4() {
        assert_eq!(
            Decimal::from_str("99999_99999_99999.99999_99999_99995").unwrap(),
            Decimal::from_i128_with_scale(1_000_000_000_000_000_000_000_000_000, 12)
        );
    }

    #[test]
    fn from_str_leading_0s_1() {
        assert_eq!(
            Decimal::from_str("00001.1").unwrap(),
            Decimal::from_i128_with_scale(11, 1)
        );
    }

    #[test]
    fn from_str_leading_0s_2() {
        assert_eq!(
            Decimal::from_str("00000_00000_00000_00000_00001.00001").unwrap(),
            Decimal::from_i128_with_scale(100001, 5)
        );
    }

    #[test]
    fn from_str_leading_0s_3() {
        assert_eq!(
            Decimal::from_str("0.00000_00000_00000_00000_00000_00100").unwrap(),
            Decimal::from_i128_with_scale(1, 28)
        );
    }

    #[test]
    fn from_str_trailing_0s_1() {
        assert_eq!(
            Decimal::from_str("0.00001_00000_00000").unwrap(),
            Decimal::from_i128_with_scale(1, 5)
        );
    }

    #[test]
    fn from_str_trailing_0s_2() {
        assert_eq!(
            Decimal::from_str("0.00001_00000_00000_00000_00000_00000").unwrap(),
            Decimal::from_i128_with_scale(1, 5)
        );
    }

    #[test]
    fn from_str_overflow_1() {
        assert_eq!(
            Decimal::from_str("99999_99999_99999_99999_99999_99999.99999"),
            Err(Error::from("Invalid decimal: overflow from too many digits"))
        );
    }

    #[test]
    fn from_str_overflow_2() {
        assert_eq!(
            Decimal::from_str("99999_99999_99999_99999_99999_11111.11111"),
            Err(Error::from("Invalid decimal: overflow from too many digits"))
        );
    }

    #[test]
    fn from_str_overflow_3() {
        assert_eq!(
            Decimal::from_str("99999_99999_99999_99999_99999_99994"),
            Err(Error::from("Invalid decimal: overflow from too many digits"))
        );
    }

    #[test]
    fn from_str_overflow_4() {
        assert_eq!(
            Decimal::from_str("99999_99999_99999_99999_99999_999.99").unwrap(),
            Decimal::from_i128_with_scale(10_000_000_000_000_000_000_000_000_000, 0)
        );
    }

    #[test]
    fn from_str_edge_cases_1() {
        assert_eq!(Decimal::from_str(""), Err(Error::from("Invalid decimal: empty")));
    }

    #[test]
    fn from_str_edge_cases_2() {
        assert_eq!(
            Decimal::from_str("0.1."),
            Err(Error::from("Invalid decimal: two decimal points"))
        );
    }

    #[test]
    fn from_str_edge_cases_3() {
        assert_eq!(
            Decimal::from_str("_"),
            Err(Error::from("Invalid decimal: must start lead with a number"))
        );
    }

    #[test]
    fn from_str_edge_cases_4() {
        assert_eq!(
            Decimal::from_str("1?2"),
            Err(Error::from("Invalid decimal: unknown character"))
        );
    }

    #[test]
    fn from_str_edge_cases_5() {
        assert_eq!(
            Decimal::from_str("."),
            Err(Error::from("Invalid decimal: no digits found"))
        );
    }
}
