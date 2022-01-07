use crate::{
    constants::{MAX_PRECISION, OVERFLOW_U96},
    error::Error,
    Decimal,
};

// dedicated implementation for the most common case.
pub fn parse_str_radix_10(str: &str) -> Result<Decimal, crate::Error> {
    if str.is_empty() {
        return Err(Error::from("Invalid decimal: empty"));
    }

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
    let mut coeff: u8 = 0; // number of digits
    let mut scale: u8 = 0;

    let mut data64: u64 = 0;

    while data64 < (u64::MAX / 10 - u8::MAX as u64) {
        // no overflow can happen here!
        if !bytes.is_empty() {
            let b = bytes[0];
            match b {
                b'0'..=b'9' => {
                    let digit = u32::from(b - b'0');
                    has_digit = true;
                    coeff += if coeff == 0 && digit == 0 { 0 } else { 1 }; // got one more digit

                    // we have already validated that we cannot overflow
                    data64 = data64 * 10 + digit as u64;
                    scale += passed_decimal_point as u8;
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

            debug_assert!(coeff < MAX_PRECISION as u8);
            if scale >= 28 {
                if !bytes.is_empty() {
                    return maybe_round(data64 as u128, bytes[0], scale, negative, passed_decimal_point);
                }
                break;
            }
        } else {
            break;
        }
    }

    let data: u128 = data64 as u128;

    if !bytes.is_empty() {
        return handle_full_128(data, bytes, scale, coeff, passed_decimal_point, negative);
    } else if !has_digit {
        return Err(Error::from("Invalid decimal: no digits found"));
    }

    handle_data(data, scale, negative)
}

#[inline(never)]
#[cold]
fn handle_full_128(
    mut data: u128,
    mut bytes: &[u8],
    mut scale: u8,
    mut coeff: u8,
    mut passed_decimal_point: bool,
    negative: bool,
) -> Result<Decimal, crate::Error> {
    while !bytes.is_empty() {
        debug_assert!(coeff > 0);
        let b = bytes[0];
        match b {
            b'0'..=b'9' => {
                let digit = u32::from(b - b'0');
                coeff += 1; // got one more digit

                // If the data is going to overflow then we should go into recovery mode
                let next = data * 10;
                if overflows(next) {
                    // This means that we have more data to process, that we're not sure what to do with.
                    // This may or may not be an issue - depending on whether we're past a decimal point
                    // or not.
                    if !passed_decimal_point {
                        return Err(Error::from("Invalid decimal: overflow from too many digits"));
                    }

                    if digit >= 5 {
                        data += 1;
                        if overflows(data) {
                            // Highly unlikely scenario which is more indicative of a bug
                            return Err(Error::from("Invalid decimal: overflow when rounding"));
                        }
                    }
                    break;
                } else {
                    data = next + digit as u128;
                    scale += passed_decimal_point as u8;
                    if overflows(data) {
                        // Highly unlikely scenario which is more indicative of a bug
                        return Err(Error::from("Invalid decimal: overflow when rounding"));
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

        if coeff > MAX_PRECISION as u8 || scale >= 28 {
            if !bytes.is_empty() {
                return maybe_round(data, bytes[0], scale, negative, passed_decimal_point);
            }
            break;
        }
    }

    handle_data(data, scale, negative)
}

#[inline(never)]
#[cold]
fn maybe_round(
    mut data: u128,
    next_byte: u8,
    scale: u8,
    negative: bool,
    passed_decimal_point: bool,
) -> Result<Decimal, crate::Error> {
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
        data += 1;
        if overflows(data) {
            // Highly unlikely scenario which is more indicative of a bug
            return Err(Error::from("Invalid decimal: overflow when rounding"));
        }
    }

    handle_data(data, scale, negative)
}

fn handle_data(data: u128, scale: u8, negative: bool) -> Result<Decimal, crate::Error> {
    debug_assert_eq!(data >> 96, 0);

    let data = [data as u32, (data >> 32) as u32, (data >> 64) as u32];

    Ok(Decimal::from_parts(data[0], data[1], data[2], negative, scale as u32))
}

#[inline]
pub fn overflows(val: u128) -> bool {
    val >= OVERFLOW_U96
}
