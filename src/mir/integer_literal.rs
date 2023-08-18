#[derive(Debug)]
pub enum ParseError {
    InvalidCharacter,
    Overflow,
}

enum State {
    Start,
    Radix,
    Binary,
    Octal,
    Decimal,
    Hex,
}

pub fn parse_int(source: &str) -> Result<u64, ParseError> {
    let mut state = State::Start;
    let mut value: u64 = 0;

    for c in source.chars() {
        let val = (c as u8) - ('0' as u8);
        let val = val as u64;
        match state {
            State::Start => match c {
                '0' => state = State::Radix,
                '1'..='9' => {
                    state = State::Decimal;
                    value = val;
                },
                _ => return Err(ParseError::InvalidCharacter),
            },
            State::Radix => match c {
                'b' => state = State::Binary,
                'o' => state = State::Octal,
                'x' => state = State::Hex,
                '0'..='9' => {
                    state = State::Decimal;
                    value = val;
                },
                _ => return Err(ParseError::InvalidCharacter),
            },
            State::Binary => match c {
                '0'..='1' => value = (value << 1) | (val & 1),
                _ => break,
            },
            State::Octal => match c {
                '0'..='7' => value = (value << 3) | (val & 7),
                _ => break,
            },
            State::Hex => match c {
                '0'..='9' => value = (value << 4) | (val & 0xF),
                'a'..='f' => {
                    let digit = ((c as u8) - ('a' as u8) + 10) & 0xF;
                    value = (value << 4) | (digit as u64);
                },
                'A'..='F' => {
                    let digit = ((c as u8) - ('A' as u8) + 10) & 0xF;
                    value = (value << 4) | (digit as u64);
                },
                _ => return Err(ParseError::InvalidCharacter),
            },
            State::Decimal => match c{
                '0'..='9' => value = (value * 10) + val,
                _ => return Err(ParseError::InvalidCharacter),
            },
        }
    }

    Ok(value)
}

#[cfg(test)]
mod test {
    use crate::mir::integer_literal::parse_int;

    fn test_parse_int(source: &str, value: u64) {
        assert_eq!(value, parse_int(source).unwrap());
    }

    #[test]
    fn literal_parsing() {
        test_parse_int("0", 0);
        test_parse_int("1", 1);
        test_parse_int("123", 123);
        test_parse_int("0123", 123);
        test_parse_int("00123", 123);
        test_parse_int("123456789", 123456789);
        test_parse_int("0b0", 0b0);
        test_parse_int("0b1", 0b1);
        test_parse_int("0b01", 0b01);
        test_parse_int("0b101", 0b101);
        test_parse_int("0o0", 0o0);
        test_parse_int("0o1", 0o1);
        test_parse_int("0x0", 0x0);
        test_parse_int("0x1", 0x1);
        test_parse_int("0x123456", 0x123456);
        test_parse_int("0x789A", 0x789A);
        test_parse_int("0x789a", 0x789A);
        test_parse_int("0xABCDEF", 0xABCDEF);
        test_parse_int("0xabcdef", 0xABCDEF);
    }
}
