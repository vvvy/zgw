use std::str::Bytes;
use std::char;

use std::error::Error as StdError;
use std::fmt;

/// Tries to decode an utf-8 uri escape. bytes are added until try succeeds.
/// p is accumulated code point. b is current byte. r is remaining byte count
/// see [http://unicode.mayastudios.com/examples/utf8.html]
#[derive(Copy, Clone, Debug)]
struct UriEscapeDecoder { p: u32, b: u8, r: u8 }

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DErr {
    InvalidHexDigit(u8),
    InvalidUtf8LeaderByte(u8),
    InvalidUtf8SeqByte(u8),
    InvalidShift(u8),
    InvalidCodepoint(u32)
}

#[derive(Debug)]
enum DResult {
    Ok(char),
    D(UriEscapeDecoder),
    Error(DErr)
}

impl DResult {
    #[cfg(test)]
    fn new() -> DResult { DResult::D(UriEscapeDecoder::new()) }

    fn from_option(v: Option<UriEscapeDecoder>, err: DErr) -> Self {
        match v {
            Some(u) => DResult::D(u),
            None => DResult::Error(err)
        }
    }
    fn from_char_option(v: Option<char>, err: DErr) -> Self {
        match v {
            Some(c) => DResult::Ok(c),
            None => DResult::Error(err)
        }
    }

    fn and_then<F>(self, f: F) -> DResult where F: FnOnce(UriEscapeDecoder) -> DResult {
        match self {
            DResult::D(d) => f(d),
            o => o
        }
    }

    //fn d(self, x: u8) -> DResult { self.and_then(|d| d.d(x)) }
    //fn d_shift(self, x: u8) -> DResult { self.d(x).shift() }
    //fn shift(self) -> DResult { self.and_then(|d| d.shift()) }

    fn try_convert(self) -> DResult { self.and_then(|d| d.try_convert()) }

    #[cfg(test)]
    fn to_option(&self) -> Option<char> {
        match self {
            &DResult::Ok(ch) => Some(ch),
            _ => None
        }
    }
}

impl UriEscapeDecoder {
    fn new() -> UriEscapeDecoder { UriEscapeDecoder { p: 0, b: 0, r: 4 } }

    fn d(self, x: u8) -> DResult {
        DResult::from_option(
            (x as char).to_digit(16)
                .map(|u| { UriEscapeDecoder { b: (self.b << 4) | (u as u8), ..self } }),
            DErr::InvalidHexDigit(x)
        )
    }

    fn shift(self) -> DResult {
        match self.r {
            4 =>
                if self.b & 0b1_0000000 == 0 {
                    DResult::D(UriEscapeDecoder { p: self.b as u32, b: 0, r: 0})
                } else if self.b & 0b111_00000 == 0b110_00000 {
                    DResult::D(UriEscapeDecoder { p: (self.b & 0b000_11111) as u32, b: 0, r: 1})
                } else if self.b & 0b1111_0000 == 0b1110_0000 {
                    DResult::D(UriEscapeDecoder { p: (self.b & 0b0000_1111) as u32, b: 0, r: 2})
                } else if self.b & 0b11111_000 == 0b11110_000 {
                    DResult::D(UriEscapeDecoder { p: (self.b & 0b00000_111) as u32, b: 0, r: 3})
                } else { DResult::Error(DErr::InvalidUtf8LeaderByte(self.b)) },
            3 | 2 | 1 =>
                if self.b & 0b11_00_0000 == 0b10_00_0000 {
                    DResult::D(UriEscapeDecoder { p: (self.p << 6) | ((self.b & 0b00_11_1111) as u32), b: 0, r: self.r - 1})
                } else { DResult::Error(DErr::InvalidUtf8SeqByte(self.b)) },
            _ => DResult::Error(DErr::InvalidShift(self.r))
        }
    }

    fn try_convert(self) -> DResult {
        if self.r == 0 {
            DResult::from_char_option(char::from_u32(self.p), DErr::InvalidCodepoint(self.p))
        } else {
            DResult::D(self)
        }
    }

    fn d_shift(self, x: u8) -> DResult {
        self.d(x).and_then(|v| v.shift())
    }

    #[cfg(test)]
    fn add_byte(&self, b: u8) -> DResult {
        match b {
            b'_' => self.shift(),
            b => self.d(b)
        }
    }
}

#[test]
fn test_uri_escape_decoder() {
    #[inline]
    fn feed(v: Vec<u8>) -> Option<char> {
        v.into_iter().fold(DResult::new(),
                           |r, b| r.and_then(|ued| ued.add_byte(b))
        ).try_convert().to_option()
    }

    assert_eq!(feed(vec![b'2', b'0', b'_']), Some(' '));
    assert_eq!(feed(vec![b'd', b'0', b'_', b'a', b'b', b'_']), Some('Ы'));
    assert_eq!(
        feed(vec![b'f', b'0', b'_', b'9', b'f', b'_', b'9', b'8', b'_', b'8', b'0', b'_']),
        Some('\u{1F600}')
    );
}

/// Scanner Error
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SErr {
    DErr(DErr),
    Unknown
}

impl fmt::Display for SErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SErr::DErr(DErr::InvalidHexDigit(v)) => write!(f, "InvalidHexDigit({})", v),
            SErr::DErr(DErr::InvalidUtf8LeaderByte(v)) => write!(f, "InvalidUtf8LeaderByte({})", v),
            SErr::DErr(DErr::InvalidUtf8SeqByte(v)) => write!(f, "InvalidUtf8SeqByte({})", v),
            SErr::DErr(DErr::InvalidShift(v)) => write!(f, "InvalidShift({})", v),
            SErr::DErr(DErr::InvalidCodepoint(v)) => write!(f, "InvalidCodepoint({})", v),
            SErr::Unknown => f.write_str("unknown")
            //ref e => f.write_str(e.description())
        }
    }
}

impl StdError for SErr {
    fn description(&self) -> &str {
        match *self {
            SErr::DErr(DErr::InvalidHexDigit(_)) => "InvalidHexDigit",
            SErr::DErr(DErr::InvalidUtf8LeaderByte(_)) => "InvalidUtf8LeaderByte",
            SErr::DErr(DErr::InvalidUtf8SeqByte(_)) => "InvalidUtf8SeqByte",
            SErr::DErr(DErr::InvalidShift(_)) => "InvalidShift",
            SErr::DErr(DErr::InvalidCodepoint(_)) => "InvalidCodepoint",
            SErr::Unknown => "unknown"
            //ref e => f.write_str(e.description())
        }
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}

/// Scanner return value
#[derive(Debug, Eq, PartialEq)]
pub enum E { CH(char), ST, ERR(SErr) }

//scanner state
#[derive(Copy, Clone)]
enum S { CH, ST, E(UriEscapeDecoder), E0(UriEscapeDecoder), E1(UriEscapeDecoder),  END }

/// Scans the URI, returning either `E::ST` for exact '/' or `E::CH(char)` for every other char.
/// '%'-escapes are decoded and returned via `E::CH`. A terminal '/', if missing, is inserted.
pub struct Scanner<'s> { bs: Bytes<'s>, s: S }

impl<'s> Scanner<'s> {
    pub fn new(uri: &'s str) -> Scanner<'s> { Scanner { bs: uri.bytes(), s: S::CH } }
}

macro_rules! return_value(
        { $value:expr } => { Some(Some($value)) }
    );
macro_rules! return_end(
        { } => { Some(None) }
    );
macro_rules! cont(
        { } => { None }
    );

impl<'s> Iterator for Scanner<'s> {
    type Item = E;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (s, rvo) = match (self.s, self.bs.next()) {
                (S::CH, Some(b'/')) | (S::ST, Some(b'/')) => (S::ST, return_value!(E::ST)),
                (S::ST, None) => (S::END, return_end!()),

                (S::CH, Some(b'%')) | (S::ST, Some(b'%')) => (S::E0(UriEscapeDecoder::new()), cont!()),
                (S::E(d), Some(b'%')) => (S::E0(d), cont!()),
                (S::E0(d), Some(xd)) => match d.d(xd) {
                    DResult::D(d1) => (S::E1(d1), cont!()),
                    DResult::Error(derr) => (S::END, return_value!(E::ERR(SErr::DErr(derr)))),
                    _ => (S::END, return_value!(E::ERR(SErr::Unknown)))
                },
                (S::E1(d), Some(xd)) => match d.d_shift(xd).try_convert() {
                    DResult::D(d1) => (S::E(d1), cont!()),
                    DResult::Error(derr) => (S::END, return_value!(E::ERR(SErr::DErr(derr)))),
                    DResult::Ok(ch) => (S::CH, return_value!(E::CH(ch)))
                },
                (S::CH, Some(b)) | (S::ST, Some(b)) => (S::CH, return_value!(E::CH(b as char))),
                (S::CH, None) => (S::END, return_value!(E::ST)),

                (S::END, _) => (S::END, return_end!()),
                _ => (S::END, return_value!(E::ERR(SErr::Unknown)))
            };
            self.s = s;
            if let Some(rv) = rvo { break rv; }
        }
    }
}

#[test]
fn test_uri_scanner() {
    fn v(p: &str) -> Vec<E> { Scanner::new(p).collect() }
    assert_eq!(v("/"), vec![E::ST]);
    assert_eq!(v("/a"), vec![E::ST, E::CH('a'), E::ST]);
    assert_eq!(v("/a/"), vec![E::ST, E::CH('a'), E::ST]);
    assert_eq!(v("/a/b"), vec![E::ST, E::CH('a'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/aA/b/"), vec![E::ST, E::CH('a'), E::CH('A'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("//"), vec![E::ST, E::ST]);

    assert_eq!(v("/a%20/b/"), vec![E::ST, E::CH('a'), E::CH(' '), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/a%20_/b/"), vec![E::ST, E::CH('a'), E::CH(' '), E::CH('_'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/%20_/b/"), vec![E::ST, E::CH(' '), E::CH('_'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/%20%2F_/b/"), vec![E::ST, E::CH(' '), E::CH('/'), E::CH('_'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/%2F/b/"), vec![E::ST, E::CH('/'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/w%D0%A8/b/"), vec![E::ST, E::CH('w'), E::CH('Ш'), E::ST, E::CH('b'), E::ST]);
    assert_eq!(v("/w%D0%A8/"), vec![E::ST, E::CH('w'), E::CH('Ш'), E::ST]);
    assert_eq!(v("/w%D0%A8"), vec![E::ST, E::CH('w'), E::CH('Ш'), E::ST]);

    assert_eq!(v("s1/s2a"), vec![E::CH('s'), E::CH('1'), E::ST, E::CH('s'), E::CH('2'), E::CH('a'), E::ST]);
}

