use phf::phf_map;

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'s> {
    // single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier(&'s str),
    String(&'s str),
    Number(f64),

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "and" => Token::And,
    "class" => Token::Class,
    "else" => Token::Else,
    "false" => Token::False,
    "fun" => Token::Fun,
    "for" => Token::For,
    "if" => Token::If,
    "nil" => Token::Nil,
    "or" => Token::Or,
    "print" => Token::Print,
    "return" => Token::Return,
    "super" => Token::Super,
    "this" => Token::This,
    "true" => Token::True,
    "var" => Token::Var,
    "while" => Token::While,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UnterminatedStringLiteral,
    UnexpectedInput,
}

pub struct Lexer<'s> {
    rest: &'s str,
    finished: bool,
}

fn is_valid_identifier(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Result<Token<'s>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        loop {
            self.eat_whitespace();
            if !self.skip_comments() {
                break;
            }
        }

        if self.rest.is_empty() {
            self.finished = true;
            return Some(Ok(Token::Eof));
        }

        let mut chars = self.rest.chars();
        let c = chars.next().expect("rest is not empty");

        if let Some((token, consumed)) = match c {
            // single-character tokens
            '(' => Some((Token::LeftParen, c.len_utf8())),
            ')' => Some((Token::RightParen, c.len_utf8())),
            '{' => Some((Token::LeftBrace, c.len_utf8())),
            '}' => Some((Token::RightBrace, c.len_utf8())),
            ',' => Some((Token::Comma, c.len_utf8())),
            '.' => Some((Token::Dot, c.len_utf8())),
            '-' => Some((Token::Minus, c.len_utf8())),
            '+' => Some((Token::Plus, c.len_utf8())),
            ';' => Some((Token::Semicolon, c.len_utf8())),
            '*' => Some((Token::Star, c.len_utf8())),
            '/' => Some((Token::Slash, c.len_utf8())),

            // one or two character tokens
            '!' => match chars.next() {
                Some('=') => Some((Token::BangEqual, c.len_utf8() + '='.len_utf8())),
                _ => Some((Token::Bang, c.len_utf8())),
            },
            '=' => match chars.next() {
                Some('=') => Some((Token::EqualEqual, c.len_utf8() + '='.len_utf8())),
                _ => Some((Token::Equal, c.len_utf8())),
            },
            '>' => match chars.next() {
                Some('=') => Some((Token::GreaterEqual, c.len_utf8() + '='.len_utf8())),
                _ => Some((Token::Greater, c.len_utf8())),
            },
            '<' => match chars.next() {
                Some('=') => Some((Token::LessEqual, c.len_utf8() + '='.len_utf8())),
                _ => Some((Token::Less, c.len_utf8())),
            },

            // string literals
            '"' => match &self.rest['"'.len_utf8()..].find('"') {
                Some(end) => {
                    let literal = &self.rest['"'.len_utf8()..end + '"'.len_utf8()];
                    if literal.contains('\n') {
                        self.finished = true;
                        return Some(Err(Error::UnterminatedStringLiteral));
                    }
                    Some((Token::String(literal), end + 2 * '"'.len_utf8()))
                }
                None => {
                    self.finished = true;
                    return Some(Err(Error::UnterminatedStringLiteral));
                }
            },

            // number literals
            '0'..='9' => {
                let mut end = self
                    .rest
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(self.rest.len());

                // &self.rest[..end] is the integral part and is guaranteed to contain
                // nothing but digits
                //
                // check for a . and a fractional part
                let rest = &self.rest[end..];
                if rest.starts_with('.') {
                    // we _might_ have a fractional part
                    // collect the next digit characters
                    let rest = &rest['.'.len_utf8()..];
                    let end_of_fractional_part = rest
                        .find(|c: char| !c.is_ascii_digit())
                        .unwrap_or(rest.len());
                    if end_of_fractional_part > 0 {
                        end += end_of_fractional_part + '.'.len_utf8();
                    }
                }

                let literal = &self.rest[..end];
                let literal = literal
                    .parse::<f64>()
                    .expect("we know that literal contains only digits");
                Some((Token::Number(literal), end))
            }

            // keyword or identifier
            _ if is_valid_identifier(c) => {
                let end = self
                    .rest
                    .find(|c: char| !(is_valid_identifier(c) || c.is_ascii_digit()))
                    .unwrap_or(self.rest.len());
                let identifier = &self.rest[..end];
                match KEYWORDS.get(identifier).cloned() {
                    Some(t) => Some((t, end)),
                    None => Some((Token::Identifier(identifier), end)),
                }
            }

            _ => {
                self.finished = true;
                return Some(Err(Error::UnexpectedInput));
            }
        } {
            self.rest = &self.rest[consumed..];
            Some(Ok(token))
        } else {
            unreachable!()
        }
    }
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        Lexer {
            rest: source,
            finished: false,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        if self.rest.starts_with(|c: char| c.is_ascii_whitespace()) {
            self.rest = self
                .rest
                .trim_start_matches(|c: char| c.is_ascii_whitespace());
            true
        } else {
            false
        }
    }

    fn skip_comments(&mut self) -> bool {
        if self.rest.starts_with("//") {
            if let Some(index) = self.rest.find('\n') {
                self.rest = &self.rest[index..];
            } else {
                self.rest = &self.rest[self.rest.len()..];
            }
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_tokenizes_an_empty_program() {
        let source = "";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_eat_whitespace() {
        let source = "  Not whitespace";
        let mut lexer = Lexer::new(source);
        lexer.eat_whitespace();
        assert!(lexer.rest.starts_with("Not whitespace"));
    }

    #[test]
    fn it_tokenizes_an_empty_program_but_with_whitespace() {
        let source = "   \n \t  \r\n  \n";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_single_character_tokens() {
        let source = "(){},.-+;*";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::LeftBrace)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightBrace)));
        assert_eq!(lexer.next(), Some(Ok(Token::Comma)));
        assert_eq!(lexer.next(), Some(Ok(Token::Dot)));
        assert_eq!(lexer.next(), Some(Ok(Token::Minus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lexer.next(), Some(Ok(Token::Star)));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_single_and_double_character_tokens() {
        let source = "! != = == > >= < <=";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Bang)));
        assert_eq!(lexer.next(), Some(Ok(Token::BangEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Equal)));
        assert_eq!(lexer.next(), Some(Ok(Token::EqualEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Greater)));
        assert_eq!(lexer.next(), Some(Ok(Token::GreaterEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Less)));
        assert_eq!(lexer.next(), Some(Ok(Token::LessEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_ignores_comments() {
        let source = "// This is a comment\n/ // But on the next line, there's a slash";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Slash)));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_string_literals() {
        let source = "\"Hello World!\"";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::String("Hello World!"))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_fails_on_unterminated_string_literals() {
        let source = "\"Hello World!";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Err(Error::UnterminatedStringLiteral)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_fails_on_multiline_string_literals() {
        let source = "\"Hello\nWorld!\"";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Err(Error::UnterminatedStringLiteral)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_really_tokenizes_string_literals() {
        let source = "\"Hello World!\" != \"Goodbye World!\"";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::String("Hello World!"))));
        assert_eq!(lexer.next(), Some(Ok(Token::BangEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::String("Goodbye World!"))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_integral_number_literals() {
        let source = "123+456";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Number(123.))));
        assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Number(456.))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);

        let source = "123. // this should be okay";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Number(123.))));
        assert_eq!(lexer.next(), Some(Ok(Token::Dot)));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_fractional_number_literals() {
        let source = "123.456";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Number(123.456))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_negative_number_literals() {
        let source = "-42";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Minus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Number(42.))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
    }

    #[test]
    fn it_tokenizes_identifiers() {
        let source = "foo bar baz";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("foo"))));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("bar"))));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("baz"))));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_keywords() {
        let source =
            "and class else false fun for if nil or print return super this true var while";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), Some(Ok(Token::And)));
        assert_eq!(lexer.next(), Some(Ok(Token::Class)));
        assert_eq!(lexer.next(), Some(Ok(Token::Else)));
        assert_eq!(lexer.next(), Some(Ok(Token::False)));
        assert_eq!(lexer.next(), Some(Ok(Token::Fun)));
        assert_eq!(lexer.next(), Some(Ok(Token::For)));
        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::Nil)));
        assert_eq!(lexer.next(), Some(Ok(Token::Or)));
        assert_eq!(lexer.next(), Some(Ok(Token::Print)));
        assert_eq!(lexer.next(), Some(Ok(Token::Return)));
        assert_eq!(lexer.next(), Some(Ok(Token::Super)));
        assert_eq!(lexer.next(), Some(Ok(Token::This)));
        assert_eq!(lexer.next(), Some(Ok(Token::True)));
        assert_eq!(lexer.next(), Some(Ok(Token::Var)));
        assert_eq!(lexer.next(), Some(Ok(Token::While)));
        assert_eq!(lexer.next(), Some(Ok(Token::Eof)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_tokenizes_an_entire_program() {
        let source = "
            add + me;
            subtract - me;
            multiply * me;
            divide / me; 
        ";
        let mut lexer = Lexer::new(source);
        assert!(lexer.all(|t| t.is_ok()));
    }
}
