use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    UnexpectedChar(char, Span),
    UnterminatedString(Span),
    InvalidNumber(String, Span),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar(c, span) => {
                write!(f, "Lexer Error at [{}]: Unexpected character '{}'", span, c)
            }
            LexError::UnterminatedString(span) => {
                write!(f, "Lexer Error at [{}]: Unterminated string literal", span)
            }
            LexError::InvalidNumber(num_str, span) => {
                write!(f, "Lexer Error at [{}]: Invalid number '{}'", span, num_str)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken(String, Span),
    UnexpectedEof,
    UnmatchedParen(Span),
    UnmatchedBracket(Span),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(tok, span) => {
                write!(f, "Parse Error at [{}]: Unexpected token '{}'", span, tok)
            }
            ParseError::UnexpectedEof => write!(f, "Parse Error: Unexpected end of file"),
            ParseError::UnmatchedParen(span) => {
                write!(f, "Parse Error at [{}]: Unmatched parenthesis", span)
            }
            ParseError::UnmatchedBracket(span) => {
                write!(f, "Parse Error at [{}]: Unmatched bracket", span)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    UnboundSymbol(String, Option<Span>),
    TypeMismatch {
        expected: String,
        got: String,
        span: Option<Span>,
    },
    ArityMismatch {
        expected: String,
        got: usize,
        span: Option<Span>,
    },
    Custom(String, Option<Span>),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnboundSymbol(sym, span) => {
                if let Some(s) = span {
                    write!(f, "Eval Error at [{}]: Unbound symbol '{}'", s, sym)
                } else {
                    write!(f, "Eval Error: Unbound symbol '{}'", sym)
                }
            }
            EvalError::TypeMismatch {
                expected,
                got,
                span,
            } => {
                if let Some(s) = span {
                    write!(
                        f,
                        "Eval Error at [{}]: Type mismatch, expected {}, got {}",
                        s, expected, got
                    )
                } else {
                    write!(
                        f,
                        "Eval Error: Type mismatch, expected {}, got {}",
                        expected, got
                    )
                }
            }
            EvalError::ArityMismatch {
                expected,
                got,
                span,
            } => {
                if let Some(s) = span {
                    write!(
                        f,
                        "Eval Error at [{}]: Wrong number of arguments, expected {}, got {}",
                        s, expected, got
                    )
                } else {
                    write!(
                        f,
                        "Eval Error: Wrong number of arguments, expected {}, got {}",
                        expected, got
                    )
                }
            }
            EvalError::Custom(msg, span) => {
                if let Some(s) = span {
                    write!(f, "Eval Error at [{}]: {}", s, msg)
                } else {
                    write!(f, "Eval Error: {}", msg)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuspError {
    Lex(LexError),
    Parse(ParseError),
    Eval(EvalError),
}

impl From<LexError> for RuspError {
    fn from(e: LexError) -> Self {
        RuspError::Lex(e)
    }
}

impl From<ParseError> for RuspError {
    fn from(e: ParseError) -> Self {
        RuspError::Parse(e)
    }
}

impl From<EvalError> for RuspError {
    fn from(e: EvalError) -> Self {
        RuspError::Eval(e)
    }
}

impl std::error::Error for RuspError {}

impl fmt::Display for RuspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuspError::Lex(e) => write!(f, "{}", e),
            RuspError::Parse(e) => write!(f, "{}", e),
            RuspError::Eval(e) => write!(f, "{}", e),
        }
    }
}

pub type RuspResult<T> = Result<T, RuspError>;
