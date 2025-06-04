use logos::Logos;

#[derive(Clone, Debug, Logos, PartialEq)]
pub enum Token {
    // Single-character tokens.
    #[token("(")]
    LtParen,
    #[token(")")]
    RtParen,
    #[token("{")]
    LtBrace,
    #[token("}")]
    RtBrace,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,
    #[token("/")]
    Slash,
    #[token("*")]
    Asterisk,

    // One or two character tokens.
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    // Literals.
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r#""[^"]*""#, |lex| lex.slice().to_string())]
    String(String),
    #[regex(r#"[0-9]+(\.[0-9]+)?"#, |lex| lex.slice().parse().map_err(|_| ()))]
    Number(f64),

    // Keywords.
    #[token("and")]
    And,
    #[token("class")]
    Class,
    #[token("else")]
    Else,
    #[token("false")]
    False,
    #[token("for")]
    For,
    #[token("fun")]
    Fun,
    #[token("if")]
    If,
    #[token("nil")]
    Nil,
    #[token("or")]
    Or,
    #[token("print")]
    Print,
    #[token("return")]
    Return,
    #[token("super")]
    Super,
    #[token("this")]
    This,
    #[token("true")]
    True,
    #[token("var")]
    Var,
    #[token("while")]
    While,

    #[regex(r"//.*", logos::skip)]
    #[regex(r"[ \r\n\t\f]+", logos::skip)]
    __Skip,
}

use std::fmt;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Single-character tokens
            Token::LtParen => write!(f, "("),
            Token::RtParen => write!(f, ")"),
            Token::LtBrace => write!(f, "{{"),
            Token::RtBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Slash => write!(f, "/"),
            Token::Asterisk => write!(f, "*"),

            // One or two character tokens
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),

            // Literals
            Token::Identifier(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Number(n) => write!(f, "{}", n),

            // Keywords
            Token::And => write!(f, "and"),
            Token::Class => write!(f, "class"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::For => write!(f, "for"),
            Token::Fun => write!(f, "fun"),
            Token::If => write!(f, "if"),
            Token::Nil => write!(f, "nil"),
            Token::Or => write!(f, "or"),
            Token::Print => write!(f, "print"),
            Token::Return => write!(f, "return"),
            Token::Super => write!(f, "super"),
            Token::This => write!(f, "this"),
            Token::True => write!(f, "true"),
            Token::Var => write!(f, "var"),
            Token::While => write!(f, "while"),
            Token::__Skip => write!(f, ""),
        }
    }
}

/// 对源代码进行词法分析，返回 token 列表
pub fn tokenize(source: &str) -> Vec<Token> {
    let mut lex = Token::lexer(source);
    std::iter::from_fn(|| {
        match lex.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(())) => {
                let span = lex.span();
                let ch = source.chars().nth(span.start).unwrap_or('?');
                crate::report_error!(crate::error::LoxError::UnexpectedCharacter(ch));
            },
            None => None,
        }
    }).collect()
}