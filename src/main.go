package main

import (
	"fmt"
	//"bufio"
	//"io"
	"io/ioutil"
	//"os"
	//"strings"
	"strings"
)

type TokenType string
type Token struct {
	TokenType
	Offset int
	Length int
}
type Tokens []Token

type Tokenizer struct {
	tokens Tokens
	chars []rune
	index int
}

type NodeType string
type Node struct {
	NodeType
	children []Node
}
type Parser struct {
	nodes []Node
	tokens Tokens
	index int
}

const (
	DEF TokenType = "DEF"// def
	AS TokenType = "AS"// as

	FUNC TokenType = "FUNC"// func
	STRUCT TokenType = "STRUCT"// struct
	INT TokenType = "INT"// int
	DOUBLE TokenType = "DOUBLE"// double
	BOOLEAN TokenType = "BOOLEAN"// boolean
	STRING TokenType = "STRING"// string

	TRUE TokenType = "TRUE"// true
	FALSE TokenType = "FALSE"// false

	ASSIGN TokenType = "ASSIGN"// =

	ADD TokenType = "ADD"// +
	SUB TokenType = "SUB"// -
	MUL TokenType = "MUL"// *
	DIV TokenType = "DIV"// /
	MOD TokenType = "MOD"// %

	BIT_AND TokenType = "BIT_AND"// &
	BIT_OR TokenType = "BIT_OR"// |
	BIT_SHIFT_R TokenType = "BIT_SHIFT_R"// >>
	BIT_SHIFT_L TokenType = "BIT_SHIFT_L"// <<

	LOGIC_AND TokenType = "LOGIC_AND"// &&
	LOGIC_OR TokenType = "LOGIC_OR"// ||
	LOGIC_NOT TokenType = "LOGIC_NOT"// !

	IF TokenType = "IF"// if
	ELSE TokenType = "ELSE"// else
	SWITCH TokenType = "SWITCH"// switch
	FOR TokenType = "FOR"// for
	WHILE TokenType = "WHILE"// while

	EQ TokenType = "EQ"// ==
	NEQ TokenType = "NEQ"// !=
	GT TokenType = "GT"// >
	GTE TokenType = "GTE"// >=
	LT TokenType = "LT"// <
	LTE TokenType = "LTE"// <=

	ARROW_R TokenType = "ARROW_R"// ->

	PAREN_L TokenType = "PAREN_L"// (
	PAREN_R TokenType = "PAREN_R"// )
	BRACE_L TokenType = "BRACE_L"// {
	BRACE_R TokenType = "BRACE_R"// }
	BRACKET_L TokenType = "BRACKET_L"// [
	BRACKET_R TokenType = "BRACKET_R"// ]

	DOUBLE_QUOTE TokenType = "DOUBLE_QUOTE"// "
	SINGLE_QUOTE TokenType = "SINGLE_QUOTE"// '

	PERIOD TokenType = "PERIOD"// .
	COMMA TokenType = "COMMA"// ,

	COLON TokenType = "COLON"// :
	SEMI_COLON TokenType = "SEMI_COLON"// ;

	RETURN TokenType = "RETURN"// return

	LITERAL_IDENTIFIER TokenType = "LITERAL_IDENTIFIER"
	LITERAL_INTEGER TokenType = "LITERAL_INTEGER"
	LITERAL_REAL TokenType = "LITERAL_REAL"
	LITERAL_STRING TokenType = "LITERAL_STRING"

	ANNOTATION_LINE TokenType = "ANNOTATION_LINE"
	ANNOTATION_BLOCK TokenType = "ANNOTATION_BLOCK"
)

const (
	STMTS NodeType = "STMTS"
	STMT NodeType = "STMT"
	STMT_EXPR NodeType = "STMT_EXPR"
	STMT_IF NodeType = "STMT_IF"
	STMT_FOR NodeType = "STMT_FOR"
	STMT_BLOCK NodeType = "STMT_BLOCK"

	EXPR NodeType = "EXPR"
	EXPR_DEFINE NodeType = "EXPR_DEFINE"
	EXPR_ASSIGN NodeType = "EXPR_ASSIGN"
	EXPR_CALL_FUNC NodeType = "EXPR_CALL_FUNC"
	EXPR_REF_DOT NodeType = "EXPR_REF_DOT"
	EXPR_BIT_AND NodeType = "EXPR_BIT_AND"
	EXPR_BIT_OR NodeType = "EXPR_BIT_OR"
	EXPR_MUL NodeType = "EXPR_MUL"
	EXPR_DIV NodeType = "EXPR_DIV"
	EXPR_MOD NodeType = "EXPR_MOD"
	EXPR_ADD NodeType = "EXPR_ADD"
	EXPR_SUB NodeType = "EXPR_SUB"
	EXPR_EQ NodeType = "EXPR_EQ"
	EXPR_NEQ NodeType = "EXPR_NEQ"
	EXPR_GT NodeType = "EXPR_GT"
	EXPR_GTE NodeType = "EXPR_GTE"
	EXPR_LT NodeType = "EXPR_LT"
	EXPR_LTE NodeType = "EXPR_LTE"
	EXPR_LOGIC_AND NodeType = "EXPR_LOGIC_AND"
	EXPR_LOGIC_OR NodeType = "EXPR_LOGIC_OR"
	EXPR_LOGIC_NOT NodeType = "EXPR_LOGIC_NOT"
)

var rules = map[NodeType][][]interface{} {
	STMTS: {{STMT}, {STMT, STMTS}},
	STMT: {{STMT_EXPR}, {STMT_IF}, {STMT_FOR}, {STMT_BLOCK}},
	STMT_EXPR: {{EXPR, SEMI_COLON}},
	STMT_IF: {{IF, PAREN_L, EXPR, PAREN_R, STMT_BLOCK}},
	STMT_FOR: {{FOR, PAREN_L, EXPR, SEMI_COLON, EXPR, SEMI_COLON, EXPR, PAREN_R, STMT_BLOCK}},
	STMT_BLOCK: {{STMT}, {BRACE_L, STMTS, BRACE_R}},

	EXPR: {{EXPR_DEFINE}, {EXPR_ASSIGN}, {EXPR_CALL_FUNC}, {EXPR_REF_DOT}, {EXPR_BIT_AND}, {EXPR_BIT_OR},
		{EXPR_MUL}, {EXPR_DIV}, {EXPR_MOD}, {EXPR_ADD}, {EXPR_SUB},
		{EXPR_EQ}, {EXPR_NEQ}, {EXPR_GT}, {EXPR_GTE}, {EXPR_LT}, {EXPR_LTE},
		{EXPR_LOGIC_AND}, {EXPR_LOGIC_OR}, {EXPR_LOGIC_NOT}},
	EXPR_DEFINE: {{DEF, LITERAL_IDENTIFIER}, {DEF, LITERAL_IDENTIFIER, AS}},
	EXPR_ASSIGN: {{LITERAL_IDENTIFIER, ASSIGN, EXPR}},
	EXPR_CALL_FUNC: {{LITERAL_IDENTIFIER, PAREN_L, PAREN_R}, {LITERAL_IDENTIFIER, PAREN_L, PAREN_R}},
	EXPR_REF_DOT: {{EXPR, PERIOD, EXPR}},
	EXPR_BIT_AND: {{EXPR, BIT_AND, EXPR}},
	EXPR_BIT_OR: {{EXPR, BIT_OR, EXPR}},
	EXPR_MUL: {{EXPR, MUL, EXPR}},
	EXPR_DIV: {{EXPR, DIV, EXPR}},
	EXPR_MOD: {{EXPR, MOD, EXPR}},
	EXPR_ADD: {{EXPR, ADD, EXPR}},
	EXPR_SUB: {{EXPR, SUB, EXPR}},
	EXPR_EQ: {{}},
	EXPR_NEQ: {{}},
	EXPR_GT: {{}},
	EXPR_GTE: {{}},
	EXPR_LT: {{}},
	EXPR_LTE: {{}},
	EXPR_LOGIC_AND: {{}},
	EXPR_LOGIC_OR: {{}},
	EXPR_LOGIC_NOT: {{}},
}

func (tokenizer *Tokenizer) accept(str string, tokenType TokenType) bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if string((*chars)[*i:*i + len(str)]) == str {
		lastChar := []rune(str)[len(str) - 1]
		nextChar := (*chars)[*i + len(str)]

		if !(isAlphabet(lastChar) && isAlphabet(nextChar)) {
			tokens.addToken(Token{tokenType, *i, len(str)})
			*i += len(str)
			return true
		}
	}

	return false
}

func (tokenizer *Tokenizer) acceptNumber() bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if !isDigit((*chars)[*i]) { return false }

	from := *i

	c := (*chars)[*i]
	for isDigit(c) {
		*i++
		c = (*chars)[*i]
	}

	if c == '.' {
		*i++

		c = (*chars)[*i]
		for isDigit(c) {
			*i++
			c = (*chars)[*i]
		}

		tokens.addToken(Token{LITERAL_REAL, from, *i - from})
	} else {
		tokens.addToken(Token{LITERAL_INTEGER, from, *i - from})
	}

	return true
}

func (tokenizer *Tokenizer) acceptString() bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if (*chars)[*i] != '"' { return false }

	from := *i
	*i++

	for *i < len(*chars) && (*chars)[*i] != '"' {
		*i++
	}
	*i++

	tokens.addToken(Token{LITERAL_STRING, from, *i - from})

	return true
}

func (tokenizer *Tokenizer) acceptIdentifier() bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if !isAlphabet((*chars)[*i]) { return false }

	from := *i

	for c := (*chars)[*i]; isAlphabet(c) || isDigit(c) || c == 0x005f; {
		*i++
		c = (*chars)[*i]
	}

	tokens.addToken(Token{LITERAL_IDENTIFIER, from, *i - from})

	return true
}

func (tokenizer *Tokenizer) acceptAnnotationSingle() bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if (*chars)[*i] != '/' || (*chars)[*i + 1] != '/' { return false }
	from := *i

	for *i < len(*chars) && (*chars)[*i] != '\n' {
		*i++
	}

	tokens.addToken(Token{ANNOTATION_LINE, from, *i - from})

	return true
}

func (tokenizer *Tokenizer) acceptAnnotationBlock() bool {
	tokens := &tokenizer.tokens
	chars := &tokenizer.chars
	i := &tokenizer.index

	if (*chars)[*i] != '/' || (*chars)[*i + 1] != '*' { return false }
	from := *i

	for *i < len(tokenizer.chars) && !((*chars)[*i - 2] == '*' && (*chars)[*i - 1] == '/') {
		*i++
	}

	tokens.addToken(Token{ANNOTATION_BLOCK, from, *i - from})

	return true
}

func isAlphabet(c rune) bool {
	return (0x0041 <= c && c <= 0x005A) || (0x0061 <= c && c <= 0x007A);
}

func isDigit(c rune) bool {
	return (0x0030 <= c && c <= 0x0039);
}

func (tokenizer *Tokenizer) Tokenize() Tokens {
	chars := &tokenizer.chars
	i := &tokenizer.index

	for *i < len(*chars) {
		c := (*chars)[*i]

		if c == 'a' {
			if tokenizer.accept("as", AS) { continue }
		} else if c == 'b' {
			if tokenizer.accept("boolean", BOOLEAN) { continue }
		} else if c == 'd' {
			if tokenizer.accept("def", DEF) {
				continue
			} else if tokenizer.accept("double", DOUBLE) {
				continue
			}
		} else if c == 'e' {
			if tokenizer.accept("else", ELSE) { continue }
		} else if c == 'f' {
			if tokenizer.accept("for", FOR) {
				continue
			} else if tokenizer.accept("func", FUNC) {
				continue
			} else if tokenizer.accept("false", FALSE) {
				continue
			}
		} else if c == 'i' {
			if tokenizer.accept("if", IF) {
				continue
			} else if tokenizer.accept("int", INT) {
				continue
			}
		} else if c == 'r' {
			if tokenizer.accept("return", RETURN) {
				continue
			}
		} else if c == 's' {
			if tokenizer.accept("string", STRING) {
				continue
			} else if tokenizer.accept("struct", STRUCT) {
				continue
			} else if tokenizer.accept("switch", SWITCH) {
				continue
			}
		} else if c == 't' {
			if tokenizer.accept("true", TRUE) {
				continue
			}
		} else if c == 'w' {
			if tokenizer.accept("while", WHILE) {
				continue
			}
		} else if c == '+' {
			if tokenizer.accept("+", ADD) { continue }
		} else if c == '-' {
			if tokenizer.accept("->", ARROW_R) {
				continue
			} else if tokenizer.accept("-", SUB) {
				continue
			}
		} else if c == '*' {
			if tokenizer.accept("*", MUL) { continue }
		} else if c == '/' {
			if tokenizer.acceptAnnotationSingle() {
				continue
			} else if tokenizer.acceptAnnotationBlock() {
				continue
			} else if tokenizer.accept("/", DIV) {
				continue
			}
		} else if c == '%' {
			if tokenizer.accept("%", MOD) {
				continue
			}
		} else if c == '=' {
			if tokenizer.accept("==", EQ) {
				continue
			} else if tokenizer.accept("=", ASSIGN) {
				continue
			}
		} else if c == '!' {
			if tokenizer.accept("!=", NEQ) {
				continue
			} else if tokenizer.accept("!", LOGIC_NOT) {
				continue
			}
		} else if c == '>' {
			if tokenizer.accept(">>", BIT_SHIFT_R) {
				continue
			} else if tokenizer.accept(">=", GTE) {
				continue
			} else if tokenizer.accept(">", GT) {
				continue
			}
		} else if c == '<' {
			if tokenizer.accept("<<", BIT_SHIFT_L) {
				continue
			} else if tokenizer.accept("<=", LTE) {
				continue
			} else if tokenizer.accept("<", LT) {
				continue
			}
		} else if c == ':' {
			if tokenizer.accept(":", COLON) { continue }
		} else if c == ';' {
			if tokenizer.accept(";", SEMI_COLON) { continue }
		} else if c == '.' {
			if tokenizer.accept(".", PERIOD) { continue }
		} else if c == ',' {
			if tokenizer.accept(",", COMMA) { continue }
		} else if c == '&' {
			if tokenizer.accept("&&", LOGIC_AND) {
				continue
			} else if tokenizer.accept("&", BIT_AND) {
				continue
			}
		} else if c == '|' {
			if tokenizer.accept("||", LOGIC_OR) {
				continue
			} else if tokenizer.accept("|", BIT_OR) {
				continue
			}
		} else if c == '(' {
			if tokenizer.accept("(", PAREN_L) { continue }
		} else if c == ')' {
			if tokenizer.accept(")", PAREN_R) { continue }
		} else if c == '{' {
			if tokenizer.accept("{", BRACE_L) { continue }
		} else if c == '}' {
			if tokenizer.accept("}", BRACE_R) { continue }
		} else if c == '[' {
			if tokenizer.accept("[", BRACKET_L) { continue }
		} else if c == ']' {
			if tokenizer.accept("]", BRACKET_R) { continue }
		} else if c == '"' {
			if tokenizer.acceptString() {
				continue
			} else if tokenizer.accept("\"", DOUBLE_QUOTE) {
				continue
			}
		} else if c == '\'' {
			if tokenizer.accept("'", SINGLE_QUOTE) { continue }
		}

		if tokenizer.acceptNumber() {
			continue
		} else if tokenizer.acceptIdentifier() {
			continue
		} else {
			*i++
		}
	}

	return tokenizer.tokens
}

func (tokens *Tokens) addToken(token Token) {
	*tokens = append(*tokens, token)
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dat, err := ioutil.ReadFile("res/code.rljl")
	check(err)
	fmt.Println(string(dat))
	fmt.Println("======================================")

	tokenizer := Tokenizer{chars: []rune(string(dat))}
	tokens := tokenizer.Tokenize()



	for i, token := range tokens {
		str := string(tokenizer.chars[token.Offset:token.Offset + token.Length])
		str = strings.Replace(str, "\r", "", -1)
		fmt.Printf("%d\t%q\t\t@%d\t%s\n", i, str, token.Offset, token.TokenType)
	}

	for _, token := range tokens {
		str := string(tokenizer.chars[token.Offset:token.Offset + token.Length])
		str = strings.Replace(str, "\r", "", -1)
		fmt.Printf("%s ", str)
	}

	//f, err := os.Open("res/code.rljl")
	//check(err)
	//
	//b1 := make([]byte, 5)
	//n1, err := f.Read(b1)
	//check(err)
	//fmt.Printf("%d bytes: %s\n", n1, string(b1))
	//
	//o2, err := f.Seek(6, 0)
	//check(err)
	//b2 := make([]byte, 2)
	//n2, err := f.Read(b2)
	//check(err)
	//fmt.Printf("%d bytes @ %d: %s\n", n2, o2, string(b2))
	//
	//o3, err := f.Seek(6, 0)
	//check(err)
	//b3 := make([]byte, 4)
	//n3, err := io.ReadAtLeast(f, b3, 2)
	//check(err)
	//fmt.Printf("%d bytes @ %d: %s\n", n3, o3, string(b3))
	//
	//_, err = f.Seek(0, 0)
	//check(err)
	//
	//r4 := bufio.NewReader(f)
	//b4, err := r4.Peek(5)
	//check(err)
	//fmt.Printf("5 bytes: %s\n", string(b4))
	//b4, err = r4.Peek(5)
	//check(err)
	//fmt.Printf("5 bytes: %s\n", string(b4))
	//
	//f.Close()
}