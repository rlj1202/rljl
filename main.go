package main

import (
	"fmt"
	"io/ioutil"
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
	STMTS TokenType = "STMTS"
	STMT TokenType = "STMT"
	STMT_EXPR TokenType = "STMT_EXPR"
	STMT_IF TokenType = "STMT_IF"
	STMT_FOR TokenType = "STMT_FOR"
	STMT_BLOCK TokenType = "STMT_BLOCK"

	EXPR TokenType = "EXPR"
	EXPR_DEFINE TokenType = "EXPR_DEFINE"
	EXPR_ASSIGN TokenType = "EXPR_ASSIGN"
	EXPR_CALL_FUNC TokenType = "EXPR_CALL_FUNC"
	EXPR_REF_DOT TokenType = "EXPR_REF_DOT"
	EXPR_BIT_AND TokenType = "EXPR_BIT_AND"
	EXPR_BIT_OR TokenType = "EXPR_BIT_OR"
	EXPR_MUL TokenType = "EXPR_MUL"
	EXPR_DIV TokenType = "EXPR_DIV"
	EXPR_MOD TokenType = "EXPR_MOD"
	EXPR_ADD TokenType = "EXPR_ADD"
	EXPR_SUB TokenType = "EXPR_SUB"
	EXPR_EQ TokenType = "EXPR_EQ"
	EXPR_NEQ TokenType = "EXPR_NEQ"
	EXPR_GT TokenType = "EXPR_GT"
	EXPR_GTE TokenType = "EXPR_GTE"
	EXPR_LT TokenType = "EXPR_LT"
	EXPR_LTE TokenType = "EXPR_LTE"
	EXPR_LOGIC_AND TokenType = "EXPR_LOGIC_AND"
	EXPR_LOGIC_OR TokenType = "EXPR_LOGIC_OR"
	EXPR_LOGIC_NOT TokenType = "EXPR_LOGIC_NOT"

	TYPE TokenType = "TYPE"
)

const (
	CURSOR TokenType = "."
)

type Rule []TokenType
type Rules []Rule
type RuleSet struct {
	TokenType
	priority int
	rules Rules
}
var ruleSets = []RuleSet {
	{TYPE, 0, Rules{{INT}, {DOUBLE}, {BOOLEAN}, {STRING}, {STRUCT}, {FUNC}}},

	{EXPR_DEFINE, 0, Rules{{DEF, LITERAL_IDENTIFIER, AS, TYPE}, {DEF, LITERAL_IDENTIFIER}}},
	{EXPR_ASSIGN, 0, Rules{{LITERAL_IDENTIFIER, ASSIGN, EXPR}}},
	{EXPR_CALL_FUNC, 0, Rules{{LITERAL_IDENTIFIER, PAREN_L, PAREN_R}, {LITERAL_IDENTIFIER, PAREN_L, PAREN_R}}},
	{EXPR, 1, Rules{{EXPR_DEFINE}, {EXPR_ASSIGN}, {EXPR_CALL_FUNC}, {EXPR_REF_DOT}, {EXPR_BIT_AND}, {EXPR_BIT_OR},
		{EXPR_MUL}, {EXPR_DIV}, {EXPR_MOD}, {EXPR_ADD}, {EXPR_SUB},
		{EXPR_EQ}, {EXPR_NEQ}, {EXPR_GT}, {EXPR_GTE}, {EXPR_LT}, {EXPR_LTE},
		{EXPR_LOGIC_AND}, {EXPR_LOGIC_OR}, {EXPR_LOGIC_NOT},
		{LITERAL_IDENTIFIER}, {LITERAL_STRING}, {LITERAL_INTEGER}, {LITERAL_REAL}}},
	{EXPR_REF_DOT, 2, Rules{{EXPR, PERIOD, EXPR}}},
	{EXPR_BIT_AND, 3, Rules{{EXPR, BIT_AND, EXPR}}},
	{EXPR_BIT_OR, 3, Rules{{EXPR, BIT_OR, EXPR}}},
	{EXPR_MUL, 4, Rules{{EXPR, MUL, EXPR}}},
	{EXPR_DIV, 4, Rules{{EXPR, DIV, EXPR}}},
	{EXPR_MOD, 4, Rules{{EXPR, MOD, EXPR}}},
	{EXPR_ADD, 5, Rules{{EXPR, ADD, EXPR}}},
	{EXPR_SUB, 5, Rules{{EXPR, SUB, EXPR}}},
	{EXPR_EQ, 6, Rules{{EXPR, EQ, EXPR}}},
	{EXPR_NEQ, 6, Rules{{EXPR, NEQ, EXPR}}},
	{EXPR_GT, 6, Rules{{EXPR, GT, EXPR}}},
	{EXPR_GTE, 6, Rules{{EXPR, GTE, EXPR}}},
	{EXPR_LT, 6, Rules{{EXPR, LT, EXPR}}},
	{EXPR_LTE, 6, Rules{{EXPR, LTE, EXPR}}},
	{EXPR_LOGIC_AND, 7, Rules{{EXPR, LOGIC_AND, EXPR}}},
	{EXPR_LOGIC_OR, 7, Rules{{EXPR, LOGIC_OR, EXPR}}},
	{EXPR_LOGIC_NOT, 7, Rules{{LOGIC_NOT, EXPR}}},

	//{STMT_EXPR, 8, Rules{{EXPR, SEMI_COLON}}},
	{STMT_IF, 8, Rules{{IF, PAREN_L, EXPR, PAREN_R, STMT_BLOCK}}},
	{STMT_FOR, 8, Rules{{FOR, PAREN_L, EXPR, SEMI_COLON, EXPR, SEMI_COLON, EXPR, PAREN_R, STMT_BLOCK}}},
	{STMT_BLOCK, 8, Rules{{STMT}, {BRACE_L, STMTS, BRACE_R}}},
	{STMT, 8, Rules{{STMT_EXPR}, {STMT_IF}, {STMT_FOR}, {STMT_BLOCK}}},
	{STMTS, 8, Rules{{STMT, STMT}, {STMTS, STMT}, {STMT}}},
}
var priorityByTokenType = map[TokenType]int{}
var rulesByTokenType = map[TokenType]Rules{}
var suppositionsByTokenType = map[TokenType]*[]TokenType{}
//var expectationsByTokenType = map[TokenType]*[]TokenType{}

func (rule Rule) match(nodes []Node) bool {
	if len(rule) != len(nodes) { return false }

	for i, node := range nodes {
		if rule[i] != node.TokenType { return false }
	}

	return true
}

func generateParsingTable() {
	type state struct {
		TokenType
		cursor int
		types []TokenType
	}

	closure := func (kernel state) {

	}

	//type Item struct {
	//	TokenType
	//	rule []TokenType
	//}
	//type State struct {
	//	items []Item
	//	children map[TokenType]State
	//}
	//
	//states := []State{}
	//
	//test := func (cursor int, ruleSets []RuleSet) {
	//	state := State{[]Item{}, map[TokenType]State{}}
	//
	//	for _, ruleSet := range ruleSets {
	//		for _, rule := range ruleSet.rules {
	//			//item := Item{ruleSet.TokenType, append(append(rule[:cursor], CURSOR), rule[cursor:]...)}
	//			item := Item{ruleSet.TokenType, make([]TokenType, len(rule))}
	//			copy(item.rule, rule)
	//			item.rule = append(append(item.rule[:cursor], CURSOR), rule[cursor:]...)
	//
	//			state.items = append(state.items, item)
	//
	//			if _, okay := state.children[rule[cursor]]; !okay {
	//				state.children[rule[cursor]] = State{[]Item{}, map[TokenType]State{}}
	//			}
	//
	//
	//		}
	//	}
	//
	//	states = append(states, state)
	//}
	//
	//test(0, ruleSets)
	//
	//for i, state := range states {
	//	fmt.Printf("state %d\n", i)
	//
	//	for _, item := range state.items {
	//		fmt.Printf("\t\t %-16s -> %s\n", item.TokenType, item.rule)
	//	}
	//}
}

func indexing() {
	for _, ruleSet := range ruleSets {
		priorityByTokenType[ruleSet.TokenType] = ruleSet.priority
		rulesByTokenType[ruleSet.TokenType] = ruleSet.rules

		for _, rule := range ruleSet.rules {
			supposition := rule[0]

			if _, okay := suppositionsByTokenType[supposition]; !okay {
				suppositionsByTokenType[supposition] = &[]TokenType{}
			}

			suppositions := suppositionsByTokenType[supposition]
			for _, token := range *suppositions {
				if token != ruleSet.TokenType {
					continue
				}
			}
			*suppositions = append(*suppositions, ruleSet.TokenType)
		}
	}
}

type Node struct {
	TokenType
	data string
	children []Node
}

func parse(tokens Tokens) (result []Node) {
	nodes := make([]Node, len(tokens))
	for i, token := range tokens {
		nodes[i] = Node{token.TokenType, "", nil}
	}

	node := nodes[0]
	if suppositions, okay := suppositionsByTokenType[node.TokenType]; okay {
		isPossible := func (tokenType TokenType) bool {
			return false
		}

		for _, supposition := range *suppositions {
			if isPossible(supposition) {

			}
		}
	}

	return
}

func _parse(tokens Tokens) (nodes []Node) {
	type NewNode struct {
		Node
		offset int
		length int
		canceled bool
	}

	nodes = make([]Node, len(tokens))
	for i, token := range tokens {
		nodes[i] = Node{token.TokenType, "", nil}
	}

	for j := 0; j < 20; j++ {
		newNodes := make([]*NewNode, 0)
		newNodesMarked := make([]*NewNode, len(nodes))

		parsing:
		for index, node := range nodes {
			var newNode *NewNode = nil

			if suppositions, okay := suppositionsByTokenType[node.TokenType]; okay {
				//fmt.Printf("suppositions %-32s, %s\n", node.TokenType, suppositions)

				supposing:
				for _, supposedTokenType := range *suppositions {
					if rules, okay := rulesByTokenType[supposedTokenType]; okay {
						//fmt.Printf("\t\t rules %-16s %s\n", supposedTokenType, rules)

						for _, rule := range rules {
							startIndex := index - len(rule) + 1

							if startIndex >= 0 && rule.match(nodes[startIndex:startIndex + len(rule)]) {
								//fmt.Printf("\t\t checking rule %-16s: %-16s <- %-8s\n", node.TokenType, supposedTokenType, rule)
								if newNode != nil {
									if priority, okay := priorityByTokenType[newNode.TokenType]; okay {
										if priorityByTokenType[supposedTokenType] > priority {
											continue supposing
										}
									}
								}

								newNode = &NewNode{Node{supposedTokenType, "", nodes[startIndex:startIndex + len(rule)]}, startIndex, len(rule), false}

								continue supposing
							}
						}
					}
				}
			}

			if newNode == nil {
				newNode = &NewNode{node, index, 1, false}
			} else {
				fmt.Printf("\t\t %4d:%-4d newNode: %s\n", newNode.offset, newNode.offset + newNode.length-1, *newNode)
			}

			newNodesToCancel := []*NewNode{}
			for _, markedNewNode := range newNodesMarked[newNode.offset:newNode.offset + newNode.length] {
				if markedNewNode != nil && !markedNewNode.canceled {
					if priority, okay := priorityByTokenType[newNode.TokenType]; okay {
						if priorityByTokenType[markedNewNode.TokenType] < priority {
							continue parsing
						} else {
							newNodesToCancel = append(newNodesToCancel, markedNewNode)
						}
					}
				}
			}
			for _, newNodeToCancel := range newNodesToCancel {
				newNodeToCancel.canceled = true
			}

			newNodes = append(newNodes, newNode)
			for i := newNode.offset; i < newNode.offset + newNode.length; i++ {
				newNodesMarked[i] = newNode
			}
		}

		nodes = []Node{}
		for _, newNode := range newNodes {
			//fmt.Printf("\t\t %-16s caceled: %t\n", newNode.TokenType, newNode.canceled)
			if !newNode.canceled {
				nodes = append(nodes, newNode.Node)
			}
		}

		for i, node := range nodes {
			fmt.Printf("%d \t %-32s%s\n", i, node.TokenType, node.children)
		}
		fmt.Print("====================\n")
	}

	return
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
	fmt.Println("")

	generateParsingTable()
	//indexing()
	//for token, suppositions := range suppositionsByTokenType {
	//	fmt.Printf("if %s comes\n", token)
	//	for i, target := range *suppositions {
	//		fmt.Printf("\t%d\t%s\n", i, target)
	//	}
	//	fmt.Println("\t\tthese are supposed to follow.")
	//}
	//
	//parse(tokens)

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