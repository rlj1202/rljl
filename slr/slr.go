package slr

import "fmt"

const (
	stmt = "STMT"
)

type production struct {
	symbol string
	symbols []string
}
var grammers = []production{
	{"PROGRAM", []string{"STMTS"}},
	{"STMTS", []string{"STMT"}},
	{"STMT", []string{"STMT", "STMT"}},
	{"STMT", []string{"STMT_EXPR"}},
	{"STMT_EXPR", []string{"EXPR"}},
	{"EXPR", []string{"DEFINE"}},
	{"DEFINE", []string{"def", "ID"}},
	{"DEFINE", []string{"def", "ID", "as", "TYPE"}},
	{"TYPE", []string{"int"}},
	{"TYPE", []string{"double"}},
}
var productsByNonterminal = map[string]*[]production {}

func (a production) equals(b production) bool {
	if a.symbol != b.symbol { return false }
	if len(a.symbols) != len(b.symbols) { return false }
	for i := 0; i < len(a.symbols); i++ {
		if a.symbols[i] != b.symbols[i] { return false }
	}

	return true
}

func indexing() {
	for _, grammer := range grammers {
		if _, okay := productsByNonterminal[grammer.symbol]; !okay {
			productsByNonterminal[grammer.symbol] = new([]production)
		}

		products := productsByNonterminal[grammer.symbol]
		*products = append(*products, grammer)
	}
}

type state []lritem
type lritem struct {
	production
	cursor int
}

func closure(kernel state) (result state) {
	result = make(state, len(kernel))
	for i, it := range kernel {
		result[i] = it
	}

	for _, item := range result {
		fmt.Printf("%-16s -> %-16s\n", item.symbol, item.symbols)
		cursorArray := make([]string, len(item.symbols) + 1)
		cursorArray[item.cursor] = "^"
		fmt.Printf("%-16s    %-16s\n", "", cursorArray)
	}
	fmt.Println("=>")

	for _, it := range kernel {
		//lhs := item.nonterminals[:item.cursor]
		rhs := it.symbols[it.cursor:]
		if len(rhs) == 0 { continue }

		if products, okay := productsByNonterminal[rhs[0]]; okay {
			product:
			for _, production := range *products {
				newItem := lritem{production, 0}

				for _, oriItem := range result {
					if oriItem.production.equals(newItem.production) {
						continue product
					}
				}

				result = append(result, newItem)
			}
		}
	}

	for _, item := range result {
		fmt.Printf("%-16s -> %-16s\n", item.symbol, item.symbols)
		cursorArray := make([]string, len(item.symbols) + 1)
		cursorArray[item.cursor] = "^"
		fmt.Printf("%-16s    %-16s\n", "", cursorArray)
	}

	return
}

func goTo(kernel state, symbol string) (result state) {
	newKernel := state{}

	for _, it := range kernel {
		rhs := it.symbols[it.cursor:]

		if len(rhs) > 0 && rhs[0] == symbol {
			newIt := lritem{it.production, it.cursor + 1}

			newKernel = append(newKernel, newIt)
		}
	}

	result = closure(newKernel)
	return
}

func main() {
	indexing()
	kernel := closure(state {
		lritem{(*productsByNonterminal["DEFINE"])[0], 0},
		lritem{(*productsByNonterminal["DEFINE"])[1], 3},
	})
	fmt.Println("===============")
	goTo(kernel, "TYPE")
}
