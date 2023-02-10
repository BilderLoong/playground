package main

import (
	"fmt"

	"golang.org/x/tour/tree"
)

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	_walk := func(t *tree.Tree, ch chan int) {
		if t == nil {
			return
		}
	}

	ch <- t.Value

	if t.Left != nil {
		_walk(t.Left, ch)
	}
	if t.Right != nil {
		_walk(t.Right, ch)
	}

	defer close(ch)
	_walk(t, ch)
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	ch1, ch2 := make(chan int), make(chan int)

	go Walk(t1, ch1)
	go Walk(t2, ch2)

	for {
		a, ok1 := <-ch1
		b, ok2 := <-ch2

		if ok1 != ok2 || a != b {
			return false
		}

		// If both `ok1` and `ok1` is false
		if !ok1 {
			break
		}

	}

	return true
}

func main() {
	t1 := tree.New(1)
	t2 := tree.New(1)
	fmt.Println(Same(t1, t2))
	fmt.Println(Same(tree.New(1), tree.New(2)))
}
