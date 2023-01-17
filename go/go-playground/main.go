package main

import (
	"strings"

	"golang.org/x/tour/wc"
)

func WordCount(s string) map[string]int {
	str_arr := strings.Fields(s)
	count_map := make(map[string]int)

	for _, word := range str_arr {
		v := count_map[word]
		count_map[word] = v + 1
		// v, ok := count_map[word]
		// if ok {
		// 	count_map[word] = v + 1
		// } else {
		// 	count_map[word] = 1
		// }
	}
	return count_map
}

func main() {
	wc.Test(WordCount)
}
