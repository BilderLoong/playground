package main

import (
	// "bufio"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/sabhiram/go-gitignore"
	// "strings"
)

func Values[M ~map[K]V, K comparable, V any](m M) []V {
	r := make([]V, 0, len(m))
	for _, v := range m {
		r = append(r, v)
	}
	return r
}

func findIntersection[V comparable](slice1, slice2 []V) []V {
	intersection := []V{}

	// Create a map to store elements of slice1 for fast lookup
	lookup := make(map[V]bool)
	for _, value := range slice1 {
		lookup[value] = true
	}

	// Check if each element in slice2 is present in the map
	for _, value := range slice2 {
		if lookup[value] {
			intersection = append(intersection, value)
		}
	}

	return intersection
}
func getUsingComponentKeys(files []string) ([]string, error) {
	var result []string

	for _, file := range files {
		// Read the JSON file into a byte slice
		jsonFile, err := ioutil.ReadFile(file)
		if err != nil {
			fmt.Println("Wrong json file", file)
			continue
		}

		// Create a map to unmarshal the JSON data
		var data map[string]interface{}

		// Unmarshal the JSON data into the map
		if err := json.Unmarshal(jsonFile, &data); err != nil {
			return nil, err
		}

		// Check if "usingComponents" key exists and is an object
		usingComponents, ok := data["usingComponents"].(map[string]interface{})
		if !ok {
			continue // Skip if "usingComponents" is not an object
		}

		// Extract and append the keys of "usingComponents"
		for _, value := range usingComponents {
			result = append(result, value.(string))
		}
	}

	return removeDuplicates(result), nil
}

func removeFileExtension(filename string) string {
	// Find the last dot (.) in the filename
	lastDotIndex := strings.LastIndex(filename, ".")

	// If there is no dot, or if the dot is at the beginning of the filename (e.g., ".json"),
	// return the original filename.
	if lastDotIndex <= 0 {
		return filename
	}

	// Otherwise, return the substring before the last dot.
	return filename[:lastDotIndex]
	// Use filepath.Ext to get the file extension
	// ext := filepath.Ext(fileName)

	// // Use filepath.Base to get the file name without extension
	// nameWithoutExt := fileName[:len(fileName)-len(ext)]

	// return nameWithoutExt
}

func removeDuplicates[V comparable](input []V) []V {
	seen := make(map[V]bool)
	result := []V{}

	for _, item := range input {
		if !seen[item] {
			seen[item] = true
			result = append(result, item)
		}
	}

	return result
}

func getComponentName(path string) string {
	parts := strings.Split(path, "/")
	if len(parts) < 3 {
		return ""
	}

	return parts[len(parts)-2]
}

func isComponent(targetString string, stringList []string) bool {
	count := 0
	for _, str := range stringList {
		if str == targetString {
			count++
		}
	}
	return (count == 3 || count == 4) && !strings.Contains(targetString, "pages/")
}

func filterComponentPath(filePaths []string) []string {
	components := []string{}
	pathsWithoutExt := []string{}
	for _, path := range filePaths {
		pathsWithoutExt = append(pathsWithoutExt, removeFileExtension(path))
	}

	for _, path := range pathsWithoutExt {
		if isComponent(path, pathsWithoutExt) {
			components = append(components, path)
		}
	}

	return removeDuplicates(components)
}

func readDirRecursivelyWith(shouldIgnore func(string) bool) func(root string) ([]string, error) {
	return func(root string) ([]string, error) {
		var files []string

		rootPath, err := filepath.Abs(root)
		if err != nil {
			return []string{}, err
		}

		entries, err := os.ReadDir(rootPath)

		if err != nil {
			return []string{}, err
		}

		for _, e := range entries {
			curPath, err := filepath.Abs(filepath.Join(rootPath, e.Name()))
			if err != nil {
				continue
			}

			if shouldIgnore(curPath) {
				continue
			}

			if e.IsDir() {
				newFiles, _ := readDirRecursivelyWith(shouldIgnore)(curPath)
				files = append(files, newFiles...)
			} else {
				files = append(files, curPath)
			}
		}

		return files, err
	}
}

func getComponentNames(componentsPath []string) (componentName []string) {
	for _, path := range componentsPath {
		componentName = append(componentName, getComponentName(path))
	}
	return componentName
}

func main() {
	// dirs, err := os.ReadDir("./tests")
	// if err != nil {
	// }
	// for _, e := range dirs {
	// 	fmt.Println(e.Name())
	// }

	gitignore, err := ignore.CompileIgnoreFile("../../.gitignore")
	if err != nil {
		fmt.Println("Error reading .gitignore file:", err)
		os.Exit(1)
	}

	readDirRecursivelyWithIgnore := readDirRecursivelyWith(func(path string) bool {
		return gitignore.MatchesPath(path)
	})

	components, err := readDirRecursivelyWithIgnore("../../apps/components")

	if err != nil {
		fmt.Println("Error", err)
	}

	menuorders, err := readDirRecursivelyWithIgnore("../../apps/menuorder")
	if err != nil {
		fmt.Println("Error", err)
	}
	mains, err := readDirRecursivelyWith(func(s string) bool {
		return strings.Contains(s, "remote")

	})("/Users/birudo/Project/menuorder-new-rms-h5-0/apps/main/dist/merchant")
	if err != nil {
		fmt.Println("Error", err)
	}

	all := append(components, menuorders...)
	mainPackageComponentPaths := filterComponentPath(mains)

	allPackageComponentPaths := filterComponentPath(all)
	allUsedComponentPaths := []string{}
	for _, path := range allPackageComponentPaths {
		allUsedComponentPaths = append(allUsedComponentPaths, path+".json")
	}
	usingComponentKeys, _ := getUsingComponentKeys(allUsedComponentPaths)

	mainNames := getComponentNames(mainPackageComponentPaths)
	usingNames := getComponentNames(usingComponentKeys)

	fmt.Println(findIntersection(mainNames, usingNames))
}
