package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
)
type Pos struct {
	x int
	y int
}
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func getNeighbors(cur Pos) []Pos {
	return []Pos{
		{cur.x - 1, cur.y},
		{cur.x + 1, cur.y},
		{cur.x, cur.y - 1},
		{cur.x, cur.y + 1},
	}
}

func getValidNeighbors(data [][]int, cur Pos) []Pos {
	neighbors := getNeighbors(cur)
	validNeighbors := []Pos{}
	for _, neighbor := range neighbors {
		if neighbor.x < 0 || 
				neighbor.y < 0 || 
				neighbor.x >= len(data) || 
				neighbor.y >= len(data[0]) ||
				data[neighbor.x][neighbor.y] - data[cur.x][cur.y] > 1 {
			continue
		}
		validNeighbors = append(validNeighbors, neighbor)
	}
	return validNeighbors
}

func showVisited(data [][]int, visited map[Pos]int) {
	for x := 0; x < len(data); x++ {
		for y := 0; y < len(data[0]); y++ {
			if _, ok := visited[Pos{x, y}]; ok {
				fmt.Print("#")
			} else {
				if data[x][y] == 25 {
					fmt.Print("X")
				} else {
					fmt.Printf("%c", data[x][y]+'a')
				}
			}
		}
		fmt.Println("")
	}
}

func bfs(data [][]int, end Pos) int {
	visited := make(map[Pos]int)
	queue := []Pos{}
	for x := 0; x < len(data); x++ {
		for y := 0; y < len(data[0]); y++ {
			if data[x][y] == 0 {
				queue = append(queue, Pos{x, y})
				visited[Pos{x, y}] = 0
			}
		}
	}
	for len(queue) > 0 {
		cur := queue[0]
		if cur == end {
			return visited[cur]
		}
		queue = queue[1:]
		for _, neighbor := range getValidNeighbors(data, cur) {
			if _, ok := visited[neighbor]; !ok {
				queue = append(queue, neighbor)
				visited[neighbor] = visited[cur] + 1
			}
		}
	}
	showVisited(data, visited)
	return -1
}

func main() {
	flatData, err := ioutil.ReadFile("input.txt")
	check(err)
	data := bytes.Split(flatData, []byte("\n"))
	xLim := len(data)
	yLim := len(data[0])
	var startPos Pos
	var endPos Pos
	myData := make([][]int, xLim)
	for i := range myData {
		myData[i] = make([]int, yLim)
	}
	for x := 0; x < xLim; x++ {
		for y := 0; y < yLim; y++ {
			v := data[x][y]
			if v == 'S' {
				myData[x][y] = 0
				startPos.x = x
				startPos.y = y
			} else if v == 'E' {
				myData[x][y] = 25
				endPos.x = x
				endPos.y = y
			} else {
				// convert byte to int
				myData[x][y] = int(v - 'a')
			}
		}
	}
	fmt.Println(bfs(myData, endPos))
}