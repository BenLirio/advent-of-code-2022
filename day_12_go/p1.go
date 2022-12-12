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

func bfs(data [][]int, start Pos, end Pos) int {
	visited := make(map[Pos]int)
	visited[start] = 0
	queue := []Pos{start}
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
	fmt.Println(bfs(myData, startPos, endPos))
}