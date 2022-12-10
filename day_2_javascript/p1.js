const fs = require('fs')

// Utils

// https://stackoverflow.com/questions/9960908/permutations-in-javascript
const permutator = (inputArr) => {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next))
     }
   }
 }
 permute(inputArr)
 return result;
}


// Solution

const rawData = fs.readFileSync('input.txt', 'utf8')
const data = rawData
  .trim()
  .split('\n')
  .map(line => line.split(' '))

const replaceLine = line => m => {
  const [p1, p2] = line
  return [
    m[p1] !== undefined ? m[p1] : p1,
    m[p2] !== undefined ? m[p2] : p2
  ]
}
const replace = data => m => data.map(replaceLine).map(f => f(m))

const replacements = permutator(['A', 'B', 'C']).map(([x, y, z]) => ({
  'X': x,
  'Y': y,
  'Z': z
}))

const nondetData = replacements.map(replace(data))

const scoreRound = ([p1, p2]) => {
  let score = 1 + p2 // symbol score
  if (p1 === p2) { score += 3 } // tie worth 3
  if ((p1+1)%3 === p2) { score += 6 } // win worth 6
  return score
}
const scoreData = data => {
  const dataInts = replace(data)({ 'A': 0, 'B': 1, 'C': 2 })
  return dataInts.map(scoreRound).reduce((a, b) => a + b, 0)
}
const scores = nondetData.map(scoreData)
const maxScore = scores.reduce((a, b) => Math.max(a, b), 0)

console.log(maxScore)