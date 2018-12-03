const {readFileSync} = require('fs');

const input = readFileSync('input.txt', 'utf8')
  .split('\n')
  .map(n => parseInt(n));


// Part 1
const part1 = input.reduce((a, v) => a + v, 0);
console.log(`part 1: ${part1}`);

// Part 2
let freq = 0;
const freq_history = [];
for (let i = 0; ; i++) {
  freq += input[i % input.length];
  if (freq_history.includes(freq)) break;
  else freq_history.push(freq);
}
console.log(`part 2: ${freq}`);