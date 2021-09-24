export {};
import { Readable } from 'stream';
import { createInterface } from 'readline';

// The string used to debug, don't forget add \n to trigger the 'line' event;
const DEBUG_STRING = 'asdfasd asd fasd f asd\n';
let s;

if (DEBUG_STRING) {
  s = new Readable();
  s.push(DEBUG_STRING);
  s.push(null);
}

// If provide the debug string, prior to use it.
const inputStream = s || process.stdin;
const rl = createInterface({
  input: inputStream,
  output: process.stdout,
});

// `lines` is used to store all the lines from input stream;
// Each line is stored as an entry of the array.
const lines: string[] = [];
rl.on('line', (line: string) => {
  // Your code
});
