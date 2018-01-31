// Buffer the stream, reverse it

var concat = require('concat-stream');

function reverseStream (stream) {
    console.log(stream.toString().split('').reverse().join(''));
}

process.stdin.pipe(concat(reverseStream));

