// Send HTTP POST to localhost:8099
// and pipe stdin into it.
// Pipe response to stdout.

var request = require('request');

var r = request.post('http://localhost:8099');

process.stdin.pipe(r);
r.pipe(process.stdout);
// Or in single line
// process.stdin.pipe(r).pipe(process.stdout);

