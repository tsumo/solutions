// Use websocket-stream to print "hello\n".
// File with be compiled with browserify
// with results available at http://localhost:8099

var ws = require('websocket-stream');

var stream = ws('ws://localhost:8099');

stream.write('hello\n');
stream.end();

