// Uppercase every even line
// Lowecase every odd line
// Pipe from the stdin to the stdout

var through = require('through2');
var split = require('split');

var isEven = false;

var stream = through(write, end);

function write (buffer, encoding, next) {
    var data = "";
    if (isEven) {
        data = buffer.toString().toUpperCase();
    } else {
        data = buffer.toString().toLowerCase();
    }
    isEven = !isEven;
    this.push(data + '\n');
    next();
}

function end (done) {
    done();
}

process.stdin.pipe(split()).pipe(stream).pipe(process.stdout);

