// HTTP server that uppercases POST requests

var http = require('http');
var through = require('through2');

var port = parseInt(process.argv[2]);

var server = http.createServer(function (req, res) {
    if (req.method == 'POST') {
        req.pipe(through(write, end)).pipe(res);
    }
});

function write (buf, enc, next) {
    this.push(buf.toString().toUpperCase());
    next();
}

function end (done) {
    done();
}

server.listen(port);

