// Html comes from stdin.
// Convert inner html to uppercase
// for elements with classname 'loud'.

var fs = require('fs');
var trumpet = require('trumpet');
var through = require('through2');

var tr = trumpet();

var stream = tr.select('.loud').createStream();

stream.pipe(through(function (buf, _, next) {
    this.push(buf.toString().toUpperCase());
    next();
})).pipe(stream);

process.stdin.pipe(tr).pipe(process.stdout);

