var http = require('http');
var map = require('through2-map');

var port = process.argv[2];

// HTTP server that receives only POST requests,
// converts POST body characters to upper-case
// and returns them

var server = http.createServer(function (request, response) {
    if (request.method == 'POST') {
        response.writeHead(200, { 'content-type': 'text/plain' });
        request.pipe(map (function (chunk) {
            return chunk.toString().toUpperCase();
        })).pipe(response);
    }
});

server.listen(port);

