var http = require('http');
var fs = require('fs');

var port = process.argv[2];
var file = process.argv[3];

// HTTP server that serves the text file

var server = http.createServer(function (request, response) {
    response.writeHead(200, { 'content-type': 'text/plain' });
    stream = fs.createReadStream(file);
    stream.pipe(response);
});

server.listen(port);

