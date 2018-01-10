var http = require('http');
var url = require('url');

var port = process.argv[2];

// HTTP server that serves JSON data
//
// For GET request to the '/api/parsetime':
//     /api/parsetime?iso=2013-08-10T12:10:15.474Z
// return JSON response:
//     {
//       "hour": 14,
//       "minute": 23,
//       "second": 15
//     }
//
// For GET request to the '/api/unixtime' return
//     { "unixtime": 1376136615474 }

var server = http.createServer(function (request, response) {
    if (request.method == 'GET') {
        urlObj = url.parse(request.url, true);
        response.writeHead(200, { 'Content-Type': 'application/json' });
        var date = new Date(urlObj.query.iso);
        var dateObj = {};
        if (urlObj.pathname == '/api/parsetime') {
            dateObj.hour = date.getHours();
            dateObj.minute = date.getMinutes();
            dateObj.second = date.getSeconds();
        }
        if (urlObj.pathname == '/api/unixtime') {
            dateObj.unixtime = date.getTime();
        }
        response.write(JSON.stringify(dateObj));
        response.end('\n');
    }
});

server.listen(port);

