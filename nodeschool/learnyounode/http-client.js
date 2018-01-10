var http = require('http');

url = process.argv[2];

// Perform http request, write each "data" event
// to a new line on the console

http.get(url, function (response) {
    response.setEncoding("utf8");
    response.on("data", function (data) {
        console.log(data);
    });
}).on("error", console.error);

