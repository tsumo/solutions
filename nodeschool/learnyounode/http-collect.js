var http = require('http');

url = process.argv[2];

// Perform http request, collect all data
// Print number of characters received
// Pring a single string of those characters

http.get(url, function (response) {
    var resString = "";
    response.setEncoding("utf8");
    response.on("data", function (data) {
        resString += data;
    });
    response.on("end", function () {
        console.log(resString.length);
        console.log(resString);
    });
}).on("error", console.error);

