var http = require('http');

url = [];

for (var i = 2; i < 5; i++) {
    url.push(process.argv[i]);
}

result = [];
count = 0;

// Perform 3 http requests, collect all data
// Print received data in order - url1, url2, url3
// Requires counting number of callbacks from http.get

function getData (url, i) {
    http.get(url, function (response) {
        acum = "";
        response.setEncoding("utf8");
        response.on("data", function (data) {
            acum += data;
        });
        response.on("end", function () {
            result[i] = acum;
            count++;
            if (count == 3) {
                printResults();
            }
        });
    });
}

function printResults() {
    for (var i = 0; i < 3; i++) {
        console.log(result[i]);
    }
}

(function () {
    for (var i = 0; i < 3; i++) {
        getData(url[i], i);
    }
} ());

