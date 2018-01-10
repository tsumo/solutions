var net = require('net');

var port = process.argv[2];

// Listen to TCP connections on the provided port
// For each connection respond with the
// "YYYY-MM-DD hh:mm"
// and close the connection

function formattedDate () {
    date = new Date();
    year = date.getFullYear();
    month = ("0" + (date.getMonth() + 1)).slice(-2);
    day = ("0" + date.getDate()).slice(-2);
    hours = ("0" + date.getHours()).slice(-2);
    minutes = ("0" + date.getMinutes()).slice(-2);
    return year + "-" + month + "-" + day + " " + hours + ":" + minutes;
}

var server = net.createServer(function (socket) {
    socket.write(formattedDate() + "\n");
    socket.end();
});

server.listen(port);

