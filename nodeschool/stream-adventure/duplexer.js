// Export single function that spawns a process
// from 'cmd' string and 'args' array
// and returns a single duplex stream joining
// stdin and stdout.

var duplexer2 = require('duplexer2');
var spawn = require('child_process').spawn;

module.exports = function (cmd, args) {
    var pr = spawn(cmd, args);
    return duplexer2(pr.stdin, pr.stdout);
};

