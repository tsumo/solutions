var fs = require('fs');

var path = process.argv[2];

var buf = fs.readFileSync(path);

var str = buf.toString();

// Number of lines in a file
console.log(str.split('\n').length - 1);

