var fs = require('fs');

path = process.argv[2];

// Read file contents asynchronously
// Parse content in callback function
// which gets executed after the i/o
// operation is done
fs.readFile(path, 'utf-8', function cb (err, data) {
    if (err) {
        return console.log(err);
    }
    console.log(data.split('\n').length - 1);
});

