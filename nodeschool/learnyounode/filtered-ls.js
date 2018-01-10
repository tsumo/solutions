var fs = require('fs');
var path = require('path');

dir = process.argv[2];
ext = process.argv[3];

// Async directory read with callback
// Filter resulting array of files by extension
fs.readdir(dir, function cb (err, list) {
    if (err) {
        console.log(err);
    }
    filtered = list.filter(function byExt (file) {
        if (path.extname(file) == '.' + ext) {
            return true;
        }
        return false;
    });
    for (i = 0; i < filtered.length; i++) {
        console.log(filtered[i]);
    }
});

