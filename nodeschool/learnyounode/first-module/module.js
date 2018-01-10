// Async directory read with callback
// Filter resulting array of files by extension
var fs = require('fs');
var path = require('path');

module.exports = function (dir, ext, cb) {
    fs.readdir(dir, function (err, list) {
        if (err) {
            return cb(err);
        }

        filtered = list.filter(function (file) {
            if (path.extname(file) == '.' + ext) {
                return true;
            }
            return false;
        });

        cb(null, filtered);
    });
};

