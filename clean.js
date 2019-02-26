var fs = require('fs');
var path = require('path');

const distPath = './dist';

if (fs.existsSync(distPath)) {
    console.log(`Removing ${distPath}...`);

    fs.readdirSync(distPath).forEach(function (fileName) {
        fs.unlinkSync(path.join(distPath, fileName));
    });

    fs.rmdirSync(distPath);
}
