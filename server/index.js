const express = require('express');
const bodyParser = require('body-parser');
var cors = require('cors');
var fs = require('fs')
const multer = require('multer');
const fileUpload = require('express-fileupload');
const upload = multer();

const ENV = process.env.NODE_ENV;
const PORT = process.env.PORT || 3001;

var db = require('./database');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use(fileUpload());


app.post("/upload", (req, res, next) => {
    console.log(req.files.file);
    //var newFile = fs.readFileSync(req.files.file.path);
    //var encode_file = newFile.toString('base64');
    // Define a JSONobject for the image attributes for saving to database
    var finalFile = {
        contentType: req.files.file.mimetype,
        file:  new Buffer(req.files.file.data, 'base64')
    };
    let fileName = req.body.fileName;
    let path = `${__dirname}/../public/${req.body.folder}/${req.body.fileName}`;
    let pubPath = `public/${req.body.folder}/${req.body.fileName}`;
    if (!finalFile.file) {
        const error = new Error('Please upload a file')
        error.httpStatusCode = 400
        return next(error)
    }
    let fileInfo = {
        Name : fileName,
        Path : pubPath
    }
    fs.writeFile(path, finalFile.file, (err) => {
        if(err)
         res.send(err);
        else
        res.send(fileInfo);
    })
});

app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

app.use('/api/instructions', require('./api/instructions'));
app.use('/api/users', require('./api/users'));

db.query('SELECT NOW()', (err, res) => {
    if(err.error)
        return console.log(err.error);
    console.log(`PostgreSQL connected: ${res[0].now}.`);
});

module.exports = app;

