const express = require('express');
const bodyParser = require('body-parser');
var progress = require('progress-stream');
var cors = require('cors');
var fs = require('fs')
const fileUpload = require('express-fileupload');
const Joi = require('joi');
const PORT = process.env.PORT || 3001;
var db = require('./database');
var streamBuffers = require('stream-buffers');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use(fileUpload());

// --------------------------------------------------------------------------------------------------------------
// UPLOAD FILES API
// --------------------------------------------------------------------------------------------------------------
app.post("/upload", (req, res, next) => {
    console.log(req.files.file);
    var finalFile = {
        contentType: req.files.file.mimetype,
        file:  new Buffer(req.files.file.data, 'base64')
    };
    let path = `${__dirname}/../public/${req.body.filePath}`;
    let instructionDirectory = path.substring(0,path.lastIndexOf('/'));
    let userDirectory = instructionDirectory.substring(0,instructionDirectory.lastIndexOf('/'));
    let fileName = path.substring(path.lastIndexOf('/'),path.length - 1);

    if (!fs.existsSync(instructionDirectory)){
        if (!fs.existsSync(userDirectory)){
            fs.mkdirSync(userDirectory);
            fs.mkdirSync(instructionDirectory);
        }
        else{
            fs.mkdirSync(instructionDirectory);
        }
    }

    let pubPath = `public/${req.body.filePath}`;
    if (!finalFile.file) {
        const error = new Error('Please upload a file')
        error.httpStatusCode = 400
        return next(error)
    }
    let fileInfo = {
        Name : fileName,
        Path : pubPath
    }

    fs.writeFile(path,finalFile.file, function(err) {
        if(err)
        {
            res.status(404).send(err.message)
        }
        else
        {
            res.send(`Saved file '${fileInfo.Name}' on '${fileInfo.Path}'`);
        }
    })
});

// --------------------------------------------------------------------------------------------------------------
// DELETE FILES API
// --------------------------------------------------------------------------------------------------------------
app.post("/delete", (req, res, next) => {
    let filePath = req.body.filePath;
    let path = `${__dirname}/../public/${filePath}`;
    let pubPath = `public/${filePath}`;
    let fileName = filePath.replace(filePath.substring(0,filePath.lastIndexOf("/")),"");
    let fileInfo = {
        Name : fileName,
        Path : pubPath
    }

    fs.unlink(path, (err) => {
        if (err) {
            console.error(err)
            res.send(err)
        }
        else{
            res.send(`Deleted file '${fileInfo.Name}' located at '${fileInfo.Path}'`);
        }  
    });
});
// --------------------------------------------------------------------------------------------------------------
// INSERT, CHANGE, DELETE DATABASE INFO API
// --------------------------------------------------------------------------------------------------------------

app.post('/', (req,res) => {
    const schema = Joi.object().keys({
        Queries : Joi.string().min(3).required()
    })

    var result = Joi.validate(req.body,schema);
    if(result.error){
        var resMsgsArray = result.error.details.map(x => x.message + '\n');
        var resMsg = resMsgsArray.join("");
        res.status(401).send(resMsg.substring(0, resMsg.length - 1));
    }
    else{
        db.query(req.body.Queries, function (err,dbRes) {
            if(err.error)
                res.status(401).send(err)
            res.send("Changes were sucessfully updated!")
        });
    }
});

// --------------------------------------------------------------------------------------------------------------
// LISTEN TO SERVER @ PORT 3001
// --------------------------------------------------------------------------------------------------------------

app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

// --------------------------------------------------------------------------------------------------------------
// FETCHING LOGIN AND USER DATA FROM DATABASE
// --------------------------------------------------------------------------------------------------------------

app.use('/api/instructions', require('./api/instructions'));
app.use('/api/users', require('./api/users'));

db.query('SELECT NOW()', (err, res) => {
    if(err.error)
        return console.log(err.error);
    console.log(`PostgreSQL connected: ${res[0].now}.`);
});

module.exports = app;

