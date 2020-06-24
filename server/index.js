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
var socket  = require('socket.io');
timeout = require('connect-timeout');
var clientSocket = require('socket.io-client');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use(fileUpload());
app.use(timeout(15000));

// --------------------------------------------------------------------------------------------------------------
// LISTEN TO SERVER @ PORT 3001
// --------------------------------------------------------------------------------------------------------------

var server = app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

app.use(express.static(__dirname + '/public'));

var io = socket(server);

io.sockets.on(`connection`, (sckt) => {
    sckt.on('message',(msgObj) =>{
        sckt.broadcast.emit('message', msgObj);
    })

    sckt.on('finished',(msgObj) =>{
        sckt.broadcast.emit('finished', msgObj);
    })
});

// --------------------------------------------------------------------------------------------------------------
// UPLOAD FILES API
// --------------------------------------------------------------------------------------------------------------
app.post("/upload", (req, res, next) => {
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


    var buffer = new Buffer(finalFile.file,'base64');

    var myReadableStreamBuffer = new streamBuffers.ReadableStreamBuffer({
        frequency: 1000,      // in milliseconds.
        chunkSize: 10000000     // in bytes.
        }); 
    
    var wrStr = fs.createWriteStream(path) ;
    var str = progress({
        length: finalFile.file.length,
        time: 10 /* ms */
    });

    var newClient = clientSocket.connect('http://localhost:3001');

    str.on('progress', function(pr) {
        console.log(`Upload started for file ${req.body.filePath}!`);
        if(pr.remaining === 0){
            newClient.emit(`finished`,{ Status: 200, Msg: `file ${req.body.filePath} saved!`, Path: req.body.filePath});
            console.log(`finished uploading ${req.body.filePath}`);
        }
        else{
            newClient.emit(`message`,{ Progress : pr, Path: req.body.filePath });
            console.log(`${pr.percentage} completed`);
        }
        downloaded = pr.percentage;
    });

    myReadableStreamBuffer.put(buffer);
    myReadableStreamBuffer
    .on('error', (error) =>{
        newClient.emit(`finished`,{ Status: 404, Msg: error.message, Path : req.body.filePath});
    })
    .pipe(str)
    .pipe(wrStr);

    res.send("done!")
    next();
});

// --------------------------------------------------------------------------------------------------------------
// DELETE FILES API
// --------------------------------------------------------------------------------------------------------------
app.post("/delete", (req, res, next) => {
    let path = `${__dirname}/../public/${req.body.filePath}`;
    let UsrPath = path.match(".*User.*?(?=\/)")[0];
    let instrPath = path.match(".*Instruction.*?(?=\/)")[0];
    let pubPath = `public/${req.body.filePath}`;
    let fileName = req.body.filePath.replace(req.body.filePath.substring(0,req.body.filePath.lastIndexOf("/")),"");
    let fileInfo = {
        Name : fileName,
        Path : pubPath
    }

    fs.unlink(path, (err) => {
        if (err) {
            console.error(err)
            res.status(404).send(err)
        } 
    });

    fs.readdir(instrPath, function(err, files) {
        if (err) {
            res.status(404).send(err);
        }
        else {
           if (!files.length) {
                fs.rmdirSync(instrPath);
                console.log("deleted instruction folder")
                fs.readdir(UsrPath, function(err2, files2) {
                    if (err2) {
                        res.status(404).send(err2);

                    } 
                    else {
                        if (!files2.length) {
                            fs.rmdirSync(UsrPath);
                            console.log("deleted user folder")
                        } 
                    }
                });
           }
        }
    });

    res.send(`Deleted file '${fileInfo.Name}' located at '${fileInfo.Path}'`);
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

