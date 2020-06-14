var progress = require('progress-stream') ;
var buff = require('stream-buffers');
var fs = require('fs');

function on(fun, progObj){
    progObj.on("progress", function(progress) {
        fun(progress);
    });
};

function getProg(size) {
        try {
            var str = progress({
                length: size,
                time: 100 /* ms */
            });

            return str;
        } catch (error) {
            return null;
        }
    };

function uploadFile(path,output,str) {
    try {

        var buffer = fs.readFileSync(path);
        var myReadableStreamBuffer = new buff.ReadableStreamBuffer({
            frequency: 1000,      // in milliseconds.
            chunkSize: 10000000     // in bytes.
        }); 
        
        myReadableStreamBuffer.put(buffer);
        myReadableStreamBuffer
        .on("error", (error) =>{
            return error.message;
        })
        .on("close", () => {
            console.log('success upload');
            return "success";
        })
        .pipe(str)
        .pipe(fs.createWriteStream(output));
    } catch (error) {
        return error;
    }
};

var stat = fs.statSync("C:/Users/jogo/Downloads/postgresql-12.1-3-windows-x64.exe")
var str = getProg(stat.size)
var s = function(p){
    console.log(p.percentage)
}

on(s,str);

uploadFile("C:/Users/jogo/Downloads/postgresql-12.1-3-windows-x64.exe","C:/Users/jogo/Documents/newFIle.exe",str);