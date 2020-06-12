import * as pStream from 'progress-stream'
import * as fs from 'fs'

export function on(fun, progObj){
    progObj.on("progress", function(ss) {
        fun(ss);
    });
};

export function getProg(size) {
        try {
            var str = pStream({
                length: size,
                time: 100 /* ms */
            });

            return str.progress();
        } catch (error) {
            return null;
        }
    };

export function uploadFile(path,output,str) {
    try {
        fs.createReadStream(path)
        .pipe(str)
        .pipe(fs.createWriteStream(output));
        return "success";
    } catch (error) {
        return error.message;
    }
};