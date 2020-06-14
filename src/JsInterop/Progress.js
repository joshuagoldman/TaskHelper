import * as pStream from 'progress-stream';
import * as io from 'socket.io-client';

export function on(fun, progObj){
    progObj.onload( function(ss) {
        fun(ss);
    });
};

export function onProgress(fun, progObj){
    try{
        progObj.on("data",function(pr){
            fun(pr);
        })
    }
    catch(error){
        console.log(error);
    }
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

export function createSocket(url) {
    try{
        var socket = io.connect(url);
        return socket;
    }
    catch(error){
        return error.message;
    }
}

export function addEventListener(handler, eventName, socketObj) {
    try{
        var emitter = 
            socketObj.on(eventName,
            // When we receive data
                function(data) {
                    handler(data);
                }
        );
        return emitter;
    }
    catch{
        return null;
    }
}