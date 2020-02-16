const http = require('http');

const server = http.createServer((req,res) => {
    if (req.url == '/Instructions') {
        res.write('You are about to save an instruction?');
        res.end
    }

    if (req.url == '/Videos') {
        res.write('you are about to save a video?');
        res.end
    }
})

server.listen(3000)

console.log("you're listening on port 3000!")

