var express = require("express"),
    app = express(),
    http = require("http").Server(app).listen(80),
    upload = require("express-fileupload");

app.use(upload())

console.log("Server Started!")
app.get("/", function(req,res){
    res.sendfile("C:/Users/jogo/Downloads/test.md")
})
app.post("/",function(req,res){
    if(req.files){
        var file = req.files.filename
            filenames = req.files.name
        console.log(req.files)
    }
})
