var express = require('express');
var Instructions = require('../model/instructions');

var router = express.Router();

router.get('/', function(req,res) {
    Instructions.retrieveAll(function (err,instructions) {
        if(err)
            return res.json(err);
        return res.json(instructions);
    });
});

router.post('/', function(req,res) {
    var Instruction = req.body;

    Instructions.insert(Instruction, function (res,result) {
        if(err)
            return res.json(err)
        return res.json(result);
    });
});

module.exports = router;