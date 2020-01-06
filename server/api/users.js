var express = require('express');
var Users = require('../model/users');

var router = express.Router();

router.get('/', function(req,res) {
    Users.retrievAll(function (err,users) {
        if(err)
            return res.json(err);
        return res.json(users);
    });
});

router.post('/', function(req,res) {
    var Instruction = req.body;

    Users.insert(Instruction, function (res,result) {
        if(err)
            return res.json(err)
        return res.json(result);
    });
});

module.exports = router;