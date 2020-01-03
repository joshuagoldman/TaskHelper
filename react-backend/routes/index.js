var express = require('express');
var router = express.Router();

var DataInit =
  {
    InstructionVideo : "",
    InstructionTxt : "",
    Title : ""   
  }

/* GET home page. */
router.get('/', function(req, res, next) {
  res.json([
    {
      Data: DataInit,
      Title : "ExampleTitle"
    }
  ]);
});

module.exports = router;
