const path = require('path');
const express = require('express');
const bodyParser = require('body-parser');

const ENV = process.env.NODE_ENV;
const PORT = process.env.PORT || 3001;

var db = require('./database');

const app = express();
app.use(express.json());
app.use(express.urlencoded(({extended:false})));
app.use(bodyParser.json());

app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

app.use('./api/instructions', require('./api/instructions'));

db.query('SELECT NOW()', (err, res) => {
    if(err.error)
        return console.log(err.error);
    console.log(`PostgreSQL connected: ${res[0].now}.`);
});

module.exports = app;

