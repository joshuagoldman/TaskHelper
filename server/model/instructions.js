const db = require("../database");

class Instructions {
    static retrievall (callback) {
        db.query("", function (err,res) {
            if(err.error)
                return callback(err)
            callback(res)
        });
    }

    static insert (userName, passWord, callback) {
        db.query(``, function (err,res) {
            if(err.error)
                return callback(err)
            return callback(res)
        });
    }
}

module.exports = Instructions