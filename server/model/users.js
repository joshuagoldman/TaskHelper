const db = require("../database");

class Users {
    static retrievAll (callback) {
        db.query(`SELECT * FROM users`, function (err,res) {
            if(err.error)
                return callback(err)
            callback(res)
        });
    }

    static insert (username, password, callback) {
        db.query(`INSERT INTO users (user_name, password) VALUES (${usename}, ${password})`, function (err,res) {
            if(err.error)
                return callback(err)
            return callback(res)
        });
    }
}

module.exports = Users