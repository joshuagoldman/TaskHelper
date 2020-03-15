const db = require("../database");

function loginQuery(usrName,password) {
    let query =
   `SELECT * FROM users
   WHERE user_name = \'${usrName}\'
   AND password = \'${password}\';`

    return query;
  } 

function userQuery(id) {
    let query =
   `select id,
        (
        select array_to_json(array_agg(row_to_json(d)))
        from (
            select title,
                (
                    select array_to_json(array_agg(row_to_json(e)))
                    from(
                        select instruction_Video, instruction_txt, part_title
                        from parts
                        where parts.instruction_id = instructions.instruction_id
                    ) e
                ) as parts 
            from instructions
            where instructions.id = users.id
        ) d
        ) as instructions
    from users
    where users.id = ${id}`

    return query;
  } 

class Users {
    static retrievAll (callback) {
        db.query(`SELECT * FROM users`, function (err,res) {
            if(err.error)
                return callback(err)
            callback(res)
        });
    }

    static retrieveUser (user,password,callback) {
        db.query(loginQuery(user,password), function (err,res) {
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
    
    static getUser (id,callback) {
        db.query(userQuery(id), function (err,res) {
            if(err.error)
                return callback(err)
            callback(res)
        });
    }
}

module.exports = Users