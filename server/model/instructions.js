const db = require("../database");

let getQuery = `select array_to_json(array_agg(row_to_json(t)))
from (
  select id,
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
) t`

class Instructions {
    static retrievAll (callback) {
        db.query(getQuery, function (err,res) {
            if(err.error)
                return callback(err)
            callback(res)
        });
    }

    static insert (instruction, callback) {
        db.query(``, function (err,res) {
            if(err.error)
                return callback(err)
            return callback(res)
        });
    }
}

module.exports = Instructions