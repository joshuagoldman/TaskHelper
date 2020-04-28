import * as t from '../Regex.js';
let input = "Joshua Goldman";
let pattern = "(?<=G).*(?=man)";
let result = t.Match(pattern,input);
console.log(result);