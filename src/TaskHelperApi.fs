module TaskHelperApi

open Fable.Core

[<Import("Instructions", "../server/model/instructions.js")>]
let insert : string ->  unit = jsNative


