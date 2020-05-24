module JsInterop

open Fable.Core

module Regex =

    /// <summary>Returns the first regex match if it exists, otherwise a None value is returned</summary>
    let Match (pattern:string) (input:string) : string Option = Fable.Core.JsInterop.import "Match" "./JsInterop/Regex.js"
   
    /// <summary>Returns a string array of all matches, if none exist, a None value is returned</summary>
    let Matches (pattern:string) (input : string) : string[] Option = Fable.Core.JsInterop.import "Matches" "./JsInterop/Regex.js"
   
    /// <summary>Returns true if any matches exists, false if none exist, and None is an error occurs</summary>
    let IsMatch (pattern:string) (input : string) : bool Option = Fable.Core.JsInterop.import "IsMatch" "./JsInterop/Regex.js"
  
type IProgress =
    abstract length : float
    abstract percentage : float
    abstract remaining : float
    [<Emit("$2.on($1,$0)")>]
    abstract on : (obj -> unit) -> string -> IProgress


module Progress =

    [<Emit("$2.on($1,$0)")>]
    let on (handler:IProgress -> unit) (event:string) (progress:IProgress)  : unit = Fable.Core.JsInterop.import "on" "progress-stream"

    let progress (fileName:string) : IProgress = Fable.Core.JsInterop.import "Progress" "./JsInterop/ProgressIO.js"
