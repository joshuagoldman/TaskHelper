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
    abstract getProg : string -> IProgress option
    abstract percentage : string
    abstract remaining : string 

module FileProgress =

    let getProg (fileSize : string) : IProgress option = Fable.Core.JsInterop.import "getProg" "./JsInterop/Progress.js"

    let on (handler : IProgress -> unit) (progObj : IProgress) : unit = Fable.Core.JsInterop.import "on" "./JsInterop/Progress.js"

    let fileUpload (filePath : string) (newPath : string) (progObj : IProgress) : string = Fable.Core.JsInterop.import "uploadFile" "./JsInterop/Progress.js"





