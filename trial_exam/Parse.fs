(* File Expr/Parse.fs *)
(* Lexing and parsing of simple expressions using fslex and fsyacc *)

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Absyn
open RePar
open ReLex

(* Plain parsing from a string, with poor error reporting *)

let lexString (str: string) : unit =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    let rec inner lexbuf =
        printfn "%A" (Token lexbuf)
        inner lexbuf

    try
      inner lexbuf
    with
      | exn -> ()

let fromString (str : string) =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try
      Main Token lexbuf
    with
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s near line %d, column %d\n" 
                  (exn.Message) (pos.Line+1) pos.Column

(* Parsing from a text file *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
      Main Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s in file %s near line %d, column %d\n" 
                  (exn.Message) filename (pos.Line+1) pos.Column

