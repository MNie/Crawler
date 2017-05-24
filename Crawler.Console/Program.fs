module Crawler.App
open FSharp.Configuration
open Crawler.Core
open System.IO

type Settings = AppSettings<"app.config">

let prettyPrint content =
    content
    |> Seq.iter (fun x -> File.AppendAllLines("result.txt", [sprintf "Name: %s, Url: %s, Parent: %A" x.Name x.Url x.Parent]))

let writeToFile content =
    let stringify = sprintf "%A" content
    prettyPrint(content) |> ignore
    printf "%s" stringify
    

[<EntryPoint>]
let main argv =
    let crawler = new Crawler(Settings.Url, Settings.Robots)
    let result = crawler.Start()
    writeToFile result
    0 // return an integer exit code
