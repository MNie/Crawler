module Crawler.App
open FSharp.Configuration
open Crawler.Core
open System.IO
open FSharp.Charting

type Settings = AppSettings<"app.config">

let prettyPrint content fileName =
    content
    |> Seq.iter (fun x -> File.AppendAllLines(fileName, [sprintf "%A" x]))

let writeToFile content file =
    let stringify = sprintf "%s: %A" file content
    prettyPrint(content)(sprintf "%s.txt" file) |> ignore
    printf "%s" stringify
    

[<EntryPoint>]
let main argv =
    let crawler = new Crawler(Settings.Url, Settings.Robots)
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let graph = new GraphOperator()
    let result = crawler.Start()
    printf "%A" (result |> Seq.toList)
    stopwatch.Stop()
    printf "Finish calculating time of execution: %A" stopwatch.ElapsedMilliseconds
    
    let nodes = graph.CalculateNodes(result)
    let edges = graph.CalculateEdges(result)
    let inEdges = graph.InPaths(result)
    let outEdges = graph.OutPaths(result)
    let shortestPaths = graph.ShortestPaths(result)
    let averagePaths = graph.AverageDistance(result)
    let clasterization = graph.Clasterization(result)
    let diameter = graph.Diameter(result)
    let maxDeep = graph.Depth(result)
    let cliques = graph.Cliques(result)

    writeToFile [nodes] "nodes"
    writeToFile [edges] "edges"
    writeToFile inEdges "in"
    writeToFile outEdges "out"
    writeToFile shortestPaths "shortest"
    writeToFile [averagePaths] "avg"
    writeToFile clasterization "claster"
    writeToFile [diameter] "diameter"
    writeToFile [maxDeep] "deep"
    writeToFile cliques "cliques"
    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
