module Crawler.App
open FSharp.Configuration
open Crawler.Core
open System.IO
open FSharp.Charting

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
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let graph = new GraphOperator()
    let result = crawler.Start()
    printf "%A" (result |> Seq.toList)
    stopwatch.Stop()
    printf "Finish calculating time of execution: %A" stopwatch.ElapsedMilliseconds
    //writeToFile result
    let nodes = graph.CalculateNodes(result)
    let edges = graph.CalculateEdges(result)
    let inEdges = graph.InPaths(result)
    let outEdges = graph.OutPaths(result)
    let shortestPaths = graph.ShortestPaths(result)
    let averagePaths = graph.AverageDistance(result)
    let maxDeep = graph.Depth(result)

    printf "nodes: %A\n" nodes
    printf "edges: %A\n" edges
    printf "inEdges: %A\n" inEdges
    printf "outEdges: %A\n" outEdges
    printf "shortes Paths: %A\n" shortestPaths
    printf "avg paths: %A\n" averagePaths
    printf "max deep: %A\n" maxDeep
    
    Chart.Bar(inEdges, "in edges").SaveChartAs("in edges.png", ChartTypes.ChartImageFormat.Png)
    Chart.Bar(outEdges, "out edges").SaveChartAs("out edges.png", ChartTypes.ChartImageFormat.Png)
    Chart.Bar(shortestPaths, "shortest paths").SaveChartAs("shortest path.png", ChartTypes.ChartImageFormat.Png)
    

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
