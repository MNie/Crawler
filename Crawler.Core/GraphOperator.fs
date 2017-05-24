namespace Crawler.Core

type GraphOperator() =
    let CalculateEdgesToParents(element: Link) =
        if element.Parent.IsSome = true then 1
        else 0
    
    let rec CalculateDepth(element: Link) =
        if element.Parent.IsSome then 
            1 + CalculateDepth(element.Parent.Value)
        else 
            0

    member this.CalculateNodes(data: seq<Link>) =
        data
        |> Seq.distinctBy(fun x -> x.Url)
        |> Seq.length

    member this.CalculateEdges(data: seq<Link>) =
        data
        |> Seq.map(fun x -> CalculateEdgesToParents(x))
        |> Seq.sum

    member this.Depth(data: seq<Link>) =
        let root = data |> Seq.find(fun x -> x.Parent.IsNone)
        data
        |> Seq.map(fun x -> CalculateDepth(x))
        |> Seq.max

