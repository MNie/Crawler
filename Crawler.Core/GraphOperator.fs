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

    let groupData(data: seq<Link>) =
        data 
        |> Seq.groupBy(fun x -> x.Url) 
        |> Seq.map(fun x -> (fst x, snd x |> Seq.filter(fun y -> y.Parent.IsSome) |> Seq.map(fun y -> y.Parent.Value.Url)))
       
    let formatResult result = 
        result
        |> Seq.map(fun x -> snd x |> Seq.length)
        |> Seq.groupBy id
        |> Seq.map(fun x -> (fst x, snd x |> Seq.length))
        |> Seq.sortBy(fun x -> fst x)

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

    member this.InPaths(data: seq<Link>) =
        groupData data
        |> formatResult

    member this.OutPaths(data: seq<Link>) =
        data
        |> Seq.filter(fun x -> x.Parent.IsSome)
        |> Seq.map(fun x -> x.Parent.Value.Url)
        |> Seq.groupBy id
        |> formatResult
        |> Seq.append [(0,1)]

    member this.ShortestPaths(data: seq<Link>): seq<int*int> =
        let groupedData = groupData data

        let nOfElements = groupedData |> Seq.length
        let lastIndex = nOfElements |> (+) -1
        let matrice = Array2D.zeroCreate nOfElements nOfElements
        let getElem index = groupedData |> Seq.item index
        
        for i in [0..lastIndex] do
            for j in [0..lastIndex] do
                let contains url = snd (getElem i) |> Seq.contains url
                if fst (getElem j) |> contains then 
                    matrice.[i, j] <- 1
                    matrice.[j, i] <- 1
        
        for k in [0..lastIndex] do
            for i in [0..lastIndex] do
                for j in [0..lastIndex] do
                    let shouldReplace =
                        let newValue = (matrice.[i, k] + matrice.[k, j])
                        let isShorter = matrice.[i, j] > newValue && newValue > 0
                        let isEmpty = matrice.[i, j] = 0 
                        let hasConnection = matrice.[i, k] <> 0 && matrice.[k, j] <> 0
                        (isShorter || isEmpty) && hasConnection
                    
                    if shouldReplace then
                        matrice.[i, j] <- matrice.[i, k] + matrice.[k, j]
        
        let partialResult = 
            [1..lastIndex]
            |> Seq.map(fun index -> 
                matrice.[index, 0..index - 1]
                |> Seq.countBy id
            )

        partialResult
        |> Seq.collect id
        |> Seq.groupBy fst
        |> Seq.map(fun x -> (fst x, snd x |> Seq.sumBy(fun y -> snd y)))

    member this.AverageDistance(data: seq<Link>) =
        let calculation = this.ShortestPaths(data)
        let divideBy = calculation |> Seq.sumBy(fun x -> snd x) |> float
        let result = 
            calculation
            |> Seq.map(fun x -> fst x * snd x)
            |> Seq.sum
            |> float
        result / divideBy
                


