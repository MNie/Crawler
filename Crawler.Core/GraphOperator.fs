namespace Crawler.Core

open System

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
        |> Seq.toList
       
    let formatResult result = 
        result
        |> Seq.map(fun x -> snd x |> Seq.length)
        |> Seq.groupBy id
        |> Seq.map(fun x -> (fst x, snd x |> Seq.length))
        |> Seq.sortBy(fun x -> fst x)

    let createMatrice data lastIndex =
        let matrice = Array2D.zeroCreate (lastIndex + 1) (lastIndex + 1)
        let getElem index = data |> Seq.item index
        
        for i in [0..lastIndex] do
            for j in [0..lastIndex] do
                let contains url = snd (getElem i) |> Seq.contains url
                if fst (getElem j) |> contains then 
                    matrice.[i, j] <- 1
                    matrice.[j, i] <- 1
        matrice

    let shortestPathsMatrice data =
        let groupedData = groupData(data)
        
        let nOfElements = groupedData |> Seq.length
        let lastIndex = nOfElements |> (+) -1
        let matrice = createMatrice groupedData lastIndex

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
        matrice, lastIndex

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
        let getElementsWithOut() = 
            data
            |> Seq.filter(fun x -> x.Parent.IsSome)
            |> Seq.map(fun x -> x.Parent.Value.Url)
            |> Seq.toList
        let elementsWithOut = getElementsWithOut()

        let elementsWithInButWithoutOut =
            data
            |> Seq.distinctBy(fun x -> x.Url)
            |> Seq.filter(fun x -> 
                elementsWithOut 
                |> Seq.filter(fun y -> y = x.Url)
                |> Seq.isEmpty
            )
            |> Seq.length

        elementsWithOut
        |> Seq.groupBy id
        |> formatResult
        |> Seq.append [0, elementsWithInButWithoutOut]

    member this.Clasterization(data: seq<Link>) =
        let groupedData = groupData(data)
        
        let nOfElements = groupedData |> Seq.length
        let lastIndex = nOfElements |> (+) -1
        let matrice = createMatrice groupedData lastIndex

        [0..lastIndex]
        |> Seq.mapi(fun i z ->
            let neighbors = 
                matrice.[i, 0..lastIndex]
                |> Seq.mapi(fun j x -> 
                    if j = i then (j, 0)
                    else (j, x)
                )
                |> Seq.filter(fun x -> snd x = 1)
                |> Seq.map fst
                |> Array.ofSeq
            
            let edgesBetween = 
                neighbors
                |> Seq.mapi(fun j x -> 
                    neighbors
                    |> Seq.mapi(fun k y ->
                        let hasConnection =
                            matrice.[neighbors.[j], neighbors.[k]] = 1
                        if j <> k && hasConnection then 1
                        else 0
                    )
                    |> Seq.sum
                )
                |> Seq.sum
                |> float
            let neighborsCount = neighbors |> Seq.length |> float
            
            if neighbors |> Seq.length < 2 then (i, 0.0)
            else (i, edgesBetween/(neighborsCount * (neighborsCount - 1.0)))
        )
        |> Seq.groupBy snd
        |> Seq.map(fun x -> (Math.Round(fst x, 2), snd x |> Seq.length))
        |> Seq.sortBy fst
        
    member this.ShortestPaths(data: seq<Link>): seq<int*int> =
        let matrice, lastIndex = shortestPathsMatrice data
        
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

    member this.Diameter(data: seq<Link>) =
        this.ShortestPaths(data)
        |> Seq.maxBy(fun x -> fst x)
        |> fst

    member this.AverageDistance(data: seq<Link>) =
        let filterNoConnections toFilter =
            toFilter
            |> Seq.filter(fun x -> fst x <> 0)

        let calculation = this.ShortestPaths(data)
        let divideBy = calculation |> filterNoConnections |> Seq.sumBy(fun x -> snd x) |> float
        let result = 
            calculation
            |> filterNoConnections
            |> Seq.map(fun x -> fst x * snd x)
            |> Seq.sum
            |> float
        result / divideBy

    member this.Cliques(data: seq<Link>) =
        let matrice, lastIndex = shortestPathsMatrice data

        [0..lastIndex]
        |> Seq.mapi(fun i x -> 
            [0..lastIndex]
            |> Seq.mapi(fun j x ->
                if i <> j then matrice.[i, j] + matrice.[j, i]
                else Int32.MaxValue
            )
            |> Seq.minBy id
        )
        |> Seq.map(fun x ->
            if x = Int32.MaxValue then 0
            else x
        )
        |> Seq.groupBy id
        |> Seq.map(fun x -> (fst x, snd x |> Seq.length))


