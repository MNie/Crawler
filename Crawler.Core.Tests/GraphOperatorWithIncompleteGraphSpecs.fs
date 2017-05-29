namespace Crawler.Core.Tests

module GraphOperatorWithIncompleteGraphSpecs =
    open Crawler.Core
    open Xunit
    open Shouldly
    let root = {Name = "root"; Url = "http://www.wp.pl/1"; Parent = None}
    let e11 = {Name = "e11"; Url = "http://www.wp.pl/2"; Parent = None}
    let e12 = {Name = "e12"; Url = "http://www.wp.pl/3"; Parent = Some(root)}
    let e21 = {Name = "e21"; Url = "http://www.wp.pl/4"; Parent = Some(e11)}
    let e22 = {Name = "e22"; Url = "http://www.wp.pl/5"; Parent = Some(e11)}
    let e31 = {Name = "e31"; Url = "http://www.wp.pl/6"; Parent = Some(e22)}
    let e41 = {Name = "e41"; Url = "http://www.wp.pl/7"; Parent = Some(e31)}
    let e41underE21 = {Name = "e41Undere21"; Url = "http://www.wp.pl/7"; Parent = Some(e21)}
    let e41underE12 = {Name = "e41undere12"; Url = "http://www.wp.pl/7"; Parent = None}
    let links = [root;e11;e12;e21;e22;e31;e41;e41underE21;e41underE12]

    let graphOperator = GraphOperator()
    [<Fact>]
    let ``when calculating size of graph``() =
        let result = graphOperator.CalculateNodes(links)
        result.ShouldBe(links |> Seq.groupBy(fun x -> x.Url) |> Seq.length)

    [<Fact>]
    let ``when calculating number of edges in graph``() =
        let result = graphOperator.CalculateEdges(links)
        result.ShouldBe(6) // count of nodes - root

    [<Fact>]
    let ``when calculating max depth of graph``() =
        let result = graphOperator.Depth(links)
        result.ShouldBe(3)

    [<Fact>]
    let ``when calculating average distance of graph``() =
        let result = graphOperator.AverageDistance(links)
        true.ShouldBe(result > 1.45)
        true.ShouldBe(result < 1.46)

    [<Fact>]
    let ``shortest paths``() =
        let result = graphOperator.ShortestPaths(links) |> List.ofSeq
        let expectedResult = 
            [
                (0, 10);
                (1, 6);
                (2, 5)
            ]
        result.ShouldBe(expectedResult)

    [<Fact>]
    let ``in paths``() =
        let result = graphOperator.InPaths(links) |> List.ofSeq
        let expectedResult = 
            [
                (0, 2);
                (1, 4);
                (2, 1)
            ]
        result.ShouldBe(expectedResult)

    [<Fact>]
    let ``out paths``() =
        let result = graphOperator.OutPaths(links) |> List.ofSeq
        let expectedResult = 
            [
                (0, 2);
                (1, 4);
                (2, 1)
            ]
        result.ShouldBe(expectedResult)

    [<Fact>]
    let ``diameter``() =
        let result = graphOperator.Diameter(links)
        result.ShouldBe(2)

    [<Fact>]
    let ``clique``() =
        let result = graphOperator.Cliques(links) |> List.ofSeq
        let expectedResult = 
            [
                (0, 7)
            ]
        result.ShouldBe(expectedResult)
    