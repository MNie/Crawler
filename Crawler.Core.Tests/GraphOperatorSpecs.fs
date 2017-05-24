namespace Crawler.Core.Tests

module GraphOperatorSpecs =
    open Crawler.Core
    open Xunit
    open Shouldly
    let root = {Name = "root"; Url = "http://www.wp.pl/1"; Parent = None}
    let e11 = {Name = "e11"; Url = "http://www.wp.pl/2"; Parent = Some(root)}
    let e12 = {Name = "e12"; Url = "http://www.wp.pl/3"; Parent = Some(root)}
    let e21 = {Name = "e21"; Url = "http://www.wp.pl/4"; Parent = Some(e11)}
    let e22 = {Name = "e22"; Url = "http://www.wp.pl/5"; Parent = Some(e11)}
    let e31 = {Name = "e31"; Url = "http://www.wp.pl/6"; Parent = Some(e22)}
    let e41 = {Name = "e41"; Url = "http://www.wp.pl/7"; Parent = Some(e31)}
    let e41underE21 = {Name = "e41Undere21"; Url = "http://www.wp.pl/7"; Parent = Some(e21)}
    let e41underE12 = {Name = "e41undere12"; Url = "http://www.wp.pl/7"; Parent = Some(e12)}
    let links = [root;e11;e12;e21;e22;e31;e41]

    let graphOperator = GraphOperator()
    [<Fact>]
    let ``when calculating size of graph``() =
        let result = graphOperator.CalculateNodes(links)
        result.ShouldBe(links.Length)

    [<Fact>]
    let ``when calculating number of edges in graph``() =
        let result = graphOperator.CalculateEdges(links)
        result.ShouldBe(links.Length - 1) // count of nodes - root

    [<Fact>]
    let ``when calculating max depth of graph``() =
        let result = graphOperator.Depth(links)
        result.ShouldBe(4)

    