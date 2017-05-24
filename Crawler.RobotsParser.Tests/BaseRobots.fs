namespace RobotsParser.Tests.Base
module BaseRobotsTests =
    open RobotsParser.Core
    open Xunit
    open Shouldly

    let linkOne = "/NR/exeres"
    let linkTwo = "/modello"

    let file = 
        sprintf "
            User-agent: *
            Disallow: %s
            Disallow: %s
        " linkOne linkTwo
    
    [<Fact>]
    let ``check base robots txt file``() =
        let subject = new RobotsTxt(file)
        let result = subject.GetDisallowRoutes()
        result.ShouldBe [|linkOne; linkTwo|]
