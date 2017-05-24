namespace RobotsParser.Tests.Base
module HelperSpecs =
    open RobotsParser.Helpers
    open Xunit
    open Shouldly

    let parent = "http://www.github.io"
    let linkToCheck = "http://www.github.io/dd"
    
    [<Fact>]
    let ``should not match postfix and return true``() =
        let result = Helper.isAllowed(linkToCheck, [|"/de"; "/nenenen"|], parent)
        result.ShouldBeTrue()

    [<Fact>]
    let ``should match postfix and return false``() =
        let result = Helper.isAllowed(linkToCheck, [|"/de"; "/nenenen"; "/dd"|], parent)
        result.ShouldBeFalse()
