namespace RobotsParser.Core
open System

type RobotsTxt(content: string) = 
    let mutable _content = content
    let disallowConst = "Disallow:"
    
    member this.GetDisallowRoutes () =
        _content.Split('\n')
        |> Seq.filter (fun (x: string) -> x.Trim().StartsWith(disallowConst, StringComparison.OrdinalIgnoreCase))
        |> Seq.filter (fun (x: string) -> String.IsNullOrWhiteSpace x |> not)
        |> Seq.map (fun x -> x.Replace(disallowConst, "").Trim())

