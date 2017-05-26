namespace Crawler.Core
open FSharp.Data
open FSharp.Collections.ParallelSeq
open System
open RobotsParser.Core
open RobotsParser.Helpers

type Link =
    {
        Name: string
        Url: string
        Parent: Link Option
    }

type Crawler(uri: Uri, robotsPostFix: string) = 
    let mutable BaseUrl = sprintf "%A" uri 
    let mutable RobotsPostFix = robotsPostFix
    
    let getBody parentUrl =
        let robotsUrl = sprintf "%s/%s" parentUrl RobotsPostFix
        let response =
            Http.RequestString(
                robotsUrl
            )
        response

    let getAllLinks parent = 
        let robotsBody = getBody parent.Url
        let robots = RobotsTxt(robotsBody)
        let disallowedRoutes = robots.GetDisallowRoutes()

        HtmlDocument.Load(parent.Url)
        |> fun x -> x.Descendants ["a"]
        |> Seq.choose (fun x -> 
            x.TryGetAttribute("href")
            |> Option.map (fun a -> { 
                    Name = x.InnerText();
                    Url = a.Value();
                    Parent = Some(parent)
                })
        )
        |> Seq.filter (fun x -> 
                (Helper.isAllowed(x.Url, disallowedRoutes, parent.Url))
                &&
                x.Url.StartsWith(BaseUrl)
            )
        |> Seq.append [parent]

    let rec getLinksRecursively parent maxDeep =
        if maxDeep > 1 then seq {
            yield getAllLinks parent
            yield! getLinksRecursively parent (maxDeep - 1) }
        else seq { yield getAllLinks parent }

    member this.Start() =
        let root = {Name="root"; Url = BaseUrl; Parent = None}
        getAllLinks root
        |> Seq.map (fun x -> getLinksRecursively(x)(5))
        |> Seq.collect id
        |> Seq.collect id
