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
            Http.AsyncRequestString(
                robotsUrl
            )
        response

    let getRobots startRobots =
        let robotsBody = 
            try
                Async.RunSynchronously startRobots
            with
                | _ -> ""
        let robots = RobotsTxt(robotsBody)
        robots.GetDisallowRoutes()

    let getValidLinks parent (allLinks: seq<HtmlNode>) =
        allLinks
        |> PSeq.choose (fun x ->
                x.TryGetAttribute("href")
                |> Option.map (fun a -> { 
                        Name = x.InnerText();
                        Url = a.Value();
                        Parent = Some(parent)
                    })
            )

    let filterNotAllowed disallowedRoutes parent data =
        data
        |> PSeq.filter (fun x ->
                (Helper.isAllowed(x.Url, disallowedRoutes, parent.Url))
                &&
                x.Url.StartsWith(BaseUrl)
                &&
                not (x.Url.Contains("@"))
            )

    let getAllLinks parent = async {
            let! startRobots = Async.StartChild (getBody parent.Url)
            let! startDownload = Async.StartChild (HtmlDocument.AsyncLoad(parent.Url))

            try
                let disallowedRoutes = getRobots(startRobots)
                let! downloadedSite = startDownload
            
                let result = 
                    downloadedSite
                    |> fun x -> x.Descendants ["a"]
                    |> getValidLinks parent
                    |> filterNotAllowed disallowedRoutes parent
                    |> PSeq.append [parent]
                return result
            with
                | _ -> return [] |> PSeq.ofList
        }

    let rec getLinksRecursively maxDeep parent =
        if maxDeep > 1 then seq {
            yield getAllLinks parent
            yield! getLinksRecursively (maxDeep - 1) parent }
        else seq { yield getAllLinks parent }

    member this.Start() =
        let root = {Name="root"; Url = BaseUrl; Parent = None}

        Async.RunSynchronously (getAllLinks(root))
        |> PSeq.map (fun x -> getLinksRecursively(10)(x))
        |> PSeq.collect id
        |> PSeq.collect(fun x -> Async.RunSynchronously x)
