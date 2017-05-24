namespace RobotsParser.Helpers

module Helper =
    let isAllowed(toCheckUrl: string, disallowedRoutes, parent: string) =
        let invalidStart postfix = sprintf "%s%s" parent postfix
        (disallowedRoutes |> Seq.forall (fun y -> not (toCheckUrl.StartsWith (invalidStart y))))