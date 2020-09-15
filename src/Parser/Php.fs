namespace MF.TranslationConsole.Parser

open MF.TranslationConsole

[<RequireQualifiedAccess>]
module internal Php =
    [<Literal>]
    let FileExtension = ".php"

    let (|HasTexts|_|): HasTexts =
        fun extension -> function
            | string when FileExtension |> Extension.equals extension ->
                let texts =
                    match string with
                    | string when string |> System.String.IsNullOrWhiteSpace -> None

                    | Regex "@Route\(\"\"" _ ->
                        Debug.message <| sprintf "[php empty route] %A" string
                        None

                    | Regex "@Route\(\"(\/?[\w+]+[\w+\-\/]*(?:{.*?})*[\w+\-\/]*)?\"" matches ->
                        Debug.message <| sprintf "[php route with path] %A -> %A" string matches
                        Some [string.TrimStart()]

                    | Regex "^\s*(?:\/\/|\/\**|\*){1}.*" _ ->
                        Debug.message <| sprintf "[php comment] %A" string
                        None

                    | Regex "->trans\(.*?\)" _ ->
                        Debug.message <| sprintf "[php translated] %A" string
                        None

                    | Regex "\$logger->[\w]+\(.*?" _ ->
                        Debug.message <| sprintf "[php log] %A" string
                        None

                    | Regex "(['|\"](?:[\w \-,]*[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+[\w \-,\.]*)+['|\"])+" matches ->
                        Debug.message <| sprintf "[php czech text] %A -> %A" string matches
                        Some matches

                    | Regex "(['|\"](?:[\w+]* [\w+]*)+['|\"])+" matches ->
                        Debug.message <| sprintf "[php text] %A -> %A" string matches
                        Some matches

                    | Regex "(['|\"].*?['|\"])+" _ ->
                        Debug.message <| sprintf "[php string] %A" string
                        None

                    | Regex "[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+" _ ->
                        Debug.message <| sprintf "[php czech] %A -> [???]" string
                        None

                    | _ -> None

                texts
                |> Option.map (
                    List.map (String.trim ' ')
                    >> List.filter (fun text ->
                        if text.Length > 3 then true
                        else
                            Debug.message <| sprintf "[skip php] %A" text
                            false
                    )
                )
            | _ -> None

    let (|Translate|_|) addRoute: TranslateText =
        fun extension -> function
            | string when FileExtension |> Extension.equals extension ->
                match string with
                | Regex "^(.*)@Route\(\"(.*?)\"(.*)$" [ before; route; rest ] ->
                    let fromLanguage = "cs"
                    let toLanguage = "en"

                    let routeId = addRoute route

                    Some (sprintf "%s@Route({\"%s\":\"%s\", \"%s\":\"%s\"}%s" before fromLanguage route toLanguage routeId rest)
                | _ ->
                    Some (sprintf "$this->translator->trans(%s)" string)
            | _ -> None
