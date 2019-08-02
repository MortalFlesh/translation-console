namespace MF.TranslationConsole.Parser

open MF.TranslationConsole

[<RequireQualifiedAccess>]
module internal Html =
    [<Literal>]
    let FileExtension = ".html"

    let private containsMarkup string =
        match string with
        | Regex "[<>]+" _
        | Regex "(&&|\|\||==|\(\)|\-\-)+" _ ->
            Debug.message <| sprintf "[html contains markup] %A" string
            true
        | _ -> false

    let (|PlainText|_|) = function
        | string when string |> containsMarkup |> not -> string.Trim ' ' |> Some
        | _ -> None

    let (|HasTexts|_|): HasTexts =
        fun extension -> function
            | string when FileExtension |> Extension.equals extension ->
                match string with
                | Regex "^\s*<!--.*?-->\s*$" _ ->
                    Debug.message <| sprintf "[html comment] %A" string
                    None

                | Regex "(?:\s*(\S{1}.*?)?<.+>(.*?)<\/.+?>(.*?\S{1})*\s*)+" matches ->
                    Debug.message <| sprintf "[html text and tag] %A -> %A" string matches
                    Some matches

                | Regex "<.+>(.*?)<\/.+?>" matches ->
                    Debug.message <| sprintf "[html singleTag] %A -> %A" string matches
                    Some matches

                | Regex "^\s*>(.*?)<\/.+?>" matches ->
                    Debug.message <| sprintf "[html singleTag partial] %A -> %A" string matches
                    Some matches

                | Regex "(.*?)<br.*?>" matches ->
                    Debug.message <| sprintf "[html text before br] %A -> %A" string matches
                    Some matches

                | Regex "^\s*<.+>\s*$" _ ->
                    Debug.message <| sprintf "[html tag] %A" string
                    None

                | Regex "\\w+=\".+?\"" _ ->
                    Debug.message <| sprintf "[html attribute] %A" string
                    None

                | Regex "^[a-z0-9\-]+$" _ ->
                    Debug.message <| sprintf "[css class] %A" string
                    None

                | PlainText text ->
                    Debug.message <| sprintf "[html plaintext] %A -> [%A]" string text
                    Some [ text ]

                | _ -> None
            | _ -> None
