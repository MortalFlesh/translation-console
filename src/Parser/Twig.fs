namespace MF.TranslationConsole.Parser

open MF.TranslationConsole

[<RequireQualifiedAccess>]
module internal Twig =
    [<Literal>]
    let FileExtension = ".html.twig"

    let (|HasTexts|_|): HasTexts =
        fun extension -> function
            | string when FileExtension |> Extension.equals extension ->
                let texts =
                    match string with
                    | string when string |> System.String.IsNullOrWhiteSpace -> None

                    | Regex "^\s*{#.*?#}\s*$" _ ->
                        Debug.message <| sprintf "[twig comment] %A" string
                        None

                    | Regex "^\s*(?:and|or).+\s*$" _ ->
                        Debug.message <| sprintf "[twig condition] %A" string
                        None

                    | Regex "^\s*([A-z ]+)\s*$" matches ->
                        Debug.message <| sprintf "[twig text] %A -> %A" string matches
                        Some matches

                    | Regex "^\s*([A-z\- ,]*[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ \(\)A-z0-9\?,\.\!\-:\"]*)" matches ->
                        Debug.message <| sprintf "[twig czech text] %A -> %A" string matches
                        Some matches

                    | Regex "('(?:[\w+ ]+[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ ]+[\w+\:\?\-]+?)+')+" matches ->
                        Debug.message <| sprintf "[twig czech strings] %A -> %A" string matches
                        Some matches

                    | Regex "(?:title|alt|content)=\"([^{}]*?)\"" matches ->
                        Debug.message <| sprintf "[html attribute with text] %A -> %A" string matches
                        Some matches

                    | Regex "^\s*{{\-?[\w'\\.\\(\\) ]+\-?}}\s*$" _ ->
                        Debug.message <| sprintf "[twig var] %A" string
                        None

                    | Regex "{%\-? trans.*?%}(.*?){%.*?%}" _ ->
                        Debug.message <| sprintf "[twig translated] %A" string
                        None

                    | Regex "{%.*?%}([^\"'=]+?){%.*?%}" matches ->
                        Debug.message <| sprintf "[twig marks with content] %A -> %A" string matches
                        Some matches

                    | Regex "^[{%}\- \(\)\[\]\w\\\/:\"',\.\|=\?~;+]+$" _ ->
                        Debug.message <| sprintf "[twig partial marks] %A" string
                        None

                    | Regex "^\s*{?\s*[\w'\"]+:\s*[A-z0-9'\\.]+,?\s*}?\s*$" _  ->
                        Debug.message <| sprintf "[twig attribute] %A" string
                        None

                    | Html.HasTexts (Extension Html.FileExtension) texts -> Some texts
                    | _ -> None

                let filterText (text: string) =
                    match text with
                    | text when text.Length < 3 ->
                        Debug.message <| sprintf "[skip too short] %A" text
                        false
                    | Regex "^[\.,\-_\s]+$" _ ->
                        Debug.message <| sprintf "[skip only signs] %A" text
                        false
                    | Regex "^&[a-z]+;$" _ ->
                        Debug.message <| sprintf "[skip html entity] %A" text
                        false
                    | Regex "(?:true|false|null)+" _ ->
                        Debug.message <| sprintf "[skip bool|null] %A" text
                        false
                    | Regex "^https?://$" _ ->
                        Debug.message <| sprintf "[skip only html] %A" text
                        false
                    | Regex "^(?:\s*[a-z]+[_\-]+[{ ]*[a-z0-9\-_]*[a-z0-9]+[} ]*\s*)+$" _ ->
                        Debug.message <| sprintf "[skip css class] %A" text
                        false
                    | Regex "(!important|@media)+" _ ->
                        Debug.message <| sprintf "[skip css] %A" text
                        false
                    | Regex "(plural\()+" _ ->
                        Debug.message <| sprintf "[skip plural] %A" text
                        false
                    | Regex "&[\w]{1,7};$" _ -> true    // ends with html entity -> keep
                    | Regex ";$" _ ->
                        Debug.message <| sprintf "[skip ends with ;] %A" text
                        false
                    | Regex "^[{%-]{2,3}.*?[}%-]{2,3}$" _ ->
                        Debug.message <| sprintf "[skip twig var] %A" text
                        false
                    | Regex "^<.+>$" _ ->
                        Debug.message <| sprintf "[skip html tag] %A" text
                        false
                    | Regex "^{\[{.*?}\]}$" _ ->
                        Debug.message <| sprintf "[skip angular var] %A" text
                        false
                    | Regex "^[# \d\.,]+$" _ ->
                        Debug.message <| sprintf "[skip numbers] %A" text
                        false
                    | _ -> true

                texts
                |> Option.map (
                    List.map (String.trim ':')
                    >> List.filter filterText
                    >> List.map (fun text ->
                        match text with
                        | Regex "^\s*(?:<a )?href=\".*?\".*?>(.*?)(?:<.*)?$" [ link ] ->
                            Debug.message <| sprintf "[link text] %A -> [%A]" text link
                            link
                        | Regex "^\s*style=\".*?\".*?>(.*?)(?:<.*)?$" [ styledText ] ->
                            Debug.message <| sprintf "[styled text] %A -> [%A]" text styledText
                            styledText
                        | _ -> text
                    )
                    >> List.filter filterText
                )
            | _ -> None

    let (|Translate|_|): TranslateText =
        fun extension -> function
            | string when FileExtension |> Extension.equals extension ->
                match string with
                | Regex "^'.*?'$" _ -> Some (sprintf "%s | trans" string)
                | _ -> Some (sprintf "{%% trans %%}%s{%% endtrans %%}" string)
            | _ -> None
