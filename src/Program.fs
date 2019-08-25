open MF.ConsoleApplication
open MF.TranslationConsole
open System.IO

[<EntryPoint>]
let main argv =
    consoleApplication {
        title "Translations"
        info ApplicationInfo.MainTitle

        command "translation:replace" {
            Description = "Replaces the texts with proper translations."
            Help = None
            Arguments = [
                Argument.requiredArray "dirs" "Directories you want to search files."
            ]
            Options = [
                Option.requiredArray "extension" (Some "e") "File extension to be filtered." (Some [])
                Option.noValue "force" (Some "f") "Whether to update files."
                Option.noValue "show-all" (Some "a") "Whether to show all files, including those with no translates."
                Option.requiredArray "ignore" (Some "i") "Patterns to ignore - it's applied on found text." (Some [])
                Option.noValue "debug-show-match" None "When there is a debug mode on, this means only lines which are match are shown."
                Option.noValue "debug-show-ignored" None "When there is a debug mode on, this means only lines which are ignored are shown."
                Option.noValue "debug-show-all" None "When there is a debug mode on, this means all lines are shown."
            ]
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let toOption = List.map (fun value -> value, "")

                let dirs = input |> Input.getArgumentValueAsList "dirs"
                let extensions = input |> Input.getOptionValueAsList "extension"
                let ignoredByUser = input |> Input.getOptionValueAsList "ignore"

                let showAll = input |> Input.isOptionValueSet "show-all"

                extensions
                |> List.filter (Translations.isAllowedExtension >> not)
                |> toOption
                |> output.Options "Not implemented extensions:"

                ignoredByUser
                |> toOption
                |> output.Options "Ignore patterns:"

                if output.IsVerbose() then
                    dirs |> toOption |> output.Options "Directories:"
                    extensions |> toOption |> output.Options "Extensions:"

                output.Message "Loading files ..."
                let files =
                    extensions
                    |> List.filter Translations.isAllowedExtension
                    |> Files.loadAllFileContents dirs
                output.Success "Done"

                output.Section "Translating files ..."
                if output.IsVerbose() then
                    files
                    |> List.map (fun (file, contents) -> [
                        file
                        contents |> List.length |> string
                    ])
                    |> output.Table [
                        sprintf "Files [%i]" (files |> List.length)
                        sprintf "Lines [%i]" (files |> List.sumBy (fun (_, lines) -> lines |> List.length))
                    ]

                let translatedFiles =
                    files
                    |> List.map File.create
                    |> Translations.translate
                    |> List.map (fun translatedFile ->
                        let filtered =
                            translatedFile.Translates
                            |> List.filter (fun translate ->
                                if ignoredByUser |> List.exists (translate |> Translate.contains) then
                                    // todo - add ignored to ConcurentDictionary<file, pattern * translate> -> show them after this step
                                    output.Message <| sprintf "<c:blue>[Ignored]</c> <c:purple>%s</c>" (translate |> Translate.original)
                                    false
                                else true
                            )

                        { translatedFile with Translates = filtered }
                    )

                let routes =
                    AdditionalTranslates.forRoutes()
                    |> List.filter (fun (_, original) ->
                        if ignoredByUser |> List.exists (fun ignore -> original.Contains(ignore)) then
                            // todo - add ignored to ConcurentDictionary<file, pattern * translate> -> show them after this step
                            output.Message <| sprintf "<c:blue>[Ignored Route]</c> <c:purple>%s</c>" original
                            false
                        else true
                    )
                output.NewLine()

                match input with
                | Input.IsSetOption "force" _ ->
                    output.Section "Replace file contents:"

                    translatedFiles
                    |> List.map Translations.replaceInFile
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore

                    output.Section "Write routes:"
                    let routeLines =
                        routes
                        |> List.map (fun (key, route) -> sprintf "%s: %s" key route)

                    System.IO.File.WriteAllLines("routes.yaml", ("# Routes" :: "# Key -> Route (this should be translated):" :: "" :: routeLines))
                | _ ->
                    let colorizeTranslate (original, replacement) =
                        if original = replacement
                        then original, sprintf "<c:red>%s</c>" replacement
                        else original, sprintf "<c:pink>%s</c>" replacement

                    translatedFiles
                    |> List.iter (fun file ->
                        match showAll, file.Translates |> List.map Translate.value with
                        | false, [] -> ()
                        | true, [] ->
                            ["<c:gray>No translates</c>", ""]
                            |> output.SimpleOptions file.Name
                        | _, translates ->
                            translates
                            |> List.map colorizeTranslate
                            |> output.SimpleOptions file.Name
                    )

                    routes
                    |> List.map colorizeTranslate
                    |> output.Options "Routes:"

                if output.IsVerbose() then
                    output.Message <| sprintf "Total translates: %i\n" (translatedFiles |> List.sumBy (fun { Translates = t} -> t |> List.length))

                if output.IsVeryVerbose() then
                    output.Section "Debug:"
                    let (showMatch, showIgnored) =
                        if input |> Input.isOptionValueSet "debug-show-all" then true, true
                        else
                            input |> Input.isOptionValueSet "debug-show-match",
                            input |> Input.isOptionValueSet "debug-show-ignored"

                    Debug.all showMatch showIgnored |> output.Messages ""

                output.Success "Done"
                ExitCode.Success
        }

        command "translation:url" {
            Description = "Create a new file from the given, and replace all translates (keys & values) with their url-safe variant."
            Help = None
            Arguments = [
                Argument.required "file" "File with translates."
                Argument.required "output" "File with URL safe translates."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let file = input |> Input.getArgumentValue "file"
                let outputFile = input |> Input.getArgumentValue "output"

                if file |> File.Exists |> not then
                    failwithf "File %s does not exits." file

                let write lines =
                    let lines = lines |> Seq.toArray
                    File.WriteAllLines(outputFile, lines)

                file
                |> File.ReadAllLines
                |> Seq.ofArray
                |> Seq.choose (function
                    | Regex "'(.*?)':\s+'(.*?)'" [key; value]
                    | Regex "\"(.*?)\":\s+'(.*?)'" [key; value]
                    | Regex "\"(.*?)\":\s+\"(.*?)\"" [key; value]
                    | Regex "'(.*?)':\s+\"(.*?)\"" [key; value]
                    | Regex "([^:]*?):([^:]*)" [key; value] -> Some (key, value)
                    | _ -> None
                )
                |> Seq.map (fun (key, value) ->
                    sprintf "'%s': '%s'"
                        (key.Trim() |> String.toUrlSafe)
                        (value.Trim() |> String.toUrlSafe)
                )
                |> write

                output.Success "Done"
                ExitCode.Success
        }

        command "translation:use" {
            Description = "Use translated file and replace values by keys."
            Help = None
            Arguments = [
                Argument.required "translated" "File with translates."
                Argument.required "target" "File with not-translated values."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let translated = input |> Input.getArgumentValue "translated"
                let target = input |> Input.getArgumentValue "target"

                if translated |> File.Exists |> not then
                    failwithf "File %s does not exits." translated
                if target |> File.Exists |> not then
                    failwithf "File %s does not exits." translated

                let write lines =
                    let lines = lines |> Seq.toArray
                    //lines
                    //|> List.ofSeq
                    //|> output.Messages "  "
                    File.WriteAllLines(target, lines)

                let targetLines =
                    target
                    |> File.ReadAllLines
                    |> Seq.ofArray

                translated
                |> File.ReadAllLines
                |> Seq.ofArray
                |> Seq.sortBy String.length
                |> Seq.rev
                |> Seq.choose (function
                    | Regex "'(.*?)':\s+'(.*?)'" [key; value]
                    | Regex "\"(.*?)\":\s+'(.*?)'" [key; value]
                    | Regex "\"(.*?)\":\s+\"(.*?)\"" [key; value]
                    | Regex "'(.*?)':\s+\"(.*?)\"" [key; value]
                    | Regex "([^:]*?):([^:]*)" [key; value] -> Some (key, value)
                    | _ -> None
                )
                |> Seq.fold (fun (target: string seq) (key, value) ->
                    target
                    |> Seq.map (fun line -> line.Replace(key, value))
                ) targetLines
                |> write

                output.Success "Done"
                ExitCode.Success
        }

        command "translation:extract" {
            Description = "Extract translates from file."
            Help = None
            Arguments = [
                Argument.required "file" "File with texts."
                Argument.required "output" "File for transtlates."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let file = input |> Input.getArgumentValue "file"
                let outputFile = input |> Input.getArgumentValue "output"

                if file |> File.Exists |> not then
                    failwithf "File %s does not exits." file

                let write lines =
                    let lines = lines |> Seq.toArray
                    //lines
                    //|> List.ofSeq
                    //|> output.Messages "  "
                    File.WriteAllLines(outputFile, lines)

                file
                |> File.ReadAllLines
                |> Seq.choose (function
                    | Regex "'(.*?)'" [text] -> // strings
                        Some [sprintf "'%s': '%s'" text text]
                    | Regex "(?:\s*(\S{1}.*?)?<.+>(.*?)<\/.+?>(.*?\S{1})*\s*)+" texts ->    // text in html
                        Some <| (texts |> List.map (fun text -> sprintf "'%s': '%s'" text text))
                    | Regex "^\s*([A-z\- ,]*[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ \(\)A-z0-9\?,\.\!\-:\"&;]*)" [text] -> // just text
                        Some [sprintf "'%s': '%s'" text text]
                    | _ -> None
                )
                |> Seq.concat
                |> Seq.distinct
                |> write

                output.Success "Done"
                ExitCode.Success
        }
    }
    |> run argv
