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
                let toOption = List.map List.singleton

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
                        then [original; sprintf "<c:red>%s</c>" replacement]
                        else [original; sprintf "<c:pink>%s</c>" replacement]

                    translatedFiles
                    |> List.iter (fun file ->
                        match showAll, file.Translates |> List.map Translate.value with
                        | false, [] -> ()
                        | true, [] ->
                            [
                                ["<c:gray>No translates</c>"]
                            ]
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
                |> Seq.map (tee (fun (k, v) -> if output.IsDebug() then output.Message <| sprintf "- %A: %A" k v))
                |> Seq.fold (fun (target: string seq) (key, value) ->
                    target
                    |> Seq.mapi (fun index line ->
                        let replaced = line.Replace(key, value)

                        //if output.IsDebug() then
                        //    output.Message <| sprintf "<c:gray>% 3i|</c> %s  %s" index line (if replaced = line then "<c:gray>// same</c>" else "// replaced")

                        if output.IsVerbose() && replaced <> line then
                            if output.IsVeryVerbose() then
                                output.Message <| sprintf "Replaced line <c:yellow>(%s)</c>" key
                            else
                                output.Message "Replaced line"

                        replaced
                    )
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
                    | Regex "'(\/v\/.*?)'" [path] -> // url paths
                        Some [sprintf "'%s': '%s'" path path]
                    //| Regex "'(.*?)'" [text] -> // strings
                    //    Some [sprintf "'%s': '%s'" text text]
                    //| Regex "(?:\s*(\S{1}.*?)?<.+>(.*?)<\/.+?>(.*?\S{1})*\s*)+" texts ->    // text in html
                    //    Some <| (texts |> List.map (fun text -> sprintf "'%s': '%s'" text text))
                    //| Regex "^\s*([A-z\- ,]*[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ \(\)A-z0-9\?,\.\!\-:\"&;]*)" [text] -> // just text
                    //    Some [sprintf "'%s': '%s'" text text]
                    | _ -> None
                )
                |> Seq.concat
                |> Seq.distinct
                |> write

                output.Success "Done"
                ExitCode.Success
        }

        command "translation:routes" {
            Description = "Extract and translate routes of Symfony Controllers from dir."
            Help = None
            Arguments = [
                Argument.required "dir" "Dir with controllers."
                Argument.required "output" "Directory for transtlates."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let dir = input |> Input.getArgumentValue "dir"
                let outputDir = input |> Input.getArgumentValue "output"

                // todo - replace translates in Controllers - mozna to bude nakonec k nicemu a udelam to rucne...

                (* if file |> File.Exists |> not then
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
                    | Regex "'(\/v\/.*?)'" [path] -> // url paths
                        Some [sprintf "'%s': '%s'" path path]
                    //| Regex "'(.*?)'" [text] -> // strings
                    //    Some [sprintf "'%s': '%s'" text text]
                    //| Regex "(?:\s*(\S{1}.*?)?<.+>(.*?)<\/.+?>(.*?\S{1})*\s*)+" texts ->    // text in html
                    //    Some <| (texts |> List.map (fun text -> sprintf "'%s': '%s'" text text))
                    //| Regex "^\s*([A-z\- ,]*[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ]+[ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮ \(\)A-z0-9\?,\.\!\-:\"&;]*)" [text] -> // just text
                    //    Some [sprintf "'%s': '%s'" text text]
                    | _ -> None
                )
                |> Seq.concat
                |> Seq.distinct
                |> write *)

                output.Success "Done"
                ExitCode.Success
        }

        command "translation:files:copy" {
            Description = "Copy files of one language to other."
            Help = None
            Arguments = [
                Argument.required "source-language" "Language you want to copy from."
                Argument.required "target-language" "Language you want to copy to."
                Argument.requiredArray "dirs" "Directories you want to search files (.yaml)."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let toOption = List.map List.singleton

                let sourceLanguage = input |> Input.getArgumentValue "source-language"
                let targetLanguage = input |> Input.getArgumentValue "target-language"
                let dirs = input |> Input.getArgumentValueAsList "dirs"

                output.Message "Loading files ..."
                let files =
                    [
                        sprintf ".%s.yaml" sourceLanguage
                    ]
                    |> Files.loadAllFileContents dirs
                if output.IsVerbose() then
                    files
                    |> List.map (fun (path, lines) -> [ path; lines |> List.length |> string ])
                    |> output.Options "Loaded files:"
                output.Success "Done"

                output.Message "Copy files ..."
                let lang language = sprintf ".%s." language
                let progress = files |> List.length |> output.ProgressStart "Copying ..."

                files
                |> List.iter (fun (file, _) ->
                    File.Copy(file, file.Replace(lang sourceLanguage, lang targetLanguage), true)
                    progress |> output.ProgressAdvance
                )
                progress |> output.ProgressFinish
                output.Success "Done"

                ExitCode.Success
        }

        command "translation:files:replace" {
            Description = "Replace translates in files by configuration."
            Help = None
            Arguments = [
                Argument.required "language" "Language of file (must be in the file name) to replace."
                Argument.required "configuration" "Configuration of replacing (Options: k->v)."
                Argument.requiredArray "dirs" "Directories you want to search files (.yaml)."
            ]
            Options = [
                Option.noValue "force" (Some "f") "Whether to update files."
            ]
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let toOption = List.map List.singleton

                let language = input |> Input.getArgumentValue "language"
                let configuration = input |> Input.getArgumentValue "configuration"
                let dirs = input |> Input.getArgumentValueAsList "dirs"

                output.Message "Loading files ..."
                let files =
                    [
                        sprintf ".%s.yaml" language
                    ]
                    |> Files.loadAllFileContents dirs
                if output.IsVerbose() then
                    files
                    |> List.map (fun (path, lines) -> [ path; lines |> List.length |> string ])
                    |> output.Options "Loaded files:"
                output.Success "Done"

                match configuration with
                | "k->v" ->
                    output.Section "Replacing Keys to Values"
                    let progress = files |> List.length |> output.ProgressStart "Copying ..."

                    files
                    |> List.iter (fun (path, lines) ->
                        let replacedLines =
                            lines
                            |> List.map (fun line ->
                                match line.Split(":", 2) with
                                | [| key; _value |] -> sprintf "%s: %s" (key.Trim()) (key.Trim())
                                | _ -> line
                            )

                        File.WriteAllLines(path, replacedLines)
                        progress |> output.ProgressAdvance
                    )
                    progress |> output.ProgressFinish

                    output.Success "Done"
                    ExitCode.Success
                | unknown ->
                    output.Error <| sprintf "Uknonw configuration %A given." unknown
                    ExitCode.Error
        }

        command "translation:files:translate" {
            Description = "Replace translates by placeholders in files."
            Help = None
            Arguments = [
                Argument.required "language" "Language of file (must be in the file name) to replace."
                Argument.required "translates" "Directory you want to search translates in (.yaml)."
                Argument.requiredArray "dirs" "Directories you want to search files to translate."
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let toOption = List.map List.singleton

                let language = input |> Input.getArgumentValue "language"
                let translates = input |> Input.getArgumentValueAsList "translates"
                let dirs = input |> Input.getArgumentValueAsList "dirs"

                output.Message "Loading files with translates ..."
                let translateFiles =
                    [
                        sprintf ".%s.yaml" language
                    ]
                    |> Files.loadAllFileContents translates
                if output.IsVerbose() then
                    translateFiles
                    |> List.map (fun (path, lines) -> [ path; lines |> List.length |> string ])
                    |> output.Options "Loaded files with translates:"
                output.Success "Done"

                output.Message "Loading files ..."
                let files =
                    [
                        ".js"
                        ".jsx"
                    ]
                    |> Files.loadAllFileContents dirs
                if output.IsVerbose() then
                    files
                    |> List.map (fun (path, lines) -> [ path; lines |> List.length |> string ])
                    |> output.Options "Loaded files:"
                output.Success "Done"

                output.Section "Replacing translates"
                let translates =
                    translateFiles
                    |> List.collect (fun (_, lines) ->
                        lines
                        |> List.choose (fun line ->
                            match line.Split(":", 2) with
                            | [| key; value |] -> Some (key.Trim(), value.Trim())
                            | _ -> None
                        )
                    )
                    |> List.sortByDescending (snd >> String.length)

                let placeholder (key: string) = sprintf "{{%s}}" (key.Trim('\''))

                let progress = translateFiles |> List.length |> output.ProgressStart "Replacing ..."

                files
                |> List.iter (fun (path, lines) ->
                    let translatedLines =
                        lines
                        |> List.map (fun line ->
                            translates
                            |> List.fold (fun (line: string) (key, value) ->
                                if line.Contains "{{" && line.Contains "}}"
                                then line
                                else line.Replace(value.Trim('\''), placeholder key)
                            ) line
                        )

                    File.WriteAllLines(path, translatedLines)
                    progress |> output.ProgressAdvance
                )
                progress |> output.ProgressFinish

                output.Success "Done"
                ExitCode.Success
        }

        command "sql:update" {
            Description = "Replace inserts in file for updates."
            Help = None
            Arguments = [
                Argument.required "file" "File with inserts."
                Argument.required "output" "File for updates."
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
                    File.AppendAllLines(outputFile, lines)

                file
                |> File.ReadAllLines
                |> Seq.choose (function
                    | Regex "INSERT INTO (\w+) \((.*?)\) VALUES \((.*?)\)\"?[\);$]{1}" [table; columns; values] as line ->
                        let columns = columns.Split "," |> Seq.map String.trimSpace

                        match values.Split "," |> Seq.map (String.trim '\'') with
                        | values when (values |> Seq.length) = (columns |> Seq.length) ->
                            let updates =
                                values
                                |> Seq.zip columns

                            let id =
                                updates
                                |> Seq.find (fst >> (=) "id")
                                |> snd

                            let updates =
                                updates
                                |> Seq.filter (fun (column, _) -> column <> "created" && column <> "updated" && column <> "id")
                                |> Seq.map (fun (column, value) -> sprintf "%s = '%s'" column value) |> String.concat ", "

                            //printfn "- %s -" table
                            //|> Seq.iter (printfn " * %A")
                            //printfn "----------"

                            sprintf "UPDATE %s SET %s WHERE id = %s;" table updates id
                            |> Some
                        | _ ->
                            printfn "%s" line
                            None
                    | _ -> None
                )
                |> Seq.distinct
                |> write

                output.Success "Done"
                ExitCode.Success
        }
    }
    |> run argv
