open MF.ConsoleApplication
open MF.TranslationConsole

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
                Option.requiredArray "extension" (Some "e") "File extension to be filtered." None
            ]
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let toOption = List.map (fun value -> value, "")

                let dirs = input |> Input.getArgumentValueAsList "dirs"
                let extensions = input |> Input.getOptionValueAsList "extension"

                let files = File.loadAllFileContents dirs extensions

                if output.IsVerbose() then
                    dirs |> toOption |> output.Options "Directories:"
                    extensions |> toOption |> output.Options "Extensions:"

                    files
                    |> List.map (fun (file, contents) -> [
                        file
                        contents |> List.length |> string
                    ])
                    |> output.Table ["File"; "Lines"]

                output.Success "Done"
                ExitCode.Success
        }
    }
    |> run argv
