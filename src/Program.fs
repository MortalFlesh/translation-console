open MF.ConsoleApplication

[<EntryPoint>]
let main argv =
    consoleApplication {
        title "Translations"

        command "translation:replace" {
            Description = "Replaces the texts with proper translations."
            Help = None
            Arguments = []
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                ExitCode.Success
        }
    }
    |> run argv
