open MF.ConsoleApplication

[<EntryPoint>]
let main argv =
    consoleApplication {
        title "Translations"
    }
    |> run argv
