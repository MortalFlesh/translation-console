namespace MF.TranslationConsole

open MF.TranslationConsole.Parser

module internal AdditionalTranslates =
    open System.Collections.Concurrent

    let private routes = new ConcurrentDictionary<string, string>()

    let addRoute route =
        let routeId = sprintf ":trans:%s" route
        routes.TryAdd(routeId, route) |> ignore
        routeId

    let forRoutes () =
        routes
        |> Seq.map (fun kv -> (kv.Key, kv.Value))
        |> Seq.toList

module internal Translations =
    let isAllowedExtension extension =
        [ Php.FileExtension; Twig.FileExtension ]
        |> List.contains extension

    let private (|HasTextToTranslate|_|) extension: string -> (string list) option = function
        | Twig.HasTexts extension texts -> Some texts
        | Php.HasTexts extension texts -> Some texts
        | _ -> None

    let private translateText extension text =
        let translated =
            match text with
            | Twig.Translate extension translated -> translated
            | Php.Translate AdditionalTranslates.addRoute extension translated -> translated
            | _ -> text

        Translate (text, translated)

    let private translateLine extension: string -> Translate list = function
        | HasTextToTranslate extension texts -> texts |> List.map (translateText extension)
        | _ -> []

    let private translateFile (file: File): TranslatedFile =
        {
            Path = file.Path
            Name = file.Name
            Translates =
                file.Lines
                |> List.collect (translateLine file.Extension)
                |> List.distinct
        }

    let translate (files: File list): TranslatedFile list =
        files
        |> List.map translateFile

    open System.IO

    let replaceInFile (file: TranslatedFile): Async<unit> =
        async {
            let! contents =
                file.Path
                |> File.ReadAllTextAsync
                |> Async.AwaitTask

            let contents =
                file.Translates
                |> List.fold (fun (contents: string) (Translate (original, replacement)) ->
                    contents.Replace(original, replacement)
                ) contents

            return!
                File.WriteAllTextAsync(file.Path, contents)
                |> Async.AwaitTask
        }
