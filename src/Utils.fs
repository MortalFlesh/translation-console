namespace MF.TranslationConsole

[<AutoOpen>]
module internal Utils =
    let tee f a =
        f a
        a

[<RequireQualifiedAccess>]
module internal Async =
    /// Lift a function to Async
    let map f xA =
        async {
        let! x = xA
        return f x
        }

    /// Lift a value to Async
    let retn x =
        async.Return x

    /// Apply an Async function to an Async value
    let apply fA xA =
        async {
         // start the two asyncs in parallel
        let! fChild = Async.StartChild fA  // run in parallel
        let! x = xA
        // wait for the result of the first one
        let! f = fChild
        return f x
        }

    /// Apply a monadic function to an Async value
    let bind f xA = async.Bind(xA,f)



module File =
    open System.IO

    type FilePath = string

    let private hasExtension (name: FilePath) (extension: string) =
        name.EndsWith(extension)

    let private fileHasExtension extensions name =
        match extensions with
        | [] -> true
        | extensions ->
            extensions
            |> List.exists (name |> hasExtension)

    let findAll (dirs: string list) (extensions: string list): FilePath list =
        let rec getAllFiles = function
            | [] -> []
            | directories -> [
                yield! directories |> Seq.collect Directory.EnumerateFiles |> Seq.filter (fileHasExtension extensions)
                yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
            ]

        getAllFiles dirs

    type Lines = string list

    let private readAllLinesAsync file: Async<FilePath * Lines> =
        file
        |> File.ReadAllLinesAsync
        |> Async.AwaitTask
        |> Async.map (fun lines -> file, lines |> Array.toList)

    let loadContents (files: FilePath list) =
        files
        |> List.map readAllLinesAsync
        |> Async.Parallel

    let loadAllFileContents dirs extensions =
        findAll dirs extensions
        |> loadContents
        |> Async.RunSynchronously
        |> Array.toList
