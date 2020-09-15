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

type FilePath = string
type Lines = string list

module Files =
    open System.IO

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

[<RequireQualifiedAccess>]
module FileSystem =
    open System.IO

    let private writeContent (writer: StreamWriter) content =
        writer.WriteLine(sprintf "%s" content)

    let writeSeqToFile (filePath: string) (data: string seq) =
        File.WriteAllLines(filePath, data)

    let writeToFile (filePath: string) data =
        File.WriteAllText(filePath, data)

    let appendToFile (filePath: string) data =
        File.AppendAllText(filePath, data)

    let readLines (filePath: string) =
        File.ReadAllLines(filePath)
        |> Seq.toList

    let readContent (filePath: string) =
        File.ReadAllText(filePath)

    let tryReadContent (filePath: string) =
        if File.Exists filePath then File.ReadAllText(filePath) |> Some
        else None

    let rec getAllDirs = function
        | [] -> []
        | directories -> List.distinct [
            let childrenDirs = directories |> Seq.collect Directory.EnumerateDirectories

            yield! directories |> List.filter Directory.Exists
            yield! childrenDirs
            yield! childrenDirs |> List.ofSeq |> getAllDirs
        ]

    let rec getAllFiles = function
        | [] -> []
        | directories -> [
            yield! directories |> Seq.collect Directory.EnumerateFiles
            yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
        ]

    let copyDir target source =
        if source |> Directory.Exists |> not then failwithf "Source %s does not exists." source
        if target |> Directory.Exists then failwithf "Target %s already exists." target

        let mkdir = Directory.CreateDirectory >> ignore
        let replacePath (path: string) = path.Replace(source, target)

        target |> mkdir

        [ source ]
        |> getAllDirs
        |> List.iter (replacePath >> mkdir)

        [ source ]
        |> getAllFiles
        |> List.iter (fun path -> File.Copy(path, replacePath path, true))

[<RequireQualifiedAccess>]
module Directory =
    let depth (path: string) =
        path.Trim('/').Split "/" |> Seq.length

[<RequireQualifiedAccess>]
module String =
    open System

    let isNullOrEmpty (string: string) =
        string |> String.IsNullOrWhiteSpace

    let trim (char: char) (string: string) =
        string.Trim().Trim(char)

    let trimSpace (string: string) =
        string.Trim()

    let toUrlSafe (string: string) =
        let (=>) a b = a, b
        let replacements = [
            ['á'; 'Á'] => 'a'
            ['č'; 'Č'] => 'c'
            ['ď'; 'Ď'] => 'd'
            ['ě'; 'Ě'] => 'e'
            ['é'; 'É'] => 'e'
            ['í'; 'Í'] => 'i'
            ['ó'; 'Ó'] => 'o'
            ['ř'; 'Ř'] => 'r'
            ['š'; 'Š'] => 's'
            ['ť'; 'Ť'] => 't'
            ['ů'; 'Ů'] => 'u'
            ['ú'; 'Ú'] => 'u'
            ['ý'; 'Ý'] => 'y'
            ['ž'; 'Ž'] => 'z'
            ['ľ'; 'l'] => 'l'
            ['Ľ'; 'L'] => 'l'
            ['.'; '?'; '!'; ','; '_'; ':'; '\''; '"'; ' '] => '-'
        ]

        replacements
        |> List.fold (fun string (keys, replace) ->
            keys
            |> List.fold (fun (string: string) key ->
                string.Replace(key, replace)
            ) string
        ) (string.ToLower())

[<AutoOpen>]
module internal Regex =
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let internal (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then
            List.tail [ for g in m.Groups -> g.Value ]
            |> List.filter (String.isNullOrEmpty >> not)
            |> Some
        else None

[<RequireQualifiedAccess>]
module internal Debug =
    open System.Collections.Concurrent

    let private debugLog = new ConcurrentBag<string>()

    let message string =
        debugLog.Add(string)

    let all showMatch showIgnored =
        debugLog
        |> Seq.map (function
            | Regex "^\[(.*?)\]\s(.*?)\s*->\s\[(.*?)\]\s*$" [ title; original; texts ] ->
                if showMatch then
                    sprintf "<c:cyan>[%s]</c> <c:white>%s</c> <c:gray>-> [</c><c:green>%s</c><c:gray>]</c>" title original texts
                else ""

            | Regex "^\[skip(.*?)\]\s(.*?)\s*$" [ title; text ] ->
                if showMatch then
                    sprintf "<c:cyan>[skip%s]</c> <c:pink>%s</c>" title text
                else ""

            | Regex "^\[(.*?)\]\s(.*?)\s*$" [ title; original ] ->
                if showIgnored then
                    sprintf "<c:cyan>[%s]</c> <c:gray>%s</c>" title original
                else ""

            | message -> message
        )
        |> Seq.filter (String.isNullOrEmpty >> not)
        |> Seq.rev
        |> Seq.toList
