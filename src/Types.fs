namespace MF.TranslationConsole

type Extension = Extension of string

[<RequireQualifiedAccess>]
module internal Extension =
    let parse (fileName: string) =
        fileName.Split(".", 2)
        |> Seq.rev
        |> Seq.head
        |> Extension

    let equals (Extension extension) (string: string) =
        extension.TrimStart '.' = string.TrimStart '.'

type internal File = {
    Path: FilePath
    Name: string
    Extension: Extension
    Lines: Lines
}

[<RequireQualifiedAccess>]
module internal File =
    let create: string * string list -> File =
        fun (path, lines) ->
            let name = path.Split "/" |> Seq.rev |> Seq.head
            let extension = name |> Extension.parse

            {
                Path = path
                Name = name
                Extension = extension
                Lines = lines
            }

type internal Translate = Translate of original: string * replacement: string

[<RequireQualifiedAccess>]
module internal Translate =
    let create = Translate
    let value (Translate (original, replacement)) = (original, replacement)
    let original (Translate (original, _)) = original

    let contains (Translate (original, _)) (subString: string) =
        original.Contains(subString)

type internal TranslatedFile = {
    Path: string
    Name: string
    Translates: Translate list
}

type HasTexts = Extension -> string -> string list option
type TranslateText = Extension -> string -> string option
