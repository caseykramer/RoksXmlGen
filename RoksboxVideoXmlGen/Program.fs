// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSharpx
open System.IO
type nfoFile = StructuredXml<"Sample.nfo">
type movieFile = StructuredXml<"SampleMovie.xml">

let inline getValue x = (^a : (member Element:System.Xml.Linq.XElement)x).Value
let inline setValue v x = (^a : (member Element:System.Xml.Linq.XElement)x).Value <- v; x

type mutableSet<'a> = System.Collections.Generic.HashSet<'a>

type state = { directory:string; output:string option; videoRoot:string option; ignoreGenres:bool }

type movie = { origtitle:string; 
               year:string option; 
               mpaa: string option; 
               description: string option; 
               path: string; 
               genre: string; 
               videocodec:string; 
               length: string option; 
               poster: string option;
               actors: string option;
               directors: string option; }

let opAsString = function
    | None -> ""
    | Some null -> ""
    | Some s -> s

let showHelp() = 
    printfn "Usage:"
    printfn "%s <directory> [-o output.xml]" <| System.Environment.GetCommandLineArgs().[0]

let getArgs argv = 
    let getOpts (op,arg) = 
        match op with
        | "-o" -> ("output",arg)
        | "-r" -> ("root",arg)
        | "-i" -> ("ignore","")
        | _ -> failwith "Unknown option: %s" op

    match argv with
    | [] ->
        showHelp() 
        Map.empty
    | [directory] -> if not <| Directory.Exists(directory) then 
                        failwith <| sprintf "Directory: %s does not exist" directory 
                     else [("directory",directory);("root","/")] |> Map.ofList
    | [directory;opt;arg] ->
        if not <| Directory.Exists(directory) then
            failwith "Directory: %s does not exist" directory
        else
            [("directory",directory)] @ ([(opt,arg)] |> List.map getOpts) |> Map.ofList
    | [directory;opt;arg;opt2;arg2] ->
        [("directory",directory)] @ ([(opt,arg);(opt2,arg2)] |> List.map getOpts) |> Map.ofList
    | _ ->
        showHelp()        
        Map.empty

let tryReduce reducer (s:'a seq) = 
    match s |> List.ofSeq with
    | [] -> Unchecked.defaultof<'a>
    | _ -> s |> Seq.reduce reducer
    
let tryHead (s:string seq) = 
    match s |> List.ofSeq with
    | [] -> ""
    | _ -> s |> Seq.head

let headOrDefault (s:'a seq) = 
    match s |> List.ofSeq with
    | [] -> Unchecked.defaultof<'a>
    | _ -> s |> Seq.head
    
let SomeString (s:string):string option = 
    match s with
    | x when x = "" -> None
    | x -> Some x


let getMovie state nfoFileName:movie =
    let videoRoot = match state.videoRoot with
                    | None -> "/"
                    | Some r -> r
    let getFullPath fileName = Path.Combine(Path.GetDirectoryName(nfoFileName),fileName).Replace(state.directory,videoRoot).Replace("\\","/")
    let nfo = (new nfoFile(filename = nfoFileName)).Root 
    let pathCategory = sprintf "[%s]" <| Path.GetDirectoryName(nfoFileName).Replace(Path.GetPathRoot(nfoFileName),"").Replace("\\","/")
    (*let genres = 
        if state.ignoreGenres then
            pathCategory
        else 
            pathCategory :: ((nfo.GetGenres() |> Seq.head).GetNames() |> Seq.map getValue |> List.ofSeq) |>  tryReduce (fun n c -> c + "," + n.ToString())  *)
    let actors = 
        match nfo.GetActors() |> List.ofSeq with
        | [] -> None
        | _ as s -> (s |> Seq.head).GetNames() |> Seq.map getValue |>  Seq.truncate 6 |> tryReduce (fun n c-> c + "," + n) |> SomeString
    let directors = if nfo.GetDirectors() |> Seq.length = 0 then None
                    else (nfo.GetDirectors() |> Seq.head).GetNames() |> Seq.map getValue |> tryReduce (fun n c-> c + "," + n) |> SomeString
    {
        origtitle = nfo.GetTitles() |> Seq.map getValue |> tryHead
        year = nfo.GetYears() |> Seq.map getValue |> tryHead |> SomeString
        mpaa = nfo.GetCertifications() |> Seq.map getValue |> tryHead|> SomeString
        description = nfo.GetPlots() |> Seq.map getValue |> tryHead |> SomeString
        path = nfo.GetFilenames() |> Seq.map getValue |> tryHead |> getFullPath
        genre = pathCategory
        videocodec = "mp4"
        length = nfo.GetRuntimes() |> Seq.map getValue |> tryHead |> SomeString
        poster = sprintf "images/%s.jpg" (nfo.GetFilenames() |> Seq.map getValue |> tryHead |> Path.GetFileNameWithoutExtension) |> SomeString
        actors = actors
        directors = directors
    }
    

let findImagesFolder dirName = 
    let rec findInDir (dir:DirectoryInfo) =
        match dir.GetDirectories() |> List.ofSeq |> List.tryFind (fun d -> d.Name.ToLowerInvariant() = "images") with
        | Some d -> d.FullName
        | None -> findInDir dir.Parent
    findInDir <| new DirectoryInfo(dirName) 

let copyThumbs dirName imageFolder = 
    let changeExtension f= Path.ChangeExtension(f,".jpg")
    let copyFile f =
        let dest = Path.Combine([|imageFolder;Path.GetFileName(f)|])
        if File.Exists(dest) then
            //printfn "File exists skipping: %s" (Path.GetFileName(f))
            ignore()
        else
            try 
                File.Copy(f,dest)
                printfn "Copied %s to image directory" (Path.GetFileName(f))
            with
            | _ as e-> 
                printfn "Skipping copying thumbnail file: %s" e.Message 
                ignore()
    let files = (Directory.GetFiles(dirName,"*.mp4") |> List.ofArray) @ (Directory.GetFiles(dirName,"*.m4v") |> List.ofArray)
    let imageFiles = files |> List.map changeExtension |> List.filter File.Exists
    imageFiles |> List.iter copyFile

let processFolder state =
    let imageFolder = findImagesFolder state.directory
    let movieFile = new movieFile()
    let vidXml = movieFile.Root    
    let viddb = vidXml.NewViddb()
    
    let rec processFiles (fileList:FileInfo list) movieList = 
        match fileList with
        | [] -> movieList
        | file::files -> processFiles files ((getMovie state file.FullName)::movieList)

    let hasVideoXml (dir:DirectoryInfo) = 
        if dir.FullName = state.directory then
            false
        else
            match dir.GetFiles("video.xml") with
            | [||] -> false
            | _ -> true

    let getMoviesFromVidXml (dir:DirectoryInfo) = 
        let file = dir.GetFiles("video.xml").[0]
        let vidFile = new movieFile(filename = file.FullName)
        (vidFile.Root.GetViddbs() |> Seq.head).GetMovies() 
            |> Seq.map (fun xml ->
                    {
                        actors = match xml.GetActors() |> headOrDefault with
                                 | null -> None
                                 | _ as x -> (x |> getValue |> SomeString)
                        genre = xml.GetGenres() |> headOrDefault |> getValue
                        origtitle = xml.GetOrigtitles() |> headOrDefault |> getValue
                        year = match xml.GetYears() |> headOrDefault with
                               | null -> None
                               | _ as x -> x|> getValue |> SomeString
                        mpaa = match xml.GetMpaas() |> headOrDefault with
                               | null -> None
                               | _ as x -> x |> getValue |> SomeString
                        description = match xml.GetDescriptions() |> headOrDefault with
                                      | null -> None
                                      | _ as x -> x |> getValue |> SomeString
                        path = xml.GetPaths() |> headOrDefault |> getValue
                        videocodec = xml.GetVideocodecs() |> headOrDefault |> getValue
                        length = match xml.GetLengths() |> headOrDefault with
                                 | null -> None
                                 | _ as x -> x |> getValue |> SomeString
                        poster = match xml.GetPosters() |> headOrDefault with
                                 | null -> None 
                                 | _ as x -> x |> getValue |> SomeString
                        directors = match xml.GetDirectors() |> headOrDefault with
                                    | null -> None
                                    | _ as x -> x |> getValue |> SomeString
                    })

            |> List.ofSeq

    let rec processDirectories dirList (processed:string mutableSet) movieList = 
        match dirList with
        | [] -> movieList
        | (dir:DirectoryInfo)::otherDirs when dir.Name.ToLowerInvariant() = "images" ->
            processDirectories otherDirs processed movieList
        | dir::otherDirs ->
            processDirectories otherDirs processed (processFilesAndFolders dir otherDirs processed movieList)
    and processFilesAndFolders (dir:System.IO.DirectoryInfo) otherDirs (processed:string mutableSet) movieList = 
        match processed.Contains(dir.FullName) with
        | true -> processDirectories (dir.GetDirectories() |> List.ofArray) processed (movieList)
        | false ->         
            match hasVideoXml dir with
            | true -> 
                let movies = getMoviesFromVidXml dir
                processed.Add(dir.FullName) |> ignore
                processDirectories otherDirs processed (movies @ movieList)
            | false ->
                printfn "Processing %s" dir.FullName
                copyThumbs (dir.FullName) imageFolder
                let nfoFiles (d:DirectoryInfo) = d.GetFiles("*.nfo") |> List.ofArray
                let movies d = processFiles (nfoFiles d) movieList
                processed.Add(dir.FullName) |> ignore
                processDirectories (dir.GetDirectories() |> List.ofArray) processed (movies dir)
    
    let movieToXml (movie:movie) = 
        let nullString s =
            match s with
            | null -> ""
            | _ -> s
        let movieXml = viddb.NewMovie()
        movieXml.NewActor() |> setValue (opAsString movie.actors) |> movieXml.AddActor
        movieXml.NewDescription() |> setValue (opAsString movie.description) |> movieXml.AddDescription
        movieXml.NewDirector() |> setValue (opAsString movie.directors) |> movieXml.AddDirector
        movieXml.NewGenre() |> setValue (nullString movie.genre) |> movieXml.AddGenre
        movieXml.NewLength() |> setValue (opAsString (movie.length |> Option.map string)) |> movieXml.AddLength
        movieXml.NewMpaa() |> setValue (opAsString movie.mpaa) |> movieXml.AddMpaa
        movieXml.NewOrigtitle() |> setValue (nullString movie.origtitle) |> movieXml.AddOrigtitle
        movieXml.NewPath() |> setValue (nullString movie.path) |> movieXml.AddPath
        movieXml.NewPoster() |> setValue (movie.poster |> opAsString) |> movieXml.AddPoster
        movieXml.NewVideocodec() |> setValue (nullString movie.videocodec) |> movieXml.AddVideocodec
        movieXml.NewYear() |> setValue (opAsString movie.year) |> movieXml.AddYear
        movieXml

    let dir = new DirectoryInfo(state.directory)
    let movies = processDirectories [dir] (new mutableSet<string>()) []
    movies |> List.map movieToXml |> List.iter viddb.AddMovie
    sprintf "<xml>%s</xml>" <| viddb.Element.ToString(System.Xml.Linq.SaveOptions.None)
    
    

let writeOutput value fileName = 
    try
        System.IO.File.WriteAllText(fileName,value)
        0
    with 
    | _ as e -> 
        printfn "Error writing output: %s" e.Message
        1

[<EntryPoint>]
let main argv = 
    let args = getArgs <| List.ofArray argv 
    match args |> Map.toList with
    | [] -> 1
    | _ -> 
        let state = 
            { directory = args.Item("directory")
              output = args.TryFind("output")
              videoRoot = args.TryFind("root")
              ignoreGenres  = true }
            
        let xml = processFolder state
        match args |> Map.tryFind "output" with
        | Some file -> writeOutput xml file
        | None -> 
            printf "%s" xml
            0
