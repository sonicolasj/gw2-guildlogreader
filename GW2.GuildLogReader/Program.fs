open System
open Domain
open FSharp.Collections
open System.Text.Json
open System.IO
open FSharp.Data
open Utils

let promptForLang () =
    let allowedLanguages = ["en"; "fr"; "de"; "es"]
    printfn "Available languages: %s" (allowedLanguages |> String.concat "/")

    let mutable lang = None
    let mutable isFirstTime = true

    while Option.isNone lang = true do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Invalid lang"

        printf "Choose a language for data: "
        let input = Console.ReadLine()

        lang <-
            if String.IsNullOrWhiteSpace input = true then
                Some "en"
            else if allowedLanguages |> List.contains input then
                Some input
            else    
                None

    lang.Value

let promptForApiKey () =
    let mutable apiKey = String.Empty
    let mutable isKeyValid = false
    let mutable isFirstTime = true

    while isKeyValid = false do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Malformed key"

        printf "Account key: "
        apiKey <- Console.ReadLine()

        isKeyValid <- (String.IsNullOrWhiteSpace apiKey) = false

    apiKey

let promptForGuildSelection (guilds: Guild list) =
    let guilds = guilds |> List.mapi (fun i guild -> (i+1, guild))

    printfn "Choose a guild"
    for key, guild in guilds do
        printfn "%i - %s [%s]" key guild.Name guild.Tag


    let guildsMap = guilds |> Map.ofList
    
    let mutable selectedGuild = None
    let mutable isFirstTime = true

    while selectedGuild |> Option.isNone do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Incorrect choice"

        printf "Your choice: "

        selectedGuild <-
            Console.ReadLine()
            |> tryParseInt
            |> Option.bind (fun key -> Map.tryFind key guildsMap)

    selectedGuild.Value

type Settings = {
    Version: string
    Language: string
    ApiKey: string
    GuildId: string
}

let saveSettings location settings =
    let options = new JsonSerializerOptions()
    options.WriteIndented <- true

    let serializedSettings = JsonSerializer.Serialize(settings, options)
    File.WriteAllText(location, serializedSettings)
    ()

let tryLoadSettings location =
    let tryGetString propertyName (json: JsonValue) = json.TryGetProperty propertyName |> Option.map (fun prop -> prop.AsString())
    let tryGetList propertyName (json: JsonValue) = json.TryGetProperty propertyName |> Option.map (fun prop -> [ for value in prop do yield value ])

    if File.Exists location then
        let json = File.ReadAllText location |> JsonValue.Parse

        maybe {
            let! version = json |> tryGetString "Version"
            let! language = json |> tryGetString "Language"
            let! apiKey = json |> tryGetString "ApiKey"
            let! guildId = json |> tryGetString "GuildId"

            return {
                Version = version
                Language = language
                ApiKey = apiKey
                GuildId = guildId
            }
        }
    else
        None

let logEntryToString log =
    let message =
        match log.Details with
        | Joined -> sprintf ":%s joined the guild" log.User.Value
        | Invited by -> sprintf ":%s has been invited by :%s" log.User.Value by
        | Kicked by ->
            if by = log.User.Value then
                sprintf ":%s has left the guild" by
            else
                sprintf ":%s has been kicked by :%s" log.User.Value by
        | RankChanged (by, oldRank, newRank) -> sprintf ":%s's rank has been changed from %s to %s by :%s" log.User.Value oldRank newRank by
        | AddedToTreasury (item, count) -> sprintf ":%s has added %i × %s to the treasury" log.User.Value count item.Name
        | UsedBank (operation, target) ->
            let operationMessage =
                match operation with
                | Add -> "added"
                | Remove -> "removed"
                | Move -> "moved"

            let targetMessage =
                match target with
                | Item (item, count) -> sprintf "%i × %s" count item.Name
                | Money amount ->
                    let copper = amount % 100
                    let copperText = if copper = 0 then "" else sprintf "%i copper" copper

                    let silver = ((amount % 10000) - copper) / 100
                    let silverText = if silver = 0 then "" else sprintf "%i silver" silver

                    let gold = amount / 10000
                    let goldText = if gold = 0 then "" else sprintf "%i gold" gold
                    
                    sprintf "%s %s %s" goldText silverText copperText 
                    |> (fun str -> str.Replace("  ", " ").Trim())

            sprintf ":%s has %s %s to the guild bank" log.User.Value operationMessage targetMessage
                
        | ChangedMotd message -> sprintf """:%s changed the message of the day to "%s" """ log.User.Value message

        | ChangedUpgrade (operation, upgrade, recipe) ->
            let operationMessage, countMessage =
                match operation with
                | Queue -> "queued", ""
                | Cancel -> "cancelled", ""
                | SpeedUp -> "sped up", ""
                | Complete (Some count) -> "completed", (sprintf "%i × " count)
                | Complete None -> "completed", ""

            let userMessage = log.User |> Option.defaultValue "The guild"

            sprintf "%s has %s the upgrade %s%s" userMessage operationMessage countMessage upgrade.Name
    
    sprintf "[%O] %s" log.Time message

[<EntryPoint; STAThread>]
let main argv =
    let saveLocation = "./settings.json"
    let settings = tryLoadSettings saveLocation

    let lang = settings |> Option.map (fun s -> s.Language) |> Option.defaultWith promptForLang
    let apiKey = settings |> Option.map (fun s -> s.ApiKey) |> Option.defaultWith promptForApiKey

    let guilds = Api.getAccountGuilds apiKey lang |> Async.RunSynchronously 

    let guild =
        settings |> Option.bind (fun s -> guilds |> List.tryFind (fun guild -> guild.Id = s.GuildId))
        |> Option.defaultWith (fun () -> promptForGuildSelection guilds)

    let logs = Api.getGuildLog apiKey guild.Id lang |> Async.RunSynchronously

    for log in logs do
        log |> logEntryToString |> Console.WriteLine

    saveSettings saveLocation {
        Version = "0.1.0"
        Language = lang
        ApiKey = apiKey
        GuildId = guild.Id
    }

    Console.ReadLine() |> ignore

    0
