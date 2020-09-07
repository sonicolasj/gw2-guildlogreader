module Api

open Utils
open FSharp.Data
open FSharp.Data.JsonExtensions

open Domain

let private get url query lang =
    let query = [query; ["lang", lang]] |> List.concat
    Http.AsyncRequestString(
        url,
        query=query,
        httpMethod="GET"
    )

let private getAuth apiKey url query lang =
    let query = [query; ["lang", lang]] |> List.concat
    Http.AsyncRequestString(
        url,
        query=query,
        httpMethod="GET",
        headers=[HttpRequestHeaders.Authorization (sprintf "Bearer %s" apiKey)]
    )

let private getGuildData apiKey guildId =
    async {
        let! responseText = getAuth apiKey (sprintf "https://api.guildwars2.com/v2/guild/%s" guildId) [] "en"
        let json = JsonValue.Parse responseText

        return {
            Id = guildId
            Name = (json?name).AsString()
            Tag = (json?tag).AsString()
        }
    }

let getAccountGuilds apiKey lang =
    let parseResponse json =
        [ for guildId in (json |> JsonValue.Parse)?guild_leader do yield guildId.AsString() ]

    async {
        let! responseText = getAuth apiKey "https://api.guildwars2.com/v2/account" [] "en" 
        let guildIds = responseText |> parseResponse

        let! guilds = Async.Parallel [ 
            for guildId in guildIds do
                yield getGuildData apiKey guildId
        ]

        return Array.toList guilds
    }

let getItemsInBatch itemIds lang =
    let parseResponse json: Item list =
        [
            for itemJson in (json |> JsonValue.Parse) do 
                yield {
                    Id = (itemJson?id).AsInteger()
                    Name = (itemJson?name).AsString()
                }
        ]

    async {
        let batches = itemIds |> List.chunkBySize 200

        let! results =
            Async.Parallel (batches |> List.map (fun itemIds ->
                let idsParam = itemIds |> List.map string |> String.concat ","
                get "https://api.guildwars2.com/v2/items" ["ids", idsParam] lang
            ))

        return
            results
            |> Array.map parseResponse
            |> List.ofArray
            |> List.concat
    }

let getUpgradesInBatch upgradeIds lang =
    let parseResponse json: Upgrade list =
        [
            for itemJson in (json |> JsonValue.Parse) do 
                yield {
                    Id = (itemJson?id).AsInteger()
                    Name = (itemJson?name).AsString()
                }
        ]

    async {
        let batches = upgradeIds |> List.chunkBySize 200

        let! results =
            Async.Parallel (batches |> List.map (fun upgradesIds ->
                let idsParam = upgradesIds |> List.map string |> String.concat ","
                get "https://api.guildwars2.com/v2/guild/upgrades" ["ids", idsParam] lang
            ))

        return
            results
            |> Array.map parseResponse
            |> List.ofArray
            |> List.concat
    }

let getString property (jsonValue: JsonValue) = jsonValue.GetProperty(property).AsString() 
let tryGetString property (jsonValue: JsonValue) = jsonValue.TryGetProperty(property) |> Option.map (fun prop -> prop.AsString())
let getInt property (jsonValue: JsonValue) = jsonValue.GetProperty(property).AsInteger()
let tryGetInt property (jsonValue: JsonValue) = jsonValue.TryGetProperty(property) |> Option.map (fun prop -> prop.AsInteger())
let getDatetime property (jsonValue: JsonValue) = jsonValue.GetProperty(property).AsDateTime()
let tryGetDatetime property (jsonValue: JsonValue) = jsonValue.TryGetProperty(property) |> Option.map (fun prop -> prop.AsDateTime())

let private getDistinct json key =
    let itemsIdsOptions = [ 
        for jsonValue in JsonValue.Parse json do 
            yield jsonValue |> tryGetInt key
    ]

    itemsIdsOptions
    |> List.filter Option.isSome
    |> List.map (fun i -> i.Value) 
    |> List.distinct

let private parseGuildLogResponse json itemsMap upgradesMap =
    [ 
        for jsonValue in JsonValue.Parse json do
            let id = jsonValue |> getInt "id"
            let time = jsonValue |> getDatetime "time"
            let user = jsonValue |> tryGetString "user"
            let type_ = jsonValue |> getString "type"

            let details =
                match type_ with
                | "joined" -> Joined
                | "invited" -> jsonValue |> getString "invited_by" |> Invited
                | "kick" -> jsonValue |> getString "kicked_by" |> Kicked
                
                | "rank_change" ->
                    let changedBy = jsonValue |> getString "changed_by"
                    let oldRank = jsonValue |> getString "old_rank"
                    let newRank = jsonValue |> getString "new_rank"
                    RankChanged (changedBy, oldRank, newRank)
                
                | "treasury" ->
                    let item = itemsMap |> Map.find (jsonValue |> getInt "item_id")
                    let count = jsonValue |> getInt "count"
                    AddedToTreasury (item, count)

                | "stash" ->
                    let operation = 
                        jsonValue 
                        |> getString "operation"
                        |> function
                            | "deposit" -> Add
                            | "withdraw" -> Remove
                            | "move" | _ -> Move
                    let coins = jsonValue |> getInt "coins"
                    let target = 
                        match coins with
                        | 0 -> 
                            let item = itemsMap |> Map.find (jsonValue |> getInt "item_id")
                            let count = jsonValue |> getInt "count"
                            Item (item, count)
                        | _ -> Money coins
                    
                    UsedBank (operation, target)

                | "motd" -> jsonValue |> getString "motd" |> ChangedMotd

                | "upgrade" ->
                    let action = 
                        jsonValue 
                        |> getString "action"
                        |> function
                            | "queued" -> Queue
                            | "cancelled" -> Cancel
                            | "sped_up" -> SpeedUp
                            | "completed" | _ ->
                                jsonValue |> tryGetInt "count" |> Complete
                
                    let upgrade = upgradesMap |> Map.find (jsonValue |> getInt "upgrade_id")
                    let recipeId = jsonValue |> tryGetInt "recipe_id"

                    ChangedUpgrade (action, upgrade, recipeId)
                
                | _ -> Joined

            yield {
                Id = id
                Time = time
                User = user
                Details = details
            }
    ]

let getGuildLog apiKey guildId lang =
    async {
        let! responseText = getAuth apiKey (sprintf "https://api.guildwars2.com/v2/guild/%s/log" guildId) [] "en"
        
        let itemsIds = getDistinct responseText "item_id"
        let! items = getItemsInBatch itemsIds lang 
        let itemsMap = items |> List.map (fun i -> i.Id, i) |> Map.ofList

        let upgradesIds = getDistinct responseText "upgrade_id"
        let! upgrades = getUpgradesInBatch upgradesIds lang 
        let upgradesMap = upgrades |> List.map (fun i -> i.Id, i) |> Map.ofList

        return parseGuildLogResponse responseText itemsMap upgradesMap
    }
