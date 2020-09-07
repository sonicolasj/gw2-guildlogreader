module Domain

open System

type ApiKey = string

type Language = string

module Language =
    let allowedLanguages: Language list = ["en"; "fr"; "de"; "es"]
    let defaultLanguage = allowedLanguages.Head
    let isLanguageAllowed lang = allowedLanguages |> List.contains lang

type GuildId = string
type Guild = {
    Id: GuildId
    Name: string
    Tag: string
}

type ItemId = int
type Item = {
    Id: ItemId
    Name: string
}

type UpgradeId = int
type Upgrade = {
    Id: UpgradeId
    Name: string
}

type User = string

type BankOperation = Add | Remove | Move
type BankOperationTarget = Item of item: Item * count: int | Money of int
type UpgradeOperation = Queue | Cancel | Complete of count: int option | SpeedUp

type EventDetails =
    | Joined
    | Invited of by: User
    | Kicked of by: User
    | RankChanged of by: User * oldRank: string * newRank: string
    | AddedToTreasury of item: Item * count: int
    | UsedBank of operation: BankOperation * target: BankOperationTarget
    | ChangedMotd of string
    | ChangedUpgrade of operation: UpgradeOperation * upgrade: Upgrade * recipe: ItemId option

type EventId = int

type Event = {
    Id: EventId
    Time: DateTime
    User: User option
    Details: EventDetails
}
