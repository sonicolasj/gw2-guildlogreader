module Utils

open System

// Source: https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) =
        Some x
   
let maybe = new MaybeBuilder()

let tryParseInt (str: string) =
    match str |> Int32.TryParse with
    | (false, _) -> None
    | (true, value) -> Some value