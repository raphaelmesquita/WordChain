open System.Collections.Generic
open FSharp.Reactive
open FSharp.Reactive.Observable

let memoize f = 
    let cache = Dictionary<_, _>()
    fun x -> 
        let ok, res = cache.TryGetValue(x)
        if ok then res
        else 
            let res = f x
            cache.[x] <- res
            res

let getWordsOfLength words length = words |> List.filter (fun x -> String.length x = length)

let getSameLengthWords getWordsOfLength word = 
    let length = String.length word
    getWordsOfLength length

let checkAdjacency word1 word2 = 
    List.zip <| List.ofSeq word1 <| List.ofSeq word2
    |> List.filter (fun (x, y) -> x <> y)
    |> List.length
    |> (=) 1

let getAdjacents checkAdjacency getSameLengthWords word = getSameLengthWords word |> List.filter (checkAdjacency word)

let getDerivedWordChains getAdjacents wordChain = 
    seq { 
        yield! wordChain
               |> List.last
               |> getAdjacents
               |> List.except wordChain
               |> List.map (fun adj -> wordChain @ [ adj ])
    }

let createWordChains getDerivedWordChains startWord endWord = 
    let rec yieldWordChains chains = 
        seq { 
            yield! chains
            yield! chains
                   |> Seq.map getDerivedWordChains
                   |> Seq.concat
                   |> yieldWordChains
        }
    yieldWordChains [ [ startWord ] ]

let checkForMatch startWord endWord mapAndResult wordChain = 
    let mapStart, mapEnd, _ = mapAndResult
    match wordChain with
    | [] -> (mapStart, mapEnd, None)
    | chain -> 
        let assembleMatchAndReturnResult mapStart mapEnd startList endList = 
            let result = 
                endList
                |> List.rev
                |> List.tail
                |> List.append startList
            (mapStart, mapEnd, Some result)
        
        let lastWord = List.last chain
        match List.head chain with
        | w when w = startWord -> 
            let mapStart = Map.add lastWord chain mapStart
            match Map.tryFind lastWord mapEnd with
            | None -> (mapStart, mapEnd, None)
            | Some cachedChain -> assembleMatchAndReturnResult mapStart mapEnd chain cachedChain
        | w when w = endWord -> 
            let mapEnd = Map.add lastWord chain mapEnd
            match Map.tryFind lastWord mapStart with
            | None -> (mapStart, mapEnd, None)
            | Some cachedChain -> assembleMatchAndReturnResult mapStart mapEnd cachedChain chain
        | invalidWord -> failwith <| sprintf "Invalid list head: %s" invalidWord

let stopwatch = System.Diagnostics.Stopwatch()

let createParallelWordChains createWordChains checkForMatch onNext onError onComplete startWord endWord count = 
    stopwatch.Start()
    let startToEnd = createWordChains startWord endWord |> Observable.toObservable
    let endToStart = createWordChains endWord startWord |> Observable.toObservable
    
    Observable.merge startToEnd endToStart
    |> Observable.scan (checkForMatch startWord endWord) (Map.empty, Map.empty, None)
    |> Observable.choose (fun (_, _, result) -> result)
    |> Observable.take count
    |> Observable.subscribe onNext onError onComplete

let onNext wordChain = printfn "%A" wordChain
let onComplete() = printfn "Completed in %d ms" stopwatch.ElapsedMilliseconds
let onError = raise

// Composicao
let words = 
    System.IO.File.ReadLines "wordlist.txt"
    |> List.ofSeq
    |> List.map (fun x -> x.ToLowerInvariant())
    |> List.distinct

let getWordsOfLength' = getWordsOfLength words |> memoize
let getSameLengthWords' = getSameLengthWords getWordsOfLength'
let getAdjacents' = getAdjacents checkAdjacency getSameLengthWords' |> memoize
let getDerivedWordChains' = getDerivedWordChains getAdjacents'
let createWordChains' = createWordChains getDerivedWordChains'
let createParallelWordChains' = createParallelWordChains createWordChains' checkForMatch onNext onError onComplete

[<EntryPoint>]
let main argv = 
    let obv = createParallelWordChains' argv.[0] argv.[1] 1 //
    obv.Dispose()
    0
