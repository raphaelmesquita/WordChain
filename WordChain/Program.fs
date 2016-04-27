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
    yieldWordChains [ [ startWord ] ] |> Seq.filter (fun x -> List.last x = endWord)

//let checkForMatch startWord endWord mapAndResult wordChain = 
//    let mapStart, mapEnd, _ = mapAndResult
//    match wordChain with
//    | [] -> (mapStart, mapEnd, None)
//    | list -> 
//        let firstWord = List.head list
//        let lastWord = List.last list
//        if firstWord = startWord then 
//            let mapStart = Map.add lastWord list mapStart
//            match Map.tryFind lastWord mapEnd with
//            | None -> (mapStart, mapEnd, None)
//            | Some cachedList -> 
//                let result = 
//                    cachedList
//                    |> List.rev
//                    |> List.tail
//                    |> List.append list
//                (mapStart, mapEnd, Some result)
//        else 
//            let mapEnd = Map.add lastWord list mapEnd
//            match Map.tryFind lastWord mapStart with
//            | None -> (mapStart, mapEnd, None)
//            | Some cachedList -> 
//                let result = 
//                    list
//                    |> List.rev
//                    |> List.skip 1
//                    |> List.append cachedList
//                (mapStart, mapEnd, Some result)
//
//let createParallelWordChains createWordChains checkForMatch onNext onError onComplete startWord endWord count = 
//    let startToEnd = createWordChains startWord endWord |> Observable.toObservable
//    let endToStart = createWordChains endWord startWord |> Observable.toObservable
//    Observable.merge startToEnd endToStart
//    |> Observable.scan (checkForMatch startWord endWord) (Map.empty, Map.empty, None)
//    |> Observable.choose (fun (_, _, result) -> result)
//    |> Observable.take count
//    |> Observable.subscribe onNext onError onComplete

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


[<EntryPoint>]
let main argv = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    createWordChains' "cat" "dog" //argv.[0] argv.[1]
    |> Seq.head
    |> printfn "%A"
    stopwatch.Stop()
    printfn "Completed in %d ms" stopwatch.ElapsedMilliseconds
    System.Console.ReadLine() |> ignore
    0
