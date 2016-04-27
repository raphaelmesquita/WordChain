open System.Collections.Generic

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
