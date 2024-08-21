
open System.Numerics
open miniRSA.RSA
open miniRSA.Util


let testWithInt message =
    let coordinator = Coordinator(
        Receiver(primeGenerator, coprimeNumFinder, moduleInverseFinder), 
        Sender()
    )

    let intToBigIntArray (message: int) = [| BigInteger(int message) |]
    let bigIntArrayToInt (message: BigInteger array) = int(message.[0])

    coordinator.send intToBigIntArray bigIntArrayToInt message |> ignore

let testWithString (message: string) =
    let message' = message.ToCharArray()

    let coordinator = Coordinator(
        Receiver(primeGenerator, coprimeNumFinder, moduleInverseFinder), 
        Sender()
    )

    let stringToBigIntArray (message: string) = message.ToCharArray() |> Array.map (fun c -> BigInteger(int c))
    let bigIntArrayToString (message: BigInteger array) = message |> Array.map (fun m -> char(int m)) |> Array.ofSeq |> System.String
    
    coordinator.send stringToBigIntArray bigIntArrayToString message |> ignore

//testWithInt 7
//testWithString "Hello, world!"

let measureTimeOfModuleInverseFinder = 
    let eulerNAndEPairs = List.init 10 (fun _ -> 
        let randomLowerLimit = fun () -> System.Random().Next(10, 1_000)
        let size = 2_000
        let p,q = primeGenerator (randomLowerLimit()) size, primeGenerator (randomLowerLimit()) size
        let n = p * q
        let eulerN = n - p - q + BigInteger.One
        let e = coprimeNumFinder eulerN
        if BigInteger.One > e || e > eulerN then failwith "Computed coprime number e is not valid (1 < e < eulerN)" 

        (eulerN, e)
    )

    printfn "Starting to measure time of moduleInverseFinder"

    let times = eulerNAndEPairs |> List.map (measureTime (fun (eulerN, e) -> moduleInverseFinder eulerN e))
    let times2 = eulerNAndEPairs |> List.map (measureTime (fun (eulerN, e) -> moduleInverseFinderSlow eulerN e))

    let averageTime = times |> List.average
    let averageTime2 = times2 |> List.average

    printfn "Average time v1: %f" averageTime
    printfn "Average time v2: %f" averageTime2

measureTimeOfModuleInverseFinder