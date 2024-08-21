
open System.Numerics
open miniRSA.RSA
open miniRSA.Util


let message = "Hello, world!"
let message' = message.ToCharArray()

let coordinator = Coordinator(
    Receiver(primeGenerator, coprimeNumFinder, moduleInverseFinderSlow), 
    Sender()
)

let stringToBigIntArray (message: string) = message.ToCharArray() |> Array.map (fun c -> BigInteger(int c))
let bigIntArrayToString (message: BigInteger array) = message |> Array.map (fun m -> char(int m)) |> Array.ofSeq |> System.String

let newMsg = coordinator.send stringToBigIntArray bigIntArrayToString message

printfn "New message: %s" newMsg