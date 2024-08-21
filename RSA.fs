module rec miniRSA.RSA

open System.Numerics
open System.Security.Cryptography

type Coordinator (receiver: Receiver, sender: Sender) =
    member this.send<'a> (messageFn: messageFn<'a>) (messageReverseFn: messageReverseFn<'a>) (message: 'a) =
        let mToString m = m.ToString()

        let ms = messageFn message
        printfn "Original message: %A" (ms |> Array.map mToString)

        let encryptedMessage = sender.generateSenderFn ms
        let decryptedMessage = receiver.decrypt encryptedMessage
        printfn "Decrypted message: %A" (decryptedMessage |> Array.map mToString)

        messageReverseFn decryptedMessage

type Receiver(primeGenerator: primeGenerator, coprimeNumFinder: coprimeNumFinder, moduleInverseFinder: moduleInverseFinder) =
    member this.decrypt (senderFn: senderFn) =
        let keyPair = this.generateKeys ()
        let eMs = senderFn keyPair.publicKey
        let decryptOne eM = BigInteger.ModPow(eM, keyPair.privateKey.d, keyPair.privateKey.n)

        eMs |> Array.map decryptOne

    member private this.generateKeys unit : keyPair =
        let randomLowerLimit = fun () -> System.Random().Next(1, 500)
        let size = 200
        let p,q = primeGenerator (randomLowerLimit()) size, primeGenerator (randomLowerLimit()) size
        let n = p * q
        let eulerN = n - p - q + BigInteger.One
        let e = coprimeNumFinder eulerN
        if BigInteger.One > e || e > eulerN then failwith "Computed coprime number e is not valid (1 < e < eulerN)" 
        
        let d = moduleInverseFinder eulerN e 
        
        printfn "d=%s e=%s n=%s" (d.ToString()) (e.ToString()) (n.ToString())
        printfn "5=%s" (BigInteger.ModPow(BigInteger.Pow(5, int e), d, n).ToString())

        {
            publicKey = {e = e; n = n};
            privateKey = {d = d; n = n}
        }

type Sender() =
    member this.generateSenderFn = fun (ms: BigInteger array) -> this.encrypt ms

    member this.encrypt (ms: BigInteger array) (publicKey: publicKey) =
        let largeEqualN m = m >= publicKey.n

        let formatErrorMsg n = sprintf "Message is too long for this module N (%s)" (n.ToString())
        if ms |> Array.exists largeEqualN then failwith (formatErrorMsg publicKey.n)

        let encryptOne m = BigInteger.ModPow(m, publicKey.e, publicKey.n)

        ms |> Array.map encryptOne

type messageFn<'a> = 'a -> BigInteger array
type messageReverseFn<'a> = BigInteger array -> 'a

type senderFn = publicKey -> BigInteger array

type keyPair = {publicKey: publicKey; privateKey: privateKey}
type publicKey = {e: BigInteger; n: BigInteger}
type privateKey = {d: BigInteger; n: BigInteger}

type primeGenerator = int -> int -> BigInteger
type coprimeNumFinder = BigInteger -> BigInteger
type moduleInverseFinder = BigInteger -> BigInteger -> BigInteger
