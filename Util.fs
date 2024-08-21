module rec miniRSA.Util

open System.Numerics

let primeGenerator lowerLimit size =
    let rec findPrime (acc: int list) (nums: bool array) i =
        if i >= nums.Length then acc else

        if nums[i] 
        then
            let newNums = nums |> Array.mapi (fun j n -> 
                if j % i = 0 then false else n
            )
            let newAcc = if i > lowerLimit then i::acc else acc

            findPrime newAcc newNums (i + 1)
        else findPrime acc nums (i + 1)
    
    let nums = Array.create (lowerLimit + size) true

    let primes = 
        findPrime [] nums 2
        
    if primes.Length = 0 then failwith "No prime number found"
    
    let r = System.Random()

    primes 
    |> List.sortBy (fun _ -> r.Next())
    |> List.head
    |> fun n -> BigInteger n

let coprimeNumFinder n =
    let rec findCoprime m =
        // m should be as large as possible, so we start from the end
        if gcd m n = BigInteger.One then m else findCoprime (m - BigInteger.One)

    findCoprime ((n - BigInteger.One) / BigInteger 2)

let moduleInverseFinderSlow M a =
    if M < a then failwith "First argument should be greater than the second one"

    let remainder = gcd M a

    if remainder <> BigInteger.One then failwith "The two numbers are not coprime"

    let rec findModuleInverse classMul aMul =
        match classMul * M + aMul * a with
        | n when n = BigInteger.One -> aMul
        | n when n > BigInteger.One -> findModuleInverse classMul (aMul - BigInteger.One)
        | n when n < BigInteger.One -> findModuleInverse (classMul + BigInteger.One) aMul
        | _ -> failwith "Unexpected pattern match"

    let inv = findModuleInverse BigInteger.One BigInteger.Zero

    if inv < BigInteger.Zero then inv + M else inv

let rec gcd a b =
    if b = BigInteger.Zero then a else gcd b (a % b)

type gdcAccItem = BigInteger * BigInteger * BigInteger * BigInteger
type gdcAcc = gdcAccItem list

let rec gcdWithAcc a b acc : gdcAcc * BigInteger =
    if b = BigInteger.Zero then (acc, a) else 
    
    let rem = a % b
    let accItem = (a, a / b, b, rem)
    gcdWithAcc b rem (accItem::acc)

// alternative

// Function to find the modular inverse
let moduleInverseFinder (M: BigInteger) (A: BigInteger) : BigInteger =
    let (g, x, _) = gcdExtended A M
    if g <> BigInteger.One then
        let errMsg = sprintf "Inverse of %s in %s doesn't exist" (A.ToString()) (M.ToString())
        failwith errMsg
    else
        (x % M + M) % M

// Extended Euclidean Algorithm to find gcd and coefficients
let rec gcdExtended (a: BigInteger) (b: BigInteger) : (BigInteger * BigInteger * BigInteger) =
    if a = BigInteger.Zero then
        (b, BigInteger.Zero, BigInteger.One)
    else
        let (gcd, x1, y1) = gcdExtended (b % a) a
        let x = y1 - (b / a) * x1
        let y = x1
        (gcd, x, y)

let measureTime f a =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let res = f a
    sw.Stop()
    sw.Elapsed.TotalMilliseconds
    