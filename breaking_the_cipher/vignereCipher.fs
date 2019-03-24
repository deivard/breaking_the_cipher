namespace vignereCipher
module vignereCipher =
    // For shiftLetter
    open caesarDecrypter
    let alphabet = ['A'..'Z'] @ [' ']
    // Partially applied function, since all shiftLetter calls in this file use the same alphabet
    let shiftLetter = caesarDecrypter.shiftLetter alphabet

    // first to last 2, helps the other first to last function to work.
    let f2l2 (x:int list) = 
        match x with
        | x :: xs -> xs @ [x]
        | _ -> []
        
    // first to last, makes it into list, sends to other f2l function, then turn back into array
    let f2l (x:int array) = 
        let list = x|> Array.toList
        f2l2 list |> List.toArray
        
    let rec decrypt (text:char list) (key:int array) it = 
        if it = key.Length - 1
            then
                match text with
                | x :: [] -> [shiftLetter x (-1*key.[0])]
                | x :: xs -> shiftLetter x (-1*key.[0]) :: decrypt xs (f2l key) 0
                | _ -> []
                else
                    match text with
                    | x :: [] -> [shiftLetter x (-1*key.[0])]
                    | x :: xs -> shiftLetter x (-1*key.[0]) :: decrypt xs key (it+1)
                    | _ -> []

    // encryption function. it is an iterator, set to 0.
    let rec crypt (text:char list) (key:int array) it = 
        if it = key.Length - 1
            then
                // To change the offset, use first to last on key.
                match text with
                | x :: [] -> [shiftLetter x key.[0]]
                | x :: xs -> shiftLetter x key.[0] :: crypt xs (f2l key) 0
                | _ -> []
                else           
                    match text with
                    | x :: [] -> [shiftLetter x key.[0]]
                    | x :: xs -> shiftLetter x key.[0] :: crypt xs key (it+1)
                    | _ -> []

    // char to int help function
    let charToInt c = int c - int '0'

    let vignereCrypt = 
        printfn "Input your string:"
        let input = System.Console.ReadLine()
        printfn "Input your key (numbers)"
        let key = System.Console.ReadLine()
        let charlist = (Seq.toList (input.ToUpper()))
        //let intKey = key |> charToInt
        let keyList = (Seq.toList (key))
        let keyInt = List.map (fun c -> c |> charToInt) keyList
        let keyIn2 = List.toArray keyInt
        printfn "%s" (System.String.Concat(Array.ofList(crypt charlist keyIn2 0)))
        System.Console.ReadKey()

    let vignereDecrypt = 
        printfn "Input encrypted string:"
        let input = System.Console.ReadLine()
        printfn "Input your key (numbers)"
        let key = System.Console.ReadLine()
        let charlist = (Seq.toList (input.ToUpper()))
        //let intKey = key |> charToInt
        let keyList = (Seq.toList (key))
        let keyInt = List.map (fun c -> c |> charToInt) keyList
        let keyIn2 = List.toArray keyInt
        printfn "%s" (System.String.Concat(Array.ofList(decrypt charlist keyIn2 0)))
        System.Console.ReadKey()
