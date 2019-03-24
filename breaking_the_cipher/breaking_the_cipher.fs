module breaking_the_cipher
open caesarDecrypter
open substitutionDecrypter
open vignereCipher

[<EntryPoint>]
let main argv =
    let path1 = "../../../ciphers/TEXT_1.txt"
    let text1 = System.IO.File.ReadAllText path1
    let path2 = "../../../ciphers/TEXT_2.txt"
    let text2 = System.IO.File.ReadAllText path2

    let rec programLoop () =
        printf "Breaking the cipher.\n
                Press 1 for part 1 (Caesar cipher)\n
                Press 2 for part 2 (Substition cipher)\n
                Press 3 for part 3 (Vignere cipher)\n
                Press any other key to quit.\n"
        let c = Console.ReadKey()
        match c.KeyInfo with
        | '1' -> caesarDecrypter.decrypt text1 |> printf "Final text: \n--------------------\n%s"; programLoop
        | '2' -> substitutionDecrypter.decrypt text2 |> printf "Final text: \n--------------------\n%s"; programLoop
        | '3' -> 
            vignereCipher.vignereCrypt |> ignore
            System.Console.ReadKey() |> ignore
            vignereCipher.vignereDecrypt |> ignore
            programLoop
        | _ -> 0

    let decryptedText = caesarDecrypter.decrypt text1
    let decryptedSubCipher = substitutionDecrypter.decrypt text2
    printf "Final text: \n--------------------\n%s" decryptedSubCipher 
    printf "Final text: \n--------------------\n%s" decryptedText 


    vignereCipher.vignereCrypt |> ignore
    System.Console.ReadKey() |> ignore
    vignereCipher.vignereDecrypt |> ignore

    0 // return an integer exit code
