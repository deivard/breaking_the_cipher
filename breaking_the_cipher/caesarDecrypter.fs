namespace caesarDecrypter

module caesarDecrypter = 
    open System
    let pathToCipher = "../../../ciphers/TEXT_1.txt"
    let textEncrypted = System.IO.File.ReadAllText pathToCipher

    let alphabet = [|'A' .. 'Z'|]

    // Shifts each letter (A-Z) in the text by n
    let shiftLetters n (text:string) = 
        let alphabetLength = (int 'Z' - int 'A')
        let newText = text |> Seq.map (fun c ->
            match c with 
            // Only shift a character if it is a letter
            | _ when int c >= int 'A' && int c <= int 'Z' ->  
                let newC = int c + (n % alphabetLength)
                //printf "Char: %c Old: %d New: %d \n" c (int c) newC
                match newC with
                // If we went out of bounds, restart from the beginning/end
                | _ when newC < int 'A' -> char (newC + (alphabetLength + 1))
                | _ when newC > int 'Z' -> char (newC - (alphabetLength + 1))
                | _ -> char newC
            | _ -> c
        ) 
        newText |> Seq.map string |> String.concat ""

    let decrypt cipher = 
        let rec decryptLoop i cipher =
            let decryptAttempt = shiftLetters i cipher
            printf "Decryption attempt #%d:\n%s" i decryptAttempt
            printf "\nIs the above decryption correct? (y/n): "
            let c = Console.ReadKey()
            match c.KeyChar with 
            | 'y' -> printf "\nGreat! The encryption key was: %d\n" i; decryptAttempt
            | _ when i = 26 -> printf "\nSorry chief, we tried all possible keys. Perhaps this cipher 
                    isn't a Caesar Cipher or you discared the correct decryption.\n"; "" // Return empty string
            | _ -> printf "\nTrying again . . . \n"; decryptLoop (i+1) cipher
        decryptLoop 1 cipher
