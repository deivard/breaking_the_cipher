namespace caesarDecrypter

module caesarDecrypter = 
    open System

    // Shifts a single char c (if it exist in the input alphabet) by n
    let shiftLetter (alphabet:char list) c n  =  
        let alphabetLength = Seq.length alphabet
        match alphabet |> List.tryFindIndex (fun elem -> elem = c) with
        // Only shift a character if it exist in the alphabet
        | Some index ->
            let newCIndex = index + (if n > 0 then (n % alphabetLength) else -1 * (-1 * n % alphabetLength))
            match newCIndex with
            // If we went out of bounds, restart from the beginning/end
            | _ when newCIndex < 0               -> alphabet.[newCIndex + alphabetLength]
            | _ when newCIndex >= alphabetLength -> alphabet.[newCIndex - alphabetLength]
            // Else return the new character
            | _                                  -> alphabet.[newCIndex] 
        // Non existing characters are just returned
        | None -> c

    // Shifts each letter (if it exist in the input alphabet) in the text by n
    let shiftLetters alphabet n (text:string) = 
        let newText = text |> Seq.map (fun c -> shiftLetter alphabet c n ) 
        newText |> Seq.map string |> String.concat ""

    let decrypt cipher = 
        let alphabet = ['A'..'Z']
        let rec decryptLoop i cipher =
            let decryptAttempt = shiftLetters alphabet i cipher
            printf "Decryption attempt #%d:\n%s" i decryptAttempt
            printf "\nIs the above decryption correct? (y/n): "
            let c = Console.ReadKey()
            match c.KeyChar with 
            | 'y' -> printf "\nGreat! The encryption key was: %d\n" i; decryptAttempt
            | _ when i = 26 -> printf "\nSorry chief, we tried all possible keys. Perhaps this cipher 
                    isn't a Caesar Cipher or you discarded the correct decryption.\n"; "" // Return empty string
            | _ -> printf "\nTrying again . . . \n"; decryptLoop (i+1) cipher
        decryptLoop 1 cipher
