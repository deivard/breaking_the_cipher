namespace substitutionDecrypter

module substitutionDecrypter = 
    open System
    // Frequencies of character appearances
    let englishFreqMap = [(' ', 0.19180) ; ('E', 0.12702) ; ('T', 0.09056) ; ('A', 0.08167) ; 
                          ('O', 0.07507) ; ('I', 0.06966) ; ('N', 0.06749) ; ('S', 0.06327) ;
                          ('H', 0.06094) ; ('R', 0.05987) ; ('D', 0.04253) ; ('L', 0.04025) ; 
                          ('C', 0.02782) ; ('U', 0.02758) ; ('M', 0.02406) ; ('W', 0.02360) ; 
                          ('F', 0.02228) ; ('G', 0.02015) ; ('Y', 0.01974) ; ('P', 0.01929) ; 
                          ('B', 0.01492) ; ('V', 0.00978) ; ('K', 0.00772) ; ('J', 0.00153) ; 
                          ('X', 0.00150) ; ('Q', 0.00095) ; ('Z', 0.00074) ; ('(', 0.00001) ;
                          (')', 0.00001) ; ('!', 0.00001) ; ('?', 0.00001) ; ('.', 0.00001) ;
                          (',', 0.00001) ; (':', 0.00001) ; ('1', 0.00001) ; ('2', 0.00001) ;
                          ('3', 0.00001) ; ('5', 0.00001) ; (''', 0.00001) ; (';', 0.00001) ;
                          ('*', 0.00001) ; ('-', 0.00001)]

    // All the different characters that appear in the text
    let alphabet = ['A' .. 'Z'] @ [ '(' ; ')' ; '!' ; '?' ; '.' ; ',' ; ':' ; '1' ; '2' ; '3' ; '5' ; ''' ; ';' ; '*' ; '-' ; ' ']

    // let path = "../../../ciphers/TEXT_2.txt"
    // let text = System.IO.File.ReadAllText path

    // Counts how many times a char occurs in a text
    let countOccurrences c text = 
        let filteredText = text |> Seq.filter (fun x -> x = c)
        Seq.length filteredText
    
    //countOccurances 'a' "Aassddeaf gerg eapef af a ag parga aaa"

    // Creates the frequency map by looping the text 'alphabet.Length' times. 
    // (Decided that this is better than recreating a list 'alphabet.Length' times, if we want to keep the immutability)
    let createTextFreqMap alphabet text =
        // Helper function that creates an occurrencesMap of the input alphabet from the input text
        let rec alphabetOccurrenceLoop alphabetList =
            match alphabetList with
            | head::tail -> (head, (countOccurrences head text))::alphabetOccurrenceLoop tail
            | []         -> []
        // Helper function that creates a frequencyMap of the occurrencesMap based on the input textLength
        let rec alphabetFreqMap occurrenceMap textLength = 
            match occurrenceMap with
            | (c, o)::tail -> (c, double o / double textLength)::alphabetFreqMap tail textLength
            | []          -> []

        let occurrencesMap = alphabetOccurrenceLoop alphabet
        let textLength = Seq.length text 
        let charFreqMap = alphabetFreqMap occurrencesMap textLength
        printf "%A" charFreqMap
        charFreqMap


    // Creates a first guess substitution key based on simple frequency analysis
    let createFirstKey englishFreqMap textFreqMap = 
        // Gets the closest guess for a letter by finding the closest frequency match in the textFreqMap.
        // We do it this way (comparing a letter in the englishFreqMap with the each letter in the textFreqMap) 
        // and not the other way around to asure we get the best match for each letter.
        // cTuple           -> char * double        (the letter to guess)
        // textFreqMap      -> (char * double) list (the list of letter frequencies that we find matches in)
        // currentBestGuess -> char * double        (the best guess with the corresponding error margin)
        let rec getClosestGuess cTuple textFreqMap currentBestGuess =
            match textFreqMap with
            // When the new guess is better than currentBestGuess, this new guess becomes the best guess and the recursive 
            // call continues with the next letter in the englishFreqMap
            | (c, f)::tail when abs (f - snd cTuple) < snd currentBestGuess -> getClosestGuess cTuple tail (c, abs (f - snd cTuple)) 
            // This matches when the guess isn't better than the currentBestGuess, which means 
            // we should keep currentBestGuess for the next recursive call
            | (c, f)::tail -> getClosestGuess cTuple tail currentBestGuess
            // When this point is reached it means we have the best guess since we checked the entire list, so we return it
            | [] -> currentBestGuess

        // Creates the first guess keymap with the form: (LetterReplaced, LetterReplacing)
        let rec innerLoop englishFreqMap textFreqMap =
            match englishFreqMap with
            | (c1,f1)::tail -> 
                            let (bestGuessChar, _) = getClosestGuess (c1,f1) textFreqMap ('0', 9000.0)
                            // Recursive call with the filtered textFreqMap (since a letter can be the best match for several other letters)
                            (c1, bestGuessChar)::(innerLoop tail (textFreqMap |> List.filter (fun (c, _) -> c <> bestGuessChar)))
            // End of the englishFreqMap is reached
            | [] -> []
        
        // Returns the first guess substitution key
        innerLoop englishFreqMap textFreqMap

    // Decrypts the input text with the keyMap with the form: (LetterReplaced, LetterReplacing)
    let decryptWithKey text keyMap =
        // Helper function that find the replacement letter in the keyMap
        let findReplacement (c:char) keyMap = 
            match int c with
            | 13 | 10 -> c // Carriage return/newline
            | _ ->
                match keyMap |> List.tryFind (fun (_, letterReplacing) -> c = letterReplacing) with
                | Some (c, r) -> c
                | None -> printf "Couldn't find char: %d in keymap\n" (int c); '§'

        let newText = text |> Seq.map (fun c -> 
            findReplacement c keyMap)

        newText |> Seq.map string |> String.concat ""


    // Replaces a keyMapEntry by swapping the old letter with the new one, the keyMapEntry that had the new letter will now contain the oldC
    let replaceKeyMapEntry keyMap oldC newC = 
        // We need to find both involved old keyMap entries
        let oldEntryWithOldC = keyMap |> List.tryFind (fun (letterReplaced, _) -> letterReplaced = oldC)
        let oldEntryWithNewC = keyMap |> List.tryFind (fun (letterReplaced, _) -> letterReplaced = newC)

        match oldEntryWithOldC with
        | Some (oldC, rOld) -> match oldEntryWithNewC with
                            // If both tryFinds returned some tuple, we filter them out and add the new entries instead.
                            // They will be reordered by this operation but it doesn't matter
                            | Some (newC, rNew) -> (oldC, rNew)::(newC, rOld)::(keyMap |> 
                                List.filter (fun (c, rc) -> (oldC <> c && rOld <> rc) && (newC <> c && rNew <> rc)))
                            | None -> keyMap
        | None -> keyMap



    let decrypt text =
        let textFreqMap = createTextFreqMap alphabet text 
        let firstKeyMap = createFirstKey englishFreqMap textFreqMap
        printf "%A\n" firstKeyMap

        let rec manualDecryptionLoop text keyMap = 
            let decrypted = decryptWithKey text keyMap
            decrypted |> printf "\n-------------------\n%s"
            printf "\n\nReplace character: "
            let c = Console.ReadKey()
            match c.KeyChar with
            // Quit the loop
            | '§' -> printf "Final Keymap: %A\n\n" keyMap; decrypted
            | _ ->
                printf " ... with: "
                let rc = Console.ReadKey()
                manualDecryptionLoop text (replaceKeyMapEntry keyMap (System.Char.ToUpper c.KeyChar) (System.Char.ToUpper rc.KeyChar))
        manualDecryptionLoop text firstKeyMap

