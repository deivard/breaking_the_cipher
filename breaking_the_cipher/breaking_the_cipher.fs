module breaking_the_cipher
open caesarDecrypter
open substitutionDecrypter



[<EntryPoint>]
let main argv =
    let path1 = "../../../ciphers/TEXT_1.txt"
    let text1 = System.IO.File.ReadAllText path1
    let path2 = "../../../ciphers/TEXT_2.txt"
    let text2 = System.IO.File.ReadAllText path2


    //let decryptedText = caesarDecrypter.decrypt text
    let decryptedSubCipher = substitutionDecrypter.decrypt text2
    printf "Final text: \n--------------------\n%s" decryptedSubCipher 

    0 // return an integer exit code
