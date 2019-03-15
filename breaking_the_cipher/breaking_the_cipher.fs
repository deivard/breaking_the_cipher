module breaking_the_cipher
open caesarDecrypter


[<EntryPoint>]
let main argv =
    let path = "../../../ciphers/TEXT_1.txt"
    let text = System.IO.File.ReadAllText path
    
    let decryptedText = caesarDecrypter.decrypt text
    
    0 // return an integer exit code
