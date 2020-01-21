module sha1create
open System.Security.Cryptography
open System.Text

let foo () =
    use sha1 = SHA1.Create()
    sha1.ComputeHash(UTF8Encoding().GetBytes("foo"))

