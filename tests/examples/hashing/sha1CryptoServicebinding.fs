module sha1CryptoServicebinding
open System.Security.Cryptography
open System.Text

let foo () =
    use sha1 = new SHA1CryptoServiceProvider()
    sha1.ComputeHash(UTF8Encoding().GetBytes("foo"))

