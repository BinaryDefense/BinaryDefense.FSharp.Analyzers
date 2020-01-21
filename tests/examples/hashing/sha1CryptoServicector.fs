module sha1CryptoServicector
open System.Security.Cryptography
open System.Text

let foo () =
    (new SHA1CryptoServiceProvider()).ComputeHash(UTF8Encoding().GetBytes("foo"))

