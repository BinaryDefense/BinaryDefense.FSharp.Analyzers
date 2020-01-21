module md5CryptoServicebinding
open System.Security.Cryptography
open System.Text

let foo () =
    use md5 = new MD5CryptoServiceProvider()
    md5.ComputeHash(UTF8Encoding().GetBytes("foo"))

