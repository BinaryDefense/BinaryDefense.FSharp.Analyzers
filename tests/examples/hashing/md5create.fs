module md5create
open System.Security.Cryptography
open System.Text

let foo () =
    use md5 = MD5.Create()
    md5.ComputeHash(UTF8Encoding().GetBytes("foo"))

