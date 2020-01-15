namespace examples
open System.Security.Cryptography
open System.Text

module Say =
    let hello (name : string)=
        let md5 = MD5.Create()
        let md5provider = new MD5CryptoServiceProvider()
        let sha1 = SHA1.Create()
        let sha1provider = new SHA1CryptoServiceProvider()
        let bytes = md5provider.ComputeHash(UTF8Encoding().GetBytes(name))
        let bytes2 = sha1provider.ComputeHash(UTF8Encoding().GetBytes(name))
        bytes |> ignore
        bytes2 |> ignore
        ()
