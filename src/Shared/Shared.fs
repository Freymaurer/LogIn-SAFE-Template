namespace Shared

type Counter = { Value : int }

type User = {
    Username : string
    Password : string
}

type TokenResult = {
   Token : string
}

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi = {
    initialCounter : unit -> Async<Counter>
    getToken : User -> Async<TokenResult>
    getTest : string -> Async<string>
    }

type ISecuredApi =
    { securedCounter : unit -> Async<Counter> }
