namespace Shared

type Counter = { Value : int }

type User = {
    Username : string
    Password : string
}

type DotnetLogInResults =
| Success of string
| Failed of string

type DotnetLogOutResults =
| Success of string
| Failed of string

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi = {
    // returns initial count of 42
    initialCounter : unit -> Async<Counter>
    }

type IDotnetApi = {
    dotnetLogIn : User -> Async<DotnetLogInResults>
}

type IDotnetSecureApi = {
    dotnetGetUser : unit -> Async<User>
    dotnetUserLogOut : unit -> Async<DotnetLogOutResults>
    getUserCounter : unit -> Async<Counter>
}