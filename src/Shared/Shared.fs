namespace Shared

type Counter = { Value : int }

type User = {
    Username : string
    Password : string
}

type TokenResult = {
   Token : string
}

type LogInResults =
| Success of TokenResult
| InvalidPasswordUser

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi = {
    // returns initial count of 42
    initialCounter : unit -> Async<Counter>
    //validated token and returns related user name and initial count of 42
    initialLoad : string -> Async<string option * Counter>
    logIn : User -> Async<LogInResults>
    validateToken : string -> Async<bool>
    }

type ISecuredApi = {
    securedCounter : unit -> Async<Counter>
    }