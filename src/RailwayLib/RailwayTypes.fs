namespace RailwayLib 

open System 

/// A port can either be a linear segment (L) or a point. A point has three 
/// ports: Stem (S) at the one end, plus (P) and minus (M) branches at the 
/// other end with plus being the default. 
type Port =
    | L of string
    | S of string
    | P of string
    | M of string
    with override this.ToString() = 
            match this with 
            | L id -> sprintf "%s" id 
            | S id -> sprintf "%s.stem" id
            | P id -> sprintf "%s.point" id 
            | M id -> sprintf "%s.minus" id

/// A railway network is a tuple of the following entities (in order): 
/// - Set of linear segment IDs, 
/// - set of point IDs, 
/// - map of port-connections in up-direction, 
/// - map of port-connections in down-direction, 
/// - set of linear segment IDs with signals on them, and
/// - map of trains' initial position and destination. 
type Network =
    | N of
        Set<string> *
        Set<string> *
        Map<Port, Port> *
        Map<Port, Port> *
        Set<string> *
        Map<string, string>

/// A train-game-state is a tuple of four entities (in order): 
/// - Location of trains travelling up, 
/// - location of trains travelling down, 
/// - set of red signals, 
/// - set of minus-points. 
type TrainGameState =
    | TGS of
        (Port option) array * 
        (Port option) array * 
        Set<string> * 
        Set<string>

/// Exception to throw when parsing network and checking for well-formedness.
// https://stackoverflow.com/a/6370923: 
//     DO NOT USE 'exception NetworkError of string'
type NetworkError (msg:string) = inherit Exception(msg)
