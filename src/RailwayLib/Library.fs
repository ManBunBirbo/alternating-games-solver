// Author: Markus Brammer Jensen, s183816
// Note:   The concepts are explained in depth in bachelor thesis. To make it a
//         bit more clear: up-direction = right, down-direction = left.

namespace RailwayLib

type Port =
    | L of string // Linear point. 
    | S of string // Stem of point.
    | P of string // Plus (default) branch of point.
    | M of string // Minus branch of point.

type Network =
    | N of
        Set<string> *  // Linear segments
        Set<string> *  // Points
        Map<Port, Port> *  // Connections (down to up).
        Map<Port, Port> *  // Connections (up to down).
        Set<string> *  // Set of signal placements.
        Map<Port, Port> // Trains' initial and final locations.

module AuxilaryRailwayFunctions = 
    let isLinear = 
        function 
        | L _ -> true 
        | _ -> false 

    let getPortId = 
        function 
        | L id 
        | S id 
        | P id 
        | M id -> id 

    // Not effecient but gets the job done for small sets.
    let rec getCombinationsFolder setCollector item =
        match setCollector with
        | [] -> []
        | set :: cs ->
            Set.add item set :: set :: getCombinationsFolder cs item

    let rec dfs conns port visited = 
        if Set.contains port visited then 
            visited 
        else 
            let vs' = Set.add port visited
            match Map.tryFind port conns with
            | None -> vs'
            | Some (L _ as p) -> dfs conns p vs' 
            | Some (M id) 
            | Some (P id) -> dfs conns (S id) vs'
            | Some (S id) -> 
                Set.union (dfs conns (P id) vs') (dfs conns (M id) vs')

module ParserFunctions =

    open AuxilaryRailwayFunctions

    let toPoint id = 
        function
        | "stem" -> S id
        | "plus" -> P id
        | "minus" -> M id
        | s -> failwith $"\"%s{s}\" is not a recognized port kind."

    let toNetwork conns signals trains = 

        let addPortsAux (linears, points) = 
            function 
            | L id1, L id2 -> Set.add id1 linears |> Set.add id2, points
            | L id1, p2 -> Set.add id1 linears, Set.add (getPortId p2) points
            | p1, L id2 -> Set.add id2 linears, Set.add (getPortId p1) points 
            | p1, p2 -> linears, Set.add (getPortId p1) points |> Set.add (getPortId p2) 

        let rec processConns (linears, points, connsUp, connsDown) =
            function 
            | [] -> (linears, points, connsUp, connsDown) 
            | (p1, p2) :: csrest -> 
                let connsUp' = Map.add p1 p2 connsUp
                let connsDown' = Map.add p2 p1 connsDown
                let linears', points' = addPortsAux (linears, points) (p1, p2) 

                processConns (linears', points', connsUp', connsDown') csrest

        let linears, points, connsUp, connsDown = 
            processConns (Set.empty, Set.empty, Map.empty, Map.empty) conns 
        
        let signals = Set.ofList signals 

        let trainsPoss = List.map (fun (id1, id2) -> L id1, L id2) trains |> Map.ofList

        N (linears, points, connsUp, connsDown, signals, trainsPoss)

// /// Library of functions checking if a network is well-formed.
// module NetworkFunctions =

//     let isLinear = function
//         | L _ -> true
//         | _ -> false

//     let connectionInvariant port1 port2 =
//         match port1, port2 with
//         | p1, p2 when isLinear p1 || isLinear p2 -> true
//         | _ -> false

//     let pointInvariant fromPorts toPorts id =
//         match List.tryFind (fun port -> getPortId port = id) fromPorts with
//         | Some (S id') ->
//             List.contains (P id') toPorts
//             && List.contains (M id') toPorts
//             && not (List.contains (S id') toPorts)
//         | Some (M id') ->
//             List.contains (P id') fromPorts
//             && List.contains (S id') toPorts
//             && not (List.contains (M id') toPorts)
//         | Some (P id') ->
//             List.contains (M id') fromPorts
//             && List.contains (S id') toPorts
//             && not (List.contains (P id') toPorts)
//         | _ -> false

//     let isWellFormed (N (linears, points, connsUp, connsDown, ss, ts)) =


//         let fromPorts, toPorts =
//             Map.keys connsUp |> Seq.toList, Map.values connsUp |> Seq.toList

//         let trainFinalPos = Map.values ts |> Seq.toList

//         // A port can at most appear once as an origin and once as a destination.
//         fromPorts = List.distinct fromPorts
//         && toPorts = List.distinct toPorts

//         // Point invariant.
//         && Set.forall (pointInvariant fromPorts toPorts) points

//         // Only connect a linear segment and a port.
//         && Map.forall connectionInvariant connsUp

//         // No linear segment and point have common ID.
//         && Set.forall (fun id -> Set.contains id points |> not) linears

//         // Signal placed on an existing linear segment.
//         && Map.forall (fun id _ -> Set.exists ((=) id) linears) ss

//         // Trains' initial and final location placed on an existing linear segment.
//         && Map.forall
//             (fun init final ->
//                 Set.contains init linears
//                 && Set.contains final linears)
//             ts

//         // Trains cannot have the same final position.
//         && trainFinalPos = List.distinct trainFinalPos

//         // No cycles.
//         // TODO Implement.
//         && true

module GameFunctions =

    open OnTheFlySolver
    open AuxilaryRailwayFunctions

    type TrainGameState =
        | TGS of
            (Port option) array *  // Positions of trains travelling UP
            (Port option) array *  // Positions of trains tavelling DOWN
            Set<string> *  // Positions of signals that ALLOW passage.
            Set<string> // Points in MINUS.

    

    let toSolver (N (linears, points, connsUp, connsDown, signals, trains)) =

        let simRel = (=)

        let edgesOne (TGS (trainsUp, trainsDown, _, _)) =
            if Array.contains None trainsUp || Array.contains None trainsDown then 
                Set.empty 
            else 
                let greenSignalsList = Set.fold getCombinationsFolder [ Set.empty ] signals

                let minusPointsList = Set.fold getCombinationsFolder [ Set.empty ] points

                let mutable set = Set.empty

                for ss in greenSignalsList do
                    for ps in minusPointsList do
                        set <- Set.add (TGS(trainsUp, trainsDown, ss, ps)) set

                set

        let edgesTwo (TGS (trainsUp, trainsDown, redSignals, minusPoints) as state) =

            let rec move currentPos conns =
                match currentPos with
                | None -> None
                | Some (L id) as pOption when Set.contains id redSignals -> pOption
                | Some p ->
                    // Can only end up at a linear section or crash.
                    match Map.tryFind p conns with
                    | Some (L _) as pOption -> pOption
                    | Some (P id) when not (Set.contains id minusPoints) -> move (Some (S id)) conns
                    | Some (M id) when Set.contains id minusPoints -> move (Some(S id)) conns
                    | Some (S id) when Set.contains id minusPoints -> move (Some(M id)) conns
                    | Some (S id) -> move (Some (P id)) conns
                    | _ -> None

            let copyArrayReplaceValue array index value =
                let arrayCopy = Array.copy array
                arrayCopy.[index] <- value
                arrayCopy

            let mutable set = Set.empty

            if Array.contains None trainsUp || Array.contains None trainsDown then 
                Set.empty 
            else 
                for i in [ 0 .. Array.length trainsUp - 1 ] do
                    set <-
                        Set.add
                            (TGS(
                                move trainsUp.[i] connsUp
                                |> copyArrayReplaceValue trainsUp i,
                                trainsDown,
                                redSignals,
                                minusPoints
                            ))
                            set

                for j in [ 0 .. Array.length trainsDown - 1 ] do
                    set <-
                        Set.add
                            (TGS(
                                trainsUp,
                                move trainsDown.[j] connsDown
                                |> copyArrayReplaceValue trainsDown j,
                                redSignals,
                                minusPoints
                            ))
                            set

                Set.filter (fun s -> s <> state) set
                

        let isTravellingUp start dest = 
            dfs connsUp start Set.empty |> Set.contains dest

        let isTravellingDown start dest = not (isTravellingUp start dest)

        let trainsUp, trainsDown = 
            Map.fold
                (fun (ups, downs) start final -> 
                    // TODO Fix: What if the final location cannot be reached at all?
                    if dfs connsUp start Set.empty |> Set.contains final then 
                        (Some start, Some final) :: ups, downs 
                    else 
                        ups, (Some start, Some final) :: downs)
                ([], [])
                trains 
        
        let upStart, upDest = List.unzip trainsUp 
        let downStart, downDest = List.unzip trainsDown

        let isGoalState (TGS (trainsUp, trainsDown, _, _)) =
            trainsUp = (Array.ofList upDest) && trainsDown = (Array.ofList downDest)

        let initState = 
            TGS (Array.ofList upStart, Array.ofList downStart, Set.empty, Set.empty)

        let initConfig = initState, One 
        new OnTheFlySolver<TrainGameState>
            (
                (edgesOne, edgesTwo, isGoalState), 
                simRel, 
                initConfig
            )



