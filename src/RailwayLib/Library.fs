// Author: Markus Brammer Jensen, s183816
// Note:   The concepts are explained in depth in bachelor thesis. To make it a
//         bit more clear: up-direction = right, down-direction = left.

namespace RailwayLib

type Port =
    | L of string // Linear point. 
    | S of string // Stem of point.
    | P of string // Plus (default) branch of point.
    | M of string // Minus branch of point.

/// A railway network is a tuple of linear segements IDs, points IDs, connections in up and down directions, signal 
/// placement IDs, and trains' start/destination position. 
type Network =
    | N of
        Set<string> *       // Linear segments
        Set<string> *       // Points
        Map<Port, Port> *   // Connections (down to up).
        Map<Port, Port> *   // Connections (up to down).
        Set<string> *       // Set of signal placements.
        Map<string, string> // Trains' initial and final locations.

module AuxilaryRailwayFunctions = 

    /// Determine if a port is linear. 
    let isLinear = 
        function 
        | L _ -> true 
        | _ -> false 

    /// Get the id of a port. 
    let getPortId = 
        function 
        | L id 
        | S id 
        | P id 
        | M id -> id 
    
    let portWithId id port = getPortId port = id

    let getCombinations set = 

        /// Use as folder with Set.fold: Get all possible combinations of elements in a set. 
        let rec getCombinationsFolder setCollector elem =
            match setCollector with
            | [] -> []
            | set :: cs ->
                // The element can either be included in each set or not. 
                set :: (Set.add elem set) :: (getCombinationsFolder cs elem)
        
        Set.fold getCombinationsFolder [ Set.empty ] set 

    /// Depth-first-search through a network from a port in the direction determined by conn(ection)s map. 
    let rec dfs conns port visited = 
        if Set.contains port visited then 
            visited 
        else 
            getNeighbours port conns 
            |> recNeighbors conns (Set.add port visited)     
    and getNeighbours port conns = 
        match Map.tryFind port conns with
        | None -> [ ]
        | Some (L _ as p) -> [ p ]
        | Some (M id) 
        | Some (P id) -> [ (S id) ]
        | Some (S id) -> [ (P id); (M id) ]
    and recNeighbors conns visited = 
        function  
        | [] -> visited
        | p :: ns -> recNeighbors conns (dfs conns p visited) ns
    

    let copyArrayReplaceValue array index value =
        let arrayCopy = Array.copy array
        arrayCopy.[index] <- value
        arrayCopy

module ParserFunctions =

    open AuxilaryRailwayFunctions

    /// Extract and apply port type to a point port. 
    let toPoint id = 
        function
        | "stem" -> S id
        | "plus" -> P id
        | "minus" -> M id
        | s -> failwith $"\"%s{s}\" is not a recognized port kind."

    /// Convert lists of conn(ection)s, signals, and trains to a railway network. 
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

        let trainsPoss = Map.ofList trains

        N (linears, points, connsUp, connsDown, signals, trainsPoss)

/// Library of functions checking if a network is well-formed.
module NetworkFunctions =

    open AuxilaryRailwayFunctions

    let connectionInvariant port1 port2 =
        // Point ports must be connected to linear segments. Linear segments 
        // can be connected to anything. 
        match port1, port2 with
        | p1, p2 when isLinear p1 || isLinear p2 -> true
        | _ -> false    

    let pointInvariant fromPorts toPorts id =
        match List.filter (portWithId id) fromPorts with
        | [ S _ ] ->
            match List.filter (portWithId id) toPorts with 
            | [ P _; M _ ] 
            | [ M _; P _ ] -> true 
            | _ -> false  
        | [ M _; P _ ] ->
            match List.filter (portWithId id) toPorts with 
            | [ S _ ] -> true 
            | _ -> false 
        | _ -> false
    
    let linearInvariant fromPorts toPorts id = 
        match List.filter (portWithId id) fromPorts with 
        | [ ] -> 
            match List.filter (portWithId id) toPorts with 
            | [ L _ ] -> true 
            | _ -> false 
        | [ L _ ] -> 
            match List.filter (portWithId id) toPorts with 
            | [ ]
            | [ L _ ] -> true 
            | _ -> false 
        | _ -> false 

    // Inspiration: https://www.geeksforgeeks.org/detect-cycle-in-a-graph/
    let rec dfsCycles conns port vs rs = 
        if Set.contains port vs && Set.contains port rs then 
            true 
        elif Set.contains port vs then 
            false 
        else 
            getNeighbours port conns 
            |> recNs conns (Set.add port vs) (Set.add port rs) 
    and recNs conns vs rs = 
        function 
        | [] -> false 
        | p :: ns -> dfsCycles conns p vs rs || recNs conns vs rs ns 


    let isWellFormed (N (linears, points, connsUp, connsDown, signals, trains)) =

        let connsUpKeys, connsUpVals = 
            Seq.toList (Map.keys connsUp), Seq.toList (Map.values connsUp)
        
        let trainFinalPos = Seq.toList (Map.values trains)
        
        // No linear segment and point have common ID.
        Set.intersect linears points = Set.empty

        && Map.forall connectionInvariant connsUp

        && Set.forall (pointInvariant connsUpKeys connsUpVals) points

        && Set.forall (linearInvariant connsUpKeys connsUpVals) linears

        // Signal placed on an existing linear segment.
        && Set.forall (fun s -> Set.contains s linears) signals

        // Trains' are placed (and must finish on) linear segments
        && Map.forall (fun id1 id2 -> Set.contains id1 linears && Set.contains id2 linears) trains 

        // Trains cannot have the same final position.
        && trainFinalPos = List.distinct trainFinalPos

        // Trains must be able to reach their destination 
        && Map.forall 
            (fun start dest -> 
                let pStart, pDest = L start, L dest

                dfs connsUp pStart Set.empty |> Set.contains pDest
                || dfs connsDown pStart Set.empty |> Set.contains pDest)
            trains

        // No cycles.
        // TODO Implement.
        && not (Map.exists (fun start _ -> dfsCycles connsUp (L start) Set.empty Set.empty) trains)

module GameFunctions =

    open OnTheFlySolver
    open AuxilaryRailwayFunctions

    type TrainGameState =
        | TGS of
            (Port option) array *   // Positions of trains travelling UP
            (Port option) array *   // Positions of trains tavelling DOWN
            Set<string> *           // Positions of signals NOT allowing passage.
            Set<string>             // Points in MINUS.

    let toSolver (N (linears, points, connsUp, connsDown, signals, trains)) =

        let redSignalsConfigs = getCombinations signals
        let minusPointsConfigs = getCombinations points 

        let hasCrashed tsUp tsDown = 
            Array.contains None tsUp || Array.contains None tsDown 

        let noTrainAt port tsUp tsDown = 
            not (Array.contains port tsUp || Array.contains port tsDown)

        let simRel = (=)

        let edgesOne (TGS (tsUp, tsDown, _, _)) = 
            if hasCrashed tsUp tsDown then Set.empty 
            else                   
                let mutable nextStates = Set.empty 

                for rsc in redSignalsConfigs do
                    for mpc in minusPointsConfigs do
                        nextStates <- 
                            Set.add (TGS (tsUp, tsDown, rsc, mpc)) nextStates

                nextStates

        let edgesTwo (TGS (tsUp, tsDown, rss, mps) as state) =

            let rec move currentPos conns =
                match currentPos with
                | None -> None
                | Some (L id) as pOpt when Set.contains id rss -> pOpt
                | Some p ->
                    match Map.tryFind p conns with
                    | Some (L _) as pOpt when noTrainAt pOpt tsDown tsDown -> pOpt
                    | Some (P id) when not (Set.contains id mps) -> move (Some (S id)) conns
                    | Some (M id) when Set.contains id mps -> move (Some(S id)) conns
                    | Some (S id) when Set.contains id mps -> move (Some(M id)) conns
                    | Some (S id) -> move (Some (P id)) conns
                    | _ -> None

            if hasCrashed tsUp tsDown then Set.empty 
            else 
                let mutable nextStates = Set.empty

                for i in [ 0 .. Array.length tsUp - 1 ] do
                    let tsUp' = move tsUp.[i] connsUp |> copyArrayReplaceValue tsUp i

                    if tsUp' <> tsUp then
                        nextStates <- Set.add (TGS (tsUp', tsDown, rss, mps)) nextStates

                for j in [ 0 .. Array.length tsDown - 1 ] do
                    let tsDown' = move tsDown.[j] connsDown |> copyArrayReplaceValue tsDown j

                    if tsDown' <> tsDown then 
                        nextStates <- Set.add (TGS (tsUp, tsDown', rss, mps)) nextStates

                nextStates
                

        let isTravellingUp start dest = 
            dfs connsUp start Set.empty |> Set.contains dest

        // let isTravellingDown start dest = not (isTravellingUp start dest)

        let trainsUp, trainsDown = 
            Map.fold
                (fun (ups, downs) start final -> 
                    let pStart, pFinal = L start, L final
                    
                    let tupleOpt = Some pStart, Some pFinal

                    // Assumes all trains can reach their destination. 
                    if dfs connsUp pStart Set.empty |> Set.contains pFinal then 
                        tupleOpt :: ups, downs 
                    else 
                        ups, tupleOpt :: downs)
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



