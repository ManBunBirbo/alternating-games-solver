namespace RailwayLib

module GenerateNetwork =

    open AuxiliaryRailwayFunctions

    let raiseNetworkError str = raise <| NetworkError str

    /// Extract and apply port type to a point port.
    let toPort (id, portType) =
        match portType with 
        | "linear" -> L id
        | "stem" -> S id
        | "plus" -> P id
        | "minus" -> M id
        | s -> raiseNetworkError $"\"%s{s}\" is not a recognized port kind for ID \"%s{id}\"."

    let processConnections connectionsList =
        let conditionallyAddConnection (p1, p2) cs =
            match Map.tryFind p1 cs with
            | None -> Map.add p1 p2 cs
            | Some _ -> raiseNetworkError $"Port \"%s{p1.ToString()}\" is not deterministic."
            
        let rec processConnectionsAux (ls, ps, csUp, csDown as pt) = function
            | [] -> pt
            | (s1, s2) :: cs -> 
                let p1, p2 = toPort s1, toPort s2
                
                let ls', ps' =
                    match p1, p2 with
                    | L id1, L id2 -> Set.add id1 ls |> Set.add id2, ps
                    | L id1, p2 -> Set.add id1 ls, Set.add (getPortId p2) ps
                    | p1, L id2 -> Set.add id2 ls, Set.add (getPortId p1) ps
                    | p1, p2 -> 
                        raiseNetworkError $"Points must not be connected (%s{p1.ToString()} -/-> %s{p2.ToString()})."

                let csUp' = conditionallyAddConnection (p1, p2) csUp
                let csDown' = conditionallyAddConnection (p2, p1) csDown

                processConnectionsAux (ls', ps', csUp', csDown') cs
        
        processConnectionsAux (Set.empty, Set.empty, Map.empty, Map.empty) connectionsList

    /// A point must have the stem port on one side and the minus and plus ports on the other side. 
    let pointInvariant fromPorts toPorts portId =
        match List.filter (portWithId portId) fromPorts with
        | [ S _ ] ->
            match List.filter (portWithId portId) toPorts with
            | [ P _; M _ ] | [ M _; P _ ] -> true
            | _ -> false
        | [ M _; P _ ] | [ P _; M _ ] ->
            match List.filter (portWithId portId) toPorts with
            | [ S _ ] -> true
            | _ -> false
        | _ -> false 

    /// A linear segment and a point cannot share ID. 
    let uniqueIDCheck linearSegments points = 
        let idIntersect = Set.intersect linearSegments points 

        if idIntersect <> Set.empty then 
            printList (Set.toList idIntersect)
            |> sprintf "Linear segments and points must not share IDs (%s)"
            |> raiseNetworkError

    /// All points must satisfy the point invariant. 
    let pointInvariantCheck fromPorts toPorts points = 
        let invalids = Set.filter (fun id -> not (pointInvariant fromPorts toPorts id)) points

        if invalids <> Set.empty then 
            printList (Set.toList invalids)
            |> sprintf "Point(s) with ID(s) %s are invalid."
            |> raiseNetworkError

    /// All signals must be placed on a linear segment. 
    let signalPlacementCheck signals linearSegments = 
        let invalids = Set.filter (fun s -> not (Set.contains s linearSegments)) signals 

        if invalids <> Set.empty then 
            printList (Set.toList invalids)
            |> sprintf "Following signal(s) not placed on linear segment(s): %s"
            |> raiseNetworkError

    /// Given a list of train tuples (start, destination) add them to a map if  each train's start and destination is unique.
    let getTrainMap linearSegments trainsList = 
        let rec collectTrains ls tsMap = function 
            | []  -> tsMap 
            | (t, _) :: _ | (_, t) :: _ when not (Set.contains t ls) -> 
                raiseNetworkError $"Train starts or ends on %s{t} which is not a linear segment."
            | (t1, t2) :: _ when Map.exists (fun s d -> s = t1 || d = t2) tsMap -> 
                raiseNetworkError $"Two trains start or end at %s{t1} or %s{t2}"
            | (t1, t2) :: ts -> collectTrains ls (Map.add t1 t2 tsMap) ts

        collectTrains linearSegments Map.empty trainsList

    /// Check if a cycle is present in a network given the connections in an arbitrary direction.   
    let cycleCheck connectionsMap = 
        let rec dfsCycles connections port visited =
            if Set.contains port visited then
                true
            else
                getNeighbours port connections
                |> cycleCheckNeighbors connections (Set.add port visited) 
        and cycleCheckNeighbors cs vs = function
            | [] -> false
            | p :: ns -> dfsCycles cs p vs || cycleCheckNeighbors cs vs ns
        
        if Map.exists (fun from _ -> dfsCycles connectionsMap from Set.empty) connectionsMap then
            raiseNetworkError "A cycle is present in the railway network."

    /// Convert lists of connections, signals, and trains to a railway network.
    let railwayParserOutputToNetwork (connectionList, signalList, trainList) =
        try 
            let (linearSegments, points, connectionsUp, connectionsDown) = processConnections connectionList

            let signals = Set.ofList signalList
            let trains = getTrainMap linearSegments trainList

            let fromPorts = Seq.toList <| Map.keys connectionsUp
            let toPorts = Seq.toList <| Map.values connectionsUp

            // Check other aspects of well-formedness. 
            uniqueIDCheck linearSegments points 
            pointInvariantCheck fromPorts toPorts points 
            signalPlacementCheck signals linearSegments 
            cycleCheck connectionsUp 

            N(linearSegments, points, connectionsUp, connectionsDown, signals, trains)
        with 
            | :? NetworkError -> reraise()
            