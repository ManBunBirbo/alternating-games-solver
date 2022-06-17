namespace RailwayLib

module GenerateNetwork =

    open AuxilaryRailwayFunctions

    let raiseNetworkError str = raise (NetworkError(str))

    /// Extract and apply port type to a point port.
    let toPort (id, portType) =
        match portType with 
        | "linear" -> L id
        | "stem" -> S id
        | "plus" -> P id
        | "minus" -> M id
        | s ->
            $"\"%s{s}\" is not a recognized port kind for ID \"%s{id}\"."
            |> raiseNetworkError

    /// Conditionally (p1 is not already a key) add a connection p1 -> p2 to a
    /// map of connections cs. 
    let condAddConn (p1, p2) cs =
        match Map.tryFindKey (fun p _ -> p = p1) cs with
        | None -> Map.add p1 p2 cs
        | Some p' -> 
            $"Port \"%s{p'.ToString()}\" is not deterministic."
            |> raiseNetworkError

    /// Process the list of conn(ection)s between ports into the following
    /// port-tuple:
    /// - Set of linear segments' IDs (ls),
    /// - set of points' IDs (ps),
    /// - connections in up-direction (csUp),
    /// - connections in down-direction (csDown).
    let processConns connsList = 

        /// Auxilary tail-recursive function doing the actual work of the
        /// processCons function. (Hidden due to strange initial values).
        let rec processConnsAux (ls, ps, csUp, csDown as pt) =
            function
            | [] -> pt
            | (p1, p2) :: cs -> // processConnsRecAux pt c cs
                let ls', ps' =
                    match p1, p2 with
                    | L id1, L id2 -> Set.add id1 ls |> Set.add id2, ps
                    | L id1, p2 -> Set.add id1 ls, Set.add (getPortId p2) ps
                    | p1, L id2 -> Set.add id2 ls, Set.add (getPortId p1) ps
                    | p1, p2 -> 
                        (p1.ToString(), p2.ToString())
                        ||> sprintf "Points must not be connected (%s -/-> %s)."
                        |> raiseNetworkError

                let csUp' = condAddConn (p1, p2) csUp
                let csDown' = condAddConn (p2, p1) csDown

                processConnsAux (ls', ps', csUp', csDown') cs
        
        processConnsAux (Set.empty, Set.empty, Map.empty, Map.empty) connsList

    /// A point must have the stem port on one side and the minus and plus 
    /// ports on the other side. 
    // TODO Does processConns and condAddConn deal with most of this?
    let pointInvariant fromPorts toPorts id =
        match List.filter (portWithId id) fromPorts with
        | [ S _ ] ->
            match List.filter (portWithId id) toPorts with
            | [ P _; M _ ]
            | [ M _; P _ ] -> true
            | _ -> false
        | [ M _; P _ ]
        | [ P _; M _ ] ->
            match List.filter (portWithId id) toPorts with
            | [ S _ ] -> true
            | _ -> false
        | _ -> false 

    /// A linear segment must appear at least once the graph and must appear 
    /// at most once in both fromPorts and toPorts. 
    // TODO Does processConns and condAddConn deal with most of this?
    let linearInvariant fromPorts toPorts id =
        match List.filter (portWithId id) fromPorts with
        | [] ->
            match List.filter (portWithId id) toPorts with
            | [ L _ ] -> true
            | _ -> false
        | [ L _ ] ->
            match List.filter (portWithId id) toPorts with
            | []
            | [ L _ ] -> true
            | _ -> false
        | _ -> false 

    /// Print a list of values in a readable format. 
    let rec printList = 
        function 
        | [] -> "" 
        | [ s ] -> s.ToString() 
        | [ s1; s2 ] -> $"%s{s1.ToString()} and %s{s2.ToString()}"
        | [ s1; s2; s3 ] -> 
            $"%s{s1.ToString()}, %s{s2.ToString()}, and %s{s3.ToString()}"
        | s :: ls -> $"%s{s.ToString()}, " + printList ls 

    /// A linear segment and a point cannot share ID. 
    let uniqueIDCheck ls ps = 
        let idIntersect = Set.intersect ls ps 

        if idIntersect <> Set.empty then 
            printList (Set.toList idIntersect)
            |> sprintf "Linear segments and points must not share IDs (%s)"
            |> raiseNetworkError

    /// All points must satifsy the point invariant. 
    let pointInvariantCheck froms tos ps = 
        let invalids = 
            Set.filter (fun id -> not (pointInvariant froms tos id)) ps

        if invalids <> Set.empty then 
            printList (Set.toList invalids)
            |> sprintf "Point(s) with ID(s) %s are invalid."
            |> raiseNetworkError

    /// All linear segments must satisfy the linear invariant. 
    let linearInvariantCheck froms tos ls = 
        let invalids = 
            Set.filter (fun id -> not (linearInvariant froms tos id)) ls 
        
        if invalids <> Set.empty then 
            printList (Set.toList invalids) 
            |> sprintf "Linear segment(s) with ID(s) %s are invalid."
            |> raiseNetworkError

    /// All signals must be placed on a linear segment. 
    let signalPlacementCheck ss ls = 
        let invalids = Set.filter (fun s -> not (Set.contains s ls)) ss 

        if invalids <> Set.empty then 
            printList (Set.toList invalids)
            |> sprintf "Following signal(s) not placed on linear segment(s): %s"
            |> raiseNetworkError

    /// Given a list of train tuples (start, destination) add them to a map if 
    /// each train's start and destination is unique.
    let getTrainMap ls tsList = 

        let rec collectTrains ls tsMap = 
            function 
            | []  -> tsMap 
            | (t, _) :: _
            | (_, t) :: _ when not (Set.contains t ls) -> 
                $"Train starts or ends on %s{t} which is not a linear segment."
                |> raiseNetworkError
            | (t1, t2) :: _ when Map.exists (fun s d -> s = t1 || d = t2) tsMap -> 
                $"Two trains start or end at %s{t1} or %s{t2}"
                |> raiseNetworkError
            | (t1, t2) :: ts -> collectTrains ls (Map.add t1 t2 tsMap) ts

        collectTrains ls Map.empty tsList

    /// Check if a cycle is present in a network given the connections in an 
    /// arbitrary direction.   
    let cycleCheck cs = 

        // Inspiration: https://www.geeksforgeeks.org/detect-cycle-in-a-graph/
        let rec dfsCycles conns port vs =
            if Set.contains port vs then
                true
            else
                getNeighbours port conns
                |> recNs conns (Set.add port vs)
        and recNs conns vs =
            function
            | [] -> false
            | p :: ns -> dfsCycles conns p vs || recNs conns vs ns
        
        if Map.exists (fun p _ -> dfsCycles cs p Set.empty) cs then
            raiseNetworkError "A cycle is present in the railway network."

    /// Convert lists of conn(ection)s, signals, and trains to a railway network.
    let toNetwork connsList signalList trainList =
        try 
            let linears, points, connsUp, connsDown = 
                List.map (fun (p1, p2) -> toPort p1, toPort p2) connsList
                |> processConns 

            let signals = Set.ofList signalList
            let trains = getTrainMap linears trainList

            let fromPorts = Seq.toList (Map.keys connsUp)
            let toPorts = Seq.toList (Map.values connsUp)

            uniqueIDCheck linears points 
            pointInvariantCheck fromPorts toPorts points 
            linearInvariantCheck fromPorts toPorts linears
            signalPlacementCheck signals linears 
            cycleCheck connsUp 

            N(linears, points, connsUp, connsDown, signals, trains)
        with 
            | :? NetworkError -> reraise() 