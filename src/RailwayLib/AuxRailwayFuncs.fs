namespace RailwayLib

module AuxiliaryRailwayFunctions =

    /// Determine if a port is linear.
    let isLinear = function
        | L _ -> true
        | _ -> false

    /// Get the id of a port.
    let getPortId = function
        | L id | S id | P id | M id -> id

    let portWithId id port = getPortId port = id

    /// Get the superset of a given set. 
    let supersetOf set =
        let rec supersetFolder setCollector elem =
            match setCollector with
            | [] -> []
            | sc :: scs -> sc :: Set.add elem sc :: supersetFolder scs elem

        Set.fold supersetFolder [ Set.empty ] set

    /// From a given port get the neighbors in conn(ection)s graph.
    let getNeighbours port conns =
        match Map.tryFind port conns with
        | None -> []
        | Some (L _ as p) -> [ p ]
        | Some (S id) -> [ (P id); (M id) ]
        | Some (M id)
        | Some (P id) -> [ (S id) ]

    /// Depth-first-search through a network from a port in the direction determined by connections map.
    let rec dfs cs port visited =
        if Set.contains port visited then
            visited
        else
            recNeighbors cs (Set.add port visited) (getNeighbours port cs)
    and recNeighbors conns visited = function
        | [] -> visited
        | p :: ns -> recNeighbors conns (dfs conns p visited) ns

    let copyArrayReplaceValue array index value =
        let arrayCopy = Array.copy array
        arrayCopy.[index] <- value
        arrayCopy

    /// Print a list of values in a readable format. 
    let rec printList = 
        function 
        | [] -> "" 
        | [ s ] -> s.ToString() 
        | [ s1; s2 ] -> $"%s{s1.ToString()} and %s{s2.ToString()}"
        | [ s1; s2; s3 ] -> 
            $"%s{s1.ToString()}, %s{s2.ToString()}, and %s{s3.ToString()}"
        | s :: ls -> $"%s{s.ToString()}, " + printList ls 




