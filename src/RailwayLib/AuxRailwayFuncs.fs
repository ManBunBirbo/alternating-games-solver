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

    /// Get the power set of a given set. 
    let powerSetOf set =
        let rec powerSetFolder setCollector elem =
            match setCollector with
            | [] -> []
            | sc :: scs -> sc :: Set.add elem sc :: powerSetFolder scs elem

        Set.fold powerSetFolder [ Set.empty ] set

    /// From a given port get the neighbors in conn(ection)s graph.
    let getNeighbours port connections =
        match Map.tryFind port connections with
        | None -> []
        | Some (L _ as p) -> [ p ]
        | Some (S id) -> [ (P id); (M id) ]
        | Some (M id)
        | Some (P id) -> [ (S id) ]

    /// Depth-first-search through a network from a port in the direction determined by connections map.
    let rec dfsAux cs port visited =
        if Set.contains port visited then
            visited
        else
            recNeighbors cs (Set.add port visited) (getNeighbours port cs)
    and recNeighbors conns visited = function
        | [] -> visited
        | p :: ns -> recNeighbors conns (dfsAux conns p visited) ns
        
    let dfs connections port = dfsAux connections port Set.empty 

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
        | [ s1; s2; s3 ] -> $"%s{s1.ToString()}, %s{s2.ToString()}, and %s{s3.ToString()}"
        | s :: ls -> $"%s{s.ToString()}, " + printList ls
    
    let canReach connections initPortOption destPortOption =
        match initPortOption, destPortOption with 
        | Some p, Some d -> Set.contains d (dfs connections p)
        | _ -> false
        



