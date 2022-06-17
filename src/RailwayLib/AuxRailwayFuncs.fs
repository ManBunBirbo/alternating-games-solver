namespace RailwayLib

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
        let rec getCombinationsFolder setCollector elem =
            match setCollector with
            | [] -> []
            | sc :: scs -> sc :: (Set.add elem sc) :: (getCombinationsFolder scs elem)

        Set.fold getCombinationsFolder [ Set.empty ] set

    /// From a given port get the neighbors in conn(ection)s graph.
    let getNeighbours port conns =
        match Map.tryFind port conns with
        | None -> []
        | Some (L _ as p) -> [ p ]
        | Some (S id) -> [ (P id); (M id) ]
        | Some (M id)
        | Some (P id) -> [ (S id) ]

    /// Depth-first-search through a network from a port in the direction determined by conn(ection)s map.
    let rec dfs conns port visited =
        if Set.contains port visited then
            visited
        else
            getNeighbours port conns
            |> recNeighbors conns (Set.add port visited)
    and recNeighbors conns visited =
        function
        | [] -> visited
        | p :: ns -> recNeighbors conns (dfs conns p visited) ns

    let copyArrayReplaceValue array index value =
        let arrayCopy = Array.copy array
        arrayCopy.[index] <- value
        arrayCopy
