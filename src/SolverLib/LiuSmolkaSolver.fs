// Author: Markus Brammer Jensen, s183816
// See references in README. 

namespace Solver
 
type FixedValue =
    | Unknown
    | Zero
    | One

type Node = int
type HyperEdge = Node list

type Solver(graph: HyperEdge list array) =

    (* Arrays are initialized when class is instantiated *)

    /// Approximation assignments and  from [liu1998a].
    let approx = Array.init (Array.length graph) (fun _ -> Unknown)
    /// Dependencies assignments [liu1998a]
    let deps = Array.init (Array.length graph) (fun _ -> [])

    /// Load tuples (node, hyperEdge) into a list 'ws' for all hyper edges from
    /// 'node' in the dependency graph 'graph'. See 'Load' proc in Fig. 2 [0].
    let load ws node =
        (ws, graph.[node])
        ||> List.fold (fun ws' hyperEdge -> (node, hyperEdge) :: ws')

    /// Local LiuSmolka algorithm (fig. 4) from [liu1998a].
    // TODO Compare with Kok's "optimized" version, page 24. 
    let rec liuSmolkaLocal =
        function
        | [] -> approx
        | (node, _) :: ws when approx.[node] <> Zero -> liuSmolkaLocal ws
        | (node, hyperEdge) :: ws ->
            match List.skipWhile (fun v -> approx.[v] = One) hyperEdge with
            | [] ->
                approx.[node] <- One
                liuSmolkaLocal (ws @ deps.[node])
            | neighbor :: neighbors when approx.[neighbor] = Zero ->
                deps.[neighbor] <- (node, neighbors) :: deps.[neighbor]
                liuSmolkaLocal ws
            | neighbor :: neighbors ->
                approx.[neighbor] <- Zero
                deps.[neighbor] <- [ (node, neighbors) ]
                liuSmolkaLocal (load ws neighbor)

    /// Run LiuSmolka algorithm on an initial node using this solver. 
    member _.liuSmolka initNode =
        // Re-initialize assignment arrays. 
        Array.iteri (fun i _ -> approx.[i] <- Unknown) approx
        Array.iteri (fun i _ -> deps.[i] <- []) deps
        approx.[initNode] <- Zero

        // Run algorithm and the return result. 
        liuSmolkaLocal (load [] initNode)


module Solver =
    /// Create solver from hyper edge list array. 
    let ofArray (array: int list list array) = new Solver(array)

    /// Create solver from hyper edge list list. 
    let ofList (list: int list list list) = new Solver(Array.ofList list)

    /// Run LiuSmolka algorithm from a given initial node on a given Solver. 
    let liuSmolka initNode (solver: Solver) = solver.liuSmolka initNode
