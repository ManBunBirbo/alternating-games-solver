module LiuSmolkaTests

open Solver
open System
open Xunit

let arrayToDepGraph array = new Solver(array)

// --- SIMPLE -----------------------------------------------------------------
// A very simple dep. graph with two nodes: Node 0 has one hyper edge
// pointing to node 0. Node 1 has the empty hyper edge, ie. it will always
// end up with fixed value One.

let simpleDepGraph =
    Array.ofList [ [ [ 1 ] ]; [ [] ] ]
    |> arrayToDepGraph

[<Fact>]
let ``Simple dep graph All explored All One From node 0`` () =
    let expected = Array.ofList [ One; One ]
    let actual = simpleDepGraph.liuSmolka 0
    Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)

[<Fact>]
let ``Simple dep graph Node 1 explored and One Node 0 Unknown From node 1`` () =
    let expected = Array.ofList [ Unknown; One ]
    let actual = simpleDepGraph.liuSmolka 1
    Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)


// --- EXAMPLE FROM LIUSMOLKA PAPER -------------------------------------------
// The example from the LiuSmolka paper. Here, we always expect, no matter
// initial node, that the entire graph is explored with node 0 having fixed
// value One while node 1 and 2 both have Zero.

let lsDepGraph =
    [ [ []; [ 1; 2 ] ]
      [ [ 0; 2 ] ]
      [ [ 0; 1 ] ] ]
    |> Array.ofList
    |> arrayToDepGraph

[<Fact>]
let ``LiuSmolka Example All Explored From any init node`` () =
    let expected = Array.ofList [ One; Zero; Zero ]

    for i in [ 0..2 ] do
        let actual = lsDepGraph.liuSmolka i

        Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)

// --- MODIFIED EXAMPLE -------------------------------------------------------
// A modified version of the LiuSmolka example from above: Now, node 2 has
// another hyper edge, the empty hyper edge, meaning that it will also always
// be One independant of initial node. This will furthermore result in node 1
// also always having fixed value One after LiuSmolka.

let modLsDepGraph =
    [ [ []; [ 1; 2 ] ]
      [ [ 0; 2 ] ]
      [ []; [ 0; 1 ] ] ]
    |> Array.ofList
    |> arrayToDepGraph

[<Fact>]
let ``Modified LiuSmolka example All explored All One Always`` () =
    let expected = Array.ofList [ One; One; One ]

    for i in [ 0..2 ] do
        let actual = modLsDepGraph.liuSmolka i

        Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)

// --- DISCONNECTED EXAMPLE ---------------------------------------------------
// Here, the modified example graph from just above is extended with a node 3
// completely disconnected from the three other nodes. This means that for
// initial node < 3, the big are will result in the same as the modified
// example above while node 3 will be unknown. With initial node 3, Node 3 will
// be the only explored node with value One while the three other nodes are
// all Unknown.

let disDepGraph =
    [ [ []; [ 1; 2 ] ]
      [ [ 0; 2 ] ]
      [ []; [ 0; 1 ] ]
      [ [] ] ]
    |> Array.ofList
    |> arrayToDepGraph

[<Fact>]
let ``Small disconnected example Exploring bigger area`` () =
    let expected = Array.ofList [ One; One; One; Unknown ]

    for i in [ 0..2 ] do
        let actual = disDepGraph.liuSmolka i

        Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)

[<Fact>]
let ``Small disconnected example Exploring small area`` () =
    let expected =
        Array.ofList [ Unknown
                       Unknown
                       Unknown
                       One ]

    let actual = disDepGraph.liuSmolka 3
    Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)
