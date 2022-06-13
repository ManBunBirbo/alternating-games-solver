module OnTheFlySolver

open System.Collections.Generic

type Player =
    | One
    | Two

let next player =
    match player with
    | One -> Two
    | Two -> One

type Game<'S when 'S: comparison> = ('S -> Set<'S>) * ('S -> Set<'S>) * ('S -> bool)

type Vertex = int

type Config<'S> = 'S * Player

type OnTheFlySolver<'S when 'S: comparison>
    (
        game: Game<'S>,
        simRel: Config<'S> -> Config<'S> -> bool,
        initConfig: Config<'S>
    ) =

    let edgesOne, edgesTwo, isGoalState = game

    // Reversed simulation relation.
    let revSimRel c2 c1 = simRel c1 c2

    // Sets for keeping track of winning an losing strategies.
    let mutable win = Set.empty
    let mutable lose = Set.empty

    // Dependencies when processesing a given vertex.
    let deps = new List<('S * Player * 'S) list>()

    // A set of already-discovered vertices.
    let disc = new HashSet<Vertex>()

    // Connect internal (vertices) and external (configuration) representation.
    let toConfig = new List<Config<'S>>()
    let toVertex = new Dictionary<Config<'S>, Vertex>()

    // Get the vertex of a given state if it exists. If not, create and return one.
    let vertexOf state =
        match toVertex.TryGetValue(state) with
        | true, vertex -> vertex
        | false, _ ->
            // A newly discovered state with no associated vertex.
            toConfig.Add state
            deps.Add []
            let vertex = toConfig.Count - 1
            toVertex.Add(state, vertex)

            vertex

    // Get the set of successor states from the relevant edges function.
    let getSuccessorConfigs (state, player) : Set<Config<'S>> =
        match player with
        | One ->
            // Player One can also (always) perform the empty action, i.e. stay in current state.
            edgesOne state |> Set.add state
        | Two ->
            let successorStates = edgesTwo state

            // Player Two can only perform empty actions when non other available.
            if Set.isEmpty successorStates then
                set [ state ]
            else
                successorStates

        // In the successor state set, assign next player to the states thus making them configurations.
        |> Set.map (fun state' -> state', next player)

    let addSuccessors config wait =
        // Auxiliary function to use as folder. Loads waiting list ws
        // while keeping track of minimum/maximum successors in ms.
        let getNewSuccessors (state, player) (ws, ms) =
            function
            | config' when player = One && Set.exists (simRel config') ms -> ws, ms
            | config' when player = Two && Set.exists (revSimRel config') ms -> ws, ms
            | state', _ as config' -> (state, player, state') :: ws, Set.add config' ms

        // Tuple of the updated waiting list along with a set of min/max successors.
        getSuccessorConfigs config
        |> Set.fold (getNewSuccessors config) (wait, Set.empty)

        // Only return the waiting list, _not_ the set of min/max successors.
        |> fst


    let inLosingStrategy (state, player as config) =
        // The config simulates a losing strategy, or
        Set.exists (simRel config) lose

        // (player One) all successors simulates a losing strategy, or
        || (player = One
            && getSuccessorConfigs config
               |> Set.forall (fun c' -> Set.exists (simRel c') lose))

        // (player Two) a successor simulates a losing strategy, or
        || (player = Two
            && getSuccessorConfigs config
               |> Set.exists (fun c' -> Set.exists (simRel c') lose))

        // (player Two) a losing config is simulated by a config consisting of
        // the given state but with player One in stead, or
        || (player = Two
            && Set.exists (simRel (state, One)) lose)

        // non of the players can move.
        || (Set.isEmpty (edgesOne state)
            && Set.isEmpty (edgesTwo state))

    let inWinningStrategy (_, player as config) =

        // A winning simulation simulates the config, or
        Set.exists (revSimRel config) win

        // (player One) a successor simulates a winning strategy, or
        || (player = One
            && getSuccessorConfigs config
               |> Set.exists (fun c' -> Set.exists (revSimRel c') win))

        // (player Two) all successors are simulated by a winning strategy.
        || (player = Two
            && getSuccessorConfigs config
               |> Set.forall (fun c' -> Set.exists (revSimRel c') win))


    let inWinLose config =
        Set.contains config win
        || Set.contains config lose

    // Add dependencies to waiting list conditionally.
    let rec addDeps (state, player) wait =
        function
        | [] -> wait
        | (s'', p', s') :: ds when
            s' = state
            && p' = next player
            && inWinLose (s'', p')
            ->
            // _Skip_ deps that _are_ in the win or lose set.
            addDeps (state, player) wait ds
        | d :: ds -> addDeps (state, player) (d :: wait) ds

    // Add configuration to win/lose set and return updated waiting list with dependencies.
    let addTo c (eitherWinOrLose: byref<_>) wait =
        eitherWinOrLose <- Set.add c eitherWinOrLose
        deps.[vertexOf c] |> addDeps c wait

    let foundWinningOrLosingStrategy () =
        Set.exists (revSimRel initConfig) win
        || Set.exists (simRel initConfig) lose

    let foundWinningStrategy () = Set.exists (revSimRel initConfig) win

    let rec loop =
        function 
        | [] -> printfn "Win: %A\nLose: %A" win lose; foundWinningStrategy ()
        | _ when foundWinningOrLosingStrategy () -> foundWinningStrategy ()
        | (s1, i, _) :: waitrest when inWinLose (s1, i) -> loop waitrest
        | (s1, i, _) :: waitrest when inLosingStrategy (s1, i) -> addTo (s1, i) &lose waitrest |> loop
        | (s1, i, _) :: waitrest when inWinningStrategy (s1, i) -> printfn "HELLO"; addTo (s1, i) &win waitrest |> loop
        | (_, i, s2) :: waitrest when inWinLose (s2, next i) -> loop waitrest
        | (s1, i, s2) :: waitrest when disc.Contains(vertexOf (s2, next i)) ->
            let v = vertexOf (s2, next i)
            deps.[v] <- (s1, i, s2) :: deps.[v]
            loop waitrest
        | (s1, i, s2) :: waitrest ->
            let config = s2, next i
            let v = vertexOf config
            disc.Add(v) |> ignore
            deps.[v] <- [ (s1, i, s2) ]

            if isGoalState s2 then 
                addTo config &win waitrest |> loop
            else 
                addSuccessors config waitrest |> loop

    member _.solve =
        // Reset. 
        toVertex.Clear()
        toConfig.Clear()
        disc.Clear()
        win <- Set.empty
        lose <- Set.empty
        deps.Clear()

        // Add initial values. 
        let vertex = vertexOf initConfig
        disc.Add(vertex) |> ignore

        // Initialize algorithm. 
        let initState, _ = initConfig

        if isGoalState initState then
            addTo initConfig &win []
        else 
            []
        |> addSuccessors initConfig
        |> loop


// Notes
// - System.Collections.ArrayList is not recommended:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.arraylist?view=net-6.0#remarks
// - System.Collections.Generic.List<T> class is used in stead:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
// - Documentation recommends System.Collections.Generic.Dictionary over
//   System.Collections.HashTable:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
// - Dictionary is implemented as a hash table:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
