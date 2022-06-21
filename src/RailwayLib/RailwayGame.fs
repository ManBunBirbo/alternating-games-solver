namespace RailwayLib

module ToGameSolver =

    open AuxiliaryRailwayFunctions
    open OnTheFlySolver.Solver
    open OnTheFlySolver.Player
    
    let mutable noOfStates = 0

    /// Any of trains crashed in either direction. 
    let hasCrashed trainsUp trainsDown = Array.contains None trainsUp || Array.contains None trainsDown

    /// No train at the location of port in either direction. 
    let noTrainAt port trainsUp trainsDown = not (Array.contains port trainsUp || Array.contains port trainsDown)

    /// Only look at signals who can currently block trains. 
    let getRelevantSignals trainsUp trainsDown redSignals =
        Set.filter 
            (fun rs -> Array.contains (Some (L rs)) trainsUp || Array.contains (Some (L rs)) trainsDown)
            redSignals
        |> supersetOf
        
    /// Only get the points that trains have a chance of reaching currently. 
    let getRelevantMinusPoints trainsUp trainsDown connectionsUp connectionsDown points = 
        /// Auxiliary function to see if a port is next to the port with id for
        /// a given map of connections cs. 
        let isNextTo id cs = function 
            | None -> false 
            | Some p -> List.exists (portWithId id) (getNeighbours p cs)

        Set.filter 
            (fun id ->
                Array.exists (isNextTo id connectionsUp) trainsUp
                || Array.exists (isNextTo id connectionsDown) trainsDown) 
            points 
        |> supersetOf

    /// Move to the next linear segment if signals/other trains allow. 
    let rec move currentPos connections (TGS (trainsUp, trainsDown, redSignals, minusPoints) as tgs) =
        match currentPos with
        | None -> None
        | Some (L id) as pOpt when Set.contains id redSignals -> pOpt
        | Some p ->
            match Map.tryFind p connections with
            | Some (L _) as pOpt when noTrainAt pOpt trainsUp trainsDown -> pOpt
            | Some (P id) when not (Set.contains id minusPoints) -> move (Some(S id)) connections tgs
            | Some (M id) when Set.contains id minusPoints -> move (Some(S id)) connections tgs
            | Some (S id) when Set.contains id minusPoints -> move (Some(M id)) connections tgs
            | Some (S id) -> move (Some(P id)) connections tgs
            | _ -> None
    
    let toSolver (N (_, points, connectionsUp, connectionsDown, signals, trains)) =   

        let simRel (TGS (trainsUP1, trainsDown1, _, _), i1 as c1) (TGS (trainsUp2, trainsDown2, _, _), i2 as c2) = 
            match i1, i2 with 
            | One, One -> trainsUP1 = trainsUp2 && trainsDown1 = trainsDown2
            | Two, Two -> c1 = c2 
            | _ -> false 

        let edgesOne (TGS (trainsUp, trainsDown, _, _)) =
            let mutable nextStates = Set.empty

            // TODO Rewrite and make n variable. 
            let n = 1
            let x' = getRelevantSignals trainsUp trainsDown signals 
            let maxS = List.maxBy Set.count x' |> Set.count
            let x = List.filter (fun s -> Set.count s >= maxS - n) x'
            let y = getRelevantMinusPoints trainsUp trainsDown connectionsUp connectionsDown points
            for rsc in x do
                for mpc in y do
                    nextStates <- Set.add (TGS(trainsUp, trainsDown, rsc, mpc)) nextStates
                    noOfStates <- noOfStates + 1

            nextStates

        let edgesTwo (TGS (tsUp, tsDown, rss, mps) as tgs) =

            let mutable nextStates = Set.empty

            for i in [ 0 .. Array.length tsUp - 1 ] do
                let tsUp' =
                    move tsUp.[i] connectionsUp tgs
                    |> copyArrayReplaceValue tsUp i

                if tsUp' <> tsUp then
                    nextStates <- Set.add (TGS(tsUp', tsDown, rss, mps)) nextStates

            for j in [ 0 .. Array.length tsDown - 1 ] do
                let tsDown' =
                    move tsDown.[j] connectionsDown tgs
                    |> copyArrayReplaceValue tsDown j

                if tsDown' <> tsDown then
                    nextStates <- Set.add (TGS(tsUp, tsDown', rss, mps)) nextStates

            nextStates

        let edges (destUp, destDown) player (TGS (tsUp, tsDown, _, _) as state) = 

            let reachable dest ts conns  = 
                Array.forall2
                    (fun t d -> 
                        match t with 
                        | Some p -> dfs conns p Set.empty |> Set.contains (Option.get d)
                        | None -> false)
                    ts 
                    dest 

            if hasCrashed tsUp tsDown || not (reachable destUp tsUp connectionsUp) || not (reachable destDown tsDown connectionsDown) then
                Set.empty 
            else 
                match player with 
                | One -> edgesOne state 
                | Two -> edgesTwo state
                
        let trainsUp, trainsDown =
            Map.fold
                (fun (ups, downs) start final ->
                    let pStart, pFinal = L start, L final

                    let tupleOpt = Some pStart, Some pFinal

                    // Train travelling up or down. Assumes all trains can reach their destination.
                    if dfs connectionsUp pStart Set.empty
                       |> Set.contains pFinal then
                        tupleOpt :: ups, downs
                    else
                        ups, tupleOpt :: downs)
                ([], [])
                trains

        let upStart, upDest = List.unzip trainsUp
        let downStart, downDest = List.unzip trainsDown

        let isGoalState (TGS (trainsUp, trainsDown, _, _)) =
            trainsUp = (Array.ofList upDest)
            && trainsDown = (Array.ofList downDest)

        let initState =
            TGS(Array.ofList upStart, Array.ofList downStart, Set.empty, Set.empty)

        let initConfig = initState, One

        let edges1 = edges (upDest |> Array.ofList, downDest |> Array.ofList) One  
        let edges2 = edges (upDest |> Array.ofList, downDest |> Array.ofList) Two

        let game = edges1, edges2, isGoalState

        new OnTheFlySolver<TrainGameState>(game, simRel, initConfig)

    let getNoOfStates () = noOfStates