namespace RailwayLib

module GameFunctions =

    open AuxilaryRailwayFunctions
    open OnTheFlySolver.Solver
    open OnTheFlySolver.Player

    /// Any of trains crashed in either direction. 
    let hasCrashed tsUp tsDown =
        Array.contains None tsUp || Array.contains None tsDown

    /// No train at the location of port in either direction. 
    let noTrainAt port tsUp tsDown =
        not (Array.contains port tsUp || Array.contains port tsDown)

    /// Only look at signals who can currently block trains. 
    let getRelevantSignals tsUp tsDown ss =
        Set.filter 
            (fun rs -> 
                Array.contains (Some (L rs)) tsUp 
                || Array.contains (Some (L rs)) tsDown)
            ss
        |> getCombinations
        
    /// Only get the points that trains have a chance of reaching currently. 
    let getRelevantMinusPoints tsUp tsDown csUp csDown ps = 
        /// Auxiliary function to see if a port is next to the port with id for
        /// a given map of connections cs. 
        let isNextTo id cs = 
            function 
            | None -> false 
            | Some p -> List.exists (portWithId id) (getNeighbours p cs)

        Set.filter 
            (fun id -> 
                Array.exists (isNextTo id csUp) tsUp 
                || Array.exists (isNextTo id csDown) tsDown) 
            ps 
        |> getCombinations

    /// Move to the next linear segment if signals/other trains allow. 
    let rec move currentPos conns (TGS (tsUp, tsDown, rss, mps) as tgs) =
        match currentPos with
        | None -> None
        | Some (L id) as pOpt when Set.contains id rss -> pOpt
        | Some p ->
            match Map.tryFind p conns with
            | Some (L _) as pOpt when noTrainAt pOpt tsUp tsDown -> pOpt
            | Some (P id) when not (Set.contains id mps) -> move (Some(S id)) conns tgs
            | Some (M id) when Set.contains id mps -> move (Some(S id)) conns tgs
            | Some (S id) when Set.contains id mps -> move (Some(M id)) conns tgs
            | Some (S id) -> move (Some(P id)) conns tgs
            | _ -> None
    
    let toSolver (N (_, points, connsUp, connsDown, signals, trains)) =   

        let simRel 
            (TGS (tsUp1, tsDown1, _, _), i1 as c1) 
            (TGS (tsUp2, tsDown2, _, _), i2 as c2) = 

            match i1, i2 with 
            | One, One -> tsUp1 = tsUp2 && tsDown1 = tsDown2
            | Two, Two -> c1 = c2 
            | _ -> false 

        let edgesOne (TGS (tsUp, tsDown, _, _)) =
            let mutable nextStates = Set.empty

            // TODO Rewrite and make n variable. 
            let n = 1
            let x' = getRelevantSignals tsUp tsDown signals 
            let maxS = List.maxBy Set.count x' |> Set.count
            let x = List.filter (fun s -> Set.count s >= maxS - n) x'
            let y = getRelevantMinusPoints tsUp tsDown connsUp connsDown points
            for rsc in x do
                for mpc in y do
                    nextStates <- Set.add (TGS(tsUp, tsDown, rsc, mpc)) nextStates

            nextStates

        let edgesTwo (TGS (tsUp, tsDown, rss, mps) as tgs) =

            let mutable nextStates = Set.empty

            for i in [ 0 .. Array.length tsUp - 1 ] do
                let tsUp' =
                    move tsUp.[i] connsUp tgs
                    |> copyArrayReplaceValue tsUp i

                if tsUp' <> tsUp then
                    nextStates <- Set.add (TGS(tsUp', tsDown, rss, mps)) nextStates

            for j in [ 0 .. Array.length tsDown - 1 ] do
                let tsDown' =
                    move tsDown.[j] connsDown tgs
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

            if hasCrashed tsUp tsDown || not (reachable destUp tsUp connsUp) || not (reachable destDown tsDown connsDown) then
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
                    if dfs connsUp pStart Set.empty
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