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
        
    let getRelevantMinusPoints tsUp tsDown csUp csDown ps = 
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
    
    let toSolver (N (_, points, connsUp, connsDown, signals, trains)) =

        let minusPointsConfigs = getCombinations points        

        let simRel (TGS (tsUp1, tsDown1, rss1, mps1), i1 as c1) (TGS (tsUp2, tsDown2, rss2, mps2), i2 as c2) = 
            match i1, i2 with 
            | One, One -> tsUp1 = tsUp2 && tsDown1 = tsDown2
            | Two, Two -> 
                // Player two just needs one losing state. If there exists a possibity for a crash 
                // in c2, and that SAME possibilty exists in c1 then that must be losing too. 
                // Array.exists2 
                //     (fun a1 a2 -> 
                //         // the move is the same 
                //         // a1 = a2?
                //         false
                //         )
                //     move 
                c1 = c2
            | _ -> false 



        let edgesOne (TGS (tsUp, tsDown, _, _)) =
            let mutable nextStates = Set.empty

            for rsc in getRelevantSignals tsUp tsDown signals do
                for mpc in getRelevantMinusPoints tsUp tsDown connsUp connsDown points do
                    nextStates <- Set.add (TGS(tsUp, tsDown, rsc, mpc)) nextStates

            nextStates

        let edgesTwo (TGS (tsUp, tsDown, rss, mps)) =

            /// Move to the next linear segment if signals/other trains allow. 
            let rec move currentPos conns =
                match currentPos with
                | None -> None
                | Some (L id) as pOpt when Set.contains id rss -> pOpt
                | Some p ->
                    match Map.tryFind p conns with
                    | Some (L _) as pOpt when noTrainAt pOpt tsDown tsDown -> pOpt
                    | Some (P id) when not (Set.contains id mps) -> move (Some(S id)) conns
                    | Some (M id) when Set.contains id mps -> move (Some(S id)) conns
                    | Some (S id) when Set.contains id mps -> move (Some(M id)) conns
                    | Some (S id) -> move (Some(P id)) conns
                    | _ -> None

            let mutable nextStates = Set.empty

            for i in [ 0 .. Array.length tsUp - 1 ] do
                let tsUp' =
                    move tsUp.[i] connsUp
                    |> copyArrayReplaceValue tsUp i

                if tsUp' <> tsUp then
                    nextStates <- Set.add (TGS(tsUp', tsDown, rss, mps)) nextStates

            for j in [ 0 .. Array.length tsDown - 1 ] do
                let tsDown' =
                    move tsDown.[j] connsDown
                    |> copyArrayReplaceValue tsDown j

                if tsDown' <> tsDown then
                    nextStates <- Set.add (TGS(tsUp, tsDown', rss, mps)) nextStates

            nextStates

        let edges (destUp, destDown) player (TGS (tsUp, tsDown, _, _) as state) = 

            let reachable dest ts conns  = 
                assert (Array.length dest = Array.length ts) 

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