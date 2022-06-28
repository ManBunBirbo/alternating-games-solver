namespace RailwayLib

open System.Collections.Generic
open RailwayLib

module ToGameSolver =

    open AuxiliaryRailwayFunctions
    open OnTheFlySolver.Solver
    open OnTheFlySolver.Player

    let discoveredStates = HashSet<TrainGameState>()

    /// Any of trains crashed in either direction.
    let hasCrashed trainsUp trainsDown =
        Array.contains None trainsUp
        || Array.contains None trainsDown

    /// No train at the location of port in either direction.
    let noTrainAt port trainsUp trainsDown =
        not (
            Array.contains port trainsUp
            || Array.contains port trainsDown
        )

    /// Only look at signals who can currently block trains.
    let getRelevantSignals trainsUp trainsDown redSignals =
        Set.filter
            (fun rs ->
                Array.contains (Some(L rs)) trainsUp
                || Array.contains (Some(L rs)) trainsDown)
            redSignals
        |> powerSetOf

    /// Only get the points that trains have a chance of reaching currently.
    let getRelevantMinusPoints trainsUp trainsDown connectionsUp connectionsDown points =
        /// Auxiliary function to see if a port is next to the port with id for a given map of connections cs.
        let isNextTo id cs =
            function
            | None -> false
            | Some p -> List.exists (portWithId id) (getNeighbours p cs)

        Set.filter
            (fun id ->
                Array.exists (isNextTo id connectionsUp) trainsUp
                || Array.exists (isNextTo id connectionsDown) trainsDown)
            points
        |> powerSetOf

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

    let moveTrains noOfTrains connectionsUp connectionsDown trainGameState = failwith "Implement me."

    let toSolver maxNoOfConcurrentTrains (N (_, points, connectionsUp, connectionsDown, signals, trains)) =

        let simRel (TGS (trainsUP1, trainsDown1, _, _), i1 as c1) (TGS (trainsUp2, trainsDown2, _, _), i2 as c2) =
            match i1, i2 with
            | One, One -> trainsUP1 = trainsUp2 && trainsDown1 = trainsDown2
            | Two, Two -> c1 = c2
            | _ -> false

        let edgesOne (TGS (trainsUp, trainsDown, _, _)) =
            let mutable nextStates = Set.empty

            // TODO Rewrite and make n as input maxNoOfConcurrentTrains for solver.
            let n = maxNoOfConcurrentTrains
            let x' = getRelevantSignals trainsUp trainsDown signals
            let maxS = List.maxBy Set.count x' |> Set.count
            let x = List.filter (fun s -> Set.count s >= maxS - n) x'

            let y =
                getRelevantMinusPoints trainsUp trainsDown connectionsUp connectionsDown points

            for rsc in x do
                for mpc in y do
                    let state = TGS(trainsUp, trainsDown, rsc, mpc)
                    nextStates <- Set.add state nextStates
                    discoveredStates.Add(state) |> ignore
            // noOfStates <- noOfStates + 1

            nextStates

        let edgesTwo (TGS (tsUp, tsDown, rss, mps) as tgs) =
            // TODO Be able to move more trains at the same time.
            let mutable nextStates = Set.empty

            for i in [ 0 .. Array.length tsUp - 1 ] do
                let tsUp' =
                    move tsUp.[i] connectionsUp tgs
                    |> copyArrayReplaceValue tsUp i

                if tsUp' <> tsUp then
                    let state = TGS(tsUp', tsDown, rss, mps)
                    nextStates <- Set.add (state) nextStates
                    discoveredStates.Add(state) |> ignore

            for j in [ 0 .. Array.length tsDown - 1 ] do
                let tsDown' =
                    move tsDown.[j] connectionsDown tgs
                    |> copyArrayReplaceValue tsDown j

                if tsDown' <> tsDown then
                    let state = TGS(tsUp, tsDown', rss, mps)
                    nextStates <- Set.add (state) nextStates
                    discoveredStates.Add(state) |> ignore

            nextStates

        let edges player (destUp, destDown) (TGS (tsUp, tsDown, _, _) as state) =
            match player with
            | _ when
                hasCrashed tsUp tsDown
                || not (Array.forall2 (canReach connectionsUp) tsUp destUp)
                || not (Array.forall2 (canReach connectionsDown) tsDown destDown)
                ->
                Set.empty
            | One -> edgesOne state
            | Two -> edgesTwo state

        let trainsUp, trainsDown =
            Map.fold
                (fun (ups, downs) start final ->
                    let pStart, pFinal = Some(L start), Some(L final)
                    // Assumes all trains can reach their destination.
                    if canReach connectionsUp pStart pFinal then
                        (pStart, pFinal) :: ups, downs
                    else
                        ups, (pStart, pFinal) :: downs)
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

        let edges1 = edges One (upDest |> Array.ofList, downDest |> Array.ofList) 
        let edges2 = edges Two (upDest |> Array.ofList, downDest |> Array.ofList) 

        let game = edges1, edges2, isGoalState

        OnTheFlySolver<TrainGameState>(game, simRel, initConfig)

    let getNoOfStates () = discoveredStates.Count + 1
    