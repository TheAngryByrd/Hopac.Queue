module Tests

open Hopac
open Expecto
open Hopac.Queue

[<Tests>]
let tests =
  testList "samples" [
    testCaseJob "filterFun" <| job {
        let inbox = BoundedMb(1)
        let finished = IVar()
        let results = ResizeArray<_>()
        let finishedCondition ()=
            if results.Count = 1 then
                IVar.tryFill finished () |> start

        let inbox' = inbox |> Job.result
        let out =
            inbox'
            |> filterFun 1 (fun a -> a % 2 = 0)

        out
        |> consumeFun (results.Add >> finishedCondition)


        do! BoundedMb.put inbox 1
        do! BoundedMb.put inbox 2


        do!
            Alt.choose [
                finished :> Alt<_>
                timeOutMillis 1000
            ]
    }
    testCaseJob "mapFun" <| job {
        let inbox = BoundedMb(1)
        let finished = IVar()
        let results = ResizeArray<_>()
        let finishedCondition ()=
            if results.Count = 2 then
                IVar.tryFill finished () |> start

        let inbox' = inbox |> Job.result
        let out =
            inbox'
            |> mapFun 1 (fun a -> a * 2)

        out
        |> consumeFun (results.Add >> finishedCondition)


        do! BoundedMb.put inbox 1
        do! BoundedMb.put inbox 2


        do!
            Alt.choose [
                finished :> Alt<_>
                timeOutMillis 1000
            ]
    }

    testCaseJob "chooseFun" <| job {

        let chooser a =
            if a % 2 = 0 then
                Some a
            else
                None
        let inbox = BoundedMb(1)
        let finished = IVar()
        let results = ResizeArray<_>()
        let finishedCondition ()=
            if results.Count = 1 then
                IVar.tryFill finished () |> start

        let inbox' = inbox |> Job.result
        let out =
            inbox'
            |> chooseFun 1 (chooser)

        out
        |> consumeFun (results.Add >> finishedCondition)


        do! BoundedMb.put inbox 1
        do! BoundedMb.put inbox 2


        do!
            Alt.choose [
                finished :> Alt<_>
                timeOutMillis 1000
            ]
    }

    testCaseJob "fromSeq" <| job {
        let finished = IVar()
        let results = ResizeArray<_>()

        let initSeq = [1..10]

        let finishedCondition ()=
            if results.Count >= initSeq.Length then
                IVar.tryFill finished () |> start

        let out = fromSeq 4 initSeq

        out
        |> consumeFun (results.Add >> finishedCondition)

        do!
            Alt.choose [
                finished :> Alt<_>
                timeOutMillis 1000 |> Alt.afterFun (fun _ -> failwith "Timeout")
            ]
        Expect.sequenceEqual results initSeq "Same"
    }
    testCaseJob "idenfinitely" <| job {
        let finished = IVar()
        let results = ResizeArray<_>()

        let inbox = Ch ()

        let finishedCondition ()=
            if results.Count >= 3 then
                IVar.tryFill finished () |> start

        let out = indefinitely 2 inbox

        out
        |> consumeFun (results.Add >> finishedCondition)

        do! Ch.send inbox 1
        do! Ch.send inbox 2
        do! Ch.send inbox 3
        do!
            Alt.choose [
                finished :> Alt<_>
                timeOutMillis 1000 |> Alt.afterFun (fun _ -> failwith "Timeout")
            ]
        Expect.isTrue (results.Count = 3) "Same"
    }

    // testCaseJob "idenfinitely2" <| job {
    //     let finished = IVar()
    //     let results = ResizeArray<_>()

    //     let inbox = Ch ()
    //     let max = 10000000
    //     let finishedCondition ()=
    //         if results.Count >= max then
    //             IVar.tryFill finished () |> start

    //     let out = indefinitely 200 inbox

    //     out
    //     |> consumeFun (results.Add >> finishedCondition)

    //     do!
    //         [1..max]
    //         |> Seq.map (Ch.send inbox)
    //         |> Job.seqIgnore

    //     do!
    //         Alt.choose [
    //             finished :> Alt<_>
    //             // timeOutMillis 1000 |> Alt.afterFun (fun _ -> failwith "Timeout")
    //         ]

    //     Expect.isTrue (results.Count = max) (sprintf "%A" results.Count)
    // }


    // testCaseJob "stream indef" <| job {

    //     let finished = IVar()
    //     let results = ResizeArray<_>()
    //     let inbox = Ch ()
    //     let max = 10000000

    //     let finishedCondition ()=
    //         if results.Count >= max then
    //             IVar.tryFill finished () |> start

    //     Stream.indefinitely inbox
    //     |> Stream.mapFun (fun x -> x*2)
    //     |> Stream.mapFun (fun x -> x/2)
    //     |> Stream.mapFun (fun x -> x-2)
    //     |> Stream.mapFun (fun x -> x+2)
    //     |> Stream.mapFun (fun x -> x-2)
    //     |> Stream.mapFun (fun x -> x+2)
    //     |> Stream.consumeFun (results.Add >> finishedCondition)

    //     do!
    //         [1..max]
    //         |> Seq.map (Ch.send inbox)
    //         |> Job.seqIgnore

    //     do!
    //         Alt.choose [
    //             finished :> Alt<_>
    //             // timeOutMillis 1000 |> Alt.afterFun (fun _ -> failwith "Timeout")
    //         ]

    //     Expect.isTrue (results.Count = max) (sprintf "%A" results.Count)
    // }


  ]
