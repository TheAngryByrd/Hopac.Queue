namespace Hopac


open Hopac
open Hopac.Infixes
open System


module Infixes =
    let (^) = (<|)

    let (>^=>) x1 x2 = x1 >> Alt.afterJob x2

module Job =
    let inline whenDo' a b = Job.whenDo b a
    let inline whenDoSome (opt: Option<'a>) (action : 'a -> #Job<unit>) =
        match opt with
        | Some a -> action a :> Job<unit>
        | None -> Job.unit ()
    let inline whenDoSome' action opt = whenDoSome opt action


module Queue =
    open Infixes
    type LazyQueue<'a> = Job<BoundedMb<'a>>

    let iterJob (iterator : 'a -> #Job<unit>) (inbox : LazyQueue<'a>) =
        inbox
        >>= (BoundedMb.take >^=> (iterator) >> Job.foreverServer)

    let consumeJob iterator = iterJob iterator >> start

    let iterFun (iterator : 'a -> unit) (inbox : LazyQueue<'a>) =
        iterJob ( Job.lift iterator ) inbox

    let consumeFun iterator = iterFun iterator >> start

    let chooseJob (queueSize : int) (predicate : 'a -> #Job<Option<'a>>) (inbox : LazyQueue<'a>) : LazyQueue<'a> =
        let outBox =  BoundedMb<'a>(queueSize)
        let iterator =
            fun next ->
                predicate next
                >>= (Job.whenDoSome' (BoundedMb.put outBox))
        iterJob iterator inbox
        >>-. outBox

    let chooseFun (queueSize : int) (predicate : 'a -> Option<'a>) (inbox : LazyQueue<'a>) : LazyQueue<'a> =
        chooseJob queueSize (Job.lift predicate) inbox

    let filterJob (queueSize : int) (predicate : 'a -> #Job<bool>) (inbox : LazyQueue<'a>) : LazyQueue<'a> =
        let outBox =  BoundedMb<'a>(queueSize)
        let iterator =
            fun next ->
                predicate next
                >>= (Job.whenDo' (BoundedMb.put outBox next))
        iterJob iterator inbox
        >>-. outBox

    let filterFun (queueSize : int) (predicate : 'a -> bool) (inbox : LazyQueue<'a>) : LazyQueue<'a> =
        filterJob queueSize (Job.lift predicate) inbox

    let mapJob (queueSize : int) (mapper : 'a -> #Job<'b>) (inbox : LazyQueue<'a>) =
        let outBox =  BoundedMb<'b>(queueSize)
        let iterator  = fun next ->
            next |> mapper >>= BoundedMb.put outBox
        iterJob iterator inbox
        >>-. outBox

    let mapFun (queueSize : int) (mapper : 'a -> 'b) (inbox : LazyQueue<'a>) =
        mapJob queueSize (Job.lift mapper) inbox


    let fromSeq queueSize (xs : 'a seq) =
        let outBox =  BoundedMb<'a>(queueSize)
        xs
        |> Seq.map(BoundedMb.put outBox)
        |> Job.seqIgnore
        |> Job.start
        >>-. outBox

    let indefinitely queueSize (j : #Job<'a>) =
        let outBox =  BoundedMb<'a>(queueSize)
        j
        >>=  BoundedMb.put outBox
        |> Job.foreverServer
        >>-. outBox


    let sendTo outbox inbox =
        iterJob (BoundedMb.put outbox) inbox


