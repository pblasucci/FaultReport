namespace MulberryLabs.FaultReport

open System
open System.Collections
open System.Collections.Generic


[<AutoOpen>]
module Library =
  let inline ``panic!`` (message : string) : 'T =
    try
      raise (InvalidProgramException message)
    with x ->
      Environment.FailFast("Fatal error; program must exit!", x)
      Unchecked.defaultof<'T>


[<Interface>]
type IFault =
  abstract Message : string
  abstract Cause : IFault option


type Demotion<'X when 'X :> exn>(source : 'X, ?message : string) =
  member _.Source : 'X = source

  member val Message : string = defaultArg message source.Message

  interface IFault with
    member me.Message = me.Message
    member _.Cause = None


type Faulty<'T when 'T : (member Message : string)> = 'T


[<RequireQualifiedAccess>]
module Fault =
  let demote (source : 'X :> exn) = Demotion<'X>(source)

  let inline derive<'T when Faulty<'T>> (faulty : 'T) : IFault =
    match box faulty with
    | :? IFault as fault -> fault
    | :? exn as source -> Demotion source
    | _ ->
      { new IFault with
          member _.Message = faulty.Message
          member _.Cause = None
      }

  let promote (toExn : IFault -> 'X) (fault : IFault) : 'X :> exn = toExn fault

  let escalate (toExn : IFault -> 'X) (fault : IFault) : 'X :> exn =
    fault |> promote toExn |> raise


type Report<'Pass, 'Fail when 'Fail :> IFault> =
  | Pass of value : 'Pass
  | Fail of fault : 'Fail

  static member op_Implicit
    (report : Report<'Pass, 'Fail>)
    : Result<'Pass, 'Fail>
    =
    match report with
    | Pass(value : 'Pass) -> Ok value
    | Fail(error : 'Fail) -> Error error

  static member op_Implicit
    (result : Result<'Pass, 'Fail>)
    : Report<'Pass, 'Fail>
    =
    match result with
    | Ok(value : 'Pass) -> Pass value
    | Error(error : 'Fail) -> Fail error


[<AutoOpen>]
module Patterns =
  let inline (|FailAs|_|)
    (report : Report<'Pass, IFault>)
    : 'Fail option when 'Fail :> IFault
    =
    match report with
    | Fail(:? 'Fail as fault) -> Some fault
    | _ -> None

  let inline (|DemotedAs|_|) (fault : IFault) : 'X option when 'X :> exn =
    match fault with
    | :? Demotion<'X> as x -> Some x.Source
    | :? Demotion<exn> as x when (x.Source :? 'X) -> Some(downcast x.Source)
    | _ -> None

  let inline (|Demoted|_|) (report : Report<'Pass, IFault>) =
    match report with
    | Fail(DemotedAs(demoted : 'X)) -> Some demoted
    | _ -> None


[<RequireQualifiedAccess>]
module Report =
  let ofFault (fault : IFault) : Report<'Pass, IFault> = Fail fault

  let ofExn (fault : 'X) : Report<'Pass, Demotion<'X>> =
    fault |> Demotion<_> |> Fail

  let inline bind
    (pass : 'Pass -> Report<'T, 'Fail>)
    (report : Report<'Pass, 'Fail>)
    : Report<'T, 'Fail>
    =
    match report with
    | Pass value -> pass value
    | Fail error -> Fail error

  let inline bindFail
    (fail : 'Fail -> Report<'Pass, 'T>)
    (report : Report<'Pass, 'Fail>)
    : Report<'Pass, 'T>
    =
    match report with
    | Pass value -> Pass value
    | Fail error -> fail error

  let map
    (pass : 'Pass -> 'T)
    (report : Report<'Pass, 'Fail>)
    : Report<'T, 'Fail>
    =
    report |> bind (pass >> Pass)

  let mapFail
    (fail : 'Fail -> 'T)
    (report : Report<'Pass, 'Fail>)
    : Report<'Pass, 'T>
    =
    report |> bindFail (fail >> Fail)

  let inline generalize
    (report : Report<'Pass, 'Fail>)
    : Report<'Pass, IFault>
    =
    report |> mapFail (fun fault -> upcast fault)

  let iter (pass : 'Pass -> unit) (report : Report<'Pass, 'Fail>) : unit =
    match report with
    | Pass value -> pass value
    | Fail _ -> ( (* noop *) )

  let iterFail (fail : 'Fail -> unit) (report : Report<'Pass, 'Fail>) : unit =
    match report with
    | Pass _ -> ( (* noop *) )
    | Fail fault -> fail fault

  let isPass (report : Report<'Pass, 'Fail>) : bool =
    match report with
    | Pass _ -> true
    | Fail _ -> false

  let isFail (report : Report<'Pass, 'Fail>) : bool =
    match report with
    | Pass _ -> false
    | Fail _ -> true

  let toResult (report : Report<'Pass, 'Fail>) : Result<'Pass, 'Fail> =
    Report.op_Implicit report

  let ofResult (result : Result<'Pass, 'Fail>) : Report<'Pass, 'Fail> =
    Report.op_Implicit result

  let toOption (report : Report<'Pass, 'Fail>) : 'Pass option =
    match report with
    | Pass value -> Some value
    | Fail _ -> None

  let ofOption
    (withFault : unit -> 'Fail)
    (option : 'Pass option)
    : Report<'Pass, 'Fail>
    =
    match option with
    | Some value -> Pass value
    | None -> Fail(withFault ())

  let toChoice (report : Report<'Pass, 'Fail>) : Choice<'Pass, 'Fail> =
    match report with
    | Pass value -> Choice1Of2 value
    | Fail fault -> Choice2Of2 fault

  let ofChoice (choice : Choice<'Pass, 'Fail>) : Report<'Pass, 'Fail> =
    match choice with
    | Choice1Of2 value -> Pass value
    | Choice2Of2 fault -> Fail fault

  let defaultValue (value : 'Pass) (report : Report<'Pass, 'Fail>) : 'Pass =
    match report with
    | Pass value' -> value'
    | Fail _ -> value

  let defaultWith
    (withFault : 'Fail -> 'Pass)
    (report : Report<'Pass, 'Fail>)
    : 'Pass
    =
    match report with
    | Pass value -> value
    | Fail fault -> withFault fault


[<Sealed>]
type CompoundFault(faults : IFault seq, ?message : string, ?cause : IFault) =
  do (* .ctor *)
    if faults |> withNull |> isNull then
      nullArg (nameof faults)
    elif Seq.length faults < 1 then
      invalidArg (nameof faults) "Must provide at least one fault."

  member val Faults : IFault seq = faults |> Seq.toArray |> Seq.readonly

  member val Message : string = defaultArg message "One or more errors occurred"

  interface IFault with
    member me.Message = me.Message
    member _.Cause = cause

  interface IEnumerable<IFault> with
    member me.GetEnumerator() = me.Faults.GetEnumerator()
    member me.GetEnumerator() = (me.Faults :> IEnumerable).GetEnumerator()


[<RequireQualifiedAccess>]
module Array =
  let divide items =
    match items with
    | Null -> nullArg (nameof items)
    | NonNull [||] -> (Array.empty, Array.empty)
    | NonNull items ->
      let passing, failing = ResizeArray<'Pass>(), ResizeArray<_>()

      for item in items do
        match item with
        | Pass value -> passing.Add(value)
        | Fail fault -> failing.Add(fault)

      (passing.ToArray(), failing.ToArray())

  let accumulate project items =
    match items with
    | Null -> nullArg (nameof items)
    | NonNull [||] -> Pass Array.empty
    | NonNull items ->
      let passing, failing = ResizeArray<'Pass>(), ResizeArray<_>()

      for item in items do
        match project item with
        | Pass value -> passing.Add(value)
        | Fail fault -> failing.Add(fault)

      if 0 < failing.Count then
        Fail(CompoundFault failing)
      else
        Pass(passing.ToArray())

  let traverse project items =
    match items with
    | Null -> nullArg (nameof items)
    | NonNull [||] -> Pass Array.empty
    | NonNull items ->
      let buffer = ResizeArray<'Pass>()
      let mutable halted = ValueOption.None
      let enum = (items :> 'T seq).GetEnumerator()

      while ValueOption.isNone halted && enum.MoveNext() do
        match project enum.Current with
        | Pass value -> buffer.Add(value)
        | Fail error -> halted <- ValueSome error

      match halted with
      | ValueSome error -> Fail error
      | ValueNone -> Pass(buffer.ToArray())

  let sequence reports = reports |> traverse id


[<RequireQualifiedAccess>]
module List =
  let divide (items : Report<'Pass, 'Fail> list) : 'Pass list * 'Fail list =
    let passing, failing = items |> List.toArray |> Array.divide
    (List.ofArray passing, List.ofArray failing)

  let accumulate
    (project : 'T -> Report<'Pass, IFault>)
    (items : 'T list)
    : Report<'Pass list, CompoundFault>
    =
    items |> List.toArray |> Array.accumulate project |> Report.map Array.toList

  let traverse
    (project : 'T -> Report<'Pass, 'Fail>)
    (items : 'T list)
    : Report<'Pass list, 'Fail>
    =
    items |> List.toArray |> Array.traverse project |> Report.map Array.toList

  let sequence
    (reports : Report<'Pass, 'Fail> list)
    : Report<'Pass list, 'Fail>
    =
    reports |> traverse id


[<RequireQualifiedAccess>]
module Seq =
  let divide (items : Report<'Pass, 'Fail> seq) : 'Pass seq * 'Fail seq =
    let passing, failing = items |> Seq.toArray |> Array.divide
    (Seq.ofArray passing, Seq.ofArray failing)

  let accumulate
    (project : 'T -> Report<'Pass, IFault>)
    (items : 'T seq)
    : Report<'Pass seq, CompoundFault>
    =
    items |> Seq.toArray |> Array.accumulate project |> Report.map Array.toSeq

  let traverse
    (project : 'T -> Report<'Pass, 'Fail>)
    (items : 'T seq)
    : Report<'Pass seq, 'Fail>
    =
    items |> Seq.toArray |> Array.traverse project |> Report.map Array.toSeq

  let sequence (reports : Report<'Pass, 'Fail> seq) : Report<'Pass seq, 'Fail> =
    reports |> traverse id


[<Sealed>]
type ReportBuilder() =
  member inline _.Zero() = Pass()
  member inline _.Return(value) = Pass value

  member inline _.ReturnFrom(report : Report<_, #IFault>) : Report<_, #IFault> =
    report

  member inline _.Bind(report, [<InlineIfLambda>] binder) =
    Report.bind binder report

  member inline _.BindReturn(report, [<InlineIfLambda>] binder) =
    Report.map binder report

  member inline _.MergeSources(one, two) =
    match one, two with
    | Pass one, Pass two -> Pass(one, two)
    | Fail one, Fail two -> Fail(CompoundFault [ one; two ])
    | Fail one, Pass _
    | Pass _, Fail one -> Fail one

  member inline _.Delay(generator : unit -> Report<_, _>) = generator

  member inline _.Run(generate : unit -> Report<_, _>) = generate ()

  member inline _.Combine(report, [<InlineIfLambda>] delayed) =
    Report.bind (fun () -> delayed ()) report

  member inline _.While([<InlineIfLambda>] condition, [<InlineIfLambda>] body) =
    let mutable go = true
    let mutable result = Pass()
    while go && condition () do
      match body () with
      | Pass _ -> ()
      | Fail _ as f ->
        go <- false
        result <- f
    result

  member inline me.TryWith
    ([<InlineIfLambda>] action, [<InlineIfLambda>] handler)
    =
    try
      me.Run(action)
    with e ->
      handler e

  member inline me.TryFinally
    ([<InlineIfLambda>] action, [<InlineIfLambda>] compensation)
    =
    try
      me.Run(action)
    finally
      compensation ()

  member inline me.Using(resource : #IDisposable, [<InlineIfLambda>] action) =
    me.TryFinally(
      (fun () -> action resource),
      (fun () ->
        if not (obj.ReferenceEquals(resource, null)) then
          resource.Dispose()
      )
    )

  member inline me.For(sequence : #seq<_>, [<InlineIfLambda>] action) =
    me.Using(
      sequence.GetEnumerator(),
      fun e ->
        me.While((fun () -> e.MoveNext()), me.Delay(fun () -> action e.Current))
    )

  member inline _.Source(report : Report<_, _>) = report
  member inline _.Source(items : #seq<_>) : #seq<_> = items


[<AutoOpen>]
module ReportBuilderExtensions =
  let report = ReportBuilder()

  type ReportBuilder with
    member inline _.Source
      (result : Result<'Pass, 'Fail>)
      : Report<'Pass, 'Fail>
      =
      Report.ofResult result

    member inline _.Source
      (choice : Choice<'Pass, 'Fail>)
      : Report<'Pass, 'Fail>
      =
      Report.ofChoice choice

    member inline _.Source(fault : IFault) : Report<'Pass, IFault> =
      Report.ofFault fault

    member inline _.Source(fault : exn) : Report<'Pass, Demotion<exn>> =
      Report.ofExn fault
