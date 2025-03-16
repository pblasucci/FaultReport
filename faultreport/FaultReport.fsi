namespace MulberryLabs.FaultReport

/// <summary>
/// Contains general-purpose failure-related utilities.
/// </summary>
/// <remarks>This module is automatically opened.</remarks>
[<AutoOpen>]
module Library =
  /// Stop the world -- I want to get off!
  val inline ``panic!`` : message : string -> 'T


/// Minimal contract provided by any failure.
[<Interface>]
type IFault =
  /// An unstructured human-readable summary of the current failure.
  abstract Message : string

  /// An optional reference to the failure which triggered the current failure
  /// (n.b. most failure do NOT have a cause).
  abstract Cause : IFault option


/// A CLR exception which has been trapped and reduced to a failure.
type Demotion<'X when 'X :> exn> =
  interface IFault

  /// <summary>
  /// Creates a new <c>Demotion&lt;'T&gt;</c> from the given subtype of <see cref="T:System.Exception"/>
  /// and, optionally, an unstructured human-readable summary of the current failure.
  /// </summary>
  /// <param name="source">The exception being demoted.</param>
  /// <param name="message">An unstructured human-readable summary of the demotion.</param>
  new : source : 'X * ?message : string -> Demotion<'X>

  /// <summary>
  /// A subtype of <see cref="T:System.Exception"/> which has been trapped and reduced to a failure.
  /// </summary>
  member Source : 'X

  /// An unstructured human-readable summary of the demotion.
  member Message : string


/// <summary>
/// A type abbreviation which helps with the automatic derivation of <see cref="T:pblasucci.FaultReport.IFault"/> instances.
/// </summary>
type Faulty<'T when 'T : (member Message : string)> = 'T


/// <summary>
/// Contains utilities for working with <see cref="T:pblasucci.FaultReport.IFault"/> instances.
/// </summary>
[<RequireQualifiedAccess>]
module Fault =
  /// <summary>
  /// Creates a new <see cref="T:pblasucci.FaultReport.IFault"/> instance from any "fault-like" values,
  /// where "fault-like" means: "has a public property, named <c>Message</c>, of type <see cref="T:System.String"/>.
  /// </summary>
  val inline derive : faulty : ^T -> IFault when ^T : (member Message : string)

  /// Reduces a captured exception to a failure.
  val demote : source : 'X -> Demotion<'X> when 'X :> exn

  /// Elevates a failure to a throwable exception.
  val promote : toExn : (IFault -> 'X) -> fault : IFault -> 'X when 'X :> exn

  /// Elevates a failure to a throwable exception, and immediately raises said exception.
  val escalate : toExn : (IFault -> 'X) -> fault : IFault -> 'X when 'X :> exn


/// <summary>
/// Represents the outcome of an operation which maybe have either: passed, or failed.
/// An instance of this type is guaranteed to only ever be in one state or the other.
/// Additionally, either state may carry additional data (n.b. for failed outcomes, the
/// additional data must implement the <see cref="T:pblasucci.FaultReport.IFault"/> contract).
/// </summary>
type Report<'Pass, 'Fail when 'Fail :> IFault> =
  /// Represents the successful outcome of an operation (i.e. it passed).
  | Pass of value : 'Pass
  /// Represents the unsuccessful outcome of an operation (i.e. it failed).
  | Fail of fault : 'Fail

  static member op_Implicit :
    report : Report<'Pass, 'Fail> -> Result<'Pass, 'Fail>

  static member op_Implicit :
    result : Result<'Pass, 'Fail> -> Report<'Pass, 'Fail>


/// <summary>
/// Contains active patterns for working with <see cref="T:pblasucci.FaultReport.IReport`1"/> instances.
/// </summary>
/// <remarks>This module is automatically opened.</remarks>
[<AutoOpen>]
module Patterns =
  /// <summary>
  /// Matches an <see cref="T:pblasucci.FaultReport.Report`2"/> instance, where the
  /// failing case has been generalized to <see cref="T:pblasucci.FaultReport.IFault"/>,
  /// only succeeding when said report has failed AND the extra failure data
  /// expressly matches the given output.
  /// </summary>
  /// <param name="report">The report against which to match.</param>
  /// <remarks>
  /// The target failure must implement the <see cref="T:pblasucci.FaultReport.IFault"/> contract.
  /// Using this active pattern often requires an explicit type annotation.
  /// </remarks>
  /// <example>
  /// <c>(|FailAs|_|)</c> can be used to extract a specific failure from an <c>IReport</c>,
  /// which typically expresses failure as the base contract, <c>IFault</c>.
  /// <code lang="fsharp">
  /// match report with
  /// | Pass value -> ...
  /// | FailAs(fault : SpecificFailure) -> ...
  /// | Fail fault -> ...
  /// </code>
  /// </example>
  val inline (|FailAs|_|) :
    report : Report<'Pass, IFault> -> 'Fail option when 'Fail :> IFault

  /// <summary>
  /// Matches an <see cref="T:pblasucci.FaultReport.Report`2"/> instance, where the
  /// failing case has been generalized to <see cref="T:pblasucci.FaultReport.IFault"/>,
  /// only succeeding when said report has failed AND the extra failure data
  /// is expressly a demotion of the given exception subtype.
  /// </summary>
  /// <param name="report">The report against which to match.</param>
  /// <remarks>
  /// The target failure must subclass <see cref="T:System.Exception"/>.
  /// Using this active pattern typically requires an explicit type annotation.
  /// </remarks>
  /// <example>
  /// <c>(|Demoted|_|)</c> can be used to extract a specific exception from an <c>IReport</c>,
  /// which typically expresses failure as the base contract, <c>IFault</c>.
  /// <code lang="fsharp">
  /// match report with
  /// | Pass value -> ...
  /// | Demoted(fault : TimeoutException) -> ...
  /// | Fail fault -> ...
  /// </code>
  /// </example>
  val inline (|Demoted|_|) :
    report : Report<'Pass, IFault> -> 'X option when 'X :> exn

  /// <summary>
  /// Matches an <see cref="T:pblasucci.FaultReport.IFault"/> instance,
  /// only succeeding when said fault is expressly a demotion of the given exception subtype,
  /// or is a demotion whose source is castable to the given exception subtype.
  /// </summary>
  /// <param name="fault">The fault against which to match.</param>
  /// <remarks>
  /// The target failure must subclass <see cref="T:System.Exception"/>.
  /// Using this active pattern typically requires an explicit type annotation.
  /// </remarks>
  /// <example>
  /// <c>(|DemotionOf|_|)</c> can be used to extract a specific exception from an <c>IFault</c>,
  /// assuming its concretion is assuming its concretion is <see cref="T:pblasucci.FaultReport.Demotion`1"/>.
  /// <code lang="fsharp">
  /// match fault with
  /// | DemotionOf(fault : TimeoutException) -> ...
  /// | _ -> ...
  /// </code>
  /// </example>
  val inline (|DemotedAs|_|) : fault : IFault -> 'X option when 'X :> exn


/// <summary>
/// Contains utilities for working with <see cref="T:pblasucci.FaultReport.Report`2"/> instances.
/// </summary>
[<RequireQualifiedAccess>]
module Report =
  /// Executes the given function against the value contained in a passing Report;
  /// otherwise, return the original (failing Report).
  val inline bind :
    pass : ('Pass -> Report<'T, 'Fail>) ->
    report : Report<'Pass, 'Fail> ->
      Report<'T, 'Fail>
      when 'Fail :> IFault

  /// Executes the given function against the value contained in a failing Report;
  /// otherwise, return the original (passing Report).
  val inline bindFail :
    fail : ('Fail -> Report<'Pass, 'T>) ->
    report : Report<'Pass, 'Fail> ->
      Report<'Pass, 'T>
      when 'Fail :> IFault and 'T :> IFault

  /// Executes the given function against the value contained in a passing Report;
  /// otherwise, return the original (failing Report).
  val map :
    pass : ('Pass -> 'T) -> report : Report<'Pass, 'Fail> -> Report<'T, 'Fail>
      when 'Fail :> IFault

  /// Executes the given function against the value contained in a failing Report;
  /// otherwise, return the original (passing Report).
  val mapFail :
    fail : ('Fail -> 'T) -> report : Report<'Pass, 'Fail> -> Report<'Pass, 'T>
      when 'Fail :> IFault and 'T :> IFault

  /// <summary>
  /// Corrects the fault-type of a Report to be <see cref="T:pblasucci.FaultReport.IFault"/>.
  /// </summary>
  val inline generalize :
    report : Report<'Pass, #IFault> -> Report<'Pass, IFault>

  /// Executes the given action if, and only if, the given Report has passed.
  val iter : pass : ('Pass -> unit) -> report : Report<'Pass, #IFault> -> unit

  /// Executes the given action if, and only if, the given Report has failed.
  val iterFail :
    fail : ('Fail -> unit) -> report : Report<'Pass, 'Fail> -> unit
      when 'Fail :> IFault

  /// <summary>
  /// Returns <c>true</c> if the given Report has passed; otherwise, returns <c>false</c>.
  /// </summary>
  val isPass : report : Report<'Pass, #IFault> -> bool

  /// <summary>
  /// Returns <c>true</c> if the given Report has failed; otherwise, returns <c>false</c>.
  /// </summary>
  val isFail : report : Report<'Pass, #IFault> -> bool

  /// <summary>
  /// Converts the given report to an instance of <see cref="T:Microsoft.FSharp.Core.FSharpResult`2"/>.
  /// </summary>
  val toResult :
    report : Report<'Pass, 'Fail> -> Result<'Pass, 'Fail> when 'Fail :> IFault

  /// <summary>
  /// Builds a report instance from the given <see cref="T:Microsoft.FSharp.Core.FSharpResult`2"/>.
  /// </summary>
  val ofResult :
    result : Result<'Pass, 'Fail> -> Report<'Pass, 'Fail> when 'Fail :> IFault

  /// <summary>
  /// Converts the given report to an instance of <see cref="T:Microsoft.FSharp.Core.FSharpOption`1"/>.
  /// </summary>
  val toOption : report : Report<'Pass, #IFault> -> 'Pass option

  /// <summary>
  /// Builds a report instance from the given <see cref="T:Microsoft.FSharp.Core.FSharpOption`1"/>,
  /// using the given factory function to create fault data if the input option is <c>None</c>.
  /// </summary>
  val ofOption :
    withFault : (unit -> 'Fail) -> option : 'Pass option -> Report<'Pass, 'Fail>
      when 'Fail :> IFault

  /// <summary>
  /// Converts the given report to an instance of <see cref="T:Microsoft.FSharp.Core.FSharpChoice`2"/>.
  /// </summary>
  val toChoice :
    report : Report<'Pass, 'Fail> -> Choice<'Pass, 'Fail> when 'Fail :> IFault

  /// <summary>
  /// Builds a report instance from the given <see cref="T:Microsoft.FSharp.Core.FSharpChoice`2"/>.
  /// </summary>
  val ofChoice :
    choice : Choice<'Pass, 'Fail> -> Report<'Pass, 'Fail> when 'Fail :> IFault

  /// For a passing Report, returns the underlying value,
  /// but returns the given value for a failing Report.
  val defaultValue : value : 'Pass -> report : Report<'Pass, #IFault> -> 'Pass

  /// For a passing Report, returns the underlying value,
  /// but returns the result of the given function for a failing Report.
  val defaultWith :
    withFault : ('Fail -> 'Pass) -> report : Report<'Pass, 'Fail> -> 'Pass
      when 'Fail :> IFault

  /// <summary>
  /// Builds a failing report instance from the given <see cref="T:pblasucci.FaultReport.IFault"/>.
  /// </summary>
  val ofFault : fault : IFault -> Report<'Pass, IFault>

  /// <summary>
  /// Builds a failing report instance from the given <see cref="T:System.Exception"/>
  /// (or one of its many subclasses).
  /// </summary>
  val ofExn : fault : 'X -> Report<'Pass, Demotion<'X>> when 'X :> exn


/// <summary>
/// An <see cref="T:pblasucci.FaultReport.IFault"/>, which contains nested <c>IFault</c> instances
/// (note: this type is necessary for certain failure-aggregation scenarios).
/// </summary>
[<Sealed>]
type CompoundFault =
  interface IFault
  interface System.Collections.Generic.IEnumerable<IFault>

  new :
    faults : IFault seq * ?message : string * ?cause : IFault -> CompoundFault

  member Faults : IFault seq

  member Message : string


/// <summary>
/// Tools for working with <see cref="T:pblasucci.FaultReport.Report`2"/>
/// in conjunction with <see cref='T:Microsoft.FSharp.Core.array`1'/>.
/// </summary>
[<RequireQualifiedAccess>]
module Array =
  /// Splits a collection of reports into two collections:
  /// one containing only the passing value;
  /// and, one containing only the failing values.
  val divide :
    items : Report<'Pass, 'Fail> array | null -> 'Pass array * 'Fail array
      when 'Fail :> IFault

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the faults into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection will always be iterated (ie: all possible failures
  /// will be collected).
  /// </remarks>
  val accumulate :
    project : ('T -> Report<'Pass, IFault>) ->
    items : 'T array | null ->
      Report<'Pass array, CompoundFault>

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the first fault into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection may not be iterated (ie: processing will stop after
  /// the first failing value is detected).
  /// </remarks>
  val traverse :
    project : ('T -> Report<'Pass, 'Fail>) ->
    items : 'T array | null ->
      Report<'Pass array, 'Fail>
      when 'Fail :> IFault

  /// Turns a collection of report instances into a single report containing either:
  /// each of the passing values; or, the first fault which was encountered.
  val sequence :
    reports : Report<'Pass, 'Fail> array -> Report<'Pass array, 'Fail>
      when 'Fail :> IFault


/// <summary>
/// Tools for working with <see cref="T:pblasucci.FaultReport.Report`2"/>
/// in conjunction with <see cref='T:Microsoft.FSharp.Collections.list`1'/>.
/// </summary>
[<RequireQualifiedAccess>]
module List =
  /// Splits a collection of reports into two collections:
  /// one containing only the passing value;
  /// and, one containing only the failing values.
  val divide :
    items : Report<'Pass, 'Fail> list -> 'Pass list * 'Fail list
      when 'Fail :> IFault

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the faults into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection will always be iterated (ie: all possible failures
  /// will be collected).
  /// </remarks>
  val accumulate :
    project : ('T -> Report<'Pass, IFault>) ->
    items : 'T list ->
      Report<'Pass list, CompoundFault>

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the first fault into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection may not be iterated (ie: processing will stop after
  /// the first failing value is detected).
  /// </remarks>
  val traverse :
    project : ('T -> Report<'Pass, 'Fail>) ->
    items : 'T list ->
      Report<'Pass list, 'Fail>
      when 'Fail :> IFault

  /// Turns a collection of report instances into a single report containing either:
  /// each of the passing values; or, the first fault which was encountered.
  val sequence :
    reports : Report<'Pass, 'Fail> list -> Report<'Pass list, 'Fail>
      when 'Fail :> IFault


/// <summary>
/// Tools for working with <see cref="T:pblasucci.FaultReport.Report`2"/>
/// in conjunction with <see cref='T:Microsoft.FSharp.Collections.seq`1'/>.
/// </summary>
[<RequireQualifiedAccess>]
module Seq =
  /// Splits a collection of reports into two collections:
  /// one containing only the passing value;
  /// and, one containing only the failing values.
  val divide :
    items : Report<'Pass, 'Fail> seq -> 'Pass seq * 'Fail seq
      when 'Fail :> IFault

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the faults into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection will always be iterated (ie: all possible failures
  /// will be collected).
  /// </remarks>
  val accumulate :
    project : ('T -> Report<'Pass, IFault>) ->
    items : 'T seq ->
      Report<'Pass seq, CompoundFault>

  /// <summary>
  /// Applies the given function to each of the given items,
  /// accumulating either the passing data or the first fault into a single
  /// <see cref="T:pblasucci.FaultReport.Report`2"/> instance.
  /// </summary>
  /// <remarks>
  /// The <c>Report</c> instance returned from this function will only be in
  /// a passing state if all the input produced passing values (when the
  /// given input function is applied to each item). Further, all items of the
  /// given input collection may not be iterated (ie: processing will stop after
  /// the first failing value is detected).
  /// </remarks>
  val traverse :
    project : ('T -> Report<'Pass, 'Fail>) ->
    items : 'T seq ->
      Report<'Pass seq, 'Fail>
      when 'Fail :> IFault

  /// Turns a collection of report instances into a single report containing either:
  /// each of the passing values; or, the first fault which was encountered.
  val sequence :
    reports : Report<'Pass, 'Fail> seq -> Report<'Pass seq, 'Fail>
      when 'Fail :> IFault


[<Sealed>]
type ReportBuilder =
  member inline Zero : unit -> Report<unit, 'Fail>

  member inline Return : value : 'Pass -> Report<'Pass, 'Fail>

  member inline ReturnFrom<'Pass, 'Fail> :
    report : Report<'Pass, 'Fail> -> Report<'Pass, 'Fail> when 'Fail :> IFault

  member inline Bind :
    report : Report<'T1, 'Fail> *
    [<InlineIfLambda>] binder : ('T1 -> Report<'T2, 'Fail>) ->
      Report<'T2, 'Fail>

  member inline BindReturn :
    report : Report<'T1, 'Fail> * [<InlineIfLambda>] binder : ('T1 -> 'T2) ->
      Report<'T2, 'Fail>

  member inline MergeSources :
    one : Report<'T1, CompoundFault> * two : Report<'T2, CompoundFault> ->
      Report<('T1 * 'T2), CompoundFault>

  member inline Delay :
    generator : (unit -> Report<'Pass, 'Fail>) -> (unit -> Report<'Pass, 'Fail>)

  member inline Run :
    generate : (unit -> Report<'Pass, 'Fail>) -> Report<'Pass, 'Fail>

  member inline Combine :
    report : Report<unit, 'Fail> *
    [<InlineIfLambda>] delayed : (unit -> Report<'Pass, 'Fail>) ->
      Report<'Pass, 'Fail>

  member inline While :
    [<InlineIfLambda>] condition : (unit -> bool) *
    [<InlineIfLambda>] body : (unit -> Report<unit, 'Fail>) ->
      Report<unit, 'Fail>

  member inline TryWith :
    [<InlineIfLambda>] action : (unit -> Report<'Pass, 'Fail>) *
    [<InlineIfLambda>] handler : (exn -> Report<'Pass, 'Fail>) ->
      Report<'Pass, 'Fail>

  member inline TryFinally :
    [<InlineIfLambda>] action : (unit -> Report<'Pass, 'Fail>) *
    [<InlineIfLambda>] compensation : (unit -> unit) ->
      Report<'Pass, 'Fail>

  member inline Using :
    resource : 'T * [<InlineIfLambda>] action : ('T -> Report<'Pass, 'Fail>) ->
      Report<'Pass, 'Fail>
      when 'T :> System.IDisposable

  member inline For :
    sequence : #seq<'T> *
    [<InlineIfLambda>] action : ('T -> Report<unit, 'Fail>) ->
      Report<unit, 'Fail>

  member inline Source : report : Report<'Pass, 'Fail> -> Report<'Pass, 'Fail>

  member inline Source : items : 'T1 -> 'T1 when 'T1 :> #seq<'T2>


[<AutoOpen>]
module ReportBuilderExtensions =
  val report : ReportBuilder

  type ReportBuilder with
    member inline Source : result : Result<'Pass, 'Fail> -> Report<'Pass, 'Fail>

    member inline Source : choice : Choice<'Pass, 'Fail> -> Report<'Pass, 'Fail>

    member inline Source : fault : IFault -> Report<'Pass, IFault>

    member inline Source : fault : exn -> Report<'Pass, Demotion<exn>>
