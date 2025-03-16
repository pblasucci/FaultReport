FaultReport
===

FaultReport proof-of-concept demonstration of some concepts which may be useful
as part of a comprehensive strategy for better failure management for F#
developers.

### Main Concepts

+ `IFault`... a simple contract which spells out the basic requirements for
  modeling a failure.
+ `Report<'Pass, 'Fail>`... a type analogous to `Result<'T, 'TError>`, except
  `'Fail` is constrained to implement `IFault`.

For most code, you can work with `Report<_,_>` exactly the same as you would
with `Result<_, _>`.
When you want to have more flexibility, you switch to working with
`Report<_, IFault>`.

The main benefits this approach has (over `Result<_, _>`) are:

+ Useless "catch all" types (like `string` or `exn`) cannot be used to represent
  failures.
+ `Report<_, _>` instances with different failure representations (like from
  different libraries) can be combined without excessive re-mapping.
+ Between the `IFault.Cause` property, and the `CompoundFault` type, complex
  error models are possible (albeit uncommon).
+ Transiting between "concrete faults" and/or "general fault" and/or exceptions
  is straight-forward and requires minimal code.

However, it's worth noting, none of the important functionality of `Result<_,_>`
is sacrificed. Specifically:

+ Exhaustive matching is still supported (ie: for failures modeled as
  discriminated unions).
+ Even when working with the more general `IFault` type, pattern matching on
  specific failure types is still possible (via the `|FailAs|_|` active
  pattern).
+ Full support for monadic handling and/or applicative functor handling of
  collection data is supported (see the collection modules).
+ Full support for computation expressions is possible (unimplemented... the
  demo application just piggy-backs on [FsToolkit.ErrorHandling][1]).

**Note: Support for consumption from C# has not been factored into this work.**
If it had been, some of the API surface would be different (most notably, the
variants on `Report<_, _>` would be private and shadowed by a total multicase
active pattern).

### Related work

+ [FaultReport: an Theoretical Alternative to Result][2]

[1]: https://github.com/demystifyfp/FsToolkit.ErrorHandling
[2]: https://paul.blasuc.ci/posts/fault-report.html

