module FsToolkit.ErrorHandling

open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open FsToolkit.ErrorHandling
open MulberryLabs.FaultReport


type BackgroundTaskReportBuilder() =
  inherit BackgroundTaskResultBuilder()

  member inline _.Source(report : Async<Report<_, _>>) : Task<Result<_, _>> =
    report |> Async.map Report.toResult |> Async.StartAsTask

  member inline _.Source(report : Task<Report<_, _>>) : Task<Result<_, _>> =
    report |> Task.map Report.toResult

  member inline _.Source(report : Report<_, _>) : Task<Result<_, _>> =
    report |> Report.toResult |> Task.singleton

  member inline _.Run(code : TaskResultCode<'T, 'Error, 'T>) =
    (if __useResumableCode then
       __stateMachine<
         TaskResultStateMachineData<'T, 'Error>,
         TaskResult<'T, 'Error>
        >
         (MoveNextMethodImpl<_>(fun sm ->
           //-- RESUMABLE CODE START
           __resumeAt sm.ResumptionPoint

           try
             let __stack_code_fin = code.Invoke(&sm)

             if __stack_code_fin && not sm.Data.IsTaskCompleted then
               sm.Data.MethodBuilder.SetResult(sm.Data.Result)
           with exn ->
             sm.Data.MethodBuilder.SetException exn
         //-- RESUMABLE CODE END
         ))
         (SetStateMachineMethodImpl<_>(fun sm state ->
           sm.Data.MethodBuilder.SetStateMachine(state)
         ))
         (AfterCode<_, TaskResult<'T, 'Error>>(fun sm ->
           // backgroundTask { ... } escapes to a background thread where necessary
           // See spec of ConfigureAwait(false) at https://devblogs.microsoft.com/dotnet/configureawait-faq/
           if
             isNull SynchronizationContext.Current
             && obj.ReferenceEquals(
               TaskScheduler.Current,
               TaskScheduler.Default
             )
           then
             sm.Data.MethodBuilder <-
               AsyncTaskResultMethodBuilder<'T, 'Error>.Create()
             sm.Data.MethodBuilder.Start(&sm)
             sm.Data.MethodBuilder.Task
           else
             let sm = sm // copy contents of state machine so we can capture it

             Task.Run<Result<'T, 'Error>>(fun () ->
               let mutable sm = sm // host local mutable copy of contents of state machine on this thread pool thread

               sm.Data.MethodBuilder <-
                 AsyncTaskResultMethodBuilder<'T, 'Error>.Create()

               sm.Data.MethodBuilder.Start(&sm)
               sm.Data.MethodBuilder.Task
             )
         ))
     else
       BackgroundTaskResultBuilder.RunDynamic(code))
    |> Task.map Report.ofResult


let backgroundTaskReport = BackgroundTaskReportBuilder()
