module Make (Q : Query.T) = struct
  type 'f query = 'f Q.t

  module Trace = Trace.Make (Q)

  module State = struct
    type t = { trace : Trace.t }
  end

  module Monad = Monad.Make (State)
  open Monad
  open Monad.Syntax

  module H = struct
    let store_memo q f kind =
      update (fun { trace } -> { trace = Trace.record_value q f kind trace })

    let retrieve_memo q = get >>| fun { trace } -> Trace.lookup_value q trace

    let add_dep q q' =
      update (fun { trace } -> { trace = Trace.record_dependency q q' trace })

    let retrieve_deps q =
      get >>| fun { trace } ->
      Trace.lookup_dependencies q trace
      |> Option.value ~default:Trace.DependencySet.empty
  end

  type 'f data = Computed of 'f | Memoized of 'f
  type 'f fetch = 'f query -> 'f data Monad.t
  type 'f pure = 'f -> 'f data Monad.t
  type 'f io = 'f -> 'f data Monad.t
  type 'f rules = 'f fetch * 'f pure * 'f io -> 'f query -> 'f data Monad.t

  let rec run rules q =
    let fetch q = run rules q in
    let pure data = return (Computed data) in
    let io data = return (Computed data) in

    rules (fetch, pure, io) q

  let rec run_with_memo : 'f. 'f rules -> 'f query -> 'f data Monad.t =
   fun rules q ->
    let fetch : 'f fetch =
     fun q' ->
      (* Add dependency relation between the two queries. *)
      let* () = H.add_dep q q' in

      (* Fetch dependency. *)
      run_with_memo rules q'
    in

    (* Return a pure result that was just computed. *)
    let pure : 'f pure =
     fun f ->
      (* Memoize the result. *)
      let+ () = H.store_memo q f Pure in

      Computed f
    in

    (* Return an impure result that was just (possibly re-) computed. *)
    let io : 'f io =
     fun f ->
      let* fmemo = H.retrieve_memo q in
      match fmemo with
      (* If new result is the same as the memoized one, return as memoized. *)
      | Some (fmemo, _) when fmemo == f -> return (Memoized f)
      (* Otherwise, memoize and return as computed. *)
      | Some _ | None ->
          let+ () = H.store_memo q f Io in
          Computed f
    in

    let rules_ q = rules (fetch, pure, io) q in
    H.retrieve_memo q >>= function
    | Some (_, Io) | None -> rules_ q
    | Some (f, Pure) ->
        let* deps = H.retrieve_deps q in
        let* reuse =
          Trace.DependencySet.fold
            { fun1 = verify_dependencies rules }
            deps (return true)
        in
        if reuse then return (Memoized f) else rules_ q

  and verify_dependencies :
        'f.
        'f rules -> 'f Trace.Dependency.t -> 'f -> bool Monad.t -> bool Monad.t
      =
   fun (Trace.Dependency.D q') _ acc ->
    acc >>= function
    | false -> return false
    | true -> (
        run_with_memo rules q' >>| function
        | Memoized _ -> true
        | Computed _ -> false)
end
