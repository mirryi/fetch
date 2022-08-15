module type S = sig
  type 'a t
  type state

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val sequence : 'a t list -> 'a list t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end

  val get : state t
  val put : state -> unit t
  val update : (state -> state) -> unit t
  val update' : (state -> 'a * state) -> 'a t
  val run_state : 'a t -> state -> 'a
end

module type STATE = sig
  type t
end

module Make (ST : STATE) : S with type state = ST.t = struct
  type state = ST.t
  type 'a t = state -> state * 'a

  let return x s = (s, x)

  let bind xf f s =
    let s', x = xf s in
    f x s'

  let map x f = bind x (fun a -> return (f a))

  let sequence ms =
    let rec sequence' ms acc =
      match ms with
      | [] -> acc
      | m :: ms ->
          bind m (fun x -> sequence' ms (map acc (fun acc -> x :: acc)))
    in

    map (sequence' ms ([] |> return)) List.rev

  let get s = (s, s)
  let put x _ = (x, ())
  let update f = bind get (fun s -> put (f s))

  let update' f =
    bind get (fun s ->
        let x, s = f s in
        bind (put s) (fun _ -> return x))

  let run_state m s = m s |> snd

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( >>= ) = bind
    let ( >>| ) = map
  end
end
