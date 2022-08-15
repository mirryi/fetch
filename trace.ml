module Make (Q : Query.T) = struct
  type 'f query = 'f Q.t
  type kind = Pure | Io

  module Dependency = struct
    type 'g t = D : 'g Q.t -> 'g t

    let compare : type g h. g t -> h t -> (g, h) Dmap.cmp =
     fun (D q) (D q') ->
      match Q.compare q q' with Lt -> Lt | Eq -> Eq | Gt -> Gt
  end

  module DependencySet = Dmap.Make (Dependency)

  module DependencyEdge = struct
    type 'f t = E : 'f Q.t -> DependencySet.t t

    let compare : type f g. f t -> g t -> (f, g) Dmap.cmp =
     fun (E q) (E q') ->
      match Q.compare q q' with Lt -> Lt | Eq -> Eq | Gt -> Gt
  end

  module DependencyMap = Dmap.Make (DependencyEdge)

  module Value = struct
    type 'v t = V : 'f Q.t -> ('f * kind) t

    let compare : type v v'. v t -> v' t -> (v, v') Dmap.cmp =
     fun (V q) (V q') ->
      match Q.compare q q' with Lt -> Lt | Eq -> Eq | Gt -> Gt
  end

  module ValueMap = Dmap.Make (Value)

  type t = { values : ValueMap.t; dependencies : DependencyMap.t }

  let record_value q value kind ({ values; _ } as trace) =
    let values = ValueMap.add (V q) (value, kind) values in
    { trace with values }

  let lookup_value q { values; _ } = ValueMap.find_opt (V q) values

  let record_dependency q q' ({ dependencies; _ } as trace) =
    let set =
      match DependencyMap.find_opt (E q) dependencies with
      | Some set -> DependencySet.add (D q') (Q.dummy q') set
      | None -> DependencySet.singleton (D q') (Q.dummy q')
    in
    let dependencies = DependencyMap.add (E q) set dependencies in
    { trace with dependencies }

  let lookup_dependencies q { dependencies; _ } =
    DependencyMap.find_opt (E q) dependencies
end
