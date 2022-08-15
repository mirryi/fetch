module type T = sig
  type 'f t

  val dummy : 'f t -> 'f
  val compare : 'f t -> 'g t -> ('f, 'g) Dmap.cmp
end
