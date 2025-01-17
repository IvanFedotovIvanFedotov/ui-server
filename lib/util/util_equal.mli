module List : sig
  type 'a t = 'a list
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Option : sig
  type 'a t = 'a option
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Pair : sig
  type ('a, 'b) t = 'a * 'b
  val equal :
    ('a -> 'a -> bool)
    -> ('b -> 'b -> bool)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> bool
end

module Result : sig
  type ('a, 'e) t = ('a, 'e) result
  val equal :
    ok:('a -> 'a -> bool)
    -> error:('e -> 'e -> bool)
    -> ('a, 'e) t
    -> ('a, 'e) t
    -> bool
end

module Int : sig
  type t = int
  val equal : t -> t -> bool
end
