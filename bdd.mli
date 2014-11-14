module type Var = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Raw : sig
  type 'a t = private Zero | One | Node of int * 'a * 'a t * 'a t

  val zero : 'a t
  val one : 'a t
  val make : 'a -> 'a t -> 'a t -> 'a t
  val uid : 'a t -> int
end

type 'a t = 'a Raw.t

module type S = sig

  type var
  type t = private var Raw.t

  val of_raw : var Raw.t -> t
  val to_raw : t -> var Raw.t

  (** Zero, false *)
  val zero : t

  (** One, true *)
  val one : t

  (** Convert a Boolean to a BDD *)
  val of_bool : bool -> t

  (** Decision variable *)
  val var : var -> t

  (** Equality function: alias for [(==)] *)
  val equal : t -> t -> bool

  (** Hashing function *)
  val hash : t -> int

  (** Negate, NOT *)
  val neg : t -> t

  (** Conjunction, AND *)
  val conj : t -> t -> t

  (** Negated conjunction, NAND *)
  val nand : t -> t -> t

  (** Disjunction, OR *)
  val disj : t -> t -> t

  (** Negated disjunction, NOR *)
  val nor : t -> t -> t

  (** Exclusive disjunction, XOR *)
  val xor : t -> t -> t

  (** Logical implication *)
  val implies : t -> t -> t

  (** Logical equality, NXOR *)
  val iff : t -> t -> t

  (** Generic Boolean binary operation *)
  val apply : (bool -> bool -> bool) -> t -> t -> t

  (** If-then-else *)
  val ite : t -> t -> t -> t

  (** Get the used variables *)
  val vars : t -> var list

  (** Compute Shannon's co-factor for the given variable-value,
      that is, replace a variable by some constant *)
  val restrict : var -> bool -> t -> t

  (** A valid assignment. Raise [Not_found] if none. *)
  val any_sat : t -> (var * bool) list

  (** A random valid assignment. Raise [Not_found] if none. *)
  val random_sat : t -> (var * bool) list

  (** All valid assignment, i.e. DNF *)
  val all_sat : t -> (var * bool) list list

  (** Iterate on all valid assignement *)
  val iter_sat : ((var * bool) list -> unit) -> t -> unit

  (** Find exists *)
  val exists_sat : ((var * bool) list -> bool) -> t -> bool

  (** Is satisfiable *)
  val is_sat : t -> bool

  (** Is tautology *)
  val is_tauto : t -> bool
end

module type Params = sig

  (** Size of the node construction cache *)
  val cons_cache_size : int

  (** Size of the negation's cache *)
  val neg_cache_size : int

  (** Size of the binary operations' cache *)
  val binop_cache_size : int

  (** Size of [ite]'s cache *)
  val ite_cache_size : int

  (** Size of [vars]'s cache *)
  val vars_cache_size : int

  (** Size of [restrict]'s cache *)
  val restrict_cache_size : int

  (** Size of [all_sat]'s cache *)
  val all_sat_cache_size : int

  (** Should cache be thrown away between two calls to [neg] *)
  val neg_cache_throwaway : bool

  (** Should cache be thrown away between two calls to a binary operator *)
  val binop_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [ite] *)
  val ite_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [vars] *)
  val vars_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [restrict v x] for some [v]-[x] *)
  val restrict_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [all_sat] *)
  val all_sat_cache_throwaway : bool

end

module DefaultParams : Params

module Make (P : Params) (V : Var) : S

module Ops (S : S): sig
  open S

  val (~?) : t -> t
  val (&?) : t -> t -> t
  val (|?) : t -> t -> t
  val (^?) : t -> t -> t
  val (=?) : t -> t -> t
  val (=>?) : t -> t -> t
end
