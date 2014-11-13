module type Var = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module type Params = sig

  (** Size of the node construction cache *)
  val cons_cache_size : int

  (** Size of the negation's cache *)
  val neg_cache_size : int

  (** Size of the binary operations' cache *)
  val binop_cache_size : int

  (** Size of [vars]'s cache *)
  val vars_cache_size : int

  (** Size of [all_sat]'s cache *)
  val all_sat_cache_size : int

  (** Size of [restrict]'s cache *)
  val restrict_cache_size : int

  (** Should cache be thrown away between two calls to [neg] *)
  val neg_cache_throwaway : bool

  (** Should cache be thrown away between two calls to a binary operator *)
  val binop_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [vars] *)
  val vars_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [restrict v x] for some [v]-[x] *)
  val restrict_cache_throwaway : bool

  (** Should cache be thrown away between two calls to [all_sat] *)
  val all_sat_cache_throwaway : bool

end

module Make (Var : Var) (P : Params) : sig

  type t = private Zero | One | Node of int * Var.t * t * t

  (** Zero, false *)
  val zero : t

  (** One, true *)
  val one : t

  (** Convert a Boolean to a BDD *)
  val of_bool : bool -> t

  (** Decision variable *)
  val var : Var.t -> t

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

  module Ops : sig
    val (~?) : t -> t
    val (&?) : t -> t -> t
    val (|?) : t -> t -> t
    val (^?) : t -> t -> t
    val (=?) : t -> t -> t
    val (=>?) : t -> t -> t
  end

  (** Get the used variables *)
  val vars : t -> Var.t list

  (** Compute Shannon's co-factor for the given variable-value,
      that is, replace a variable by some constant *)
  val restrict : Var.t -> bool -> t -> t

  (** A valid assignment. Raise [Not_found] if none. *)
  val any_sat : t -> (Var.t * bool) list

  (** A random valid assignment. Raise [Not_found] if none. *)
  val random_stat : t -> (Var.t * bool) list

  (** All valid assignment, i.e. DNF *)
  val all_sat : t -> (Var.t * bool) list list

  (** Iterate on all valid assignement *)
  val iter_sat : ((Var.t * bool) list -> unit) -> t -> unit

  (** Find exists *)
  val exists_sat : ((Var.t * bool) list -> bool) -> t -> bool

  (** Is satisfiable *)
  val is_sat : t -> bool

  (** Is tautology *)
  val is_tauto : t -> bool
end
