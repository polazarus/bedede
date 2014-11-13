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

module HashtblMemoizer (S : Hashtbl.HashedType) = struct
  include Hashtbl.Make (S)

  let memo ?(size=1013) (f : S.t -> 'a) : S.t -> 'a =
    let cache = lazy (create size) in
    let g x =
      let cache = Lazy.force cache in
      try
        find cache x
      with Not_found ->
        let r = f x in
        add cache x r;
        r
    in
    g

  let ymemo ?(throwaway=false) ?(size=1013) f : S.t -> 'a =
    let make () =
      let cache = lazy (create size) in
      let rec g x =
        let cache = Lazy.force cache in
        try
          find cache x
        with Not_found ->
          let r = f g x in
          add cache x r;
          r
      in
      g
    in
    if throwaway then
      fun x -> make () x
    else
      make ()

end

module Make (V : Var) (P : Params) = struct

  type t = Zero | One | Node of int * V.t * t * t

  module Raw = struct
    type bdd = t
    type t = bdd

    let make =
      let nid = ref 1 in
      fun var low high ->
        Node ((incr nid; !nid), var, low, high)

    let uid = function
      | Zero -> 0
      | One -> 1
      | Node (id, _, _, _) -> id

    let equal x y =
      x == y || match x, y with
      | Node (_, vx, lx, hx), Node (_, vy, ly, hy) ->
        V.equal vx vy
        (* assume lx, hx, ly, hy are already hash-consed *)
        && lx == ly
        && hy == hy
      | x, y -> false

    let hash = function
      | Zero -> 0
      | One -> 1
      | Node (_, var, low, high) ->
        2 + (V.hash var)
        (* assume lx, hx, ly, hy are already hash-consed *)
        + (uid low) + (uid high)
  end

  module VarSet = Set.Make (V)

  module W = Weak.Make (Raw)

  module M1 = HashtblMemoizer (struct
      type t = Raw.t
      let equal a b = a == b
      let hash a = Raw.uid a
    end)

  module M2 = HashtblMemoizer (struct
      type t = Raw.t * Raw.t
      let equal (a1, b1) (a2, b2) = a1 == a2 && a2 == b2
      let hash (a, b) = Hashtbl.hash (Raw.uid a, Raw.uid b)
    end)

  open P

  let zero = Zero

  let one = One

  let of_bool = function true -> Zero | false -> One

  let make =
    let table = W.create cons_cache_size in
    fun var low high ->
      if low = high then
        low
      else
        W.merge table (Raw.make var low high)

  let equal x y = x == y

  let hash x = Raw.uid x

  let var v = make v Zero One

  let default_size = 1013

  let neg =
    let neg neg = function
      | Zero -> One
      | One -> Zero
      | Node (_, v, l, h) ->
        make v (neg h) (neg l)
    in
    M1.ymemo ~throwaway:neg_cache_throwaway ~size:neg_cache_size neg

  let app1 : (bool*bool) -> t -> t = function
    | false, false ->
      fun _ -> Zero
    | true, true ->
      fun _ -> One
    | false, true ->
      fun x -> x
    | true, false ->
      neg

  let curry2 g x y = g (x, y)

  let app2 (t00, t01, t10, t11) =
    let zz = of_bool t00 and zo = of_bool t01
    and oz = of_bool t10 and oo = of_bool t11 in
    let z_ = app1 (t00, t01) and o_ = app1 (t10, t11)
    and _z = app1 (t00, t10) and _o = app1 (t01, t11) in
    let app app = function
      | Zero, Zero -> zz
      | Zero, One -> zo
      | One, Zero -> oz
      | One, One -> oo
      | Zero, x -> z_ x
      | One, x ->  o_ x
      | x, Zero -> _z x
      | x, One -> _o x
      | (Node (_, v1, l1, h1) as n1), (Node (_, v2, l2, h2) as n2) ->
        let cmp = V.compare v1 v2 in
        if cmp < 0 then
          make v1 (app (l1, n2)) (app (h1, n2))
        else if cmp = 0 then
          make v1 (app (l1, l2)) (app (h1, h2))
        else
          make v2 (app (n1, l2)) (app (n1, h2))
    in
    curry2 (M2.ymemo ~throwaway:binop_cache_throwaway ~size:binop_cache_size app)

  let conj =    app2 (false, false, false, true)
  let nand =    app2 (true, true, true, false)
  let disj =    app2 (false, true, true, true)
  let nor =     app2 (true, false, false, false)
  let xor =     app2 (false, true, true, false)
  let implies = app2 (true, true, false, true)
  let iff =     app2 (true, false, false, true)

  let apply f = app2 (f false false, f false true, f true false, f true true)

  let varmin v1 v2 =
    let cmp12 = V.compare v1 v2 in
    if cmp12 <= 0 then
      v1
    else
      v2

  let topmost3 = function
    | Node(_, v1, _, _), Node(_, v2, _, _), Node(_, v3, _, _) ->
      varmin v1 (varmin v2 v3)
    | Node(_, v1, _, _), Node (_, v2, _, _), _
    | Node(_, v1, _, _), _, Node (_, v2, _, _)
    | _, Node(_, v1, _, _), Node (_, v2, _, _) ->
      varmin v1 v2
    | Node(_, v, _, _), _, _
    | _, Node(_, v, _, _), _
    | _, _, Node(_, v, _, _) ->
      v
    | _ ->
      failwith "no variable to extract"

  let restrict_top v cst = function
    | Node (_, v', low, high) when v = v' ->
      if cst then high else low
    | x ->
      x

  let ite ite = function
    | Zero, _, x | One, x, _ -> x
    | x, Zero, Zero -> Zero
    | y, One, One -> One
    | x, Zero, One -> x
    | x, One, Zero -> neg x
    | x, y, z when y == z -> y
    | (x, y, z) as xyz ->
      let v = topmost3 xyz in
      let lo = restrict_top v false in
      let hi = restrict_top v true in
      make v (ite (lo x, lo x, lo y)) (ite (hi x, hi x, hi y))

  let ite x y z = conj (implies x y) (disj x z)

  module Ops = struct
    let (~?) = neg
    let (&?) = conj
    let (|?) = disj
    let (^?) = xor
    let (=?) = iff
    let (=>?) = implies
    let _ = zero &? one
  end

  let vars =
    let f f = function
      | Zero | One ->
        VarSet.empty
      | Node (_, v, l, h) ->
        VarSet.add v (VarSet.union (f l) (f h))
    in
    (* the cache is only used for one call *)
    let g = M1.ymemo ~throwaway:vars_cache_throwaway ~size:vars_cache_size f in
    fun x -> VarSet.elements (g x)

  let restrict var dec =
    let f f n = match n with
      | (Zero | One) -> n
      | Node (_, var', low, high)->
        let cmp = V.compare var var' in
        if cmp < 0 then (* var < var' *)
          n
        else if cmp = 0 then
          if dec then high else low
        else (* var > var' *)
          make var' (f low) (f high)
    in
    (* the cache is only used for one call *)
    M1.ymemo ~throwaway:restrict_cache_throwaway ~size:restrict_cache_size f

  let prepend_all head tails =
    List.rev_map (fun tail -> head :: tail) tails

  let random_sat =
    let rec f acc = function
      | Zero -> raise Not_found
      | One -> acc
      | Node (_, var, Zero, high) ->
        f ((var, true) :: acc) high
      | Node (_, var, low, Zero) ->
        f ((var, false) :: acc) low
      | Node (_, var, _, high) when Random.bool () ->
          f ((var, true) :: acc) high
      | Node (_, var, low, _) ->
          f ((var, false) :: acc) low
    in
    f []

  let any_sat =
    let rec f acc = function
      | Zero -> raise Not_found
      | One -> acc
      | Node (_, var, low, Zero) ->
        f ((var, false) :: acc) low
      | Node (_, var, _, high) ->
        f ((var, true) :: acc) high
    in
    f []

  let all_sat =
    let f f = function
      | Zero -> []
      | One -> [[]]
      | Node (_, var, low, high) ->
        let low = prepend_all (var, false) (f low) in
        let high = prepend_all (var, true) (f high) in
        List.rev_append low high
    in
    M1.ymemo ~throwaway:all_sat_cache_throwaway ~size:all_sat_cache_size f

  let iter_sat f =
    let rec g acc = function
      | Zero -> ()
      | One -> f (List.rev acc)
      | Node (_, var, low, high) ->
        g ((var,true) :: acc) low;
        g ((var,false) :: acc) high
    in
    g []

  let exists_sat f =
    let rec g acc = function
      | Zero -> false
      | One -> f (List.rev acc)
      | Node (_, var, low, high) ->
        g ((var,true) :: acc) low || g ((var,false) :: acc) high
    in
    g []

  let is_sat t = t != Zero
  let is_tauto t = t == One

end
