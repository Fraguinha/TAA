module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type T = sig
  type dir = Left | Right
  type elt
  type t

  exception Invalid_rotation
  exception Invalid_change

  val empty : t
  val is_empty : t -> bool
  val value : t -> elt
  val min : t -> elt
  val max : t -> elt
  val left : t -> t
  val right : t -> t
  val size : t -> int
  val height : t -> int
  val rotate : dir -> t -> t
  val change : dir -> t -> t -> t
  val find : elt -> t -> t
  val insert : elt -> t -> t
  val remove : elt -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module BST (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = Node of { l : t; v : elt; r : t } | Leaf
  type dir = Left | Right

  exception Invalid_rotation
  exception Invalid_change

  let empty = Leaf
  let is_empty tree = match tree with Leaf -> true | Node _ -> false

  let value tree =
    match tree with Node { v; _ } -> v | Leaf -> raise Not_found

  let rec min tree =
    match tree with
    | Node { l = Leaf; v; _ } -> v
    | Node { l; _ } -> min l
    | Leaf -> raise Not_found

  let rec max tree =
    match tree with
    | Node { r = Leaf; v; _ } -> v
    | Node { r; _ } -> max r
    | Leaf -> raise Not_found

  let left tree = match tree with Node { l; _ } -> l | Leaf -> Leaf
  let right tree = match tree with Node { r; _ } -> r | Leaf -> Leaf

  let rec size tree =
    match tree with
    | Node { l; r; _ } ->
        let left = size l in
        let right = size r in
        left + 1 + right
    | Leaf -> 0

  let rec height tree =
    match tree with
    | Node { l; r; _ } ->
        let l = height l in
        let r = height r in
        if l > r then 1 + l else 1 + r
    | Leaf -> 0

  let rotate direction tree =
    match direction with
    | Left -> (
        match tree with
        | Node { l = a; v = y; r = Node { l = b; v = x; r = c } } ->
            Node { l = Node { l = a; v = y; r = b }; v = x; r = c }
        | Node _ | Leaf -> raise Invalid_rotation)
    | Right -> (
        match tree with
        | Node { l = Node { l = a; v = x; r = b }; v = y; r = c } ->
            Node { l = a; v = x; r = Node { l = b; v = y; r = c } }
        | Node _ | Leaf -> raise Invalid_rotation)

  let rec find key tree =
    match tree with
    | Node { v; _ } when key = v -> tree
    | Node { v; r; _ } when key > v -> find v r
    | Node { v; l; _ } when key < v -> find v l
    | Node _ -> assert false
    | Leaf -> raise Not_found

  let rec insert key tree =
    match tree with
    | Node { v; _ } when v = key -> tree
    | Node { l; v; r } when v < key ->
        let r = insert key r in
        Node { l; v; r }
    | Node { l; v; r } when v > key ->
        let l = insert key l in
        Node { l; v; r }
    | Node _ -> assert false
    | Leaf -> Node { l = Leaf; v = key; r = Leaf }

  let rec remove key tree =
    match tree with
    | Node { l = Leaf; v; r = Leaf } when key <> v -> tree
    | Node { l = Leaf; v; r = Leaf } when key = v -> Leaf
    | Node { l = Leaf; v; _ } when key < v -> tree
    | Node { l = Leaf; v; r } when key = v -> r
    | Node { l = Leaf; v; r } when key > v ->
        let r = remove key r in
        Node { l = Leaf; v; r }
    | Node { r = Leaf; v; _ } when key > v -> tree
    | Node { r = Leaf; v; l } when key = v -> l
    | Node { r = Leaf; v; l } when key < v ->
        let l = remove key l in
        Node { r = Leaf; l; v }
    | Node { l; v; r } when key = v ->
        let v = max l in
        let l = remove v l in
        Node { l; v; r }
    | Node { l; v; r } when key > v ->
        let r = remove key r in
        Node { l; v; r }
    | Node { l; v; r } when key < v ->
        let l = remove key l in
        Node { l; v; r }
    | Node _ -> assert false
    | Leaf -> Leaf

  let change child replacement tree =
    match child with
    | Left -> (
        match tree with
        | Node { v; r; _ } -> Node { l = replacement; v; r }
        | Leaf -> raise Invalid_change)
    | Right -> (
        match tree with
        | Node { l; v; _ } -> Node { l; v; r = replacement }
        | Leaf -> raise Invalid_change)

  let rec map f tree =
    match tree with
    | Node { l; v; r } ->
        let l = map f l in
        let v = f v in
        let r = map f r in
        Node { l; v; r }
    | Leaf -> Leaf

  let rec iter f tree =
    match tree with
    | Node { l; v; r } ->
        iter f l;
        f v;
        iter f r
    | Leaf -> ()

  let rec fold f init tree =
    match tree with
    | Node { l; v; r } ->
        let init = fold f init l in
        let init = f init v in
        fold f init r
    | Leaf -> init
end

module AVL (Ord : OrderedType) = struct
  include BST (Ord)

  let is_balanced tree = abs (height (left tree) - height (right tree)) < 2

  let rec balance tree =
    if is_empty tree || is_balanced tree then tree
    else
      let tree =
        match tree with
        | Node { l; r; _ }
          when height l > height r && height (left l) >= height (right l) ->
            tree |> rotate Right
        | Node { l; r; _ }
          when height l > height r && height (left l) < height (right l) ->
            tree |> change Left (rotate Left l) |> rotate Right
        | Node { l; r; _ }
          when height r > height l && height (right r) >= height (left r) ->
            tree |> rotate Left
        | Node { l; r; _ }
          when height r > height l && height (right r) < height (left r) ->
            tree |> change Right (rotate Right r) |> rotate Left
        | Node _ -> assert false
        | Leaf -> Leaf
      in
      tree
      |> change Left (balance (left tree))
      |> change Right (balance (right tree))

  let insert x tree = tree |> insert x |> balance
end

module RBT (Ord : OrderedType) = struct
  type color = Red | Black

  include BST (struct
    type t = Ord.t * color

    let value t =
      let value, _ = t in
      value

    let compare x y = Ord.compare (value x) (value y)
  end)

  let color tree =
    match tree with
    | Node { v; _ } ->
        let _, color = v in
        color
    | Leaf -> Black

  let insert x tree = insert (x, Red) tree
end

module ST (Ord : OrderedType) = struct
  include BST (Ord)

  let splay x tree = tree
  let find x tree = tree |> splay x |> find x
  let insert x tree = tree |> insert x |> splay x
  let remove x tree = tree |> splay x |> remove x
end
