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
  val mem : elt -> t -> bool
  val find : elt -> t -> t
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module BST (Ord : OrderedType) = struct
  type dir = Left | Right
  type elt = Ord.t
  type t = Leaf | Node of { l : t; v : elt; r : t }

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

  let change side replacement tree =
    match side with
    | Left -> (
        match tree with
        | Node { v; r; _ } -> Node { l = replacement; v; r }
        | Leaf -> raise Invalid_change)
    | Right -> (
        match tree with
        | Node { l; v; _ } -> Node { l; v; r = replacement }
        | Leaf -> raise Invalid_change)

  let rec mem el tree =
    match tree with
    | Node { v; _ } when el = v -> true
    | Node { v; r; _ } when el > v -> mem v r
    | Node { v; l; _ } when el < v -> mem v l
    | Node _ -> false
    | Leaf -> raise Not_found

  let rec find el tree =
    match tree with
    | Node { v; _ } when el = v -> tree
    | Node { v; r; _ } when el > v -> find el r
    | Node { v; l; _ } when el < v -> find el l
    | Node _ -> assert false
    | Leaf -> raise Not_found

  let rec insert el tree =
    match tree with
    | Node { v; _ } when v = el -> tree
    | Node { l; v; r } when v < el ->
        let r = insert el r in
        Node { l; v; r }
    | Node { l; v; r } when v > el ->
        let l = insert el l in
        Node { l; v; r }
    | Node _ -> assert false
    | Leaf -> Node { l = Leaf; v = el; r = Leaf }

  let rec delete el tree =
    match tree with
    | Node { l = Leaf; v; r = Leaf } when el <> v -> tree
    | Node { l = Leaf; v; r = Leaf } when el = v -> Leaf
    | Node { l = Leaf; v; _ } when el < v -> tree
    | Node { l = Leaf; v; r } when el = v -> r
    | Node { l = Leaf; v; r } when el > v ->
        let r = delete el r in
        Node { l = Leaf; v; r }
    | Node { r = Leaf; v; _ } when el > v -> tree
    | Node { r = Leaf; v; l } when el = v -> l
    | Node { r = Leaf; v; l } when el < v ->
        let l = delete el l in
        Node { r = Leaf; l; v }
    | Node { l; v; r } when el = v ->
        let v = max l in
        let l = delete v l in
        Node { l; v; r }
    | Node { l; v; r } when el > v ->
        let r = delete el r in
        Node { l; v; r }
    | Node { l; v; r } when el < v ->
        let l = delete el l in
        Node { l; v; r }
    | Node _ -> assert false
    | Leaf -> Leaf

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

  let insert el tree = tree |> insert el |> balance
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

  let value tree =
    match tree with
    | Node { v; _ } ->
        let value, _ = v in
        value
    | Leaf -> raise Not_found

  let balance tree =
    match tree with
    | Node
        {
          l = Node { l = Node { l = a; v = x, Red; r = b }; v = y, Red; r = c };
          v = z, Black;
          r = d;
        }
    | Node
        {
          l = Node { l = a; v = x, Red; r = Node { l = b; v = y, Red; r = c } };
          v = z, Black;
          r = d;
        }
    | Node
        {
          l = a;
          v = x, Black;
          r = Node { l = Node { l = b; v = y, Red; r = c }; v = z, Red; r = d };
        }
    | Node
        {
          l = a;
          v = x, Black;
          r = Node { l = b; v = y, Red; r = Node { l = c; v = z, Red; r = d } };
        } ->
        Node
          {
            l = Node { l = a; v = (x, Black); r = b };
            v = (y, Red);
            r = Node { l = c; v = (z, Black); r = d };
          }
    | Node _ -> tree
    | Leaf -> assert false

  let insert el tree =
    let rec ins tree =
      match tree with
      | Node { v = v, _; _ } when el = v -> tree
      | Node { l; v = v, c; r } when el < v ->
          let l = ins l in
          balance (Node { l; v = (v, c); r })
      | Node { l; v = v, c; r } when el > v ->
          let r = ins r in
          balance (Node { l; v = (v, c); r })
      | Node _ -> assert false
      | Leaf -> Node { l = Leaf; v = (el, Red); r = Leaf }
    in
    match ins tree with
    | Node { l; v = v, _; r } -> Node { l; v = (v, Black); r }
    | Leaf -> assert false

  let delete el tree =
    let rec del el tree =
      match tree with
      | Node { l; v = v, _; r } when el = v ->
          let el, _ = max l in
          let l = del el l in
          balance (Node { l; v = (el, Red); r })
      | Node { l; v = v, c; r } when el < v ->
          let l = del el l in
          balance (Node { l; v = (v, c); r })
      | Node { l; v = v, c; r } when el > v ->
          let r = del el r in
          balance (Node { l; v = (v, c); r })
      | Node _ -> assert false
      | Leaf -> tree
    in
    match del el tree with
    | Node { l; v = v, _; r } -> Node { l; v = (v, Black); r }
    | Leaf -> assert false
end

module ST (Ord : OrderedType) = struct
  include BST (Ord)

  let splay el tree = tree
  let find el tree = tree |> splay el |> find el
  let insert el tree = tree |> insert el |> splay el
  let remove el tree = tree |> splay el |> delete el
end
