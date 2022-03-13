module BST = Tree.BST (Int)
module AVL = Tree.AVL (Int)
module RBT = Tree.RBT (Int)
module ST = Tree.ST (Int)

let () = Random.self_init ()

let gaussian mean sd =
  let box_muller () =
    1.
    +. sqrt (-2. *. log (Random.float 1.))
       *. cos (2. *. Float.pi *. Random.float 1.)
  in
  (sd *. box_muller ()) +. mean |> int_of_float

let n = 10_000

let normal =
  Printf.printf "generating %#d normally distributed nodes\n" n;
  List.init n (fun _ ->
      let n = float_of_int n in
      let mean = n /. 2. in
      let sd = n *. 0.05 in
      gaussian mean sd)

let sorted =
  Printf.printf "generating %#d sorted nodes\n" n;
  List.init n (fun i -> i)

let random =
  Printf.printf "generating %#d random nodes\n" n;
  List.init n (fun _ -> Random.int n)

let () =
  Printf.printf "\ninserting %#d sorted nodes\n\n" n;
  print_endline "BST";
  let bst = List.fold_left (fun t i -> BST.insert i t) BST.empty sorted in
  print_string "height left: ";
  print_int (BST.height (BST.left bst));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right bst));
  print_newline ();
  print_endline "AVL";
  let avl = List.fold_left (fun t i -> AVL.insert i t) AVL.empty sorted in
  print_string "height left: ";
  print_int (AVL.height (AVL.left avl));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right avl));
  print_newline ();
  print_endline "RBT";
  let rbt = List.fold_left (fun t i -> RBT.insert i t) RBT.empty sorted in
  print_string "height left: ";
  print_int (RBT.height (RBT.left rbt));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right rbt));
  print_newline ();
  print_endline "ST";
  let st = List.fold_left (fun t i -> ST.insert i t) ST.empty sorted in
  print_string "height left: ";
  print_int (ST.height (ST.left st));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right st));
  print_newline ()

let () =
  Printf.printf "\ninserting %#d random nodes\n\n" n;
  print_endline "BST";
  let bst = List.fold_left (fun t i -> BST.insert i t) BST.empty random in
  print_string "height left: ";
  print_int (BST.height (BST.left bst));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right bst));
  print_newline ();
  print_endline "AVL";
  let avl = List.fold_left (fun t i -> AVL.insert i t) AVL.empty random in
  print_string "height left: ";
  print_int (AVL.height (AVL.left avl));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right avl));
  print_newline ();
  print_endline "RBT";
  let rbt = List.fold_left (fun t i -> RBT.insert i t) RBT.empty random in
  print_string "height left: ";
  print_int (RBT.height (RBT.left rbt));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right rbt));
  print_newline ();
  print_endline "ST";
  let st = List.fold_left (fun t i -> ST.insert i t) ST.empty random in
  print_string "height left: ";
  print_int (ST.height (ST.left st));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right st));
  print_newline ()
