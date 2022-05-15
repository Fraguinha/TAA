module BST = Tree.BST (Int)
module AVL = Tree.AVL (Int)
module RBT = Tree.RBT (Int)
module ST = Tree.ST (Int)
module TH = Tree.TH (Int)

let () = Random.self_init ()
let n = 10_000

let sorted =
  Printf.printf "generating %#d sorted numbers\n" n;
  List.init n (fun i -> i)

let random =
  Printf.printf "generating %#d random numbers\n" n;
  List.init n (fun _ -> Random.int n)

let () = Printf.printf "\ninserting %#d sorted nodes\n\n" n

let () =
  print_endline "BST";
  let bst = List.fold_left (fun t i -> BST.insert i t) BST.empty sorted in
  print_string "height left: ";
  print_int (BST.height (BST.left bst));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right bst));
  print_newline ()

let () =
  print_endline "AVL";
  let avl = List.fold_left (fun t i -> AVL.insert i t) AVL.empty sorted in
  print_string "height left: ";
  print_int (AVL.height (AVL.left avl));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right avl));
  print_newline ()

let () =
  print_endline "RBT";
  let rbt = List.fold_left (fun t i -> RBT.insert i t) RBT.empty sorted in
  print_string "height left: ";
  print_int (RBT.height (RBT.left rbt));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right rbt));
  print_newline ()

let () =
  print_endline "ST";
  let st = List.fold_left (fun t i -> ST.insert i t) ST.empty sorted in
  print_string "height left: ";
  print_int (ST.height (ST.left st));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right st));
  print_newline ()

let () =
  print_endline "TH";
  let th = List.fold_left (fun t i -> TH.insert i t) TH.empty sorted in
  print_string "height left: ";
  print_int (TH.height (TH.left th));
  print_newline ();
  print_string "height right: ";
  print_int (TH.height (TH.right th));
  print_newline ()

let () = Printf.printf "\ninserting %#d random nodes\n\n" n

let () =
  print_endline "BST";
  let bst = List.fold_left (fun t i -> BST.insert i t) BST.empty random in
  print_string "height left: ";
  print_int (BST.height (BST.left bst));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right bst));
  print_newline ()

let () =
  print_endline "AVL";
  let avl = List.fold_left (fun t i -> AVL.insert i t) AVL.empty random in
  print_string "height left: ";
  print_int (AVL.height (AVL.left avl));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right avl));
  print_newline ()

let () =
  print_endline "RBT";
  let rbt = List.fold_left (fun t i -> RBT.insert i t) RBT.empty random in
  print_string "height left: ";
  print_int (RBT.height (RBT.left rbt));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right rbt));
  print_newline ()

let () =
  print_endline "ST";
  let st = List.fold_left (fun t i -> ST.insert i t) ST.empty random in
  print_string "height left: ";
  print_int (ST.height (ST.left st));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right st));
  print_newline ()

let () =
  print_endline "TH";
  let th = List.fold_left (fun t i -> TH.insert i t) TH.empty random in
  print_string "height left: ";
  print_int (TH.height (TH.left th));
  print_newline ();
  print_string "height right: ";
  print_int (TH.height (TH.right th));
  print_newline ()
