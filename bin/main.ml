module BST = Tree.BST (Int)
module AVL = Tree.AVL (Int)
module RBT = Tree.RBT (Int)
module ST = Tree.ST (Int)

let n = 100
let random = List.init n (fun _ -> Random.int n)
let order = List.init n (fun i -> i)
let () = Printf.printf "inserting %d nodes randomly\n\n" n

let () =
  print_endline "BST";
  let t = List.fold_left (fun t i -> BST.insert i t) BST.empty random in
  print_string "height left: ";
  print_int (BST.height (BST.left t));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right t));
  print_newline ()

let () =
  print_endline "AVL";
  let t = List.fold_left (fun t i -> AVL.insert i t) AVL.empty random in
  print_string "height left: ";
  print_int (AVL.height (AVL.left t));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right t));
  print_newline ()

let () =
  print_endline "RBT";
  let t = List.fold_left (fun t i -> RBT.insert i t) RBT.empty random in
  print_string "height left: ";
  print_int (RBT.height (RBT.left t));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right t));
  print_newline ()

let () =
  print_endline "ST";
  let t = List.fold_left (fun t i -> ST.insert i t) ST.empty random in
  print_string "height left: ";
  print_int (ST.height (ST.left t));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right t));
  print_newline ()

let () = Printf.printf "\ninserting %d nodes in order\n\n" n

let () =
  print_endline "BST";
  let t = List.fold_left (fun t i -> BST.insert i t) BST.empty order in
  print_string "height left: ";
  print_int (BST.height (BST.left t));
  print_newline ();
  print_string "height right: ";
  print_int (BST.height (BST.right t));
  print_newline ()

let () =
  print_endline "AVL";
  let t = List.fold_left (fun t i -> AVL.insert i t) AVL.empty order in
  print_string "height left: ";
  print_int (AVL.height (AVL.left t));
  print_newline ();
  print_string "height right: ";
  print_int (AVL.height (AVL.right t));
  print_newline ()

let () =
  print_endline "RBT";
  let t = List.fold_left (fun t i -> RBT.insert i t) RBT.empty order in
  print_string "height left: ";
  print_int (RBT.height (RBT.left t));
  print_newline ();
  print_string "height right: ";
  print_int (RBT.height (RBT.right t));
  print_newline ()

let () =
  print_endline "ST";
  let t = List.fold_left (fun t i -> ST.insert i t) ST.empty order in
  print_string "height left: ";
  print_int (ST.height (ST.left t));
  print_newline ();
  print_string "height right: ";
  print_int (ST.height (ST.right t));
  print_newline ()
