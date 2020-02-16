type dtree = Leaf of int | Node of (char * dtree * dtree)

type graph = ((int list) * int) list

type pair_encoding = ((char list)*graph)

let tLeft : dtree = Node('w', 
                Node ('x', 
                    Leaf(2),
                    Leaf(5)),
                Leaf(8))

let tRight : dtree = Node('w',
                Node('x',
                    Leaf(2),
                    Leaf(5)),
                Node('y',
                    Leaf(7),
                    Leaf(5) ) )
let tUnevenSym : dtree = Node('w',
                            Node('x',
                                Node('x',
                                    Leaf(2),
                                    Leaf(5)),
                                Leaf(5)),
                            Node('y',
                                Leaf(7),
                                    Node('x',
                                    Leaf(2),
                                    Leaf(5)) ) )


let f_graph : graph = [([0;0;0] , 0); ([0;0;1] , 1); ([0;1;0] , 1); ([0;1;1] , 0); ([1;0;0] , 1); ([1;0;1] , 0); ([1;1;0] , 0); ([1;1;1] , 1)]

let rec dTree_height: dtree -> int = fun t ->
    match t with
    | Leaf(_) -> 0
    | Node(_, lt, rt) -> 1 + (max (dTree_height lt) (dTree_height rt))

let rec dTree_size: dtree -> int = fun t -> 
    match t with
    | Leaf(_) -> 1
    | Node(_, lt, rt) -> 1 + (dTree_size lt) + (dTree_size rt)

let rec dTree_paths: dtree -> int list list = fun t -> 
    match t with
    | Leaf(_) -> [[]]
    | Node(_, lt, rt) -> List.map (fun i -> 0::i) (dTree_paths lt)
                            @
                        List.map (fun i -> 1::i) (dTree_paths rt) 

let rec dTree_is_perfect: dtree -> bool = fun t -> 
    match t with 
    | Leaf(_) -> true
    | Node(_, lt, rt) -> ((dTree_height lt) = (dTree_height rt)) && (dTree_is_perfect lt) && (dTree_is_perfect rt)

let rec dTree_map: (char -> char) -> (int -> int) -> dtree -> dtree = fun f g t -> 
    match t with
    | Leaf(n) -> Leaf(g n)
    | Node(c, lt, rt) -> Node(f c, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree: char list -> dtree = fun l ->
    match l with
    | [] -> Leaf(0)
    | h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec replace_leaf_at_helper: dtree -> (char list * int) -> dtree = fun d n ->
	match n with 
	| (l, i) -> (
		match l, d with
		| [], Leaf(_) -> Leaf(i)
		| h::t, Node(_, lt, rt) -> (
			match h with
			| 0 -> Node(_, replace_leaf_at_helper lt (t, i), rt)
			| 1 -> Node(_, lt, replace_leaf_at_helper rt (t, i))
		)
		| h::t, Leaf(_) -> failwith"graph location is out of bounds"
		| [], Node(_, _, _) -> failwith"graph location does not exist in tree"
	)

let rec replace_leaf_at: dtree -> graph -> dtree = fun d g ->
	match g with
	| [] -> d
	| h::t -> replace_leaf_at (replace_leaf_at_helper d h) t

let bf_to_dTree: pair_encoding -> dtree = fun pe ->
    match pe with
    | (l, g) -> replace_leaf_at (list_to_tree l) g
