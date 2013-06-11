type Tree<'T> =
  | Empty
  | Node of 'T * Tree<'T> * Tree<'T>


let rec tree_member tree x =
  match tree with
    | Empty -> false
    | Node (y, a, b) ->
        match x with
          | _ when y > x -> tree_member a x
          | _ when y < x -> tree_member b x
          | _ -> true

let rec insert tree x =
  match tree with
    | Empty -> Node(x, Empty, Empty)
    | Node (y, a, b) ->
      match x with
        | _ when x < y -> Node(y, (insert a x) , b)
        | _ when x > y -> Node(y, a, (insert b x))
        | _ -> Node(y, a, b)
       

let xs = Node('d',
              Node('b',
                   Node('a', Empty, Empty),
                   Node('c', Empty, Empty)),
              Node('g',
                   Node('f', Empty, Empty),
                   Node('h', Empty, Empty)))
                                  
tree_member xs 'e'

insert xs 'e'
