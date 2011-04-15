open Util

type 'a t = Empty
          | Node of 'a node_data
and 'a node_data = {
  data:    'a;
  prio:    int;
  left:    'a t;
  right:   'a t;
}

let empty = Empty

let is_empty = function
 | Empty -> true
 | _     -> false

let rec insert prio data = function
  | Empty  -> Node {
                data  = data;
                prio  = prio;
                left  = Empty;
                right = Empty;
              }
  | Node n when prio < n.prio
           -> Node {
                data  = data;
                prio  = prio;
                left  = insert n.prio n.data n.right;
                right = n.left;
              }
  | Node n -> Node {
               data  = n.data;
               prio  = n.prio;
               left  = insert prio data n.right;
               right = n.left;
              }

let peek = function
 | Empty  -> raise Not_found
 | Node n -> n.data

let rec remove_min = function
 | Empty -> raise Not_found
 | Node { left  = Empty; right = r } -> r
 | Node { right = Empty; left  = l } -> l
 | Node { left  = Node l; right = Node r } ->
     if r.prio < l.prio then
       Node {
         data  = r.data;
         prio  = r.prio;
         left  = Node l;
         right = remove_min (Node r);
       }
     else
       Node {
         data  = l.data;
         prio  = l.prio;
         left  = remove_min (Node l);
         right = Node r;
       }

