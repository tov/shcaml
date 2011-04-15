(** Miscellaneous utility types and values. *)

(** Indicates a bug in the library.  This exception is raised in
    cases that violate library or language invariants. *)
exception Bug

(** The identity function *)
let id x       = x
(** The {i K} combinator *)
let const x y  = x
(** Function composition *)
let (%) f g x  = f (g x)
(** Function application *)
let ($) f x    = f x
(** Function application, right associative *)
let (^$) f x   = f x
(** Flip the arguments of a function *)
let flip f x y = f y x

(** Anonymous sum types *)
type ('a, 'b) either =
 | Left of 'a
 | Right of 'b

(** Projection from the anonymous sum type {!either} *)
let either e l r = match e with
 | Left x  -> l x
 | Right x -> r x

(** Projection from [option] *)
let maybe o n s = match o with
  | None   -> n ()
  | Some x -> s x

(** Application inside [option] *)
let oapply f o = match o with
  | None   -> None
  | Some x -> Some (f x)

(** Map exceptions to [None] *)
let option_of_exn thunk =
  try Some (thunk ()) with _ -> None

(** Call a thunk and then run cleanup code.  [Util.unwind_protect thunk
    after] calls [thunk].  If [thunk] returns a value, it calls [after] and
    then returns the value.  If [thunk] raises, it calls [after] and then
    re-raises. *)
let unwind_protect thunk after =
  let result = try thunk ()
                 with e -> after (); raise e in
    after ();
    result

(** Loop until a thunk returns [Some v].  [Util.while_none cond body]
    calls [cond].  If [cond] returns [Some v] then it returns [v]; if
    [cond] returns [None], it calls [body] and then tries iterates. *)
let rec while_none cond body =
  match cond () with
  | Some r -> r
  | None   -> body (); while_none cond body

(** Find an element in a list.  Like [List.find'], but returns an
 * [option] rather than raising. *)
let rec find' p lst = match lst with
  | []               -> None
  | x :: _ when p x  -> Some x
  | _ :: xs          -> find' p xs

(** Record type for universally quantified {i around} advice. *)
type protector       = { protector: 'a . (unit -> 'a) -> 'a; }
