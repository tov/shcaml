open Soup

let fix_include_with file signame to_add =
  let soup = read_file file |> parse in
  let enclosing_pre =
    soup $$ "pre > span.keyword"
    |> filter (fun n -> R.leaf_text n = "include")
    |> map R.parent
    |> filter (fun n -> (n $ "a" |> R.leaf_text) = signame)
    |> R.first in
  (* make this function idempotent *)
  enclosing_pre $ "a" $$ "~ *" |> iter delete; 
  append_child enclosing_pre (parse to_add);
  write_file file (to_string soup)

let _ =
  Unix.chdir "api.docdir";

  fix_include_with "Fitting.html" "FittingSig.S"
    {|<code class="type">
  with type initial      = <a href="LineShtream.html">LineShtream</a>.initial
   and type 'a elem      = 'a <a href="LineShtream.html">LineShtream</a>.elem
   and type 'a shtream   = 'a <a href="LineShtream.html">LineShtream</a>.t
   and type 'a coshtream = 'a <a href="LineShtream.html">LineShtream</a>.co_t</code>|};

  fix_include_with "LineShtream.html" "AnyShtreamSig.S"
    {|<code class="type"> with module Elem = <a href="LineShtream.LineElem.html">LineElem</a></code>|};

  fix_include_with "StringShtream.html" "AnyShtreamSig.S"
    {|<code class="type"> with module Elem = <a href="StringShtream.StringElem.html">StringElem</a></code>|};

  fix_include_with "AnyShtreamSig.S.html" "Shtream.COMMON"
    {|<code class="type">
  with type 'a t    = 'a <a href="Shtream.html">Shtream</a>.t
   and type 'a co_t = 'a <a href="Shtream.html">Shtream</a>.co_t</code>|};

  fix_include_with "Shtream.html" "ShtreamSig.S"
    {|<code class="type">
  with type 'a t    := 'a <a href="Shtream.html#TYPEt">t</a>
   and type 'a co_t := 'a <a href="Shtream.html#TYPEco_t">co_t</a></code>|}
