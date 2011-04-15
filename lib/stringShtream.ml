open Util

module StringElem = struct
  type 'a elem  = string
  type initial  = unit
  let reader    () = input_line
  let string_of () = id
  let of_string () = id
end

include AnyShtream.Make(StringElem)
