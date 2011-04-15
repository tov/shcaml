#directory "..";;
#load "line.cmo";;

let foo = Line.line "foo";;
let bar = Line.raw foo;;
let baz = Line.Ls.inode foo;;

