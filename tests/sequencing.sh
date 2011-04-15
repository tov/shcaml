#!/bin/sh

source `dirname $0`/test_lib.sh

prepare run_in simple <<END
let c = run_in begin
  trans id ^>> command "echo here" ^>> trans id
end in try while true do
  print_endline (input_line c)
done with
| End_of_file -> ()
END

run hello <<END
hello
END

check1 <<END
hello
here
END

check2 <<END
END

run a b <<END
a
b
END

check1 <<END
a
b
here
END

check2 <<END
END

prepare run simple <<END
run begin
  trans id ^>> command "echo here" ^>> trans id
end
END

run hello <<END
hello
END

check1 <<END
hello
here
END

check2 <<END
END

finish
