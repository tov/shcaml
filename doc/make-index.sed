# This script is meant to be run with sed -nf

# This takes the styled script tutorial doc/tutorial.ml (which is
# Ocaml-parsable) and generates doc/INDEX for use as the HTML
# documentation index.

:start
/^\s*(\*\+\s*$/bcomment0
/^\s*$/{
    n
    bstart
}
bcode0

:comment0
n
/^\s*$/bcomment0
:comment1
/^> \?/bresponse0
/^\s*\*\+)\s*$/{
    n
    bstart
}
p
n
bcomment1

:response0
s/^> \?/{v /
h
:response1
n
/^> \?/{
    s/^> \?//
    s/^\(\s*\)-/\1~/
    H
    bresponse1
}
x
s/$/\
\
\
v}/
p
x
bcomment1

:code0
s/^/{[# /
h
:code1
n
/^\s*(\*\+\s*$/{
    g
    s/\s*$/]}/
    p
    bcomment0
}
s/^/  /
H
bcode1
