# This script is meant to be run with sed -nf

# This takes the styled script tutorial doc/tutorial.ml (which is
# Ocaml-parsable) and generates doc/INDEX for use as the HTML
# documentation index.

:start
/^[[:space:]]*(\*\**[[:space:]]*$/bcomment0
/^[[:space:]]*$/{
    n
    bstart
}
bcode0

:comment0
n
/^[[:space:]]*$/bcomment0
:comment1
/^> /bresponse0
/^[[:space:]]*\*\**)[[:space:]]*$/{
    n
    bstart
}
p
n
bcomment1

:response0
s/^> /{v /
h
:response1
n
/^> /{
    s/^> //
    s/^\([[:space:]]*\)-/\1~/
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
/^[[:space:]]*(\*\**[[:space:]]*$/{
    g
    s/[[:space:]]*$/]}/
    p
    bcomment0
}
s/^/  /
H
bcode1
