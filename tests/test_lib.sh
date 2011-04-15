name=
subname=

infile=`tempfile`
outfile=`tempfile`
errfile=`tempfile`
difffile=`tempfile`
fifo=`tempfile`

rm -f $fifo
mkfifo $fifo

prepare () {
    name="$*"
    subname="$*"
    cat >$infile <<....EOF
        #use "topfind";;
        #camlp4o;;
        #require "shcaml";;
....EOF
    cat >>$infile
}

run () {
    subname="$*"
    ocaml $infile >$outfile 2>$errfile
}

check () {
    if ! diff - $1 >$difffile; then
        echo
        echo "$name:$subname ($2)"
        cat $difffile
    fi
}

check1 () {
    check $outfile stdout
}

check2 () {
    check $errfile stderr
}

finish () {
    rm -f $infile $outfile $errfile $difffile $fifo
    echo done.
}
