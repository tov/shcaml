#!/bin/sh

usage () {
    exec 1>&2
    echo ""
    echo "Usage: $0 -[Ff]"
    echo "Configure a git work tree to use git-svn to pull from the"
    echo "Caml-Shcaml SVN repository."
    echo ""
    echo "Options"
    echo "  -f   fetch SVN metadata"
    echo "  -F   don't fetch SVN metadata"
    echo ""
}

fetch=

for arg; do
    case "$arg" in
        -f) fetch=yes
            ;;
        -F) fetch=no
            ;;
        -h|--help)
            usage
            exit
            ;;
        *)  echo "$0: unknown argument: \`$arg'" >&2
            usage
            exit 1
            ;;
    esac
done

if [ -z "$fetch" ]; then
    echo "$0: no argument provided" >&2
    usage
    exit 1
fi

# Tell git to git the remote SVN trunk, and then git it:
git config --add remote.origin.fetch '+refs/remotes/trunk:refs/remotes/trunk'
git fetch origin

# Initialize git-svn.  It will use the trunk that we already gitted.
git svn init -t tags -b branches -T trunks/shcaml \
    svn+ssh://osprepo.janestcapital.com/home/svn/repos/osp/2007/caml-shcaml

# Optionally fetch:
if [ "$fetch" = yes ]; then
    git svn fetch
fi

