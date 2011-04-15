# Autoconf script for Caml-Shcaml

# Don't run just anywhere
AC_INIT(shtop.mli)

# Packages we depend on
PACKAGES="camlp4 unix pcre"

# Our version number
VERSION="0.1.1"

# Our version name
CODENAME="Shmooz"

# shlock shmaltz shadken shanda shaygets shayner shiksa
# shlemiel shlep shlimazl shmendrick shmegegge
# shmeer shmooz shmutzik shnorrer shtik shtetl shtunk
# shvitz 

# Used:
# 0.1.0 shmatta
# 0.1.1 shmooz

# We need ocamlfind
AC_PATH_PROG(OCAMLFIND,ocamlfind,no)
if test "$OCAMLFIND" = no; then
    AC_MSG_ERROR(Cannot find ocamlfind.)
fi

# We assume that ocamlfind can find ocamlc, ocamlopt, etc.  But we need
# to find ocamllex:
AC_PATH_PROG(OCAMLLEX,ocamllex,no)
if test "$OCAMLLEX" = no; then
    AC_MSG_ERROR(Cannot find ocamllex.)
fi

# For some reason, we want the build time.
AC_PATH_PROG(DATEPROG,date,no)
if test "$DATEPROG" = no; then
    AC_MSG_ERROR(Cannot find date.)
fi
BUILDTIME="`$DATEPROG`"

# We use CPP to put together some source files.
AC_PROG_CPP

# Check for findlib libraries:
for lib in $PACKAGES; do
    AC_MSG_CHECKING(for findlib package: $lib)
    if $OCAMLFIND query $lib >/dev/null 2>&1; then
        AC_MSG_RESULT(ok)
    else
    AC_MSG_ERROR(Cannot find findlib package: $lib)
    fi
done

AC_SUBST(PACKAGES)
AC_SUBST(VERSION)
AC_SUBST(CODENAME)
AC_SUBST(BUILDTIME)

AC_OUTPUT(Makefile syntax/Makefile META lib/version.ml)
chmod a-w Makefile syntax/Makefile META lib/version.ml
