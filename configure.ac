
AC_INIT([guile-commonmark], [0.2])
AC_CONFIG_SRCDIR([commonmark.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl srfi-64 required for tests, introduced by Guile 2.0.11
GUILE_MODULE_REQUIRED([srfi srfi-64])


AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/gcmark], [chmod +x scripts/gcmark])

AC_OUTPUT
