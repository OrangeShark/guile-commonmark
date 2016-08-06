
AC_INIT([guile-commonmark], [0.1])
AC_CONFIG_SRCDIR(commonmark)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PKG([2.2 2.0])
GUILE_PROGS([2.0.11])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])

AC_OUTPUT
