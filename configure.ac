
AC_INIT([commonmark], [0.1.dev])
AC_CONFIG_SRCDIR(commonmark)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

PKG_CHECK_MODULES([GUILE], [guile-2.0])

GUILE_PROGS

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
