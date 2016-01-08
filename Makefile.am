include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(prefix)/share/guile/site/2.0

SOURCES =                           \
  commonmark/utils.scm              \
  commonmark/node.scm               \
  commonmark/blocks.scm             \
  commonmark/inlines.scm            \
  commonmark/sxml.scm               \
  commonmark.scm

TESTS =	                            \
  tests/node.scm                    \
  tests/blocks.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST += pre-inst-env.in
