include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(prefix)/share/guile/site/2.0

SOURCES =                           \
  commonmark/utils.scm              \
  commonmark/common.scm             \
  commonmark/entities.scm           \
  commonmark/node.scm               \
  commonmark/blocks.scm             \
  commonmark/inlines.scm            \
  commonmark/sxml.scm               \
  commonmark.scm

TESTS =	                                       \
  tests/node.scm                               \
  tests/blocks/paragraphs.scm                  \
  tests/blocks/atx-headings.scm                \
  tests/blocks/block-quotes.scm                \
  tests/blocks/code-blocks.scm                 \
  tests/blocks/fenced-code.scm                 \
  tests/blocks/link-reference-definitions.scm  \
  tests/blocks/list-items.scm                  \
  tests/blocks/lists.scm                       \
  tests/blocks/setext-headings.scm             \
  tests/blocks/thematic-breaks.scm             \
  tests/inlines/code-spans.scm                 \
  tests/inlines/emphasis.scm                   \
  tests/inlines/backslash-escape.scm           \
  tests/inlines/softbreak.scm                  \
  tests/inlines/hardbreak.scm                  \
  tests/inlines/links.scm                      \
  tests/inlines/images.scm                     \
  tests/inlines/autolinks.scm                  \
  tests/inlines/entities.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST += pre-inst-env.in
