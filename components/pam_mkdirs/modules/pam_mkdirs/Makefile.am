#
# Copyright (c) 2005, 2006, 2009 Thorsten Kukuk <kukuk@suse.de>
# Copyright (c) 2008 Red Hat, Inc.
#

CLEANFILES = *~
MAINTAINERCLEANFILES =

EXTRA_DIST =

man_MANS =

XMLS =

TESTS =

securelibdir = $(SECUREDIR)
secureconfdir = $(SCONFIGDIR)

AM_CFLAGS = -I$(top_srcdir)/libpam/include -I$(top_srcdir)/libpamc/include \
	    -DMKDIRS_HELPER=\"$(sbindir)/mkdirs_helper\"

securelib_LTLIBRARIES = pam_mkdirs.la
pam_mkdirs_la_SOURCES = pam_mkdirs.c
pam_mkdirs_la_LIBADD = $(top_builddir)/libpam/libpam.la
pam_mkdirs_la_LDFLAGS = -no-undefined -avoid-version -module
if HAVE_VERSIONING
  pam_mkdirs_la_LDFLAGS += -Wl,--version-script=$(srcdir)/../modules.map
endif

sbin_PROGRAMS = mkdirs_helper
mkdirs_helper_SOURCES = mkdirs_helper.c
mkdirs_helper_LDADD = $(top_builddir)/libpam/libpam.la
