
if GNC_HAVE_GUILE_WWW

# Do nothing.

else # !GNC_HAVE_GUILE_WWW

gncscmdir = ${GNC_SHAREDIR}/guile-modules/www
gncscm_DATA = cgi.scm http.scm main.scm url.scm

noinst_DATA = .scm-links

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = ${gncscm_DATA}
endif

.scm-links: 
	$(RM) -rf www
	mkdir -p www
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd www; for A in $(gncscm_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf www

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

endif # GNC_HAVE_GUILE_WWW (else clause)

EXTRA_DIST = README README.gnucash wwwcat cgi.scm http.scm main.scm url.scm
