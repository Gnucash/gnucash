
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
	rm -f www
	$(LN_S) -f . www
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

CLEANFILES = www .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

endif # GNC_HAVE_GUILE_WWW (else clause)

EXTRA_DIST = README README.gnucash wwwcat cgi.scm http.scm main.scm url.scm
