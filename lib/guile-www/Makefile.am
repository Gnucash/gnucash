
if GNC_HAVE_GUILE_WWW

# Do nothing.

else # !GNC_HAVE_GUILE_WWW

gncscmdir = ${GNC_SHAREDIR}/guile-modules/www
gncscm_DATA = cgi.scm http.scm main.scm url.scm

noinst_DATA = .scm-links

.scm-links: 
	rm -f www
	ln -sf . www
	touch .scm-links
CLEANFILES = www .scm-links

endif # GNC_HAVE_GUILE_WWW (else clause)

EXTRA_DIST = README README.gnucash wwwcat cgi.scm http.scm main.scm url.scm

