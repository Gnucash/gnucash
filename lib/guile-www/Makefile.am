
gncscmdir = ${GNC_SHAREDIR}/guile-modules/www
gncscm_DATA = cgi.scm http.scm main.scm url.scm

EXTRA_DIST = README README.gnucash wwwcat ${gncscm_DATA}

noinst_DATA = .scm-links

.scm-links: 
	rm -f www
	ln -sf . www
	touch .scm-links
CLEANFILES = www .scm-links
