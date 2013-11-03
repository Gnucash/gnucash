ACCT_TST_FILES = \
	Money95bank_fr.gml2 \
	Money95invst.gml2 \
	Money95mutual.gml2 \
	Money95stocks.gml2 \
	abc.gml2 \
	abcall.gml2 \
	carols-data-file.gml2 \
	cbb-export.gml2 \
	conrads-file.gml2 \
	every.gml2 \
	goonies-file.gml2 \
	hierachical-data-file.gml2 \
	ms-money.gml2 \
	pricedb1.gml2 

EXTRA_DIST = $(ACCT_TST_FILES)

check:
	for file in $(ACCT_TST_FILES); do			\
		test $(srcdir)/$$file -ef $(builddir)/$$file ||	\
		cp $(srcdir)/$$file $(builddir)/$$file;		\
	done;

clean-local:
	rm -f *.gnucash
