
SWIG_FILTER    := %P.h %/util.h
SWIG_HDRS      := $(filter-out ${SWIG_FILTER},$(wildcard ../engine/*.h))

# Because Swig's include mechanism is not too smart.
SWIG_BASENAMES := $(notdir ${SWIG_HDRS})

CFLAGS := -I.. \
          -I../.. \
          -I../engine \
          -I../register \
          -I./../../include \
          -I/usr/include/readline \
          -I/usr/lib/perl5/i386-linux/5.004/CORE

CFLAGS  += -O2 -Wall -Wno-unused

LIBENG  := ../libengine.a

all: guile perl5

# This is .PHONY because it has to be re-generated *every* time.  Who
# knows when headers are added to the engine dir?
xacc.i:
	@echo "%module xacc" > xacc.i
	@echo "%{" >> xacc.i
	@($(foreach hdr,${SWIG_BASENAMES},echo "#include <${hdr}>"; )) >> xacc.i
	@echo "%}" >> xacc.i
	@($(foreach hdr,${SWIG_BASENAMES},echo %include ${hdr}; )) >> xacc.i
.PHONY: xacc.i

%_wrap.c: xacc.i

##### Guile

guile: guile/xacc-guile
.PHONY: guile

##### Guile

xacc-guile: xacc-guile.o xacc-guile_wrap.o $(LIBENG) 

guile/xacc-guile_wrap.c: xacc.i ${SWIG_HDRS}
	swig -I../engine -guile -o $@ $< 

guile/xacc-guile: guile/xacc-guile.o guile/xacc-guile_wrap.o $(LIBENG) 
	${CC} $^ ${LIBENG} -lguile -lreadline -lm -o $@
CLEANFILES += guile/xacc-guile

##### Perl5

perl5: perl5/xacc.so

perl5/xacc.so: perl5/xacc-perl5_wrap.c $(LIBENG) 
	${CC} ${CFLAGS} -shared $^ -lm -o $@ -Dbool=char -fpic
CLEANFILES += perl5/xacc.so

perl5/xacc-perl5_wrap.c: xacc.i ${SWIG_HDRS}
	swig -I../engine -perl5 -o $@ $< 

clean:
	rm -f *.o *~ *.bak *.i ${CLEANFILES}
	(cd guile && rm -f *.o *~ *.bak *_wrap.c *.doc)
	(cd perl5 && rm -f *.o *~ *.bak *_wrap.c *.doc *.pm)

# Local Variables:
# tab-width: 2
# End:

# DO NOT DELETE THIS LINE -- make depend depends on it.
